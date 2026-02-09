use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::sync::Arc;

use crate::ast::container::AST;
use crate::ast::grammar::{
    self, Any, BinaryOperator, Expression, FunctionBody, LocalIdentifier, TypeExpression,
    UnaryOperator,
};
use crate::ast::modules::{Module, ModulesStore};
use crate::ast::slice::Slice;
use crate::types::fits::FitsContext;

use super::infer::InferTypeContext;

#[derive(Debug, Clone, Copy)]
pub struct NormalizeContext<'a> {
    pub modules: Option<&'a ModulesStore>,
    pub current_module: Option<&'a Module>,
}

/// The result of resolving a local identifier to its declaration site.
#[derive(Debug, Clone)]
pub enum ResolvedIdentifier<'a> {
    /// A const declaration, possibly in a different module
    ConstDeclaration {
        decl: grammar::ConstDeclaration,
        module: Option<&'a Module>,
    },
    /// A function parameter in the current module
    FunctionParam {
        name: AST<grammar::PlainIdentifier>,
        param_index: usize,
        func_node: AST<Any>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Any,
    Unknown,
    Never,
    Nil,
    Boolean {
        value: Option<bool>,
    },
    Number {
        min_value: Option<i64>,
        max_value: Option<i64>,
    },
    String {
        value: Option<Slice>,
    },
    Tuple {
        elements: Vec<Type>,
    },
    Array {
        element: Arc<Type>,
    },
    Object {
        fields: BTreeMap<String, Type>,
    },

    /// An opaque, special type. Mostly used to represent JS built-ins,
    /// classes, etc. An interface is similar to an Object type, but
    /// 1. An Object type can be assigned to a compatible Interface type, but
    ///    the reverse is *not* true
    /// 2. While Object always represents an actual plain JS object/record,
    ///    Interface can be backed by anything with the specified
    ///    properties/methods; it makes no guarantees about cloneability, etc
    Interface {
        name: String,
        fields: BTreeMap<String, Type>,
    },
    FuncType {
        args: Vec<Type>,
        args_spread: Option<Arc<Type>>,
        returns: Arc<Type>,
    },
    Union {
        variants: Vec<Type>,
    },
    BinaryOperation {
        operator: BinaryOperator,
        left: Arc<Type>,
        right: Arc<Type>,
    },
    UnaryOperation {
        operator: UnaryOperator,
        operand: Arc<Type>,
    },
    IfElse {
        condition: Arc<Type>,
        consequent: Arc<Type>,
        alternate: Arc<Type>,
    },
    Invocation {
        function: Arc<Type>,
        args: Vec<Type>,
    },
    PropertyAccess {
        subject: Arc<Type>,
        property: String,
    },
    LocalIdentifier {
        identifier: LocalIdentifier,
    },
}

impl Type {
    /// Some types can be exactly equivalent in the set of values they describe,
    /// but structurally represented differently. For example, a union that
    /// contains one or more other unions doesn't change its meaning if those
    /// unions are flattened. Other examples include:
    /// - A union containing two of the exact same type
    /// - A union containing a more general type like `number` alongside a more
    ///   specific type like `42` (the more general type can subsume both of
    ///   them)
    /// - A union containing two object types where one is broader than the
    ///   other
    /// - Two unions that contain the same set of elements in a different
    ///   ordering
    ///
    /// This function makes a best-effort to simplify and normalize as many of
    /// these cases as possible, to make types more human-readable but also to
    /// create more situations where exactly-equivalent types have the exact
    /// same structural representation.
    ///
    /// It also, additionally, "interprets" certain types that are lazily
    /// resolved, to try and figure out their "real" type. Local identifiers
    /// are a major instance- this function determines what "real" type a given
    /// variable turns out to have.
    pub fn normalize(self, ctx: NormalizeContext<'_>) -> Self {
        use Type::*;

        match self {
            Union { variants } => {
                let flattened_and_unique = variants
                    .into_iter()
                    .flat_map(|v| {
                        let v = v.normalize(ctx); // recurse

                        // flatten any inner unions
                        match v {
                            Union {
                                variants: variants_inner,
                            } => variants_inner,
                            other => vec![other],
                        }
                    })
                    .collect::<HashSet<Type>>(); // collect into a HashSet to uniquify

                let mut v: Vec<Type> = flattened_and_unique.into_iter().collect();
                v.sort(); // finally, sort the types for consistency

                // Remove variants that are subsumed by other variants.
                // E.g. `true | boolean` collapses to `boolean` because
                // `true` fits into `boolean`.
                let fits_ctx = FitsContext {
                    modules: ctx.modules,
                };
                let keep: Vec<bool> = v
                    .iter()
                    .enumerate()
                    .map(|(i, candidate)| {
                        !v.iter().enumerate().any(|(j, other)| {
                            i != j
                                && candidate.clone().fits(other.clone(), fits_ctx)
                                && !other.clone().fits(candidate.clone(), fits_ctx)
                        })
                    })
                    .collect();
                let mut keep_iter = keep.iter();
                v.retain(|_| *keep_iter.next().unwrap());

                if v.len() == 1 {
                    v.into_iter().next().unwrap()
                } else {
                    Union { variants: v }
                }
            }
            LocalIdentifier { identifier } => resolve_local_identifier(&identifier, ctx),
            Invocation { function, args } => {
                let func_type = function.as_ref().clone().normalize(ctx);
                match func_type {
                    FuncType { returns, .. } => returns.as_ref().clone(),
                    _ => Unknown,
                }
            }
            PropertyAccess { subject, property } => {
                let subject_normalized = subject.as_ref().clone().normalize(ctx);
                match &subject_normalized {
                    Object { fields } | Interface { fields, .. } => {
                        fields.get(&property).cloned().unwrap_or(Never)
                    }
                    _ => PropertyAccess {
                        subject: Arc::new(subject_normalized),
                        property,
                    },
                }
            }
            BinaryOperation {
                operator,
                left,
                right,
            } => normalize_binary_operation(
                operator,
                left.as_ref().clone().normalize(ctx),
                right.as_ref().clone().normalize(ctx),
                ctx,
            ),
            UnaryOperation { operator, operand } => {
                normalize_unary_operation(operator, operand.as_ref().clone().normalize(ctx))
            }
            IfElse {
                condition,
                consequent,
                alternate,
            } => normalize_if_else(
                condition.as_ref().clone().normalize(ctx),
                consequent.as_ref().clone().normalize(ctx),
                alternate.as_ref().clone().normalize(ctx),
                ctx,
            ),
            Tuple { elements } => Tuple {
                elements: elements.into_iter().map(|e| e.normalize(ctx)).collect(),
            },
            Array { element } => Array {
                element: Arc::new(element.as_ref().clone().normalize(ctx)),
            },
            Object { fields } => Object {
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k, v.normalize(ctx)))
                    .collect(),
            },
            Interface { name, fields } => Interface {
                name,
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k, v.normalize(ctx)))
                    .collect(),
            },
            FuncType {
                args,
                args_spread,
                returns,
            } => FuncType {
                args: args.into_iter().map(|a| a.normalize(ctx)).collect(),
                args_spread: args_spread.map(|s| Arc::new(s.as_ref().clone().normalize(ctx))),
                returns: Arc::new(returns.as_ref().clone().normalize(ctx)),
            },
            _ => self,
        }
    }

    /// Widens exact literal types to their general counterparts.
    /// For example, `ExactBoolean(true)` becomes `Boolean`, `ExactNumber(42)`
    /// becomes `Number`, `ExactString("foo")` becomes `String`.
    /// This is used when inferring the type of a variable declaration without
    /// an explicit type annotation.
    pub fn widen(self) -> Self {
        match self {
            Type::Boolean { value: Some(_) } => Type::Boolean { value: None },
            Type::Number {
                min_value: Some(_), ..
            }
            | Type::Number {
                max_value: Some(_), ..
            } => Type::Number {
                min_value: None,
                max_value: None,
            },
            Type::String { value: Some(_) } => Type::String { value: None },
            other => other,
        }
    }
}

impl AST<Expression> {
    /// Given an expression at some specific point in the AST tree, look
    /// upwards to try and find what type it "should" have based on its context,
    /// if any.
    ///
    /// Examples:
    /// - The value of a const declaration, if the const has an annotated type,
    ///   is expected to be that annotated type
    /// - The value of an argument to a function invocation is expected to be
    ///   the type of that function's Nth parameter
    ///
    /// This will be used for things like inferring the parameter types of a
    /// function that doesn't have them annotated, but is passed into a context
    /// where they're known (lambdas)
    pub fn expected_type(&self) -> Option<Type> {
        let parent = self.parent()?;
        let norm_ctx = NormalizeContext {
            modules: None,
            current_module: None,
        };

        match parent.details()? {
            Any::Declaration(grammar::Declaration::ConstDeclaration(const_decl)) => const_decl
                .type_annotation
                .as_ref()
                .map(|(_colon, type_expr)| Type::from(type_expr.unpack())),
            Any::Expression(Expression::Invocation(inv)) => {
                let arg_index = inv.arguments.iter().position(|arg| arg.ptr_eq(self))?;

                let ctx = InferTypeContext {
                    modules: None,
                    current_module: None,
                };
                let func_type = inv.function.infer_type(ctx).normalize(norm_ctx);

                match func_type {
                    Type::FuncType { args, .. } => args.into_iter().nth(arg_index),
                    _ => None,
                }
            }
            Any::FunctionBody(FunctionBody::Expression(_)) => {
                let func_body_node = parent;
                let func_node = func_body_node.parent()?;

                match func_node.details()? {
                    Any::Expression(Expression::FunctionExpression(func)) => func
                        .return_type
                        .as_ref()
                        .map(|(_colon, type_expr)| Type::from(type_expr.unpack())),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

/// The shared core of identifier resolution. Walks up the AST scope chain to
/// find where an identifier is declared — as a const declaration (possibly in
/// an imported module) or as a function parameter.
///
/// This is the single source of truth for "what does this name refer to?".
/// Both type inference (`resolve_local_identifier`) and go-to-definition in
/// the LSP consume this result.
pub fn resolve_identifier<'a>(
    identifier: &grammar::LocalIdentifier,
    ctx: NormalizeContext<'a>,
) -> Option<ResolvedIdentifier<'a>> {
    let name = identifier.identifier.slice().as_str();

    // Walk up the AST from the identifier's AST node to find a containing scope
    let mut current = identifier.identifier.parent();

    while let Some(node) = current {
        match node.details() {
            Some(Any::Module(module)) => {
                // First, search module-level const declarations for one matching our name
                let const_match = module
                    .declarations
                    .iter()
                    .filter(|decl| decl.details().is_some())
                    .filter_map(|decl| match decl.unpack() {
                        grammar::Declaration::ConstDeclaration(const_decl) => Some(const_decl),
                        grammar::Declaration::ImportDeclaration(_) => None,
                    })
                    .find(|const_decl| const_decl.identifier.slice().as_str() == name);

                if let Some(const_decl) = const_match {
                    return Some(ResolvedIdentifier::ConstDeclaration {
                        decl: const_decl,
                        module: None,
                    });
                }

                // Next, check import declarations for a matching imported name
                if let (Some(store), Some(current_module)) = (ctx.modules, ctx.current_module) {
                    let import_match = module
                        .declarations
                        .iter()
                        .filter(|decl| decl.details().is_some())
                        .filter_map(|decl| match decl.unpack() {
                            grammar::Declaration::ImportDeclaration(import_decl) => {
                                Some(import_decl)
                            }
                            grammar::Declaration::ConstDeclaration(_) => None,
                        })
                        .find_map(|import_decl| {
                            import_decl
                                .imports
                                .iter()
                                .find(|spec| {
                                    let local_name = spec
                                        .alias
                                        .as_ref()
                                        .map(|(_, alias)| alias.slice().as_str())
                                        .unwrap_or_else(|| spec.name.slice().as_str());
                                    local_name == name
                                })
                                .map(|spec| {
                                    let original_name = spec.name.slice().as_str().to_string();
                                    let import_path =
                                        import_decl.path.unpack().contents.as_str().to_string();
                                    (original_name, import_path)
                                })
                        });

                    if let Some((original_name, import_path)) = import_match {
                        return resolve_imported_identifier(
                            &original_name,
                            &import_path,
                            store,
                            current_module,
                        );
                    }
                }

                return None;
            }
            Some(Any::Expression(Expression::FunctionExpression(func))) => {
                // Check if one of the function's parameters matches our name
                let param_match = func
                    .parameters
                    .iter()
                    .enumerate()
                    .find(|(_, (param_name, _type_ann))| param_name.slice().as_str() == name);

                if let Some((param_index, (param_name, _type_ann))) = param_match {
                    return Some(ResolvedIdentifier::FunctionParam {
                        name: param_name.clone(),
                        param_index,
                        func_node: node.clone(),
                    });
                }

                // Not a parameter of this function — keep walking up
            }
            _ => {}
        }

        current = node.parent();
    }

    None
}

/// The built-in `js` global object type. This is an Interface that exposes
/// JavaScript built-ins to Bagel code.
pub fn js_global_type() -> Type {
    Type::Interface {
        name: "js".to_string(),
        fields: BTreeMap::from([(
            String::from("console"),
            Type::Interface {
                name: "Console".to_string(),
                fields: BTreeMap::from([(
                    String::from("log"),
                    Type::FuncType {
                        args: vec![],
                        args_spread: Some(Arc::new(Type::Unknown)),
                        returns: Arc::new(Type::Never),
                    },
                )]),
            },
        )]),
    }
}

/// Thin wrapper around `resolve_identifier` that extracts the type from the
/// resolution result. Called by `normalize()` for `Type::LocalIdentifier`.
fn resolve_local_identifier(
    identifier: &grammar::LocalIdentifier,
    ctx: NormalizeContext<'_>,
) -> Type {
    // Special built-in: the `js` global
    if identifier.identifier.slice().as_str() == "js" {
        return js_global_type();
    }

    match resolve_identifier(identifier, ctx) {
        Some(ResolvedIdentifier::ConstDeclaration { decl, module }) => {
            let decl_ctx = match module {
                Some(m) => NormalizeContext {
                    modules: ctx.modules,
                    current_module: Some(m),
                },
                None => ctx,
            };
            resolve_declaration_type(&decl, decl_ctx)
        }
        Some(ResolvedIdentifier::FunctionParam {
            name: _,
            param_index,
            func_node,
        }) => {
            // Check if the parameter has a type annotation
            match func_node.details() {
                Some(Any::Expression(Expression::FunctionExpression(func))) => {
                    match &func.parameters[param_index].1 {
                        Some((_colon, type_expr)) => Type::from(type_expr.unpack()),
                        None => {
                            // Try contextual typing: get expected type for the
                            // function expression and extract the parameter type
                            let func_expr_node: AST<Expression> = match &func_node {
                                AST::Valid(inner, _) => AST::<Expression>::new(inner.clone()),
                                AST::Malformed { inner, message } => {
                                    AST::<Expression>::new_malformed(inner.clone(), message.clone())
                                }
                            };
                            func_expr_node
                                .expected_type()
                                .and_then(|t| match t.normalize(ctx) {
                                    Type::FuncType { args, .. } => {
                                        args.into_iter().nth(param_index)
                                    }
                                    _ => None,
                                })
                                .unwrap_or(Type::Unknown)
                        }
                    }
                }
                _ => Type::Unknown,
            }
        }
        None => Type::Unknown,
    }
}

/// Resolve an identifier imported from another module, returning the
/// declaration site in the imported module.
fn resolve_imported_identifier<'a>(
    name: &str,
    import_path: &str,
    store: &'a ModulesStore,
    current_module: &Module,
) -> Option<ResolvedIdentifier<'a>> {
    let imported_module = store.find_imported(current_module, import_path)?;

    // Find the exported const declaration with the matching name in the imported module
    let imported_module_data = imported_module.ast.unpack();
    imported_module_data
        .declarations
        .iter()
        .filter(|decl| decl.details().is_some())
        .filter_map(|decl| match decl.unpack() {
            grammar::Declaration::ConstDeclaration(const_decl) => Some(const_decl),
            grammar::Declaration::ImportDeclaration(_) => None,
        })
        .find(|const_decl| const_decl.identifier.slice().as_str() == name)
        .map(|const_decl| ResolvedIdentifier::ConstDeclaration {
            decl: const_decl,
            module: Some(imported_module),
        })
}

/// Resolve the type of a declaration from its type annotation (if present)
/// or by inferring from its value expression.
fn resolve_declaration_type(
    decl: &grammar::ConstDeclaration,
    norm_ctx: NormalizeContext<'_>,
) -> Type {
    let ctx = InferTypeContext {
        modules: norm_ctx.modules,
        current_module: norm_ctx.current_module,
    };
    decl.value.infer_type(ctx).normalize(norm_ctx)
}

/// Helper to create an exact String type from a computed string value.
fn exact_string(s: std::string::String) -> Type {
    Type::String {
        value: Some(Slice::new(Arc::new(s))),
    }
}

/// Returns true if a type is "falsy" at the type level (exactly false or nil).
fn is_exact_falsy(t: &Type) -> bool {
    matches!(t, Type::Boolean { value: Some(false) } | Type::Nil)
}

/// Returns true if a type is "truthy" at the type level (an exact non-false,
/// non-nil value).
fn is_exact_truthy(t: &Type) -> bool {
    matches!(
        t,
        Type::Boolean { value: Some(true) }
            | Type::Number {
                min_value: Some(_),
                max_value: Some(_)
            }
            | Type::String { value: Some(_) }
    )
}

/// Helper: construct an exact number type (min == max == value).
fn exact_number(value: i64) -> Type {
    Type::Number {
        min_value: Some(value),
        max_value: Some(value),
    }
}

/// Helper: try to extract an exact i64 from a Number type where min == max.
fn as_exact_number(t: &Type) -> Option<i64> {
    match t {
        Type::Number {
            min_value: Some(min),
            max_value: Some(max),
        } if min == max => Some(*min),
        _ => None,
    }
}

/// Helper: try to extract an exact string value from a String type.
fn as_exact_string(t: &Type) -> Option<&Slice> {
    match t {
        Type::String { value: Some(v) } => Some(v),
        _ => None,
    }
}

/// Helper: try to extract an exact bool value from a Boolean type.
fn as_exact_bool(t: &Type) -> Option<bool> {
    match t {
        Type::Boolean { value: Some(v) } => Some(*v),
        _ => None,
    }
}

/// Normalize a binary operation type by computing exact results where possible.
fn normalize_binary_operation(
    operator: BinaryOperator,
    left: Type,
    right: Type,
    ctx: NormalizeContext<'_>,
) -> Type {
    use BinaryOperator::*;

    let any_number = Type::Number {
        min_value: None,
        max_value: None,
    };
    let any_string = Type::String { value: None };

    match operator {
        Add => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) => l.checked_add(r).map(exact_number).unwrap_or(any_number),
            _ => match (as_exact_string(&left), as_exact_string(&right)) {
                (Some(l), Some(r)) => exact_string(format!("{}{}", l.as_str(), r.as_str())),
                _ => match (as_exact_string(&left), as_exact_number(&right)) {
                    (Some(l), Some(r)) => exact_string(format!("{}{}", l.as_str(), r)),
                    _ => match (as_exact_number(&left), as_exact_string(&right)) {
                        (Some(l), Some(r)) => exact_string(format!("{}{}", l, r.as_str())),
                        _ => match (&left, &right) {
                            (Type::String { .. }, _) | (_, Type::String { .. }) => any_string,
                            _ => any_number,
                        },
                    },
                },
            },
        },
        Subtract => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) => l.checked_sub(r).map(exact_number).unwrap_or(any_number),
            _ => any_number,
        },
        Multiply => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) => l.checked_mul(r).map(exact_number).unwrap_or(any_number),
            _ => any_number,
        },
        Divide => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) if r != 0 => {
                l.checked_div(r).map(exact_number).unwrap_or(any_number)
            }
            _ => any_number,
        },

        Equal => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) => Type::Boolean {
                value: Some(l == r),
            },
            _ => match (as_exact_string(&left), as_exact_string(&right)) {
                (Some(l), Some(r)) => Type::Boolean {
                    value: Some(l.as_str() == r.as_str()),
                },
                _ => match (as_exact_bool(&left), as_exact_bool(&right)) {
                    (Some(l), Some(r)) => Type::Boolean {
                        value: Some(l == r),
                    },
                    _ => match (&left, &right) {
                        (Type::Nil, Type::Nil) => Type::Boolean { value: Some(true) },
                        _ => Type::Boolean { value: None },
                    },
                },
            },
        },
        NotEqual => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) => Type::Boolean {
                value: Some(l != r),
            },
            _ => match (as_exact_string(&left), as_exact_string(&right)) {
                (Some(l), Some(r)) => Type::Boolean {
                    value: Some(l.as_str() != r.as_str()),
                },
                _ => match (as_exact_bool(&left), as_exact_bool(&right)) {
                    (Some(l), Some(r)) => Type::Boolean {
                        value: Some(l != r),
                    },
                    _ => match (&left, &right) {
                        (Type::Nil, Type::Nil) => Type::Boolean { value: Some(false) },
                        _ => Type::Boolean { value: None },
                    },
                },
            },
        },
        LessThan => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) => Type::Boolean { value: Some(l < r) },
            _ => Type::Boolean { value: None },
        },
        LessThanOrEqual => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) => Type::Boolean {
                value: Some(l <= r),
            },
            _ => Type::Boolean { value: None },
        },
        GreaterThan => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) => Type::Boolean { value: Some(l > r) },
            _ => Type::Boolean { value: None },
        },
        GreaterThanOrEqual => match (as_exact_number(&left), as_exact_number(&right)) {
            (Some(l), Some(r)) => Type::Boolean {
                value: Some(l >= r),
            },
            _ => Type::Boolean { value: None },
        },

        And => {
            if is_exact_falsy(&left) {
                left
            } else if is_exact_truthy(&left) {
                right
            } else {
                Type::Union {
                    variants: vec![left, right],
                }
                .normalize(ctx)
            }
        }
        Or => {
            if is_exact_truthy(&left) {
                left
            } else if is_exact_falsy(&left) {
                right
            } else {
                Type::Union {
                    variants: vec![left, right],
                }
                .normalize(ctx)
            }
        }
        NullishCoalescing => {
            if matches!(left, Type::Nil) {
                right
            } else if is_exact_truthy(&left) {
                left
            } else {
                Type::Union {
                    variants: vec![left, right],
                }
                .normalize(ctx)
            }
        }
    }
}

/// Normalize a unary operation type by computing exact results where possible.
fn normalize_unary_operation(operator: UnaryOperator, operand: Type) -> Type {
    match operator {
        UnaryOperator::Not => match as_exact_bool(&operand) {
            Some(value) => Type::Boolean {
                value: Some(!value),
            },
            None => match operand {
                Type::Nil => Type::Boolean { value: Some(true) },
                _ => Type::Boolean { value: None },
            },
        },
    }
}

/// Normalize an if/else expression type.
/// If the condition is known to be truthy, return the consequent type.
/// If the condition is known to be falsy, return the alternate type.
/// Otherwise, return a union of both branches.
fn normalize_if_else(
    condition: Type,
    consequent: Type,
    alternate: Type,
    ctx: NormalizeContext<'_>,
) -> Type {
    if is_exact_truthy(&condition) {
        consequent
    } else if is_exact_falsy(&condition) {
        alternate
    } else {
        Type::Union {
            variants: vec![consequent, alternate],
        }
        .normalize(ctx)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Unknown => write!(f, "unknown"),
            Type::Never => write!(f, "never"),
            Type::Nil => write!(f, "nil"),
            Type::Boolean { value: None } => write!(f, "boolean"),
            Type::Boolean { value: Some(v) } => write!(f, "{}", v),
            Type::Number {
                min_value: None,
                max_value: None,
            } => write!(f, "number"),
            Type::Number {
                min_value: Some(min),
                max_value: Some(max),
            } if min == max => write!(f, "{}", min),
            Type::Number {
                min_value,
                max_value,
            } => match (min_value, max_value) {
                (Some(min), None) => write!(f, "{}..", min),
                (None, Some(max)) => write!(f, "..{}", max),
                (Some(min), Some(max)) => write!(f, "{}..{}", min, max),
                _ => unreachable!(), // None/None handled above
            },
            Type::String { value: None } => write!(f, "string"),
            Type::String { value: Some(v) } => write!(f, "'{}'", v.as_str()),
            Type::Tuple { elements } => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Type::Array { element } => write!(f, "{}[]", element),
            Type::Object { fields } => {
                write!(f, "{{ ")?;
                for (i, (key, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, " }}")
            }
            Type::Interface { name, fields } => {
                write!(f, "{} ", name)?;
                if !fields.is_empty() {
                    write!(f, "{{ ")?;
                    for (i, (key, value)) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: {}", key, value)?;
                    }
                    write!(f, " }}")?;
                }
                Ok(())
            }
            Type::FuncType {
                args,
                args_spread,
                returns,
            } => {
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "arg{}: {}", i, arg)?;
                }
                if let Some(spread) = args_spread {
                    if !args.is_empty() {
                        write!(f, ", ")?;
                    }
                    write!(f, "...args: {}", spread)?;
                }
                write!(f, ") => {}", returns)
            }
            Type::Union { variants } => {
                for (i, variant) in variants.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", variant)?;
                }
                Ok(())
            }
            Type::BinaryOperation {
                operator,
                left,
                right,
            } => write!(f, "({} {} {})", left, operator.as_str(), right),
            Type::UnaryOperation { operator, operand } => {
                write!(f, "({}{})", operator.as_str(), operand)
            }
            Type::IfElse {
                condition,
                consequent,
                alternate,
            } => write!(
                f,
                "(if {} {{ {} }} else {{ {} }})",
                condition, consequent, alternate
            ),
            Type::Invocation { function, args } => {
                write!(f, "{}(", function)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Type::PropertyAccess { subject, property } => {
                write!(f, "{}.{}", subject, property)
            }
            Type::LocalIdentifier { identifier } => {
                write!(f, "{}", identifier.identifier.slice().as_str())
            }
        }
    }
}

impl From<TypeExpression> for Type {
    fn from(type_expr: TypeExpression) -> Self {
        use TypeExpression::*;

        match type_expr {
            UnknownTypeExpression(_) => Type::Unknown,
            NilTypeExpression(_) => Type::Nil,
            BooleanTypeExpression(_) => Type::Boolean { value: None },
            NumberTypeExpression(_) => Type::Number {
                min_value: None,
                max_value: None,
            },
            StringTypeExpression(_) => Type::String { value: None },
            TupleTypeExpression(tuple) => {
                let elements = tuple
                    .elements
                    .into_iter()
                    .map(|elem| Type::from(elem.unpack()))
                    .collect();
                Type::Tuple { elements }
            }
            ArrayTypeExpression(array) => {
                let element = Arc::new(Type::from(array.element.unpack()));
                Type::Array { element }
            }
            ObjectTypeExpression(obj) => {
                let fields = obj
                    .fields
                    .into_iter()
                    .map(|(name, _colon, type_expr)| {
                        let field_name = name.slice().as_str().to_string();
                        let field_type = Type::from(type_expr.unpack());
                        (field_name, field_type)
                    })
                    .collect();
                Type::Object { fields }
            }
            FunctionTypeExpression(func) => {
                let args = func
                    .parameters
                    .into_iter()
                    .map(|(_name_colon, type_expr)| Type::from(type_expr.unpack()))
                    .collect();
                let returns = Arc::new(Type::from(func.return_type.unpack()));
                Type::FuncType {
                    args,
                    args_spread: None,
                    returns,
                }
            }
            RangeTypeExpression(range) => Type::Number {
                min_value: range
                    .start
                    .as_ref()
                    .map(|s| s.as_str().parse::<i64>().unwrap()),
                max_value: range
                    .end
                    .as_ref()
                    .map(|s| s.as_str().parse::<i64>().unwrap()),
            },
            UnionTypeExpression(union) => {
                let variants = union
                    .variants
                    .into_iter()
                    .map(|variant| Type::from(variant.unpack()))
                    .collect();
                Type::Union { variants }
            }
            ParenthesizedTypeExpression(paren) => Type::from(paren.expression.unpack()),
            TypeOfTypeExpression(type_of) => {
                let ctx = InferTypeContext {
                    modules: None,
                    current_module: None,
                };
                type_of.expression.infer_type(ctx)
            }
        }
    }
}
