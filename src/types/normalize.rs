use std::collections::HashSet;
use std::sync::Arc;

use crate::ast::container::AST;
use crate::ast::grammar::{
    self, Any, BinaryOperator, Declaration, Expression, FunctionBody, UnaryOperator,
};
use crate::ast::modules::{Module, ModulesStore};
use crate::ast::slice::Slice;
use crate::types::fits::FitsContext;
use crate::types::infer::InferTypeContext;
use crate::types::Type;

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

impl ResolvedIdentifier<'_> {
    pub fn name(&self) -> &str {
        match self {
            ResolvedIdentifier::ConstDeclaration { decl, .. } => decl.identifier.slice().as_str(),
            ResolvedIdentifier::FunctionParam { name, .. } => name.slice().as_str(),
        }
    }
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
            Invocation { function, args: _ } => {
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
                        fields.get(&property).cloned().unwrap_or(Poisoned)
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
            } => {
                if condition.as_ref() == &Type::TRUE {
                    consequent.as_ref().clone().normalize(ctx)
                } else if condition.as_ref() == &Type::FALSE {
                    alternate.as_ref().clone().normalize(ctx)
                } else {
                    Type::Union {
                        variants: vec![consequent.as_ref().clone(), alternate.as_ref().clone()],
                    }
                    .normalize(ctx)
                }
            }
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
            Type::Boolean { value: Some(_) } => Type::ANY_BOOLEAN,
            Type::Number {
                min_value: Some(_), ..
            }
            | Type::Number {
                max_value: Some(_), ..
            } => Type::ANY_NUMBER,
            Type::String { value: Some(_) } => Type::ANY_STRING,
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
                .and_then(|(_colon, type_expr)| type_expr.unpack().map(Type::from)),
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
            Any::Expression(Expression::PipeCallExpression(pipe)) => {
                // subject is arg0, explicit arguments start at index 1
                let arg_index = if pipe.subject.ptr_eq(self) {
                    Some(0)
                } else {
                    pipe.arguments
                        .iter()
                        .position(|arg| arg.ptr_eq(self))
                        .map(|i| i + 1)
                };

                arg_index.and_then(|idx| {
                    let func = pipe.function.as_ref()?;
                    let func_expr: AST<Expression> = func.clone().upcast();
                    let infer_ctx = InferTypeContext {
                        modules: norm_ctx.modules,
                        current_module: norm_ctx.current_module,
                    };
                    let func_type = func_expr.infer_type(infer_ctx).normalize(norm_ctx);

                    match func_type {
                        Type::FuncType { args, .. } => args.into_iter().nth(idx),
                        _ => None,
                    }
                })
            }
            Any::FunctionBody(FunctionBody::Expression(_)) => {
                let func_body_node = parent;
                let func_node = func_body_node.parent()?;

                match func_node.details()? {
                    Any::Expression(Expression::FunctionExpression(func)) => func
                        .return_type
                        .as_ref()
                        .and_then(|(_colon, type_expr)| type_expr.unpack().map(Type::from)),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

/// Walks up the AST scope chain from the given node and collects all
/// identifiers (const declarations, imports, function parameters) that are
/// visible at that point.
pub fn identifiers_in_scope<'a>(
    node: &AST<Any>,
    ctx: NormalizeContext<'a>,
) -> Vec<ResolvedIdentifier<'a>> {
    let mut results = Vec::new();
    let mut current = node.parent();

    while let Some(ancestor) = current {
        match ancestor.details() {
            Some(Any::Module(module)) => {
                // Collect module-level const declarations
                results.extend(
                    module
                        .declarations
                        .iter()
                        .filter_map(|decl| match decl.unpack() {
                            Some(Declaration::ConstDeclaration(const_decl)) => Some(const_decl),
                            _ => None,
                        })
                        .map(|const_decl| ResolvedIdentifier::ConstDeclaration {
                            decl: const_decl,
                            module: None,
                        }),
                );

                // Collect imported identifiers
                if let (Some(store), Some(current_module)) = (ctx.modules, ctx.current_module) {
                    results.extend(
                        module
                            .declarations
                            .iter()
                            .filter_map(|decl| match decl.unpack() {
                                Some(Declaration::ImportDeclaration(import_decl)) => {
                                    Some(import_decl)
                                }
                                _ => None,
                            })
                            .flat_map(|import_decl| {
                                let import_path = import_decl
                                    .path
                                    .unpack()
                                    .map(|lit| lit.contents.as_str().to_string());
                                import_decl
                                    .imports
                                    .iter()
                                    .filter_map(move |spec| {
                                        let original_name = spec.name.slice().as_str().to_string();
                                        let path = import_path.as_ref()?;
                                        resolve_imported_identifier(
                                            &original_name,
                                            path,
                                            store,
                                            current_module,
                                        )
                                    })
                                    .collect::<Vec<_>>()
                            }),
                    );
                }

                // Module is the top-level scope; stop walking
                break;
            }
            Some(Any::Expression(Expression::FunctionExpression(func))) => {
                results.extend(func.parameters.iter().enumerate().map(
                    |(param_index, (param_name, _type_ann))| ResolvedIdentifier::FunctionParam {
                        name: param_name.clone(),
                        param_index,
                        func_node: ancestor.clone(),
                    },
                ));
            }
            _ => {}
        }

        current = ancestor.parent();
    }

    results
}

/// Resolve a specific identifier name at the given AST node. Walks up the
/// scope chain and returns the first declaration matching `name`.
pub fn resolve_identifier<'a>(
    name: &str,
    node: &AST<Any>,
    ctx: NormalizeContext<'a>,
) -> Option<ResolvedIdentifier<'a>> {
    identifiers_in_scope(node, ctx)
        .into_iter()
        .find(|r| r.name() == name)
}

/// The built-in `js` global object type. This is an Interface that exposes
/// JavaScript built-ins to Bagel code.
pub fn js_global_type() -> Type {
    Type::Interface {
        name: "JsGlobals".to_string(),
        fields: std::collections::BTreeMap::from([(
            String::from("console"),
            Type::Interface {
                name: "Console".to_string(),
                fields: std::collections::BTreeMap::from([(
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
    identifier: &AST<grammar::LocalIdentifier>,
    ctx: NormalizeContext<'_>,
) -> Type {
    let name = identifier.slice().as_str();

    // Special built-in: the `js` global
    if name == "js" {
        return js_global_type();
    }

    let node: AST<Any> = identifier.clone().upcast();
    match resolve_identifier(name, &node, ctx) {
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
                        Some((_colon, type_expr)) => {
                            type_expr.unpack().map(Type::from).unwrap_or(Type::Poisoned)
                        }
                        None => {
                            // Try contextual typing: get expected type for the
                            // function expression and extract the parameter type
                            let func_expr_node: AST<Expression> = match &func_node {
                                AST::Valid(inner, _) => AST::<Expression>::new(inner.clone()),
                                AST::Malformed(slice, message) => {
                                    AST::Malformed(slice.clone(), message.clone())
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
        None => Type::Poisoned,
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
    let imported_module_data = imported_module.ast.unpack()?;
    imported_module_data
        .declarations
        .iter()
        .filter(|decl| decl.details().is_some())
        .filter_map(|decl| match decl.unpack().unwrap() {
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
fn exact_string(s: String) -> Type {
    Type::String {
        value: Some(Slice::new(Arc::new(s))),
    }
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

    let fits_ctx = FitsContext {
        modules: ctx.modules,
    };

    if left == Type::Poisoned || right == Type::Poisoned {
        return Type::Poisoned;
    }

    // Check operand compatibility; return Poisoned if either operand doesn't fit
    if let Some(allowed) = operator.allowed_operand_type() {
        if !left.clone().fits(allowed.clone(), fits_ctx) || !right.clone().fits(allowed, fits_ctx) {
            return Type::Poisoned;
        }
    }

    match operator {
        Add => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                l.checked_add(r)
                    .map(exact_number)
                    .unwrap_or(Type::ANY_NUMBER)
            } else if let (Some(l), Some(r)) = (as_exact_string(&left), as_exact_string(&right)) {
                exact_string(format!("{}{}", l.as_str(), r.as_str()))
            } else if let (Some(l), Some(r)) = (as_exact_string(&left), as_exact_number(&right)) {
                exact_string(format!("{}{}", l.as_str(), r))
            } else if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_string(&right)) {
                exact_string(format!("{}{}", l, r.as_str()))
            } else if matches!(&left, Type::String { .. }) || matches!(&right, Type::String { .. })
            {
                Type::ANY_STRING
            } else {
                Type::ANY_NUMBER
            }
        }
        Subtract => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                l.checked_sub(r)
                    .map(exact_number)
                    .unwrap_or(Type::ANY_NUMBER)
            } else {
                Type::ANY_NUMBER
            }
        }
        Multiply => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                l.checked_mul(r)
                    .map(exact_number)
                    .unwrap_or(Type::ANY_NUMBER)
            } else {
                Type::ANY_NUMBER
            }
        }
        Divide => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                if r != 0 {
                    l.checked_div(r)
                        .map(exact_number)
                        .unwrap_or(Type::ANY_NUMBER)
                } else {
                    Type::ANY_NUMBER
                }
            } else {
                Type::ANY_NUMBER
            }
        }

        Equal => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean {
                    value: Some(l == r),
                }
            } else if let (Some(l), Some(r)) = (as_exact_string(&left), as_exact_string(&right)) {
                Type::Boolean {
                    value: Some(l.as_str() == r.as_str()),
                }
            } else if let (Some(l), Some(r)) = (as_exact_bool(&left), as_exact_bool(&right)) {
                Type::Boolean {
                    value: Some(l == r),
                }
            } else if left == Type::Nil && right == Type::Nil {
                Type::TRUE
            } else {
                Type::ANY_BOOLEAN
            }
        }
        NotEqual => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean {
                    value: Some(l != r),
                }
            } else if let (Some(l), Some(r)) = (as_exact_string(&left), as_exact_string(&right)) {
                Type::Boolean {
                    value: Some(l.as_str() != r.as_str()),
                }
            } else if let (Some(l), Some(r)) = (as_exact_bool(&left), as_exact_bool(&right)) {
                Type::Boolean {
                    value: Some(l != r),
                }
            } else if left == Type::Nil && right == Type::Nil {
                Type::FALSE
            } else {
                Type::ANY_BOOLEAN
            }
        }
        LessThan => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean { value: Some(l < r) }
            } else {
                Type::ANY_BOOLEAN
            }
        }
        LessThanOrEqual => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean {
                    value: Some(l <= r),
                }
            } else {
                Type::ANY_BOOLEAN
            }
        }
        GreaterThan => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean { value: Some(l > r) }
            } else {
                Type::ANY_BOOLEAN
            }
        }
        GreaterThanOrEqual => {
            if let (Some(l), Some(r)) = (as_exact_number(&left), as_exact_number(&right)) {
                Type::Boolean {
                    value: Some(l >= r),
                }
            } else {
                Type::ANY_BOOLEAN
            }
        }

        And => {
            if let (Some(left), Some(right)) = (as_exact_bool(&left), as_exact_bool(&right)) {
                (left && right).into()
            } else {
                Type::ANY_BOOLEAN
            }
        }
        Or => {
            if let (Some(left), Some(right)) = (as_exact_bool(&left), as_exact_bool(&right)) {
                (left || right).into()
            } else {
                Type::ANY_BOOLEAN
            }
        }
        NullishCoalescing => {
            if left == Type::Nil {
                right
            } else if !Type::Nil.fits(left.clone(), fits_ctx) {
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
    let fits_ctx = FitsContext { modules: None };
    let allowed = operator.allowed_operand_type();

    if !operand.clone().fits(allowed, fits_ctx) {
        return Type::Poisoned;
    }

    match operator {
        UnaryOperator::Not => match as_exact_bool(&operand) {
            Some(value) => (!value).into(),
            None => match operand {
                Type::Nil => Type::TRUE,
                _ => Type::ANY_BOOLEAN,
            },
        },
    }
}
