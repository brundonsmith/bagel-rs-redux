pub mod fits;
pub mod infer;

use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::sync::Arc;

use crate::ast::container::AST;
use crate::ast::grammar::{
    self, Any, BinaryOperator, Expression, FunctionBody, LocalIdentifier, UnaryOperator,
};
use crate::ast::slice::Slice;

use self::infer::InferTypeContext;

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

        /// If true, object may have additional fields (row polymorphic)
        is_open: bool,
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
    pub fn normalize(self) -> Self {
        use Type::*;

        match self {
            Union { variants } => {
                let flattened_and_unique = variants
                    .into_iter()
                    .flat_map(|v| {
                        let v = v.normalize(); // recurse

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
                let ctx = crate::types::fits::FitsContext {};
                let keep: Vec<bool> = v
                    .iter()
                    .enumerate()
                    .map(|(i, candidate)| {
                        !v.iter().enumerate().any(|(j, other)| {
                            i != j
                                && candidate.clone().fits(other.clone(), ctx)
                                && !other.clone().fits(candidate.clone(), ctx)
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
            LocalIdentifier { identifier } => resolve_local_identifier(&identifier),
            BinaryOperation {
                operator,
                left,
                right,
            } => normalize_binary_operation(
                operator,
                left.as_ref().clone().normalize(),
                right.as_ref().clone().normalize(),
            ),
            UnaryOperation { operator, operand } => {
                normalize_unary_operation(operator, operand.as_ref().clone().normalize())
            }
            IfElse {
                condition,
                consequent,
                alternate,
            } => normalize_if_else(
                condition.as_ref().clone().normalize(),
                consequent.as_ref().clone().normalize(),
                alternate.as_ref().clone().normalize(),
            ),
            Tuple { elements } => Tuple {
                elements: elements.into_iter().map(|e| e.normalize()).collect(),
            },
            Array { element } => Array {
                element: Arc::new(element.as_ref().clone().normalize()),
            },
            Object { fields, is_open } => Object {
                fields: fields
                    .into_iter()
                    .map(|(k, v)| (k, v.normalize()))
                    .collect(),
                is_open,
            },
            FuncType {
                args,
                args_spread,
                returns,
            } => FuncType {
                args: args.into_iter().map(|a| a.normalize()).collect(),
                args_spread: args_spread.map(|s| Arc::new(s.as_ref().clone().normalize())),
                returns: Arc::new(returns.as_ref().clone().normalize()),
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

        match parent.details()? {
            Any::Declaration(grammar::Declaration::ConstDeclaration(const_decl)) => const_decl
                .type_annotation
                .as_ref()
                .map(|(_colon, type_expr)| Type::from(type_expr.unpack())),
            Any::Expression(Expression::Invocation(inv)) => {
                let arg_index = inv.arguments.iter().position(|arg| arg.ptr_eq(self))?;

                let ctx = InferTypeContext {};
                let func_type = inv.function.infer_type(ctx).normalize();

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

/// Given a `LocalIdentifier` from a type, walk up the AST to find where the
/// identifier is declared, then resolve its type from the declaration's value
/// expression or type annotation.
fn resolve_local_identifier(identifier: &grammar::LocalIdentifier) -> Type {
    let name = identifier.identifier.slice().as_str();

    // Walk up the AST from the identifier's AST node to find a containing scope
    let mut current = identifier.identifier.parent();

    while let Some(node) = current {
        match node.details() {
            Some(Any::Module(module)) => {
                // Search module-level declarations for one matching our name
                return module
                    .declarations
                    .iter()
                    .filter(|decl| decl.details().is_some())
                    .filter_map(|decl| match decl.unpack() {
                        grammar::Declaration::ConstDeclaration(const_decl) => Some(const_decl),
                    })
                    .find(|const_decl| const_decl.identifier.slice().as_str() == name)
                    .map(|const_decl| resolve_declaration_type(&const_decl))
                    .unwrap_or(Type::Unknown);
            }
            Some(Any::Expression(Expression::FunctionExpression(func))) => {
                // Check if one of the function's parameters matches our name
                let param_match = func
                    .parameters
                    .iter()
                    .enumerate()
                    .find(|(_, (param_name, _type_ann))| param_name.slice().as_str() == name);

                if let Some((param_index, (_param_name, type_ann))) = param_match {
                    return match type_ann {
                        Some((_colon, type_expr)) => Type::from(type_expr.unpack()),
                        None => {
                            // Try contextual typing: get expected type for the
                            // function expression and extract the parameter type
                            let func_expr_node: AST<Expression> = match &node {
                                AST::Valid(inner, _) => AST::<Expression>::new(inner.clone()),
                                AST::Malformed { inner, message } => {
                                    AST::<Expression>::new_malformed(inner.clone(), message.clone())
                                }
                            };
                            func_expr_node
                                .expected_type()
                                .and_then(|t| match t.normalize() {
                                    Type::FuncType { args, .. } => {
                                        args.into_iter().nth(param_index)
                                    }
                                    _ => None,
                                })
                                .unwrap_or(Type::Unknown)
                        }
                    };
                }

                // Not a parameter of this function — keep walking up
            }
            _ => {}
        }

        current = node.parent();
    }

    // Identifier not found in any scope
    Type::Unknown
}

/// Resolve the type of a declaration from its type annotation (if present)
/// or by inferring from its value expression.
fn resolve_declaration_type(decl: &grammar::ConstDeclaration) -> Type {
    let ctx = InferTypeContext {};
    decl.value.infer_type(ctx).normalize()
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
fn normalize_binary_operation(operator: BinaryOperator, left: Type, right: Type) -> Type {
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
                .normalize()
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
                .normalize()
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
                .normalize()
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
fn normalize_if_else(condition: Type, consequent: Type, alternate: Type) -> Type {
    if is_exact_truthy(&condition) {
        consequent
    } else if is_exact_falsy(&condition) {
        alternate
    } else {
        Type::Union {
            variants: vec![consequent, alternate],
        }
        .normalize()
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
            Type::Object {
                fields,
                is_open: jopen,
            } => {
                write!(f, "{{")?;
                for (i, (key, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                if *jopen && !fields.is_empty() {
                    write!(f, ", ...")?;
                } else if *jopen {
                    write!(f, "...")?;
                }
                write!(f, "}}")
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
            Type::LocalIdentifier { identifier } => {
                write!(f, "{}", identifier.identifier.slice().as_str())
            }
        }
    }
}

impl From<crate::ast::grammar::TypeExpression> for Type {
    fn from(type_expr: crate::ast::grammar::TypeExpression) -> Self {
        use crate::ast::grammar::TypeExpression::*;

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
                Type::Object {
                    fields,
                    is_open: false,
                }
            }
            FunctionTypeExpression(func) => {
                let args = func
                    .parameters
                    .into_iter()
                    .map(|(_name, _colon, type_expr)| Type::from(type_expr.unpack()))
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
        }
    }
}
