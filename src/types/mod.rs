pub mod fits;
pub mod infer;

use std::collections::{BTreeMap, HashSet};
use std::fmt;
use std::sync::Arc;

use crate::ast::grammar::{self, Any, BinaryOperator, Expression, LocalIdentifier, UnaryOperator};
use crate::ast::slice::Slice;

use self::infer::InferTypeContext;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Any,
    Unknown,
    Never,
    Nil,
    Boolean,
    ExactBoolean {
        value: bool,
    },
    Number,
    ExactNumber {
        value: i64,
    },
    String,
    ExactString {
        value: Slice,
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
            } => normalize_binary_operation(operator, left.as_ref().clone().normalize(), right.as_ref().clone().normalize()),
            UnaryOperation { operator, operand } => {
                normalize_unary_operation(operator, operand.as_ref().clone().normalize())
            }
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
            Type::ExactBoolean { .. } => Type::Boolean,
            Type::ExactNumber { .. } => Type::Number,
            Type::ExactString { .. } => Type::String,
            other => other,
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
                    .find(|decl| decl.unpack().identifier.slice().as_str() == name)
                    .map(|decl| resolve_declaration_type(&decl.unpack()))
                    .unwrap_or(Type::Unknown);
            }
            Some(Any::Expression(Expression::FunctionExpression(func))) => {
                // Check if one of the function's parameters matches our name
                let param_match = func
                    .parameters
                    .iter()
                    .find(|(param_name, _type_ann)| param_name.slice().as_str() == name);

                if let Some((_param_name, type_ann)) = param_match {
                    return match type_ann {
                        Some((_colon, type_expr)) => Type::from(type_expr.unpack()),
                        None => Type::Unknown,
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
fn resolve_declaration_type(decl: &grammar::Declaration) -> Type {
    let ctx = InferTypeContext {};
    decl.value.infer_type(ctx).normalize()
}

/// Helper to create an ExactString type from a computed string value.
fn exact_string(s: String) -> Type {
    Type::ExactString {
        value: Slice::new(Arc::new(s)),
    }
}

/// Returns true if a type is "falsy" at the type level (exactly false or nil).
fn is_exact_falsy(t: &Type) -> bool {
    matches!(t, Type::ExactBoolean { value: false } | Type::Nil)
}

/// Returns true if a type is "truthy" at the type level (an exact non-false,
/// non-nil value).
fn is_exact_truthy(t: &Type) -> bool {
    match t {
        Type::ExactBoolean { value: true }
        | Type::ExactNumber { .. }
        | Type::ExactString { .. } => true,
        _ => false,
    }
}

/// Normalize a binary operation type by computing exact results where possible.
fn normalize_binary_operation(operator: BinaryOperator, left: Type, right: Type) -> Type {
    use BinaryOperator::*;
    use Type::*;

    match operator {
        Add => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) => l
                .checked_add(*r)
                .map(|value| ExactNumber { value })
                .unwrap_or(Number),
            // String concatenation cases
            (ExactString { value: l }, ExactString { value: r }) => {
                exact_string(format!("{}{}", l.as_str(), r.as_str()))
            }
            (ExactString { value: l }, ExactNumber { value: r }) => {
                exact_string(format!("{}{}", l.as_str(), r))
            }
            (ExactNumber { value: l }, ExactString { value: r }) => {
                exact_string(format!("{}{}", l, r.as_str()))
            }
            (ExactString { .. } | String, _) | (_, ExactString { .. } | String) => String,
            _ => Number,
        },
        Subtract => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) => l
                .checked_sub(*r)
                .map(|value| ExactNumber { value })
                .unwrap_or(Number),
            _ => Number,
        },
        Multiply => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) => l
                .checked_mul(*r)
                .map(|value| ExactNumber { value })
                .unwrap_or(Number),
            _ => Number,
        },
        Divide => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) if *r != 0 => l
                .checked_div(*r)
                .map(|value| ExactNumber { value })
                .unwrap_or(Number),
            _ => Number,
        },

        Equal => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) => ExactBoolean { value: l == r },
            (ExactString { value: l }, ExactString { value: r }) => {
                ExactBoolean { value: l.as_str() == r.as_str() }
            }
            (ExactBoolean { value: l }, ExactBoolean { value: r }) => {
                ExactBoolean { value: l == r }
            }
            (Nil, Nil) => ExactBoolean { value: true },
            _ => Boolean,
        },
        NotEqual => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) => ExactBoolean { value: l != r },
            (ExactString { value: l }, ExactString { value: r }) => {
                ExactBoolean { value: l.as_str() != r.as_str() }
            }
            (ExactBoolean { value: l }, ExactBoolean { value: r }) => {
                ExactBoolean { value: l != r }
            }
            (Nil, Nil) => ExactBoolean { value: false },
            _ => Boolean,
        },
        LessThan => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) => ExactBoolean { value: l < r },
            _ => Boolean,
        },
        LessThanOrEqual => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) => ExactBoolean { value: l <= r },
            _ => Boolean,
        },
        GreaterThan => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) => ExactBoolean { value: l > r },
            _ => Boolean,
        },
        GreaterThanOrEqual => match (&left, &right) {
            (ExactNumber { value: l }, ExactNumber { value: r }) => ExactBoolean { value: l >= r },
            _ => Boolean,
        },

        And => {
            if is_exact_falsy(&left) {
                left
            } else if is_exact_truthy(&left) {
                right
            } else {
                Union { variants: vec![left, right] }.normalize()
            }
        }
        Or => {
            if is_exact_truthy(&left) {
                left
            } else if is_exact_falsy(&left) {
                right
            } else {
                Union { variants: vec![left, right] }.normalize()
            }
        }
        NullishCoalescing => {
            if matches!(left, Nil) {
                right
            } else if is_exact_truthy(&left) {
                left
            } else {
                Union { variants: vec![left, right] }.normalize()
            }
        }
    }
}

/// Normalize a unary operation type by computing exact results where possible.
fn normalize_unary_operation(operator: UnaryOperator, operand: Type) -> Type {
    use Type::*;

    match operator {
        UnaryOperator::Not => match operand {
            ExactBoolean { value } => ExactBoolean { value: !value },
            Nil => ExactBoolean { value: true },
            _ => Boolean,
        },
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Unknown => write!(f, "unknown"),
            Type::Never => write!(f, "never"),
            Type::Nil => write!(f, "nil"),
            Type::Boolean => write!(f, "boolean"),
            Type::ExactBoolean { value } => write!(f, "{}", value),
            Type::Number => write!(f, "number"),
            Type::ExactNumber { value } => write!(f, "{}", value),
            Type::String => write!(f, "string"),
            Type::ExactString { value } => write!(f, "\"{}\"", value.as_str()),
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
            BooleanTypeExpression(_) => Type::Boolean,
            NumberTypeExpression(_) => Type::Number,
            StringTypeExpression(_) => Type::String,
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
            UnionTypeExpression(union) => {
                let variants = union
                    .variants
                    .into_iter()
                    .map(|variant| Type::from(variant.unpack()))
                    .collect();
                Type::Union { variants }
            }
        }
    }
}
