use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;

use crate::ast::container::AST;
use crate::ast::grammar::{self, BinaryOperator, LocalIdentifier, TypeExpression, UnaryOperator};
use crate::ast::slice::Slice;

use super::infer::InferTypeContext;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Any,
    Unknown,
    Poisoned,
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
        identifier: AST<LocalIdentifier>,
    },
    NamedType {
        name: String,
        identifier: AST<grammar::PlainIdentifier>,
    },
}

impl Type {
    pub const ANY_NUMBER: Type = Type::Number {
        min_value: None,
        max_value: None,
    };
    pub const ANY_STRING: Type = Type::String { value: None };
    pub const ANY_BOOLEAN: Type = Type::Boolean { value: None };
    pub const TRUE: Type = Type::Boolean { value: Some(true) };
    pub const FALSE: Type = Type::Boolean { value: Some(false) };
}

impl From<bool> for Type {
    fn from(value: bool) -> Self {
        Self::Boolean { value: Some(value) }
    }
}

impl BinaryOperator {
    /// Returns the type that operands of this operator must fit, or `None` if
    /// any type is allowed (e.g. `==`, `!=`, `??`).
    pub fn allowed_operand_type(&self) -> Option<Type> {
        match self {
            BinaryOperator::Add => Some(Type::Union {
                variants: vec![Type::ANY_NUMBER, Type::ANY_STRING],
            }),
            BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanOrEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanOrEqual => Some(Type::ANY_NUMBER),
            BinaryOperator::And | BinaryOperator::Or => Some(Type::Union {
                variants: vec![Type::ANY_BOOLEAN, Type::Nil],
            }),
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::NullishCoalescing => None,
        }
    }
}

impl UnaryOperator {
    /// Returns the type that the operand of this operator must fit.
    pub fn allowed_operand_type(&self) -> Type {
        match self {
            UnaryOperator::Not => Type::Union {
                variants: vec![Type::ANY_BOOLEAN, Type::Nil],
            },
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Unknown => write!(f, "unknown"),
            Type::Poisoned => write!(f, "unknown"),
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
            Type::LocalIdentifier { identifier } => match identifier.unpack() {
                Some(id) => write!(f, "{}", id.slice.as_str()),
                None => write!(f, "<unknown>"),
            },
            Type::NamedType { name, .. } => write!(f, "{}", name),
        }
    }
}

impl From<TypeExpression> for Type {
    fn from(type_expr: TypeExpression) -> Self {
        use TypeExpression::*;

        match type_expr {
            UnknownTypeExpression(_) => Type::Unknown,
            NilTypeExpression(_) => Type::Nil,
            BooleanTypeExpression(_) => Type::ANY_BOOLEAN,
            NumberTypeExpression(_) => Type::ANY_NUMBER,
            StringTypeExpression(_) => Type::ANY_STRING,
            TupleTypeExpression(tuple) => {
                let elements = tuple
                    .elements
                    .into_iter()
                    .map(|elem| elem.unpack().map(Type::from).unwrap_or(Type::Poisoned))
                    .collect();
                Type::Tuple { elements }
            }
            ArrayTypeExpression(array) => {
                let element = Arc::new(
                    array
                        .element
                        .unpack()
                        .map(Type::from)
                        .unwrap_or(Type::Poisoned),
                );
                Type::Array { element }
            }
            ObjectTypeExpression(obj) => {
                let fields = obj
                    .fields
                    .into_iter()
                    .map(|(name, _colon, type_expr)| {
                        let field_name = name.slice().as_str().to_string();
                        let field_type =
                            type_expr.unpack().map(Type::from).unwrap_or(Type::Poisoned);
                        (field_name, field_type)
                    })
                    .collect();
                Type::Object { fields }
            }
            FunctionTypeExpression(func) => {
                let args = func
                    .parameters
                    .into_iter()
                    .map(|(_name_colon, type_expr)| {
                        type_expr.unpack().map(Type::from).unwrap_or(Type::Poisoned)
                    })
                    .collect();
                let returns = Arc::new(
                    func.return_type
                        .unpack()
                        .map(Type::from)
                        .unwrap_or(Type::Poisoned),
                );
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
                    .map(|variant| variant.unpack().map(Type::from).unwrap_or(Type::Poisoned))
                    .collect();
                Type::Union { variants }
            }
            NillableTypeExpression(nillable) => {
                let subject = nillable
                    .subject
                    .unpack()
                    .map(Type::from)
                    .unwrap_or(Type::Poisoned);
                Type::Union {
                    variants: vec![subject, Type::Nil],
                }
            }
            ParenthesizedTypeExpression(paren) => paren
                .expression
                .unpack()
                .map(Type::from)
                .unwrap_or(Type::Poisoned),
            TypeOfTypeExpression(type_of) => {
                let ctx = InferTypeContext {
                    modules: None,
                    current_module: None,
                };
                type_of.expression.infer_type(ctx)
            }
            NamedTypeExpression(named) => {
                let name = named.identifier.slice().as_str().to_string();
                Type::NamedType {
                    name,
                    identifier: named.identifier,
                }
            }
        }
    }
}
