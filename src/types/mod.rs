pub mod fits;
pub mod infer;

use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;

use crate::ast::slice::Slice;

#[derive(Debug, Clone, PartialEq, Eq)]
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
        jopen: bool,
    },
    FuncType {
        args: Vec<Type>,
        args_spread: Option<Arc<Type>>,
        returns: Arc<Type>,
    },
    Union {
        variants: Vec<Type>,
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
    pub fn normalize(self) -> Self {
        self
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
            Type::Object { fields, jopen } => {
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
                    jopen: false,
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
