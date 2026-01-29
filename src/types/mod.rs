pub mod infer;

use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

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
        element: Rc<Type>,
    },
    Object {
        fields: BTreeMap<String, Type>,

        /// If true, object may have additional fields (row polymorphic)
        jopen: bool,
    },
    FuncType {
        args: Vec<Type>,
        args_spread: Option<Rc<Type>>,
        returns: Rc<Type>,
    },
    Union {
        variants: Vec<Type>,
    },
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
