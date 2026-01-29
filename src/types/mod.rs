pub mod infer;

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
