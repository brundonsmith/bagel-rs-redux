use type_hierarchy::type_hierarchy;

use super::container::{Malformed, AST};
use super::slice::Slice;

// Terminal nodes (literals and identifiers)
// Note: The slice for each node is stored in ASTInner, not in these structs

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NilLiteral;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NumberLiteral;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PlainIdentifier;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalIdentifier {
    pub identifier: AST<PlainIdentifier>,
}

// Binary operation nodes

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryOperation {
    pub left: AST<Expression>,
    pub operator: AST<BinaryOperator>,
    pub right: AST<Expression>,
}

// Declaration node

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Declaration {
    pub const_keyword: Slice,
    pub identifier: AST<PlainIdentifier>,
    pub equals: Slice,
    pub value: AST<Expression>,
}

// Module node (represents a whole document)

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub declarations: Vec<AST<Declaration>>,
}

// Type hierarchy

type_hierarchy! {
    Any {
        Module,
        Declaration,
        Expression {
            NilLiteral,
            BooleanLiteral,
            NumberLiteral,
            BinaryOperation,
            LocalIdentifier,
        },
        PlainIdentifier,
        BinaryOperator,
        Malformed
    }
}
