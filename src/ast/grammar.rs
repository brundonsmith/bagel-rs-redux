//! AST node definitions for the Bagel language grammar.
//!
//! This module contains the AST data structures. Each grammar rule gets its own
//! struct or enum, and the `type_hierarchy!` macro generates the enum hierarchy
//! with `From` and `TryFrom` implementations.

use type_hierarchy::type_hierarchy;

use super::container::AST;
use super::slice::Slice;

/// Terminal nodes (literals and identifiers)
/// Note: The slice for each node is stored in ASTInner, not in these structs
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NilLiteral;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NumberLiteral;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringLiteral {
    pub open_quote: Slice,
    pub contents: Slice,
    pub close_quote: Slice,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlainIdentifier;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalIdentifier {
    pub identifier: AST<PlainIdentifier>,
}

/// Binary operation nodes
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    NullishCoalescing,
    Or,
    And,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl BinaryOperator {
    /// Returns the string representation of this operator
    pub const fn as_str(&self) -> &'static str {
        match self {
            BinaryOperator::NullishCoalescing => "??",
            BinaryOperator::Or => "||",
            BinaryOperator::And => "&&",
            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanOrEqual => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanOrEqual => ">=",
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
        }
    }

    /// Attempts to parse a string into a BinaryOperator
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "??" => Some(BinaryOperator::NullishCoalescing),
            "||" => Some(BinaryOperator::Or),
            "&&" => Some(BinaryOperator::And),
            "==" => Some(BinaryOperator::Equal),
            "!=" => Some(BinaryOperator::NotEqual),
            "<" => Some(BinaryOperator::LessThan),
            "<=" => Some(BinaryOperator::LessThanOrEqual),
            ">" => Some(BinaryOperator::GreaterThan),
            ">=" => Some(BinaryOperator::GreaterThanOrEqual),
            "+" => Some(BinaryOperator::Add),
            "-" => Some(BinaryOperator::Subtract),
            "*" => Some(BinaryOperator::Multiply),
            "/" => Some(BinaryOperator::Divide),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOperator {
    Not,
}

impl UnaryOperator {
    pub const fn as_str(&self) -> &'static str {
        match self {
            UnaryOperator::Not => "!",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnaryOperation {
    pub operator: AST<UnaryOperator>,
    pub operand: AST<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryOperation {
    pub left: AST<Expression>,
    pub operator: AST<BinaryOperator>,
    pub right: AST<Expression>,
}

/// Invocation node: Expression "(" Expression (?:"," Expression)* ","? ")"
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Invocation {
    pub function: AST<Expression>,
    pub open_paren: Slice,
    pub arguments: Vec<AST<Expression>>,
    pub commas: Vec<Slice>,
    pub trailing_comma: Option<Slice>,
    pub close_paren: Option<Slice>,
}

/// FunctionExpression node: (?:"(" Param (?:"," Param)* ","? ")") or PlainIdentifier "=>" Expression
/// where Param = PlainIdentifier (":" TypeExpression)?
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionExpression {
    pub open_paren: Option<Slice>,
    pub parameters: Vec<(AST<PlainIdentifier>, Option<(Slice, AST<TypeExpression>)>)>,
    pub commas: Vec<Slice>,
    pub trailing_comma: Option<Slice>,
    pub close_paren: Option<Slice>,
    pub arrow: Slice,
    pub body: AST<FunctionBody>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctionBody {
    Expression(AST<Expression>),
    Block(AST<Block>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<AST<Statement>>,
}

/// ArrayLiteral node: "[" Expression (?:"," Expression)* ","? "]"
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayLiteral {
    pub open_bracket: Slice,
    pub elements: Vec<AST<Expression>>,
    pub commas: Vec<Slice>,
    pub trailing_comma: Option<Slice>,
    pub close_bracket: Option<Slice>,
}

/// IfElseExpression node: "if" Expression "{" Expression "}" ("else" ("{" Expression "}" | IfElseExpression))?
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfElseExpression {
    pub if_keyword: Slice,
    pub condition: AST<Expression>,
    pub open_brace: Slice,
    pub consequent: AST<Expression>,
    pub close_brace: Slice,
    pub else_clause: Option<ElseClause>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ElseClause {
    ElseBlock {
        else_keyword: Slice,
        open_brace: Slice,
        expression: AST<Expression>,
        close_brace: Slice,
    },
    ElseIf {
        else_keyword: Slice,
        if_else: AST<IfElseExpression>,
    },
}

/// ObjectLiteral node: "{" (PlainIdentifier ":" Expression (?:"," PlainIdentifier ":" Expression)*)? ","? "}"
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectLiteral {
    pub open_brace: Slice,
    pub fields: Vec<(AST<PlainIdentifier>, Slice, AST<Expression>)>, // (key, colon, value)
    pub commas: Vec<Slice>,
    pub trailing_comma: Option<Slice>,
    pub close_brace: Option<Slice>,
}

// Type expression nodes
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnknownTypeExpression;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NilTypeExpression;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanTypeExpression;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NumberTypeExpression;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringTypeExpression;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleTypeExpression {
    pub open_bracket: Slice,
    pub elements: Vec<AST<TypeExpression>>,
    pub commas: Vec<Slice>,
    pub trailing_comma: Option<Slice>,
    pub close_bracket: Slice,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayTypeExpression {
    pub element: AST<TypeExpression>,
    pub open_bracket: Slice,
    pub close_bracket: Slice,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectTypeExpression {
    pub open_brace: Slice,
    pub fields: Vec<(AST<PlainIdentifier>, Slice, AST<TypeExpression>)>, // (name, colon, type)
    pub commas: Vec<Slice>,
    pub trailing_comma: Option<Slice>,
    pub close_brace: Slice,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTypeExpression {
    pub open_paren: Slice,
    pub parameters: Vec<(AST<PlainIdentifier>, Slice, AST<TypeExpression>)>, // (name, colon, type)
    pub commas: Vec<Slice>,
    pub trailing_comma: Option<Slice>,
    pub close_paren: Slice,
    pub arrow: Slice,
    pub return_type: AST<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RangeTypeExpression {
    pub start: Option<Slice>,
    pub dots: Slice,
    pub end: Option<Slice>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnionTypeExpression {
    pub variants: Vec<AST<TypeExpression>>,
    pub pipes: Vec<Slice>,
}

/// Declaration node
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstDeclaration {
    pub const_keyword: Slice,
    pub identifier: AST<PlainIdentifier>,
    pub type_annotation: Option<(Slice, AST<TypeExpression>)>, // (colon, type)
    pub equals: Slice,
    pub value: AST<Expression>,
}

/// Module node (represents a whole document)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub declarations: Vec<AST<Declaration>>,
}

// Type hierarchy
//
// See the `type_hierarchy!` macro documentation in type_hierarchy/src/lib.rs for details
// on how this generates enums with From/TryFrom implementations.
type_hierarchy! {
    Any {
        Module,
        Declaration {
            ConstDeclaration
        }
        Expression {
            NilLiteral,
            BooleanLiteral,
            NumberLiteral,
            StringLiteral,
            BinaryOperation,
            UnaryOperation,
            LocalIdentifier,
            Invocation,
            FunctionExpression,
            ArrayLiteral,
            ObjectLiteral,
            IfElseExpression,
        },
        TypeExpression {
            UnknownTypeExpression,
            NilTypeExpression,
            BooleanTypeExpression,
            NumberTypeExpression,
            StringTypeExpression,
            TupleTypeExpression,
            ArrayTypeExpression,
            ObjectTypeExpression,
            FunctionTypeExpression,
            RangeTypeExpression,
            UnionTypeExpression,
        },
        Statement {
            Invocation,
            Block
        },
        PlainIdentifier,
        BinaryOperator,
        UnaryOperator,
        FunctionBody
    }
}
