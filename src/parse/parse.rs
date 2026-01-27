use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::char,
    combinator::{map, recognize},
    multi::{many0, many1},
    sequence::tuple,
};
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{
    container::{ASTInner, Parentable, AST},
    grammar::*,
    slice::Slice,
};

use super::utils::{expect_tag, w, ParseResult, RawParseError, RawParseErrorDetails};

// Reserved keywords that cannot be used as identifiers
const KEYWORDS: &[&str] = &["nil", "true", "false", "const"];

fn is_keyword(s: &str) -> bool {
    KEYWORDS.contains(&s)
}

// Helper to create AST nodes
fn make_ast<TKind>(slice: Slice, details: TKind) -> AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    AST::new(Rc::new(ASTInner {
        parent: RefCell::new(None),
        slice,
        details: details.clone().into(),
    }))
}

// Parser for PlainIdentifier: [a-z]+ (but not a keyword)
pub fn plain_identifier(i: Slice) -> ParseResult<AST<PlainIdentifier>> {
    let (remaining, matched) = take_while1(|c: char| c.is_ascii_lowercase())(i)?;

    // Reject reserved keywords
    if is_keyword(matched.as_str()) {
        return Err(nom::Err::Error(RawParseError {
            src: matched,
            details: RawParseErrorDetails::Expected("identifier (not a keyword)".to_string()),
        }));
    }

    let node = PlainIdentifier;
    Ok((remaining, make_ast(matched, node)))
}

// Parser for NilLiteral: "nil"
pub fn nil_literal(i: Slice) -> ParseResult<AST<NilLiteral>> {
    let (remaining, matched) = tag("nil")(i)?;
    let node = NilLiteral;
    Ok((remaining, make_ast(matched, node)))
}

// Parser for BooleanLiteral: "true" | "false"
pub fn boolean_literal(i: Slice) -> ParseResult<AST<BooleanLiteral>> {
    let (remaining, matched) = alt((tag("true"), tag("false")))(i)?;
    let value = matched.as_str() == "true";
    let node = BooleanLiteral { value };
    Ok((remaining, make_ast(matched, node)))
}

// Parser for NumberLiteral: [0-9]+(?:\.[0-9]+)?
pub fn number_literal(i: Slice) -> ParseResult<AST<NumberLiteral>> {
    let (remaining, matched) = recognize(tuple((
        take_while1(|c: char| c.is_ascii_digit()),
        nom::combinator::opt(tuple((
            char('.'),
            take_while1(|c: char| c.is_ascii_digit()),
        ))),
    )))(i)?;

    let node = NumberLiteral;
    Ok((remaining, make_ast(matched, node)))
}

// Parser for LocalIdentifier: PlainIdentifier (used as an expression)
pub fn local_identifier(i: Slice) -> ParseResult<AST<LocalIdentifier>> {
    let start = i.clone();
    let (remaining, mut identifier) = plain_identifier(i)?;

    let consumed_len = start.len() - remaining.len();
    let span = start.slice_range(0, Some(consumed_len));

    let local_id = LocalIdentifier {
        identifier: identifier.clone(),
    };

    let node = make_ast(span, local_id);
    identifier.set_parent(&node);

    Ok((remaining, node))
}


// Generic binary operation parser
// Takes a list of operators (tag strings and their corresponding enum values)
// and the next-precedence parser to use for operands
fn binary_operation<F>(
    i: Slice,
    operators: &[(&str, BinaryOperator)],
    next_parser: F,
) -> ParseResult<AST<Expression>>
where
    F: Fn(Slice) -> ParseResult<AST<Expression>>,
{
    // Parse the first operand
    let (remaining, first) = next_parser(i)?;

    // Parse zero or more (operator, operand) pairs
    let operator_parser = |input: Slice| {
        // Try to match any of the operators
        for (op_tag, op_enum) in operators {
            if let Ok((next_input, op_slice)) = w(tag(*op_tag))(input.clone()) {
                let operator = make_ast(op_slice, op_enum.clone());
                let (next_input, operand) = w(&next_parser)(next_input)?;
                return Ok((next_input, (operator, operand)));
            }
        }
        Err(nom::Err::Error(RawParseError {
            src: input,
            details: RawParseErrorDetails::Expected("operator".to_string()),
        }))
    };

    let (remaining, pairs) = many0(operator_parser)(remaining)?;

    // Fold the operators left-to-right
    let result = pairs.into_iter().fold(first, |mut left, (mut operator, mut right)| {
        let span = left.slice().spanning(right.slice());

        let bin_op = BinaryOperation {
            left: left.clone(),
            operator: operator.clone(),
            right: right.clone(),
        };

        let node = make_ast(span, bin_op);
        left.set_parent(&node);
        operator.set_parent(&node);
        right.set_parent(&node);

        node.upcast()
    });

    Ok((remaining, result))
}

// Parser for Expression (precedence-aware)
pub fn expression(i: Slice) -> ParseResult<AST<Expression>> {
    additive_expression(i)
}

// Additive expressions: + and -
fn additive_expression(i: Slice) -> ParseResult<AST<Expression>> {
    binary_operation(
        i,
        &[("+", BinaryOperator::Add), ("-", BinaryOperator::Subtract)],
        multiplicative_expression,
    )
}

// Multiplicative expressions: * and /
fn multiplicative_expression(i: Slice) -> ParseResult<AST<Expression>> {
    binary_operation(
        i,
        &[("*", BinaryOperator::Multiply), ("/", BinaryOperator::Divide)],
        primary_expression,
    )
}

// Primary expressions: literals and identifiers
fn primary_expression(i: Slice) -> ParseResult<AST<Expression>> {
    alt((
        map(nil_literal, |n| n.upcast()),
        map(boolean_literal, |n| n.upcast()),
        map(number_literal, |n| n.upcast()),
        map(local_identifier, |n| n.upcast()),
    ))(i)
}

// Parser for Declaration: "const" PlainIdentifier "=" Expression
pub fn declaration(i: Slice) -> ParseResult<AST<Declaration>> {
    let start = i.clone();
    let (remaining, const_keyword) = w(tag("const"))(i)?;
    let (remaining, mut identifier) = w(plain_identifier)(remaining)?;
    let (remaining, equals) = w(expect_tag("="))(remaining)?;
    let (remaining, mut value) = w(expression)(remaining)?;

    // Span from start of input to end of last parsed element
    let consumed_len = start.len() - remaining.len();
    let span = start.slice_range(0, Some(consumed_len));

    let decl = Declaration {
        const_keyword,
        identifier: identifier.clone(),
        equals,
        value: value.clone(),
    };

    let node = make_ast(span, decl);
    identifier.set_parent(&node);
    value.set_parent(&node);

    Ok((remaining, node))
}

// Parser for Module: Declaration+
pub fn module(i: Slice) -> ParseResult<AST<Module>> {
    let start = i.clone();

    // Parse one or more declarations
    let (remaining, mut declarations) = many1(w(declaration))(i)?;

    let consumed_len = start.len() - remaining.len();
    let span = start.slice_range(0, Some(consumed_len));

    let module_node = Module {
        declarations: declarations.clone(),
    };

    let node = make_ast(span, module_node);

    // Set parent for all declarations
    declarations.iter_mut().for_each(|decl| {
        decl.set_parent(&node);
    });

    Ok((remaining, node))
}

// Top-level parser for Any
pub fn any(i: Slice) -> ParseResult<AST<Any>> {
    alt((
        map(module, |m| m.upcast()),
        map(declaration, |d| d.upcast()),
        map(expression, |e| e.upcast()),
    ))(i)
}
