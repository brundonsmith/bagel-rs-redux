use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::char,
    combinator::{map, recognize},
    sequence::tuple,
};
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{
    container::{ASTInner, Parentable, AST},
    grammar::*,
    slice::Slice,
};

use super::utils::{expect_tag, w, ParseResult};

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

// Parser for PlainIdentifier: [a-z]+
pub fn plain_identifier(i: Slice) -> ParseResult<AST<PlainIdentifier>> {
    let (remaining, matched) = take_while1(|c: char| c.is_ascii_lowercase())(i)?;
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

// Parser for plain identifier as an expression
pub fn plain_identifier_expr(i: Slice) -> ParseResult<AST<PlainIdentifier>> {
    plain_identifier(i)
}


// Parser for Expression (precedence-aware)
pub fn expression(i: Slice) -> ParseResult<AST<Expression>> {
    additive_expression(i)
}

// Additive expressions: + and -
fn additive_expression(i: Slice) -> ParseResult<AST<Expression>> {
    let (mut remaining, mut left) = multiplicative_expression(i)?;

    loop {
        let check = w(alt((tag("+"), tag("-"))))(remaining.clone());
        if let Ok((next_remaining, op_slice)) = check {
            let operator = if op_slice == "+" {
                BinaryOperator::Add
            } else {
                BinaryOperator::Subtract
            };

            let (next_remaining, mut right) = w(multiplicative_expression)(next_remaining)?;
            let span = left.slice().spanning(right.slice());

            let bin_op = BinaryOperation {
                left: left.clone(),
                operator,
                operator_slice: op_slice,
                right: right.clone(),
            };

            let node = make_ast(span, bin_op);
            left.set_parent(&node);
            right.set_parent(&node);

            left = node.upcast();
            remaining = next_remaining;
        } else {
            break;
        }
    }

    Ok((remaining, left))
}

// Multiplicative expressions: * and /
fn multiplicative_expression(i: Slice) -> ParseResult<AST<Expression>> {
    let (mut remaining, mut left) = primary_expression(i)?;

    loop {
        let check = w(alt((tag("*"), tag("/"))))(remaining.clone());
        if let Ok((next_remaining, op_slice)) = check {
            let operator = if op_slice == "*" {
                BinaryOperator::Multiply
            } else {
                BinaryOperator::Divide
            };

            let (next_remaining, mut right) = w(primary_expression)(next_remaining)?;
            let span = left.slice().spanning(right.slice());

            let bin_op = BinaryOperation {
                left: left.clone(),
                operator,
                operator_slice: op_slice,
                right: right.clone(),
            };

            let node = make_ast(span, bin_op);
            left.set_parent(&node);
            right.set_parent(&node);

            left = node.upcast();
            remaining = next_remaining;
        } else {
            break;
        }
    }

    Ok((remaining, left))
}

// Primary expressions: literals and identifiers
fn primary_expression(i: Slice) -> ParseResult<AST<Expression>> {
    alt((
        map(nil_literal, |n| n.upcast()),
        map(boolean_literal, |n| n.upcast()),
        map(number_literal, |n| n.upcast()),
        map(plain_identifier_expr, |n| n.upcast()),
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
    let mut declarations = Vec::new();
    let mut remaining = i;

    // Parse at least one declaration
    let (next_remaining, first_decl) = w(declaration)(remaining)?;
    declarations.push(first_decl);
    remaining = next_remaining;

    // Parse remaining declarations
    loop {
        // Try to parse another declaration
        match w(declaration)(remaining.clone()) {
            Ok((next_remaining, decl)) => {
                declarations.push(decl);
                remaining = next_remaining;
            }
            Err(_) => break,
        }
    }

    let consumed_len = start.len() - remaining.len();
    let span = start.slice_range(0, Some(consumed_len));

    let module_node = Module {
        declarations: declarations.clone(),
    };

    let node = make_ast(span, module_node);

    // Set parent for all declarations
    for decl in &mut declarations {
        decl.set_parent(&node);
    }

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
