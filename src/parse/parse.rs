use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::char,
    combinator::{map, recognize},
    multi::{many0, many1},
    sequence::{preceded, tuple},
};
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{
    container::{ASTInner, Parentable, AST},
    grammar::*,
    slice::Slice,
};

use super::utils::{expect_tag, w, whitespace, ParseResult, RawParseError, RawParseErrorDetails};

macro_rules! seq {
    ($( $s:expr ),* $(,)?) => {
        tuple(( $(preceded(whitespace, $s)),* ))
    };
}

macro_rules! binary_operation {
    ($input:expr, [ $( $op_enum:expr ),* $(,)? ], $next_parser:expr) => {{
        map(
            tuple((
                $next_parser,
                many0(alt((
                    $(
                        map(
                            seq!(tag($op_enum.as_str()), $next_parser),
                            move |(op_slice, operand)| {
                                let operator = make_ast(op_slice, $op_enum);
                                (operator, operand)
                            }
                        ),
                    )*
                )))
            )),
            |(first, pairs)| {
                // Fold the operators left-to-right
                pairs
                    .into_iter()
                    .fold(first, |mut left, (mut operator, mut right)| {
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
                    })
            }
        )($input)
    }};
}

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

// Parser for Expression (precedence-aware)
pub fn expression(i: Slice) -> ParseResult<AST<Expression>> {
    additive_expression(i)
}

// Additive expressions: + and -
fn additive_expression(i: Slice) -> ParseResult<AST<Expression>> {
    binary_operation!(
        i,
        [BinaryOperator::Add, BinaryOperator::Subtract],
        multiplicative_expression
    )
}

// Multiplicative expressions: * and /
fn multiplicative_expression(i: Slice) -> ParseResult<AST<Expression>> {
    binary_operation!(
        i,
        [BinaryOperator::Multiply, BinaryOperator::Divide],
        primary_expression
    )
}

// Parser for FunctionExpression: (?:"(" PlainIdentifier (?:"," PlainIdentifier)* ","? ")") or PlainIdentifier "=>" Expression
pub fn function_expression(i: Slice) -> ParseResult<AST<FunctionExpression>> {
    let start = i.clone();

    // Try parsing with parentheses first: (a, b, c) => expr
    if let Ok((remaining, open_paren)) = w(tag("("))(i.clone()) {
        // Parse parameter list
        let mut parameters = Vec::new();
        let mut commas = Vec::new();
        let mut current = remaining;
        let mut trailing_comma = None;

        // Parse first parameter if present (not immediately a close paren)
        if let Ok((after_param, param)) = w(plain_identifier)(current.clone()) {
            parameters.push(param.clone());
            current = after_param;

            // Parse subsequent ", param" pairs
            loop {
                if let Ok((after_comma, comma)) = w(tag(","))(current.clone()) {
                    // Check if there's another parameter or if this is a trailing comma
                    if let Ok((after_param, param)) = w(plain_identifier)(after_comma.clone()) {
                        commas.push(comma);
                        parameters.push(param.clone());
                        current = after_param;
                    } else {
                        // Trailing comma
                        trailing_comma = Some(comma);
                        current = after_comma;
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        // Expect closing paren, arrow, and body expression
        let (remaining, (close_paren, arrow, mut body)) =
            seq!(expect_tag(")"), expect_tag("=>"), expression)(current)?;

        let consumed_len = start.len() - remaining.len();
        let span = start.slice_range(0, Some(consumed_len));

        let func_expr = FunctionExpression {
            open_paren: Some(open_paren),
            parameters: parameters.clone(),
            commas,
            trailing_comma,
            close_paren: Some(close_paren),
            arrow,
            body: body.clone(),
        };

        let node = make_ast(span, func_expr);
        for param in &mut parameters {
            param.set_parent(&node);
        }
        body.set_parent(&node);

        return Ok((remaining, node));
    }

    // Try parsing without parentheses: x => expr
    let (remaining, (mut param, (arrow, mut body))) =
        tuple((plain_identifier, seq!(tag("=>"), expression)))(i)?;

    let consumed_len = start.len() - remaining.len();
    let span = start.slice_range(0, Some(consumed_len));

    let func_expr = FunctionExpression {
        open_paren: None,
        parameters: vec![param.clone()],
        commas: vec![],
        trailing_comma: None,
        close_paren: None,
        arrow,
        body: body.clone(),
    };

    let node = make_ast(span, func_expr);
    param.set_parent(&node);
    body.set_parent(&node);

    Ok((remaining, node))
}

// Parse invocation arguments: "(" Expression (?:"," Expression)* ","? ")"
// Returns (open_paren, arguments, commas, trailing_comma, close_paren)
fn parse_invocation_args(
    i: Slice,
) -> ParseResult<(
    Slice,
    Vec<AST<Expression>>,
    Vec<Slice>,
    Option<Slice>,
    Option<Slice>,
)> {
    let (remaining, open_paren) = tag("(")(i)?;

    let mut arguments = Vec::new();
    let mut commas = Vec::new();
    let mut current = remaining;
    let mut trailing_comma = None;

    // Parse first argument if present
    if let Ok((after_arg, arg)) = w(expression)(current.clone()) {
        arguments.push(arg);
        current = after_arg;

        // Parse subsequent ", arg" pairs
        loop {
            if let Ok((after_comma, comma)) = w(tag(","))(current.clone()) {
                // Check if there's another argument or if this is a trailing comma
                if let Ok((after_arg, arg)) = w(expression)(after_comma.clone()) {
                    commas.push(comma);
                    arguments.push(arg);
                    current = after_arg;
                } else {
                    // Trailing comma
                    trailing_comma = Some(comma);
                    current = after_comma;
                    break;
                }
            } else {
                break;
            }
        }
    }

    // Try to parse closing paren, recover on error
    let (remaining, close_paren) = match w(tag(")"))(current.clone()) {
        Ok((r, cp)) => (r, Some(cp)),
        Err(_) => {
            // Try to recover by finding matching close paren
            let mut depth = 1;
            let mut pos = 0;
            for (idx, ch) in current.as_str().char_indices() {
                match ch {
                    '(' => depth += 1,
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            pos = idx;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            if depth == 0 {
                // Found matching paren - skip to it
                let close_paren = current.clone().slice_range(pos, Some(pos + 1));
                let remaining = current.slice_range(pos + 1, None);
                (remaining, Some(close_paren))
            } else {
                // No matching paren found
                (current, None)
            }
        }
    };

    Ok((
        remaining,
        (open_paren, arguments, commas, trailing_comma, close_paren),
    ))
}

// Postfix expressions: primary followed by zero or more invocations
fn postfix_expression(i: Slice) -> ParseResult<AST<Expression>> {
    let start = i.clone();
    let (mut remaining, mut expr) = atom_expression(i)?;

    // Parse zero or more invocation suffixes
    loop {
        // Check if there's an opening paren (with optional whitespace)
        if let Ok((after_args, (open_paren, mut arguments, commas, trailing_comma, close_paren))) =
            w(parse_invocation_args)(remaining.clone())
        {
            let consumed_len = start.len() - after_args.len();
            let span = start.clone().slice_range(0, Some(consumed_len));

            let invocation = Invocation {
                function: expr.clone(),
                open_paren,
                arguments: arguments.clone(),
                commas,
                trailing_comma,
                close_paren,
            };

            let node = make_ast(span, invocation);
            expr.set_parent(&node);
            arguments.set_parent(&node);

            expr = node.upcast();
            remaining = after_args;
        } else {
            break;
        }
    }

    Ok((remaining, expr))
}

// Atom expressions: literals, identifiers, function expressions, and parenthesized expressions
fn atom_expression(i: Slice) -> ParseResult<AST<Expression>> {
    alt((
        map(nil_literal, |n| n.upcast()),
        map(boolean_literal, |n| n.upcast()),
        map(number_literal, |n| n.upcast()),
        map(function_expression, |n| n.upcast()),
        map(local_identifier, |n| n.upcast()),
    ))(i)
}

// Primary expressions: postfix expressions (which include atoms and invocations)
fn primary_expression(i: Slice) -> ParseResult<AST<Expression>> {
    postfix_expression(i)
}

// Parser for Declaration: "const" PlainIdentifier "=" Expression
pub fn declaration(i: Slice) -> ParseResult<AST<Declaration>> {
    let start = i.clone();
    let (remaining, (const_keyword, mut identifier, equals, mut value)) =
        seq!(tag("const"), plain_identifier, expect_tag("="), expression)(i)?;

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
