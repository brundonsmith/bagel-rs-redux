use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::char,
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    sequence::{preceded, tuple},
};
use std::sync::Arc;

use crate::ast::{
    container::{ASTInner, Parentable, AST},
    grammar::*,
    slice::Slice,
};

use super::utils::{backtrack, expect_tag, w, whitespace, ParseResult};

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
    use std::sync::RwLock;
    AST::new(Arc::new(ASTInner {
        parent: Arc::new(RwLock::new(None)),
        slice,
        details: details.clone().into(),
    }))
}

// Parser for PlainIdentifier: [a-z]+ (but not a keyword)
pub fn plain_identifier(i: Slice) -> ParseResult<AST<PlainIdentifier>> {
    map(
        nom::combinator::verify(
            take_while1(|c: char| c.is_ascii_lowercase()),
            |matched: &Slice| !is_keyword(matched.as_str()),
        ),
        |matched: Slice| make_ast(matched, PlainIdentifier),
    )(i)
}

// Parser for NilLiteral: "nil"
pub fn nil_literal(i: Slice) -> ParseResult<AST<NilLiteral>> {
    map(tag("nil"), |matched: Slice| make_ast(matched, NilLiteral))(i)
}

// Parser for BooleanLiteral: "true" | "false"
pub fn boolean_literal(i: Slice) -> ParseResult<AST<BooleanLiteral>> {
    map(alt((tag("true"), tag("false"))), |matched: Slice| {
        let value = matched.as_str() == "true";
        make_ast(matched, BooleanLiteral { value })
    })(i)
}

// Parser for NumberLiteral: [0-9]+(?:\.[0-9]+)?
pub fn number_literal(i: Slice) -> ParseResult<AST<NumberLiteral>> {
    map(
        recognize(tuple((
            take_while1(|c: char| c.is_ascii_digit()),
            opt(tuple((
                char('.'),
                take_while1(|c: char| c.is_ascii_digit()),
            ))),
        ))),
        |matched: Slice| make_ast(matched, NumberLiteral),
    )(i)
}

// Parser for StringLiteral: single-quoted string with escape sequences
pub fn string_literal(i: Slice) -> ParseResult<AST<StringLiteral>> {
    let (remaining, open_quote) = tag("'")(i)?;

    // Parse string contents until we hit a closing quote or end of input
    let mut chars = remaining.as_str().chars();
    let mut len = 0;
    let mut escaped = false;

    for ch in chars {
        if escaped {
            // Any character after backslash is consumed
            escaped = false;
            len += ch.len_utf8();
        } else if ch == '\\' {
            // Start escape sequence
            escaped = true;
            len += 1;
        } else if ch == '\'' {
            // Found closing quote
            break;
        } else {
            len += ch.len_utf8();
        }
    }

    let contents = remaining.clone().slice_range(0, Some(len));
    let after_contents = remaining.slice_range(len, None);

    // Try to parse closing quote with backtracking
    let (remaining, close_quote_opt) = backtrack(tag("'"), "'", "'")(after_contents)?;

    let close_quote = close_quote_opt.unwrap_or_else(|| {
        // If no closing quote, use zero-width slice at end
        remaining.clone().slice_range(0, Some(0))
    });

    let span = open_quote.spanning(&close_quote);
    let node = make_ast(
        span,
        StringLiteral {
            open_quote,
            contents,
            close_quote,
        },
    );

    Ok((remaining, node))
}

// Parser for LocalIdentifier: PlainIdentifier (used as an expression)
pub fn local_identifier(i: Slice) -> ParseResult<AST<LocalIdentifier>> {
    map(plain_identifier, |identifier: AST<PlainIdentifier>| {
        let node = make_ast(identifier.slice().clone(), LocalIdentifier { identifier });
        node.unpack().identifier.set_parent(&node);

        node
    })(i)
}

// Parser for Expression (precedence-aware, lowest to highest)
pub fn expression(i: Slice) -> ParseResult<AST<Expression>> {
    nullish_coalescing_expression(i)
}

// Nullish coalescing: ??
fn nullish_coalescing_expression(i: Slice) -> ParseResult<AST<Expression>> {
    binary_operation!(
        i,
        [BinaryOperator::NullishCoalescing],
        logical_or_expression
    )
}

// Logical OR: ||
fn logical_or_expression(i: Slice) -> ParseResult<AST<Expression>> {
    binary_operation!(i, [BinaryOperator::Or], logical_and_expression)
}

// Logical AND: &&
fn logical_and_expression(i: Slice) -> ParseResult<AST<Expression>> {
    binary_operation!(i, [BinaryOperator::And], equality_expression)
}

// Equality: == and !=
fn equality_expression(i: Slice) -> ParseResult<AST<Expression>> {
    binary_operation!(
        i,
        [BinaryOperator::Equal, BinaryOperator::NotEqual],
        additive_expression
    )
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
        unary_expression
    )
}

// Unary prefix expressions: !
fn unary_expression(i: Slice) -> ParseResult<AST<Expression>> {
    // Try unary ! first, then fall through to primary
    alt((
        map(
            seq!(tag("!"), unary_expression),
            |(op_slice, mut operand)| {
                let span = op_slice.spanning(operand.slice());
                let mut operator = make_ast(op_slice, UnaryOperator::Not);

                let unary_op = UnaryOperation {
                    operator: operator.clone(),
                    operand: operand.clone(),
                };

                let node: AST<Expression> = make_ast(span, unary_op).upcast();
                operator.set_parent(&node);
                operand.set_parent(&node);

                node
            },
        ),
        primary_expression,
    ))(i)
}

// Parser for FunctionExpression: (?:"(" Param (?:"," Param)* ","? ")") or PlainIdentifier "=>" Expression
// where Param = PlainIdentifier (":" TypeExpression)?
pub fn function_expression(i: Slice) -> ParseResult<AST<FunctionExpression>> {
    let start = i.clone();

    // Try parsing with parentheses first: (a: number, b: string) => expr
    if let Ok((remaining, open_paren)) = w(tag("("))(i.clone()) {
        // Parse parameter list
        let mut parameters: Vec<(AST<PlainIdentifier>, Option<(Slice, AST<TypeExpression>)>)> = Vec::new();
        let mut commas = Vec::new();
        let mut current = remaining;
        let mut trailing_comma = None;

        // Parse first parameter if present (not immediately a close paren)
        if let Ok((after_param, param)) = w(plain_identifier)(current.clone()) {
            let (after_type_ann, type_ann) = parse_optional_type_annotation(after_param)?;
            parameters.push((param.clone(), type_ann));
            current = after_type_ann;

            // Parse subsequent ", param" pairs
            loop {
                if let Ok((after_comma, comma)) = w(tag(","))(current.clone()) {
                    // Check if there's another parameter or if this is a trailing comma
                    if let Ok((after_param, param)) = w(plain_identifier)(after_comma.clone()) {
                        let (after_type_ann, type_ann) = parse_optional_type_annotation(after_param)?;
                        commas.push(comma);
                        parameters.push((param.clone(), type_ann));
                        current = after_type_ann;
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

        // Parse closing paren with backtracking, then arrow and body
        let (current, close_paren) = w(backtrack(tag(")"), ")", "("))(current)?;
        let (remaining, (arrow, mut body)) = seq!(expect_tag("=>"), expression)(current)?;

        let consumed_len = start.len() - remaining.len();
        let span = start.slice_range(0, Some(consumed_len));

        let func_expr = FunctionExpression {
            open_paren: Some(open_paren),
            parameters: parameters.clone(),
            commas,
            trailing_comma,
            close_paren,
            arrow,
            body: body.clone(),
        };

        let node = make_ast(span, func_expr);
        for (mut param, type_ann) in parameters {
            param.set_parent(&node);
            if let Some((_colon, mut type_expr)) = type_ann {
                type_expr.set_parent(&node);
            }
        }
        body.set_parent(&node);

        return Ok((remaining, node));
    }

    // Try parsing without parentheses: x => expr (no type annotation allowed here)
    let (remaining, (mut param, (arrow, mut body))) =
        tuple((plain_identifier, seq!(tag("=>"), expression)))(i)?;

    let consumed_len = start.len() - remaining.len();
    let span = start.slice_range(0, Some(consumed_len));

    let func_expr = FunctionExpression {
        open_paren: None,
        parameters: vec![(param.clone(), None)],
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

/// Parses an optional `: TypeExpression` type annotation.
fn parse_optional_type_annotation(i: Slice) -> ParseResult<Option<(Slice, AST<TypeExpression>)>> {
    opt(seq!(w(tag(":")), type_expression))(i)
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
    let (remaining, close_paren) = w(backtrack(tag(")"), ")", "("))(current)?;

    Ok((
        remaining,
        (open_paren, arguments, commas, trailing_comma, close_paren),
    ))
}

// Postfix expressions: primary followed by zero or more invocations
fn postfix_expression(i: Slice) -> ParseResult<AST<Expression>> {
    map(
        tuple((atom_expression, many0(w(parse_invocation_args)))),
        move |(first, invocations)| {
            invocations.into_iter().fold(
                first,
                |mut expr, (open_paren, mut arguments, commas, trailing_comma, close_paren)| {
                    let span = expr
                        .slice()
                        .spanning(close_paren.as_ref().unwrap_or(&open_paren));

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

                    node.upcast()
                },
            )
        },
    )(i)
}

// Atom expressions: literals, identifiers, function expressions, and parenthesized expressions
// Parser for ArrayLiteral: "[" Expression (?:"," Expression)* ","? "]"
fn array_literal(i: Slice) -> ParseResult<AST<ArrayLiteral>> {
    let (remaining, open_bracket) = tag("[")(i)?;

    let mut elements = Vec::new();
    let mut commas = Vec::new();
    let mut current = remaining;
    let mut trailing_comma = None;

    // Parse first element if present
    if let Ok((after_elem, elem)) = w(expression)(current.clone()) {
        elements.push(elem);
        current = after_elem;

        // Parse subsequent ", elem" pairs
        loop {
            if let Ok((after_comma, comma)) = w(tag(","))(current.clone()) {
                // Check if there's another element or if this is a trailing comma
                if let Ok((after_elem, elem)) = w(expression)(after_comma.clone()) {
                    commas.push(comma);
                    elements.push(elem);
                    current = after_elem;
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

    // Try to parse closing bracket with backtracking
    let (remaining, close_bracket) = w(backtrack(tag("]"), "]", "["))(current)?;

    let span = open_bracket.spanning(close_bracket.as_ref().unwrap_or(&open_bracket));

    let array_lit = ArrayLiteral {
        open_bracket,
        elements: elements.clone(),
        commas,
        trailing_comma,
        close_bracket,
    };

    let node = make_ast(span, array_lit);
    for elem in &mut elements {
        elem.set_parent(&node);
    }

    Ok((remaining, node))
}

// Parser for ObjectLiteral: "{" (PlainIdentifier ":" Expression (?:"," PlainIdentifier ":" Expression)*)? ","? "}"
fn object_literal(i: Slice) -> ParseResult<AST<ObjectLiteral>> {
    let (remaining, open_brace) = tag("{")(i)?;

    let mut fields = Vec::new();
    let mut commas = Vec::new();
    let mut current = remaining;
    let mut trailing_comma = None;

    // Parse first field if present
    if let Ok((after_key, key)) = w(plain_identifier)(current.clone()) {
        if let Ok((after_colon, colon)) = w(tag(":"))(after_key.clone()) {
            if let Ok((after_value, value)) = w(expression)(after_colon.clone()) {
                fields.push((key, colon, value));
                current = after_value;

                // Parse subsequent ", key: value" triples
                loop {
                    if let Ok((after_comma, comma)) = w(tag(","))(current.clone()) {
                        // Check if there's another field or if this is a trailing comma
                        if let Ok((after_key, key)) = w(plain_identifier)(after_comma.clone()) {
                            if let Ok((after_colon, colon)) = w(tag(":"))(after_key) {
                                if let Ok((after_value, value)) = w(expression)(after_colon) {
                                    commas.push(comma);
                                    fields.push((key, colon, value));
                                    current = after_value;
                                    continue;
                                }
                            }
                        }
                        // Trailing comma
                        trailing_comma = Some(comma);
                        current = after_comma;
                        break;
                    } else {
                        break;
                    }
                }
            }
        }
    }

    // Try to parse closing brace with backtracking
    let (remaining, close_brace) = w(backtrack(tag("}"), "}", "{"))(current)?;

    let span = open_brace.spanning(close_brace.as_ref().unwrap_or(&open_brace));

    let obj_lit = ObjectLiteral {
        open_brace,
        fields: fields.clone(),
        commas,
        trailing_comma,
        close_brace,
    };

    let node = make_ast(span, obj_lit);
    for (key, _, value) in &mut fields {
        key.set_parent(&node);
        value.set_parent(&node);
    }

    Ok((remaining, node))
}

fn atom_expression(i: Slice) -> ParseResult<AST<Expression>> {
    alt((
        map(nil_literal, |n| n.upcast()),
        map(boolean_literal, |n| n.upcast()),
        map(number_literal, |n| n.upcast()),
        map(string_literal, |n| n.upcast()),
        map(array_literal, |n| n.upcast()),
        map(object_literal, |n| n.upcast()),
        map(function_expression, |n| n.upcast()),
        map(local_identifier, |n| n.upcast()),
    ))(i)
}

// Primary expressions: postfix expressions (which include atoms and invocations)
fn primary_expression(i: Slice) -> ParseResult<AST<Expression>> {
    postfix_expression(i)
}

// Type expression parsers

fn unknown_type_expression(i: Slice) -> ParseResult<AST<UnknownTypeExpression>> {
    map(tag("unknown"), |matched: Slice| {
        make_ast(matched, UnknownTypeExpression)
    })(i)
}

fn nil_type_expression(i: Slice) -> ParseResult<AST<NilTypeExpression>> {
    map(tag("null"), |matched: Slice| {
        make_ast(matched, NilTypeExpression)
    })(i)
}

fn boolean_type_expression(i: Slice) -> ParseResult<AST<BooleanTypeExpression>> {
    map(tag("boolean"), |matched: Slice| {
        make_ast(matched, BooleanTypeExpression)
    })(i)
}

fn number_type_expression(i: Slice) -> ParseResult<AST<NumberTypeExpression>> {
    map(tag("number"), |matched: Slice| {
        make_ast(matched, NumberTypeExpression)
    })(i)
}

fn string_type_expression(i: Slice) -> ParseResult<AST<StringTypeExpression>> {
    map(tag("string"), |matched: Slice| {
        make_ast(matched, StringTypeExpression)
    })(i)
}

fn array_type_expression(i: Slice) -> ParseResult<AST<ArrayTypeExpression>> {
    let (remaining, mut element) = primary_type_expression(i)?;
    let (remaining, open_bracket) = w(tag("["))(remaining)?;
    let (remaining, close_bracket_opt) = w(backtrack(tag("]"), "]", "["))(remaining)?;

    let close_bracket = close_bracket_opt.unwrap_or_else(|| {
        // If no closing bracket found, use a zero-width slice at the end
        remaining.clone().slice_range(0, Some(0))
    });

    let span = element.slice().spanning(&close_bracket);
    let node = make_ast(
        span,
        ArrayTypeExpression {
            element: element.clone(),
            open_bracket,
            close_bracket,
        },
    );
    element.set_parent(&node);
    Ok((remaining, node))
}

fn primary_type_expression(i: Slice) -> ParseResult<AST<TypeExpression>> {
    alt((
        map(unknown_type_expression, |n| n.upcast()),
        map(nil_type_expression, |n| n.upcast()),
        map(boolean_type_expression, |n| n.upcast()),
        map(number_type_expression, |n| n.upcast()),
        map(string_type_expression, |n| n.upcast()),
    ))(i)
}

fn postfix_type_expression(i: Slice) -> ParseResult<AST<TypeExpression>> {
    alt((
        map(array_type_expression, |n| n.upcast()),
        primary_type_expression,
    ))(i)
}

pub fn type_expression(i: Slice) -> ParseResult<AST<TypeExpression>> {
    postfix_type_expression(i)
}

// Parser for Declaration: "const" PlainIdentifier (":" TypeExpression)? "=" Expression
pub fn declaration(i: Slice) -> ParseResult<AST<Declaration>> {
    map(
        seq!(
            tag("const"),
            plain_identifier,
            opt(seq!(tag(":"), type_expression)),
            expect_tag("="),
            expression
        ),
        |(const_keyword, mut identifier, type_annotation, equals, mut value)| {
            let span = const_keyword.spanning(value.slice());

            // let mut type_annotation_opt = type_annotation;

            let decl = Declaration {
                const_keyword,
                identifier: identifier.clone(),
                type_annotation: type_annotation.clone(),
                equals,
                value: value.clone(),
            };

            let node = make_ast(span, decl);
            identifier.set_parent(&node);
            if let Some((_, mut type_expr)) = type_annotation.clone() {
                type_expr.set_parent(&node);
            }
            value.set_parent(&node);

            node
        },
    )(i)
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
