use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::char,
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    sequence::{preceded, tuple},
};
use std::sync::Arc;

use crate::{
    ast::{
        container::{ASTInner, Parentable, AST},
        grammar::*,
        slice::Slice,
    },
    check::{BagelError, BagelErrorDetails},
    config::RuleSeverity,
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
const KEYWORDS: &[&str] = &[
    "nil", "true", "false", "const", "export", "from", "import", "as", "if", "else", "typeof",
];

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

/// Consumes characters from `input` up to (but not including) the first
/// occurrence of any of `stop_chars` at nesting depth 0. `open_chars` and
/// `close_chars` define bracket pairs that affect nesting depth. Returns
/// `None` if no content was consumed (the stop char is immediately next).
fn consume_malformed_until(
    input: &Slice,
    stop_chars: &[&str],
    open_chars: &[&str],
    close_chars: &[&str],
) -> Option<(Slice, Slice)> {
    let text = input.as_str();
    let trimmed_start = text.len() - text.trim_start().len();
    let mut depth = 0i32;
    let mut end_pos = None;

    for (idx, _) in text.trim_start().char_indices() {
        let abs_idx = trimmed_start + idx;
        let remaining = &text[abs_idx..];

        if depth == 0 && stop_chars.iter().any(|s| remaining.starts_with(s)) {
            end_pos = Some(abs_idx);
            break;
        }

        if open_chars.iter().any(|s| remaining.starts_with(s)) {
            depth += 1;
        } else if close_chars.iter().any(|s| remaining.starts_with(s)) {
            depth -= 1;
        }
    }

    let end = end_pos?;
    if end == 0 {
        return None;
    }

    let malformed_slice = input.clone().slice_range(0, Some(end));
    let rest = input.clone().slice_range(end, None);
    Some((malformed_slice, rest))
}

/// Helper to create a Malformed AST node covering a slice of unparseable source.
fn make_malformed<TKind>(slice: Slice, message: &str) -> AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    AST::Malformed(slice, message.to_string())
}

// Shared parser for identifier slices: [a-z]+ (but not a keyword)
fn identifier_slice(i: Slice) -> ParseResult<Slice> {
    nom::combinator::verify(
        take_while1(|c: char| c.is_ascii_lowercase()),
        |matched: &Slice| !is_keyword(matched.as_str()),
    )(i)
}

// Parser for PlainIdentifier: [a-z]+ (but not a keyword)
pub fn plain_identifier(i: Slice) -> ParseResult<AST<PlainIdentifier>> {
    map(identifier_slice, |matched: Slice| {
        make_ast(matched.clone(), PlainIdentifier { slice: matched })
    })(i)
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
        |matched: Slice| make_ast(matched.clone(), NumberLiteral { slice: matched }),
    )(i)
}

// Parser for StringLiteral: single-quoted string with escape sequences
pub fn string_literal(i: Slice) -> ParseResult<AST<StringLiteral>> {
    let (remaining, open_quote) = tag("'")(i)?;

    // Parse string contents until we hit a closing quote or end of input
    let chars = remaining.as_str().chars();
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
    map(identifier_slice, |matched: Slice| {
        make_ast(matched.clone(), LocalIdentifier { slice: matched })
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
        relational_expression
    )
}

// Relational: <, <=, >, >=
// Note: parse >= and <= before > and < so the two-char operators match first
fn relational_expression(i: Slice) -> ParseResult<AST<Expression>> {
    binary_operation!(
        i,
        [
            BinaryOperator::LessThanOrEqual,
            BinaryOperator::GreaterThanOrEqual,
            BinaryOperator::LessThan,
            BinaryOperator::GreaterThan,
        ],
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

    // Try parsing with parentheses: (a: number, b: string) => expr
    // The entire path is soft — if "=>" isn't found after ")", we fall through
    // to the non-parenthesized form below (and then to parenthesized_expression
    // via alt() in atom_expression).
    let parenthesized_result = w(tag("("))(i.clone())
        .ok()
        .and_then(|(remaining, open_paren)| {
            let mut parameters: Vec<(AST<PlainIdentifier>, Option<(Slice, AST<TypeExpression>)>)> =
                Vec::new();
            let mut commas = Vec::new();
            let mut current = remaining;
            let mut trailing_comma = None;

            // Parse first parameter if present
            if let Ok((after_param, param)) = w(plain_identifier)(current.clone()) {
                if let Ok((after_type_ann, type_ann)) = parse_optional_type_annotation(after_param)
                {
                    parameters.push((param.clone(), type_ann));
                    current = after_type_ann;

                    // Parse subsequent ", param" pairs
                    while let Ok((after_comma, comma)) = w(tag(","))(current.clone()) {
                        match w(plain_identifier)(after_comma.clone()) {
                            Ok((after_param, param)) => {
                                match parse_optional_type_annotation(after_param) {
                                    Ok((after_type_ann, type_ann)) => {
                                        commas.push(comma);
                                        parameters.push((param.clone(), type_ann));
                                        current = after_type_ann;
                                    }
                                    Err(_) => {
                                        trailing_comma = Some(comma);
                                        current = after_comma;
                                    }
                                }
                            }
                            Err(_) => {
                                trailing_comma = Some(comma);
                                current = after_comma;
                            }
                        }
                    }
                }
            }

            // Parse closing paren, optional return type, then arrow and body
            let (current, close_paren) = w(backtrack(tag(")"), ")", "("))(current).ok()?;
            let close_paren = close_paren?;
            let (current, return_type) = parse_optional_type_annotation(current).ok()?;
            let (remaining, (arrow, body)) = seq!(w(tag("=>")), function_body)(current).ok()?;

            Some((
                remaining,
                open_paren,
                parameters,
                commas,
                trailing_comma,
                close_paren,
                return_type,
                arrow,
                body,
            ))
        });

    if let Some((
        remaining,
        open_paren,
        parameters,
        commas,
        trailing_comma,
        close_paren,
        return_type,
        arrow,
        mut body,
    )) = parenthesized_result
    {
        let consumed_len = start.len() - remaining.len();
        let span = start.slice_range(0, Some(consumed_len));

        let func_expr = FunctionExpression {
            open_paren: Some(open_paren),
            parameters: parameters.clone(),
            commas,
            trailing_comma,
            close_paren: Some(close_paren),
            return_type: return_type.clone(),
            arrow,
            body: body.clone(),
        };

        let node = make_ast(span, func_expr);
        parameters.into_iter().for_each(|(mut param, type_ann)| {
            param.set_parent(&node);
            if let Some((_colon, mut type_expr)) = type_ann {
                type_expr.set_parent(&node);
            }
        });
        if let Some((_colon, mut ret_type)) = return_type {
            ret_type.set_parent(&node);
        }
        body.set_parent(&node);

        Ok((remaining, node))
    } else {
        // Try parsing without parentheses: x => expr
        let (remaining, (mut param, (arrow, mut body))) =
            tuple((plain_identifier, seq!(tag("=>"), function_body)))(i)?;

        let consumed_len = start.len() - remaining.len();
        let span = start.slice_range(0, Some(consumed_len));

        let func_expr = FunctionExpression {
            open_paren: None,
            parameters: vec![(param.clone(), None)],
            commas: vec![],
            trailing_comma: None,
            close_paren: None,
            return_type: None,
            arrow,
            body: body.clone(),
        };

        let node = make_ast(span, func_expr);
        param.set_parent(&node);
        body.set_parent(&node);

        Ok((remaining, node))
    }
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

/// Postfix suffix: either an invocation `(args)` or a property access `.property`
enum PostfixSuffix {
    Invocation(
        Slice,
        Vec<AST<Expression>>,
        Vec<Slice>,
        Option<Slice>,
        Option<Slice>,
    ),
    PropertyAccess(Slice, AST<PlainIdentifier>),
}

// Primary expressions: atom followed by zero or more postfix suffixes (.property or (args))
fn postfix_expression(i: Slice) -> ParseResult<AST<Expression>> {
    let postfix_suffix = alt((
        map(
            w(parse_invocation_args),
            |(open_paren, args, commas, trailing_comma, close_paren)| {
                PostfixSuffix::Invocation(open_paren, args, commas, trailing_comma, close_paren)
            },
        ),
        |i: Slice| {
            let (after_dot, dot) = preceded(whitespace, tag("."))(i)?;
            let prop = match preceded(whitespace, plain_identifier)(after_dot.clone()) {
                Ok((remaining, prop)) => (remaining, prop),
                Err(nom::Err::Error(_)) => {
                    // Dot with no valid identifier: create a zero-width malformed node
                    let empty_slice = after_dot.clone().slice_range(0, Some(0));
                    (after_dot, make_malformed(empty_slice, "identifier"))
                }
                Err(e) => return Err(e),
            };
            Ok((prop.0, PostfixSuffix::PropertyAccess(dot, prop.1)))
        },
    ));

    map(
        tuple((atom_expression, many0(postfix_suffix))),
        |(first, suffixes)| {
            suffixes
                .into_iter()
                .fold(first, |mut current_expr, suffix| match suffix {
                    PostfixSuffix::Invocation(
                        open_paren,
                        mut arguments,
                        commas,
                        trailing_comma,
                        close_paren,
                    ) => {
                        let span = current_expr
                            .slice()
                            .spanning(close_paren.as_ref().unwrap_or(&open_paren));

                        let inv = Invocation {
                            function: current_expr.clone(),
                            open_paren,
                            arguments: arguments.clone(),
                            commas,
                            trailing_comma,
                            close_paren,
                        };

                        let node = make_ast(span, inv);
                        current_expr.set_parent(&node);
                        arguments.set_parent(&node);

                        node.upcast()
                    }
                    PostfixSuffix::PropertyAccess(dot, mut prop) => {
                        let span = current_expr.slice().spanning(prop.slice());

                        let prop_access = PropertyAccessExpression {
                            subject: current_expr.clone(),
                            dot,
                            property: prop.clone(),
                        };

                        let node = make_ast(span, prop_access);
                        current_expr.set_parent(&node);
                        prop.set_parent(&node);

                        node.upcast()
                    }
                })
        },
    )(i)
}

// Atom expressions: literals, identifiers, function expressions, and parenthesized expressions
// Parser for ArrayLiteral: "[" Expression (?:"," Expression)* ","? "]"
fn array_literal(i: Slice) -> ParseResult<AST<ArrayLiteral>> {
    let (remaining, open_bracket) = tag("[")(i)?;

    let mut elements = Vec::new();
    let mut commas = Vec::new();
    let mut current = remaining.clone();
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

    // Check if there's unparseable content before ]
    // If so, replace all parsed elements with a single Malformed node
    // covering everything from after [ to before ]
    let has_junk = !current.as_str().trim_start().starts_with(']')
        && !current.as_str().trim_start().is_empty();

    if has_junk {
        // Find the ] using consume_malformed_until on the ORIGINAL remaining (after [)
        if let Some((malformed_slice, rest)) =
            consume_malformed_until(&remaining, &["]"], &["["], &["]"])
        {
            elements = vec![make_malformed(malformed_slice, "Expected expression")];
            commas = vec![];
            trailing_comma = None;
            current = rest;
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

// Parser for IfElseExpression: "if" Expression "{" Expression "}" ("else" ("{" Expression "}" | IfElseExpression))?
fn if_else_expression(i: Slice) -> ParseResult<AST<IfElseExpression>> {
    let start = i.clone();

    let (remaining, (if_keyword, mut condition, open_brace, mut consequent, close_brace)) =
        seq!(
            tag("if"),
            expression,
            expect_tag("{"),
            expression,
            expect_tag("}")
        )(i)?;

    // Try to parse else clause
    let (remaining, else_clause) = match w(tag("else"))(remaining.clone()) {
        Ok((after_else, else_keyword)) => {
            // Try else-if first, then fall back to else block
            match w(if_else_expression)(after_else.clone()) {
                Ok((after_else_if, if_else)) => {
                    let clause = ElseClause::ElseIf {
                        else_keyword,
                        if_else,
                    };
                    (after_else_if, Some(clause))
                }
                Err(_) => {
                    // Parse else { expression }
                    let (after_block, (else_open_brace, else_expr, else_close_brace)) =
                        seq!(expect_tag("{"), expression, expect_tag("}"))(after_else)?;

                    let clause = ElseClause::ElseBlock {
                        else_keyword,
                        open_brace: else_open_brace,
                        expression: else_expr,
                        close_brace: else_close_brace,
                    };
                    (after_block, Some(clause))
                }
            }
        }
        Err(_) => (remaining, None),
    };

    let consumed_len = start.len() - remaining.len();
    let span = start.slice_range(0, Some(consumed_len));

    let if_else_expr = IfElseExpression {
        if_keyword,
        condition: condition.clone(),
        open_brace,
        consequent: consequent.clone(),
        close_brace,
        else_clause,
    };

    let node = make_ast(span, if_else_expr);
    condition.set_parent(&node);
    consequent.set_parent(&node);

    // Set parents on AST children within the else clause
    match &node.unpack().unwrap().else_clause {
        Some(ElseClause::ElseBlock { expression, .. }) => {
            expression.clone().set_parent(&node);
        }
        Some(ElseClause::ElseIf { if_else, .. }) => {
            if_else.clone().set_parent(&node);
        }
        None => {}
    }

    Ok((remaining, node))
}

// Parser for a statement: any expression is accepted so that the AST preserves
// structure for LSP features like autocomplete. Non-invocation expressions
// are rejected later during type checking.
fn statement(i: Slice) -> ParseResult<AST<Statement>> {
    map(postfix_expression, |expr| expr.upcast::<Statement>())(i)
}

// Parser for a block: "{" statement* "}"
fn block(i: Slice) -> ParseResult<AST<Block>> {
    let (remaining, open_brace) = tag("{")(i)?;

    // Custom loop instead of many0 to catch both Errors and Failures
    let mut statements = Vec::new();
    let mut current = remaining;
    loop {
        match w(statement)(current.clone()) {
            Ok((rest, stmt)) => {
                statements.push(stmt);
                current = rest;
            }
            Err(_) => break,
        }
    }

    // If there's unparseable content before }, consume it as a malformed statement
    let current = match consume_malformed_until(&current, &["}"], &["{"], &["}"]) {
        Some((malformed_slice, rest)) if !malformed_slice.as_str().trim().is_empty() => {
            statements.push(make_malformed(malformed_slice, "Expected statement"));
            rest
        }
        _ => current,
    };

    let (remaining, close_brace) = w(expect_tag("}"))(current)?;

    let span = open_brace.spanning(&close_brace);
    let node = make_ast(
        span,
        Block {
            statements: statements.clone(),
        },
    );
    statements.set_parent(&node);

    Ok((remaining, node))
}

// Parser for a function body: block or expression
fn function_body(i: Slice) -> ParseResult<AST<FunctionBody>> {
    alt((
        map(block, |mut block_ast| {
            let span = block_ast.slice().clone();
            let body = make_ast(span, FunctionBody::Block(block_ast.clone()));
            block_ast.set_parent(&body);
            body
        }),
        map(expression, |mut expr_body| {
            let body = make_ast(
                expr_body.slice().clone(),
                FunctionBody::Expression(expr_body.clone()),
            );
            expr_body.set_parent(&body);
            body
        }),
    ))(i)
}

fn parenthesized_expression(i: Slice) -> ParseResult<AST<ParenthesizedExpression>> {
    let (remaining, (open_paren, mut inner, close_paren)) =
        seq!(tag("("), expression, expect_tag(")"))(i)?;

    let span = open_paren.spanning(&close_paren);
    let node = make_ast(
        span,
        ParenthesizedExpression {
            open_paren,
            expression: inner.clone(),
            close_paren,
        },
    );
    inner.set_parent(&node);

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
        map(if_else_expression, |n| n.upcast()),
        map(function_expression, |n| n.upcast()),
        map(parenthesized_expression, |n| n.upcast()),
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
    map(tag("nil"), |matched: Slice| {
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

// Parser for RangeTypeExpression: integer? ".." integer?  (at least one integer required)
// where integer = "-"? [0-9]+
fn range_type_expression(i: Slice) -> ParseResult<AST<RangeTypeExpression>> {
    let start_pos = i.clone();

    // Try to parse optional start integer
    let (remaining, start) = opt(recognize(tuple((
        opt(char('-')),
        take_while1(|c: char| c.is_ascii_digit()),
    ))))(i)?;

    // Parse ".."
    let (remaining, dots) = tag("..")(remaining)?;

    // Try to parse optional end integer
    let (remaining, end) = opt(recognize(tuple((
        opt(char('-')),
        take_while1(|c: char| c.is_ascii_digit()),
    ))))(remaining)?;

    // At least one side must be present
    if start.is_none() && end.is_none() {
        return Err(nom::Err::Error(BagelError {
            src: start_pos,
            severity: RuleSeverity::Error,
            details: BagelErrorDetails::ParseError {
                message: "Range type requires at least one bound".to_string(),
            },
            related: vec![],
        }));
    }

    let consumed_len = start_pos.len() - remaining.len();
    let span = start_pos.slice_range(0, Some(consumed_len));

    let node = make_ast(span, RangeTypeExpression { start, dots, end });

    Ok((remaining, node))
}

/// Parse a single function type parameter: either "name: Type" or just "Type"
fn function_type_param(
    i: Slice,
) -> ParseResult<(Option<(AST<PlainIdentifier>, Slice)>, AST<TypeExpression>)> {
    // Try "name: Type" first
    if let Ok((after_name, name)) = w(plain_identifier)(i.clone()) {
        if let Ok((after_colon, colon)) = w(tag(":"))(after_name) {
            if let Ok((after_type, type_expr)) = w(type_expression)(after_colon) {
                return Ok((after_type, (Some((name, colon)), type_expr)));
            }
        }
    }
    // Fall back to just "Type"
    map(w(type_expression), |type_expr| (None, type_expr))(i)
}

fn function_type_expression(i: Slice) -> ParseResult<AST<FunctionTypeExpression>> {
    let start = i.clone();

    let (remaining, open_paren) = tag("(")(i.clone())?;

    let mut parameters: Vec<(Option<(AST<PlainIdentifier>, Slice)>, AST<TypeExpression>)> =
        Vec::new();
    let mut commas = Vec::new();
    let mut current = remaining;
    let mut trailing_comma = None;

    // Parse first parameter if present
    if let Ok((after_param, param)) = function_type_param(current.clone()) {
        parameters.push(param);
        current = after_param;

        // Parse subsequent ", param" pairs
        loop {
            match w(tag(","))(current.clone()) {
                Ok((after_comma, comma)) => match function_type_param(after_comma.clone()) {
                    Ok((after_param, param)) => {
                        commas.push(comma);
                        parameters.push(param);
                        current = after_param;
                    }
                    Err(_) => {
                        trailing_comma = Some(comma);
                        current = after_comma;
                        break;
                    }
                },
                Err(_) => break,
            }
        }
    }

    // Use soft matching for ")" and "=>" so that if this isn't actually a
    // function type (e.g. it's a parenthesized type like `(number)`), we
    // return a soft error and let alt() fall through.
    let (current, close_paren) = match w(backtrack(tag(")"), ")", "("))(current) {
        Ok((c, Some(cp))) => (c, cp),
        _ => {
            return Err(nom::Err::Error(BagelError {
                src: i,
                severity: RuleSeverity::Error,
                details: BagelErrorDetails::ParseError {
                    message: "not a function type".to_owned(),
                },
                related: vec![],
            }))
        }
    };
    let (remaining, (arrow, mut return_type)) = match seq!(w(tag("=>")), type_expression)(current) {
        Ok(result) => result,
        Err(_) => {
            return Err(nom::Err::Error(BagelError {
                src: i,
                severity: RuleSeverity::Error,
                details: BagelErrorDetails::ParseError {
                    message: "not a function type".to_owned(),
                },
                related: vec![],
            }))
        }
    };

    let consumed_len = start.len() - remaining.len();
    let span = start.slice_range(0, Some(consumed_len));

    let func_type = FunctionTypeExpression {
        open_paren,
        parameters: parameters.clone(),
        commas,
        trailing_comma,
        close_paren,
        arrow,
        return_type: return_type.clone(),
    };

    let node = make_ast(span, func_type);
    for (name_colon, mut type_expr) in parameters {
        if let Some((mut name, _)) = name_colon {
            name.set_parent(&node);
        }
        type_expr.set_parent(&node);
    }
    return_type.set_parent(&node);

    Ok((remaining, node))
}

fn parenthesized_type_expression(i: Slice) -> ParseResult<AST<ParenthesizedTypeExpression>> {
    let (remaining, (open_paren, mut inner, close_paren)) =
        seq!(tag("("), type_expression, expect_tag(")"))(i)?;

    let span = open_paren.spanning(&close_paren);
    let node = make_ast(
        span,
        ParenthesizedTypeExpression {
            open_paren,
            expression: inner.clone(),
            close_paren,
        },
    );
    inner.set_parent(&node);

    Ok((remaining, node))
}

fn typeof_type_expression(i: Slice) -> ParseResult<AST<TypeOfTypeExpression>> {
    let (remaining, (keyword, mut expr)) = seq!(tag("typeof"), expression)(i)?;

    let span = keyword.spanning(expr.slice());
    let node = make_ast(
        span,
        TypeOfTypeExpression {
            keyword,
            expression: expr.clone(),
        },
    );
    expr.set_parent(&node);

    Ok((remaining, node))
}

fn object_type_expression(i: Slice) -> ParseResult<AST<ObjectTypeExpression>> {
    let (remaining, open_brace) = tag("{")(i)?;

    let mut fields = Vec::new();
    let mut commas = Vec::new();
    let mut current = remaining;
    let mut trailing_comma = None;

    // Parse fields: "name: type" separated by commas
    while let Ok((after_key, key)) = w(plain_identifier)(current.clone()) {
        match w(tag(":"))(after_key) {
            Ok((after_colon, colon)) => match w(type_expression)(after_colon) {
                Ok((after_type, type_expr)) => {
                    fields.push((key, colon, type_expr));
                    current = after_type;

                    match w(tag(","))(current.clone()) {
                        Ok((after_comma, comma)) => {
                            commas.push(comma);
                            current = after_comma;
                        }
                        Err(_) => break,
                    }
                }
                Err(_) => break,
            },
            Err(_) => break,
        }
    }

    // Detect trailing comma (last comma without a following field)
    if commas.len() == fields.len() && !commas.is_empty() {
        trailing_comma = commas.pop();
    }

    let (remaining, close_brace) = w(backtrack(tag("}"), "}", "{"))(current)?;

    let span = open_brace.spanning(close_brace.as_ref().unwrap_or(&open_brace));

    let node = make_ast(
        span,
        ObjectTypeExpression {
            open_brace: open_brace.clone(),
            fields: fields.clone(),
            commas,
            trailing_comma,
            close_brace: close_brace.unwrap_or(open_brace),
        },
    );
    for (key, _, type_expr) in &mut fields {
        key.set_parent(&node);
        type_expr.set_parent(&node);
    }

    Ok((remaining, node))
}

fn primary_type_expression(i: Slice) -> ParseResult<AST<TypeExpression>> {
    alt((
        map(unknown_type_expression, |n| n.upcast()),
        map(nil_type_expression, |n| n.upcast()),
        map(boolean_type_expression, |n| n.upcast()),
        map(range_type_expression, |n| n.upcast()),
        map(number_type_expression, |n| n.upcast()),
        map(string_type_expression, |n| n.upcast()),
        map(typeof_type_expression, |n| n.upcast()),
        map(object_type_expression, |n| n.upcast()),
        map(function_type_expression, |n| n.upcast()),
        map(parenthesized_type_expression, |n| n.upcast()),
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

// Parser for ConstDeclaration: "export"? "const" PlainIdentifier (":" TypeExpression)? "=" Expression
fn const_declaration(i: Slice) -> ParseResult<AST<ConstDeclaration>> {
    map(
        seq!(
            opt(tag("export")),
            tag("const"),
            plain_identifier,
            opt(seq!(tag(":"), type_expression)),
            expect_tag("="),
            expression
        ),
        |(export_keyword, const_keyword, mut identifier, type_annotation, equals, mut value)| {
            let span = export_keyword
                .as_ref()
                .unwrap_or(&const_keyword)
                .spanning(value.slice());

            let decl = ConstDeclaration {
                export_keyword,
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

// Parser for ImportDeclaration: "from" StringLiteral "import" "{" ImportSpecifier ("," ImportSpecifier)* ","? "}"
fn import_declaration(i: Slice) -> ParseResult<AST<ImportDeclaration>> {
    let (remaining, from_keyword) = tag("from")(i)?;
    let (remaining, mut path) = w(string_literal)(remaining)?;
    let (remaining, import_keyword) = w(tag("import"))(remaining)?;
    let (remaining, open_brace) = w(tag("{"))(remaining)?;

    let mut imports = Vec::new();
    let mut commas = Vec::new();
    let mut current = remaining;
    let mut trailing_comma = None;

    // Parse first import specifier if present
    if let Ok((after_spec, spec)) = w(import_specifier)(current.clone()) {
        imports.push(spec);
        current = after_spec;

        // Parse subsequent ", specifier" pairs
        loop {
            if let Ok((after_comma, comma)) = w(tag(","))(current.clone()) {
                if let Ok((after_spec, spec)) = w(import_specifier)(after_comma.clone()) {
                    commas.push(comma);
                    imports.push(spec);
                    current = after_spec;
                } else {
                    trailing_comma = Some(comma);
                    current = after_comma;
                    break;
                }
            } else {
                break;
            }
        }
    }

    let (remaining, close_brace) = w(backtrack(tag("}"), "}", "{"))(current)?;

    let span = from_keyword.spanning(close_brace.as_ref().unwrap_or(&open_brace));

    let decl = ImportDeclaration {
        from_keyword,
        path: path.clone(),
        import_keyword,
        open_brace,
        imports: imports.clone(),
        commas,
        trailing_comma,
        close_brace,
    };

    let node = make_ast(span, decl);
    path.set_parent(&node);
    for mut spec in imports {
        spec.name.set_parent(&node);
        if let Some((_, mut alias)) = spec.alias {
            alias.set_parent(&node);
        }
    }

    Ok((remaining, node))
}

// Parser for ImportSpecifier: PlainIdentifier ("as" PlainIdentifier)?
fn import_specifier(i: Slice) -> ParseResult<ImportSpecifier> {
    map(
        seq!(plain_identifier, opt(seq!(tag("as"), plain_identifier))),
        |(name, alias)| ImportSpecifier { name, alias },
    )(i)
}

// Parser for Declaration: ConstDeclaration | ImportDeclaration
pub fn declaration(i: Slice) -> ParseResult<AST<Declaration>> {
    alt((
        map(const_declaration, |d| d.upcast()),
        map(import_declaration, |d| d.upcast()),
    ))(i)
}

// Parser for Module: Declaration+
pub fn module(i: Slice) -> ParseResult<AST<Module>> {
    let start = i.clone();

    // Parse one or more declarations
    let (remaining, const_declarations) = many1(w(declaration))(i)?;

    let consumed_len = start.len() - remaining.len();
    let span = start.slice_range(0, Some(consumed_len));

    let mut declarations: Vec<AST<Declaration>> = const_declarations;

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
