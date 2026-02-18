//! Parser utilities for the Bagel language.
//!
//! This module provides common types and helper functions for parsing

use nom::{
    bytes::complete::{tag, take_while},
    IResult,
};

use crate::ast::slice::Slice;
use crate::check::{BagelError, BagelErrorDetails};
use crate::config::RuleSeverity;

// --- Util types ---

/// The standard result type for all parsers in Bagel.
///
/// Each AST node parser takes a `Slice` and returns `ParseResult<AST<TKind>>`
/// where `TKind` is the expected AST node struct or enum.
pub type ParseResult<T> = IResult<Slice, T, BagelError>;

impl nom::error::ParseError<Slice> for BagelError {
    fn from_error_kind(input: Slice, kind: nom::error::ErrorKind) -> Self {
        Self {
            src: input,
            severity: RuleSeverity::Error,
            details: BagelErrorDetails::ParseError {
                message: format!("{:?}", kind),
            },
            related: vec![],
        }
    }

    fn append(_input: Slice, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: Slice, ch: char) -> Self {
        Self {
            src: input,
            severity: RuleSeverity::Error,
            details: BagelErrorDetails::ParseError {
                message: format!("expected '{}'", ch),
            },
            related: vec![],
        }
    }
}

impl nom::error::ContextError<Slice> for BagelError {
    fn add_context(_input: Slice, _ctx: &'static str, other: Self) -> Self {
        other
    }
}

impl<E> nom::error::FromExternalError<Slice, E> for BagelError {
    fn from_external_error(input: Slice, kind: nom::error::ErrorKind, _e: E) -> Self {
        Self {
            src: input,
            severity: RuleSeverity::Error,
            details: BagelErrorDetails::ParseError {
                message: format!("{:?}", kind),
            },
            related: vec![],
        }
    }
}

// --- Util parsers ---

/// Consumes zero or more whitespace characters.
///
/// Matches spaces, newlines, tabs, and carriage returns. This parser
/// always succeeds, even if there's no whitespace to consume.
pub fn whitespace(i: Slice) -> ParseResult<Slice> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i)
}

/// Converts a parser's soft errors into hard failures for required syntax.
///
/// When parsing required elements (like closing parentheses or equals signs),
/// use `expect()` to ensure that parse errors become `Failure` instead of `Error`.
/// This prevents nom's `alt()` combinator from trying other alternatives when
/// the required syntax is missing.
///
/// # Parameters
/// - `f`: The parser to wrap
/// - `description`: Human-readable description of what's expected (e.g., "expression")
///
/// # Example
/// ```ignore
/// // Parse a required closing parenthesis
/// let (remaining, close_paren) = expect(tag(")"), "closing parenthesis")(input)?;
/// ```
pub fn expect<TResult, F: FnMut(Slice) -> ParseResult<TResult>>(
    f: F,
    description: &'static str,
) -> impl FnMut(Slice) -> ParseResult<TResult> {
    expect_inner(f, description, false)
}

/// Converts a tag parser's soft errors into hard failures for required tokens.
///
/// This is a convenience wrapper around `expect()` specifically for exact string
/// matches. It automatically quotes the token in error messages.
///
/// # Parameters
/// - `t`: The exact string token to match (e.g., "=", ")", "const")
///
/// # Example
/// ```ignore
/// // Parse a required equals sign
/// let (remaining, equals) = expect_tag("=")(input)?;
/// ```
pub fn expect_tag(t: &'static str) -> impl FnMut(Slice) -> ParseResult<Slice> {
    expect_inner(tag(t), t, true)
}

/// Internal implementation for `expect()` and `expect_tag()`.
///
/// Wraps a parser to convert `Error` results into `Failure` with a descriptive
/// error message. The `quoted` parameter controls whether the description should
/// be wrapped in single quotes in the error message.
fn expect_inner<TResult, F: FnMut(Slice) -> ParseResult<TResult>>(
    mut f: F,
    description: &'static str,
    quoted: bool,
) -> impl FnMut(Slice) -> ParseResult<TResult> {
    move |i: Slice| {
        let res = f(i.clone());

        if matches!(res, Err(nom::Err::Error(_))) {
            let message = if quoted {
                format!("'{}'", description)
            } else {
                description.to_owned()
            };

            Err(nom::Err::Failure(BagelError {
                src: i,
                severity: RuleSeverity::Error,
                details: BagelErrorDetails::ParseError { message },
                related: vec![],
            }))
        } else {
            res
        }
    }
}

/// Wraps a parser to backtrack to the matching closing delimiter on failure.
///
/// When parsing paired delimiters (like `()`, `[]`, `{}`), this combinator will
/// attempt the provided parser. If it fails, it will search for the matching
/// closing delimiter and skip to it, returning the slice that was skipped.
///
/// This enables better error recovery when parsing incomplete or malformed code.
///
/// # Parameters
/// - `f`: The parser to attempt
/// - `closing_char`: The closing delimiter to search for (e.g., ")", "]", "}")
/// - `opening_char`: The opening delimiter for depth tracking (e.g., "(", "[", "{")
///
/// # Example
/// ```ignore
/// // Parse closing paren with backtracking on error
/// let (remaining, close_paren) = backtrack(
///     tag(")"),
///     ")",
///     "("
/// )(input)?;
/// ```
pub fn backtrack<'a>(
    mut f: impl FnMut(Slice) -> ParseResult<Slice> + 'a,
    closing_char: &'a str,
    opening_char: &'a str,
) -> impl FnMut(Slice) -> ParseResult<Option<Slice>> + 'a {
    move |i: Slice| {
        match f(i.clone()) {
            Ok((remaining, matched)) => Ok((remaining, Some(matched))),
            Err(_) => {
                // Try to recover by finding matching closing delimiter
                let mut depth = 1;
                let mut pos = 0;
                let text = i.as_str();

                for (idx, _) in text.char_indices() {
                    if text[idx..].starts_with(opening_char) {
                        depth += 1;
                    } else if text[idx..].starts_with(closing_char) {
                        depth -= 1;
                        if depth == 0 {
                            pos = idx;
                            break;
                        }
                    }
                }

                if depth == 0 {
                    // Found matching delimiter - skip to it
                    let matched = i.clone().slice_range(pos, Some(pos + closing_char.len()));
                    let remaining = i.slice_range(pos + closing_char.len(), None);
                    Ok((remaining, Some(matched)))
                } else {
                    // No matching delimiter found - consume nothing
                    Ok((i, None))
                }
            }
        }
    }
}
