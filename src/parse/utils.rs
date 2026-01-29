//! Parser utilities for the Bagel language.
//!
//! This module provides common types and helper functions for parsing

use nom::{
    bytes::complete::{tag, take_while},
    sequence::preceded,
    IResult, Parser,
};

use crate::ast::slice::Slice;

// --- Util types ---

/// The standard result type for all parsers in Bagel.
///
/// Each AST node parser takes a `Slice` and returns `ParseResult<AST<TKind>>`
/// where `TKind` is the expected AST node struct or enum.
pub type ParseResult<T> = IResult<Slice, T, RawParseError>;

/// Custom parse error type for Bagel.
///
/// Contains the source location (as a `Slice`) and details about what went wrong.
#[derive(Debug, Clone, PartialEq)]
pub struct RawParseError {
    /// The slice where the error occurred
    pub src: Slice,
    /// Details about what kind of error occurred
    pub details: RawParseErrorDetails,
}

/// Details about what kind of parse error occurred.
#[derive(Debug, Clone, PartialEq)]
pub enum RawParseErrorDetails {
    /// Expected a specific piece of syntax (e.g., "expected ')'")
    Expected(String),
    /// A nom error kind
    Kind(nom::error::ErrorKind),
    /// Expected a specific character
    Char(char),
}

impl nom::error::ParseError<Slice> for RawParseError {
    fn from_error_kind(input: Slice, kind: nom::error::ErrorKind) -> Self {
        Self {
            src: input,
            details: RawParseErrorDetails::Kind(kind),
        }
    }

    fn append(_input: Slice, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: Slice, ch: char) -> Self {
        Self {
            src: input,
            details: RawParseErrorDetails::Char(ch),
        }
    }
}

impl nom::error::ContextError<Slice> for RawParseError {
    fn add_context(_input: Slice, _ctx: &'static str, other: Self) -> Self {
        other
    }
}

impl<E> nom::error::FromExternalError<Slice, E> for RawParseError {
    fn from_external_error(input: Slice, kind: nom::error::ErrorKind, _e: E) -> Self {
        Self {
            src: input,
            details: RawParseErrorDetails::Kind(kind),
        }
    }
}

// --- Util parsers ---

/// Wraps a parser to consume optional preceding whitespace.
///
/// This is a convenience combinator that makes it easy to handle whitespace
/// in the grammar without explicitly writing it everywhere. For example,
/// `w(tag("="))` will match "=" with any amount of whitespace before it.
///
/// # Example
/// ```ignore
/// // Parse "const" keyword with optional preceding whitespace
/// let (remaining, keyword) = w(tag("const"))(input)?;
/// ```
pub fn w<O, G>(parser: G) -> impl FnMut(Slice) -> ParseResult<O>
where
    G: Parser<Slice, O, RawParseError>,
{
    preceded(whitespace, parser)
}

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
            let details = if quoted {
                format!("'{}'", description)
            } else {
                description.to_owned()
            };

            Err(nom::Err::Failure(RawParseError {
                src: i,
                details: RawParseErrorDetails::Expected(details),
            }))
        } else {
            res
        }
    }
}
