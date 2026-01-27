use nom::{
    bytes::complete::{tag, take_while},
    sequence::preceded,
    IResult, Parser,
};

use crate::ast::slice::Slice;

// --- Util types ---

pub type ParseResult<T> = IResult<Slice, T, RawParseError>;

#[derive(Debug, Clone, PartialEq)]
pub struct RawParseError {
    pub src: Slice,
    pub details: RawParseErrorDetails,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RawParseErrorDetails {
    Expected(String),
    Kind(nom::error::ErrorKind),
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

pub fn w<O, G>(parser: G) -> impl FnMut(Slice) -> ParseResult<O>
where
    G: Parser<Slice, O, RawParseError>,
{
    preceded(whitespace, parser)
}

pub fn whitespace(i: Slice) -> ParseResult<Slice> {
    take_while(|c| c == ' ' || c == '\n' || c == '\t' || c == '\r')(i)
}

pub fn expect<TResult, F: FnMut(Slice) -> ParseResult<TResult>>(
    f: F,
    description: &'static str,
) -> impl FnMut(Slice) -> ParseResult<TResult> {
    expect_inner(f, description, false)
}

pub fn expect_tag(t: &'static str) -> impl FnMut(Slice) -> ParseResult<Slice> {
    expect_inner(tag(t), t, true)
}

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
