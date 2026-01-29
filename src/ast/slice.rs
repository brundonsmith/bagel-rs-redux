//! Zero-allocation substring references for the Bagel language.
//!
//! As documented in GRAMMAR.md section 1, any exact strings parsed out of the
//! original document unchanged should be stored as a `Slice` that refers to
//! the document, instead of allocating a new `String`.

use std::ops::{RangeFrom, RangeTo};
use std::{fmt::Debug, sync::Arc};

use memoize::memoize;
use nom::{AsChar, Compare, InputIter, InputLength, InputTake, Offset, UnspecializedInput};

/// Zero-allocation substring reference into source code.
///
/// Represents a contiguous substring of some `Arc<String>`. Optimized to allow
/// substringing, concatenation, and resizing without allocations, as well as
/// cheap cloning. This is used throughout the AST to reference source code
/// without copying strings.
///
/// # Identity Semantics
/// Two slices are not considered equal unless they refer to the **same**
/// (pointer-equivalent) `Arc<String>`, and the exact same indices within it!
/// This ensures that slices from different source files or different positions
/// are always distinct.
///
/// # Example
/// ```ignore
/// let source = Arc::new("const x = 42".to_string());
/// let slice = Slice::new(source);
/// let keyword = slice.clone().slice_range(0, Some(5)); // "const"
/// assert_eq!(keyword.as_str(), "const");
/// ```
#[derive(Clone, Eq)]
pub struct Slice {
    /// The full source code string
    pub full_string: Arc<String>,
    /// Start index (byte offset) into the full string
    pub start: usize,
    /// End index (byte offset) into the full string
    pub end: usize,
}

impl Slice {
    /// Creates a new slice covering the entire string.
    pub fn new(full_string: Arc<String>) -> Self {
        let end = full_string.len();

        Self {
            full_string,
            start: 0,
            end,
        }
    }

    /// Returns the length of this slice in bytes.
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Returns true if this slice completely contains the other slice.
    ///
    /// Both slices must refer to the same source string.
    pub fn contains(&self, other: &Slice) -> bool {
        self.full_string.as_ptr() == other.full_string.as_ptr()
            && self.start <= other.start
            && self.end >= other.end
    }

    /// Joins two slices, creating a new slice that spans from the minimum
    /// start to the maximum end of both slices.
    pub fn join(self, other: &Slice) -> Slice {
        Self {
            full_string: self.full_string,
            start: usize::min(self.start, other.start),
            end: usize::max(self.end, other.end),
        }
    }

    /// Returns the actual string content of this slice.
    pub fn as_str(&self) -> &str {
        &self.full_string[self.start..self.end]
    }

    /// Creates a new slice spanning from this slice to another.
    ///
    /// This is commonly used in parsers to create a parent node's slice
    /// from its first and last child nodes. Both slices must refer to
    /// the same source string.
    ///
    /// # Example
    /// ```ignore
    /// let span = left_child.slice().spanning(right_child.slice());
    /// let node = make_ast(span, BinaryOperation { left, operator, right });
    /// ```
    pub fn spanning(&self, other: &Slice) -> Slice {
        Self {
            full_string: self.full_string.clone(),
            start: usize::min(self.start, other.start),
            end: usize::max(self.end, other.end),
        }
    }

    /// Creates a sub-slice with relative offsets.
    ///
    /// The `start` and `end` parameters are relative to this slice's start,
    /// not the full string. If `end` is `None`, the slice extends to the
    /// end of this slice.
    ///
    /// # Example
    /// ```ignore
    /// let full = Slice::new(Arc::new("const x = 42".to_string()));
    /// let keyword = full.clone().slice_range(0, Some(5)); // "const"
    /// ```
    pub fn slice_range(self, start: usize, end: Option<usize>) -> Slice {
        Self {
            full_string: self.full_string,
            start: self.start + start,
            end: end.map(|end| self.start + end).unwrap_or(self.end),
        }
    }
}

#[memoize]
pub fn nothing_slice() -> Slice {
    Slice::new(Arc::new("".to_owned()))
}

impl core::hash::Hash for Slice {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.full_string.as_ptr().hash(state);
        self.start.hash(state);
        self.end.hash(state);
    }
}

impl PartialEq for Slice {
    fn eq(&self, other: &Self) -> bool {
        self.full_string.as_ptr() == other.full_string.as_ptr()
            && self.start == other.start
            && self.end == other.end
    }
}

impl PartialEq<&str> for Slice {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<Slice> for &str {
    fn eq(&self, other: &Slice) -> bool {
        *self == other.as_str()
    }
}

impl PartialEq<&str> for &Slice {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<&Slice> for &str {
    fn eq(&self, other: &&Slice) -> bool {
        *self == other.as_str()
    }
}

impl Debug for Slice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Slice({:?})", self.as_str()))
    }
}

// --- nom traits ---

impl InputLength for Slice {
    fn input_len(&self) -> usize {
        self.as_str().input_len()
    }
}

impl InputTake for Slice {
    fn take(&self, count: usize) -> Self {
        self.clone().slice_range(0, Some(count))
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        (
            self.clone().slice_range(count, None),
            self.clone().slice_range(0, Some(count)),
        )
    }
}

impl InputIter for Slice {
    type Item = char;
    type Iter = SliceCharIndices;
    type IterElem = SliceChars;

    fn iter_indices(&self) -> Self::Iter {
        SliceCharIndices {
            code: self.full_string.clone(),
            index: self.start,
        }
    }

    fn iter_elements(&self) -> Self::IterElem {
        SliceChars {
            code: self.full_string.clone(),
            index: self.start,
        }
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.as_str().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.as_str().slice_index(count)
    }
}

impl UnspecializedInput for Slice {}

impl Offset for Slice {
    fn offset(&self, second: &Self) -> usize {
        second.start - self.start
    }
}

impl nom::Slice<RangeFrom<usize>> for Slice {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.clone().slice_range(range.start, None)
    }
}

impl nom::Slice<RangeTo<usize>> for Slice {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.clone().slice_range(0, Some(range.end))
    }
}

impl<'a> Compare<&'a str> for Slice {
    fn compare(&self, t: &'a str) -> nom::CompareResult {
        self.as_str().compare(t)
    }

    fn compare_no_case(&self, t: &'a str) -> nom::CompareResult {
        self.as_str().compare_no_case(t)
    }
}

// --- internal use by nom traits ---

pub struct SliceCharIndices {
    code: Arc<String>,
    index: usize,
}

impl Iterator for SliceCharIndices {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_char) = self.code.as_str()[self.index..].chars().next() {
            let res = (self.index, next_char);
            self.index += next_char.len();
            Some(res)
        } else {
            None
        }
    }
}

pub struct SliceChars {
    code: Arc<String>,
    index: usize,
}

impl Iterator for SliceChars {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_char) = self.code.as_str()[self.index..].chars().next() {
            let res = next_char;
            self.index += next_char.len();
            Some(res)
        } else {
            None
        }
    }
}

#[test]
fn take_split() {
    let code = Arc::new(String::from("ksjdfg"));
    let s = Slice::new(code.clone());

    let index = 1;
    let (a, b) = s.take_split(index);
    assert_eq!((a.as_str(), b.as_str()), s.as_str().take_split(index));

    let index = 2;
    let (a, b) = s.take_split(index);
    assert_eq!((a.as_str(), b.as_str()), s.as_str().take_split(index));

    let s = Slice {
        full_string: code.clone(),
        start: 2,
        end: s.len() - 1,
    };

    let index = 1;
    let (a, b) = s.take_split(index);
    assert_eq!((a.as_str(), b.as_str()), s.as_str().take_split(index));

    let index = 2;
    let (a, b) = s.take_split(index);
    assert_eq!((a.as_str(), b.as_str()), s.as_str().take_split(index));
}

#[test]
fn slice_range() {
    let code = Arc::new(String::from("2136547612534721634"));

    let slice = Slice {
        full_string: code.clone(),
        start: 0,
        end: 12,
    };

    let slice = slice.slice_range(4, None);
    assert_eq!(
        slice,
        Slice {
            full_string: code.clone(),
            start: 4,
            end: 12
        }
    );

    let slice = slice.slice_range(0, None);
    assert_eq!(
        slice,
        Slice {
            full_string: code.clone(),
            start: 4,
            end: 12
        }
    );

    let slice = slice.slice_range(0, Some(6));
    assert_eq!(
        slice,
        Slice {
            full_string: code.clone(),
            start: 4,
            end: 10
        }
    );

    let slice = slice.slice_range(1, Some(2));
    assert_eq!(
        slice,
        Slice {
            full_string: code.clone(),
            start: 5,
            end: 6
        }
    );
}
