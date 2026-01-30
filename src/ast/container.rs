//! AST container types for the Bagel language.
//!
//! This module provides the core AST infrastructure

use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::sync::{Arc, RwLock, Weak};

use crate::ast::grammar::Any;
use crate::ast::slice::Slice;

/// Generic wrapper for all AST nodes in Bagel.
///
/// `AST<TKind>` is an enum that represents either a valid parsed node or a malformed node.
/// This design acknowledges that any AST node can fail to parse, and forces proper handling
/// of malformed cases throughout the codebase.
///
/// # Type Parameters
/// - `TKind`: The specific AST node type (e.g., `Expression`, `Declaration`, `Module`)
///
/// # Example
/// ```ignore
/// // Create a valid AST node
/// let node = make_ast(slice, NumberLiteral);
///
/// // Pattern match to handle both cases
/// match node {
///     AST::Valid(inner, _) => { /* work with valid node */ }
///     AST::Malformed { inner, message } => { /* handle error */ }
/// }
/// ```
#[derive(Clone, PartialEq, Eq)]
pub enum AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    Valid(Arc<ASTInner>, PhantomData<TKind>),
    Malformed {
        inner: Arc<ASTInner>,
        message: String,
    },
}

impl<TKind> std::hash::Hash for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + Debug,
    Any: From<TKind>,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            AST::Valid(inner, _) => Arc::as_ptr(inner).hash(state),
            AST::Malformed { inner, .. } => Arc::as_ptr(inner).hash(state),
        }
    }
}

impl<TKind> Debug for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + Debug,
    Any: From<TKind>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AST::Valid(inner, _) => f.debug_tuple("AST").field(inner).finish(),
            AST::Malformed { inner, message } => f
                .debug_struct("AST::Malformed")
                .field("inner", inner)
                .field("message", message)
                .finish(),
        }
    }
}

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    /// Creates a new valid AST node from an ASTInner.
    ///
    /// This is typically called from parser functions via the `make_ast` helper.
    pub fn new(inner: Arc<ASTInner>) -> Self {
        AST::Valid(inner, PhantomData)
    }

    /// Creates a new malformed AST node with an error message.
    pub fn new_malformed(inner: Arc<ASTInner>, message: String) -> Self {
        AST::Malformed { inner, message }
    }

    /// Returns true if this is a valid (non-malformed) node.
    pub fn is_valid(&self) -> bool {
        matches!(self, AST::Valid(_, _))
    }

    /// Returns true if this is a malformed node.
    pub fn is_malformed(&self) -> bool {
        matches!(self, AST::Malformed { .. })
    }

    /// Returns the source code slice for this AST node.
    ///
    /// Every AST node stores the `Slice` representing the portion of source code
    /// it was parsed from. This allows zero-allocation substring references.
    pub fn slice(&self) -> &Slice {
        match self {
            AST::Valid(inner, _) => &inner.slice,
            AST::Malformed { inner, .. } => &inner.slice,
        }
    }

    /// Returns the details of this AST node if it's valid, or None if malformed.
    pub fn details(&self) -> Option<&Any> {
        match self {
            AST::Valid(inner, _) => Some(&inner.details),
            AST::Malformed { .. } => None,
        }
    }

    /// Returns the error message if this is a malformed node.
    pub fn malformed_message(&self) -> Option<&str> {
        match self {
            AST::Malformed { message, .. } => Some(message),
            _ => None,
        }
    }

    pub fn ptr_eq<TOtherKind>(&self, other: &AST<TOtherKind>) -> bool
    where
        TOtherKind: Clone + TryFrom<Any>,
        Any: From<TOtherKind>,
    {
        let self_ptr = match self {
            AST::Valid(inner, _) => Arc::as_ptr(inner),
            AST::Malformed { inner, .. } => Arc::as_ptr(inner),
        };
        let other_ptr = match other {
            AST::Valid(inner, _) => Arc::as_ptr(inner),
            AST::Malformed { inner, .. } => Arc::as_ptr(inner),
        };
        std::ptr::eq(self_ptr, other_ptr)
    }

    pub fn parent(&self) -> Option<AST<Any>> {
        let inner = match self {
            AST::Valid(inner, _) => inner,
            AST::Malformed { inner, .. } => inner,
        };
        inner
            .parent
            .read()
            .unwrap()
            .as_ref()
            .and_then(|weak| weak.upgrade())
            .map(|node| AST::<Any>::Valid(node, PhantomData))
    }

    pub fn contains_child(&self, other: &AST<Any>) -> bool {
        let mut current = Some(other.clone());

        while let Some(some_current) = current {
            if self.ptr_eq::<Any>(&some_current) {
                return true;
            }

            current = some_current.parent();
        }

        false
    }

    pub fn upcast<TExpected>(self) -> AST<TExpected>
    where
        TExpected: Clone + TryFrom<Any> + From<TKind>,
        Any: From<TExpected>,
    {
        match self {
            AST::Valid(inner, _) => AST::<TExpected>::Valid(inner, PhantomData),
            AST::Malformed { inner, message } => AST::<TExpected>::Malformed { inner, message },
        }
    }

    pub fn try_downcast<TExpected>(self) -> Option<AST<TExpected>>
    where
        TExpected: Clone + TryFrom<Any> + TryFrom<TKind>,
        Any: From<TExpected>,
    {
        match self {
            AST::Valid(inner, _) => {
                TExpected::try_from(inner.details.clone())
                    .ok()
                    .map(|_| AST::<TExpected>::Valid(inner, PhantomData))
            }
            AST::Malformed { inner, message } => {
                Some(AST::<TExpected>::Malformed { inner, message })
            }
        }
    }

    pub fn unpack(&self) -> TKind {
        match self {
            AST::Valid(inner, _) => match TKind::try_from(inner.details.clone()) {
                Ok(res) => res,
                Err(_) => unreachable!(),
            },
            AST::Malformed { .. } => panic!("Cannot unpack malformed AST node"),
        }
    }

    pub fn try_unpack<TExpected>(&self) -> Option<TExpected>
    where
        TExpected: TryFrom<Any>,
        Any: From<TExpected>,
    {
        match self {
            AST::Valid(inner, _) => TExpected::try_from(inner.details.clone()).ok(),
            AST::Malformed { .. } => None,
        }
    }
}

/// Convenience trait for setting parent relationships on AST nodes.
///
/// Parsers must call `set_parent()` on all
/// newly-created child nodes. This trait allows setting parents on nodes
/// contained within collections (e.g., `Option<AST<T>>` or `Vec<AST<T>>`)
/// with a single method call.
///
/// # Example
/// ```ignore
/// let mut children = vec![child1, child2, child3];
/// children.set_parent(&parent_node); // Sets parent on all children at once
///
/// let mut optional_child = Some(child);
/// optional_child.set_parent(&parent_node); // Works with Option too
/// ```
pub trait Parentable {
    fn set_parent<TParentKind>(&mut self, _parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        // do nothing by default
    }
}

impl<TKind> Parentable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        let self_inner = match self {
            AST::Valid(inner, _) => inner,
            AST::Malformed { inner, .. } => inner,
        };
        let parent_inner = match parent {
            AST::Valid(inner, _) => inner,
            AST::Malformed { inner, .. } => inner,
        };
        *self_inner.as_ref().parent.write().unwrap() = Some(Arc::downgrade(parent_inner));
    }
}

impl<T> Parentable for Option<T>
where
    T: Parentable,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        self.iter_mut().for_each(|x| x.set_parent(parent));
    }
}

impl<T> Parentable for Vec<T>
where
    T: Parentable,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        self.iter_mut().for_each(|x| x.set_parent(parent));
    }
}

impl<T, U> Parentable for (T, U)
where
    T: Parentable,
    U: Parentable,
{
    fn set_parent<TParentKind>(&mut self, parent: &AST<TParentKind>)
    where
        TParentKind: Clone + TryFrom<Any>,
        Any: From<TParentKind>,
    {
        self.0.set_parent(parent);
        self.1.set_parent(parent);
    }
}

/// Inner data structure containing metadata common to all AST nodes.
///
/// `ASTInner` contains all metadata that _every_
/// node carries, regardless of its specific type. This includes:
/// - A weak reference to the parent node (to avoid reference cycles)
/// - The original source code `Slice` this node was parsed from
/// - The semantic details specific to this node type (wrapped in `Any`)
///
/// `ASTInner` is always wrapped in `AST<TKind>` for type-safe access.
#[derive(Clone)]
pub struct ASTInner {
    /// Weak reference to the parent node (to avoid reference cycles)
    pub parent: Arc<RwLock<Option<Weak<ASTInner>>>>,
    /// The source code slice this node was parsed from
    pub slice: Slice,
    /// The type-specific details of this AST node
    pub details: Any,
}

impl Display for ASTInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.details))
    }
}

impl Debug for ASTInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}, {:?}", self.slice, self.details))
    }
}

impl core::hash::Hash for ASTInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.details.hash(state);
    }
}

impl PartialEq for ASTInner {
    fn eq(&self, other: &Self) -> bool {
        self.details == other.details
    }
}

impl Eq for ASTInner {}
