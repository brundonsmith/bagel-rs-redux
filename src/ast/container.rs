//! AST container types for the Bagel language.
//!
//! This module provides the core AST infrastructure

use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::rc::{Rc, Weak};

use crate::ast::grammar::Any;
use crate::ast::slice::Slice;

/// Generic wrapper for all AST nodes in Bagel.
///
/// `AST<TKind>` wraps an `ASTInner` in an `Rc` for cheap cloning, and uses
/// `PhantomData` to represent the specific type of AST node contained within.
/// This allows type-safe casting between AST node types without unwrapping or
/// cloning the underlying `Rc`.
///
/// # Type Parameters
/// - `TKind`: The specific AST node type (e.g., `Expression`, `Declaration`, `Module`)
///
/// # Example
/// ```ignore
/// // Create a new AST node
/// let node = make_ast(slice, NumberLiteral);
///
/// // Upcast to a more general type
/// let expr: AST<Expression> = node.upcast();
///
/// // Try to downcast back
/// let number: Option<AST<NumberLiteral>> = expr.try_downcast();
/// ```
#[derive(Clone, PartialEq, Eq)]
pub struct AST<TKind>(Rc<ASTInner>, PhantomData<TKind>)
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>;

impl<TKind> std::hash::Hash for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + Debug,
    Any: From<TKind>,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<TKind> Debug for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + Debug,
    Any: From<TKind>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("AST").field(&self.0).finish()
    }
}

impl<TKind> AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    /// Creates a new AST node from an ASTInner.
    ///
    /// This is typically called from parser functions via the `make_ast` helper.
    pub fn new(inner: Rc<ASTInner>) -> Self {
        AST(inner, PhantomData)
    }

    /// Returns the source code slice for this AST node.
    ///
    /// Every AST node stores the `Slice` representing the portion of source code
    /// it was parsed from. This allows zero-allocation substring references.
    pub fn slice(&self) -> &Slice {
        &self.0.slice
    }

    pub fn details(&self) -> &Any {
        &self.0.details
    }

    pub fn ptr_eq<TOtherKind>(&self, other: &AST<TOtherKind>) -> bool
    where
        TOtherKind: Clone + TryFrom<Any>,
        Any: From<TOtherKind>,
    {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub fn parent(&self) -> Option<AST<Any>> {
        self.0
            .parent
            .borrow()
            .as_ref()
            .map(|weak| weak.upgrade())
            .flatten()
            .map(|node| AST::<Any>(node, PhantomData))
    }

    pub fn contains_child(&self, other: &AST<Any>) -> bool {
        let mut current = Some(other.clone());

        while let Some(some_current) = current {
            if self.ptr_eq::<Any>(&some_current) {
                return true;
            }

            current = some_current.parent();
        }

        return false;
    }

    pub fn upcast<TExpected>(self) -> AST<TExpected>
    where
        TExpected: Clone + TryFrom<Any> + From<TKind>,
        Any: From<TExpected>,
    {
        AST::<TExpected>(self.0, PhantomData)
    }

    pub fn try_downcast<TExpected>(self) -> Option<AST<TExpected>>
    where
        TExpected: Clone + TryFrom<Any> + TryFrom<TKind>,
        Any: From<TExpected>,
    {
        TExpected::try_from(self.0.details.clone())
            .ok()
            .map(|_| AST::<TExpected>(self.0, PhantomData))
    }

    pub fn unpack(&self) -> TKind {
        match TKind::try_from(self.0.details.clone()) {
            Ok(res) => res,
            Err(_) => unreachable!(),
        }
    }

    pub fn try_unpack<TExpected>(&self) -> Option<TExpected>
    where
        TExpected: TryFrom<Any>,
        Any: From<TExpected>,
    {
        TExpected::try_from(self.0.details.clone()).ok()
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
        self.0
            .as_ref()
            .parent
            .replace(Some(Rc::downgrade(&parent.0)));
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
    pub parent: RefCell<Option<Weak<ASTInner>>>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Malformed(Slice);
