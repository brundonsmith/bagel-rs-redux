use std::cell::RefCell;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::rc::{Rc, Weak};

use crate::ast::grammar::Any;
use crate::ast::slice::Slice;

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
    pub fn new(inner: Rc<ASTInner>) -> Self {
        AST(inner, PhantomData)
    }

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

/// We'll frequently need to set `.parent` on AST nodes that are contained
/// within a collection (eg Option or Vec). This is a convenience trait to
/// let us apply the operation to all of those in a single method call
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
        if let Some(s) = self {
            s.set_parent(parent);
        }
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
        for ast in self.iter_mut() {
            ast.set_parent(parent);
        }
    }
}

#[derive(Clone)]
pub struct ASTInner {
    pub parent: RefCell<Option<Weak<ASTInner>>>,
    pub slice: Slice,
    pub details: Any,
}

impl Debug for ASTInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.details))
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
