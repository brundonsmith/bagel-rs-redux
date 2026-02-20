//! AST container types for the Bagel language.
//!
//! This module provides the core AST infrastructure

use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::sync::{Arc, RwLock, Weak};

use crate::ast::grammar::{
    Any, Declaration, ElseClause, Expression, FunctionBody, Statement, TypeExpression,
};
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
/// let node = make_ast(slice.clone(), NumberLiteral { slice });
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
    Malformed(Slice, String),
}

impl<TKind> std::hash::Hash for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + Debug,
    Any: From<TKind>,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            AST::Valid(inner, _) => Arc::as_ptr(inner).hash(state),
            AST::Malformed(slice, message) => {
                slice.hash(state);
                message.hash(state);
            }
        }
    }
}

impl<TKind> PartialOrd for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + PartialEq,
    Any: From<TKind>,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(ptr_of(self).cmp(&ptr_of(other)))
    }
}

impl<TKind> Ord for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + Eq + PartialOrd,
    Any: From<TKind>,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        ptr_of(self).cmp(&ptr_of(other))
    }
}

fn ptr_of<TKind>(ast: &AST<TKind>) -> usize
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    match ast {
        AST::Valid(inner, _) => Arc::as_ptr(inner) as usize,
        AST::Malformed(slice, _) => slice.start,
    }
}

impl<TKind> Debug for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + Debug,
    Any: From<TKind>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AST::Valid(inner, _) => Debug::fmt(inner, f),
            AST::Malformed(slice, message) => f
                .debug_struct("Malformed")
                .field("slice", slice)
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

    /// Returns true if this is a valid (non-malformed) node.
    pub fn is_valid(&self) -> bool {
        matches!(self, AST::Valid(_, _))
    }

    /// Returns true if this is a malformed node.
    pub fn is_malformed(&self) -> bool {
        matches!(self, AST::Malformed(..))
    }

    /// Returns the source code slice for this AST node.
    ///
    /// Every AST node stores the `Slice` representing the portion of source code
    /// it was parsed from. This allows zero-allocation substring references.
    pub fn slice(&self) -> &Slice {
        match self {
            AST::Valid(inner, _) => &inner.slice,
            AST::Malformed(slice, _) => slice,
        }
    }

    /// Returns the details of this AST node if it's valid, or None if malformed.
    pub fn details(&self) -> Option<&Any> {
        match self {
            AST::Valid(inner, _) => Some(&inner.details),
            AST::Malformed(..) => None,
        }
    }

    /// Returns the error message if this is a malformed node.
    pub fn malformed_message(&self) -> Option<&str> {
        match self {
            AST::Malformed(_, message) => Some(message),
            _ => None,
        }
    }

    pub fn ptr_eq<TOtherKind>(&self, other: &AST<TOtherKind>) -> bool
    where
        TOtherKind: Clone + TryFrom<Any>,
        Any: From<TOtherKind>,
    {
        match (self, other) {
            (AST::Valid(a, _), AST::Valid(b, _)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }

    pub fn parent(&self) -> Option<AST<Any>> {
        match self {
            AST::Valid(inner, _) => inner
                .parent
                .read()
                .unwrap()
                .as_ref()
                .and_then(|weak: &Weak<ASTInner>| weak.upgrade())
                .map(|node| AST::<Any>::Valid(node, PhantomData)),
            AST::Malformed(..) => None,
        }
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
            AST::Malformed(slice, message) => AST::<TExpected>::Malformed(slice, message),
        }
    }

    pub fn try_downcast<TExpected>(self) -> Option<AST<TExpected>>
    where
        TExpected: Clone + TryFrom<Any> + TryFrom<TKind>,
        Any: From<TExpected>,
    {
        match self {
            AST::Valid(inner, _) => TExpected::try_from(inner.details.clone())
                .ok()
                .map(|_| AST::<TExpected>::Valid(inner, PhantomData)),
            AST::Malformed(slice, message) => Some(AST::<TExpected>::Malformed(slice, message)),
        }
    }

    pub fn unpack(&self) -> Option<TKind> {
        match self {
            AST::Valid(inner, _) => match TKind::try_from(inner.details.clone()) {
                Ok(res) => Some(res),
                Err(_) => unreachable!(),
            },
            AST::Malformed(..) => None,
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
            AST::Malformed(..) => return,
        };
        let parent_inner = match parent {
            AST::Valid(inner, _) => inner,
            AST::Malformed(..) => return,
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
        f.debug_struct("ASTInner")
            .field("slice", &self.slice)
            .field("details", &self.details)
            .finish()
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

impl Any {
    /// Calls `f` for each direct child AST node of this node.
    /// Zero-allocation alternative to collecting children into a Vec.
    pub fn for_each_child(&self, f: &mut dyn FnMut(AST<Any>)) {
        match self {
            Any::Module(module) => {
                module
                    .declarations
                    .iter()
                    .for_each(|d| f(d.clone().upcast()));
            }
            Any::Declaration(decl) => match decl {
                Declaration::ConstDeclaration(c) => {
                    f(c.identifier.clone().upcast());
                    if let Some((_, type_ann)) = &c.type_annotation {
                        f(type_ann.clone().upcast());
                    }
                    f(c.value.clone().upcast());
                }
                Declaration::ImportDeclaration(imp) => {
                    f(imp.path.clone().upcast());
                    imp.imports.iter().for_each(|spec| {
                        f(spec.name.clone().upcast());
                        if let Some((_, alias)) = &spec.alias {
                            f(alias.clone().upcast());
                        }
                    });
                }
            },
            Any::Expression(expr) => match expr {
                Expression::NilLiteral(_)
                | Expression::BooleanLiteral(_)
                | Expression::NumberLiteral(_)
                | Expression::StringLiteral(_)
                | Expression::LocalIdentifier(_) => {}
                Expression::BinaryOperation(bin_op) => {
                    f(bin_op.left.clone().upcast());
                    f(bin_op.operator.clone().upcast());
                    f(bin_op.right.clone().upcast());
                }
                Expression::UnaryOperation(unary_op) => {
                    f(unary_op.operator.clone().upcast());
                    f(unary_op.operand.clone().upcast());
                }
                Expression::Invocation(inv) => {
                    f(inv.function.clone().upcast());
                    inv.arguments.iter().for_each(|arg| f(arg.clone().upcast()));
                }
                Expression::FunctionExpression(func) => {
                    func.parameters.iter().for_each(|(name, type_ann)| {
                        f(name.clone().upcast());
                        if let Some((_, t)) = type_ann {
                            f(t.clone().upcast());
                        }
                    });
                    if let Some((_, ret)) = &func.return_type {
                        f(ret.clone().upcast());
                    }
                    f(func.body.clone().upcast());
                }
                Expression::ArrayLiteral(arr) => {
                    arr.elements.iter().for_each(|e| f(e.clone().upcast()));
                }
                Expression::ObjectLiteral(obj) => {
                    obj.fields.iter().for_each(|(key, _, value)| {
                        f(key.clone().upcast());
                        f(value.clone().upcast());
                    });
                }
                Expression::IfElseExpression(if_else) => {
                    f(if_else.condition.clone().upcast());
                    f(if_else.consequent.clone().upcast());
                    match &if_else.else_clause {
                        Some(ElseClause::ElseBlock { expression, .. }) => {
                            f(expression.clone().upcast());
                        }
                        Some(ElseClause::ElseIf {
                            if_else: nested, ..
                        }) => {
                            f(nested.clone().upcast());
                        }
                        None => {}
                    }
                }
                Expression::ParenthesizedExpression(paren) => {
                    f(paren.expression.clone().upcast());
                }
                Expression::PropertyAccessExpression(prop_access) => {
                    f(prop_access.subject.clone().upcast());
                    f(prop_access.property.clone().upcast());
                }
                Expression::PipeCallExpression(pipe) => {
                    f(pipe.subject.clone().upcast());
                    if let Some(func) = &pipe.function {
                        f(func.clone().upcast());
                    }
                    pipe.arguments
                        .iter()
                        .for_each(|arg| f(arg.clone().upcast()));
                }
            },
            Any::TypeExpression(type_expr) => match type_expr {
                TypeExpression::UnknownTypeExpression(_)
                | TypeExpression::NilTypeExpression(_)
                | TypeExpression::BooleanTypeExpression(_)
                | TypeExpression::NumberTypeExpression(_)
                | TypeExpression::StringTypeExpression(_)
                | TypeExpression::RangeTypeExpression(_) => {}
                TypeExpression::TupleTypeExpression(tuple) => {
                    tuple.elements.iter().for_each(|e| f(e.clone().upcast()));
                }
                TypeExpression::ArrayTypeExpression(array) => {
                    f(array.element.clone().upcast());
                }
                TypeExpression::ObjectTypeExpression(obj) => {
                    obj.fields.iter().for_each(|(name, _, typ)| {
                        f(name.clone().upcast());
                        f(typ.clone().upcast());
                    });
                }
                TypeExpression::FunctionTypeExpression(func) => {
                    func.parameters.iter().for_each(|(name_colon, typ)| {
                        if let Some((name, _)) = name_colon {
                            f(name.clone().upcast());
                        }
                        f(typ.clone().upcast());
                    });
                    f(func.return_type.clone().upcast());
                }
                TypeExpression::UnionTypeExpression(union) => {
                    union.variants.iter().for_each(|v| f(v.clone().upcast()));
                }
                TypeExpression::ParenthesizedTypeExpression(paren) => {
                    f(paren.expression.clone().upcast());
                }
                TypeExpression::TypeOfTypeExpression(type_of) => {
                    f(type_of.expression.clone().upcast());
                }
                TypeExpression::NillableTypeExpression(nillable) => {
                    f(nillable.subject.clone().upcast());
                }
            },
            Any::FunctionBody(body) => match body {
                FunctionBody::Expression(expr) => f(expr.clone().upcast()),
                FunctionBody::Block(block) => f(block.clone().upcast()),
            },
            Any::Statement(statement) => match statement {
                Statement::Expression(expr) => {
                    Any::Expression(expr.clone()).for_each_child(f);
                }
                Statement::Block(block) => {
                    block.statements.iter().for_each(|s| f(s.clone().upcast()));
                }
            },
            Any::PlainIdentifier(_) | Any::BinaryOperator(_) | Any::UnaryOperator(_) => {}
        }
    }
}

/// Controls traversal behavior in `walk_ast`.
pub enum WalkAction {
    /// Continue traversing into children.
    Continue,
    /// Skip this node's children but continue the traversal.
    SkipChildren,
    /// Stop the entire traversal immediately.
    Stop,
}

/// Pre-order traversal of the AST. Calls `visitor` on each node.
/// Returns `false` if the traversal was stopped early via `WalkAction::Stop`.
pub fn walk_ast(node: &AST<Any>, visitor: &mut dyn FnMut(&AST<Any>) -> WalkAction) -> bool {
    match visitor(node) {
        WalkAction::Stop => false,
        WalkAction::SkipChildren => true,
        WalkAction::Continue => {
            if let Some(details) = node.details() {
                let mut should_continue = true;
                details.for_each_child(&mut |child| {
                    if should_continue {
                        should_continue = walk_ast(&child, visitor);
                    }
                });
                should_continue
            } else {
                true
            }
        }
    }
}

/// Finds the deepest (most specific) node in the AST that satisfies `predicate`.
/// "Deepest" means: if both a parent and child match, the child is preferred.
pub fn find_deepest(node: &AST<Any>, predicate: &dyn Fn(&AST<Any>) -> bool) -> Option<AST<Any>> {
    if !predicate(node) {
        None
    } else {
        // This node matches; see if any child matches more specifically
        let mut deepest_child = None;
        if let Some(details) = node.details() {
            details.for_each_child(&mut |child| {
                if deepest_child.is_none() {
                    deepest_child = find_deepest(&child, predicate);
                }
            });
        }
        deepest_child.or_else(|| Some(node.clone()))
    }
}
