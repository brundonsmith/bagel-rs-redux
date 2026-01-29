use std::fmt::Write;

use crate::{
    ast::{container::AST, grammar::Any},
    config::Config,
};

#[derive(Debug, Clone, Copy)]
pub struct EmitContext<'a> {
    pub config: &'a Config,
}

pub trait Emittable {
    fn emit<W: Write>(&self, f: &mut W, ctx: EmitContext) -> core::fmt::Result;
}

impl<TKind> Emittable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn emit<W: Write>(&self, f: &mut W, ctx: EmitContext) -> core::fmt::Result {
        match self.details() {
            Any::Module(module) => todo!(),
            Any::Declaration(declaration) => todo!(),
            Any::Expression(expression) => todo!(),
            Any::TypeExpression(_type_expression) => todo!(),
            Any::PlainIdentifier(plain_identifier) => todo!(),
            Any::BinaryOperator(binary_operator) => todo!(),
            Any::Malformed(malformed) => todo!(),
        }
    }
}
