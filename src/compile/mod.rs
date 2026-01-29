use std::fmt::Write;

use crate::{
    ast::{container::AST, grammar::Any},
    config::Config,
};

#[derive(Debug, Clone, Copy)]
pub struct CompileContext<'a> {
    pub config: &'a Config,
    // pub modules: &'a ModulesStore,
    // pub current_module: &'a ParsedModule,
    // pub include_types: bool,
    // pub qualify_identifiers_with: Option<&'a HashMap<ModuleID, usize>>,
    // pub qualify_all_identifiers: bool,
}

pub trait Compilable {
    /// Given some Bagel AST node, compile it into JavaScript code
    ///
    /// Each implementation is responsible for compiling its own child nodes
    /// (as appropriate, in context)
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> core::fmt::Result;
}

impl<TKind> Compilable for AST<TKind>
where
    TKind: Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    fn compile<'a, W: Write>(&self, ctx: CompileContext<'a>, f: &mut W) -> core::fmt::Result {
        match self.details() {
            Any::Module(module) => todo!(),
            Any::Declaration(declaration) => todo!(),
            Any::Expression(expression) => todo!(),
            Any::PlainIdentifier(plain_identifier) => todo!(),
            Any::BinaryOperator(binary_operator) => todo!(),
            Any::Malformed(malformed) => todo!(),
        }
    }
}
