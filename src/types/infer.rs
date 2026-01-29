use crate::{
    ast::{container::AST, grammar::Expression},
    types::Type,
};

#[derive(Clone, Copy, Debug)]
pub struct InferTypeContext {
    // pub modules: &'a ModulesStore,
    // pub current_module: &'a ParsedModule,
    // pub expressions_encountered: &'a Vec<AST<Expression>>,
}

impl AST<Expression> {
    pub fn infer_type(&self, ctx: InferTypeContext) -> Type {
        match self.details() {
            crate::ast::grammar::Any::Module(module) => todo!(),
            crate::ast::grammar::Any::Declaration(declaration) => todo!(),
            crate::ast::grammar::Any::Expression(expression) => todo!(),
            crate::ast::grammar::Any::PlainIdentifier(plain_identifier) => todo!(),
            crate::ast::grammar::Any::BinaryOperator(binary_operator) => todo!(),
            crate::ast::grammar::Any::Malformed(malformed) => todo!(),
        }
    }
}
