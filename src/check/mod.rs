use crate::{
    ast::{container::AST, grammar::Any, slice::Slice},
    config::{Config, RuleSeverity},
};

#[derive(Debug, Clone)]
pub struct CheckContext<'a> {
    pub config: &'a Config,
    // pub modules: &'a ModulesStore,
    // pub current_module: &'a ParsedModule,
    // pub nearest_func_or_proc: Option<FuncOrProc>,
    // pub in_expression_context: bool,
}

#[derive(Debug, Clone)]
pub struct BagelError {
    // pub module_id: ModuleID,
    pub src: Slice,
    pub severity: RuleSeverity,
}

#[derive(Debug, Clone)]
pub enum BagelErrorDetails {
    ParseError { message: String },
    MiscError { message: String },
}

pub trait Checkable {
    /// Checks a given AST node and its children for any reportable errors. This
    /// includes:
    /// - Any Malformed syntax subtrees
    /// - Type errors
    /// - Configurable "rules" as defined in the Config (skipping the ones set
    ///   to RuleSeverityOrOff::Off)
    ///
    /// The implementation of check() should...
    /// 1. Recurse to all children of the current AST node (call check() on them)
    /// 2. Report any errors on the current node using report_error()
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F);
}

impl<TKind> Checkable for AST<TKind>
where
    TKind: Clone + TryFrom<Any> + TryFrom<TKind>,
    Any: From<TKind>,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
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

impl<T> Checkable for Option<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        if let Some(sel) = self {
            sel.check(ctx, report_error);
        }
    }
}

impl<T> Checkable for Vec<T>
where
    T: Checkable,
{
    fn check<'a, F: FnMut(BagelError)>(&self, ctx: &CheckContext<'a>, report_error: &mut F) {
        for el in self.iter() {
            el.check(ctx, report_error);
        }
    }
}

// impl<'a> CheckContext<'a> {
//     pub fn in_func(&self, func: AST<Func>) -> CheckContext<'a> {
//         CheckContext {
//             modules: self.modules,
//             current_module: self.current_module,
//             nearest_func_or_proc: Some(FuncOrProc::Func(func)),
//             in_expression_context: self.in_expression_context,
//         }
//     }

//     pub fn in_proc(&self, proc: AST<Proc>) -> CheckContext<'a> {
//         CheckContext {
//             modules: self.modules,
//             current_module: self.current_module,
//             nearest_func_or_proc: Some(FuncOrProc::Proc(proc)),
//             in_expression_context: self.in_expression_context,
//         }
//     }

//     pub fn in_expression_context(&self) -> Self {
//         CheckContext {
//             modules: self.modules,
//             current_module: self.current_module,
//             nearest_func_or_proc: self.nearest_func_or_proc.clone(),
//             in_expression_context: true,
//         }
//     }

//     pub fn in_statement_context(&self) -> Self {
//         CheckContext {
//             modules: self.modules,
//             current_module: self.current_module,
//             nearest_func_or_proc: self.nearest_func_or_proc.clone(),
//             in_expression_context: false,
//         }
//     }
// }
