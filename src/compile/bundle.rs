use std::fmt::Write;

use crate::{
    ast::modules::{Module, ModulesStore},
    compile::{Compilable, CompileContext},
    config::Config,
};

#[derive(Debug, Clone, Copy)]
pub struct BundleContext<'a> {
    pub config: &'a Config,
    pub modules: &'a ModulesStore,
    pub entry_module: &'a Module,
}

pub fn bundle<'a, W: Write>(ctx: BundleContext<'a>, f: &mut W) -> core::fmt::Result {
    for module in ctx.modules.topological_sort() {
        module.ast.clone().compile(
            CompileContext {
                config: ctx.config,
                modules: ctx.modules,
                prefix_identifiers_with_module_ids: true,
                current_module: Some(module),
            },
            f,
        )?;
    }

    write!(
        f,
        "module_{}_main()",
        ctx.modules.module_id(&ctx.entry_module.path).unwrap()
    )?;

    Ok(())
}
