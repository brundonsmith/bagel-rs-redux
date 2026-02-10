use std::fmt::Write;
use std::sync::{Arc, RwLock};

use crate::{
    ast::{
        container::{ASTInner, AST},
        grammar::{self, Any, Declaration},
        modules::ModulesStore,
    },
    compile::{Compilable, CompileContext},
    config::Config,
};

#[derive(Debug, Clone, Copy)]
pub struct BundleContext<'a> {
    pub config: &'a Config,
    pub modules: &'a ModulesStore,
}

pub fn bundle<'a, W: Write>(ctx: BundleContext<'a>, f: &mut W) -> core::fmt::Result {
    for module in ctx.modules.topological_sort() {
        module.ast.clone().without_imports().compile(
            CompileContext {
                config: ctx.config,
                modules: ctx.modules,
            },
            f,
        )?;
    }

    writeln!(f, "main();")?;

    Ok(())
}

impl AST<grammar::Module> {
    pub fn without_imports(self) -> Self {
        match self.unpack() {
            None => self,
            Some(module) => {
                let filtered = module
                    .declarations
                    .into_iter()
                    .filter(|decl| {
                        !matches!(decl.unpack(), Some(Declaration::ImportDeclaration(_)))
                    })
                    .collect();

                AST::new(Arc::new(ASTInner {
                    parent: Arc::new(RwLock::new(None)),
                    slice: self.slice().clone(),
                    details: Any::Module(grammar::Module {
                        declarations: filtered,
                    }),
                }))
            }
        }
    }
}
