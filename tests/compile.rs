use bagel::ast::modules::ModulesStore;
use bagel::ast::slice::Slice;
use bagel::compile::{Compilable, CompileContext};
use bagel::config::Config;
use bagel::parse;
use insta::assert_snapshot;
use std::collections::HashMap;
use std::sync::Arc;

mod common;

fn test_compile(code: &str) -> String {
    let slice = Slice::new(Arc::new(code.to_string()));
    match parse::module(slice) {
        Err(err) => format!("{}\n---\nParse error: {:#?}", code.trim(), err),
        Ok((_, parsed)) => {
            let config = Config::default();
            let modules = ModulesStore {
                modules: HashMap::new(),
            };
            let mut compiled = String::new();
            let success = parsed.compile(
                CompileContext {
                    config: &config,
                    modules: &modules,
                },
                &mut compiled,
            );

            match success {
                Err(err) => format!("{}\n---\nCompile error: {:#?}", code.trim(), err),
                Ok(()) => format!("{}\n---\n{}", code.trim(), compiled),
            }
        }
    }
}

macro_rules! compile_test {
    ($name:ident, $sample:ident) => {
        #[test]
        fn $name() {
            assert_snapshot!(test_compile(common::samples::$sample));
        }
    };
}

compile_test!(literals, LITERALS);
compile_test!(arithmetic, ARITHMETIC);
compile_test!(comparison_and_logic, COMPARISON_AND_LOGIC);
compile_test!(declarations, DECLARATIONS);
compile_test!(imports, IMPORTS);
compile_test!(functions, FUNCTIONS);
compile_test!(function_types, FUNCTION_TYPES);
compile_test!(if_else, IF_ELSE);
compile_test!(collections, COLLECTIONS);
compile_test!(property_access, PROPERTY_ACCESS);
compile_test!(invocations, INVOCATIONS);
compile_test!(type_annotations, TYPE_ANNOTATIONS);
compile_test!(type_errors, TYPE_ERRORS);
compile_test!(bad_syntax, BAD_SYNTAX);
