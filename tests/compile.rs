use bagel::ast::modules::{Module, ModulePath, ModulesStore};
use bagel::ast::slice::Slice;
use bagel::compile::{Compilable, CompileContext};
use bagel::config::Config;
use bagel::parse;
use insta::assert_snapshot;
use std::sync::Arc;

mod common;

fn test_compile(code: &str) -> String {
    let slice = Slice::new(Arc::new(code.to_string()));
    match parse::module(slice.clone()) {
        Err(err) => format!("{}\n---\nParse error: {:#?}", code.trim(), err),
        Ok((_, parsed)) => {
            let config = Config::default();
            let mut modules = ModulesStore::new();

            let path = ModulePath::Mock("Foo".into());
            modules.insert(
                path.clone(),
                Module {
                    path: path.clone(),
                    source: slice,
                    ast: parsed.clone(),
                },
            );

            let current_module = modules.get(&path).unwrap();
            let mut compiled = String::new();
            let success = parsed.compile(
                CompileContext {
                    config: &config,
                    modules: &modules,
                    prefix_identifiers_with_module_ids: true,
                    current_module: Some(current_module),
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
compile_test!(comments, COMMENTS);
compile_test!(blank_lines, BLANK_LINES);
compile_test!(pipe_call, PIPE_CALL);
compile_test!(bad_syntax, BAD_SYNTAX);
compile_test!(unparseable_junk, UNPARSEABLE_JUNK);
