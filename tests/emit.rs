use bagel::ast::modules::ModulesStore;
use bagel::ast::slice::Slice;
use bagel::config::Config;
use bagel::emit::{EmitContext, Emittable};
use bagel::parse;
use insta::assert_snapshot;
use std::sync::Arc;

mod common;

fn test_emit(code: &str) -> String {
    let slice = Slice::new(Arc::new(code.to_string()));
    match parse::module(slice) {
        Err(err) => format!("{}\n---\nParse error: {:#?}", code.trim(), err),
        Ok((_, parsed)) => {
            let config = Config::default();
            let modules = ModulesStore::new();
            let mut emitted = String::new();
            let success = parsed.emit(
                EmitContext {
                    config: &config,
                    modules: &modules,
                },
                &mut emitted,
            );

            match success {
                Err(err) => format!("{}\n---\nEmit error: {:#?}", code.trim(), err),
                Ok(()) => {
                    let re_parsed = parse::module(Slice::new(Arc::new(emitted.to_string())));
                    let re_parse_note = match re_parsed {
                        Ok(_) => String::new(),
                        Err(err) => format!("\n---\nRe-parse error: {:#?}", err),
                    };

                    // TODO: Ensure it re-parsed to a structurally equivalent AST

                    format!("{}\n---\n{}{}", code.trim(), emitted, re_parse_note)
                }
            }
        }
    }
}

macro_rules! emit_test {
    ($name:ident, $sample:ident) => {
        #[test]
        fn $name() {
            assert_snapshot!(test_emit(common::samples::$sample));
        }
    };
}

emit_test!(literals, LITERALS);
emit_test!(arithmetic, ARITHMETIC);
emit_test!(comparison_and_logic, COMPARISON_AND_LOGIC);
emit_test!(declarations, DECLARATIONS);
emit_test!(imports, IMPORTS);
emit_test!(functions, FUNCTIONS);
emit_test!(function_types, FUNCTION_TYPES);
emit_test!(if_else, IF_ELSE);
emit_test!(collections, COLLECTIONS);
emit_test!(property_access, PROPERTY_ACCESS);
emit_test!(invocations, INVOCATIONS);
emit_test!(type_annotations, TYPE_ANNOTATIONS);
emit_test!(type_errors, TYPE_ERRORS);
emit_test!(bad_syntax, BAD_SYNTAX);
