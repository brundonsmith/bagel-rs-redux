use bagel::ast::modules::ModulesStore;
use bagel::ast::slice::Slice;
use bagel::check::{CheckContext, Checkable};
use bagel::config::Config;
use bagel::parse;
use insta::assert_snapshot;
use std::sync::Arc;

mod common;

fn test_check(code: &str) -> String {
    let slice = Slice::new(Arc::new(code.to_string()));
    match parse::module(slice) {
        Err(err) => format!("{}\n---\nParse error: {:#?}", code.trim(), err),
        Ok((_, parsed)) => {
            let config = Config::default();
            let modules = ModulesStore::new();
            let mut errors = Vec::new();
            let errors_ref = &mut errors;
            parsed.check(
                &CheckContext {
                    config: &config,
                    modules: &modules,
                    current_module: None,
                },
                &mut |e| errors_ref.push(e),
            );

            format!("{}\n---\n{:#?}", code.trim(), errors)
        }
    }
}

macro_rules! check_test {
    ($name:ident, $sample:ident) => {
        #[test]
        fn $name() {
            assert_snapshot!(test_check(common::samples::$sample));
        }
    };
}

check_test!(literals, LITERALS);
check_test!(arithmetic, ARITHMETIC);
check_test!(comparison_and_logic, COMPARISON_AND_LOGIC);
check_test!(declarations, DECLARATIONS);
check_test!(imports, IMPORTS);
check_test!(functions, FUNCTIONS);
check_test!(function_types, FUNCTION_TYPES);
check_test!(if_else, IF_ELSE);
check_test!(collections, COLLECTIONS);
check_test!(property_access, PROPERTY_ACCESS);
check_test!(invocations, INVOCATIONS);
check_test!(type_annotations, TYPE_ANNOTATIONS);
check_test!(type_errors, TYPE_ERRORS);
check_test!(comments, COMMENTS);
check_test!(blank_lines, BLANK_LINES);
check_test!(pipe_call, PIPE_CALL);
check_test!(bad_syntax, BAD_SYNTAX);
check_test!(unparseable_junk, UNPARSEABLE_JUNK);
check_test!(tuple_types, TUPLE_TYPES);
check_test!(tuple_type_errors, TUPLE_TYPE_ERRORS);
check_test!(union_types, UNION_TYPES);
check_test!(union_type_errors, UNION_TYPE_ERRORS);
check_test!(named_types, NAMED_TYPES);
check_test!(named_type_errors, NAMED_TYPE_ERRORS);
