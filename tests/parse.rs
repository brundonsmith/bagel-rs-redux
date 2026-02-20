use bagel::ast::slice::Slice;
use bagel::parse;
use insta::assert_snapshot;
use std::sync::Arc;

mod common;

fn test_parse(code: &str) -> String {
    let slice = Slice::new(Arc::new(code.to_string()));
    let result = parse::module(slice);
    format!("{}\n---\n{:#?}", code.trim(), result)
}

macro_rules! parse_test {
    ($name:ident, $sample:ident) => {
        #[test]
        fn $name() {
            assert_snapshot!(test_parse(common::samples::$sample));
        }
    };
}

parse_test!(literals, LITERALS);
parse_test!(arithmetic, ARITHMETIC);
parse_test!(comparison_and_logic, COMPARISON_AND_LOGIC);
parse_test!(declarations, DECLARATIONS);
parse_test!(imports, IMPORTS);
parse_test!(functions, FUNCTIONS);
parse_test!(function_types, FUNCTION_TYPES);
parse_test!(if_else, IF_ELSE);
parse_test!(collections, COLLECTIONS);
parse_test!(property_access, PROPERTY_ACCESS);
parse_test!(invocations, INVOCATIONS);
parse_test!(type_annotations, TYPE_ANNOTATIONS);
parse_test!(type_errors, TYPE_ERRORS);
parse_test!(comments, COMMENTS);
parse_test!(blank_lines, BLANK_LINES);
parse_test!(pipe_call, PIPE_CALL);
parse_test!(bad_syntax, BAD_SYNTAX);
parse_test!(unparseable_junk, UNPARSEABLE_JUNK);
parse_test!(tuple_types, TUPLE_TYPES);
parse_test!(tuple_type_errors, TUPLE_TYPE_ERRORS);
parse_test!(union_types, UNION_TYPES);
parse_test!(union_type_errors, UNION_TYPE_ERRORS);
parse_test!(named_types, NAMED_TYPES);
parse_test!(named_type_errors, NAMED_TYPE_ERRORS);
