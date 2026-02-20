use bagel::ast::container::{walk_ast, WalkAction, AST};
use bagel::ast::grammar::{Any, Expression};
use bagel::ast::slice::Slice;
use bagel::parse;
use bagel::types::infer::InferTypeContext;
use bagel::types::NormalizeContext;
use insta::assert_snapshot;
use std::collections::BTreeMap;
use std::sync::Arc;

mod common;

/// Recursively walks an AST and collects all expressions with their inferred types
fn collect_expression_types(ast: &AST<Any>, results: &mut BTreeMap<String, String>) {
    walk_ast(ast, &mut |node| {
        if let Some(expr) = node.clone().try_downcast::<Expression>() {
            if expr.is_valid() {
                let ctx = InferTypeContext {
                    modules: None,
                    current_module: None,
                };
                let norm_ctx = NormalizeContext {
                    modules: None,
                    current_module: None,
                };
                let inferred_type = expr.infer_type(ctx).normalize(norm_ctx);
                let code = expr.slice().as_str().to_string();
                let type_str = format!("{}", inferred_type);
                results.insert(code, type_str);
            }
        }
        WalkAction::Continue
    });
}

/// Formats collected expression types as a string with one line per expression
fn format_expression_types(results: &BTreeMap<String, String>) -> String {
    results
        .iter()
        .map(|(code, type_str)| format!("{} -> {}", code, type_str))
        .collect::<Vec<_>>()
        .join("\n")
}

fn test_infer(code: &str) -> String {
    let slice = Slice::new(Arc::new(code.to_string()));
    match parse::module(slice) {
        Err(err) => format!("{}\n---\nParse error: {:#?}", code.trim(), err),
        Ok((_, parsed)) => {
            let mut results = BTreeMap::new();
            collect_expression_types(&parsed.upcast(), &mut results);

            let types_output = format_expression_types(&results);
            format!("{}\n---\n{}", code.trim(), types_output)
        }
    }
}

macro_rules! infer_test {
    ($name:ident, $sample:ident) => {
        #[test]
        fn $name() {
            assert_snapshot!(test_infer(common::samples::$sample));
        }
    };
}

infer_test!(literals, LITERALS);
infer_test!(arithmetic, ARITHMETIC);
infer_test!(comparison_and_logic, COMPARISON_AND_LOGIC);
infer_test!(declarations, DECLARATIONS);
infer_test!(imports, IMPORTS);
infer_test!(functions, FUNCTIONS);
infer_test!(function_types, FUNCTION_TYPES);
infer_test!(if_else, IF_ELSE);
infer_test!(collections, COLLECTIONS);
infer_test!(property_access, PROPERTY_ACCESS);
infer_test!(invocations, INVOCATIONS);
infer_test!(type_annotations, TYPE_ANNOTATIONS);
infer_test!(type_errors, TYPE_ERRORS);
infer_test!(comments, COMMENTS);
infer_test!(blank_lines, BLANK_LINES);
infer_test!(pipe_call, PIPE_CALL);
infer_test!(bad_syntax, BAD_SYNTAX);
infer_test!(unparseable_junk, UNPARSEABLE_JUNK);
infer_test!(tuple_types, TUPLE_TYPES);
infer_test!(tuple_type_errors, TUPLE_TYPE_ERRORS);
infer_test!(union_types, UNION_TYPES);
infer_test!(union_type_errors, UNION_TYPE_ERRORS);
infer_test!(named_types, NAMED_TYPES);
infer_test!(named_type_errors, NAMED_TYPE_ERRORS);
