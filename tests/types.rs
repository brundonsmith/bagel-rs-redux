use bagel::ast::container::{walk_ast, WalkAction, AST};
use bagel::ast::grammar::{Any, Expression};
use bagel::ast::slice::Slice;
use bagel::parse::parse;
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

fn test_infer(code: &str) {
    println!("----- input code -----\n{}\n----------------------", code);

    let slice = Slice::new(Arc::new(code.to_string()));
    let (_, parsed) = parse::module(slice).unwrap();

    let mut results = BTreeMap::new();
    collect_expression_types(&parsed.upcast(), &mut results);

    let output = format_expression_types(&results);
    assert_snapshot!(output);
}

#[test]
fn parser_test_1() {
    test_infer(common::SAMPLE_1);
}
