use bagel::ast::container::AST;
use bagel::ast::grammar::{Any, Expression};
use bagel::ast::slice::Slice;
use bagel::parse::parse;
use bagel::types::infer::InferTypeContext;
use insta::assert_snapshot;
use std::collections::BTreeMap;
use std::sync::Arc;

mod common;

/// Recursively walks an AST and collects all expressions with their inferred types
fn collect_expression_types(ast: &AST<Any>, results: &mut BTreeMap<String, String>) {
    // Try to downcast to Expression
    if let Some(expr) = ast.clone().try_downcast::<Expression>() {
        let ctx = InferTypeContext {};
        let inferred_type = expr.infer_type(ctx);
        let code = expr.slice().as_str().to_string();
        let type_str = format!("{}", inferred_type);
        results.insert(code, type_str);
    }

    // Recursively visit children
    if let Some(details) = ast.details() {
        match details {
            Any::Module(module) => {
                for decl in &module.declarations {
                    collect_expression_types(&decl.clone().upcast(), results);
                }
            }
            Any::Declaration(decl) => {
                collect_expression_types(&decl.value.clone().upcast(), results);
            }
            Any::Expression(expr) => {
                use bagel::ast::grammar::Expression::*;
                match expr {
                    BinaryOperation(bin_op) => {
                        collect_expression_types(&bin_op.left.clone().upcast(), results);
                        collect_expression_types(&bin_op.right.clone().upcast(), results);
                    }
                    LocalIdentifier(_local_id) => {
                        // LocalIdentifier doesn't have sub-expressions
                    }
                    Invocation(inv) => {
                        collect_expression_types(&inv.function.clone().upcast(), results);
                        for arg in &inv.arguments {
                            collect_expression_types(&arg.clone().upcast(), results);
                        }
                    }
                    FunctionExpression(func) => {
                        collect_expression_types(&func.body.clone().upcast(), results);
                    }
                    _ => {
                        // Leaf expressions (literals) have no children
                    }
                }
            }
            _ => {
                // Other node types don't contain expressions
            }
        }
    }
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
