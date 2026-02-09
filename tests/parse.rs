use bagel::ast::slice::Slice;
use bagel::parse;
use insta::assert_debug_snapshot;
use std::sync::Arc;

mod common;

fn test_parse(code: &str) {
    println!("----- input code -----\n{}\n----------------------", code);

    let slice = Slice::new(Arc::new(code.to_string()));

    assert_debug_snapshot!(parse::module(slice));
}

#[test]
fn parser_test_1() {
    test_parse(common::samples::SAMPLE_1);
}
