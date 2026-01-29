use bagel_language_server::ast::slice::Slice;
use bagel_language_server::compile::{Compilable, CompileContext};
use bagel_language_server::config::Config;
use bagel_language_server::parse::parse;
use insta::assert_snapshot;
use std::sync::Arc;

mod common;

fn test_compile(code: &str) {
    println!("----- input code -----\n{}\n----------------------", code);

    let slice = Slice::new(Arc::new(code.to_string()));
    let (_, parsed) = parse::module(slice).unwrap();
    let config = Config::default();
    let mut compiled = String::new();
    let success = parsed.compile(CompileContext { config: &config }, &mut compiled);
    println!("{}", compiled);

    assert!(success.is_ok());
    assert_snapshot!(compiled);
}

#[test]
fn compile_test_1() {
    test_compile(common::SAMPLE_1);
}
