use bagel_language_server::ast::slice::Slice;
use bagel_language_server::config::Config;
use bagel_language_server::emit::{EmitContext, Emittable};
use bagel_language_server::parse::parse;
use insta::assert_snapshot;
use std::sync::Arc;

mod common;

fn test_emit(code: &str) {
    println!("----- input code -----\n{}\n----------------------", code);

    let slice = Slice::new(Arc::new(code.to_string()));
    let (_, parsed) = parse::module(slice).unwrap();
    let config = Config::default();
    let mut emitted = String::new();
    let success = parsed.emit(EmitContext { config: &config }, &mut emitted);
    println!("{}", emitted);

    assert!(success.is_ok());
    assert_snapshot!(emitted);

    let re_parsed = parse::module(Slice::new(Arc::new(emitted.to_string())));
    assert!(re_parsed.is_ok());

    // TODO: Ensure it re-parsed to a structurally equivalent AST
}

#[test]
fn emit_test_1() {
    test_emit(common::SAMPLE_1);
}
