use bagel::ast::modules::ModulesStore;
use bagel::ast::slice::Slice;
use bagel::config::Config;
use bagel::emit::{EmitContext, Emittable};
use bagel::parse;
use insta::assert_snapshot;
use std::collections::HashMap;
use std::sync::Arc;

mod common;

fn test_emit(code: &str) {
    println!("----- input code -----\n{}\n----------------------", code);

    let slice = Slice::new(Arc::new(code.to_string()));
    let (_, parsed) = parse::module(slice).unwrap();
    let config = Config::default();
    let modules = ModulesStore {
        modules: HashMap::new(),
    };
    let mut emitted = String::new();
    let success = parsed.emit(
        EmitContext {
            config: &config,
            modules: &modules,
        },
        &mut emitted,
    );
    println!("{}", emitted);

    assert!(success.is_ok());
    assert_snapshot!(emitted);

    let re_parsed = parse::module(Slice::new(Arc::new(emitted.to_string())));
    assert!(re_parsed.is_ok());

    // TODO: Ensure it re-parsed to a structurally equivalent AST
}

#[test]
fn emit_test_1() {
    test_emit(common::samples::SAMPLE_1);
}
