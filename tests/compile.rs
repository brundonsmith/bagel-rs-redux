use bagel::ast::modules::ModulesStore;
use bagel::ast::slice::Slice;
use bagel::compile::{Compilable, CompileContext};
use bagel::config::Config;
use bagel::parse::parse;
use insta::assert_snapshot;
use std::collections::HashMap;
use std::sync::Arc;

mod common;

fn test_compile(code: &str) {
    println!("----- input code -----\n{}\n----------------------", code);

    let slice = Slice::new(Arc::new(code.to_string()));
    let (_, parsed) = parse::module(slice).unwrap();
    let config = Config::default();
    let modules = ModulesStore {
        modules: HashMap::new(),
    };
    let mut compiled = String::new();
    let success = parsed.compile(
        CompileContext {
            config: &config,
            modules: &modules,
        },
        &mut compiled,
    );
    println!("{}", compiled);

    assert!(success.is_ok());
    assert_snapshot!(compiled);
}

#[test]
fn compile_test_1() {
    test_compile(common::SAMPLE_1);
}
