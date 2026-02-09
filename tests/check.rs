use bagel::ast::modules::ModulesStore;
use bagel::ast::slice::Slice;
use bagel::check::{CheckContext, Checkable};
use bagel::config::Config;
use bagel::parse;
use insta::assert_debug_snapshot;
use std::collections::HashMap;
use std::sync::Arc;

mod common;

fn test_check(code: &str) {
    println!("----- input code -----\n{}\n----------------------", code);

    let slice = Slice::new(Arc::new(code.to_string()));
    let (_, parsed) = parse::module(slice).unwrap();

    let config = Config::default();
    let modules = ModulesStore {
        modules: HashMap::new(),
    };
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

    assert_debug_snapshot!(errors);
}

#[test]
fn parser_test_1() {
    test_check(common::samples::SAMPLE_1);
}
