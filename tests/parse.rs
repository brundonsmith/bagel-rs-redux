use bagel_language_server::ast::slice::Slice;
use bagel_language_server::parse::parse;
use insta::assert_debug_snapshot;
use std::rc::Rc;

fn test_parse(code: &str) {
    println!("----- input code -----\n{}\n----------------------", code);

    let slice = Slice::new(Rc::new(code.to_string()));

    assert_debug_snapshot!(parse::module(slice));
}

#[test]
fn parser_test_1() {
    test_parse(
        "const x = 42
const y = true
const z = false
const a = nil
const b = y * 2 + x",
    );
}
