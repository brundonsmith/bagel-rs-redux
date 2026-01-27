use bagel_language_server::ast::slice::Slice;
use bagel_language_server::parse::parse;
use insta::assert_debug_snapshot;
use std::rc::Rc;

#[test]
fn test_simple_declaration() {
    let code = "const x = 42";
    let slice = Slice::new(Rc::new(code.to_string()));

    match parse::declaration(slice) {
        Ok((remaining, ast)) => {
            println!("Parse successful!");
            println!("Input length: {}", code.len());
            println!(
                "AST slice: start={}, end={}",
                ast.slice().start,
                ast.slice().end
            );
            println!("AST slice text: {:?}", ast.slice().as_str());
            println!(
                "Remaining: start={}, end={}",
                remaining.start, remaining.end
            );
            assert!(ast.slice().start <= ast.slice().end);
        }
        Err(e) => {
            panic!("Parse error: {:?}", e);
        }
    }
}

#[test]
fn test_multiline_code() {
    let code = "const x = 42
const y = true
const z = false
const a = nil
const b = y * 2 + x";

    println!("Input code:\n{}\n", code);
    println!("Code length: {}", code.len());

    let slice = Slice::new(Rc::new(code.to_string()));

    assert_debug_snapshot!(parse::any(slice));
}
