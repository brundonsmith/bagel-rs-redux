use std::rc::Rc;
use bagel_language_server::ast::slice::Slice;
use bagel_language_server::parse::parse;

#[test]
fn test_binary_operation() {
    let code = "x + y * 2";
    let slice = Slice::new(Rc::new(code.to_string()));

    match parse::expression(slice) {
        Ok((remaining, ast)) => {
            println!("Parse successful!");
            println!("Input length: {}", code.len());
            println!("AST slice: start={}, end={}", ast.slice().start, ast.slice().end);
            println!("AST slice text: {:?}", ast.slice().as_str());
            println!("AST: {:#?}", ast);
            assert!(ast.slice().start <= ast.slice().end);
        }
        Err(e) => {
            panic!("Parse error: {:?}", e);
        }
    }
}
