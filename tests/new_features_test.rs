use std::rc::Rc;
use bagel_language_server::ast::slice::Slice;
use bagel_language_server::parse::parse;

#[test]
fn test_boolean_literal() {
    let code = "true";
    let slice = Slice::new(Rc::new(code.to_string()));

    match parse::boolean_literal(slice) {
        Ok((remaining, ast)) => {
            println!("Parse successful!");
            println!("AST slice text: {:?}", ast.slice().as_str());
            assert_eq!(ast.slice().as_str(), "true");
        }
        Err(e) => {
            panic!("Parse error: {:?}", e);
        }
    }

    let code = "false";
    let slice = Slice::new(Rc::new(code.to_string()));

    match parse::boolean_literal(slice) {
        Ok((remaining, ast)) => {
            println!("Parse successful!");
            println!("AST slice text: {:?}", ast.slice().as_str());
            assert_eq!(ast.slice().as_str(), "false");
        }
        Err(e) => {
            panic!("Parse error: {:?}", e);
        }
    }
}

#[test]
fn test_module() {
    let code = "const x = 42
const y = true
const z = nil";

    let slice = Slice::new(Rc::new(code.to_string()));

    match parse::module(slice) {
        Ok((remaining, ast)) => {
            println!("Parse successful!");
            println!("Remaining: {:?}", remaining.as_str());
            println!("AST: {:#?}", ast);
            assert!(ast.slice().start <= ast.slice().end);
        }
        Err(e) => {
            panic!("Parse error: {:?}", e);
        }
    }
}
