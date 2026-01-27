use std::rc::Rc;
use bagel_language_server::ast::slice::Slice;
use bagel_language_server::parse::parse;

#[test]
fn test_slice_boundaries() {
    let code = "x + y";
    let full = Slice::new(Rc::new(code.to_string()));

    println!("Full slice: start={}, end={}, text={:?}", full.start, full.end, full.as_str());

    // Parse just the identifier
    match parse::local_identifier(full) {
        Ok((remaining, x_ast)) => {
            println!("Parsed 'x':");
            println!("  x_ast slice: start={}, end={}, text={:?}",
                x_ast.slice().start, x_ast.slice().end, x_ast.slice().as_str());
            println!("  remaining: start={}, end={}, text={:?}",
                remaining.start, remaining.end, remaining.as_str());

            // Now try to parse from the remaining
            let remaining2 = remaining.slice_range(2, None); // Skip " + "
            println!("\nAfter skipping ' + ':");
            println!("  remaining2: start={}, end={}, text={:?}",
                remaining2.start, remaining2.end, remaining2.as_str());
        }
        Err(e) => panic!("Parse error: {:?}", e),
    }
}
