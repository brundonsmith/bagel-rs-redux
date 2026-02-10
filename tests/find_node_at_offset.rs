use bagel::ast::container::find_deepest;
use bagel::ast::grammar::Any;
use bagel::ast::slice::Slice;
use bagel::parse;
use insta::assert_snapshot;
use std::sync::Arc;

mod common;

#[test]
fn find_node_at_offset() {
    let code = common::samples::FUNCTIONS;
    let slice = Slice::new(Arc::new(code.to_string()));
    let (_, ast) = parse::module(slice).unwrap();
    let ast = ast.upcast::<Any>();

    let lines: String = (0..code.len())
        .map(|i| {
            let preview: String = code[i..].chars().take(10).collect();
            let preview = preview.replace('\n', "\\n");
            let node = find_deepest(&ast, &|node| {
                let s = node.slice();
                i >= s.start && i <= s.end
            });
            let node_str = match &node {
                Some(n) => format!("{:?}", n),
                None => "None".to_string(),
            };
            format!("{:>3} | {:>12} | {}", i, preview, node_str)
        })
        .collect::<Vec<_>>()
        .join("\n");

    assert_snapshot!(format!("{}\n---\n{}", code.trim(), lines));
}
