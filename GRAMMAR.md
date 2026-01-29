# The Bagel Programming Language

## Grammar

The following is an approximate notation of Bagel's valid grammar. It incorporates some JavaScript RegEx notation in places, because that's familiar to the author:

```
Any = Module | Declaration | Expression

Module = Declaration+

Declaration = "const" PlainIdentifier "=" Expression

Expression = NilLiteral | BooleanLiteral | NumberLiteral | LocalIdentifier | BinaryOperation | Invocation
Invocation = Expression "(" Expression (?:"," Expression)* ","? ")"
FunctionExpression = (?:"(" PlainIdentifier (?:"," PlainIdentifier)* ","? ")") or PlainIdentifier "=>" Expression
BinaryOperation = Expression BinaryOperator Expression
BinaryOperator = "+" | "-" | "*" | "/"
NilLiteral = "nil"
BooleanLiteral = "true" | "false"
NumberLiteral = [0-9]+(?:\.[0-9]+)?
LocalIdentifier = PlainIdentifier
PlainIdentifier = [a-z]+
```

"Module" represents an entire code file/document. This should be the root node of the AST when parsing a whole document.

This grammar should be used for the following things:

### 1. Generate data structures to model the AST

Each AST node should get its own data type (either struct or enum, as appropriate).
- *Every single node defined in the grammar* (in the format `Foo =`) should get its own AST node that's part of the grammar and represented in the hierarchy, and eventually wrapped in an `AST<>` struct (see following for more details).
- Repeating sequences within a node should be represented with a `Vec`, and optional nodes should be represented with an `Option`
- Any exact strings parsed out of the original document unchanged should be stored as a `Slice` that refers to the document, instead of allocating a new `String`
- Each token in the parsed node should be represented with its own `Slice`. If it represents an entire child AST node, then that `AST` struct will include its own `Slice` already. But for example things like opening and closing braces, parenthesis, etc, should get their own `Slice`s even though they aren't full AST nodes.
- Special exact-strings that exist in the grammar should get syntax highlighting accordingly. They should also be forbidden from being used as identifiers.

The `type_hierarchy!` macro is used to generate a hierarchy of types. You will use this to represent AST nodes that only represent groups of possible other nodes. So for example,
```
Any = Expression | Statement
Expression = NumberLiteral | StringLiteral | BinaryOperation | FunctionInvocation
Statement = VariableAssignment | FunctionInvocation
NumberLiteral = [0-9]+(?:\.[0-9]+)?
...
```
```rust
type_hierarchy! {
    Any {
        Expression {
            NumberLiteral,
            StringLiteral,
            BinaryOperation,
            FunctionInvocation,
        },
        Statement {
            VariableAssignment,
            FunctionInvocation,
        }
    }
}
```

This will produce enums that wrap each level of node, and implementations of `From` and `TryFrom` that allow casting between supersets and members of a set.

#### `AST<TKind>` and `ASTInner`

These data structures will just represent the semantic info that's specific to each AST node. For info that exists for _every_ node, look to `AST<TKind>` and `ASTInner`, declared in `container.rs`. `ASTInner` contains all metadata that _every_ node carries, including a weak reference to its `parent` node and the original `Slice` of the source code that it came from.

`AST<TKind>` holds an `ASTInner`. It wraps it in an `Rc` so it can be cheaply cloned, and includes `PhantomData` to represent the specific type of AST node contained within. This allows us to perform the same casting that our "real" AST can do, but without unwrapping or cloning the `Rc`. It's all type-shuffling, but it leans on the same trait implementations created by the macro.

`AST<TKind>` also implements a `Parentable` trait. This is a convenience trait to allow setting `parent` easily, and even transiently through `Option` and `Vec`, with a single method call.

In practice, everywhere we actually work with AST nodes in the code should take the form of an `AST<TKind>`. This includes parsing results, and even inner references from one AST node to the next. Example:
```
Invocation = Expression "(" Expression ","... ")"
```
```rust
struct Invocation {
    function: AST<Expression>,
    arguments: Vec<AST<Expression>>,
}
```

### 2. Write a parser

The parser should live in `src/parse/parse.rs` and use the `nom` library (version 7). Each AST node should get its own parser function, which takes a `Slice` and returns a `ParseResult<AST<TKind>>` where `TKind` is the expected AST node struct (or enum).

- Utilize the functions and types in `src/parse/utils.rs` wherever they make sense. In particular, see `expect()` for requiring pieces of syntax and giving an error if they're missing.
- Be sure to call `set_parent()` on all newly-created child nodes
- Whenever parsing something that involves matching braces or parenthesis, if you encounter a syntax error, attempt to recover by walking until you hit the closing brace that matches the open one. If you do, include a `Malformed` node in the tree referencing the failed Slice. This will be useful for the LSP.

### 3. Generate a TextMate grammar

The grammar file is `vscode-extension/syntaxes/bagel.tmLanguage.json`. Keep `vscode-extension/language-configuration.json` updated too.

## General practices
- After writing your code, use `cargo build` and `cargo test` to validate the changes you just made and iterate until things pass
- If the files you're generating already exist, don't re-write them from scratch, make only the necessary changes to bring them into alignment with this spec (and fix errors) while keeping them otherwise the same