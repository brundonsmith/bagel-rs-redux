
# Project

This is an LSP for a new programming language called Bagel. It's statically typed and similar to TypeScript, and compiles to JavaScript, but will have some key differences.

# Best practices
- Do not run `cargo insta` to update test snapshots; I'll review and approve those myself
- Do not cat one-off Rust scripts just to test out your changes. If you're testing out new language functionality, add it to the samples under tests/
- Avoid using imperative control-flow like early returns, `continue`, etc. Even normal for-loops are not preferred most of the time (unless the body of the loop is itself doing something imperative, like writing to a stream); as much as is reasonable, write in a functional style (map, filter, fold, pattern-matching, etc)
- In parse/mod.rs stick to a parser-combinator-heavy style. Even local variables here are frowned upon; parser combinators should make almost all of those unnecessary.
- After you make a batch of changes, run `cargo fmt` to re-format the code
- DO NOT use `break` statements or early-returns unless the alternative code will be dramatically more complicated. Prefer iterator functions and functional patterns over explicit loops.
- Always gravitate towards the best, cleanest possible architecture given new information. Even if a ton of files have to be changed to accomodate an abstraction, that's okay. Do what's best, not what's minimally-changing.