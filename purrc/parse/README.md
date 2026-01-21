# purrc/parse

Parser for purr0. This module will implement a small, deterministic parser for the
frozen purr0 grammar. Goals for the first pass:

- Parse top-level `struct` and `func` declarations
- Parse `let` bindings, `if`, `while`, `return`, and expression forms used by the compiler
- Support `T?` shorthand by rewriting it to `option<T>` in the parser
- Support expression-only **lambdas**: `(x: i32): i32 => x + 1`, rewritten to an anonymous top-level `func` node during parsing
- Produce an explicit AST suitable for unit tests and straightforward lowering to IR

This README tracks the incremental plan and links to tests in `compiler/tests/parse`.
