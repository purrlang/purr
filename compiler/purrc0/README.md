# purrc0 (bootstrap compiler)

Host-language bootstrap compiler for Purr.

Scope (v0 bootstrap):
- Single source file
- Frozen keyword set only
- Structs, enums, funcs, var, control flow
- No actors (stub errors only)
- No interfaces/impl

Pipeline:
1. Lex
2. Parse (AST with spans)
3. Typecheck (minimal)
4. Emit C

This compiler is intentionally small, explicit, and deterministic.

Layout:
- src/ast.rs — AST nodes
- src/lexer.rs — lexer
- src/parser.rs — parser
- src/typecheck.rs — minimal semantic checks
- src/emit_c.rs — C emitter
- rt/ — tiny C runtime shim

Bootstrap helper:
- tools/build.py — build the canonical hello example (requires a C compiler)
