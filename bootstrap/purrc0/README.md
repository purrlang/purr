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
4. Lower to simple IR

This compiler is intentionally small, explicit, and deterministic.
