# compiler/purrc

Self-hosted compiler implementation of Purr (`purrc`).

Layout:
- `main.pu` — compiler entry point
- `parse/` — parser
- `ast/` — AST nodes and utilities
- `typecheck/` — type checker
- `lower/` — lowering/transforms
- `emit/` — codegen (e.g., `llvm.pu`)
- `diagnostics/` — error messages and tests
