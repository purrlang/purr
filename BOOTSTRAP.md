# Bootstrap & Trust Chain

This document explains the bootstrap ladder and trust chain used to move from a minimal hand-written compiler to the fully self-hosted compiler implementation.

1. `bootstrap/purrc0` - host-language bootstrap compiler (single-file, no actors).
2. `bootstrap/purr0` - frozen, minimal subset used to seed a stage-1 compiler.
3. `bootstrap/llvm` - hand-written LLVM IR that serves as a seed compiler (`purr0c`).
4. Stage-1: build a compiler with `purr0` that emits LLVM; compare results and iterate.

The idea is to keep small, auditable steps between each stage.
