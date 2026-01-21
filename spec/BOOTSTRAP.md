# Bootstrap & Trust Chain

This document explains the bootstrap ladder and trust chain used to move from a minimal Rust compiler to the fully self-hosted Purr compiler.

1. compiler/purrc0 — Rust bootstrap compiler (single-file, no actors).
2. purrc — self-hosted compiler (initially stubbed).

The idea is to keep small, auditable steps between each stage.
