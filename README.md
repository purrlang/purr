# purr

Minimal repository skeleton for the Purr language.

Repo layout:
- compiler/purrc0 — Rust bootstrap compiler
- purrc — self-hosted compiler (stub)
- runtime — minimal runtime
- stdlib — standard library
- spec — language + execution model specs
- examples — canonical examples
- tools — Python build/test tooling
- tests — compiler conformance tests

Entry points:
- tools/build.py — build a Purr program via purrc0
- tools/test.py — run conformance tests (stub)

See spec/SPEC.md, spec/BOOTSTRAP.md, and spec/MANIFESTO.md.
