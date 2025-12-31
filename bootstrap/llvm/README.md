# bootstrap/llvm

Hand-written LLVM IR seed compiler and build script. This directory holds the small, auditable seed compiler used to bootstrap the frontend.

How it works
- `purr0c.ll` is an LLVM IR module for a tiny emitter executable. When built, `purr0c` prints a small LLVM IR module (text) to stdout.
- The intended workflow is:
  1. Build `purr0c` from `purr0c.ll` (see `build.sh`).
  2. Run `./purr0c > program.ll` to generate an LLVM module for a purr0 program.
  3. Use `llc` + `clang` to compile `program.ll` to an executable.

Notes
- This emitter is intentionally small and hand-written; it is a starting point and
  should remain auditable. Replace it with a full emitter as you expand the
  bootstrap compiler.

