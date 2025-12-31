#!/usr/bin/env bash
set -euo pipefail

# Test script for the purr0 emitter workflow.
# Steps:
# 1) Build the emitter (purr0c) from purr0c.ll
# 2) Run emitter to produce program.ll
# 3) Lower program.ll to object and link to an executable
# 4) Run the produced executable

# Check for required commands early and give helpful messages.
REQUIRED_CMDS=(llc clang sed)
for cmd in "${REQUIRED_CMDS[@]}"; do
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "Error: required command '$cmd' not found in PATH." >&2
    echo "On Windows, you can install LLVM and ensure 'llc' and 'clang' are on PATH." >&2
    echo "On Linux/macOS, install the LLVM tools (e.g., apt install llvm clang)." >&2
    exit 2
  fi
done

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$SCRIPT_DIR"

echo "== Building purr0c =="
bash build.sh purr0c.ll

if [ ! -x "./purr0c" ]; then
  echo "Error: ./purr0c not found or not executable" >&2
  exit 2
fi

echo "== Running emitter to generate program.ll =="
./purr0c > program.ll

echo "-- program.ll (first 200 lines) --"
sed -n '1,200p' program.ll || true

echo "== Compiling program.ll =="
llc -filetype=obj program.ll -o program.o
clang -o program program.o -lm

echo "== Running generated program =="
./program
EXIT_CODE=$?

echo "== Program exited with code $EXIT_CODE =="
if [ "$EXIT_CODE" -ne 0 ]; then
  echo "Error: generated program did not exit cleanly" >&2
  exit $EXIT_CODE
fi

echo "Emitter workflow succeeded."
