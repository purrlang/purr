#!/usr/bin/env bash
set -euo pipefail

# Convenience wrapper to run the bootstrap emitter test from repo root
DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
"$DIR/bootstrap/llvm/test_emitter.sh"
