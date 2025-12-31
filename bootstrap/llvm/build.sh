#!/usr/bin/env bash
# build.sh - build purr0c.exe from purr0c.ll using llc + clang
set -euo pipefail

FILE=${1:-purr0c.ll}
if [ ! -f "$FILE" ]; then
  echo "Error: file '$FILE' not found" >&2
  echo "Usage: $0 [purr0c.ll]" >&2
  exit 2
fi

llc -filetype=obj "$FILE" -o purr0c.o
clang -o purr0c purr0c.o -lm

echo "Built purr0c from $FILE"
