#!/usr/bin/env bash
# build.sh - build purr0c.exe from purr0c.ll using llc + clang
set -e

if [ -z "${1}" ]; then
  echo "Usage: $0 purr0c.ll"
  exit 2
fi

llc -filetype=obj "$1" -o purr0c.o
clang -o purr0c purr0c.o -lm

echo "Built purr0c"
