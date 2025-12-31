# Authoritative Purr v1 Decisions

This document is the **authoritative resolution** to all currently open Purr design questions.
These decisions are **FINAL for Purr v1** unless explicitly revised in a later version.

Nothing outside this document is required to proceed with implementation.

---

# Purr v1 – FINAL DECISIONS

## 1) Purr v1 Scope

### Targets
- Native compilation via LLVM: **YES**
- WASM: **NO** (post-v1)
- Supported OS:
  - Linux
  - macOS
  - Windows

### Language Features
- Modules / imports: **YES**
- `result<T, E>`: **YES**
- `T?` (optional): **YES** (syntax sugar over `option<T>`)
- Lambdas: **YES** (expression-only, no captures)
- Generics: **NO**
- Effects and `do`: **YES** (syntax-level only)
- Concurrency (`actor/stream/spawn/await`): **RESERVED ONLY**
- FFI (C interop): **NO**

### Non-goals for v1
- Garbage collection
- async / await
- pattern matching
- inheritance / interfaces
- reflection
- macros
- custom allocators exposed to users

---

## 2) `SPEC.md` Status

`SPEC.md` MUST be normative, not descriptive.

`SPEC.md` MUST define:
- Lexing rules
- Grammar
- Evaluation order
- Type rules
- Overflow rules
- Equality rules
- Optional and result semantics

`SPEC.md` MUST NOT contain:
- Alternatives
- Philosophy
- Future design notes

If behavior is not specified, it is **undefined and unsupported**.

---

## 3) Lexical Decisions

### Comments
- Line comments only: `//`

### Whitespace
- Whitespace is insignificant
- Semicolons are **REQUIRED** to terminate statements

### Identifiers
- ASCII letters, digits, underscore
- Must not start with a digit
- No Unicode identifiers in v1

### Strings
- UTF-8 byte sequences
- Escape sequences: `\n` `\t` `\\` `\"`
- UTF-8 validity is **not enforced at runtime**

### Numeric Literals
- Base-10 only
- Signed literals only
- No numeric separators
- Literal overflow is a **compile-time error**

---

## 4) Type System (v1)

### Primitive types
- `bool`
- `i32`
- `i64`
- `f64`
- `string`
- `bytes`
- `void`

No unsigned integers.
No null.
No implicit conversions.

### Optionals
- `T?` is syntax sugar for `option<T>`
- Layout: `{ has: bool, value: T }`

### Results
- `result<T, E>` layout: `{ ok: bool, t: T, e: E }`
- Errors are values, never control flow

### Equality
`==` and `!=` are supported for:
- `bool`, `i32`, `i64`, `f64`
- `string` (bytewise)
- `struct`s (fieldwise)

### Overflow
- Integer overflow **TRAPS** at runtime

---

## 5) Control Flow

- `if` / `else` are statements only
- `while` is the only loop construct
- Evaluation order is strictly left-to-right
- Function arguments evaluate left-to-right
- `&&` and `||` short-circuit

---

## 6) Mutability Model

- Everything is immutable by default
- `let` is the ONLY binding keyword
- Rebinding allowed ONLY within `func` scope
- Rebinding never mutates shared state
- Struct fields are immutable

---

## 7) Lambdas

- Expression-only
- Explicit parameter and return types
- No captures
- No implicit effects
- Lowered to normal functions

Syntax:

```text
(x: i32): i32 => x + 1
```

---

## 8) Effects and `do`

- `do` is a syntactic effect boundary
- No effect typing in v1
- IO, time, randomness, allocation must occur inside `do`
- Stdlib effectful APIs REQUIRE `do`

---

## 9) Modules and Imports

- Every source file MUST declare exactly one `module <name>;`.
- Module names are fully-qualified dot-separated identifiers (logical namespace), not filesystem paths.
- Multiple files may declare the same module name; all such files form a single module and share one namespace.
- Import syntax:
  - `import <module>;`
  - `import <module> as <alias>;`
- Imports refer ONLY to fully-qualified module names; path-based/relative imports are forbidden.
- If no alias is specified, the alias defaults to the final segment of the module name; explicit aliasing is required on collisions.
- All references to imported symbols MUST be qualified with the module alias; unqualified access is forbidden.
- No visibility modifiers in v1.
- No implicit re-exports in v1.
- Cyclic imports are forbidden.

---

## 10) Runtime ABI (v1)

### `string`
- Layout: `{ ptr: *u8, len: i64 }`
- Immutable
- Runtime-owned

### `bytes`
- Layout: `{ ptr: *u8, len: i64 }`

### Structs
- Field order as declared
- Natural alignment
- No reordering

### Memory
- No GC
- Runtime allocator
- No destructors
- Leak-on-exit acceptable

---

## 11) Compiler Pipeline

Required pipeline:

1. Lex
2. Parse
3. Typecheck
4. Lower to IR
5. Emit LLVM IR
6. `llc` + platform linker (Linux/macOS/Windows)

---

## 12) Bootstrap Verification

- `purr0c` (handwritten LLVM IR) compiles `purrc.pu`
- `purrc` recompiles itself
- LLVM IR must be structurally equivalent
- Bit-for-bit binaries are NOT required in v1

---

# END OF DECISIONS