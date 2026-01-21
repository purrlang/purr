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
- Namespaces / `use`: **YES**
- `result<T, E>`: **YES** (predeclared type constructor)
- `T?` (optional): **YES** (syntax sugar over `option<T>`)
- Lambdas: **YES** (expression-only, no captures)
- Generics: **NO** (except predeclared type constructors)
- Effects: **YES** (explicit capability inputs; no `do`)
- Concurrency (`actor/on/spawn`): **YES**
- FFI (C interop): **NO**

### Non-goals for v1
- Garbage collection
- async / await
- implicit polymorphism
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
- Semicolons are **REQUIRED** to terminate non-block statements

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

Predeclared type constructors:
- `option<T>`
- `result<T, E>`
- `mailbox<T>`
- `reply<T>`

No unsigned integers.
No null; use `nil` for optionals.
No implicit conversions.

### Optionals
- `T?` is syntax sugar for `option<T>`
- Layout: `{ has: bool, value: T }`
- `nil` is the only literal optional value

### `var` inference
- If a `var` declaration omits its type, it is inferred from the initializer
- If the initializer is `nil`, an explicit optional type is required

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
- `for` is the only loop construct and takes a single condition
- `switch` uses `case` clauses and optional `else`
- Evaluation order is strictly left-to-right
- Function arguments evaluate left-to-right
- `&&` and `||` short-circuit

---

## 5.5) Interfaces

- Interfaces declare required METHOD signatures only
- No fields, no default implementations
- Structural satisfaction only
- Free functions never satisfy interfaces

---

## 6) Mutability Model

- `var` is the ONLY binding keyword
- Function parameters are immutable
- `var` bindings are mutable and single-writer within scope
- Rebinding allowed ONLY within `func` scope
- Global mutable state is forbidden
- Assignments across actor boundaries are forbidden

---

## 6.5) Functions vs Methods

- A function is a method iff it is declared as `Type.Method`.
- Methods have no receivers or `this`.
- Methods are satisfied structurally by interfaces; free functions never satisfy interfaces.
- Dot-call syntax never performs method dispatch; `value.f(...)` is only valid if `f` is a function-typed field.

Return types:
- A function or handler with no return type defaults to `void`.
- `return` with a value is invalid in `void` functions.

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

## 8) Effects and explicit capabilities

- IO, time, randomness, allocation are effects
- Effects require explicit capability parameters
- No ambient or implicit effects
- Stdlib effectful APIs REQUIRE an explicit capability in scope

---

## 9) Actors and Concurrency

- Actors are single-threaded state owners
- Actor handlers run to completion, one at a time
- Message ordering per actor is deterministic (FIFO)
- `spawn` is the only way to introduce concurrency
- Mailboxes are lock-free MPSC and bounded
- Request/response uses explicit reply channels (`reply<T>`)
- No shared mutable state across actor boundaries

Actor fields are declared directly in the actor body and are mutable only within `on` handlers.

---

## 10) Namespaces and Use

- Every source file MUST declare exactly one `namespace <name>;`.
- Namespace names are fully-qualified dot-separated identifiers (logical namespace), not filesystem paths.
- Multiple files may declare the same namespace name; all such files form a single namespace and share one namespace.
- `use` syntax:
  - `use <namespace>;`
  - `use <namespace> = <alias>;`
- `use` refers ONLY to fully-qualified namespace names; path-based/relative `use` is forbidden.
- If no alias is specified, the alias defaults to the final segment of the namespace name; explicit aliasing is required on collisions.
- All references to imported symbols MUST be qualified with the namespace alias; unqualified access is forbidden.
- No visibility modifiers in v1.
- No implicit re-exports in v1.
- Cyclic dependencies are forbidden.

---

## 11) Runtime ABI (v1)

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
- Heap allocation must be explicit via stdlib APIs
- No destructors
- Leak-on-exit acceptable

---

## 12) Compiler Pipeline

Required pipeline:

1. Lex
2. Parse
3. Typecheck
4. Lower to IR
5. Emit LLVM IR
6. `llc` + platform linker (Linux/macOS/Windows)

---

## 13) Bootstrap Verification

- `purr0c` (handwritten LLVM IR) compiles `purrc.pu`
- `purrc` recompiles itself
- LLVM IR must be structurally equivalent
- Bit-for-bit binaries are NOT required in v1

---

# END OF DECISIONS