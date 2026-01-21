# purr0 SPEC

purr0 is a deliberately tiny, **frozen** subset of the Purr language whose single purpose is to act as the bootstrap language for the real purr compiler.

Design goals
- Minimal and auditable: small surface area so a hand-written compiler can be inspected and trusted.
- Easy to lower to LLVM IR: features chosen map directly to simple LLVM constructs.
- Frozen: once the initial compiler is produced and used to build the real compiler, purr0 must not change.

Non-negotiable rules
- No generics, traits, macros, reflection, async/await, or hidden allocation.
- Prefer immutability, single-assignment, and SSA-friendly constructs.
- If a feature is hard to express in LLVM IR, it does not belong in purr0.

Types
- i32
- i64
- bool
- string (opaque pointer + length, no Unicode semantics)
- void
- struct (fixed layout, no generics)

Variables
- `let` bindings are immutable (single-assignment).
- Shadowing is permitted (useful for loop updates).
- No mutation after binding.

Functions
- Top-level functions only.
- Parameters and return types are explicit.
- No closures. Recursion is allowed but not required.

Lambdas (anonymous functions)
- Lambdas are expression-only, explicit (param and return types required), and must be trivially desugared to a top-level `func` by the compiler.
- Syntax: `(x: i32): i32 => x + 1`.
- Lowering model: every lambda is rewritten into a fresh top-level `func __lambda_N(...) T { return <expr>; }` with no captures. Lambdas do not introduce runtime closures in v1.
- Restrictions: no implicit captures, no multi-statement bodies, no type inference for parameters or return types, no async lambdas, and no implicit effects (use `do` for effectful blocks).
- Lambdas may be assigned to local bindings, passed as arguments, or returned as values; they are purely syntactic convenience and must lower precisely to named `func` definitions.

Control flow
- `if` / `else` (statement form required; expression form optional)
- `while` loops
- `return`
- No exceptions, no `defer`.

Expressions
- Integer arithmetic: `+ - * /`
- Comparisons: `== != < <= > >=`
- Boolean ops: `&& || !`
- Function calls
- Struct literals and field access

Structs
- Fixed-layout value types
- No methods, no inheritance
- Example:

  struct Token {
    kind: i32;
    pos: i32;
  }

Memory model
- Conceptually stack-only for language semantics; language does not define heap allocation APIs.
- Strings are runtime-provided pointers + lengths; purr0 defines no GC assumptions.

Modules
- Single-file compilation unit
- No imports and no visibility modifiers

Errors
- No exceptions; errors are modeled explicitly via return values or error structs.

Option & result shorthand
- `option<T>` is the explicit optional type used to represent a value that may be absent.
- For convenience, `T?` is provided as syntactic sugar for `option<T>` (e.g., `User?` == `option<User>`). This is purely syntactic and lowers to a discriminated struct: `{ has: bool, value: T }`.

Stability policy
- purr0 is frozen after the bootstrap compiler is produced; further language evolution occurs in the full purr language, not purr0.

See `grammar.md` for formal grammar and `RATIONALE.md` for a short explanation of each choice.
