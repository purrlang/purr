# purr0 Rationale

This short document explains why the purr0 features exist and why others are
excluded. Keep it concise so future reviewers understand the trade-offs.

Types: i32/i64/bool/string/void/struct
- Chosen because they map directly to LLVM IR primitive types or simple
  compound types. Avoids complex runtime representations.

Immutable single-assignment and shadowing
- Simplifies lowering to SSA form and eliminates complex mutation semantics.
- Shadowing provides a simple way to express loop updates without assignments.

Functions: top-level, explicit signatures
- No closures or generics keeps call and stack frames simple.

Control flow: if/while/return, no exceptions
- Directly map to LLVM branches and loops. No unwinding/exception ABI to
  support at this stage.

Structs: fixed layout value types
- Fixed layout is easy to lower to stack allocation and field accesses.

Strings as pointer+length
- Keep runtime contract simple; language does not define allocation or
  Unicode semantics.

Errors as return values
- Explicit and easy to model in IR; no exception semantics to implement.

Out-of-scope features
- Generics, traits, macros, reflection, async/await, actor/stream abstractions,
  operator overloading, and other high-level features are explicitly excluded
  because they significantly complicate bootstrapping and lowering to LLVM IR.
