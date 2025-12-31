# Purr keyword set

This file records the **frozen** keyword set for purr and the short rationale.
Treat this as normative: keywords are small, single-meaning, and unambiguous.

## Keyword list (flat)
module, import, struct, type, func, let, return, if, else, while, result, error, do, actor (reserved), stream (reserved), spawn (reserved), await (reserved), true, false

## Rationale (short)
- module — compilation unit / namespace; grouping only.
- import — explicit module import; no implicit resolution.
- struct — fixed-layout product type.
- type — declare a named alias or type definition.
- func — top-level function declaration (explicit params/returns).
- let — immutable binding (single-assignment); shadowing allowed.
- return — function return, explicit and local.
- if / else — conditional control flow (no other meanings).
- while — loop with explicit condition; maps to branch/phi.
- result — type constructor for `result<T,E>` (explicit errors).
- error — produce an error value for `result` (no exceptions).
- do — explicit effectful block / effect boundary.
- actor / stream / spawn / await — **reserved** concurrency keywords; semantics provided by runtime when enabled.
- true / false — boolean literals; no truthy/falsy coercions.

## Why excluded keywords are forbidden (summary)
- Synonyms like `fn` are disallowed; use `func`.
- Mutable/aliasing keywords (`var`, `const`, `mutable`) create multiple binding models; we keep only immutable `let`.
- `null` and top types (`any`, `object`) break structural correctness; use `T?` and explicit `result` instead.
- Control forms like `for`, `switch`, `match` increase surface area; stick to `while` and library helpers for clarity.
- Exception-style keywords (`try`, `catch`, `throw`) hide control flow; `result` is explicit.
- OOP keywords (`class`, `interface`, `extends`, `implements`) introduce hidden identity and polymorphism; prefer plain `struct` + functions.

## Notes
- This set is intentionally small and frozen early. Additions require strong justification and a coordinated deprecation path.
