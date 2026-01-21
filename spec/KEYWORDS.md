# Purr keyword set

This file records the **frozen** keyword set for Purr and the short rationale.
Treat this as normative: keywords are small, single-meaning, and unambiguous.

## Keyword list (flat)
namespace, use,
struct, enum, interface,
actor, on, spawn,
func, var, return,
if, else, for, switch, case,
nil, true, false

## Rationale (short)
- namespace — compilation unit / namespace; grouping only.
- use — explicit namespace import; no implicit resolution.
- struct — fixed-layout product type.
- enum — closed tagged union; explicit cases only.
- interface — structural method requirements only.
- actor — single-threaded state owner.
- on — actor message handler definition.
- spawn — explicit concurrency introduction; creates an actor.
- func — top-level function or method declaration.
- var — mutable binding (single-writer, no shared mutation across actors).
- return — function return, explicit and local.
- if / else — conditional control flow (no other meanings).
- for — single loop construct with explicit init/condition/step.
- switch / case — explicit multi-branch control flow.
- nil — absence value for optionals.
- true / false — boolean literals; no truthy/falsy coercions.

## Why excluded keywords are forbidden (summary)
- Synonyms like `fn` are disallowed; use `func`.
- Extra binding keywords (`let`, `const`, `mutable`) create multiple models; keep only `var`.
- Exception-style keywords (`try`, `catch`, `throw`) hide control flow; errors are explicit values.
- OOP keywords (`class`, `extends`, `implements`) introduce hidden identity and polymorphism; use `struct` + functions.
- Async keywords (`async`, `await`) hide scheduling; concurrency is explicit via actors.

## Notes
- This set is intentionally small and frozen early. Additions require strong justification and a coordinated deprecation path.
