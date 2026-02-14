# PLAN.md — Purr Implementation Roadmap

Bootstrap a compiler. Prove the language works. Then build a message broker.

Every milestone compiles and runs a test program. The project is never in a broken state between sessions.

---

## Metrics

Track for every gauntlet program and every generated Purr program:
- First compile success (yes/no)
- Runtime correctness (yes/no)
- Number of tokens generated
- Number of regeneration attempts
- Failure category if any: syntax, type error, semantic misunderstanding, missing feature, stdlib violation

Convergence matters. If the language is working, regeneration attempts drop rapidly after small tweaks.

---

## Phase 1 — Bootstrap

Goal: a working compiler pipeline that produces executables from trivial programs.

### M1: Hello World

Spec coverage: Sections 2.1–2.5, 3.1–3.2, 3.4, 9.6, 10.7

Implement:
- Lexer with span tracking (line/col)
- Recursive descent parser
- AST: `actor`, `on start`, `print_string(string)`
- Semantic analysis: exactly one `Main` actor, one `start` handler
- IR: flatten handlers into functions, `CallPrint` + `Return` instructions
- C99 codegen: generate `.c`, invoke `cc`
- Driver: `purrc file.purr` → executable
- Runtime: `purr_runtime_init()`, `purr_print()`
- Built-in: `print_string`, `print_i32` from `purr.std.debug`

Compiler written in OCaml. Pipeline: source → tokens → AST → sema → IR → C → executable.

Test program:
```
actor Main {
    on start() {
        print_string("hello world")
    }
}
```

### M2: Variables and Integers

Spec coverage: Sections 2.6, 3.3, 3.4, 4.1, 4.3.1–4.3.3

Implement:
- `var` declarations with explicit types: `var x: i32 = 42`
- `var` declarations with type inference: `var x = 42`
- Assignment: `x = 10`
- Types: `i32`, `i64`, `string`, `bool`
- Integer and boolean literals, `true`/`false`
- Scope tracking in sema (symbol table)
- Codegen: C local variables, `int32_t`/`int64_t`

### M3: Expressions and Operators

Spec coverage: Sections 3.5, 3.6, 4.3.5–4.3.7, 5.1–5.3

Implement:
- Arithmetic: `+ - * /`
- Comparison: `== != < <= > >=`
- Boolean: `&& || !`
- Parenthesized expressions
- **Mandatory parenthesization**: mixing operator categories without parens is a compile error
- Type checking: operands must match exactly
- Integer overflow traps at runtime
- Short-circuit evaluation for `&&` and `||`
- Left-to-right evaluation order

### M4: Functions

Spec coverage: Sections 3.2, 4.2, 4.3.2

Implement:
- `fn` declarations outside actors
- Parameters with explicit types
- Return types (explicit, defaults to `void`)
- `return` statements
- Function calls as expressions
- Symbol table: function signatures, arity checking
- Codegen: C functions, parameter passing

### M5: Control Flow and Test Runner

Spec coverage: Sections 3.4, 4.12, 5.4, 10.9

Implement:
- `if expr { } else { }`
- `for expr { }` (while-style loop)
- IR: `Branch`, `Jump`, `Label` instructions
- `test "name" { body }` declarations
- `purrc --test` mode: collect tests, run, report pass/fail
- `expect_eq_i32`, `expect_true`, `expect_false` from `purr.test`
- Codegen: C `if`/`else`, `while`

Test program:
```
fn factorial(n: i32) i32 {
    var result: i32 = 1
    var i: i32 = 1
    for i <= n {
        result = result * i
        i = i + 1
    }
    return result
}

test "factorial of 5" {
    expect_eq_i32(factorial(5), 120)
}

test "factorial of 0" {
    expect_eq_i32(factorial(0), 1)
}
```

---

## GATE 1A — Algorithmic Correctness (no ADTs)

**Freeze. Generate programs using only M1–M5 features: scalars, functions, control flow, tests.**

Goal: isolate whether the LLM can produce correct algorithmic code in Purr's syntax. All programs include `test` declarations — correctness is automated via `purrc --test`.

### M6: The Scalar Gauntlet

Generate 5 programs. First-attempt correctness. Track metrics.

Programs (each includes `test` declarations):
1. **Ring buffer** — fixed-size circular buffer of `i32` using parallel arrays, insert/read with modular wrap
2. **Rate limiter** — token bucket using `i32` counters, accepts or rejects based on available tokens
3. **FizzBuzz state machine** — classify integers using `if`/`else` chains, accumulate output
4. **GCD + LCM** — Euclidean algorithm, then derive LCM, test with multiple pairs
5. **Binary search** — search a sorted `i32` array (simulated as function returning element at index)

Scoring:
- **4/5+ correct** → syntax and control flow are LLM-friendly, proceed to ADTs
- **2–3/5** → diagnose: is it syntax? operator rules? scoping? Fix language, re-run
- **Below 2/5** → fundamental syntax problems, redesign before continuing

---

## Phase 2 — Type System

Goal: structs, enums, containers, and enough type machinery to model real data.

### M7: Structs

Spec coverage: Sections 3.2, 3.5, 4.6, 9.3

Implement:
- `struct` declarations with typed fields
- Struct literals: `Point { x: 10, y: 20 }`
- Field access: `p.x`
- Stack-allocated, passed by copy
- Struct equality (fieldwise, declared order)
- Codegen: C structs, natural alignment, declared field order
- **Stdlib exclusivity rule** (Section 4.14): reject reserved container names

### M8: Enums and Switch

Spec coverage: Sections 3.2, 3.4, 4.7, 5.5

Implement:
- `enum` with `case` variants, optional associated data
- `switch` expression matching
- No fallthrough, `else` clause for default
- Codegen: tagged unions in C

### M9: Optionals, Results, and Containers

Spec coverage: Sections 3.3, 4.3.0, 4.4, 4.5, 10.8

Implement:
- `option<T>`, `T?` sugar, `nil` literal
- `result<T, E>`
- `list<T>` with `list_new`, `list_append`, `list_get`, `list_length`, `list_each`
- `map<K, V>` with `map_new`, `map_get`, `map_set`, `map_has`, `map_keys`, `map_each`
- `fixed<T, N>` and `slice<T>` with their operations
- Runtime: C implementations for list (dynamic array) and map (hash table)
- `purr.std.bytes` and `purr.std.string` operations
- `purr.std.math` operations

This is the largest language milestone. Container runtime implementation in C is the bulk of the work.

---

## GATE 1B — ADT Correctness

**Freeze. Generate programs using M1–M9 features: full type system, containers, no concurrency.**

### M10: The ADT Gauntlet

Generate 5 programs. First-attempt correctness. Track metrics.

Programs:
1. **Bounded queue** — FIFO queue using `list<i32>` and a struct, `result` for full/empty
2. **Retry state machine** — enum-driven transitions: `Idle → Trying → Backoff → Failed`, switch dispatch
3. **Message frame encoder** — pack a `Header` struct into bytes using `purr.std.bytes`, unpack, verify round-trip
4. **Prefix router** — match a key against a `map` of prefixes, return `option<Route>` for longest match
5. **Lease manager** — struct-based lease table using `map`, check expiry, return `result` for expired/valid/unknown

Scoring:
- **4/5+ correct** → type system is LLM-friendly, proceed
- **2–3/5** → diagnose: is it enums? optionals? containers? Fix, re-run
- **Below 2/5** → type system needs redesign

Compare results with Gate 1A. If 1A passed but 1B fails, the problem is ADTs specifically.

---

## Phase 2.5 — Bench Runner

Goal: measure compiler/runtime behavior so language tweaks can be evaluated quantitatively.

### M10.5: Bench Infrastructure

Spec coverage: Sections 4.13, 10.10

Implement:
- `bench "name" iterations N { setup { } run { } }` declarations
- `purrc --bench` mode: collect benchmarks, run, report
- Runtime instrumentation counters: `alloc_count`, `message_count`, `scheduler_steps`, `bytes_allocated`
- Machine-readable output format
- Benchmarks run on deterministic scheduler

Test:
```
bench "list append 1000" iterations 1000 {
    setup {
        var l = list_new()
    }
    run {
        l = list_append(l, 42)
    }
}
```

From this point forward, every stdlib change and compiler change includes a bench run to detect regressions.

---

## Phase 3 — FFI and IO

Goal: call C functions, talk to the OS. Purr orchestrates — it does not reimplement libc.

### M11: FFI

Spec coverage: Sections 3.2, 4.10, 9.5

Implement:
- `extern fn` declarations (no body)
- C type mapping per Section 9.5
- Linker integration: pass extra `.c` or `.o` files to `cc`
- Exempt from effect checking
- No callbacks from C into Purr

### M12: Thin OS Wrappers

Implement:
- `extern fn` wrappers for POSIX: `open`, `read`, `write`, `close`, `socket`, `bind`, `listen`, `accept`
- A single `purr.os` namespace with only `extern fn` declarations and thin helpers
- No abstraction layers. The OS is a foreign runtime permanently.

### M13: Namespaces

Spec coverage: Sections 8.1–8.7

Implement:
- `namespace` declaration (first non-comment token)
- `use` with qualified access
- Alias defaulting and explicit aliases
- Multi-file namespaces
- All required compiler errors from Section 8.7
- Driver: accept multiple source files, resolve namespaces

---

## Phase 4 — Actors and Concurrency

Goal: real message passing, scheduling, and the actor runtime.

### M14: Messages

Spec coverage: Sections 3.2, 4.11

Implement:
- `message` declarations as first-class types
- Messages are value types, copy-on-send
- Messages distinct from structs (compiler enforces)
- Codegen: C structs with message type tags

### M15: Actor State and Handlers

Spec coverage: Sections 7.1–7.2

Implement:
- `state` fields in actors
- Multiple `on` handlers per actor
- `self.field` access within handlers
- Handler mutation of state fields
- Private `fn` inside actors
- Sema: enforce no cross-actor state access
- Codegen: actor struct in C with state fields

### M16: Spawn, Send, and Deterministic Scheduler

Spec coverage: Sections 3.4, 3.5, 7.3–7.4, 7.6

Implement:
- `spawn ActorName { fields }` → returns `mailbox<ActorName>`
- `send target MessageName { fields }`
- Runtime: mailbox queues (bounded MPSC)
- **Deterministic single-step scheduler**: process one message at a time, round-robin, fully deterministic
- No OS integration, no threads, no IO — pure message processing
- Actor test support: `spawn_test`, `send_test`, `drain`, `expect_output`

### M17: Request/Response

Spec coverage: Section 7.5

Implement:
- `reply<T>` type: single-use mailbox, capacity 1
- Request messages carry `reply<T>` field
- Reply channel as a special mailbox in C

### M18: Lambdas

Spec coverage: Section 3.5

Implement:
- `(params): Type => expr` syntax
- No captures (lambda is a function pointer)
- Function type: `(i32): bool`
- Codegen: C function pointers

---

## GATE 2 — Concurrency Validation

**Freeze. Generate actor programs on the deterministic scheduler. No IO, no OS.**

### M19: The Actor Gauntlet

Generate 5 actor programs. First-attempt correctness. Track metrics. All programs use `test` declarations with `spawn_test`/`send_test`/`drain`.

Programs:
1. **Ping-pong** — two actors exchange N messages, count round trips
2. **Fan-out/fan-in** — producer spawns N workers, each replies, producer aggregates
3. **Pipeline** — chain of 3 actors, each transforms a message and forwards
4. **Supervisor** — actor monitors a child, restarts on failure (simulated via message)
5. **Load balancer** — round-robin dispatch across a pool of worker actors

Scoring: same as prior gates. If the LLM can't write correct actor programs on a deterministic scheduler, adding OS-level async won't help.

Run bench suite after gate to establish baseline actor performance invariants.

---

## Phase 5 — Networking

Goal: TCP servers using actors, backed by an OS event loop.

### M20: TCP Server (blocking)

Implement:
- `extern fn` wrappers from M12 for socket/bind/listen/accept/read/write/close
- Listener actor: accepts connections, spawns handler actors
- Blocking IO, single-threaded

Test: TCP echo server — connect with netcat, send text, receive it back.

### M21: Event Loop Scheduler

Spec coverage: Section 7.6

Implement:
- Integrate `epoll` (Linux) / `kqueue` (macOS) into runtime
- Second scheduler mode alongside deterministic
- Actors yield on IO, scheduler polls for readiness
- Non-blocking sockets

Test: echo server handles multiple concurrent connections.

Bench: compare message throughput between deterministic and event-loop schedulers.

---

## Phase 6 — In-Memory Broker

Goal: a working message broker with no persistence. Proves the actor model end-to-end.

### M22: Wire Protocol

Implement:
- Binary protocol over TCP: `PUBLISH topic payload`, `SUBSCRIBE topic`, `ACK id`
- Encode/decode using `purr.std.bytes`
- `purr.broker.protocol` namespace

Test: parse hand-crafted binary messages, verify round-trip.

### M23: Topic Routing

Implement:
- Broker actor receives Publish, fans out to subscriber actors via `map`
- Subscriber actors forward to TCP connections

Test: two clients subscribe to "orders", third publishes, both receive.

Bench: fan-out to N subscribers, measure `message_count` and `scheduler_steps` scaling.

### M24: Consumer Groups and ACK

Implement:
- Consumer groups: multiple consumers share a subscription
- Broker tracks per-consumer delivery state via `map`
- ACK/NACK protocol, redelivery on timeout

Test: two consumers in a group, publish 100 messages, each delivered exactly once across the group.

**This is a working message broker.**

---

## Phase 7 — Persistence (Optional)

Goal: make the broker durable. Independent of the core broker.

### M25: Append-Only Log

Implement:
- Append messages to a log file via FFI file IO
- Replay log on broker startup
- Subscribers track offsets

Test: publish messages, restart broker, verify messages survive.

### M26: Log Compaction

Implement:
- Background compaction actor truncates acknowledged messages
- Offset tracking survives compaction

### M27: LSM Storage (stretch goal)

Implement:
- Sorted memtable, SSTable flush, compaction
- Used for broker metadata
- Compaction is an actor

---

## Stdlib Evolution Rule

Programs compose stdlib primitives — they do not reinvent them (Section 4.14).

When the LLM repeatedly writes the same helper across gauntlet programs, promote it into the standard library. The stdlib grows only through observed need at gates, never through upfront design.

You are curating a training vocabulary. Userland code is disposable. Stdlib is the language.

---

## Milestone Dependency Graph

```
M1 ── M2 ── M3 ── M4 ── M5
                          │
                   ┌─ GATE 1A (M6) ─┐
                   │                 │
            M7 ── M8 ── M9
                          │
                   ┌─ GATE 1B (M10) ─┐
                   │                  │
                  M10.5 (bench)
                          │
            M11 ── M12 ── M13
                          │
      M14 ── M15 ── M16 (deterministic) ── M17 ── M18
                                                   │
                                          ┌─ GATE 2 (M19) ─┐
                                          │                  │
                                   M20 ── M21 (event loop)
                                          │
                            M22 ── M23 ── M24 ← working broker
                                          │
                            M25 ── M26 ── M27 (optional)
```

## Estimated Complexity

| Milestone    | Sessions | Risk       | Notes                                           |
|--------------|----------|------------|-------------------------------------------------|
| M1           | 1        | Low        | Architecture established here                    |
| M2           | 1        | Low        | Symbol table is the key addition                 |
| M3           | 1        | Low        | Mandatory parens simplify precedence             |
| M4           | 1        | Low        | Straightforward with existing pipeline           |
| M5           | 1–2      | Low        | Control flow + test runner                       |
| **GATE 1A**  | **1–2**  | **—**      | **5 scalar programs. Measure. Fix.**             |
| M7           | 1–2      | Low        | C struct layout + exclusivity rule enforcement   |
| M8           | 2        | Medium     | Tagged unions, pattern matching                  |
| M9           | 2–3      | Medium     | Containers require C runtime implementation      |
| **GATE 1B**  | **1–2**  | **—**      | **5 ADT programs. Compare with 1A. Fix.**        |
| M10.5        | 1        | Low        | Bench runner + runtime counters                  |
| M11          | 1        | Low        | Declare-only, linker does the work               |
| M12          | 1        | Low        | Thin extern wrappers                             |
| M13          | 2        | Medium     | Multi-file compilation, namespace resolver       |
| M14          | 1        | Low        | Message types are specialized structs            |
| M15          | 1–2      | Medium     | Actor struct codegen, self access                |
| M16          | 2–3      | High       | Deterministic scheduler, mailbox queues          |
| M17          | 1–2      | Medium     | Reply channels on top of M16 mailboxes           |
| M18          | 1        | Low        | Function pointers in C                           |
| **GATE 2**   | **2–3**  | **—**      | **5 actor programs + bench baseline**            |
| M20          | 1–2      | Medium     | TCP wrappers, blocking first                     |
| M21          | 2–3      | High       | epoll/kqueue, second scheduler mode              |
| M22          | 1        | Low        | Binary parsing with purr.std.bytes               |
| M23          | 1–2      | Medium     | Fan-out routing via map + bench scaling          |
| M24          | 2        | High       | Delivery guarantees, timeout handling            |
| M25          | 1–2      | Medium     | Append log, replay on startup                    |
| M26          | 1–2      | Medium     | Compaction actor                                 |
| M27          | 3–4      | High       | Full LSM, stretch goal                           |

Total to working in-memory broker (M1–M24): ~33–42 sessions.
Total including persistence (M25–M27): ~38–50 sessions.

High-risk milestones: M16 (deterministic scheduler), M21 (event loop), M24 (delivery guarantees).

Gates are where the project lives or dies. If the LLM can't pass them, no amount of additional milestones will save the broker. Fix the language, not the programs.
