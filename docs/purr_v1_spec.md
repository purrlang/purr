# Purr v1 Language Specification (Normative)

This document is the **normative** language specification for **Purr v1**.

- This spec is **requirements-only**.
- If a behavior is not specified here, it is **undefined and unsupported**.
- This spec contains **no alternatives**, **no philosophy**, and **no future notes**.

---

## 1. Scope

Targets:
- Native compilation via C99 codegen: **YES**
- LLVM backend: **NO** (may be added as a second backend in a future version)
- WASM: **NO**
- Supported OS: Linux, macOS

Language features in v1:
- Namespaces / `use`: **YES**
- `result<T, E>`: **YES** (predeclared type constructor)
- `T?` optionals: **YES** (syntax sugar over `option<T>`)
- Lambdas: **YES** (expression-only, no captures)
- Generics: **NO** (except predeclared type constructors)
- Effects: **YES** (explicit capability inputs; no `do`)
- Concurrency (`actor/on/spawn`): **YES**
- Interfaces (structural, method-only): **YES**
- FFI (`extern`): **YES** (declare-only C function signatures, no callbacks)
- Built-in test runner (`purrc --test`): **YES**
- Built-in bench runner (`purrc --bench`): **YES**

Non-goals in v1:
- Garbage collection
- async/await
- implicit polymorphism
- reflection
- macros
- user-exposed custom allocators
- Windows support

## 2. Lexical specification

### 2.1 Comments

- Line comments begin with `//` and continue to the end of the line.
- There are no block comments.

### 2.2 Whitespace and statement termination

- Statements are **newline-terminated**.
- Newlines are significant as statement terminators.
- A newline is suppressed (continuation) when the line ends with a binary operator, comma, or opening brace.
- Blank lines are ignored.
- Indentation is insignificant.
- There are no semicolons in the language.

### 2.3 Identifiers

- Identifiers use ASCII characters only.
- Allowed characters: `[A-Za-z_]` followed by zero or more `[A-Za-z0-9_]`.
- Identifiers must not start with a digit.
- Unicode identifiers are not supported in v1.

### 2.4 Keywords

Reserved keywords:
`actor`, `bench`, `case`, `else`, `enum`, `extern`, `false`, `fn`, `for`, `if`, `interface`, `iterations`, `message`, `namespace`, `nil`, `on`, `return`, `run`, `send`, `setup`, `spawn`, `start`, `state`, `struct`, `switch`, `test`, `true`, `use`, `var`

Keywords are not valid identifiers.

### 2.5 String literals

- String literals are delimited by double quotes: `"..."`.
- Strings represent UTF-8 byte sequences.
- Escape sequences supported:
	- `\n` (newline)
	- `\t` (tab)
	- `\\` (backslash)
	- `\"` (double quote)
- UTF-8 validity is **not enforced at runtime**.

### 2.6 Numeric literals

- Base-10 only.
- Signed literals only.
- No numeric separators.
- Literal overflow is a **compile-time error**.

Integer literal form:
- `-?[0-9]+`

Floating literal form (for `f64`):
- `-?[0-9]+\.[0-9]+`

## 3. Grammar (EBNF)

Terminals:
- `identifier`
- `int-literal`
- `float-literal`
- `string-literal`
- `bool-literal` (`true` | `false`)
- `nil-literal` (`nil`)
- punctuation and operator tokens

Notes:
- Newlines terminate non-block statements.
- `if`/`else` are statements only.

### 3.1 Program structure

```
Program := NamespaceDecl NL { UseDecl NL } { Declaration }

NamespaceDecl := "namespace" NamespaceName

NamespaceName := Identifier { "." Identifier }

UseDecl := "use" NamespaceName [ "=" Identifier ]

Declaration := StructDecl | EnumDecl | InterfaceDecl | ActorDecl
             | FunctionDecl | ExternDecl | MessageDecl | TestDecl | BenchDecl
```

### 3.2 Declarations

```
StructDecl := "struct" Identifier "{" FieldList "}"
FieldList := { Type Identifier NL }

EnumDecl := "enum" Identifier "{" EnumCaseList "}"
EnumCaseList := { EnumCase }
EnumCase := "case" Identifier [ "(" [ ParamList ] ")" ] NL

InterfaceDecl := "interface" Identifier "{" { MethodSig } "}"
MethodSig := "fn" Identifier "(" [ ParamList ] ")" Type NL

ActorDecl := "actor" Identifier "{" { ActorMember } "}"
ActorMember := StateField | OnHandler | FunctionDecl
StateField := "state" Type Identifier NL
OnHandler := "on" Identifier "(" [ ParamList ] ")" [ Type ] Block

FunctionDecl := "fn" FunctionName "(" [ ParamList ] ")" [ Type ] Block
FunctionName := Identifier | Identifier "." Identifier

ExternDecl := "extern" "fn" Identifier "(" [ ParamList ] ")" [ Type ] NL

MessageDecl := "message" Identifier "{" FieldList "}"

TestDecl := "test" string-literal Block

BenchDecl := "bench" string-literal "iterations" int-literal "{" [ SetupClause ] RunClause "}"
SetupClause := "setup" Block
RunClause := "run" Block

ParamList := Param { "," Param }
Param := Identifier ":" Type

Block := "{" { Statement } "}"
```

### 3.3 Types

```
Type :=
    TypeName
    | TypeCtor
    | FunctionType
    | Type "?"

TypeName := Identifier

TypeCtor := Identifier "<" Type { "," Type } ">"

FunctionType := "(" [ TypeList ] ")" ":" Type
TypeList := Type { "," Type }
```

Notes:
- `T?` is syntax sugar for `option<T>`.
- There are no user-defined generics; only predeclared type constructors are permitted.

### 3.4 Statements

```
Statement :=
    VarDecl
    | AssignStmt
    | IfStmt
    | ForStmt
    | SwitchStmt
    | ReturnStmt
    | SendStmt
    | ExprStmt
    | Block

VarDecl := "var" Identifier [ ":" Type ] "=" Expression NL

AssignStmt := LValue "=" Expression NL

LValue := Identifier { "." Identifier }

IfStmt := "if" Expression Block [ "else" Block ]

ForStmt := "for" Expression Block

SwitchStmt := "switch" Expression "{" { CaseClause } [ ElseClause ] "}"
CaseClause := "case" CasePattern Block
ElseClause := "else" Block
CasePattern := Literal | Identifier

ReturnStmt := "return" [ Expression ] NL

SendStmt := "send" Expression Expression NL

ExprStmt := Expression NL
```

### 3.5 Expressions

```
Expression := LogicalOrExpr

LogicalOrExpr := LogicalAndExpr { "||" LogicalAndExpr }
LogicalAndExpr := EqualityExpr { "&&" EqualityExpr }
EqualityExpr := RelationalExpr { ("==" | "!=") RelationalExpr }
RelationalExpr := AddExpr { ("<" | "<=" | ">" | ">=") AddExpr }
AddExpr := MulExpr { ("+" | "-") MulExpr }
MulExpr := UnaryExpr { ("*" | "/") UnaryExpr }
UnaryExpr := [ "!" | "-" ] Postfix

Postfix := Primary { CallSuffix | FieldSuffix }
CallSuffix := "(" [ ArgList ] ")"
ArgList := Expression { "," Expression }
FieldSuffix := "." Identifier

Lambda := "(" [ ParamList ] ")" ":" Type "=>" Expression

SpawnExpr := "spawn" Identifier "{" [ InitList ] "}"

Primary :=
    int-literal
    | float-literal
    | bool-literal
    | string-literal
    | "nil"
    | Identifier
    | StructLiteral
    | Lambda
    | SpawnExpr
    | "(" Expression ")"

StructLiteral := Identifier "{" [ InitList ] "}"
InitList := Identifier ":" Expression { "," Identifier ":" Expression }

Literal := int-literal | float-literal | bool-literal | string-literal | "nil"
```

### 3.6 Mandatory parenthesization

Mixing operators from different precedence categories in the same expression without parentheses is a **compile error**.

Operators within the same category may be chained freely.

Categories:
- Multiplicative: `* /`
- Additive: `+ -`
- Relational: `< <= > >=`
- Equality: `== !=`
- Logical AND: `&&`
- Logical OR: `||`

Valid:
```
var a: i32 = x * y * z
var b: i32 = (x * y) + z
var c: bool = (a + b) > (c + d)
```

Invalid:
```
var a: i32 = x * y + z           // ERROR: mixed categories
var b: bool = x + y > z          // ERROR: mixed categories
```

## 4. Static semantics

### 4.1 Mutability and binding

- `var` is the only binding form.
- Function parameters are immutable.
- `var` bindings are mutable and single-writer within their scope.
- Rebinding (shadowing) is permitted **only within function scope**.
- Global mutable state is forbidden.
- Assignment targets must be local `var` bindings or fields reachable from a local `var`.
- Assignments across actor boundaries are forbidden.

### 4.2 Functions vs methods

- A function is a method iff it is declared as `Type.Method`.
- Methods do not have receivers, `this`, or implicit parameters.
- Methods are dispatched by explicit qualification only; there is no dot-dispatch from values.
- Free functions never satisfy interfaces.
- A call expression on a field access (e.g., `value.f(...)`) is valid only if `f` is a field of function type; it does not perform method dispatch.

### 4.3 Type checking

#### 4.3.0 Predeclared types

Predeclared type names: `bool`, `i32`, `i64`, `f64`, `string`, `bytes`, `void`.

Predeclared type constructors:
- `option<T>` — optional value
- `result<T, E>` — success or error
- `list<T>` — ordered, growable sequence
- `map<K, V>` — key-value mapping (`K` must support equality)
- `fixed<T, N>` — fixed-size array (N is a compile-time integer constant)
- `slice<T>` — borrowed view into a `fixed` or `list`
- `mailbox<T>` — actor mailbox
- `reply<T>` — single-use reply channel

Any other `TypeCtor` is a compile-time error.

#### 4.3.1 `var` inference

- If a `var` declaration omits its type, it is inferred from the initializer.
- If the initializer is `nil`, the `var` declaration MUST provide an explicit optional type.
- If the type cannot be inferred unambiguously, it is a compile-time error.

#### 4.3.2 Return types

- A function or handler with no return type defaults to `void`.
- Return statements in `void` functions must omit a value.

#### 4.3.3 Integer literals

- Integer literals are type-checked against context (e.g., `var x: i32 = 1`).
- An integer literal must fit in its target type (`i32` or `i64`), or it is a compile-time error.
- An integer literal without a required type context is a compile-time error.

#### 4.3.4 Floating literals

- A floating literal has type `f64`.
- If a floating literal cannot be represented as `f64`, it is a compile-time error.

#### 4.3.5 Arithmetic operators

For operands of type `i32`, `i64`, or `f64`:
- `+ - * /` are defined.
- Operand types must match exactly.

#### 4.3.6 Comparison operators

- `< <= > >=` are defined for `i32`, `i64`, and `f64` only.
- Operand types must match exactly.

#### 4.3.7 Boolean operators

- `!` requires `bool`.
- `&&` and `||` require `bool` operands and produce `bool`.

### 4.4 Optionals

- `T?` is syntax sugar for `option<T>`.
- `option<T>` is a value type with the runtime layout: `{ has: bool, value: T }`.
- `nil` is the only literal value of type `option<T>` and is valid for any `T`.

### 4.5 Results

- `result<T, E>` is a value type with runtime layout: `{ ok: bool, t: T, e: E }`.
- Errors are values; there are no exceptions.

### 4.6 Equality

`==` and `!=` are defined for:
- `bool`, `i32`, `i64`
- `f64`
- `string` (bytewise)
- `struct` types (fieldwise, in declared field order)

For `f64`, equality uses IEEE-754 semantics:
- `NaN == x` is false for all `x` (including `NaN`).
- `NaN != x` is true for all `x`.

Equality is undefined for all other types.

### 4.7 `switch` typing

- The `switch` scrutinee and each `case` pattern must have the same type.
- `case` patterns must be compile-time constants or enum case constructors.

### 4.8 Interfaces

- Interfaces declare required METHOD signatures only.
- Interfaces contain no fields and no implementations.
- Satisfaction is structural: a type satisfies an interface iff it defines all required methods.
- Free functions never satisfy interfaces.

### 4.9 Reserved declarations

- Any use of undeclared keywords is a compile-time error.

### 4.10 FFI

- `extern fn` declares a C function signature callable from Purr.
- The compiler maps Purr types to C types as defined in Section 9.
- `extern` functions have no body.
- `extern` functions are not subject to effect checking (they are assumed to be effectful).
- The programmer is responsible for linking the corresponding C implementation.
- Callbacks from C into Purr are not supported.

### 4.11 Messages

- `message` declarations define types that can be sent between actors.
- Messages are value types. Sending a message copies it.
- Messages may only contain value types as fields.
- Messages are distinct from structs; a struct cannot be sent as a message and a message cannot be used as a struct.

### 4.12 Test declarations

- `test "name" { body }` declares a test case.
- Test declarations are top-level and may appear in any source file.
- Tests are collected and executed by `purrc --test`.
- Test body may use any statement.
- Tests run in source order within a file and in unspecified order across files.

### 4.13 Bench declarations

- `bench "name" iterations N { setup { ... } run { ... } }` declares a benchmark.
- The `setup` clause runs once before iteration begins. `setup` is optional.
- The `run` clause runs N times.
- Benchmarks are collected and executed by `purrc --bench`.
- Benchmarks measure invariants, not wall-clock speed (see Section 10.9).

### 4.14 Stdlib exclusivity rule

Programs must not implement data structures that replicate the semantics of predeclared type constructors.

The compiler MUST reject user-defined types that duplicate `list`, `map`, `fixed`, `slice`, `option`, or `result` semantics. Specifically:

- A struct with fields and methods that form an ordered growable sequence is rejected if `list<T>` serves the same purpose.
- A struct with fields and methods that form a key-value mapping is rejected if `map<K, V>` serves the same purpose.

This rule is enforced by name: any user-defined type named `List`, `Map`, `Queue`, `Stack`, `Set`, `Dict`, `Array`, `Vector`, `HashMap`, `TreeMap`, `LinkedList`, `Deque`, `Buffer`, `Ring`, `Heap`, `Pool` (case-insensitive) is a compile-time error.

Programs compose stdlib primitives. They do not reinvent them.

## 5. Dynamic semantics

### 5.1 Evaluation order

Evaluation order is strictly left-to-right for:
- function arguments
- operator operands
- struct initializer expressions

### 5.2 Short-circuiting

- `&&` and `||` short-circuit.

### 5.3 Integer overflow

- Integer overflow **traps at runtime**.
- Literal overflow is a compile-time error.

### 5.4 `for` loops

- `for cond { body }` evaluates `cond` before each iteration.
- The loop executes `body` while `cond` is true.

### 5.5 `switch`

- `switch (value)` evaluates `value` once.
- `case` clauses are tested in source order.
- The first matching `case` executes; no fallthrough is permitted.
- `else` executes when no `case` matches.

## 6. Effects and explicit capabilities

### 6.1 Capability inputs

- IO, time, randomness, and allocation are effects.
- Effects are only available through explicit capability values passed as parameters.
- The standard library defines opaque capability types (e.g., `io.File`, `time.Clock`, `rand.Source`, `mem.Allocator`).
- A function that performs effects MUST take the required capability as an explicit parameter.
- `extern fn` declarations are exempt from effect checking.

### 6.2 Effect restrictions

- There are no implicit or ambient effects.
- The compiler MUST reject calls to effectful standard library APIs unless the required capability is in scope.

## 7. Actors and concurrency

### 7.1 Actor model

- Actors are single-threaded state owners.
- Actor handlers run to completion, one at a time.
- No shared mutable state is permitted across actor boundaries.
- Message passing uses lock-free bounded MPSC mailboxes.
- Message ordering per actor is deterministic and FIFO.
- `spawn` is the only way to introduce concurrency.

### 7.2 Actor declarations

- An `actor` declaration defines:
	- actor-owned state fields declared with the `state` keyword
	- one or more `on` handlers
	- zero or more private `fn` declarations (accessible only within the actor)
- Actor state fields are accessible only within `on` handlers and private functions of the same actor.
- Handlers may mutate actor state fields.
- Private `fn` declarations inside an actor are the actor's internal logic. They are not visible outside the actor. This is the only visibility mechanism in the language.

### 7.3 Spawn and mailboxes

- `spawn ActorName { field: value, ... }` creates a new actor instance.
- The initialization value must provide all actor state fields.
- Mailboxes are bounded; capacity is defined by the runtime for each actor type.
- `spawn` returns `mailbox<ActorName>`.

### 7.4 Message sending

- `send target MessageName { field: value, ... }` sends a message to an actor's mailbox.
- `target` must be of type `mailbox<ActorName>`.
- The message type must correspond to an `on` handler defined on the target actor.

### 7.5 Request/response

- Request/response is message passing plus a reply channel.
- `reply<T>` is a predeclared, single-use mailbox with capacity 1.
- A request message that expects a response MUST carry a `reply<T>` field.

### 7.6 Scheduler modes

The runtime provides two scheduler modes:

- **Deterministic mode** (default): single-step, round-robin, fully deterministic message ordering. No OS integration, no threads, no IO. Used for testing, development, and benchmarking.
- **Event-loop mode**: integrates `epoll` (Linux) / `kqueue` (macOS). Actors yield on IO, scheduler polls for readiness. Non-blocking sockets. Used for production.

Both modes guarantee: handlers run to completion, FIFO ordering per actor, no shared state.

## 8. Namespaces and use

Purr namespaces are both:
- the unit of compilation
- the unit of namespacing

A namespace's fully-qualified name is its identity.

### 8.1 Namespace names

- A namespace name is a dot-separated identifier (namespace hierarchy).
	- Example: `xyzcorp.compiler`
- Namespace names are **logical identifiers**, not filesystem paths.
- Namespace names are never inferred.
- File and directory structure are irrelevant to namespace identity.

### 8.2 Per-file namespace declaration

- Every source file MUST declare exactly one namespace name.
- The namespace declaration syntax is:

	`namespace <namespace.name>`

- The namespace declaration MUST be the first non-comment token in the file.
- Nested namespace declarations are forbidden.

### 8.3 Multi-file namespaces

- Multiple source files may declare the same namespace name.
- All such files form a single namespace.
- All symbols declared across those files share one namespace.
- File order has no semantic meaning.

### 8.4 use declarations

Syntax:
- `use <namespace.name>`
- `use <namespace.name> = <alias>`

Rules:
- `use` refers ONLY to fully-qualified namespace names.
- Path-based or relative `use` is forbidden.
- Wildcard imports are forbidden.
- Cyclic namespace dependencies are forbidden.
- There are no visibility modifiers (except actor-private functions; see Section 7.2).
- There are no implicit re-exports in v1.

### 8.5 Aliasing

- Aliases are explicit and local to the file.
- An alias must be an identifier.
- If a `use` omits `= <alias>`, the alias defaults to the final segment of the namespace name.
	- Example: `use xyzcorp.compiler` defaults to alias `compiler`.
- Aliasing is REQUIRED when importing namespaces whose final segment would collide within the file.

### 8.6 Name resolution

- All references to imported symbols MUST be qualified with the namespace alias.
- Unqualified access to imported symbols is forbidden.

### 8.7 Compiler errors (required)

The compiler MUST reject the following with clear diagnostics:

- Missing namespace declaration in a file.
- Multiple namespace declarations in a single file.
- A namespace declaration that is not the first non-comment token.
- Importing by filesystem path (e.g., `use "./x.pu"`).
- Relative `use` declarations.
- Wildcard imports.
- `use` alias that is not an identifier.
- Alias collision within a file (two `use` declarations using the same alias).
- Final-segment collision without explicit aliases.
- Unqualified reference to an imported symbol.

## 9. Runtime ABI (v1)

### 9.1 `string`

- Runtime layout: `{ ptr: *u8, len: i64 }`.
- Strings are immutable.
- Strings are runtime-owned.

### 9.2 `bytes`

- Runtime layout: `{ ptr: *u8, len: i64 }`.

### 9.3 Struct layout

- Fields are laid out in declared order.
- Natural alignment is used.
- No field reordering is performed.

### 9.4 Memory model

- No garbage collection.
- The runtime provides an allocator.
- Heap allocation must be explicit via standard library APIs.
- There are no destructors.
- Leak-on-exit is acceptable.

### 9.5 C type mapping (FFI)

Purr types map to C types as follows for `extern fn` declarations:

| Purr type  | C type              |
|------------|---------------------|
| `bool`     | `_Bool`             |
| `i32`      | `int32_t`           |
| `i64`      | `int64_t`           |
| `f64`      | `double`            |
| `string`   | `purr_string`       |
| `bytes`    | `purr_bytes`        |
| `void`     | `void`              |

`purr_string` and `purr_bytes` are C structs defined in the runtime header matching the layouts in 9.1 and 9.2.

### 9.6 C codegen

- The compiler generates portable C99 from the IR.
- The generated C is compiled by invoking the system C compiler (`cc`).
- The runtime is linked as a C source file alongside the generated code.
- The compiler is responsible for invoking `cc` and producing the final executable.

## 10. Standard library

The standard library defines the only correct way to perform common operations. Programs compose stdlib primitives — they do not reinvent them.

### 10.1 Architecture

The standard library has three layers:

| Library      | Purpose                     | Serves                        |
|--------------|-----------------------------|-------------------------------|
| `purr.std`   | Deterministic primitives    | Runtime + compiler correctness|
| `purr.test`  | Semantic verification       | Language validation (gates)   |
| `purr.bench` | Performance invariants      | System evolution              |

`purr.std` defines what programs are allowed to mean.
`purr.test` proves the model understands the meaning.
`purr.bench` proves the compiler/runtime preserve the meaning efficiently.

### 10.2 Design constraints

- No collections that hide allocation behavior.
- No dynamic formatting APIs.
- All operations O(1) or with documented cost.
- Types encode intent (`list` ≠ `fixed`).
- Effectful operations require explicit capabilities.
- Stdlib grows only through observed need at validation gates, never through upfront design.

### 10.3 `purr.std.bytes`

Explicit layout manipulation for protocol work.

```
fn read_i32_le(b: bytes, offset: i64) i32
fn write_i32_le(b: bytes, offset: i64, value: i32)
fn read_i64_le(b: bytes, offset: i64) i64
fn write_i64_le(b: bytes, offset: i64, value: i64)
fn copy_range(dst: bytes, doff: i64, src: bytes, soff: i64, len: i64)
fn bytes_length(b: bytes) i64
fn bytes_equal(a: bytes, b: bytes) bool
fn bytes_slice(b: bytes, start: i64, end: i64) bytes
```

### 10.4 `purr.std.string`

String operations. Strings are immutable byte sequences.

```
fn length(s: string) i64
fn equals(a: string, b: string) bool
fn starts_with(s: string, prefix: string) bool
fn ends_with(s: string, suffix: string) bool
fn substring(s: string, start: i64, end: i64) string
fn concat(a: string, b: string) string
fn index_of(s: string, needle: string) i64?
```

### 10.5 `purr.std.math`

Total, deterministic operations only.

```
fn min_i32(a: i32, b: i32) i32
fn max_i32(a: i32, b: i32) i32
fn clamp_i32(x: i32, lo: i32, hi: i32) i32
fn abs_i32(x: i32) i32
fn min_i64(a: i64, b: i64) i64
fn max_i64(a: i64, b: i64) i64
```

### 10.6 `purr.std.time`

Deterministic logical time. Requires `time.Clock` capability.

```
fn tick(clock: time.Clock) i64
fn ticks_add(a: i64, delta: i64) i64
fn ticks_before(a: i64, b: i64) bool
```

### 10.7 `purr.std.debug`

Diagnostic output. No formatting, no interpolation.

```
fn print_string(s: string)
fn print_i32(v: i32)
fn print_i64(v: i64)
fn print_bool(v: bool)
fn panic(msg: string)
```

### 10.8 Predeclared container operations

Operations on predeclared type constructors `list<T>`, `map<K, V>`, `fixed<T, N>`, `slice<T>`:

**`list<T>`**
```
fn list_new() list<T>
fn list_append(l: list<T>, value: T) list<T>
fn list_get(l: list<T>, index: i64) T?
fn list_set(l: list<T>, index: i64, value: T) list<T>
fn list_length(l: list<T>) i64
fn list_each(l: list<T>, f: (T): void)
```

**`map<K, V>`**
```
fn map_new() map<K, V>
fn map_get(m: map<K, V>, key: K) V?
fn map_set(m: map<K, V>, key: K, value: V) map<K, V>
fn map_delete(m: map<K, V>, key: K) map<K, V>
fn map_has(m: map<K, V>, key: K) bool
fn map_keys(m: map<K, V>) list<K>
fn map_each(m: map<K, V>, f: (K, V): void)
```

**`fixed<T, N>`**
```
fn fixed_get(a: fixed<T, N>, index: i64) T
fn fixed_set(a: fixed<T, N>, index: i64, value: T)
fn fixed_length(a: fixed<T, N>) i64
fn fixed_slice(a: fixed<T, N>, start: i64, end: i64) slice<T>
```

**`slice<T>`**
```
fn slice_get(s: slice<T>, index: i64) T
fn slice_length(s: slice<T>) i64
fn slice_copy(dst: slice<T>, src: slice<T>)
fn slice_fill(dst: slice<T>, value: T)
```

### 10.9 `purr.test`

Test assertions. Used inside `test` declarations.

```
fn expect_eq_i32(actual: i32, expected: i32)
fn expect_eq_i64(actual: i64, expected: i64)
fn expect_eq_string(actual: string, expected: string)
fn expect_eq_bool(actual: bool, expected: bool)
fn expect_true(cond: bool)
fn expect_false(cond: bool)
fn expect_nil(opt: T?)
fn expect_some(opt: T?)
```

Actor testing (runs on deterministic scheduler):
```
fn spawn_test(actor: ActorInit) mailbox<ActorName>
fn send_test(target: mailbox<ActorName>, msg: Message)
fn drain()
fn expect_output(expected: string)
```

`purrc --test file.purr` collects all `test` declarations and runs them. Output per test:
```
PASS "test name"
FAIL "test name": expected 42, got 0 at line 15
```

Exit code 0 if all pass, 1 if any fail.

### 10.10 `purr.bench`

Benchmarks measure **invariants**, not wall-clock speed. The goal is to detect behavioral changes across compiler and runtime revisions.

Measurable counters (provided by runtime instrumentation):
- `alloc_count` — number of heap allocations during the run clause
- `message_count` — number of messages dispatched during the run clause
- `scheduler_steps` — number of scheduler steps during the run clause
- `bytes_allocated` — total bytes allocated during the run clause

```
bench "map insert 1000" iterations 1000 {
    setup {
        var m = map_new()
    }
    run {
        m = map_set(m, i, i)
    }
}
```

`purrc --bench file.purr` collects all `bench` declarations and runs them. Output format (machine-readable, stable across revisions):
```
BENCH name="map insert 1000" iterations=1000 alloc_count=1000 bytes_allocated=48000 ns=1823400 ns_per_iter=1823
```

Benchmarks run on the deterministic scheduler. Results must be reproducible.

## 11. Complete example

```
namespace app.broker

use purr.std.string
use purr.std.debug

message Subscribe {
    string topic
}

message Publish {
    string topic
    bytes payload
}

message Deliver {
    string topic
    bytes payload
}

actor Broker {
    state map<string, list<mailbox<Consumer>>> topics

    on Subscribe(msg) {
        var subs = map_get(self.topics, msg.topic)
        var updated = list_append(subs, msg.sender)
        self.topics = map_set(self.topics, msg.topic, updated)
    }

    on Publish(msg) {
        var subs = map_get(self.topics, msg.topic)
        list_each(subs, (sub): void =>
            send sub Deliver { topic: msg.topic, payload: msg.payload })
    }
}

actor Consumer {
    on Deliver(msg) {
        debug.print_string(msg.topic)
    }
}

actor Main {
    on start() {
        var broker = spawn Broker { topics: map_new() }
        var c1 = spawn Consumer {}
        send broker Subscribe { topic: "orders" }
    }
}

test "broker routes to subscriber" {
    var broker = spawn_test(Broker { topics: map_new() })
    var consumer = spawn_test(Consumer {})
    send_test(broker, Subscribe { topic: "orders" })
    send_test(broker, Publish { topic: "orders", payload: to_bytes("hello") })
    drain()
    expect_output("orders")
}

bench "broker fanout 100 subscribers" iterations 10000 {
    setup {
        var broker = spawn_test(Broker { topics: map_new() })
        var i: i32 = 0
        for i < 100 {
            var c = spawn_test(Consumer {})
            send_test(broker, Subscribe { topic: "orders" })
            i = i + 1
        }
    }
    run {
        send_test(broker, Publish { topic: "orders", payload: to_bytes("msg") })
        drain()
    }
}
```
