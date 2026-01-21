# Purr v1 Language Specification (Normative)

This document is the **normative** language specification for **Purr v1**.

- This spec is **requirements-only**.
- If a behavior is not specified here, it is **undefined and unsupported**.
- This spec contains **no alternatives**, **no philosophy**, and **no future notes**.

## 1. Scope

Targets:
- Native compilation via LLVM: **YES**
- WASM: **NO**
- Supported OS: Linux, macOS, Windows

Language features in v1:
- Namespaces / `use`: **YES**
- `result<T, E>`: **YES** (predeclared type constructor)
- `T?` optionals: **YES** (syntax sugar over `option<T>`)
- Lambdas: **YES** (expression-only, no captures)
- Generics: **NO** (except built-in type constructors)
- Effects: **YES** (explicit capability inputs; no `do`)
- Concurrency (`actor/on/spawn`): **YES**
- Interfaces (structural, method-only): **YES**
- FFI: **NO**

Non-goals in v1:
- Garbage collection
- async/await
- implicit polymorphism
- reflection
- macros
- user-exposed custom allocators

## 2. Lexical specification

### 2.1 Comments

- Line comments begin with `//` and continue to the end of the line.
- There are no block comments.

### 2.2 Whitespace

- Whitespace is insignificant except as a separator.
- Semicolons are **required** to terminate non-block statements.

### 2.3 Identifiers

- Identifiers use ASCII characters only.
- Allowed characters: `[A-Za-z_]` followed by zero or more `[A-Za-z0-9_]`.
- Identifiers must not start with a digit.
- Unicode identifiers are not supported in v1.

### 2.4 Keywords

Keywords are defined in `KEYWORDS.md`. Keywords are not valid identifiers.

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
- Non-block statements must end with `;`.
- `if`/`else` are statements only.

### 3.1 Program structure

Program := NamespaceDecl { UseDecl } { Declaration }

NamespaceDecl := "namespace" NamespaceName ";"

NamespaceName := Identifier { "." Identifier }

UseDecl := "use" NamespaceName [ "=" Identifier ] ";"

Declaration := StructDecl | EnumDecl | InterfaceDecl | ActorDecl | FunctionDecl

StructDecl := "struct" Identifier "{" FieldList "}"
FieldList := { Type Identifier ";" }

EnumDecl := "enum" Identifier "{" EnumCaseList "}"
EnumCaseList := { EnumCase }
EnumCase := "case" Identifier [ "(" [ ParamList ] ")" ] ";"

InterfaceDecl := "interface" Identifier "{" { MethodSig } "}"
MethodSig := "func" Identifier "(" [ ParamList ] ")" Type ";"

ActorDecl := "actor" Identifier "{" { ActorField | OnHandler } "}"
ActorField := Type Identifier ";"
OnHandler := "on" Identifier "(" [ ParamList ] ")" [ Type ] Block

FunctionDecl := "func" FunctionName "(" [ ParamList ] ")" [ Type ] Block
FunctionName := Identifier | Identifier "." Identifier

ParamList := Param { "," Param }
Param := Identifier ":" Type

Block := "{" { Statement } "}"

### 3.2 Types

Type :=
	TypeName
	| TypeCtor
	| FunctionType
	| Type "?"

TypeName := Identifier

TypeCtor := Identifier "<" Type { "," Type } ">"

FunctionType := "(" [ TypeList ] ")" ":" Type
TypeList := Type { "," Type }

Notes:
- `T?` is syntax sugar for `option<T>`.
- There are no user-defined generics; only predeclared type constructors are permitted.

### 3.3 Statements

Statement :=
	VarDecl
	| AssignStmt
	| IfStmt
	| ForStmt
	| SwitchStmt
	| ReturnStmt
	| ExprStmt
	| Block

VarDecl := "var" Identifier [ ":" Type ] "=" Expression ";"

AssignStmt := LValue "=" Expression ";"

LValue := Identifier { "." Identifier }

IfStmt := "if" Expression Block [ "else" Block ]

ForStmt := "for" Expression Block

SwitchStmt := "switch" Expression "{" { CaseClause } [ ElseClause ] "}"
CaseClause := "case" CasePattern Block
ElseClause := "else" Block
CasePattern := Literal | Identifier

ReturnStmt := "return" [ Expression ] ";"

ExprStmt := Expression ";"

### 3.4 Expressions

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

SpawnExpr := "spawn" StructLiteral

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

- Predeclared type names: `bool`, `i32`, `i64`, `f64`, `string`, `bytes`, `void`.
- Predeclared type constructors: `option<T>`, `result<T,E>`, `mailbox<T>`, `reply<T>`.
- Any other `TypeCtor` is a compile-time error.

#### 4.3.0 `var` inference

- If a `var` declaration omits its type, it is inferred from the initializer.
- If the initializer is `nil`, the `var` declaration MUST provide an explicit optional type.
- If the type cannot be inferred unambiguously, it is a compile-time error.

#### 4.3.0a Return types

- A function or handler with no return type defaults to `void`.
- Return statements in `void` functions must omit a value.

#### 4.3.1 Integer literals

- Integer literals are type-checked against context (e.g., `var x: i32 = 1;`).
- An integer literal must fit in its target type (`i32` or `i64`), or it is a compile-time error.
- An integer literal without a required type context is a compile-time error.

#### 4.3.2 Floating literals

- A floating literal has type `f64`.
- If a floating literal cannot be represented as `f64`, it is a compile-time error.

#### 4.3.3 Arithmetic operators

For operands of type `i32`, `i64`, or `f64`:
- `+ - * /` are defined.
- Operand types must match exactly.

#### 4.3.4 Comparison operators

- `< <= > >=` are defined for `i32`, `i64`, and `f64` only.
- Operand types must match exactly.

#### 4.3.5 Boolean operators

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
- The standard library defines opaque capability types (e.g., `io.Reader`, `time.Clock`, `rand.Source`, `mem.Allocator`).
- A function that performs effects MUST take the required capability as an explicit parameter.

### 6.2 Effect restrictions

- There are no implicit or ambient effects.
- The compiler MUST reject calls to effectful APIs unless the required capability is in scope.

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
	- actor-owned fields declared directly in the actor body
	- one or more `on` handlers
- Actor fields are accessible only within `on` handlers of the same actor.
- Handlers may mutate actor fields.

### 7.3 Spawn and mailboxes

- `spawn ActorInit` creates a new actor instance, where `ActorInit` is a struct literal for the actor type.
- The actor initialization value must provide all actor fields.
- Mailboxes are bounded; capacity is defined by the runtime for each actor type.
- `spawn` returns `mailbox<ActorName.Message>`.

### 7.4 Request/response

- Request/response is message passing plus a reply channel.
- `reply<T>` is a predeclared, single-use mailbox with capacity 1.
- A request message that expects a response MUST carry a `reply<T>`.

## 8. Namespaces and use

Purr namespaces are both:
- the unit of compilation
- the unit of namespacing

A namespace’s fully-qualified name is its identity.

### 8.1 Namespace names

- A namespace name is a dot-separated identifier (namespace hierarchy).
	- Example: `xyzcorp.compiler`
- Namespace names are **logical identifiers**, not filesystem paths.
- Namespace names are never inferred.
- File and directory structure are irrelevant to namespace identity.

### 8.2 Per-file namespace declaration

- Every source file MUST declare exactly one namespace name.
- The namespace declaration syntax is:

	`namespace <namespace.name>;`

- The namespace declaration MUST be the first non-comment token in the file.
- Nested namespace declarations are forbidden.

### 8.3 Multi-file namespaces

- Multiple source files may declare the same namespace name.
- All such files form a single namespace.
- All symbols declared across those files share one namespace.
- File order has no semantic meaning.

### 8.4 use declarations

Syntax:
- `use <namespace.name>;`
- `use <namespace.name> = <alias>;`

Rules:
- `use` refers ONLY to fully-qualified namespace names.
- Path-based or relative `use` is forbidden.
- Wildcard imports are forbidden.
- Cyclic namespace dependencies are forbidden.
- There are no visibility modifiers in v1.
- There are no implicit re-exports in v1.

### 8.5 Aliasing

- Aliases are explicit and local to the file.
- An alias must be an identifier.
- If a `use` omits `= <alias>`, the alias defaults to the final segment of the namespace name.
	- Example: `use xyzcorp.compiler;` defaults to alias `compiler`.
- Aliasing is REQUIRED when importing namespaces whose final segment would collide within the file.
	- Example: `xyzcorp.compiler` and `abccorp.compiler`.

### 8.6 Name resolution

- All references to imported symbols MUST be qualified with the namespace alias.
- Unqualified access to imported symbols is forbidden.

Example (qualified access required):

```text
namespace app.main;

use xyzcorp.compiler = xyz;

func main() i32 {
	// OK: qualified
	var v: i32 = xyz.version();

	// ERROR: unqualified
	// var v2: i32 = version();
	return 0;
}
```

### 8.7 Examples

#### Multi-vendor collision (alias required)

```text
namespace app.main;

use xyzcorp.compiler = xyz;
use abccorp.compiler = abc;

func main() i32 {
	var a: i32 = xyz.version();
	var b: i32 = abc.version();
	return a + b;
}
```

#### Multi-file namespace (single namespace)

File A:

```text
namespace xyzcorp.compiler;

func version() i32 { return 1; }
```

File B:

```text
namespace xyzcorp.compiler;

func build() i32 { return version(); }
```

Both files contribute to the same `xyzcorp.compiler` namespace.

### 8.8 Compiler errors (required)

The compiler MUST reject the following with clear diagnostics:

- Missing namespace declaration in a file.
- Multiple namespace declarations in a single file.
- A namespace declaration that is not the first non-comment token.
- Importing by filesystem path (e.g., `use "./x.pu";`).
- Relative `use` declarations.
- Wildcard imports.
- `use` alias that is not an identifier.
- Alias collision within a file (two `use` declarations using the same alias).
- Final-segment collision without explicit aliases:
	- `use xyzcorp.compiler; use abccorp.compiler;` (both default alias `compiler`).
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
