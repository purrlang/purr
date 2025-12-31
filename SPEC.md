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
- Modules/imports: **YES**
- `result<T, E>`: **YES**
- `T?` optionals: **YES** (syntax sugar over `option<T>`)
- Lambdas: **YES** (expression-only, no captures)
- Generics: **NO** (except the built-in type constructors `option<T>` and `result<T,E>`)
- Effects and `do`: **YES** (syntax-level only)
- Concurrency (`actor/stream/spawn/await`): **reserved only**
- FFI: **NO**

Non-goals in v1:
- Garbage collection
- async/await
- pattern matching
- inheritance/interfaces
- reflection
- macros
- user-exposed custom allocators

## 2. Lexical specification

### 2.1 Comments

- Line comments begin with `//` and continue to the end of the line.
- There are no block comments.

### 2.2 Whitespace

- Whitespace is insignificant except as a separator.
- Semicolons are **required** to terminate statements.

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
- punctuation and operator tokens

Notes:
- Statements must end with `;`.
- `if`/`else` are statements only.

### 3.1 Program structure

Program := ModuleDecl { ImportDecl } { Declaration }

ModuleDecl := "module" ModuleName ";"

ModuleName := Identifier { "." Identifier }

ImportDecl := "import" ModuleName [ "as" Identifier ] ";"

Declaration := StructDecl | FunctionDecl

StructDecl := "struct" Identifier "{" FieldList "}"
FieldList := { Identifier ":" Type ";" }

FunctionDecl := "func" Identifier "(" [ ParamList ] ")" "->" Type Block

ParamList := Param { "," Param }
Param := Identifier ":" Type

Block := "{" { Statement } "}"

### 3.2 Types

PrimitiveType := "bool" | "i32" | "i64" | "f64" | "string" | "bytes" | "void"

Type :=
	PrimitiveType
	| Identifier
	| OptionType
	| ResultType
	| FunctionType
	| Type "?"

OptionType := "option" "<" Type ">"
ResultType := "result" "<" Type "," Type ">"

FunctionType := "(" [ TypeList ] ")" ":" Type
TypeList := Type { "," Type }

Notes:
- `T?` is syntax sugar for `option<T>`.
- There are no user-defined generics.

### 3.3 Statements

Statement :=
	LetDecl
	| IfStmt
	| WhileStmt
	| ReturnStmt
	| DoStmt
	| ExprStmt

LetDecl := "let" Identifier ":" Type "=" Expression ";"

IfStmt := "if" "(" Expression ")" Block [ "else" Block ]

WhileStmt := "while" "(" Expression ")" Block

ReturnStmt := "return" [ Expression ] ";"

DoStmt := "do" Block

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

Primary :=
	int-literal
	| float-literal
	| bool-literal
	| string-literal
	| Identifier
	| StructLiteral
	| Lambda
	| "(" Expression ")"

StructLiteral := Identifier "{" [ InitList ] "}"
InitList := Identifier ":" Expression { "," Identifier ":" Expression }

## 4. Static semantics

### 4.1 Immutability and binding

- `let` is the only binding form.
- All bindings are immutable.
- Rebinding (shadowing) is permitted **only within function scope**.
- Rebinding does not mutate shared state; it creates a new binding.

### 4.2 Type checking

- There are no implicit conversions.
- For binary operators, both operands must have the same type unless otherwise specified below.

#### 4.2.1 Integer literals

- Integer literals are type-checked against context (e.g., `let x: i32 = 1;`).
- An integer literal must fit in its target type (`i32` or `i64`), or it is a compile-time error.
- An integer literal without a required type context is a compile-time error.

#### 4.2.2 Floating literals

- A floating literal has type `f64`.
- If a floating literal cannot be represented as `f64`, it is a compile-time error.

#### 4.2.3 Arithmetic operators

For operands of type `i32`, `i64`, or `f64`:
- `+ - * /` are defined.
- Operand types must match exactly.

#### 4.2.4 Comparison operators

- `< <= > >=` are defined for `i32`, `i64`, and `f64` only.
- Operand types must match exactly.

#### 4.2.5 Boolean operators

- `!` requires `bool`.
- `&&` and `||` require `bool` operands and produce `bool`.

### 4.3 Optionals

- `T?` is syntax sugar for `option<T>`.
- `option<T>` is a value type with the runtime layout: `{ has: bool, value: T }`.

### 4.4 Results

- `result<T, E>` is a value type with runtime layout: `{ ok: bool, t: T, e: E }`.
- Errors are values; there are no exceptions.

### 4.5 Equality

`==` and `!=` are defined for:
- `bool`, `i32`, `i64`
- `f64`
- `string` (bytewise)
- `struct` types (fieldwise, in declared field order)

For `f64`, equality uses IEEE-754 semantics:
- `NaN == x` is false for all `x` (including `NaN`).
- `NaN != x` is true for all `x`.

Equality is undefined for all other types.

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

## 6. Effects and `do`

### 6.1 `do` blocks

- `do { ... }` is a syntactic effect boundary.
- Purr v1 has no effect typing.

### 6.2 Effect restrictions

- IO, time, randomness, and allocation are effects.
- Any operation the implementation designates as effectful (including stdlib effectful APIs) is permitted only within a `do` block.
- Calling an effectful API outside a `do` block is a compile-time error.

## 7. Modules and imports

Purr modules are both:
- the unit of compilation
- the unit of namespacing

A module’s fully-qualified name is its identity.

### 7.1 Module names

- A module name is a dot-separated identifier (namespace hierarchy).
	- Example: `xyzcorp.compiler`
- Module names are **logical identifiers**, not filesystem paths.
- Module names are never inferred.
- File and directory structure are irrelevant to module identity.

### 7.2 Per-file module declaration

- Every source file MUST declare exactly one module name.
- The module declaration syntax is:

	`module <module.name>;`

- The module declaration MUST be the first non-comment token in the file.
- Nested module declarations are forbidden.

### 7.3 Multi-file modules

- Multiple source files may declare the same module name.
- All such files form a single module.
- All symbols declared across those files share one module namespace.
- File order has no semantic meaning.

### 7.4 Imports

Syntax:
- `import <module.name>;`
- `import <module.name> as <alias>;`

Rules:
- Imports refer ONLY to fully-qualified module names.
- Path-based or relative imports are forbidden.
- Wildcard imports are forbidden.
- Cyclic imports are forbidden.
- There are no visibility modifiers in v1.
- There are no implicit re-exports in v1.

### 7.5 Aliasing

- Aliases are explicit and local to the importing file.
- An alias must be an identifier.
- If an import omits `as <alias>`, the alias defaults to the final segment of the module name.
	- Example: `import xyzcorp.compiler;` defaults to alias `compiler`.
- Aliasing is REQUIRED when importing modules whose final segment would collide within the file.
	- Example: `xyzcorp.compiler` and `abccorp.compiler`.

### 7.6 Name resolution

- All references to imported symbols MUST be qualified with the module alias.
- Unqualified access to imported symbols is forbidden.

Example (qualified access required):

```text
module app.main;

import xyzcorp.compiler as xyz;

func main() -> i32 {
	// OK: qualified
	let v: i32 = xyz.version();

	// ERROR: unqualified
	// let v2: i32 = version();
	return 0;
}
```

### 7.7 Examples

#### Multi-vendor collision (alias required)

```text
module app.main;

import xyzcorp.compiler as xyz;
import abccorp.compiler as abc;

func main() -> i32 {
	let a: i32 = xyz.version();
	let b: i32 = abc.version();
	return a + b;
}
```

#### Multi-file module (single namespace)

File A:

```text
module xyzcorp.compiler;

func version() -> i32 { return 1; }
```

File B:

```text
module xyzcorp.compiler;

func build() -> i32 { return version(); }
```

Both files contribute to the same `xyzcorp.compiler` module namespace.

### 7.8 Compiler errors (required)

The compiler MUST reject the following with clear diagnostics:

- Missing module declaration in a file.
- Multiple module declarations in a single file.
- A module declaration that is not the first non-comment token.
- Importing by filesystem path (e.g., `import "./x.pu";`).
- Relative imports.
- Wildcard imports.
- Import alias that is not an identifier.
- Alias collision within a file (two imports using the same alias).
- Final-segment collision without explicit aliases:
	- `import xyzcorp.compiler; import abccorp.compiler;` (both default alias `compiler`).
- Unqualified reference to an imported symbol.

## 8. Runtime ABI (v1)

### 8.1 `string`

- Runtime layout: `{ ptr: *u8, len: i64 }`.
- Strings are immutable.
- Strings are runtime-owned.

### 8.2 `bytes`

- Runtime layout: `{ ptr: *u8, len: i64 }`.

### 8.3 Struct layout

- Fields are laid out in declared order.
- Natural alignment is used.
- No field reordering is performed.

### 8.4 Memory model

- No garbage collection.
- The runtime provides an allocator.
- There are no destructors.
- Leak-on-exit is acceptable.
