# purr0 Grammar (EBNF)

Terminals: identifier, int-literal, bool-literal (`true|false`), string-literal, punctuation

Program := { Declaration }

Declaration := StructDecl | FunctionDecl

StructDecl := "struct" Identifier "{" FieldList "}"

FieldList := { Identifier ":" Type ";" }

FunctionDecl := "func" Identifier "(" [ ParamList ] ")" "->" Type Block

ParamList := Param { "," Param }
Param := Identifier ":" Type

BaseType := "i32" | "i64" | "bool" | "string" | "void" | Identifier

Type := BaseType [ "?" ]

Block := "{" { Statement } "}"

Notes:
- `T?` is syntactic sugar for `option<T>` and is rewritten by the parser into the explicit `option<T>` type. It does not introduce nullable semantics or implicit conversions.
Statement :=
  LetDecl
  | IfStmt
  | WhileStmt
  | ReturnStmt
  | ExprStmt

LetDecl := "let" Identifier ":" Type "=" Expression ";"

IfStmt := "if" "(" Expression ")" Block [ "else" Block ]

WhileStmt := "while" "(" Expression ")" Block

ReturnStmt := "return" [ Expression ] ";"

ExprStmt := Expression ";"

Expression :=
  LogicalOrExpr

LogicalOrExpr := LogicalAndExpr { "||" LogicalAndExpr }
LogicalAndExpr := EqualityExpr { "&&" EqualityExpr }
EqualityExpr := RelationalExpr { ("==" | "!=") RelationalExpr }
RelationalExpr := AddExpr { ("<" | "<=" | ">" | ">=") AddExpr }
AddExpr := MulExpr { ("+" | "-") MulExpr }
MulExpr := UnaryExpr { ("*" | "/") UnaryExpr }
UnaryExpr := [ "!" | "-" ] Primary

Lambda := "(" [ ParamList ] ")" ":" Type "=>" Expression

Primary := int-literal | bool-literal | string-literal | Identifier | FunctionCall | StructLiteral | Lambda | "(" Expression ")" | FieldAccess

FunctionCall := Identifier "(" [ ArgList ] ")"
ArgList := Expression { "," Expression }

Notes:
- `Lambda` is an expression-only form and is syntactic sugar for an anonymous `func`.
- Syntax requires explicit parameter types and an explicit return type: `(x: i32): i32 => x + 1`.
- No block-bodied lambdas (`=>` must be followed by a single expression).
StructLiteral := Identifier "{" [ InitList ] "}"
InitList := Identifier ":" Expression { "," Identifier ":" Expression }

FieldAccess := Expression "." Identifier

Notes:
- Statements end with `;` and struct fields are terminated by `;` to simplify parsing.
- No assignment statements (only immutable `let` bindings and shadowing).

