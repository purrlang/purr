# purr0 Grammar (EBNF)

Terminals: identifier, int-literal, bool-literal (`true|false`), string-literal, punctuation

Program := { Declaration }

Declaration := StructDecl | FunctionDecl

StructDecl := "struct" Identifier "{" FieldList "}"

FieldList := { Identifier ":" Type ";" }

FunctionDecl := "func" Identifier "(" [ ParamList ] ")" "->" Type Block

ParamList := Param { "," Param }
Param := Identifier ":" Type

Type := "i32" | "i64" | "bool" | "string" | "void" | Identifier

Block := "{" { Statement } "}"

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
Primary := int-literal | bool-literal | string-literal | Identifier | FunctionCall | StructLiteral | "(" Expression ")" | FieldAccess

FunctionCall := Identifier "(" [ ArgList ] ")"
ArgList := Expression { "," Expression }

StructLiteral := Identifier "{" [ InitList ] "}"
InitList := Identifier ":" Expression { "," Identifier ":" Expression }

FieldAccess := Expression "." Identifier

Notes:
- Statements end with `;` and struct fields are terminated by `;` to simplify parsing.
- No assignment statements (only immutable `let` bindings and shadowing).

