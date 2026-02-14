type t =
  | Actor
  | On
  | Start
  | Print
  | Var
  | True
  | False
  | Ident of string
  | IntLit of int64
  | StringLit of string
  | Type of string  (* i32, i64, string, bool *)
  | LBrace
  | RBrace
  | LParen
  | RParen
  | Semicolon
  | Colon
  | Equals
  (* M3: Operators and NEWLINE *)
  | Newline
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | EqualEqual
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | And
  | Or
  | Not
  | Comma
  (* M4: Functions *)
  | Fn
  | Return
  | Arrow
  (* M5: Control flow *)
  | If
  | Else
  | For
  | In
  | Test
  | EOF

type token = {
  kind: t;
  span: Span.t;
}

let pp_kind fmt = function
  | Actor -> Format.fprintf fmt "actor"
  | On -> Format.fprintf fmt "on"
  | Start -> Format.fprintf fmt "start"
  | Print -> Format.fprintf fmt "print"
  | Var -> Format.fprintf fmt "var"
  | True -> Format.fprintf fmt "true"
  | False -> Format.fprintf fmt "false"
  | Ident s -> Format.fprintf fmt "Ident(%s)" s
  | IntLit i -> Format.fprintf fmt "IntLit(%Ld)" i
  | StringLit s -> Format.fprintf fmt "StringLit(%S)" s
  | Type t -> Format.fprintf fmt "Type(%s)" t
  | LBrace -> Format.fprintf fmt "{"
  | RBrace -> Format.fprintf fmt "}"
  | LParen -> Format.fprintf fmt "("
  | RParen -> Format.fprintf fmt ")"
  | Semicolon -> Format.fprintf fmt ";"
  | Colon -> Format.fprintf fmt ":"
  | Equals -> Format.fprintf fmt "="
  | Newline -> Format.fprintf fmt "<newline>"
  | Plus -> Format.fprintf fmt "+"
  | Minus -> Format.fprintf fmt "-"
  | Star -> Format.fprintf fmt "*"
  | Slash -> Format.fprintf fmt "/"
  | Percent -> Format.fprintf fmt "%%"
  | EqualEqual -> Format.fprintf fmt "=="
  | NotEqual -> Format.fprintf fmt "!="
  | Less -> Format.fprintf fmt "<"
  | LessEqual -> Format.fprintf fmt "<="
  | Greater -> Format.fprintf fmt ">"
  | GreaterEqual -> Format.fprintf fmt ">="
  | And -> Format.fprintf fmt "&&"
  | Or -> Format.fprintf fmt "||"
  | Not -> Format.fprintf fmt "!"
  | Comma -> Format.fprintf fmt ","
  | EOF -> Format.fprintf fmt "EOF"
  (* M4: Function tokens *)
  | Fn -> Format.fprintf fmt "fn"
  | Return -> Format.fprintf fmt "return"
  | Arrow -> Format.fprintf fmt "->"
  (* M5: Control flow tokens *)
  | If -> Format.fprintf fmt "if"
  | Else -> Format.fprintf fmt "else"
  | For -> Format.fprintf fmt "for"
  | In -> Format.fprintf fmt "in"
  | Test -> Format.fprintf fmt "test"

let pp fmt tok =
  Format.fprintf fmt "%a at %a" pp_kind tok.kind Span.pp tok.span
