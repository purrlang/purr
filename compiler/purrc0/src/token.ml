type t =
  | Actor
  | On
  | Start
  | State  (* M15: Actor state fields *)
  | Spawn  (* M16: Spawn actor instances *)
  | Send   (* M16: Send messages to actors *)
  | Mailbox (* M16: Mailbox type for actor references *)
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
  | Extern  (* M11: FFI - extern C functions *)
  (* M5: Control flow *)
  | If
  | Else
  | For
  | In
  | Test
  (* M10.5: Benchmarking *)
  | Bench
  | Iterations
  | Setup
  | Run
  | Alloc_count
  | Message_count
  | Scheduler_steps
  | Bytes_allocated
  (* M7: Structs *)
  | Struct
  | Dot
  (* M12: Namespaces *)
  | Namespace
  | Use
  (* M14: Messages *)
  | Message
  (* M8: Enums & Switch *)
  | Enum
  | Switch
  | Pipe
  (* M9: Containers *)
  | Nil
  | Option
  | Result
  | List
  | Map
  | Fixed
  | Slice
  | LBracket  (* [ *)
  | RBracket  (* ] *)
  | EOF

type token = {
  kind: t;
  span: Span.t;
}

let pp_kind fmt = function
  | Actor -> Format.fprintf fmt "actor"
  | On -> Format.fprintf fmt "on"
  | Start -> Format.fprintf fmt "start"
  | State -> Format.fprintf fmt "state"
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
  | Extern -> Format.fprintf fmt "extern"
  (* M5: Control flow tokens *)
  | If -> Format.fprintf fmt "if"
  | Else -> Format.fprintf fmt "else"
  | For -> Format.fprintf fmt "for"
  | In -> Format.fprintf fmt "in"
  | Test -> Format.fprintf fmt "test"
  (* M10.5: Benchmark tokens *)
  | Bench -> Format.fprintf fmt "bench"
  | Iterations -> Format.fprintf fmt "iterations"
  | Setup -> Format.fprintf fmt "setup"
  | Run -> Format.fprintf fmt "run"
  | Alloc_count -> Format.fprintf fmt "alloc_count"
  | Message_count -> Format.fprintf fmt "message_count"
  | Scheduler_steps -> Format.fprintf fmt "scheduler_steps"
  | Bytes_allocated -> Format.fprintf fmt "bytes_allocated"
  (* M7: Struct tokens *)
  | Struct -> Format.fprintf fmt "struct"
  | Dot -> Format.fprintf fmt "."
  (* M12: Namespace tokens *)
  | Namespace -> Format.fprintf fmt "namespace"
  | Use -> Format.fprintf fmt "use"
  (* M14: Message tokens *)
  | Message -> Format.fprintf fmt "message"
  (* M8: Enum & Switch tokens *)
  | Enum -> Format.fprintf fmt "enum"
  | Switch -> Format.fprintf fmt "switch"
  | Pipe -> Format.fprintf fmt "|"
  (* M9: Container tokens *)
  | Nil -> Format.fprintf fmt "nil"
  | Option -> Format.fprintf fmt "option"
  | Result -> Format.fprintf fmt "result"
  | List -> Format.fprintf fmt "list"
  | Map -> Format.fprintf fmt "map"
  | Fixed -> Format.fprintf fmt "fixed"
  | Slice -> Format.fprintf fmt "slice"
  | LBracket -> Format.fprintf fmt "["
  | RBracket -> Format.fprintf fmt "]"

let pp fmt tok =
  Format.fprintf fmt "%a at %a" pp_kind tok.kind Span.pp tok.span
