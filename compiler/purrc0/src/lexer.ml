open Ast

type token_kind =
  | Ident of string
  | IntLit of int
  | StringLit of string
  | KwNamespace
  | KwUse
  | KwFunc
  | KwReturn
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Dot
  | Comma
  | Semi
  | EOF

type token = {
  kind : token_kind;
  span : span;
}

exception LexError of string * int

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let keyword_or_ident s =
  match s with
  | "namespace" -> KwNamespace
  | "use" -> KwUse
  | "func" -> KwFunc
  | "return" -> KwReturn
  | _ -> Ident s

let lex source =
  let len = String.length source in
  let rec skip_ws i =
    if i >= len then i
    else
      match source.[i] with
      | ' ' | '\t' | '\r' | '\n' -> skip_ws (i + 1)
      | _ -> i
  in
  let rec lex_ident i =
    let rec loop j =
      if j < len && (is_alpha source.[j] || is_digit source.[j]) then loop (j + 1)
      else j
    in
    let j = loop i in
    let text = String.sub source i (j - i) in
    (text, j)
  in
  let rec lex_number i =
    let rec loop j = if j < len && is_digit source.[j] then loop (j + 1) else j in
    let j = loop i in
    let text = String.sub source i (j - i) in
    (int_of_string text, j)
  in
  let rec lex_string i =
    let buf = Buffer.create 16 in
    let rec loop j =
      if j >= len then raise (LexError ("unterminated string", i))
      else
        match source.[j] with
        | '"' -> (Buffer.contents buf, j + 1)
        | '\\' ->
            if j + 1 >= len then raise (LexError ("unterminated escape", j))
            else (
              Buffer.add_char buf source.[j + 1];
              loop (j + 2))
        | c ->
            Buffer.add_char buf c;
            loop (j + 1)
    in
    loop (i + 1)
  in
  let rec next i acc =
    let i = skip_ws i in
    if i >= len then
      let span = { start_pos = i; end_pos = i } in
      List.rev ({ kind = EOF; span } :: acc)
    else
      let start_pos = i in
      let add kind j =
        let tok = { kind; span = { start_pos; end_pos = j } } in
        next j (tok :: acc)
      in
      match source.[i] with
      | '(' -> add LParen (i + 1)
      | ')' -> add RParen (i + 1)
      | '{' -> add LBrace (i + 1)
      | '}' -> add RBrace (i + 1)
      | '.' -> add Dot (i + 1)
      | ',' -> add Comma (i + 1)
      | ';' -> add Semi (i + 1)
      | '"' ->
          let text, j = lex_string i in
          add (StringLit text) j
      | c when is_alpha c ->
          let text, j = lex_ident i in
          add (keyword_or_ident text) j
      | c when is_digit c ->
          let value, j = lex_number i in
          add (IntLit value) j
      | c -> raise (LexError ("unexpected character: " ^ String.make 1 c, i))
  in
  next 0 []
