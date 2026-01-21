open Ast
open Lexer

type state = {
  tokens : token array;
  mutable index : int;
}

exception ParseError of string * span

let current st =
  if st.index >= Array.length st.tokens then st.tokens.(Array.length st.tokens - 1)
  else st.tokens.(st.index)

let advance st =
  let tok = current st in
  st.index <- st.index + 1;
  tok

let expect st kind =
  let tok = current st in
  if tok.kind = kind then ignore (advance st)
  else raise (ParseError ("unexpected token", tok.span))

let expect_ident st =
  match (current st).kind with
  | Ident name ->
      let tok = advance st in
      (name, tok.span)
  | _ -> raise (ParseError ("expected identifier", (current st).span))

let rec parse_path st =
  let name, span = expect_ident st in
  let rec loop parts last_span =
    match (current st).kind with
    | Dot ->
        ignore (advance st);
        let next_name, next_span = expect_ident st in
        loop (parts @ [ next_name ]) (span_union last_span next_span)
    | _ -> (parts, last_span)
  in
  loop [ name ] span

let parse_namespace st =
  let start = (advance st).span in
  let path, end_span = parse_path st in
  expect st Semi;
  Namespace (path, span_union start end_span)

let parse_use st =
  let start = (advance st).span in
  let path, end_span = parse_path st in
  expect st Semi;
  Use (path, span_union start end_span)

let parse_literal st =
  match (current st).kind with
  | StringLit s ->
      let tok = advance st in
      Literal (String s, tok.span)
  | IntLit i ->
      let tok = advance st in
      Literal (Int i, tok.span)
  | _ -> raise (ParseError ("expected literal", (current st).span))

let parse_expr st =
  match (current st).kind with
  | Ident _ ->
      let path, span = parse_path st in
      (match (current st).kind with
      | LParen ->
          ignore (advance st);
          let rec args acc =
            match (current st).kind with
            | RParen ->
                ignore (advance st);
                List.rev acc
            | _ ->
                let expr = parse_expr st in
                (match (current st).kind with
                | Comma ->
                    ignore (advance st);
                    args (expr :: acc)
                | RParen ->
                    ignore (advance st);
                    List.rev (expr :: acc)
                | _ -> raise (ParseError ("expected ',' or ')'", (current st).span)))
          in
          let args = args [] in
          Call (path, args, span)
      | _ -> raise (ParseError ("expected call expression", (current st).span)))
  | StringLit _ | IntLit _ -> parse_literal st
  | _ -> raise (ParseError ("expected expression", (current st).span))

let parse_stmt st =
  match (current st).kind with
  | KwReturn ->
      let start = (advance st).span in
      let expr = parse_expr st in
      let end_span =
        match expr with
        | Literal (_, s) -> s
        | Call (_, _, s) -> s
      in
      expect st Semi;
      Return (expr, span_union start end_span)
  | _ ->
      let expr = parse_expr st in
      expect st Semi;
      Expr expr

let parse_func st =
  let start = (advance st).span in
  let name, _ = expect_ident st in
  expect st LParen;
  expect st RParen;
  let return_type, return_span = expect_ident st in
  expect st LBrace;
  let rec body acc =
    match (current st).kind with
    | RBrace ->
        let end_span = (advance st).span in
        (List.rev acc, end_span)
    | EOF -> raise (ParseError ("unexpected end of file", (current st).span))
    | _ ->
        let stmt = parse_stmt st in
        body (stmt :: acc)
  in
  let stmts, end_span = body [] in
  Func { name; return_type; body = stmts; span = span_union start end_span }

let parse_program source =
  let tokens = Array.of_list (lex source) in
  let st = { tokens; index = 0 } in
  let rec items acc =
    match (current st).kind with
    | KwNamespace -> items (parse_namespace st :: acc)
    | KwUse -> items (parse_use st :: acc)
    | KwFunc -> items (parse_func st :: acc)
    | EOF -> List.rev acc
    | _ -> raise (ParseError ("unexpected token", (current st).span))
  in
  let items = items [] in
  let span =
    match items with
    | [] -> { start_pos = 0; end_pos = 0 }
    | _ ->
        let first = List.hd items in
        let last = List.hd (List.rev items) in
        let item_span = function
          | Namespace (_, s) -> s
          | Use (_, s) -> s
          | Func f -> f.span
        in
        span_union (item_span first) (item_span last)
  in
  { items; span }
