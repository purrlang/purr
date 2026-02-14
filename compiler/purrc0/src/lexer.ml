type state = {
  file: string;
  source: string;
  mutable pos: int;
  mutable line: int;
  mutable col: int;
}

let make file source = {
  file;
  source;
  pos = 0;
  line = 1;
  col = 1;
}

let current st =
  if st.pos >= String.length st.source then None
  else Some st.source.[st.pos]

let advance st =
  match current st with
  | None -> ()
  | Some c ->
      st.pos <- st.pos + 1;
      if c = '\n' then (
        st.line <- st.line + 1;
        st.col <- 1
      ) else (
        st.col <- st.col + 1
      )

let peek st =
  if st.pos + 1 >= String.length st.source then None
  else Some st.source.[st.pos + 1]

let skipHorizontalWhitespace st =
  while match current st with
    | Some (' ' | '\t') -> advance st; true
    | _ -> false
  do () done

let skipWhitespace st =
  while match current st with
    | Some (' ' | '\t' | '\n' | '\r') -> advance st; true
    | _ -> false
  do () done

let isIdentifierStart c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let isIdentifierChar c =
  isIdentifierStart c || (c >= '0' && c <= '9')

let readIdentifier st =
  let start = st.pos in
  while match current st with
    | Some c when isIdentifierChar c -> advance st; true
    | _ -> false
  do () done;
  String.sub st.source start (st.pos - start)

let readStringLiteral st =
  advance st; (* skip opening quote *)
  let buf = Buffer.create 16 in
  let rec loop () =
    match current st with
    | None -> Error "Unterminated string literal"
    | Some '"' -> advance st; Ok (Buffer.contents buf)
    | Some '\\' ->
        advance st;
        (match current st with
        | Some '"' -> Buffer.add_char buf '"'; advance st; loop ()
        | Some '\\' -> Buffer.add_char buf '\\'; advance st; loop ()
        | Some 'n' -> Buffer.add_char buf '\n'; advance st; loop ()
        | Some 't' -> Buffer.add_char buf '\t'; advance st; loop ()
        | Some 'r' -> Buffer.add_char buf '\r'; advance st; loop ()
        | _ -> Error "Invalid escape sequence in string literal")
    | Some c -> Buffer.add_char buf c; advance st; loop ()
  in
  loop ()

let readInteger st =
  let start = st.pos in
  while match current st with
    | Some c when c >= '0' && c <= '9' -> advance st; true
    | _ -> false
  do () done;
  let num_str = String.sub st.source start (st.pos - start) in
  try
    let n = Int64.of_string num_str in
    Ok n
  with _ ->
    Error (Printf.sprintf "Integer literal out of range: %s" num_str)

let tokenize file source =
  let st = make file source in
  let rec loop tokens =
    skipHorizontalWhitespace st;
    match current st with
    | None ->
        let span = Span.make file st.line st.col in
        Ok (List.rev ({ Token.kind = Token.EOF; span } :: tokens))
    | Some '\n' ->
        (* Check if we should emit NEWLINE token *)
        let should_emit_newline = match tokens with
          | [] -> false  (* Skip initial newlines *)
          | last :: _ ->
              (* Don't emit newline after operators, commas, opening brackets, or after just-opened *)
              (match last.Token.kind with
               | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
               | Token.EqualEqual | Token.NotEqual | Token.Less | Token.LessEqual
               | Token.Greater | Token.GreaterEqual | Token.And | Token.Or | Token.Comma
               | Token.LBrace | Token.LParen -> false
               | _ -> true)
        in
        let span = Span.make file st.line st.col in
        advance st;
        if should_emit_newline then
          loop ({ Token.kind = Token.Newline; span } :: tokens)
        else
          loop tokens
    | Some '\r' ->
        advance st;
        loop tokens  (* Skip carriage return *)
    | Some '{' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.LBrace; span } :: tokens)
    | Some '}' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.RBrace; span } :: tokens)
    | Some '(' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.LParen; span } :: tokens)
    | Some ')' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.RParen; span } :: tokens)
    | Some ':' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.Colon; span } :: tokens)
    | Some '=' ->
        let span = Span.make file st.line st.col in
        advance st;
        (match current st with
        | Some '=' ->
            advance st;
            loop ({ Token.kind = Token.EqualEqual; span } :: tokens)
        | _ ->
            loop ({ Token.kind = Token.Equals; span } :: tokens))
    | Some '!' ->
        let span = Span.make file st.line st.col in
        advance st;
        (match current st with
        | Some '=' ->
            advance st;
            loop ({ Token.kind = Token.NotEqual; span } :: tokens)
        | _ ->
            loop ({ Token.kind = Token.Not; span } :: tokens))
    | Some '<' ->
        let span = Span.make file st.line st.col in
        advance st;
        (match current st with
        | Some '=' ->
            advance st;
            loop ({ Token.kind = Token.LessEqual; span } :: tokens)
        | _ ->
            loop ({ Token.kind = Token.Less; span } :: tokens))
    | Some '>' ->
        let span = Span.make file st.line st.col in
        advance st;
        (match current st with
        | Some '=' ->
            advance st;
            loop ({ Token.kind = Token.GreaterEqual; span } :: tokens)
        | _ ->
            loop ({ Token.kind = Token.Greater; span } :: tokens))
    | Some '&' ->
        let span = Span.make file st.line st.col in
        advance st;
        (match current st with
        | Some '&' ->
            advance st;
            loop ({ Token.kind = Token.And; span } :: tokens)
        | _ ->
            Error (Error.make file span.Span.line span.Span.col "Expected '&' after '&'"))
    | Some '|' ->
        let span = Span.make file st.line st.col in
        advance st;
        (match current st with
        | Some '|' ->
            advance st;
            loop ({ Token.kind = Token.Or; span } :: tokens)
        | _ ->
            Error (Error.make file span.Span.line span.Span.col "Expected '|' after '|'"))
    | Some ';' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.Semicolon; span } :: tokens)
    | Some '+' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.Plus; span } :: tokens)
    | Some '-' ->
        let span = Span.make file st.line st.col in
        advance st;
        (match current st with
        | Some '>' ->
            advance st;
            loop ({ Token.kind = Token.Arrow; span } :: tokens)
        | _ ->
            loop ({ Token.kind = Token.Minus; span } :: tokens))
    | Some '*' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.Star; span } :: tokens)
    | Some '/' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.Slash; span } :: tokens)
    | Some '%' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.Percent; span } :: tokens)
    | Some ',' ->
        let span = Span.make file st.line st.col in
        advance st;
        loop ({ Token.kind = Token.Comma; span } :: tokens)
    | Some '"' ->
        let span = Span.make file st.line st.col in
        (match readStringLiteral st with
        | Error msg -> Error (Error.make file span.Span.line span.Span.col msg)
        | Ok s ->
            loop ({ Token.kind = Token.StringLit s; span } :: tokens))
    | Some c when c >= '0' && c <= '9' ->
        let span = Span.make file st.line st.col in
        (match readInteger st with
        | Error msg -> Error (Error.make file span.Span.line span.Span.col msg)
        | Ok n ->
            loop ({ Token.kind = Token.IntLit n; span } :: tokens))
    | Some c when isIdentifierStart c ->
        let span = Span.make file st.line st.col in
        let ident = readIdentifier st in
        let kind = match ident with
          | "actor" -> Token.Actor
          | "on" -> Token.On
          | "start" -> Token.Start
          | "print_string" -> Token.Print
          | "var" -> Token.Var
          | "true" -> Token.True
          | "false" -> Token.False
          | "i32" -> Token.Type "i32"
          | "i64" -> Token.Type "i64"
          | "string" -> Token.Type "string"
          | "bool" -> Token.Type "bool"
          | "fn" -> Token.Fn
          | "return" -> Token.Return
          | "if" -> Token.If
          | "else" -> Token.Else
          | "for" -> Token.For
          | "in" -> Token.In
          | "test" -> Token.Test
          | _ -> Token.Ident ident
        in
        loop ({ Token.kind; span } :: tokens)
    | Some c ->
        let span = Span.make file st.line st.col in
        Error (Error.make file span.Span.line span.Span.col
          (Printf.sprintf "Unexpected character: %c" c))
  in
  loop []
