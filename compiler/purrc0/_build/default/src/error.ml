type t = {
  file: string;
  line: int;
  col: int;
  message: string;
}

let make file line col message = { file; line; col; message }

let fromSpan span message = {
  file = span.Span.file;
  line = span.Span.line;
  col = span.Span.col;
  message;
}

let format e =
  Printf.sprintf "error: %s:%d:%d: %s" e.file e.line e.col e.message

let pp fmt e =
  Format.fprintf fmt "%s" (format e)
