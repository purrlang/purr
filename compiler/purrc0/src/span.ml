type t = {
  file: string;
  line: int;
  col: int;
}

let make file line col = { file; line; col }

let pp fmt span =
  Format.fprintf fmt "%s:%d:%d" span.file span.line span.col
