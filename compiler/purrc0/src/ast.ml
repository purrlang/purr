type span = {
  start_pos : int;
  end_pos : int;
}

type ident = string

type path = ident list

type literal =
  | String of string
  | Int of int

type expr =
  | Literal of literal * span
  | Call of path * expr list * span

type stmt =
  | Expr of expr
  | Return of expr * span

type func = {
  name : ident;
  return_type : ident;
  body : stmt list;
  span : span;
}

type item =
  | Namespace of path * span
  | Use of path * span
  | Func of func

type program = {
  items : item list;
  span : span;
}

let span_union a b = { start_pos = a.start_pos; end_pos = b.end_pos }
