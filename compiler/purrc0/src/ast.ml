type ty =
  | I32
  | I64
  | String
  | Bool
  | Struct of string  (* M7: Struct type with struct name *)
  | Enum of string  (* M8: Enum type with enum name *)

(* M3: Operator types *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

type unop =
  | Not | Neg

type expr =
  | IntLit of int64 * Span.t
  | BoolLit of bool * Span.t
  | StringLit of string * Span.t
  | Ident of string * Span.t
  | BinOp of { left: expr; op: binop; right: expr; span: Span.t }
  | UnOp of { op: unop; operand: expr; span: Span.t }
  | Call of { name: string; args: expr list; span: Span.t }
  (* M7: Struct expressions *)
  | StructLit of { struct_name: string; fields: (string * expr) list; span: Span.t }
  | FieldAccess of { object_: expr; field: string; span: Span.t }
  (* M8: Enum variant creation *)
  | EnumVariant of { enum_name: string; variant_name: string; span: Span.t }

type program = {
  structs: struct_def list;  (* M7: Struct definitions *)
  enums: enum_def list;  (* M8: Enum definitions *)
  actors: actor_def list;
}

and struct_def = {
  name: string;
  fields: struct_field list;
  span: Span.t;
}

and struct_field = {
  name: string;
  ty: ty;
  span: Span.t;
}

(* M8: Enum definition *)
and enum_def = {
  name: string;
  variants: enum_variant list;
  span: Span.t;
}

and enum_variant = {
  name: string;
  span: Span.t;
}

and actor_def = {
  name: string;
  functions: func_def list;
  handlers: handler list;
  span: Span.t;
}

and handler = {
  body: stmt list;
  span: Span.t;
}

and stmt =
  | Print of {
    value: expr;
    span: Span.t;
  }
  | VarDecl of {
    name: string;
    ty: ty option;
    init: expr;
    span: Span.t;
  }
  | Assign of {
    name: string;
    value: expr;
    span: Span.t;
  }
  | Return of {
    value: expr option;
    span: Span.t;
  }
  (* M5: Control flow statements *)
  | If of {
    condition: expr;
    then_body: stmt list;
    else_body: stmt list option;
    span: Span.t;
  }
  | For of {
    var: string;
    start_: expr;
    end_: expr;
    body: stmt list;
    span: Span.t;
  }
  | Test of {
    name: string;
    body: stmt list;
    span: Span.t;
  }

(* M4: Function parameters and definitions *)
type param = {
  name: string;
  ty: ty;
  span: Span.t;
}

type func_def = {
  name: string;
  params: param list;
  return_ty: ty;
  body: stmt list;
  span: Span.t;
}
