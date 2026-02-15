type ty =
  | I32
  | I64
  | String
  | Bool
  | Void  (* M4: Explicit void return type *)
  | Struct of string  (* M7: Struct type with struct name *)
  | Enum of string  (* M8: Enum type with enum name *)
  (* M9: Generic container types *)
  | Option of ty  (* T? or option<T> *)
  | Result of ty * ty  (* result<T, E> *)
  | List of ty  (* list<T> *)
  | Map of ty * ty  (* map<K, V> *)
  | Fixed of ty * int  (* fixed<T, N> - array of N elements *)
  | Slice of ty  (* slice<T> - dynamic array slice *)
  | Nil  (* nil literal type for options *)
  | Mailbox of string  (* M16: Mailbox type for actor references - stores actor type name *)

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
  (* M9: Container expressions *)
  | NilLit of Span.t  (* nil literal *)
  | SomeLit of { value: expr; span: Span.t }  (* Some(expr) *)
  | NoneLit of Span.t  (* None *)
  | OkLit of { value: expr; span: Span.t }  (* Ok(expr) *)
  | ErrLit of { error: expr; span: Span.t }  (* Err(expr) *)
  | ListLit of { elements: expr list; span: Span.t }  (* [elem1, elem2, ...] *)
  | IndexAccess of { object_: expr; index: expr; span: Span.t }  (* obj[index] *)
  (* M16: Actor operations *)
  | Spawn of { actor_type: string; fields: (string * expr) list; span: Span.t }  (* spawn ActorType { field: value, ... } *)

type program = {
  namespace_name: string;  (* M12: Namespace name (e.g., "xyzcorp.compiler") *)
  uses: use_decl list;  (* M12: Use declarations *)
  structs: struct_def list;  (* M7: Struct definitions *)
  enums: enum_def list;  (* M8: Enum definitions *)
  messages: message_def list;  (* M14: Message definitions *)
  benches: bench_def list;  (* M10.5: Benchmark definitions *)
  extern_funcs: extern_def list;  (* M11: Extern C function declarations *)
  actors: actor_def list;
  toplevel_funcs: func_def list;  (* M4: Top-level function declarations *)
  toplevel_tests: test_def list;  (* M5: Top-level test declarations *)
}

(* M12: Use declaration *)
and use_decl = {
  namespace: string;  (* Fully-qualified namespace name *)
  alias: string;  (* Alias for the namespace (defaults to final segment) *)
  span: Span.t;
}

(* M5: Top-level test declaration *)
and test_def = {
  test_name: string;
  test_body: stmt list;
  test_span: Span.t;
}

(* M10.5: Benchmark definition *)
and bench_def = {
  name: string;
  iterations: int;  (* Number of iterations *)
  setup_body: stmt list;  (* Setup code runs once *)
  run_body: stmt list;  (* Run code executes in each iteration *)
  span: Span.t;
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

(* M14: Message definition *)
and message_def = {
  name: string;
  fields: struct_field list;  (* Reuse struct field structure *)
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
  state_fields: struct_field list;  (* M15: State fields *)
  functions: func_def list;
  handlers: handler list;
  span: Span.t;
}

and handler = {
  message_type: string;  (* M15: Type of message this handler processes (e.g., "start", "Ping") *)
  params: param list;  (* M15: Parameters from message fields *)
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
  (* M8: Switch expression matching on enum variants *)
  | Switch of {
    subject: expr;
    cases: (string * stmt list) list;  (* variant_name * body *)
    else_body: stmt list option;
    span: Span.t;
  }
  (* M16: Send message to actor *)
  | Send of {
    target: expr;  (* Expression that evaluates to a mailbox *)
    message_type: string;  (* Type of message to send *)
    fields: (string * expr) list;  (* Message field values *)
    span: Span.t;
  }

(* M4: Function parameters and definitions *)
and param = {
  name: string;
  ty: ty;
  span: Span.t;
}

and func_def = {
  name: string;
  params: param list;
  return_ty: ty;
  body: stmt list;
  span: Span.t;
}

(* M11: Extern C function declarations *)
and extern_def = {
  name: string;
  params: param list;
  return_ty: ty;
  span: Span.t;
}
