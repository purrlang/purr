open Ast

type ir_expr =
  | IRString of string
  | IRInt of int
  | IRCall of string list * ir_expr list

type ir_stmt =
  | IRExpr of ir_expr
  | IRReturn of ir_expr

type ir_func = {
  name : string;
  return_type : string;
  body : ir_stmt list;
}

type ir_program = {
  functions : ir_func list;
}

let rec lower_expr = function
  | Literal (String s, _) -> IRString s
  | Literal (Int i, _) -> IRInt i
  | Call (path, args, _) -> IRCall (path, List.map lower_expr args)

let lower_stmt = function
  | Expr e -> IRExpr (lower_expr e)
  | Return (e, _) -> IRReturn (lower_expr e)

let lower program =
  let functions =
    List.filter_map
      (function
        | Func f ->
            Some
              {
                name = f.name;
                return_type = f.return_type;
                body = List.map lower_stmt f.body;
              }
        | _ -> None)
      program.items
  in
  { functions }
