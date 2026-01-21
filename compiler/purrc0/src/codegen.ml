open Ir

exception CodegenError of string

let c_type = function
  | "i32" -> "int"
  | other -> raise (CodegenError ("unsupported type: " ^ other))

let emit_expr out = function
  | IRString s -> Printf.fprintf out "\"%s\"" (String.escaped s)
  | IRInt i -> Printf.fprintf out "%d" i
  | IRCall (path, args) ->
      (match path with
      | [ "core"; "Print" ] ->
          (match args with
          | [ IRString s ] -> Printf.fprintf out "puts(\"%s\")" (String.escaped s)
          | _ -> raise (CodegenError "core.Print expects a single string literal"))
      | _ ->
          raise
            (CodegenError
               "only core.Print calls are supported in this bootstrap") )

let emit_stmt out = function
  | IRExpr expr ->
      emit_expr out expr;
      Printf.fprintf out ";\n"
  | IRReturn expr ->
      Printf.fprintf out "return ";
      emit_expr out expr;
      Printf.fprintf out ";\n"

let emit_function out f =
  Printf.fprintf out "%s %s() {\n" (c_type f.return_type) f.name;
  List.iter (emit_stmt out) f.body;
  Printf.fprintf out "}\n\n"

let emit_program out program =
  Printf.fprintf out "#include <stdio.h>\n\n";
  List.iter (emit_function out) program.functions
