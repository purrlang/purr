let escapeString s =
  let buf = Buffer.create (String.length s + 10) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\r' -> Buffer.add_string buf "\\r"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let typeToC (ty: Ast.ty) : string =
  match ty with
  | Ast.I32 -> "int32_t"
  | Ast.I64 -> "int64_t"
  | Ast.String -> "const char*"
  | Ast.Bool -> "_Bool"

let valueToC (v: Ir.value) : string =
  match v with
  | Ir.IntVal n -> Printf.sprintf "%LdLL" n
  | Ir.BoolVal b -> if b then "1" else "0"
  | Ir.StringVal s -> Printf.sprintf "\"%s\"" (escapeString s)
  | Ir.VarRef name -> name

let binopToC (op: Ast.binop) : string =
  match op with
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Mod -> "%"
  | Ast.Eq -> "=="
  | Ast.Neq -> "!="
  | Ast.Lt -> "<"
  | Ast.Lte -> "<="
  | Ast.Gt -> ">"
  | Ast.Gte -> ">="
  | Ast.And -> "&&"
  | Ast.Or -> "||"

let unopToC (op: Ast.unop) : string =
  match op with
  | Ast.Not -> "!"
  | Ast.Neg -> "-"

let generateC ir =
  let buf = Buffer.create 1024 in
  
  (* Header *)
  Buffer.add_string buf "#include \"purr_runtime.h\"\n";
  Buffer.add_string buf "#include <stdint.h>\n";
  Buffer.add_string buf "#include <stdbool.h>\n\n";
  
  (* Generate functions *)
  List.iter (fun (func: Ir.func) ->
    Buffer.add_string buf (Printf.sprintf "void %s(void) {\n" func.name);
    
    (* First pass: collect and emit variable declarations *)
    List.iter (fun instr ->
      match instr with
      | Ir.DeclareVar { name; ty } ->
          Buffer.add_string buf (Printf.sprintf "    %s %s;\n" (typeToC ty) name)
      | _ -> ()
    ) func.body;
    
    let has_vars = List.exists (fun instr ->
      match instr with
      | Ir.DeclareVar _ -> true
      | _ -> false
    ) func.body in
    if has_vars then Buffer.add_string buf "\n";
    
    (* Second pass: emit statements *)
    List.iter (fun instr ->
      match instr with
      | Ir.DeclareVar _ ->
          ()  (* Already handled above *)
      | Ir.Assign { name; value } ->
          let v_str = valueToC value in
          Buffer.add_string buf (Printf.sprintf "    %s = %s;\n" name v_str)
      | Ir.BinOp { result; op; left; right; result_ty } ->
          let left_str = valueToC left in
          let right_str = valueToC right in
          let op_str = binopToC op in
          Buffer.add_string buf (Printf.sprintf "    %s = %s %s %s;\n" result left_str op_str right_str)
      | Ir.UnOp { result; op; operand; result_ty } ->
          let op_str = unopToC op in
          let operand_str = valueToC operand in
          Buffer.add_string buf (Printf.sprintf "    %s = %s%s;\n" result op_str operand_str)
      | Ir.Call { result; func_name; args; result_ty } ->
          (* M4: Function call *)
          let args_str = String.concat ", " (List.map valueToC args) in
          Buffer.add_string buf (Printf.sprintf "    %s = %s(%s);\n" result func_name args_str)
      | Ir.CallPrint v ->
          (match v with
           | Ir.StringVal s ->
               Buffer.add_string buf (Printf.sprintf "    print_string(\"%s\");\n" (escapeString s))
           | Ir.IntVal n ->
               Buffer.add_string buf (Printf.sprintf "    print_i64(%LdLL);\n" n)
           | Ir.BoolVal b ->
               let b_val = if b then "1" else "0" in
               Buffer.add_string buf (Printf.sprintf "    print_bool((_Bool)%s);\n" b_val)
           | Ir.VarRef name ->
               (match Hashtbl.find_opt func.var_types name with
                | Some Ast.I32 -> Buffer.add_string buf (Printf.sprintf "    print_i32(%s);\n" name)
                | Some Ast.I64 -> Buffer.add_string buf (Printf.sprintf "    print_i64(%s);\n" name)
                | Some Ast.String -> Buffer.add_string buf (Printf.sprintf "    print_string(%s);\n" name)
                | Some Ast.Bool -> Buffer.add_string buf (Printf.sprintf "    print_bool(%s);\n" name)
                | None -> Buffer.add_string buf (Printf.sprintf "    print_i64((int64_t)%s); /* unknown type */\n" name)))
      | Ir.Return ret_val ->
          (* M4: Return statement *)
          (match ret_val with
           | None -> Buffer.add_string buf "    return;\n"
           | Some v ->
               let v_str = valueToC v in
               Buffer.add_string buf (Printf.sprintf "    return %s;\n" v_str))
      (* M5: Control flow instructions *)
      | Ir.IfJump { condition; true_label; false_label } ->
          let cond_str = valueToC condition in
          Buffer.add_string buf (Printf.sprintf "    if (%s) goto %s; else goto %s;\n" cond_str true_label false_label)
      | Ir.Goto label ->
          Buffer.add_string buf (Printf.sprintf "    goto %s;\n" label)
      | Ir.Label label ->
          Buffer.add_string buf (Printf.sprintf "%s:\n" label)
    ) func.body;
    
    Buffer.add_string buf "}\n\n"
  ) ir.Ir.functions;
  
  (* main function *)
  Buffer.add_string buf "int main(void) {\n";
  Buffer.add_string buf "    runtimeInit();\n";
  Buffer.add_string buf (Printf.sprintf "    %s();\n" ir.Ir.entry);
  Buffer.add_string buf "    return 0;\n";
  Buffer.add_string buf "}\n";
  
  Buffer.contents buf
