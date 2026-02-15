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

let rec typeToC (ty: Ast.ty) : string =
  match ty with
  | Ast.I32 -> "int32_t"
  | Ast.I64 -> "int64_t"
  | Ast.String -> "const char*"
  | Ast.Bool -> "_Bool"
  | Ast.Void -> "void"  (* M4: Void return type *)
  | Ast.Struct name -> Printf.sprintf "struct %s" name (* M7: Struct types *)
  | Ast.Enum name -> "int"  (* M8: Enum types represented as int *)
  (* M9: Container types - proper C runtime types *)
  | Ast.Option _ -> "void*"  (* Option<T> as opaque pointer *)
  | Ast.Result _ -> "void*"  (* Result<T,E> as opaque pointer *)
  | Ast.List _ -> "PurrList*"  (* list<T> as pointer to PurrList *)
  | Ast.Map _ -> "PurrMap*"  (* map<K,V> as pointer to PurrMap *)
  | Ast.Fixed (elem_ty, size) ->
      Printf.sprintf "%s[%d]" (typeToC elem_ty) size  (* fixed<T,N> as C array *)
  | Ast.Slice _ -> "PurrSlice*"  (* slice<T> as pointer to PurrSlice *)
  | Ast.Nil -> "void"  (* Nil doesn't really have a C representation *)

and valueToC (v: Ir.value) : string =
  match v with
  | Ir.IntVal n -> Printf.sprintf "%LdLL" n
  | Ir.BoolVal b -> if b then "1" else "0"
  | Ir.StringVal s -> Printf.sprintf "\"%s\"" (escapeString s)
  | Ir.VarRef name -> name
  | Ir.StructVal (struct_name, fields) ->
      (* M7: Generate struct initializer with field values *)
      let field_strs = List.map (fun (fname, fval) ->
        Printf.sprintf ".%s = %s" fname (valueToC fval)
      ) fields in
      Printf.sprintf "{%s}" (String.concat ", " field_strs)
  | Ir.EnumVal (enum_name, variant_name) ->
      (* M8: Enum values represented as enum constants *)
      Printf.sprintf "%s_%s" enum_name variant_name

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
  
  (* M7: Generate struct definitions *)
  List.iter (fun (sdef: Ast.struct_def) ->
    Buffer.add_string buf (Printf.sprintf "struct %s {\n" sdef.name);
    List.iter (fun (field: Ast.struct_field) ->
      Buffer.add_string buf (Printf.sprintf "    %s %s;\n" (typeToC field.ty) field.name)
    ) sdef.fields;
    Buffer.add_string buf "};\n\n"
  ) ir.Ir.structs;

  (* M8: Generate enum definitions *)
  List.iter (fun (edef: Ast.enum_def) ->
    Buffer.add_string buf (Printf.sprintf "/* enum %s */\n" edef.name);
    let variant_counter = ref 0 in
    List.iter (fun (variant: Ast.enum_variant) ->
      Buffer.add_string buf (Printf.sprintf "#define %s_%s %d\n" edef.name variant.name !variant_counter);
      incr variant_counter
    ) edef.variants;
    Buffer.add_string buf "\n"
  ) ir.Ir.enums;

  (* M14: Generate message definitions (as structs for now) *)
  List.iter (fun (mdef: Ast.message_def) ->
    Buffer.add_string buf (Printf.sprintf "struct %s {\n" mdef.name);
    List.iter (fun (field: Ast.struct_field) ->
      Buffer.add_string buf (Printf.sprintf "    %s %s;\n" (typeToC field.ty) field.name)
    ) mdef.fields;
    Buffer.add_string buf "};\n\n"
  ) ir.Ir.messages;

  (* Generate function forward declarations *)
  List.iter (fun (func: Ir.func) ->
    let return_type = typeToC func.return_ty in
    let params_str = match func.params with
      | [] -> "void"
      | params ->
          String.concat ", " (List.map (fun (name, ty) ->
            Printf.sprintf "%s %s" (typeToC ty) name
          ) params)
    in
    Buffer.add_string buf (Printf.sprintf "%s %s(%s);\n" return_type func.name params_str)
  ) ir.Ir.functions;
  Buffer.add_string buf "\n";

  (* Generate function implementations *)
  List.iter (fun (func: Ir.func) ->
    let return_type = typeToC func.return_ty in
    let params_str = match func.params with
      | [] -> "void"
      | params ->
          String.concat ", " (List.map (fun (name, ty) ->
            Printf.sprintf "%s %s" (typeToC ty) name
          ) params)
    in
    Buffer.add_string buf (Printf.sprintf "%s %s(%s) {\n" return_type func.name params_str);
    
    (* First pass: collect and emit variable declarations — skip void temps *)
    List.iter (fun instr ->
      match instr with
      | Ir.DeclareVar { name; ty } when ty <> Ast.Void && ty <> Ast.Nil ->
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
          (* M4: Function call — remap Purr names to C names where needed *)
          let c_func_name = match func_name with
            | "mapSet" -> "map_set_str"
            | "mapGet" -> "map_get_str"
            | "mapHas" -> "map_has_str"
            | n -> n
          in
          let args_str = String.concat ", " (List.map valueToC args) in
          (* Void-returning functions cannot be assigned to a variable in C *)
          if result_ty = Ast.Void then
            Buffer.add_string buf (Printf.sprintf "    %s(%s);\n" c_func_name args_str)
          else
            Buffer.add_string buf (Printf.sprintf "    %s = %s(%s);\n" result c_func_name args_str)
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
                | Some (Ast.Enum _) -> Buffer.add_string buf (Printf.sprintf "    print_i64((int64_t)%s); /* enum value */\n" name)
                | None -> Buffer.add_string buf (Printf.sprintf "    print_i64((int64_t)%s); /* unknown type */\n" name)))
      | Ir.Return ret_val ->
          (* M4: Return statement *)
          (match ret_val with
           | None -> Buffer.add_string buf "    return;\n"
           | Some v ->
               let v_str = valueToC v in
               Buffer.add_string buf (Printf.sprintf "    return %s;\n" v_str))
      (* M5: Control flow instructions *)
      | Ir.JumpIfFalse { condition; label } ->
          let cond_str = valueToC condition in
          Buffer.add_string buf (Printf.sprintf "    if (!(%s)) goto %s;\n" cond_str label)
      | Ir.Jump label ->
          Buffer.add_string buf (Printf.sprintf "    goto %s;\n" label)
      | Ir.Label label ->
          Buffer.add_string buf (Printf.sprintf "%s:\n" label)
      (* M7: Field assignment *)
      | Ir.FieldAssign { object_name; field; value; field_ty } ->
          let v_str = valueToC value in
          Buffer.add_string buf (Printf.sprintf "    %s.%s = %s;\n" object_name field v_str)
    ) func.body;
    
    Buffer.add_string buf "}\n\n"
  ) ir.Ir.functions;

  (* M10.5: Generate benchmark functions *)
  List.iter (fun (bench: Ast.bench_def) ->
    let func_name = Printf.sprintf "bench_%s" bench.name in
    Buffer.add_string buf (Printf.sprintf "void %s(void) {\n" func_name);
    Buffer.add_string buf (Printf.sprintf "    /* Benchmark: %s (iterations: %d) */\n" bench.name bench.iterations);

    (* TODO: Generate setup_body statements *)
    (* For now, we'll generate placeholder comments *)
    Buffer.add_string buf (Printf.sprintf "    int64_t __iterations = %dLL;\n" bench.iterations);
    Buffer.add_string buf "    for (int64_t __i = 0; __i < __iterations; __i++) {\n";
    Buffer.add_string buf "        /* run_body code would go here */\n";
    Buffer.add_string buf "    }\n";
    Buffer.add_string buf "}\n\n"
  ) ir.Ir.benches;

  (* M5: If tests exist, generate forward declarations and a run_tests() function *)
  let has_tests = ir.Ir.toplevel_tests <> [] in
  if has_tests then begin
    List.iter (fun (tdef: Ast.test_def) ->
      let safe_name = String.concat "_" (String.split_on_char ' ' tdef.Ast.test_name) in
      let func_id = Printf.sprintf "__test__%s" safe_name in
      Buffer.add_string buf (Printf.sprintf "void %s(void);\n" func_id)
    ) ir.Ir.toplevel_tests;
    Buffer.add_string buf "\nvoid run_tests(void) {\n";
    List.iter (fun (tdef: Ast.test_def) ->
      let safe_name = String.concat "_" (String.split_on_char ' ' tdef.Ast.test_name) in
      let func_id = Printf.sprintf "__test__%s" safe_name in
      Buffer.add_string buf (Printf.sprintf "    %s();\n" func_id)
    ) ir.Ir.toplevel_tests;
    Buffer.add_string buf "}\n\n"
  end;

  (* main function *)
  Buffer.add_string buf "int main(void) {\n";
  Buffer.add_string buf "    runtimeInit();\n";
  Buffer.add_string buf (Printf.sprintf "    %s();\n" ir.Ir.entry);
  if has_tests then
    Buffer.add_string buf "    run_tests();\n";
  Buffer.add_string buf "    return 0;\n";
  Buffer.add_string buf "}\n";

  Buffer.contents buf
