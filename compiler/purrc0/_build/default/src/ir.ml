type func_id = string

type value =
  | IntVal of int64
  | BoolVal of bool
  | StringVal of string
  | VarRef of string
  | StructVal of string * (string * value) list (* M7: struct_name and field values *)
  | EnumVal of string * string  (* M8: enum_name and variant_name *)

type instr =
  | DeclareVar of { name: string; ty: Ast.ty }
  | Assign of { name: string; value: value }
  | BinOp of { result: string; op: Ast.binop; left: value; right: value; result_ty: Ast.ty }
  | UnOp of { result: string; op: Ast.unop; operand: value; result_ty: Ast.ty }
  | Call of { result: string; func_name: string; args: value list; result_ty: Ast.ty }  (* M4: Function calls *)
  | CallPrint of value
  | Return of value option  (* M4: Return with optional value *)
  (* M5: Control flow instructions *)
  | JumpIfFalse of { condition: value; label: string }  (* Jump if condition is false *)
  | Jump of string
  | Label of string
  (* M7: Struct field assignment *)
  | FieldAssign of { object_name: string; field: string; value: value; field_ty: Ast.ty }

type func = {
  id: func_id;
  name: string;
  params: (string * Ast.ty) list;  (* M4: Function parameters *)
  return_ty: Ast.ty;  (* M4: Return type *)
  body: instr list;
  var_types: (string, Ast.ty) Hashtbl.t;  (* Track variable types for codegen *)
}

type program_ir = {
  structs: Ast.struct_def list;  (* M7: Struct definitions *)
  enums: Ast.enum_def list;  (* M8: Enum definitions *)
  messages: Ast.message_def list;  (* M14: Message definitions *)
  benches: Ast.bench_def list;  (* M10.5: Benchmark definitions *)
  entry: func_id;
  functions: func list;
}

let exprToValue (expr: Ast.expr) : value =
  match expr with
  | Ast.IntLit (n, _) -> IntVal n
  | Ast.BoolLit (b, _) -> BoolVal b
  | Ast.StringLit (s, _) -> StringVal s
  | Ast.Ident (name, _) -> VarRef name
  | Ast.EnumVariant { enum_name; variant_name; _ } ->
      EnumVal (enum_name, variant_name)  (* M8: Enum variant *)
  | Ast.Call _ ->
      failwith "Function calls must be lowered with generateExprInstructions"
  | Ast.StructLit _ ->
      failwith "Struct literals must be lowered with generateExprInstructions"
  | Ast.FieldAccess _ ->
      failwith "Field access must be lowered with generateExprInstructions"
  | _ ->
      failwith "Complex expressions must be lowered with generateExprInstructions"

(* Lower an expression into instructions and return the resulting value *)
let rec generateExprInstructions (expr: Ast.expr) (var_types: (string, Ast.ty) Hashtbl.t) (temp_counter: int ref) (enums: Ast.enum_def list) : instr list * value * int ref =
  match expr with
  | Ast.IntLit (_, _) | Ast.BoolLit (_, _) | Ast.StringLit (_, _) ->
      ([], exprToValue expr, temp_counter)
  | Ast.Ident (name, _) ->
      (* M8: Check if this is an enum variant *)
      let enum_opt = List.find_opt (fun (edef: Ast.enum_def) ->
        List.exists (fun (v: Ast.enum_variant) -> v.name = name) edef.variants
      ) enums in
      (match enum_opt with
       | Some edef ->
           (* This is an enum variant *)
           ([], EnumVal (edef.name, name), temp_counter)
       | None ->
           (* This is a regular variable *)
           ([], exprToValue expr, temp_counter))
  | Ast.BinOp { left; op; right; span } ->
      let (left_instrs, left_val, tc1) = generateExprInstructions left var_types temp_counter enums in
      let (right_instrs, right_val, tc2) = generateExprInstructions right var_types tc1 enums in
      let temp_name = Printf.sprintf "__temp_%d" !tc2 in
      incr tc2;
      let result_ty = match Sema.inferExprType expr with
        | Some t -> t
        | None -> failwith "Cannot infer type for binary operation"
      in
      Hashtbl.replace var_types temp_name result_ty;
      let binop_instr = BinOp { result = temp_name; op; left = left_val; right = right_val; result_ty } in
      (left_instrs @ right_instrs @ [binop_instr], VarRef temp_name, tc2)
  | Ast.UnOp { op; operand; span } ->
      let (operand_instrs, operand_val, tc1) = generateExprInstructions operand var_types temp_counter enums in
      let temp_name = Printf.sprintf "__temp_%d" !tc1 in
      incr tc1;
      let result_ty = match Sema.inferExprType expr with
        | Some t -> t
        | None -> failwith "Cannot infer type for unary operation"
      in
      Hashtbl.replace var_types temp_name result_ty;
      let unop_instr = UnOp { result = temp_name; op; operand = operand_val; result_ty } in
      (operand_instrs @ [unop_instr], VarRef temp_name, tc1)
  | Ast.Call { name; args; span } ->
      (* M4: Function call - evaluate all arguments first *)
      let rec eval_args arg_list acc_instrs acc_vals tc =
        match arg_list with
        | [] -> (List.rev acc_instrs, List.rev acc_vals, tc)
        | arg :: rest ->
            let (arg_instrs, arg_val, tc') = generateExprInstructions arg var_types (ref !tc) enums in
            tc := !tc';
            eval_args rest (acc_instrs @ arg_instrs) (arg_val :: acc_vals) tc'
      in
      let (arg_instrs, arg_vals, tc2) = eval_args args [] [] temp_counter in
      let temp_name = Printf.sprintf "__temp_%d" !tc2 in
      incr tc2;
      let result_ty = match Sema.inferExprType expr with
        | Some t -> t
        | None -> failwith "Cannot infer type for function call"
      in
      Hashtbl.replace var_types temp_name result_ty;
      let call_instr = Call { result = temp_name; func_name = name; args = arg_vals; result_ty } in
      (arg_instrs @ [call_instr], VarRef temp_name, tc2)
  | Ast.StructLit { struct_name; fields; span } ->
      (* M7: Struct literal - evaluate all field values *)
      let rec eval_fields field_list acc_instrs acc_vals tc =
        match field_list with
        | [] -> (List.rev acc_instrs, List.rev acc_vals, tc)
        | (fname, fexpr) :: rest ->
            let (finstrs, fval, tc') = generateExprInstructions fexpr var_types (ref !tc) enums in
            tc := !tc';
            eval_fields rest (acc_instrs @ finstrs) ((fname, fval) :: acc_vals) tc'
      in
      let (field_instrs, field_vals, tc2) = eval_fields fields [] [] temp_counter in
      let temp_name = Printf.sprintf "__temp_%d" !tc2 in
      incr tc2;
      let result_ty = Ast.Struct struct_name in
      Hashtbl.replace var_types temp_name result_ty;
      let struct_val = StructVal (struct_name, field_vals) in
      (field_instrs @ [Assign { name = temp_name; value = struct_val }], VarRef temp_name, tc2)
  | Ast.EnumVariant { enum_name; variant_name; _ } ->
      (* M8: Enum variant - just create the enum value directly *)
      let temp_name = Printf.sprintf "__temp_%d" !temp_counter in
      incr temp_counter;
      let result_ty = Ast.Enum enum_name in
      Hashtbl.replace var_types temp_name result_ty;
      let enum_val = EnumVal (enum_name, variant_name) in
      ([Assign { name = temp_name; value = enum_val }], VarRef temp_name, temp_counter)
  | Ast.FieldAccess { object_; field; span } ->
      (* M7: Field access - evaluate object and extract field *)
      (match object_ with
       | Ast.Ident (obj_name, _) ->
           (* Simple field access on a variable *)
           let temp_name = Printf.sprintf "__temp_%d" !temp_counter in
           incr temp_counter;
           (* We'll track this at codegen time *)
           ([], VarRef obj_name, temp_counter)  (* Simplified: just return reference to object *)
       | _ ->
           (* For complex expressions, evaluate first *)
           let (obj_instrs, obj_val, tc') = generateExprInstructions object_ var_types temp_counter enums in
           temp_counter := !tc';
           (obj_instrs, obj_val, temp_counter))  (* Simplified: return object value *)
  | Ast.Spawn { actor_type; fields; span } ->
      (* M16: Spawn expression - evaluate all field values *)
      let rec eval_fields field_list acc_instrs acc_vals tc =
        match field_list with
        | [] -> (List.rev acc_instrs, List.rev acc_vals, tc)
        | (fname, fexpr) :: rest ->
            let (finstrs, fval, tc') = generateExprInstructions fexpr var_types (ref !tc) enums in
            tc := !tc';
            eval_fields rest (acc_instrs @ finstrs) ((fname, fval) :: acc_vals) tc'
      in
      let (field_instrs, field_vals, tc2) = eval_fields fields [] [] temp_counter in
      let temp_name = Printf.sprintf "__temp_%d" !tc2 in
      incr tc2;
      let result_ty = Ast.Mailbox actor_type in
      Hashtbl.replace var_types temp_name result_ty;
      let spawn_val = StructVal (Printf.sprintf "__spawn_%s" actor_type, field_vals) in
      (field_instrs @ [Assign { name = temp_name; value = spawn_val }], VarRef temp_name, tc2)

let lower program =
  (* Generate IR for all functions and handlers *)
  let functions = ref [] in

  List.iter (fun (actor: Ast.actor_def) ->
    (* Process function definitions *)
    List.iter (fun (func: Ast.func_def) ->
      let func_id = Printf.sprintf "%s_%s" actor.name func.name in
      let body_instrs = ref [] in
      let var_types = Hashtbl.create 16 in
      let temp_counter = ref 0 in
      
      (* Add parameters to var_types *)
      List.iter (fun (param: Ast.param) ->
        Hashtbl.add var_types param.name param.ty
      ) func.params;
      
      (* Process all statements *)
      List.iter (fun (stmt: Ast.stmt) ->
        match stmt with
        | Ast.Print { value; _ } ->
            let needs_temp = match value with
               | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
               | _ -> false
            in
            if needs_temp then
              let (exprs_instrs, final_val, tc') = generateExprInstructions value var_types temp_counter program.Ast.enums in
              temp_counter := !tc';
              body_instrs := !body_instrs @ exprs_instrs @ [CallPrint final_val]
            else
              let v = exprToValue value in
              body_instrs := !body_instrs @ [CallPrint v]
        |  Ast.VarDecl { name; ty; init; _ } ->
            let resolved_ty = match ty with
              | Some t -> t
              | None ->
                  match Sema.inferExprType init with
                  | Some t -> t
                  | None -> failwith (Printf.sprintf "Cannot infer type for %s" name)
            in
            Hashtbl.add var_types name resolved_ty;
            let needs_temp = match init with
               | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
               | _ -> false
            in
            if needs_temp then
              let (exprs_instrs, init_val, tc') = generateExprInstructions init var_types temp_counter program.Ast.enums in
              temp_counter := !tc';
              body_instrs := !body_instrs @ [DeclareVar { name; ty = resolved_ty }] @ exprs_instrs @ [Assign { name; value = init_val }]
            else
              let v = exprToValue init in
              body_instrs := !body_instrs @ [
                DeclareVar { name; ty = resolved_ty };
                Assign { name; value = v };
              ]
        | Ast.Assign { name; value; _ } ->
            let needs_temp = match value with
               | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
               | _ -> false
            in
            if needs_temp then
              let (exprs_instrs, final_val, tc') = generateExprInstructions value var_types temp_counter program.Ast.enums in
              temp_counter := !tc';
              body_instrs := !body_instrs @ exprs_instrs @ [Assign { name; value = final_val }]
            else
              let v = exprToValue value in
              body_instrs := !body_instrs @ [Assign { name; value = v }]
        | Ast.Return { value; _ } ->
            (match value with
             | None ->
                 body_instrs := !body_instrs @ [Return None]
             | Some expr ->
                 let needs_temp = match expr with
                    | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                    | _ -> false
                 in
                 if needs_temp then
                   let (exprs_instrs, ret_val, tc') = generateExprInstructions expr var_types temp_counter program.Ast.enums in
                   temp_counter := !tc';
                   body_instrs := !body_instrs @ exprs_instrs @ [Return (Some ret_val)]
                 else
                   let v = exprToValue expr in
                   body_instrs := !body_instrs @ [Return (Some v)])
        | Ast.If { condition; then_body; else_body; _ } ->
            (* M5: If/else lowering *)
            let (cond_instrs, cond_val, tc') = generateExprInstructions condition var_types temp_counter program.Ast.enums in
            temp_counter := !tc';
            let else_label = Printf.sprintf "__else_%d" !temp_counter in
            incr temp_counter;
            let end_label = Printf.sprintf "__end_if_%d" !temp_counter in
            incr temp_counter;
            body_instrs := !body_instrs @ cond_instrs @ [JumpIfFalse { condition = cond_val; label = else_label }];
            (* Process then body *)
            let then_instrs = ref [] in
            List.iter (fun (stmt: Ast.stmt) ->
              match stmt with
              | Ast.Print { value; _ } ->
                  let needs_temp = match value with
                     | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                     | _ -> false
                  in
                  if needs_temp then
                    let (exprs_instrs, final_val, tc'') = generateExprInstructions value var_types temp_counter program.Ast.enums in
                    temp_counter := !tc'';
                    then_instrs := !then_instrs @ exprs_instrs @ [CallPrint final_val]
                  else
                    let v = exprToValue value in
                    then_instrs := !then_instrs @ [CallPrint v]
              | Ast.VarDecl { name; ty; init; _ } ->
                  let resolved_ty = match ty with
                    | Some t -> t
                    | None ->
                        match Sema.inferExprType init with
                        | Some t -> t
                        | None -> failwith (Printf.sprintf "Cannot infer type for %s" name)
                  in
                  Hashtbl.add var_types name resolved_ty;
                  let needs_temp = match init with
                     | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                     | _ -> false
                  in
                  if needs_temp then
                    let (exprs_instrs, init_val, tc'') = generateExprInstructions init var_types temp_counter program.Ast.enums in
                    temp_counter := !tc'';
                    then_instrs := !then_instrs @ [DeclareVar { name; ty = resolved_ty }] @ exprs_instrs @ [Assign { name; value = init_val }]
                  else
                    let v = exprToValue init in
                    then_instrs := !then_instrs @ [
                      DeclareVar { name; ty = resolved_ty };
                      Assign { name; value = v };
                    ]
              | Ast.Assign { name; value; _ } ->
                  let needs_temp = match value with
                     | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                     | _ -> false
                  in
                  if needs_temp then
                    let (exprs_instrs, final_val, tc'') = generateExprInstructions value var_types temp_counter program.Ast.enums in
                    temp_counter := !tc'';
                    then_instrs := !then_instrs @ exprs_instrs @ [Assign { name; value = final_val }]
                  else
                    let v = exprToValue value in
                    then_instrs := !then_instrs @ [Assign { name; value = v }]
              | Ast.Return { value; _ } ->
                  (match value with
                   | None ->
                       then_instrs := !then_instrs @ [Return None]
                   | Some expr ->
                       let needs_temp = match expr with
                          | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                          | _ -> false
                       in
                       if needs_temp then
                         let (exprs_instrs, ret_val, tc'') = generateExprInstructions expr var_types temp_counter program.Ast.enums in
                         temp_counter := !tc'';
                         then_instrs := !then_instrs @ exprs_instrs @ [Return (Some ret_val)]
                       else
                         let v = exprToValue expr in
                         then_instrs := !then_instrs @ [Return (Some v)])
              | _ -> ()  (* Other statements not yet handled in if body *)
            ) then_body;
            body_instrs := !body_instrs @ !then_instrs @ [Jump end_label; Label else_label];
            (* Process else body *)
            (match else_body with
             | Some else_stmts ->
                 let else_instrs = ref [] in
                 List.iter (fun (stmt: Ast.stmt) ->
                   match stmt with
                   | Ast.Print { value; _ } ->
                       let needs_temp = match value with
                          | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                          | _ -> false
                       in
                       if needs_temp then
                         let (exprs_instrs, final_val, tc'') = generateExprInstructions value var_types temp_counter program.Ast.enums in
                         temp_counter := !tc'';
                         else_instrs := !else_instrs @ exprs_instrs @ [CallPrint final_val]
                       else
                         let v = exprToValue value in
                         else_instrs := !else_instrs @ [CallPrint v]
                   | Ast.VarDecl { name; ty; init; _ } ->
                       let resolved_ty = match ty with
                         | Some t -> t
                         | None ->
                             match Sema.inferExprType init with
                             | Some t -> t
                             | None -> failwith (Printf.sprintf "Cannot infer type for %s" name)
                       in
                       Hashtbl.add var_types name resolved_ty;
                       let needs_temp = match init with
                          | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                          | _ -> false
                       in
                       if needs_temp then
                         let (exprs_instrs, init_val, tc'') = generateExprInstructions init var_types temp_counter program.Ast.enums in
                         temp_counter := !tc'';
                         else_instrs := !else_instrs @ [DeclareVar { name; ty = resolved_ty }] @ exprs_instrs @ [Assign { name; value = init_val }]
                       else
                         let v = exprToValue init in
                         else_instrs := !else_instrs @ [
                           DeclareVar { name; ty = resolved_ty };
                           Assign { name; value = v };
                         ]
                   | Ast.Assign { name; value; _ } ->
                       let needs_temp = match value with
                          | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                          | _ -> false
                       in
                       if needs_temp then
                         let (exprs_instrs, final_val, tc'') = generateExprInstructions value var_types temp_counter program.Ast.enums in
                         temp_counter := !tc'';
                         else_instrs := !else_instrs @ exprs_instrs @ [Assign { name; value = final_val }]
                       else
                         let v = exprToValue value in
                         else_instrs := !else_instrs @ [Assign { name; value = v }]
                   | Ast.Return { value; _ } ->
                       (match value with
                        | None ->
                            else_instrs := !else_instrs @ [Return None]
                        | Some expr ->
                            let needs_temp = match expr with
                               | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                               | _ -> false
                            in
                            if needs_temp then
                              let (exprs_instrs, ret_val, tc'') = generateExprInstructions expr var_types temp_counter program.Ast.enums in
                              temp_counter := !tc'';
                              else_instrs := !else_instrs @ exprs_instrs @ [Return (Some ret_val)]
                            else
                              let v = exprToValue expr in
                              else_instrs := !else_instrs @ [Return (Some v)])
                   | _ -> ()  (* Other statements not yet handled in else body *)
                 ) else_stmts;
                 body_instrs := !body_instrs @ !else_instrs
             | None -> ());
            body_instrs := !body_instrs @ [Label end_label]
        | Ast.For { var; start_; end_; body; _ } ->
            (* M5: For loop lowering *)
            let resolved_ty = match Sema.inferExprType start_ with
              | Some t -> t
              | None -> failwith "Cannot infer loop variable type"
            in
            Hashtbl.add var_types var resolved_ty;
            let (start_instrs, start_val, tc') = generateExprInstructions start_ var_types temp_counter program.Ast.enums in
            temp_counter := !tc';
            let (end_instrs, end_val, tc'') = generateExprInstructions end_ var_types temp_counter program.Ast.enums in
            temp_counter := !tc'';
            let loop_label = Printf.sprintf "__loop_%d" !temp_counter in
            incr temp_counter;
            let end_label = Printf.sprintf "__end_loop_%d" !temp_counter in
            incr temp_counter;
            let inc_temp = Printf.sprintf "__inc_%d" !temp_counter in
            incr temp_counter;
            body_instrs := !body_instrs @ [DeclareVar { name = var; ty = resolved_ty }] @ start_instrs @ [Assign { name = var; value = start_val }] @ [Label loop_label];
            (* Process loop body *)
            let loop_body_instrs = ref [] in
            List.iter (fun (stmt: Ast.stmt) ->
              match stmt with
              | Ast.Print { value; _ } ->
                  let needs_temp = match value with
                     | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                     | _ -> false
                  in
                  if needs_temp then
                    let (exprs_instrs, final_val, tc''') = generateExprInstructions value var_types temp_counter program.Ast.enums in
                    temp_counter := !tc''';
                    loop_body_instrs := !loop_body_instrs @ exprs_instrs @ [CallPrint final_val]
                  else
                    let v = exprToValue value in
                    loop_body_instrs := !loop_body_instrs @ [CallPrint v]
              | Ast.VarDecl { name; ty; init; _ } ->
                  let resolved_ty' = match ty with
                    | Some t -> t
                    | None ->
                        match Sema.inferExprType init with
                        | Some t -> t
                        | None -> failwith (Printf.sprintf "Cannot infer type for %s" name)
                  in
                  Hashtbl.add var_types name resolved_ty';
                  let needs_temp = match init with
                     | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                     | _ -> false
                  in
                  if needs_temp then
                    let (exprs_instrs, init_val, tc''') = generateExprInstructions init var_types temp_counter program.Ast.enums in
                    temp_counter := !tc''';
                    loop_body_instrs := !loop_body_instrs @ [DeclareVar { name; ty = resolved_ty' }] @ exprs_instrs @ [Assign { name; value = init_val }]
                  else
                    let v = exprToValue init in
                    loop_body_instrs := !loop_body_instrs @ [
                      DeclareVar { name; ty = resolved_ty' };
                      Assign { name; value = v };
                    ]
              | Ast.Assign { name; value; _ } ->
                  let needs_temp = match value with
                     | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                     | _ -> false
                  in
                  if needs_temp then
                    let (exprs_instrs, final_val, tc''') = generateExprInstructions value var_types temp_counter program.Ast.enums in
                    temp_counter := !tc''';
                    loop_body_instrs := !loop_body_instrs @ exprs_instrs @ [Assign { name; value = final_val }]
                  else
                    let v = exprToValue value in
                    loop_body_instrs := !loop_body_instrs @ [Assign { name; value = v }]
              | _ -> ()  (* Other statements not yet handled in loop body *)
            ) body;
            (* Generate loop condition check and increment *)
            let cond_temp = Printf.sprintf "__cond_%d" !temp_counter in
            incr temp_counter;
            body_instrs := !body_instrs @ !loop_body_instrs @ end_instrs @ [BinOp {
              result = cond_temp;
              op = Ast.Lt;
              left = VarRef var;
              right = end_val;
              result_ty = Ast.Bool
            };
            JumpIfFalse { condition = VarRef cond_temp; label = end_label };
            BinOp {
              result = inc_temp;
              op = Ast.Add;
              left = VarRef var;
              right = IntVal 1L;
              result_ty = resolved_ty
            };
            Assign { name = var; value = VarRef inc_temp };
            Jump loop_label;
            Label end_label]
        | Ast.Test { name; body; _ } ->
            (* M5: Test declaration - placeholder *)
            ()
        | Ast.Send { target; message_type; fields; _ } ->
            (* M16: Send statement - placeholder for now *)
            ()
      ) func.body;
      
      let func_ir = {
        id = func_id;
        name = func_id;
        params = List.map (fun (p: Ast.param) -> (p.name, p.ty)) func.params;  (* M4: Extract parameters *)
        return_ty = func.return_ty;  (* M4: Extract return type *)
        body = !body_instrs;
        var_types = var_types;
      } in
      functions := func_ir :: !functions
    ) actor.functions;
    
    (* Process handlers *)
    List.iter (fun (handler: Ast.handler) ->
      let func_id = Printf.sprintf "%s_on_start" actor.name in
      let body_instrs = ref [] in
      let var_types = Hashtbl.create 16 in
      let temp_counter = ref 0 in
      
      (* Process all statements, generate instructions *)
      List.iter (fun (stmt: Ast.stmt) ->
        match stmt with
        | Ast.Print { value; _ } ->
            let needs_temp = match value with
               | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
               | _ -> false
            in
            if needs_temp then
              let (exprs_instrs, final_val, tc') = generateExprInstructions value var_types temp_counter program.Ast.enums in
              temp_counter := !tc';
              body_instrs := !body_instrs @ exprs_instrs @ [CallPrint final_val]
            else
              let v = exprToValue value in
              body_instrs := !body_instrs @ [CallPrint v]
        | Ast.VarDecl { name; ty; init; _ } ->
            let resolved_ty = match ty with
              | Some t -> t
              | None ->
                  match Sema.inferExprType init with
                  | Some t -> t
                  | None -> failwith (Printf.sprintf "Cannot infer type for %s" name)
            in
            Hashtbl.add var_types name resolved_ty;
            let needs_temp = match init with
               | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
               | _ -> false
            in
            if needs_temp then
              let (exprs_instrs, init_val, tc') = generateExprInstructions init var_types temp_counter program.Ast.enums in
              temp_counter := !tc';
              body_instrs := !body_instrs @ [DeclareVar { name; ty = resolved_ty }] @ exprs_instrs @ [Assign { name; value = init_val }]
            else
              let v = exprToValue init in
              body_instrs := !body_instrs @ [
                DeclareVar { name; ty = resolved_ty };
                Assign { name; value = v };
              ]
        | Ast.Assign { name; value; _ } ->
            let needs_temp = match value with
               | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
               | _ -> false
            in
            if needs_temp then
              let (exprs_instrs, final_val, tc') = generateExprInstructions value var_types temp_counter program.Ast.enums in
              temp_counter := !tc';
              body_instrs := !body_instrs @ exprs_instrs @ [Assign { name; value = final_val }]
            else
              let v = exprToValue value in
              body_instrs := !body_instrs @ [Assign { name; value = v }]
        | Ast.Return { value; _ } ->
            (* M4: Handle return statements *)
            (match value with
             | None ->
                 body_instrs := !body_instrs @ [Return None]
             | Some expr ->
                 let needs_temp = match expr with
                    | Ast.BinOp _ | Ast.UnOp _ | Ast.Call _ -> true
                    | _ -> false
                 in
                 if needs_temp then
                   let (exprs_instrs, ret_val, tc') = generateExprInstructions expr var_types temp_counter program.Ast.enums in
                   temp_counter := !tc';
                   body_instrs := !body_instrs @ exprs_instrs @ [Return (Some ret_val)]
                 else
                   let v = exprToValue expr in
                   body_instrs := !body_instrs @ [Return (Some v)])
        | Ast.Send { target; message_type; fields; _ } ->
            (* M16: Send statement - placeholder for now *)
            ()
      ) handler.body;
      
      let body = !body_instrs @ [Return None] in
      let func = {
        id = func_id;
        name = func_id;
        params = [];  (* M5: Handlers have no parameters *)
        return_ty = Ast.I32;  (* M5: Handlers return i32 (or void) *)
        body = body;
        var_types = var_types;
      } in
      functions := func :: !functions
    ) actor.handlers
  ) program.Ast.actors;

  (* Entry point is Main.on_start *)
  let entry = "Main_on_start" in
  {
    structs = program.Ast.structs;  (* M7: Include struct definitions *)
    enums = program.Ast.enums;  (* M8: Include enum definitions *)
    messages = program.Ast.messages;  (* M14: Include message definitions *)
    benches = program.Ast.benches;  (* M10.5: Include benchmark definitions *)
    entry = entry;
    functions = List.rev !functions;
  }
