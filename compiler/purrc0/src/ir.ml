type func_id = string

type value =
  | IntVal of int64
  | BoolVal of bool
  | StringVal of string
  | VarRef of string

type instr =
  | DeclareVar of { name: string; ty: Ast.ty }
  | Assign of { name: string; value: value }
  | BinOp of { result: string; op: Ast.binop; left: value; right: value; result_ty: Ast.ty }
  | UnOp of { result: string; op: Ast.unop; operand: value; result_ty: Ast.ty }
  | Call of { result: string; func_name: string; args: value list; result_ty: Ast.ty }  (* M4: Function calls *)
  | CallPrint of value
  | Return of value option  (* M4: Return with optional value *)
  (* M5: Control flow instructions *)
  | IfJump of { condition: value; true_label: string; false_label: string }
  | Goto of string
  | Label of string

type func = {
  id: func_id;
  name: string;
  body: instr list;
  var_types: (string, Ast.ty) Hashtbl.t;  (* Track variable types for codegen *)
}

type program_ir = {
  entry: func_id;
  functions: func list;
}

let exprToValue (expr: Ast.expr) : value =
  match expr with
  | Ast.IntLit (n, _) -> IntVal n
  | Ast.BoolLit (b, _) -> BoolVal b
  | Ast.StringLit (s, _) -> StringVal s
  | Ast.Ident (name, _) -> VarRef name
  | Ast.Call _ ->
      failwith "Function calls must be lowered with generateExprInstructions"
  | _ ->
      failwith "Complex expressions must be lowered with generateExprInstructions"

(* Lower an expression into instructions and return the resulting value *)
let rec generateExprInstructions (expr: Ast.expr) (var_types: (string, Ast.ty) Hashtbl.t) (temp_counter: int ref) : instr list * value * int ref =
  match expr with
  | Ast.IntLit (n, _) | Ast.BoolLit (_, _) | Ast.StringLit (_, _) | Ast.Ident (_, _) ->
      ([], exprToValue expr, temp_counter)
  | Ast.BinOp { left; op; right; span } ->
      let (left_instrs, left_val, tc1) = generateExprInstructions left var_types temp_counter in
      let (right_instrs, right_val, tc2) = generateExprInstructions right var_types tc1 in
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
      let (operand_instrs, operand_val, tc1) = generateExprInstructions operand var_types temp_counter in
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
            let (arg_instrs, arg_val, tc') = generateExprInstructions arg var_types (ref !tc) in
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
              let (exprs_instrs, final_val, tc') = generateExprInstructions value var_types temp_counter in
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
              let (exprs_instrs, init_val, tc') = generateExprInstructions init var_types temp_counter in
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
              let (exprs_instrs, final_val, tc') = generateExprInstructions value var_types temp_counter in
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
                   let (exprs_instrs, ret_val, tc') = generateExprInstructions expr var_types temp_counter in
                   temp_counter := !tc';
                   body_instrs := !body_instrs @ exprs_instrs @ [Return (Some ret_val)]
                 else
                   let v = exprToValue expr in
                   body_instrs := !body_instrs @ [Return (Some v)])
        | Ast.If { condition; then_body; else_body; _ } ->
            (* M5: If/else lowering *)
            let (cond_instrs, cond_val, tc') = generateExprInstructions condition var_types temp_counter in
            temp_counter := !tc';
            let else_label = Printf.sprintf "__else_%d" !temp_counter in
            incr temp_counter;
            let end_label = Printf.sprintf "__end_if_%d" !temp_counter in
            incr temp_counter;
            body_instrs := !body_instrs @ cond_instrs @ [IfJump { condition = cond_val; true_label = ""; false_label = else_label }];
            (* Process then body *)
            List.iter (fun stmt ->
              match stmt with
              | Ast.Return { value; _ } ->
                  (match value with
                   | None -> body_instrs := !body_instrs @ [Return None]
                   | Some expr ->
                       let (e_instrs, e_val, tc'') = generateExprInstructions expr var_types temp_counter in
                       temp_counter := !tc'';
                       body_instrs := !body_instrs @ e_instrs @ [Return (Some e_val)])
              | _ -> ()) (* Simplified for now *)
            ) then_body;
            body_instrs := !body_instrs @ [Goto end_label; Label else_label];
            (* Process else body *)
            (match else_body with
             | None -> ()
             | Some stmts ->
                 List.iter (fun stmt ->
                   match stmt with
                   | Ast.Return { value; _ } ->
                       (match value with
                        | None -> body_instrs := !body_instrs @ [Return None]
                        | Some expr ->
                            let (e_instrs, e_val, tc'') = generateExprInstructions expr var_types temp_counter in
                            temp_counter := !tc'';
                            body_instrs := !body_instrs @ e_instrs @ [Return (Some e_val)])
                   | _ -> ()) (* Simplified for now *)
                 ) stmts);
            body_instrs := !body_instrs @ [Label end_label]
        | Ast.For { var; start_; end_; body; _ } ->
            (* M5: For loop lowering *)
            let resolved_ty = match Sema.inferExprType start_ with
              | Some t -> t
              | None -> failwith "Cannot infer loop variable type"
            in
            Hashtbl.add var_types var resolved_ty;
            let (start_instrs, start_val, tc') = generateExprInstructions start_ var_types temp_counter in
            temp_counter := !tc';
            let (end_instrs, end_val, tc'') = generateExprInstructions end_ var_types temp_counter in
            temp_counter := !tc'';
            let loop_label = Printf.sprintf "__loop_%d" !temp_counter in
            incr temp_counter;
            let end_label = Printf.sprintf "__end_loop_%d" !temp_counter in
            incr temp_counter;
            body_instrs := !body_instrs @ [DeclareVar { name = var; ty = resolved_ty }] @ start_instrs @ [Assign { name = var; value = start_val }] @ [Label loop_label];
            (* Loop body and increment *)
            List.iter (fun stmt ->
              match stmt with
              | _ -> ()) (* Simplified for now *)
            ) body;
            let inc_temp = Printf.sprintf "__inc_%d" !temp_counter in
            incr temp_counter;
            body_instrs := !body_instrs @ [BinOp { 
              result = inc_temp; 
              op = Ast.Add; 
              left = VarRef var; 
              right = IntVal 1L; 
              result_ty = resolved_ty 
            }; 
            Assign { name = var; value = VarRef inc_temp }] @ end_instrs;
            (* Back-edge check *)
            let cond_temp = Printf.sprintf "__cond_%d" !temp_counter in
            incr temp_counter;
            body_instrs := !body_instrs @ [BinOp {
              result = cond_temp;
              op = Ast.Lte;
              left = VarRef var;
              right = end_val;
              result_ty = Ast.Bool
            };
            IfJump { condition = VarRef cond_temp; true_label = loop_label; false_label = end_label };
            Label end_label]
        | Ast.Test { name; body; _ } ->
            (* M5: Test declaration - just process body *)
            List.iter (fun stmt ->
              match stmt with
              | Ast.Return { value; _ } ->
                  (match value with
                   | None -> body_instrs := !body_instrs @ [Return None]
                   | Some expr ->
                       let (e_instrs, e_val, tc') = generateExprInstructions expr var_types temp_counter in
                       temp_counter := !tc';
                       body_instrs := !body_instrs @ e_instrs @ [Return (Some e_val)])
              | _ -> ()) (* Simplified for now *)
            ) body
      ) func.body;
      
      let func_ir = {
        id = func_id;
        name = func_id;
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
              let (exprs_instrs, final_val, tc') = generateExprInstructions value var_types temp_counter in
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
              let (exprs_instrs, init_val, tc') = generateExprInstructions init var_types temp_counter in
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
              let (exprs_instrs, final_val, tc') = generateExprInstructions value var_types temp_counter in
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
                   let (exprs_instrs, ret_val, tc') = generateExprInstructions expr var_types temp_counter in
                   temp_counter := !tc';
                   body_instrs := !body_instrs @ exprs_instrs @ [Return (Some ret_val)]
                 else
                   let v = exprToValue expr in
                   body_instrs := !body_instrs @ [Return (Some v)])
      ) handler.body;
      
      let body = !body_instrs @ [Return None] in
      let func = {
        id = func_id;
        name = func_id;
        body = body;
        var_types = var_types;
      } in
      functions := func :: !functions
    ) actor.handlers
  ) program.Ast.actors;

  (* Entry point is Main.on_start *)
  let entry = "Main_on_start" in
  {
    entry = entry;
    functions = List.rev !functions;
  }
