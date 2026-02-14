type symbol = { name: string; ty: Ast.ty; span: Span.t }

(* M4: Function signature for type checking *)
type func_sig = { params: Ast.ty list; return_ty: Ast.ty; span: Span.t }

type context = {
  symbols: symbol list;
  functions: (string, func_sig) Hashtbl.t;  (* M4: Track function signatures *)
}

let inferExprType (expr: Ast.expr) : Ast.ty option =
  match expr with
  | Ast.IntLit (n, _) ->
      (* Default integers to i32; if out of range, would need i64 *)
      if n >= Int64.of_int32 Int32.min_int && n <= Int64.of_int32 Int32.max_int then
        Some Ast.I32
      else
        Some Ast.I64
  | Ast.BoolLit (_, _) -> Some Ast.Bool
  | Ast.StringLit (_, _) -> Some Ast.String
  | Ast.Ident (_, _) -> None  (* Cannot infer from identifier alone *)
  | Ast.BinOp { left; op; right; span } ->
      (* Type the binary operation *)
      (match op with
       | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod ->
           (* Arithmetic - both operands must be same numeric type *)
           (match (inferExprType left, inferExprType right) with
            | (Some (Ast.I32 | Ast.I64 as t1), Some (Ast.I32 | Ast.I64 as t2)) when t1 = t2 -> Some t1
            | _ -> None)
       | Ast.Eq | Ast.Neq | Ast.Lt | Ast.Lte | Ast.Gt | Ast.Gte ->
           (* Comparisons return bool *)
           Some Ast.Bool
       | Ast.And | Ast.Or ->
           (* Logical ops return bool *)
           Some Ast.Bool)
  | Ast.UnOp { op; operand; span } ->
      (match op with
       | Ast.Not ->
           (* Logical not returns bool *)
           Some Ast.Bool
       | Ast.Neg ->
           (* Negation preserves numeric type *)
           (match inferExprType operand with
            | Some (Ast.I32 | Ast.I64 as t) -> Some t
            | _ -> None))
  | Ast.Call _ ->
      None  (* Cannot infer call return type without context *)

let rec checkExprType (expr: Ast.expr) (expected_ty: Ast.ty) (ctx: context) : (unit, Error.t) result =
  match expr with
  | Ast.IntLit (_, span) ->
      (match expected_ty with
       | Ast.I32 | Ast.I64 -> Ok ()
       | _ -> Error (Error.fromSpan span (Printf.sprintf "Expected %s but got integer literal" 
          (match expected_ty with Ast.I32 -> "i32" | Ast.I64 -> "i64" | Ast.String -> "string" | Ast.Bool -> "bool"))))
  | Ast.BoolLit (_, span) ->
      (match expected_ty with
       | Ast.Bool -> Ok ()
       | _ -> Error (Error.fromSpan span "Expected bool but got boolean literal"))
  | Ast.StringLit (_, span) ->
      (match expected_ty with
       | Ast.String -> Ok ()
       | _ -> Error (Error.fromSpan span "Expected string but got string literal"))
  | Ast.Ident (name, span) ->
      (match List.find_opt (fun (s: symbol) -> s.name = name) ctx.symbols with
       | None -> Error (Error.fromSpan span (Printf.sprintf "Undefined variable: %s" name))
       | Some sym ->
           if sym.ty = expected_ty then Ok ()
           else Error (Error.fromSpan span 
             (Printf.sprintf "Type mismatch: expected %s but got %s"
               (match expected_ty with Ast.I32 -> "i32" | Ast.I64 -> "i64" | Ast.String -> "string" | Ast.Bool -> "bool")
               (match sym.ty with Ast.I32 -> "i32" | Ast.I64 -> "i64" | Ast.String -> "string" | Ast.Bool -> "bool"))))
  | Ast.BinOp { left; op; right; span } ->
      (* Type check the binary operation *)
      (match op with
       | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod ->
           (* Arithmetic - operands must be numeric and match expected type *)
           (match expected_ty with
            | Ast.I32 | Ast.I64 ->
                (match checkExprType left expected_ty ctx with
                 | Error e -> Error e
                 | Ok () ->
                     (match checkExprType right expected_ty ctx with
                      | Error e -> Error e
                      | Ok () -> Ok ()))
            | _ -> Error (Error.fromSpan span "Arithmetic operation requires numeric types"))
       | Ast.Eq | Ast.Neq | Ast.Lt | Ast.Lte | Ast.Gt | Ast.Gte ->
           (* Comparisons return bool *)
           (match expected_ty with
            | Ast.Bool ->
                (* Check operands are compatible - must be same type *)
                (match (inferExprType left, inferExprType right) with
                 | (Some ltype, Some rtype) when ltype = rtype ->
                     (match checkExprType left ltype ctx with
                      | Error e -> Error e
                      | Ok () ->
                          (match checkExprType right rtype ctx with
                           | Error e -> Error e
                           | Ok () -> Ok ()))
                 | _ -> Error (Error.fromSpan span "Comparison operands must be same type"))
            | _ -> Error (Error.fromSpan span "Comparison returns bool"))
       | Ast.And | Ast.Or ->
           (* Logical ops require bool operands and return bool *)
           (match expected_ty with
            | Ast.Bool ->
                (match checkExprType left Ast.Bool ctx with
                 | Error e -> Error e
                 | Ok () ->
                     (match checkExprType right Ast.Bool ctx with
                      | Error e -> Error e
                      | Ok () -> Ok ()))
            | _ -> Error (Error.fromSpan span "Logical operation returns bool")))
  | Ast.UnOp { op; operand; span } ->
      (match op with
       | Ast.Not ->
           (* Logical not requires bool operand *)
           (match expected_ty with
            | Ast.Bool ->
                (match checkExprType operand Ast.Bool ctx with
                 | Error e -> Error e
                 | Ok () -> Ok ())
            | _ -> Error (Error.fromSpan span "Logical not returns bool"))
       | Ast.Neg ->
           (* Negation for numeric types *)
           (match expected_ty with
            | Ast.I32 | Ast.I64 ->
                (match checkExprType operand expected_ty ctx with
                 | Error e -> Error e
                 | Ok () -> Ok ())
            | _ -> Error (Error.fromSpan span "Negation requires numeric type")))
  | Ast.Call { name; args; span } ->
      (* Look up function signature *)
      (match Hashtbl.find_opt ctx.functions name with
       | None -> Error (Error.fromSpan span (Printf.sprintf "Unknown function: %s" name))
       | Some sig_info ->
           (* Check argument count *)
           if List.length args <> List.length sig_info.params then
             Error (Error.fromSpan span 
               (Printf.sprintf "Function %s expects %d arguments but got %d" 
                 name (List.length sig_info.params) (List.length args)))
           else
             (* Check each argument type *)
             (match List.fold_left2 (fun acc arg param_ty ->
                match acc with
                | Error e -> Error e
                | Ok () -> checkExprType arg param_ty ctx
              ) (Ok ()) args sig_info.params 
             with
              | Error e -> Error e
              | Ok () ->
                  (* Check return type matches expected *)
                  if sig_info.return_ty = expected_ty then
                    Ok ()
                  else
                    Error (Error.fromSpan span 
                      (Printf.sprintf "Function returns %s but expected %s"
                        (match sig_info.return_ty with Ast.I32 -> "i32" | Ast.I64 -> "i64" | Ast.String -> "string" | Ast.Bool -> "bool")
                        (match expected_ty with Ast.I32 -> "i32" | Ast.I64 -> "i64" | Ast.String -> "string" | Ast.Bool -> "bool")))))


let checkStmt (stmt: Ast.stmt) (ctx: context) : (context, Error.t) result =
  match stmt with
  | Ast.Print { value; span } ->
      (* Print accepts strings and can print any scalar *)
      (match value with
       | Ast.StringLit _ -> Ok ctx  (* Direct string literal is always OK *)
       | Ast.IntLit _ | Ast.BoolLit _ -> Ok ctx  (* Literals are OK *)
       | Ast.Ident (name, id_span) ->
           (* Check if variable exists *)
           (match List.find_opt (fun (s: symbol) -> s.name = name) ctx.symbols with
            | None -> Error (Error.fromSpan id_span (Printf.sprintf "Undefined variable: %s" name))
            | Some _ -> Ok ctx)  (* Any variable type is printable *)
       | _ -> Error (Error.fromSpan span "Invalid expression in print"))
  | Ast.VarDecl { name; ty; init; span } ->
      (* Determine the type *)
      let resolved_ty = match ty with
        | Some t -> t
        | None ->
            match inferExprType init with
            | Some t -> t
            | None -> 
                (* Cannot infer type without explicit annotation *)
                failwith (Printf.sprintf "Cannot infer type for variable %s" name)
      in
      (* Check if the name is already defined *)
      (match List.find_opt (fun (s: symbol) -> s.name = name) ctx.symbols with
       | Some _ -> Error (Error.fromSpan span (Printf.sprintf "Variable %s already defined" name))
       | None ->
           (* Type-check the initializer *)
           (match checkExprType init resolved_ty ctx with
            | Error e -> Error e
            | Ok () ->
                (* Add to symbol table *)
                let new_symbol = { name; ty = resolved_ty; span } in
                Ok { symbols = new_symbol :: ctx.symbols }))
  | Ast.Assign { name; value; span } ->
      (* Look up the variable *)
      (match List.find_opt (fun (s: symbol) -> s.name = name) ctx.symbols with
       | None -> Error (Error.fromSpan span (Printf.sprintf "Undefined variable: %s" name))
       | Some sym ->
           (* Type-check the value *)
           (match checkExprType value sym.ty ctx with
            | Error e -> Error e
            | Ok () -> Ok ctx))
  | Ast.Return { value; span } ->
      (* M4: Return statement validation *)
      (match value with
       | None -> Ok ctx  (* Return with no value is OK for now *)
       | Some expr ->
           (* Just validate the expression is well-formed *)
           (match inferExprType expr with
            | None -> 
                (* Try to infer from expression, but we can't validate against expected return type here *)
                (* That requires context about the enclosing function *)
                Error (Error.fromSpan span "Cannot infer type of return expression")
            | Some _ -> Ok ctx))
  | Ast.If { condition; then_body; else_body; span } ->
      (* M5: If/else statement validation *)
      (match inferExprType condition with
       | None -> Error (Error.fromSpan span "Cannot infer type of condition")
       | Some Ast.Bool ->
           (* Condition is bool, check bodies *)
           (match checkStmtList then_body ctx with
            | Error e -> Error e
            | Ok ctx' ->
                (match else_body with
                 | None -> Ok ctx'
                 | Some stmts ->
                     (match checkStmtList stmts ctx with
                      | Error e -> Error e
                      | Ok _ -> Ok ctx')))
       | Some _ -> Error (Error.fromSpan span "Condition must be boolean"))
  | Ast.For { var; start_; end_; body; span } ->
      (* M5: For loop validation *)
      (match inferExprType start_ with
       | None -> Error (Error.fromSpan span "Cannot infer type of start expression")
       | Some start_ty ->
           (match inferExprType end_ with
            | None -> Error (Error.fromSpan span "Cannot infer type of end expression")
            | Some end_ty ->
                if start_ty <> end_ty then
                  Error (Error.fromSpan span (Printf.sprintf "Start and end types must match"))
                else
                  (* Add loop variable to context *)
                  let new_symbol = { name = var; ty = start_ty; span } in
                  let ctx_with_var = { symbols = new_symbol :: ctx.symbols } in
                  (match checkStmtList body ctx_with_var with
                   | Error e -> Error e
                   | Ok _ -> Ok ctx)))
  | Ast.Test { name; body; span } ->
      (* M5: Test declaration validation *)
      (* Test body is just a sequence of assertions *)
      (match checkStmtList body ctx with
       | Error e -> Error e
       | Ok _ -> Ok ctx)

let rec checkStmtList (stmts: Ast.stmt list) (ctx: context) : (context, Error.t) result =
  match stmts with
  | [] -> Ok ctx
  | stmt :: rest ->
      (match checkStmt stmt ctx with
       | Error e -> Error e
       | Ok ctx' -> checkStmtList rest ctx')

let checkHandler (handler: Ast.handler) (func_table: (string, func_sig) Hashtbl.t) : (unit, Error.t) result =
  let init_ctx = { symbols = []; functions = func_table } in
  match checkStmtList handler.Ast.body init_ctx with
  | Error e -> Error e
  | Ok _ -> Ok ()

let checkProgram program =
  (* Check: exactly one Main actor *)
  let main_count = ref 0 in
  let seen_actors = Hashtbl.create 16 in
  let errors = ref [] in

  List.iter (fun (actor: Ast.actor_def) ->
    if actor.name = "Main" then
      incr main_count;
    if Hashtbl.mem seen_actors actor.name then
      errors := Error.fromSpan actor.span
        (Printf.sprintf "Duplicate actor name: %s" actor.name) :: !errors
    else
      Hashtbl.add seen_actors actor.name ()
  ) program.Ast.actors;

  if !main_count = 0 then
    errors := Error.make "" 0 0 "No Main actor found" :: !errors;
  if !main_count > 1 then
    errors := Error.make "" 0 0 "Multiple Main actors found" :: !errors;

  (* Check: Main actor has at least one handler *)
  (match List.find_opt (fun (a: Ast.actor_def) -> a.name = "Main") program.Ast.actors with
   | None -> ()
   | Some main_actor ->
       if List.length main_actor.handlers = 0 then
         errors := Error.fromSpan main_actor.span
           "Main actor must have at least one handler" :: !errors);

  (* M4: Type-check all functions and handlers *)
  List.iter (fun (actor: Ast.actor_def) ->
    (* Build function signature table for this actor *)
    let func_table = Hashtbl.create 16 in
    List.iter (fun (func: Ast.func_def) ->
      let param_types = List.map (fun (p: Ast.param) -> p.ty) func.params in
      Hashtbl.add func_table func.name { params = param_types; return_ty = func.return_ty; span = func.span }
    ) actor.functions;
    
    (* Check function bodies *)
    List.iter (fun (func: Ast.func_def) ->
      let init_ctx = { 
        symbols = List.map (fun (p: Ast.param) -> { name = p.name; ty = p.ty; span = p.span }) func.params;
        functions = func_table;
      } in
      match checkStmtList func.body init_ctx with
      | Error e -> errors := e :: !errors
      | Ok _ -> ()
    ) actor.functions;
    List.iter (fun (handler: Ast.handler) ->
      match checkHandler handler func_table with
      | Ok () -> ()
      | Error e -> errors := e :: !errors
    ) actor.handlers
  ) program.Ast.actors;

  if !errors <> [] then
    Error (List.hd !errors)
  else
    Ok program
