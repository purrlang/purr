type state = {
  tokens: Token.token array;
  mutable index: int;
}

let make tokens = {
  tokens = Array.of_list tokens;
  index = 0;
}

let current st =
  if st.index >= Array.length st.tokens then
    st.tokens.(Array.length st.tokens - 1)
  else
    st.tokens.(st.index)

let advance st =
  let tok = current st in
  st.index <- st.index + 1;
  tok

let expect st expected_kind =
  let tok = current st in
  if tok.Token.kind = expected_kind then begin
    ignore (advance st);
    Ok ()
  end
  else
    Error (Error.fromSpan tok.span
      (Printf.sprintf "Expected %s but got something else"
        (match expected_kind with
         | Token.Actor -> "actor"
         | Token.On -> "on"
         | Token.Start -> "start"
         | Token.Print -> "print_string"
         | Token.Var -> "var"
         | Token.Semicolon -> ";"
         | Token.LBrace -> "{"
         | Token.RBrace -> "}"
         | Token.LParen -> "("
         | Token.RParen -> ")"
         | Token.Colon -> ":"
         | Token.Equals -> "="
         | Token.Fn -> "fn"
         | Token.Return -> "return"
         | Token.Arrow -> "->"
         | _ -> "token")))

let expectIdent st =
  let tok = current st in
  match tok.Token.kind with
  | Token.Ident name ->
      ignore (advance st);
      Ok (name, tok.span)
  | _ ->
      Error (Error.fromSpan tok.span "Expected identifier")

let expectString st =
  let tok = current st in
  match tok.Token.kind with
  | Token.StringLit s ->
      ignore (advance st);
      Ok (s, tok.span)
  | _ ->
      Error (Error.fromSpan tok.span "Expected string literal")

let expectInt st =
  let tok = current st in
  match tok.Token.kind with
  | Token.IntLit n ->
      ignore (advance st);
      Ok (n, tok.span)
  | _ ->
      Error (Error.fromSpan tok.span "Expected integer literal")

(* M9: Parse types including generics like option<T>, list<T>, result<T,E> *)
let rec parse_type st =
  let tok = current st in
  match tok.Token.kind with
  | Token.Type ty ->
      ignore (advance st);
      let base_ty = match ty with
        | "i32" -> Ast.I32
        | "i64" -> Ast.I64
        | "string" -> Ast.String
        | "bool" -> Ast.Bool
        | _ -> failwith "Unknown type"
      in
      (* Check for ? suffix for option sugar: i32? = option<i32> *)
      let next_tok = current st in
      (match next_tok.Token.kind with
       | _ -> Ok (base_ty, tok.span))
  | Token.Nil ->
      ignore (advance st);
      Ok (Ast.Nil, tok.span)
  | Token.Option ->
      ignore (advance st);
      let _ = expect st Token.Less in
      (match parse_type st with
       | Error e -> Error e
       | Ok (inner_ty, _) ->
           let _ = expect st Token.Greater in
           Ok (Ast.Option inner_ty, tok.span))
  | Token.Result ->
      ignore (advance st);
      let _ = expect st Token.Less in
      (match parse_type st with
       | Error e -> Error e
       | Ok (ok_ty, _) ->
           let _ = expect st Token.Comma in
           (match parse_type st with
            | Error e -> Error e
            | Ok (err_ty, _) ->
                let _ = expect st Token.Greater in
                Ok (Ast.Result (ok_ty, err_ty), tok.span)))
  | Token.List ->
      ignore (advance st);
      let _ = expect st Token.Less in
      (match parse_type st with
       | Error e -> Error e
       | Ok (elem_ty, _) ->
           let _ = expect st Token.Greater in
           Ok (Ast.List elem_ty, tok.span))
  | Token.Map ->
      ignore (advance st);
      let _ = expect st Token.Less in
      (match parse_type st with
       | Error e -> Error e
       | Ok (key_ty, _) ->
           let _ = expect st Token.Comma in
           (match parse_type st with
            | Error e -> Error e
            | Ok (val_ty, _) ->
                let _ = expect st Token.Greater in
                Ok (Ast.Map (key_ty, val_ty), tok.span)))
  | Token.Fixed ->
      ignore (advance st);
      let _ = expect st Token.Less in
      (match parse_type st with
       | Error e -> Error e
       | Ok (elem_ty, _) ->
           let _ = expect st Token.Comma in
           (match expectInt st with
            | Error e -> Error e
            | Ok (size_lit, _) ->
                let size = Int64.to_int size_lit in
                let _ = expect st Token.Greater in
                Ok (Ast.Fixed (elem_ty, size), tok.span)))
  | Token.Slice ->
      ignore (advance st);
      let _ = expect st Token.Less in
      (match parse_type st with
       | Error e -> Error e
       | Ok (elem_ty, _) ->
           let _ = expect st Token.Greater in
           Ok (Ast.Slice elem_ty, tok.span))
  | _ ->
      Error (Error.fromSpan tok.span "Expected type")

let expectType st =
  parse_type st

let parse_param st =
  let tok = current st in
  match expectIdent st with
  | Error e -> Error e
  | Ok (name, _) ->
      let _ = expect st Token.Colon in
      match expectType st with
      | Error e -> Error e
      | Ok (ty, _) ->
          Ok ({ name; ty; span = tok.span } : Ast.param)

let tok_to_binop = function
  | Token.Plus -> Some Ast.Add
  | Token.Minus -> Some Ast.Sub
  | Token.Star -> Some Ast.Mul
  | Token.Slash -> Some Ast.Div
  | Token.Percent -> Some Ast.Mod
  | Token.EqualEqual -> Some Ast.Eq
  | Token.NotEqual -> Some Ast.Neq
  | Token.Less -> Some Ast.Lt
  | Token.LessEqual -> Some Ast.Lte
  | Token.Greater -> Some Ast.Gt
  | Token.GreaterEqual -> Some Ast.Gte
  | Token.And -> Some Ast.And
  | Token.Or -> Some Ast.Or
  | _ -> None

(* Returns the category of an operator (Arith, Cmp, or Logical) *)
let op_category = function
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent -> 0  (* Arith *)
  | Token.EqualEqual | Token.NotEqual | Token.Less | Token.LessEqual | Token.Greater | Token.GreaterEqual -> 1  (* Cmp *)
  | Token.And | Token.Or -> 2  (* Logical *)
  | _ -> -1

(* Parse a primary expression: literal, identifier, parenthesized expr, or unary op *)
let rec parse_primary st =
  let tok = current st in
  match tok.Token.kind with
  | Token.IntLit n ->
      ignore (advance st);
      Ok (Ast.IntLit (n, tok.span))
  | Token.True ->
      ignore (advance st);
      Ok (Ast.BoolLit (true, tok.span))
  | Token.False ->
      ignore (advance st);
      Ok (Ast.BoolLit (false, tok.span))
  | Token.Nil ->
      ignore (advance st);
      Ok (Ast.NilLit tok.span)
  | Token.StringLit s ->
      ignore (advance st);
      Ok (Ast.StringLit (s, tok.span))
  | Token.LBracket ->
      (* M9: List literal [elem1, elem2, ...] *)
      ignore (advance st);
      (match parse_list_elements st [] with
       | Error e -> Error e
       | Ok elements -> Ok (Ast.ListLit { elements; span = tok.span }))
  | Token.Ident name ->
      ignore (advance st);
      let span = tok.span in
      (* Check if this is a function call *)
      let next_tok = current st in
      (match next_tok.Token.kind with
       | Token.LParen ->
           (* Function call *)
           ignore (advance st);
           let rec parse_args acc =
             let arg_tok = current st in
             (match arg_tok.Token.kind with
              | Token.RParen ->
                  ignore (advance st);
                  Ok (List.rev acc)
              | Token.Comma ->
                  ignore (advance st);
                  parse_arg_expr acc
              | _ when acc = [] ->
                  parse_arg_expr acc
              | _ ->
                  Error (Error.fromSpan arg_tok.span "Expected ',' or ')' in argument list"))
           and parse_arg_expr acc =
             match parse_expr st with
             | Error e -> Error e
             | Ok arg -> parse_args (arg :: acc)
           in
           (match parse_args [] with
            | Error e -> Error e
            | Ok args -> Ok (Ast.Call { name; args; span }))
       | Token.LBrace ->
           (* M7: Struct literal *)
           ignore (advance st);
           let rec parse_fields acc =
             let field_tok = current st in
             (match field_tok.Token.kind with
              | Token.RBrace ->
                  ignore (advance st);
                  Ok (List.rev acc)
              | _ ->
                  (match expectIdent st with
                   | Error e -> Error e
                   | Ok (field_name, _) ->
                       let _ = expect st Token.Colon in
                       (match parse_expr st with
                        | Error e -> Error e
                        | Ok field_expr ->
                            let next_tok = current st in
                            if next_tok.Token.kind = Token.Comma then
                              let _ = advance st in
                              parse_fields ((field_name, field_expr) :: acc)
                            else if next_tok.Token.kind = Token.RBrace then
                              parse_fields ((field_name, field_expr) :: acc)
                            else
                              Error (Error.fromSpan next_tok.span "Expected ',' or '}' in struct literal"))))
           in
           (match parse_fields [] with
            | Error e -> Error e
            | Ok fields -> Ok (Ast.StructLit { struct_name = name; fields; span }))
       | _ ->
           (* Just an identifier *)
           Ok (Ast.Ident (name, span)))
  | Token.LParen ->
      ignore (advance st);
      (match parse_expr st with
       | Error e -> Error e
       | Ok expr ->
           let _ = expect st Token.RParen in
           Ok expr)
  | Token.Not ->
      ignore (advance st);
      (match parse_primary st with
       | Error e -> Error e
       | Ok operand ->
           Ok (Ast.UnOp { op = Ast.Not; operand; span = tok.span }))
  | Token.Minus ->
      (* Check if this is unary minus or subtraction *)
      (* For now, treat as unary minus in primary position *)
      ignore (advance st);
      (match parse_primary st with
       | Error e -> Error e
       | Ok operand ->
           Ok (Ast.UnOp { op = Ast.Neg; operand; span = tok.span }))
  | _ ->
      Error (Error.fromSpan tok.span "Expected primary expression (literal, identifier, or parenthesized expression)")

(* Parse operators within a single category *)
and parse_binop_in_category st left_expr category =
  let tok = current st in
  match op_category tok.Token.kind with
  | c when c = category && c >= 0 ->
      (match tok_to_binop tok.Token.kind with
       | Some op ->
           ignore (advance st);
           (match parse_primary st with
            | Error e -> Error e
            | Ok right_expr ->
                let binop_expr = Ast.BinOp { left = left_expr; op; right = right_expr; span = tok.span } in
                (* Continue parsing more operators in the same category *)
                parse_binop_in_category st binop_expr category)
       | None -> Ok left_expr)
  | _ -> Ok left_expr

(* M7: Apply postfix operations (field access) *)
(* M9: Also apply index access *)
and apply_postfix st expr =
  let tok = current st in
  match tok.Token.kind with
  | Token.Dot ->
      ignore (advance st);
      (match expectIdent st with
       | Error e -> Error e
       | Ok (field_name, _) ->
           let field_expr = Ast.FieldAccess { object_ = expr; field = field_name; span = tok.span } in
           apply_postfix st field_expr)
  | Token.LBracket ->
      (* M9: Index access obj[index] *)
      ignore (advance st);
      (match parse_expr st with
       | Error e -> Error e
       | Ok index_expr ->
           let _ = expect st Token.RBracket in
           let index_access = Ast.IndexAccess { object_ = expr; index = index_expr; span = tok.span } in
           apply_postfix st index_access)
  | _ -> Ok expr

(* M9: Helper function for parsing list elements *)
and parse_list_elements st acc =
  let elem_tok = current st in
  match elem_tok.Token.kind with
  | Token.RBracket ->
      ignore (advance st);
      Ok (List.rev acc)
  | Token.Comma ->
      ignore (advance st);
      (match parse_expr st with
       | Error e -> Error e
       | Ok elem -> parse_list_elements st (elem :: acc))
  | _ when acc = [] ->
      (match parse_expr st with
       | Error e -> Error e
       | Ok elem -> parse_list_elements st (elem :: acc))
  | _ ->
      Error (Error.fromSpan elem_tok.span "Expected ',' or ']' in list literal")

(* Parse a full expression with mandatory parenthesization between categories *)
and parse_expr st =
  match parse_primary st with
  | Error e -> Error e
  | Ok primary_expr ->
      (match apply_postfix st primary_expr with
       | Error e -> Error e
       | Ok postfix_expr ->
           (* Check if there's a binary operator *)
           let tok = current st in
           let cat = op_category tok.Token.kind in
           if cat < 0 then
             Ok postfix_expr
           else
             (* Parse the first category *)
             match parse_binop_in_category st postfix_expr cat with
             | Error e -> Error e
             | Ok expr1 ->
                 (* Check if there's another operator from a different category *)
                 let tok2 = current st in
                 let cat2 = op_category tok2.Token.kind in
                 if cat2 < 0 || cat2 = cat then
                   Ok expr1
                 else
                   (* Different category - require parentheses *)
                   Error (Error.fromSpan tok2.span
                     "Operators from different categories must be parenthesized (e.g., (a + b) * c not a + b * c)"))

(* M5: Parse if/else statement *)
and parse_if_stmt st =
  let tok = current st in
  let _ = expect st Token.If in
  (match parse_expr st with
   | Error e -> Error e
   | Ok condition ->
       let _ = expect st Token.LBrace in
       (match parseStmtList st [] with
        | Error e -> Error e
        | Ok then_body ->
            let tok2 = current st in
            let else_body = 
              if tok2.Token.kind = Token.Else then
                let _ = advance st in
                let _ = expect st Token.LBrace in
                (match parseStmtList st [] with
                 | Error e -> raise (Failure (Error.format e))
                 | Ok body -> Some body)
              else
                None
            in
            Ok (Ast.If {
              condition; 
              then_body; 
              else_body; 
              span = tok.span 
            })))

(* M5: Parse for loop *)
and parse_for_stmt st =
  let tok = current st in
  let _ = expect st Token.For in
  (match expectIdent st with
   | Error e -> Error e
   | Ok (var, _) ->
       let _ = expect st Token.In in
       (match parse_expr st with
        | Error e -> Error e
        | Ok start_ ->
            let _ = expect st Token.Comma in
            (match parse_expr st with
             | Error e -> Error e
             | Ok end_ ->
                 let _ = expect st Token.LBrace in
                 (match parseStmtList st [] with
                  | Error e -> Error e
                  | Ok body ->
                      Ok (Ast.For { 
                        var; 
                        start_; 
                        end_; 
                        body; 
                        span = tok.span 
                      })))))

(* M5: Parse test declaration *)
and parse_test_decl st =
  let tok = current st in
  let _ = expect st Token.Test in
  (match expectIdent st with
   | Error e -> Error e
   | Ok (name, _) ->
       let _ = expect st Token.LBrace in
       (match parseStmtList st [] with
        | Error e -> Error e
        | Ok body ->
            Ok (Ast.Test { 
              name; 
              body; 
              span = tok.span 
            })))

and parse_stmt st =
  let tok = current st in
  match tok.Token.kind with
  | Token.Return ->
      let _ = advance st in
      let tok2 = current st in
      (match tok2.Token.kind with
       | Token.Semicolon ->
           let _ = advance st in
           Ok (Ast.Return { value = None; span = tok.span })
       | Token.Newline ->
           (* Allow newline after return with no value *)
           let _ = advance st in
           Ok (Ast.Return { value = None; span = tok.span })
       | _ ->
           (match parse_expr st with
            | Error e -> Error e
            | Ok value ->
                let _ = expect st Token.Semicolon in
                Ok (Ast.Return { value = Some value; span = tok.span })))
  | Token.Print ->
      let _ = advance st in
      let _ = expect st Token.LParen in
      (match parse_expr st with
       | Error e -> Error e
       | Ok value ->
           let _ = expect st Token.RParen in
           let _ = expect st Token.Semicolon in
           Ok (Ast.Print { value; span = tok.span }))
  | Token.Var ->
      let _ = advance st in
      (match expectIdent st with
       | Error e -> Error e
       | Ok (name, _) ->
           let tok2 = current st in
           (match tok2.Token.kind with
            | Token.Colon ->
                (* var name: type = expr *)
                let _ = advance st in
                (match expectType st with
                 | Error e -> Error e
                 | Ok (ty, _) ->
                     let _ = expect st Token.Equals in
                     (match parse_expr st with
                      | Error e -> Error e
                      | Ok init ->
                          let _ = expect st Token.Semicolon in
                          Ok (Ast.VarDecl { 
                            name; 
                            ty = Some ty; 
                            init; 
                            span = tok.span 
                          })))
            | Token.Equals ->
                (* var name = expr (type inference) *)
                let _ = advance st in
                (match parse_expr st with
                 | Error e -> Error e
                 | Ok init ->
                     let _ = expect st Token.Semicolon in
                     Ok (Ast.VarDecl { 
                       name; 
                       ty = None; 
                       init; 
                       span = tok.span 
                     }))
            | _ ->
                Error (Error.fromSpan tok2.span "Expected ':' or '=' in variable declaration")))
  | Token.Ident name ->
      (* Check if this is an assignment *)
      let _ = advance st in
      let tok2 = current st in
      (match tok2.Token.kind with
       | Token.Equals ->
           let _ = advance st in
           (match parse_expr st with
            | Error e -> Error e
            | Ok value ->
                let _ = expect st Token.Semicolon in
                Ok (Ast.Assign { name; value; span = tok.span }))
       | _ ->
           Error (Error.fromSpan tok.span "Expected assignment operator"))
  | Token.If ->
      parse_if_stmt st
  | Token.For ->
      parse_for_stmt st
  | Token.Test ->
      parse_test_decl st
  | _ ->
      Error (Error.fromSpan tok.span "Expected statement (print, var, if, for, test, or assignment)")

and parseStmtList st acc =
  (* Skip optional newlines *)
  while current st |> fun t -> t.Token.kind = Token.Newline do
    ignore (advance st)
  done;
  
  let tok = current st in
  match tok.Token.kind with
  | Token.RBrace -> Ok (List.rev acc)
  | Token.EOF ->
      Error (Error.fromSpan tok.span "Unexpected end of file in handler")
  | _ ->
      match parse_stmt st with
      | Error e -> Error e
      | Ok stmt -> parseStmtList st (stmt :: acc)

let rec parse_func st =
  let tok = current st in
  match tok.Token.kind with
  | Token.Fn ->
      let _ = advance st in
      (match expectIdent st with
       | Error e -> Error e
       | Ok (name, _) ->
           let _ = expect st Token.LParen in
           (* Parse parameters *)
           let rec parse_params acc =
             let param_tok = current st in
             (match param_tok.Token.kind with
              | Token.RParen ->
                  ignore (advance st);
                  Ok (List.rev acc)
              | Token.Comma ->
                  ignore (advance st);
                  (match parse_param st with
                   | Error e -> Error e
                   | Ok param -> parse_params (param :: acc))
              | _ when acc = [] ->
                  (match parse_param st with
                   | Error e -> Error e
                   | Ok param -> parse_params [param])
              | _ ->
                  Error (Error.fromSpan param_tok.span "Expected ',' or ')' in parameter list"))
           in
           (match parse_params [] with
            | Error e -> Error e
            | Ok params ->
                let _ = expect st Token.Arrow in
                (match expectType st with
                 | Error e -> Error e
                 | Ok (return_ty, _) ->
                     let _ = expect st Token.LBrace in
                     (match parseStmtList st [] with
                      | Error e -> Error e
                      | Ok body ->
                          let tok2 = current st in
                          (match tok2.Token.kind with
                           | Token.RBrace ->
                               let _ = advance st in
                               Ok ({ name; params; return_ty; body; span = tok.span } : Ast.func_def)
                           | _ ->
                               Error (Error.fromSpan tok2.span "Expected }"))))))
  | _ ->
      Error (Error.fromSpan tok.span "Expected 'fn'")

let rec parse_handler st =
  let tok = current st in
  match tok.Token.kind with
  | Token.On ->
      let _ = advance st in
      let tok2 = current st in
      (match tok2.Token.kind with
       | Token.Start ->
           let _ = advance st in
           let tok3 = current st in
           (match tok3.Token.kind with
            | Token.LParen ->
                let _ = advance st in
                let _ = expect st Token.RParen in
                let tok4 = current st in
                (match tok4.Token.kind with
                 | Token.LBrace ->
                     let _ = advance st in
                     (match parseStmtList st [] with
                      | Error e -> Error e
                      | Ok body ->
                          let tok5 = current st in
                          (match tok5.Token.kind with
                           | Token.RBrace ->
                               let _ = advance st in
                               Ok { Ast.body; span = tok.span }
                           | _ ->
                               Error (Error.fromSpan tok5.span "Expected }")))
                 | _ ->
                     Error (Error.fromSpan tok4.span "Expected {"))
            | _ ->
                Error (Error.fromSpan tok3.span "Expected '('"))
       | _ ->
           Error (Error.fromSpan tok2.span "Expected 'start'"))
  | _ ->
      Error (Error.fromSpan tok.span "Expected 'on'")

let rec parse_actor_body st funcs handlers =
  (* Skip optional newlines *)
  while current st |> fun t -> t.Token.kind = Token.Newline do
    ignore (advance st)
  done;
  
  let tok = current st in
  match tok.Token.kind with
  | Token.RBrace -> Ok (List.rev funcs, List.rev handlers)
  | Token.EOF ->
      Error (Error.fromSpan tok.span "Unexpected end of file in actor")
  | Token.Fn ->
      (match parse_func st with
       | Error e -> Error e
       | Ok func -> parse_actor_body st (func :: funcs) handlers)
  | Token.On ->
      (match parse_handler st with
       | Error e -> Error e
       | Ok handler -> parse_actor_body st funcs (handler :: handlers))
  | _ ->
      Error (Error.fromSpan tok.span "Expected 'fn', 'on', or '}'")

let parse_actor st =
  let tok = current st in
  match tok.Token.kind with
  | Token.Actor ->
      let _ = advance st in
      (match expectIdent st with
       | Error e -> Error e
       | Ok (name, _) ->
           let tok2 = current st in
           (match tok2.Token.kind with
            | Token.LBrace ->
                let _ = advance st in
                (match parse_actor_body st [] [] with
                 | Error e -> Error e
                 | Ok (functions, handlers) ->
                     let tok3 = current st in
                     (match tok3.Token.kind with
                      | Token.RBrace ->
                          let _ = advance st in
                          Ok ({ name; functions; handlers; span = tok.span } : Ast.actor_def)
                      | _ ->
                          Error (Error.fromSpan tok3.span "Expected }")))
            | _ ->
                Error (Error.fromSpan tok2.span "Expected '{'")))
  | _ ->
      Error (Error.fromSpan tok.span "Expected 'actor'")

let rec parseActorList st acc =
  let tok = current st in
  match tok.Token.kind with
  | Token.EOF -> Ok (List.rev acc)
  | Token.Actor ->
      (match parse_actor st with
       | Error e -> Error e
       | Ok actor -> parseActorList st (actor :: acc))
  | _ ->
      Error (Error.fromSpan tok.span "Expected 'actor' or EOF")

(* M7: Parse struct definition *)
let parse_struct st =
  let tok = current st in
  match tok.Token.kind with
  | Token.Struct ->
      let _ = advance st in
      (match expectIdent st with
       | Error e -> Error e
       | Ok (struct_name, _) ->
           let _ = expect st Token.LBrace in
           (* Parse struct fields *)
           let rec parseFields (acc: Ast.struct_field list) : (Ast.struct_field list, Error.t) result =
             let tok = current st in
             if tok.Token.kind = Token.RBrace then
               Ok (List.rev acc)
             else
               (match expectIdent st with
                | Error e -> Error e
                | Ok (field_name, _) ->
                    let _ = expect st Token.Colon in
                    (match expectType st with
                     | Error e -> Error e
                     | Ok (ty, _) ->
                         let tok2 = current st in
                         if tok2.Token.kind = Token.Comma then
                           let _ = advance st in
                           parseFields ({ Ast.name = field_name; ty; span = tok.span } :: acc)
                         else
                           parseFields ({ Ast.name = field_name; ty; span = tok.span } :: acc)))
           in
           (match parseFields [] with
            | Error e -> Error e
            | Ok fields ->
                let _ = expect st Token.RBrace in
                Ok { Ast.name = struct_name; fields; span = tok.span }))
  | _ ->
      Error (Error.fromSpan tok.span "Expected 'struct'")

(* M8: Parse enum definition *)
let parse_enum st =
  let tok = current st in
  match tok.Token.kind with
  | Token.Enum ->
      let _ = advance st in
      (match expectIdent st with
       | Error e -> Error e
       | Ok (enum_name, _) ->
           let _ = expect st Token.LBrace in
           (* Parse enum variants separated by pipes *)
           let rec parseVariants acc =
             let tok = current st in
             if tok.Token.kind = Token.RBrace then
               Ok (List.rev acc)
             else
               (match expectIdent st with
                | Error e -> Error e
                | Ok (variant_name, _) ->
                    let variant = { Ast.name = variant_name; span = tok.span } in
                    let tok2 = current st in
                    if tok2.Token.kind = Token.Pipe then
                      let _ = advance st in
                      parseVariants (variant :: acc)
                    else if tok2.Token.kind = Token.RBrace then
                      parseVariants (variant :: acc)
                    else
                      Error (Error.fromSpan tok2.span "Expected '|' or '}' in enum definition"))
           in
           (match parseVariants [] with
            | Error e -> Error e
            | Ok variants ->
                let _ = expect st Token.RBrace in
                Ok { Ast.name = enum_name; variants; span = tok.span }))
  | _ ->
      Error (Error.fromSpan tok.span "Expected 'enum'")

(* M10.5: Parse bench declaration *)
let parse_bench_decl st =
  let tok = current st in
  let _ = expect st Token.Bench in
  (match expectString st with
   | Error e -> Error e
   | Ok (name, _) ->
       let _ = expect st Token.Iterations in
       (match expectInt st with
        | Error e -> Error e
        | Ok (iterations_lit, _) ->
            let iterations = Int64.to_int iterations_lit in
            let _ = expect st Token.LBrace in
            (* Parse setup block *)
            let setup_tok = current st in
            (match setup_tok.Token.kind with
             | Token.Setup ->
                 let _ = advance st in
                 let _ = expect st Token.LBrace in
                 (match parseStmtList st [] with
                  | Error e -> Error e
                  | Ok setup_body ->
                      (match expect st Token.RBrace with
                       | Error e -> Error e
                       | Ok () ->
                           let run_tok = current st in
                           (match run_tok.Token.kind with
                            | Token.Run ->
                                let _ = advance st in
                                let _ = expect st Token.LBrace in
                                (match parseStmtList st [] with
                                 | Error e -> Error e
                                 | Ok run_body ->
                                     let _ = expect st Token.RBrace in
                                     let _ = expect st Token.RBrace in
                                     Ok ({ name; iterations; setup_body; run_body; span = tok.span } : Ast.bench_def))
                            | _ ->
                                Error (Error.fromSpan run_tok.span "Expected 'run' block in bench"))))
             | _ ->
                 Error (Error.fromSpan setup_tok.span "Expected 'setup' block in bench"))))

let rec parseStructEnumAndActors st structs enums benches actors =
  let tok = current st in
  match tok.Token.kind with
  | Token.EOF ->
      Ok { Ast.structs = List.rev structs; enums = List.rev enums; benches = List.rev benches; actors = List.rev actors }
  | Token.Struct ->
      (match parse_struct st with
       | Error e -> Error e
       | Ok struct_def -> parseStructEnumAndActors st (struct_def :: structs) enums benches actors)
  | Token.Enum ->
      (match parse_enum st with
       | Error e -> Error e
       | Ok enum_def -> parseStructEnumAndActors st structs (enum_def :: enums) benches actors)
  | Token.Bench ->
      (match parse_bench_decl st with
       | Error e -> Error e
       | Ok bench_def -> parseStructEnumAndActors st structs enums (bench_def :: benches) actors)
  | Token.Actor ->
      (match parse_actor st with
       | Error e -> Error e
       | Ok actor -> parseStructEnumAndActors st structs enums benches (actor :: actors))
  | _ ->
      Error (Error.fromSpan tok.span "Expected 'struct', 'enum', 'bench', 'actor', or EOF")

let parse tokens =
  let st = make tokens in
  parseStructEnumAndActors st [] [] [] []
