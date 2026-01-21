open Ast

exception SemanticError of string * span

let check program =
  let funcs =
    List.filter_map
      (function
        | Func f -> Some f
        | _ -> None)
      program.items
  in
  let seen = Hashtbl.create 16 in
  List.iter
    (fun f ->
      if Hashtbl.mem seen f.name then
        raise (SemanticError ("duplicate function: " ^ f.name, f.span))
      else Hashtbl.add seen f.name f)
    funcs;
  let main =
    List.find_opt (fun f -> f.name = "main") funcs
  in
  (match main with
  | None -> raise (SemanticError ("missing main function", program.span))
  | Some f ->
      if f.return_type <> "i32" then
        raise (SemanticError ("main must return i32", f.span)));
  ()
