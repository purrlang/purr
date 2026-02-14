let readFile filename =
  try
    let ic = open_in filename in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Ok s
  with Sys_error msg ->
    Error (Error.make filename 0 0 msg)

let writeFile filename contents =
  try
    let oc = open_out filename in
    output_string oc contents;
    close_out oc;
    Ok ()
  with Sys_error msg ->
    Error (Error.make filename 0 0 msg)

let compileCSource cFile exeName =
  let runtimeDir = "runtime" in
  let runtimeC = Filename.concat runtimeDir "purr_runtime.c" in
  let runtimeH = Filename.concat runtimeDir "purr_runtime.h" in
  
  (* Try to find cc, gcc, or clang *)
  let ccCmd =
    let candidates = ["cc"; "gcc"; "clang"; "cl"] in
    let rec findCc = function
      | [] -> None
      | cc :: rest ->
          let cmd = Printf.sprintf "where %s >nul 2>&1" cc in
          if Sys.command cmd = 0 then Some cc else findCc rest
    in
    findCc candidates
  in
  
  match ccCmd with
  | None ->
      Error (Error.make cFile 0 0 "C compiler not found (tried cc, gcc, clang, cl)")
  | Some cc ->
      let cmd = Printf.sprintf "%s %s %s -o %s -I %s"
        cc cFile runtimeC exeName runtimeDir
      in
      if Sys.command cmd = 0 then
        Ok ()
      else
        Error (Error.make cFile 0 0 (Printf.sprintf "C compilation failed with command: %s" cmd))

let compile inputFile =
  match readFile inputFile with
  | Error e ->
      Error e
  | Ok source ->
      (* Tokenize *)
      (match Lexer.tokenize inputFile source with
       | Error e -> Error e
       | Ok tokens ->
           (* Parse *)
           (match Parser.parse tokens with
            | Error e -> Error e
            | Ok ast ->
                (* Semantic analysis *)
                (match Sema.checkProgram ast with
                 | Error e -> Error e
                 | Ok validatedAst ->
                     (* Lower to IR *)
                     let ir = Ir.lower validatedAst in
                     (* Codegen *)
                     let c_code = Codegen_c.generateC ir in
                     (* Write C file *)
                     let baseName = Filename.chop_extension inputFile in
                     let cFile = baseName ^ ".c" in
                     let exeName = baseName ^ ".exe" in
                     (match writeFile cFile c_code with
                      | Error e -> Error e
                      | Ok () ->
                          (* Compile C to exe *)
                          compileCSource cFile exeName))))
