let () =
  match Sys.argv with
  | [| _; inputFile |] ->
      (match Purrc0_lib.Driver.compile inputFile with
       | Ok () ->
           Printf.printf "Compilation successful\n";
           exit 0
       | Error e ->
           Printf.eprintf "%s\n" (Purrc0_lib.Error.format e);
           exit 1)
  | _ ->
      Printf.eprintf "Usage: purrc <file.purr>\n";
      exit 1
