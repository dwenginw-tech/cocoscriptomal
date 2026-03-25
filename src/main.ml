let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "CocoScript Compiler v0.1\n";
    Printf.printf "Usage: cocoscript <file.coco>\n";
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let output_name =
    if Array.length Sys.argv >= 3 then Sys.argv.(2)
    else Filename.remove_extension filename
  in
  let ic = open_in filename in
  let source = In_channel.input_all ic in
  close_in ic;
  Compiler.compile source output_name
