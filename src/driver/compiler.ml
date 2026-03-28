let is_windows = Sys.os_type = "Win32" || Sys.os_type = "Cygwin"

let find_tool name =
  if is_windows then
    let exe_dir = Filename.dirname Sys.executable_name in
    let bundled = Filename.concat (Filename.concat exe_dir "tools") name in
    if Sys.file_exists bundled then bundled
    else name
  else
    (* On Linux/Unix, use system tools *)
    name

let builtin_modules = ["io"]

let resolve_includes base_dir (prog : Ast.program) =
  let included_funcs = ref [] in
  List.iter (fun inc_path ->
    if List.mem inc_path builtin_modules then ()
    else
    let full_path =
      let p = Filename.concat base_dir (inc_path ^ ".coco") in
      if Sys.file_exists p then p
      else
        let exe_dir = Filename.dirname Sys.executable_name in
        let lib_path = Filename.concat (Filename.concat exe_dir "lib") (inc_path ^ ".coco") in
        if Sys.file_exists lib_path then lib_path
        else
          let src_lib = Filename.concat "src/lib" (inc_path ^ ".coco") in
          if Sys.file_exists src_lib then src_lib
          else
            let lib_cwd = Filename.concat "lib" (inc_path ^ ".coco") in
            if Sys.file_exists lib_cwd then lib_cwd
            else failwith (Printf.sprintf "cannot find include: %s" inc_path)
    in
    let ic = open_in full_path in
    let source = In_channel.input_all ic in
    close_in ic;
    let tokens = Lexer.tokenize source in
    let inc_ast = Parser.parse tokens in
    included_funcs := inc_ast.functions @ !included_funcs
  ) prog.includes;
  { prog with functions = List.rev !included_funcs @ prog.functions }

let compile source output_name =
  let tokens = Lexer.tokenize source in
  let ast = Parser.parse tokens in
  let base_dir = Filename.dirname output_name in
  let ast = resolve_includes base_dir ast in
  let asm = Codegen.gen_program ast in

  let asm_file = output_name ^ ".asm" in
  let obj_file = output_name ^ (if is_windows then ".obj" else ".o") in
  let exe_file = output_name ^ (if is_windows then ".exe" else "") in
  let oc = open_out asm_file in
  output_string oc asm;
  close_out oc;

  let nasm = find_tool (if is_windows then "nasm.exe" else "nasm") in
  let gcc = find_tool (if is_windows then "gcc.exe" else "gcc") in
  let obj_format = if is_windows then "win64" else "elf64" in
  let nasm_cmd = Printf.sprintf "\"%s\" -f %s %s -o %s" nasm obj_format asm_file obj_file in
  let nasm_ret = Sys.command nasm_cmd in
  if nasm_ret <> 0 then
    failwith "nasm assembly failed"
  else begin
    let link_cmd = 
      if is_windows then
        Printf.sprintf
          "\"%s\" %s -o %s -lmsvcrt -lkernel32 -nostartfiles -e main"
          gcc obj_file exe_file
      else
        Printf.sprintf
          "\"%s\" %s -o %s -no-pie -nostartfiles -e main"
          gcc obj_file exe_file
    in
    let link_ret = Sys.command link_cmd in
    if link_ret <> 0 then
      failwith "linking failed"
    else
      Printf.printf "compiled: %s\n" exe_file
  end
