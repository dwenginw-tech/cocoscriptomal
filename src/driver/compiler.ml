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
  Module_resolver.check_circular_deps prog base_dir;
  Module_resolver.resolve_imports prog base_dir

let compile source output_name =
  let tokens = Lexer.tokenize source in
  let ast = Parser.parse tokens in
  let base_dir = Filename.dirname output_name in
  let ast = resolve_includes base_dir ast in
  let ast = Optimizer.optimize_program ast in
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
