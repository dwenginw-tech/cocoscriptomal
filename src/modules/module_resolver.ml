(* module_resolver.ml - Module system for CocoScript *)

type module_info = {
  name: string;
  path: string;
  exports: string list; (* exported function names *)
  ast: Ast.program;
}

type import_spec =
  | ImportAll of string (* import "module" *)
  | ImportFrom of string * string list (* from "module" import func1, func2 *)
  | ImportAs of string * string (* import "module" as alias *)

let builtin_modules = ["io"; "math"; "string"]

(* Module search paths *)
let get_search_paths base_dir =
  let exe_dir = Filename.dirname Sys.executable_name in
  [
    base_dir;                                    
    Filename.concat base_dir "lib";              
    Filename.concat exe_dir "lib";               
    "src/lib";                                  
    "lib";                                       
  ]

let find_module_file module_name base_dir =
  if List.mem module_name builtin_modules then
    (* Builtin modules are always available *)
    None
  else
    let search_paths = get_search_paths base_dir in
    let filename = module_name ^ ".coco" in
    let rec search = function
      | [] -> 
        Error.compile_error
          ~hint:(Printf.sprintf "Make sure '%s' exists in one of: %s" 
            filename (String.concat ", " search_paths))
          (Printf.sprintf "Cannot find module: %s" module_name)
      | path :: rest ->
        let full_path = Filename.concat path filename in
        if Sys.file_exists full_path then
          Some full_path
        else
          search rest
    in
    search search_paths

let load_module module_name base_dir =
  match find_module_file module_name base_dir with
  | None -> 
    { name = module_name; path = "<builtin>"; exports = []; ast = { includes = []; functions = [] } }
  | Some path ->
    let ic = open_in path in
    let source = In_channel.input_all ic in
    close_in ic;
    let tokens = Lexer.tokenize source in
    let ast = Parser.parse tokens in
    (* Extract exported function names *)
    let exports = List.map (fun (f : Ast.func_def) -> f.name) ast.functions in
    { name = module_name; path; exports; ast }

let resolve_imports (prog : Ast.program) base_dir =
  let loaded_modules = Hashtbl.create 16 in
  
  let rec load_with_deps module_name =
    if Hashtbl.mem loaded_modules module_name then
      Hashtbl.find loaded_modules module_name
    else begin
      let mod_info = load_module module_name base_dir in
      Hashtbl.add loaded_modules module_name mod_info;
      List.iter (fun dep ->
        ignore (load_with_deps dep)
      ) mod_info.ast.includes;
      mod_info
    end
  in
  
  List.iter (fun inc -> ignore (load_with_deps inc)) prog.includes;
  
  let all_funcs = ref [] in
  Hashtbl.iter (fun _ mod_info ->
    all_funcs := mod_info.ast.functions @ !all_funcs
  ) loaded_modules;
  
  { prog with functions = !all_funcs @ prog.functions }

let check_circular_deps (prog : Ast.program) base_dir =
  let visiting = Hashtbl.create 16 in
  let visited = Hashtbl.create 16 in
  
  let rec visit module_name path =
    if Hashtbl.mem visited module_name then
      ()
    else if Hashtbl.mem visiting module_name then
      Error.compile_error
        ~hint:"Reorganize your imports to break the cycle"
        (Printf.sprintf "Circular dependency detected: %s" 
          (String.concat " -> " (List.rev (module_name :: path))))
    else begin
      Hashtbl.add visiting module_name true;
      let mod_info = load_module module_name base_dir in
      List.iter (fun dep -> visit dep (module_name :: path)) mod_info.ast.includes;
      Hashtbl.remove visiting module_name;
      Hashtbl.add visited module_name true
    end
  in
  
  List.iter (fun inc -> visit inc []) prog.includes

