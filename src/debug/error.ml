(* error.ml - Centralized error handling with nice formatting *)

type error_type =
  | SyntaxError
  | TypeError
  | RuntimeError
  | CompileError

type source_location = {
  file: string;
  line: int;
  column: int;
}

type error_info = {
  err_type: error_type;
  message: string;
  location: source_location option;
  hint: string option;
}

(* ANSI color codes *)
let red = "\027[31m"
let yellow = "\027[33m"
let blue = "\027[34m"
let bold = "\027[1m"
let reset = "\027[0m"

let error_type_to_string = function
  | SyntaxError -> "Syntax Error"
  | TypeError -> "Type Error"
  | RuntimeError -> "Runtime Error"
  | CompileError -> "Compile Error"

let format_location loc =
  Printf.sprintf "%s:%d:%d" loc.file loc.line loc.column

let print_error err =
  let type_str = error_type_to_string err.err_type in
  
  (* Print error header *)
  Printf.eprintf "%s%s%s: %s\n" red bold type_str reset;
  
  (* Print location if available *)
  (match err.location with
   | Some loc ->
     let loc_str = format_location loc in
     Printf.eprintf "  %s%s-->%s %s\n" blue bold reset loc_str
   | None -> ());
  
  (* Print message *)
  Printf.eprintf "  %s\n" err.message;
  
  (* Print hint if available *)
  (match err.hint with
   | Some hint ->
     Printf.eprintf "\n  %s%sHint:%s %s\n" yellow bold reset hint
   | None -> ());
  
  Printf.eprintf "\n";
  flush stderr

let raise_error err =
  print_error err;
  exit 1

(* Helper functions for common errors *)
let syntax_error ?hint ?location message =
  raise_error {
    err_type = SyntaxError;
    message;
    location;
    hint;
  }

let type_error ?hint ?location message =
  raise_error {
    err_type = TypeError;
    message;
    location;
    hint;
  }

let compile_error ?hint ?location message =
  raise_error {
    err_type = CompileError;
    message;
    location;
    hint;
  }

let runtime_error ?hint ?location message =
  raise_error {
    err_type = RuntimeError;
    message;
    location;
    hint;
  }

(* Create a location *)
let make_location file line column =
  { file; line; column }
