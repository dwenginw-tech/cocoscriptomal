type method_info = {
  mname : string;
  params : string list;
  body : Ast.stmt list;
}

type class_info = {
  cname : string;
  methods : method_info list;
  mutable fields : string list;
  mutable field_types : (string * string) list;
}

let classes : (string, class_info) Hashtbl.t = Hashtbl.create 16

let register_class name methods =
  let info = { cname = name; methods; fields = []; field_types = [] } in
  Hashtbl.replace classes name info;
  info

let find_class name =
  match Hashtbl.find_opt classes name with
  | Some c -> c
  | None -> failwith (Printf.sprintf "undefined class: %s" name)

(* field offsets are 1-based; slot 0 is reserved *)
let field_offset cls field_name =
  let rec find i = function
    | [] ->
      cls.fields <- cls.fields @ [field_name];
      List.length cls.fields
    | f :: _ when f = field_name -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 1 cls.fields

let method_index cls meth_name =
  let rec find i = function
    | [] -> failwith (Printf.sprintf "undefined method: %s.%s" cls.cname meth_name)
    | m :: _ when m.mname = meth_name -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 cls.methods
