(* codegen.ml — CocoScript x86-64 backend
   Targets NASM + Windows x64 calling convention (rcx, rdx, r8, r9).
   We emit raw asm text into a buffer, then flush to .asm file.
   see: https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention *)

type valtype = TInt | TStr | TFloat | TObj of string | TClosure | TArray of valtype

(* codegen state — threaded through everything *)
type context = {
  buf : Buffer.t;
  mutable lbl_seq : int;
  mutable locals : (string * int) list;
  mutable sp_off : int;
  mutable strtab : (string * string) list;
  mutable ftab : (float * string) list;
  mutable type_env : (string * valtype) list;
  mutable loop_end : string option;
  mutable current_class : string option;
  mutable pending_lambdas : string list;
}

let create () = {
  buf = Buffer.create 4096;
  lbl_seq = 0;
  locals = [];
  sp_off = 0;
  strtab = [];
  ftab = [];
  type_env = [];
  loop_end = None;
  current_class = None;
  pending_lambdas = [];
}

(* append one line of asm to the output buffer *)
let asm cg s = Buffer.add_string cg.buf s; Buffer.add_char cg.buf '\n'
let asmf cg fmt = Printf.ksprintf (asm cg) fmt

let next_label cg prefix =
  cg.lbl_seq <- cg.lbl_seq + 1;
  Printf.sprintf "%s_%d" prefix cg.lbl_seq

(* intern a string literal into the data section, dedup by value *)
let intern_string cg str =
  match List.assoc_opt str cg.strtab with
  | Some label -> label
  | None ->
    let label = next_label cg "str" in
    cg.strtab <- (str, label) :: cg.strtab;
    label

let intern_float cg f =
  match List.assoc_opt f cg.ftab with
  | Some label -> label
  | None ->
    let label = next_label cg "flt" in
    cg.ftab <- (f, label) :: cg.ftab;
    label

(* push a new local onto the stack frame *)
let alloc_local cg name =
  cg.sp_off <- cg.sp_off + 8;
  cg.locals <- (name, cg.sp_off) :: cg.locals;
  asmf cg "    sub rsp, 8"

let lookup_var cg name =
  match List.assoc_opt name cg.locals with
  | Some off -> off
  | None -> failwith (Printf.sprintf "undefined variable: %s" name)

(* name mangling: main stays main, everything else gets a _coco_ prefix
   so we don't collide with libc symbols *)
let mangle name =
  if name = "main" then "main" else "_coco_" ^ name

(* figure out which class a mangled function name belongs to, if any *)
let detect_class fname =
  let result = ref None in
  Hashtbl.iter (fun cname (cls : Class.class_info) ->
    List.iter (fun (m : Class.method_info) ->
      if cname ^ "_" ^ m.mname = fname then
        result := Some cname
    ) cls.methods
  ) Class.classes;
  !result

(* very rough type inference — just enough for print formatting
   and class dispatch. doesn't do full type checking *)
let rec infer_type cg (expr : Ast.expr) =
  match expr with
  | Ast.StringLit _ -> TStr
  | Ast.FloatLit _ -> TFloat
  | Ast.BinOp (Concat, _, _) -> TStr
  | Ast.Call ("input", _) -> TStr
  | Ast.Call ("tostring", _) -> TStr
  | Ast.Call ("toint", _) -> TInt
  | Ast.Call ("len", _) -> TInt
  | Ast.Call (name, _) when Hashtbl.mem Class.classes name -> TObj name
  | Ast.Lambda _ -> TClosure
  | Ast.TableLit (first :: _) -> TArray (infer_type cg first)
  | Ast.TableLit [] -> TArray TInt
  | Ast.FieldGet (obj, field) ->
    (let cls_name = match infer_type cg obj with
      | TObj cn -> Some cn | _ -> None
    in
    match cls_name with
    | Some cn ->
      let cls = Class.find_class cn in
      (match List.assoc_opt field cls.field_types with
       | Some "str" -> TStr | Some "float" -> TFloat
       | Some c when Hashtbl.mem Class.classes c -> TObj c
       | _ -> TInt)
    | None -> TInt)
  | Ast.Ident name ->
    (match List.assoc_opt name cg.type_env with
     | Some t -> t
     | None -> TInt)
  | _ -> TInt

(* find variables used in a lambda body that aren't defined locally *)
let free_vars params body =
  let used = Hashtbl.create 16 in
  let defined = Hashtbl.create 16 in
  List.iter (fun p -> Hashtbl.replace defined p true) params;
  let builtins = ["print";"exit";"halt";"exec";"input";"len";"tostring";"toint";"push";"pop"] in
  List.iter (fun b -> Hashtbl.replace defined b true) builtins;
  let rec walk_expr = function
    | Ast.Ident name -> Hashtbl.replace used name true
    | Ast.BinOp (_, a, b) -> walk_expr a; walk_expr b
    | Ast.UnaryOp (_, e) -> walk_expr e
    | Ast.Call (_, args) -> List.iter walk_expr args
    | Ast.Index (a, b) -> walk_expr a; walk_expr b
    | Ast.TableLit es -> List.iter walk_expr es
    | Ast.FieldGet (e, _) -> walk_expr e
    | Ast.MethodCall (e, _, args) -> walk_expr e; List.iter walk_expr args
    | Ast.NewObj (_, args) -> List.iter walk_expr args
    | Ast.Lambda (_, _) -> () (* nested lambdas don't contribute *)
    | _ -> ()
  and walk_stmt = function
    | Ast.LocalDecl (name, e) ->
      walk_expr e; Hashtbl.replace defined name true
    | Ast.MultiDecl pairs ->
      List.iter (fun (name, e) -> walk_expr e; Hashtbl.replace defined name true) pairs
    | Ast.Assign (_, e) -> walk_expr e
    | Ast.ExprStmt e -> walk_expr e
    | Ast.Return e -> walk_expr e
    | Ast.IfStmt (c, t, f) ->
      walk_expr c; List.iter walk_stmt t; List.iter walk_stmt f
    | Ast.WhileStmt (c, b) -> walk_expr c; List.iter walk_stmt b
    | Ast.ForStmt (name, lo, hi, b) ->
      Hashtbl.replace defined name true;
      walk_expr lo; walk_expr hi; List.iter walk_stmt b
    | Ast.ForEach (name, col, b) ->
      Hashtbl.replace defined name true;
      walk_expr col; List.iter walk_stmt b
    | Ast.FieldSet (o, _, v) -> walk_expr o; walk_expr v
    | Ast.IndexAssign (a, b, c) -> walk_expr a; walk_expr b; walk_expr c
    | _ -> ()
  in
  List.iter walk_stmt body;
  Hashtbl.fold (fun name _ acc ->
    if Hashtbl.mem defined name then acc else name :: acc
  ) used []

let rec compile_expr cg (expr : Ast.expr) =
  match expr with
  | Ast.IntLit n ->
    asmf cg "    mov rax, %d" n
  | Ast.BoolLit true ->
    asm cg "    mov rax, 1"
  | Ast.BoolLit false ->
    asm cg "    mov rax, 0"
  | Ast.Nil ->
    asm cg "    xor rax, rax"
  | Ast.StringLit s ->
    let label = intern_string cg s in
    asmf cg "    lea rax, [rel %s]" label
  | Ast.Ident name ->
    let off = lookup_var cg name in
    asmf cg "    mov rax, [rbp-%d]" off
  | Ast.BinOp (op, left, right) ->
    compile_expr cg left;
    asm cg "    push rax";
    compile_expr cg right;
    asm cg "    mov rcx, rax";
    asm cg "    pop rax";
    emit_binop cg op
  | Ast.UnaryOp (Neg, e) ->
    compile_expr cg e;
    asm cg "    neg rax"
  | Ast.UnaryOp (Not, e) ->
    compile_expr cg e;
    asm cg "    cmp rax, 0";
    asm cg "    sete al";
    asm cg "    movzx rax, al"
  | Ast.Call (name, args) ->
    compile_call cg name args
  | Ast.FloatLit f ->
    let label = intern_float cg f in
    asmf cg "    movsd xmm0, [rel %s]" label;
    asm cg "    movq rax, xmm0"
  | Ast.Index (arr, idx) ->
    compile_expr cg arr;
    asm cg "    push rax";
    compile_expr cg idx;
    asm cg "    mov rcx, rax";
    asm cg "    pop rax";
    asm cg "    mov rax, [rax+rcx*8]"
  | Ast.TableLit elems ->
    let n = List.length elems in
    (* allocate n+1 slots: length at [ptr], elements at [ptr+8..] *)
    asmf cg "    mov rcx, %d" ((n + 1) * 8);
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    add rsp, 32";
    asmf cg "    mov qword [rax], %d" n;
    asm cg "    add rax, 8";  (* return pointer past the length slot *)
    asm cg "    push rax";
    List.iteri (fun i elem ->
      compile_expr cg elem;
      asm cg "    pop rcx";
      asmf cg "    mov [rcx+%d], rax" (i * 8);
      asm cg "    push rcx"
    ) elems;
    asm cg "    pop rax"
  | Ast.FieldGet (obj, field) ->
    let cls_name = match infer_type cg obj with
      | TObj cn -> cn
      | _ -> failwith "field access on non-object"
    in
    let cls = Class.find_class cls_name in
    let off = Class.field_offset cls field in
    compile_expr cg obj;
    asmf cg "    mov rax, [rax+%d]" (off * 8)
  | Ast.MethodCall (obj, meth, args) ->
    let cls_name = match infer_type cg obj with
      | TObj cn -> cn
      | _ -> failwith "method call on non-object"
    in
    compile_call cg (cls_name ^ "_" ^ meth) (obj :: args)
  | Ast.NewObj (cls_name, args) ->
    compile_new_obj cg cls_name args
  | Ast.Lambda (params, body) ->
    compile_lambda cg params body

and emit_binop cg (op : Ast.binop) =
  match op with
  | Add -> asm cg "    add rax, rcx"
  | Sub -> asm cg "    sub rax, rcx"
  | Mul -> asm cg "    imul rax, rcx"
  | Div -> asm cg "    cqo"; asm cg "    idiv rcx"
  | Mod -> asm cg "    cqo"; asm cg "    idiv rcx"; asm cg "    mov rax, rdx"
  | Eq ->
    asm cg "    cmp rax, rcx";
    asm cg "    sete al";
    asm cg "    movzx rax, al"
  | Neq ->
    asm cg "    cmp rax, rcx";
    asm cg "    setne al";
    asm cg "    movzx rax, al"
  | Lt ->
    asm cg "    cmp rax, rcx";
    asm cg "    setl al";
    asm cg "    movzx rax, al"
  | Gt ->
    asm cg "    cmp rax, rcx";
    asm cg "    setg al";
    asm cg "    movzx rax, al"
  | Lte ->
    asm cg "    cmp rax, rcx";
    asm cg "    setle al";
    asm cg "    movzx rax, al"
  | Gte ->
    asm cg "    cmp rax, rcx";
    asm cg "    setge al";
    asm cg "    movzx rax, al"
  | And -> asm cg "    and rax, rcx"
  | Or -> asm cg "    or rax, rcx"
  | Concat ->
    (* rax = left string pointer, rcx = right string pointer.
       FIXME: leaks memory on every concat, need a gc eventually *)
    asm cg "    push rcx";
    asm cg "    push rax";
    asm cg "    sub rsp, 32";  (* shadow space *)
    asm cg "    mov rcx, 1024";
    asm cg "    call _coco_alloc";
    asm cg "    add rsp, 32";
    asm cg "    pop r8";         (* left str *)
    asm cg "    pop r9";         (* right str *)
    asm cg "    mov rcx, rax";   (* buffer *)
    asm cg "    push rax";       (* save buffer ptr for result *)
    asm cg "    lea rdx, [rel fmt_concat]";
    asm cg "    sub rsp, 32";
    asm cg "    call sprintf";
    asm cg "    add rsp, 32";
    asm cg "    pop rax"         (* buffer ptr is our result *)

(* compile an anonymous function: build the lambda as a top-level function,
   allocate an env struct with captured values, return a closure pair *)
and compile_lambda cg params body =
  let captures = free_vars params body in
  let lam_name = next_label cg "_coco_lambda" in
  (* compile the lambda body into a separate buffer *)
  let lam_buf = Buffer.create 512 in
  let lam_asm s = Buffer.add_string lam_buf s; Buffer.add_char lam_buf '\n' in
  let lam_asmf fmt = Printf.ksprintf lam_asm fmt in
  lam_asmf "global %s" lam_name;
  lam_asmf "%s:" lam_name;
  lam_asm "    push rbp";
  lam_asm "    mov rbp, rsp";
  (* _env in rcx, user params shifted to rdx/r8/r9 *)
  let lam_locals = ref [] in
  let lam_sp = ref 0 in
  let lam_alloc name =
    lam_sp := !lam_sp + 8;
    lam_locals := (name, !lam_sp) :: !lam_locals;
    lam_asm "    sub rsp, 8"
  in
  (* bind captured vars from env struct (rcx) *)
  lam_asm "    push rcx";
  List.iteri (fun i cap_name ->
    lam_alloc cap_name;
    lam_asmf "    mov rax, [rcx+%d]" (i * 8);
    lam_asmf "    mov [rbp-%d], rax" !lam_sp
  ) captures;
  lam_asm "    pop rcx";
  (* bind user params from shifted registers *)
  let shifted_regs = [| "rdx"; "r8"; "r9" |] in
  List.iteri (fun i pname ->
    lam_alloc pname;
    if i < 3 then
      lam_asmf "    mov [rbp-%d], %s" !lam_sp shifted_regs.(i)
  ) params;
  (* compile body using a temporary sub-context *)
  let sub_cg = {
    buf = lam_buf;
    lbl_seq = cg.lbl_seq;
    locals = !lam_locals;
    sp_off = !lam_sp;
    strtab = cg.strtab;
    ftab = cg.ftab;
    type_env = List.filter_map (fun name ->
      match List.assoc_opt name cg.type_env with
      | Some t -> Some (name, t)
      | None -> None
    ) captures;
    loop_end = None;
    current_class = None;
    pending_lambdas = [];
  } in
  List.iter (compile_stmt sub_cg) body;
  (* sync shared state back *)
  cg.lbl_seq <- sub_cg.lbl_seq;
  cg.strtab <- sub_cg.strtab;
  cg.ftab <- sub_cg.ftab;
  lam_asm "    xor rax, rax";
  lam_asm "    mov rsp, rbp";
  lam_asm "    pop rbp";
  lam_asm "    ret";
  lam_asm "";
  (* queue the lambda code for emission after the current function *)
  cg.pending_lambdas <- Buffer.contents lam_buf :: cg.pending_lambdas;
  cg.pending_lambdas <- sub_cg.pending_lambdas @ cg.pending_lambdas;
  (* now emit closure creation at the call site *)
  let ncap = List.length captures in
  if ncap > 0 then begin
    (* malloc env struct *)
    asmf cg "    mov rcx, %d" (ncap * 8);
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    add rsp, 32";
    (* copy captured values into env *)
    List.iteri (fun i cap_name ->
      let off = lookup_var cg cap_name in
      asmf cg "    mov rcx, [rbp-%d]" off;
      asmf cg "    mov [rax+%d], rcx" (i * 8)
    ) captures;
    asm cg "    push rax"  (* save env ptr *)
  end else
    asm cg "    push 0";  (* null env *)
  (* malloc closure pair [func_ptr | env_ptr] *)
  asm cg "    mov rcx, 16";
  asm cg "    sub rsp, 32";
  asm cg "    call _coco_alloc";
  asm cg "    add rsp, 32";
  asm cg "    pop rcx";  (* env ptr *)
  asmf cg "    lea rdx, [rel %s]" lam_name;
  asm cg "    mov [rax+0], rdx";  (* func ptr *)
  asm cg "    mov [rax+8], rcx"   (* env ptr *)
  (* rax = closure value *)

(* call a closure stored in a local variable *)
and compile_closure_call cg name args =
  (* evaluate user args and push them *)
  List.iter (fun arg ->
    compile_expr cg arg;
    asm cg "    push rax"
  ) (List.rev args);
  (* load closure struct *)
  let closure_off = lookup_var cg name in
  asmf cg "    mov rax, [rbp-%d]" closure_off;
  asm cg "    mov r10, [rax+0]";  (* func_ptr *)
  asm cg "    mov rcx, [rax+8]";  (* env_ptr -> rcx (hidden first arg) *)
  (* pop user args into rdx, r8, r9 *)
  let n = List.length args in
  let shifted_regs = [| "rdx"; "r8"; "r9" |] in
  for i = 0 to min n 3 - 1 do
    asmf cg "    pop %s" shifted_regs.(i)
  done;
  (* clean up any remaining pushed args *)
  if n > 3 then
    asmf cg "    add rsp, %d" ((n - 3) * 8);
  asm cg "    sub rsp, 32";
  asm cg "    call r10";
  asm cg "    add rsp, 32"

and compile_new_obj cg cls_name args =
  asm cg "    mov rcx, 256";
  asm cg "    sub rsp, 32";
  asm cg "    call _coco_alloc";
  asm cg "    add rsp, 32";
  let cls = Class.find_class cls_name in
  let has_init = List.exists (fun (m : Class.method_info) ->
    m.mname = "init"
  ) cls.methods in
  (* pre-register field types by matching init params to constructor args.
     if init does self.x = x, the field x gets the type of the arg *)
  if has_init then begin
    let init_m = List.find (fun (m : Class.method_info) ->
      m.mname = "init") cls.methods in
    List.iter2 (fun param arg ->
      let ty = infer_type cg arg in
      let ty_str = match ty with
        | TStr -> "str" | TFloat -> "float" | TObj c -> c
        | TClosure | TArray _ -> "int" | TInt -> "int"
      in
      (* assume field name matches param name — common pattern *)
      cls.field_types <- (param, ty_str) :: (List.remove_assoc param cls.field_types)
    ) init_m.params (List.filteri (fun i _ -> i < List.length init_m.params) args)
  end;
  if has_init then begin
    asm cg "    push rax";
    List.iter (fun arg ->
      compile_expr cg arg;
      asm cg "    push rax"
    ) (List.rev args);
    let nargs = List.length args in
    asmf cg "    mov rax, [rsp+%d]" (nargs * 8);
    asm cg "    push rax";
    let n = nargs + 1 in
    let regs = [| "rcx"; "rdx"; "r8"; "r9" |] in
    let shadow = max n 4 * 8 in
    asmf cg "    sub rsp, %d" shadow;
    for i = 0 to n - 1 do
      if i < 4 then
        asmf cg "    mov %s, [rsp+%d]" regs.(i) (shadow + i * 8)
    done;
    asmf cg "    call %s" (mangle (cls_name ^ "_init"));
    asmf cg "    add rsp, %d" (shadow + n * 8);
    asm cg "    pop rax"
  end

and compile_call cg name args =
  if List.assoc_opt name cg.locals <> None &&
     List.assoc_opt name cg.type_env = Some TClosure then
    compile_closure_call cg name args
  else if Hashtbl.mem Class.classes name then
    compile_new_obj cg name args
  else if name = "print" then
    builtin_print cg args
  else if name = "exit" then
    builtin_exit cg args
  else if name = "halt" then
    builtin_halt cg args
  else if name = "exec" then
    builtin_exec cg args
  else if name = "input" then
    builtin_input cg args
  else if name = "len" then
    builtin_len cg args
  else if name = "tostring" then
    builtin_tostring cg args
  else if name = "toint" then
    builtin_toint cg args
  else if name = "push" then
    builtin_push cg args
  else if name = "pop" then
    builtin_pop cg args
  else begin
    let regs = [| "rcx"; "rdx"; "r8"; "r9" |] in
    let n = List.length args in
    (* evaluate all args and push to stack first —
       avoid clobbering args in nested calls, learned this the hard way with clamp() *)
    List.iter (fun arg ->
      compile_expr cg arg;
      asm cg "    push rax"
    ) (List.rev args);
    (* pop into the right registers *)
    let shadow = max n 4 * 8 in
    asmf cg "    sub rsp, %d" shadow;
    for i = 0 to n - 1 do
      if i < 4 then begin
        asmf cg "    mov %s, [rsp+%d]" regs.(i) (shadow + i * 8)
      end
    done;
    asmf cg "    call %s" (mangle name);
    asmf cg "    add rsp, %d" (shadow + n * 8)
  end

and builtin_print cg args =
  match args with
  | [] -> failwith "print takes at least 1 argument"
  | _ ->
    List.iter (fun arg ->
      let ty = infer_type cg arg in
      compile_expr cg arg;
      asm cg "    sub rsp, 32";
      (match ty with
      | TFloat ->
        asm cg "    movq xmm1, rax";
        asm cg "    mov rdx, rax";
        asm cg "    lea rcx, [rel fmt_float]"
      | TStr ->
        asm cg "    mov rdx, rax";
        asm cg "    lea rcx, [rel fmt_str]"
      | TInt | TObj _ | TClosure | TArray _ ->
        asm cg "    mov rdx, rax";
        asm cg "    lea rcx, [rel fmt_int]");
      asm cg "    call printf";
      asm cg "    add rsp, 32"
    ) args

and builtin_exit cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    sub rsp, 32";
    asm cg "    mov rcx, rax";
    asm cg "    call exit";
    asm cg "    add rsp, 32"
  | [] ->
    asm cg "    sub rsp, 32";
    asm cg "    xor rcx, rcx";
    asm cg "    call exit";
    asm cg "    add rsp, 32"
  | _ -> failwith "exit takes 0 or 1 argument"

and builtin_halt cg args =
  match args with
  | [] ->
    let label = intern_string cg "pause" in
    (* align stack to 16 bytes before calling system().
       if we don't do this, system() segfaults on some win10 builds *)
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asmf cg "    lea rcx, [rel %s]" label;
    asm cg "    call system";
    asm cg "    mov rsp, rbx"
  | _ -> failwith "halt takes no arguments"

and builtin_exec cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    (* same stack alignment dance as halt — TODO: factor this out *)
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    mov rcx, rax";
    asm cg "    call system";
    asm cg "    mov rsp, rbx"
  | _ -> failwith "exec takes exactly 1 argument"

and builtin_input cg args =
  match args with
  | [] ->
    (* allocate a 1k buffer for the input string.
       FIXME: no bounds check, user could overflow this *)
    asm cg "    mov rcx, 1024";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    add rsp, 32";
    asm cg "    push rax";
    asm cg "    lea rcx, [rel fmt_input]";
    asm cg "    mov rdx, rax";
    asm cg "    sub rsp, 32";
    asm cg "    call scanf";
    asm cg "    add rsp, 32";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call getchar";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rax"
  | _ -> failwith "input takes no arguments"

and builtin_len cg args =
  match args with
  | [arg] ->
    let ty = infer_type cg arg in
    compile_expr cg arg;
    (match ty with
     | TStr ->
       asm cg "    mov rbx, rsp";
       asm cg "    and rsp, -16";
       asm cg "    sub rsp, 32";
       asm cg "    mov rcx, rax";
       asm cg "    call strlen";
       asm cg "    mov rsp, rbx"
     | _ ->
       (* array: length stored at [ptr - 8] *)
       asm cg "    mov rax, [rax-8]")
  | _ -> failwith "len takes exactly 1 argument"

and builtin_tostring cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    push rax";
    asm cg "    mov rcx, 64";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rdx";
    asm cg "    push rax";
    asm cg "    mov r8, rdx";
    asm cg "    lea rdx, [rel fmt_int_bare]";
    asm cg "    mov rcx, rax";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call sprintf";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rax"
  | _ -> failwith "tostring takes exactly 1 argument"

and builtin_toint cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    mov rcx, rax";
    asm cg "    call atoi";
    asm cg "    mov rsp, rbx"
  | _ -> failwith "toint takes exactly 1 argument"

(* push(array, value) — appends to end, bumps length.
   arrays are over-allocated so we just write past the current length *)
and builtin_push cg args =
  match args with
  | [arr; value] ->
    compile_expr cg arr;
    asm cg "    push rax";  (* save array ptr *)
    compile_expr cg value;
    asm cg "    mov rdx, rax";  (* value *)
    asm cg "    pop rax";  (* array ptr *)
    asm cg "    mov rcx, [rax-8]";  (* current length *)
    asm cg "    mov [rax+rcx*8], rdx";  (* store at end *)
    asm cg "    inc qword [rax-8]"  (* bump length *)
  | _ -> failwith "push takes 2 arguments (array, value)"

(* pop(array) — removes and returns last element *)
and builtin_pop cg args =
  match args with
  | [arr] ->
    compile_expr cg arr;
    asm cg "    dec qword [rax-8]";
    asm cg "    mov rcx, [rax-8]";
    asm cg "    mov rax, [rax+rcx*8]"
  | _ -> failwith "pop takes 1 argument (array)"

and compile_stmt cg (stmt : Ast.stmt) =
  match stmt with
  | Ast.LocalDecl (name, expr) ->
    let ty = infer_type cg expr in
    cg.type_env <- (name, ty) :: cg.type_env;
    compile_expr cg expr;
    alloc_local cg name;
    let off = lookup_var cg name in
    asmf cg "    mov [rbp-%d], rax" off
  | Ast.Assign (name, expr) ->
    compile_expr cg expr;
    let off = lookup_var cg name in
    asmf cg "    mov [rbp-%d], rax" off
  | Ast.IfStmt (cond, then_body, else_body) ->
    let else_lbl = next_label cg "else" in
    let end_lbl = next_label cg "endif" in
    compile_expr cg cond;
    asm cg "    cmp rax, 0";
    asmf cg "    je near %s" else_lbl; (* nasm needs `near` or it guesses wrong jump sizes *)
    List.iter (compile_stmt cg) then_body;
    asmf cg "    jmp near %s" end_lbl;
    asmf cg "%s:" else_lbl;
    List.iter (compile_stmt cg) else_body;
    asmf cg "%s:" end_lbl
  | Ast.WhileStmt (cond, body) ->
    let start_lbl = next_label cg "while" in
    let end_lbl = next_label cg "endwhile" in
    let old_loop_end = cg.loop_end in
    cg.loop_end <- Some end_lbl;
    asmf cg "%s:" start_lbl;
    compile_expr cg cond;
    asm cg "    cmp rax, 0";
    asmf cg "    je near %s" end_lbl;
    List.iter (compile_stmt cg) body;
    asmf cg "    jmp near %s" start_lbl;
    asmf cg "%s:" end_lbl;
    cg.loop_end <- old_loop_end
  | Ast.ForStmt (name, start, finish, body) ->
    compile_expr cg start;
    alloc_local cg name;
    let off = lookup_var cg name in
    asmf cg "    mov [rbp-%d], rax" off;
    let start_lbl = next_label cg "for" in
    let end_lbl = next_label cg "endfor" in
    let old_loop_end = cg.loop_end in
    cg.loop_end <- Some end_lbl;
    asmf cg "%s:" start_lbl;
    asmf cg "    mov rax, [rbp-%d]" off;
    asm cg "    push rax";
    compile_expr cg finish;
    asm cg "    mov rcx, rax";
    asm cg "    pop rax";
    asm cg "    cmp rax, rcx";
    asmf cg "    jg near %s" end_lbl;
    List.iter (compile_stmt cg) body;
    asmf cg "    mov rax, [rbp-%d]" off;
    asm cg "    inc rax";
    asmf cg "    mov [rbp-%d], rax" off;
    asmf cg "    jmp near %s" start_lbl;
    asmf cg "%s:" end_lbl;
    cg.loop_end <- old_loop_end
  | Ast.ForEach (name, collection, body) ->
    (* infer element type from the collection *)
    let elem_ty = match infer_type cg collection with
      | TArray t -> t | _ -> TInt in
    cg.type_env <- (name, elem_ty) :: cg.type_env;
    compile_expr cg collection;
    let arr_name = "_foreach_arr_" ^ name in
    let idx_name = "_foreach_idx_" ^ name in
    alloc_local cg arr_name;
    let arr_off = lookup_var cg arr_name in
    asmf cg "    mov [rbp-%d], rax" arr_off;
    alloc_local cg idx_name;
    let idx_off = lookup_var cg idx_name in
    asm cg "    xor rax, rax";
    asmf cg "    mov [rbp-%d], rax" idx_off;
    alloc_local cg name;
    let var_off = lookup_var cg name in
    let start_lbl = next_label cg "foreach" in
    let end_lbl = next_label cg "endforeach" in
    let old_loop_end = cg.loop_end in
    cg.loop_end <- Some end_lbl;
    asmf cg "%s:" start_lbl;
    (* check _i < len *)
    asmf cg "    mov rax, [rbp-%d]" idx_off;
    asmf cg "    mov rcx, [rbp-%d]" arr_off;
    asm cg "    cmp rax, [rcx-8]";
    asmf cg "    jge near %s" end_lbl;
    (* name = arr[_i] *)
    asmf cg "    mov rcx, [rbp-%d]" arr_off;
    asmf cg "    mov rdx, [rbp-%d]" idx_off;
    asm cg "    mov rax, [rcx+rdx*8]";
    asmf cg "    mov [rbp-%d], rax" var_off;
    List.iter (compile_stmt cg) body;
    (* _i++ *)
    asmf cg "    mov rax, [rbp-%d]" idx_off;
    asm cg "    inc rax";
    asmf cg "    mov [rbp-%d], rax" idx_off;
    asmf cg "    jmp near %s" start_lbl;
    asmf cg "%s:" end_lbl;
    cg.loop_end <- old_loop_end
  | Ast.Return expr ->
    compile_expr cg expr;
    asm cg "    mov rsp, rbp";
    asm cg "    pop rbp";
    asm cg "    ret"
  | Ast.Break ->
    (match cg.loop_end with
     | Some lbl -> asmf cg "    jmp near %s" lbl
     | None -> failwith "break outside of loop")
  | Ast.ExprStmt expr -> compile_expr cg expr
  | Ast.MultiDecl pairs ->
    List.iter (fun (name, expr) ->
      let ty = infer_type cg expr in
      cg.type_env <- (name, ty) :: cg.type_env;
      compile_expr cg expr;
      alloc_local cg name;
      let off = lookup_var cg name in
      asmf cg "    mov [rbp-%d], rax" off
    ) pairs
  | Ast.Include _ -> ()
  | Ast.ClassDecl _ -> () (* methods already flattened by parser *)
  | Ast.FieldSet (obj, field, value) ->
    let cls_name = match infer_type cg obj with
      | TObj cn -> cn
      | _ -> failwith "field set on non-object"
    in
    let cls = Class.find_class cls_name in
    let off = Class.field_offset cls field in
    compile_expr cg value;
    asm cg "    push rax";
    compile_expr cg obj;
    asm cg "    pop rcx";
    asmf cg "    mov [rax+%d], rcx" (off * 8)
  | Ast.IndexAssign (arr, idx, value) ->
    compile_expr cg arr;
    asm cg "    push rax";
    compile_expr cg idx;
    asm cg "    push rax";
    compile_expr cg value;
    asm cg "    mov rdx, rax";
    asm cg "    pop rcx";
    asm cg "    pop rax";
    asm cg "    mov [rax+rcx*8], rdx"

(* emit a complete function: prologue, body, epilogue *)
let compile_function cg (f : Ast.func_def) =
  cg.locals <- [];
  cg.sp_off <- 0;
  cg.type_env <- [];
  cg.current_class <- detect_class f.name;
  (match cg.current_class with
   | Some cn -> cg.type_env <- ("self", TObj cn) :: cg.type_env
   | None -> ());
  let mangled = mangle f.name in
  asmf cg "global %s" mangled;
  asmf cg "%s:" mangled;
  asm cg "    push rbp";
  asm cg "    mov rbp, rsp";
  (* bind parameters from registers to stack slots.
     win64 only passes the first 4 in regs, rest on stack (but we don't support >4 yet) *)
  let regs = [| "rcx"; "rdx"; "r8"; "r9" |] in
  List.iteri (fun i name ->
    alloc_local cg name;
    let off = lookup_var cg name in
    if i < 4 then asmf cg "    mov [rbp-%d], %s" off regs.(i)
  ) f.params;
  List.iter (compile_stmt cg) f.body;
  (* default return 0 if control falls through *)
  asm cg "    xor rax, rax";
  asm cg "    mov rsp, rbp";
  asm cg "    pop rbp";
  asm cg "    ret";
  asm cg "";
  (* emit any lambda functions that were created inside this function *)
  List.iter (fun lam_code -> Buffer.add_string cg.buf lam_code) cg.pending_lambdas;
  cg.pending_lambdas <- []

(* encode a string for NASM db directives, emitting non-printable
   and special chars as raw byte values instead of inside quotes *)
let encode_nasm_string str =
  let buf = Buffer.create (String.length str * 4) in
  let in_quotes = ref false in
  String.iter (fun ch ->
    let code = Char.code ch in
    if code >= 32 && code < 127 && ch <> '"' && ch <> '\\' then begin
      if not !in_quotes then (if Buffer.length buf > 0 then Buffer.add_string buf ", "; Buffer.add_char buf '"'; in_quotes := true);
      Buffer.add_char buf ch
    end else begin
      if !in_quotes then (Buffer.add_char buf '"'; in_quotes := false);
      if Buffer.length buf > 0 then Buffer.add_string buf ", ";
      Buffer.add_string buf (string_of_int code)
    end
  ) str;
  if !in_quotes then Buffer.add_char buf '"';
  Buffer.contents buf

(* pre-scan: find constructor calls and register field types before
   compiling class methods. this way Dog_info knows self.name is a string
   even though Dog_init compiles before main *)
let preregister_field_types cg (prog : Ast.program) =
  let scan_expr = function
    | Ast.Call (name, args) when Hashtbl.mem Class.classes name ->
      let cls = Class.find_class name in
      let init_opt = List.find_opt (fun (m : Class.method_info) ->
        m.mname = "init") cls.methods in
      (match init_opt with
       | Some init_m ->
         List.iteri (fun i arg ->
           if i < List.length init_m.params then begin
             let param = List.nth init_m.params i in
             let ty_str = match infer_type cg arg with
               | TStr -> "str" | TFloat -> "float"
               | TObj c -> c | TClosure | TArray _ -> "int" | TInt -> "int"
             in
             List.iter (function
               | Ast.FieldSet (Ast.Ident "self", field, Ast.Ident p)
                 when p = param ->
                 cls.field_types <-
                   (field, ty_str) :: List.remove_assoc field cls.field_types
               | _ -> ()
             ) init_m.body
           end
         ) args
       | None -> ())
    | _ -> ()
  in
  List.iter (fun (f : Ast.func_def) ->
    List.iter (function
      | Ast.LocalDecl (_, e) | Ast.ExprStmt e -> scan_expr e
      | _ -> ()
    ) f.body
  ) prog.functions

(* main entry point — called from compiler.ml *)
let compile_program (prog : Ast.program) =
  let cg = create () in
  asm cg "bits 64";
  asm cg "default rel";
  asm cg "";
  (* external symbols we link against from msvcrt / ucrt *)
  asm cg "extern printf";
  asm cg "extern exit";
  asm cg "extern malloc";
  asm cg "extern free";
  asm cg "extern system";
  asm cg "extern sprintf";
  asm cg "extern scanf";
  asm cg "extern strlen";
  asm cg "extern atoi";
  asm cg "extern getchar";
  asm cg "";
  asm cg "section .text";
  asm cg "";
  Gc.emit_allocator (asm cg);
  preregister_field_types cg prog;
  (List.iter (compile_function cg) prog.functions;
  (* data section: format strings + interned literals *)
  asm cg "section .data";
  asm cg "    fmt_int db \"%d\", 10, 0";
  asm cg "    fmt_str db \"%s\", 10, 0";
  asm cg "    fmt_concat db \"%s%s\", 0";
  asm cg "    fmt_input db \"%1023[^\", 10, \"]\", 0";
  asm cg "    fmt_float db \"%f\", 10, 0";
  asm cg "    fmt_int_bare db \"%d\", 0";
  Gc.emit_arena_data (asm cg);
  List.iter (fun (str, label) ->
    asmf cg "    %s db %s, 0" label (encode_nasm_string str)
  ) cg.strtab;
  List.iter (fun (f, label) ->
    asmf cg "    %s dq %.17g" label f
  ) cg.ftab;
  Buffer.contents cg.buf : string)

(* backward compat alias — compiler.ml calls Codegen.gen_program *)
let gen_program = compile_program
