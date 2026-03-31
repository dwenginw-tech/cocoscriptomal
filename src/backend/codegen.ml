(* codegen.ml — CocoScript x86-64 backend
   Supports both Windows (x64 calling convention) and Linux (System V AMD64 ABI).
   We emit raw asm text into a buffer, then flush to .asm file. *)

(* OS detection *)
let is_windows = Sys.os_type = "Win32" || Sys.os_type = "Cygwin"
let is_linux = Sys.os_type = "Unix"

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
  func_ret : (string, valtype) Hashtbl.t;
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
  func_ret = Hashtbl.create 16;
}

(* append one line of asm to the output buffer *)
let asm cg s = Buffer.add_string cg.buf s; Buffer.add_char cg.buf '\n'
let asmf cg fmt = Printf.ksprintf (asm cg) fmt

(* Platform-specific helper: emit print for an integer *)
let emit_print_int cg =
  if is_linux then begin
    (* Linux: convert int to string, then write syscall *)
    asm cg "    push rax";
    asm cg "    mov rdi, 64";
    asm cg "    call _coco_alloc";
    asm cg "    pop rdx";
    asm cg "    push rax";
    (* sprintf(buf, fmt, value) -> rdi=buf, rsi=fmt, rdx=value *)
    asm cg "    mov rdi, rax";
    asm cg "    lea rsi, [rel fmt_int_bare]";
    (* rdx already has the value *)
    asm cg "    xor rax, rax";
    asm cg "    call sprintf";
    asm cg "    pop rdi";
    (* strlen to get length *)
    asm cg "    push rdi";
    asm cg "    call strlen";
    asm cg "    mov rdx, rax";
    asm cg "    pop rdi";
    (* write(1, buf, len) *)
    asm cg "    mov rax, 1";
    asm cg "    mov rsi, rdi";
    asm cg "    mov rdi, 1";
    asm cg "    syscall";
    (* write newline *)
    asm cg "    mov rax, 1";
    asm cg "    mov rdi, 1";
    asm cg "    lea rsi, [rel newline]";
    asm cg "    mov rdx, 1";
    asm cg "    syscall"
  end else begin
    (* Windows: printf *)
    asm cg "    mov rdx, rax";
    asm cg "    lea rcx, [rel fmt_int]";
    asm cg "    sub rsp, 32";
    asm cg "    call printf";
    asm cg "    add rsp, 32"
  end

(* Platform-specific helper: emit print for a string *)
let emit_print_str cg =
  if is_linux then begin
    (* Linux: strlen + write syscall *)
    asm cg "    push rax";
    asm cg "    mov rdi, rax";
    asm cg "    call strlen";
    asm cg "    mov rdx, rax";
    asm cg "    pop rsi";
    asm cg "    mov rax, 1";
    asm cg "    mov rdi, 1";
    asm cg "    syscall";
    (* write newline *)
    asm cg "    mov rax, 1";
    asm cg "    mov rdi, 1";
    asm cg "    lea rsi, [rel newline]";
    asm cg "    mov rdx, 1";
    asm cg "    syscall"
  end else begin
    (* Windows: printf *)
    asm cg "    mov rdx, rax";
    asm cg "    lea rcx, [rel fmt_str]";
    asm cg "    sub rsp, 32";
    asm cg "    call printf";
    asm cg "    add rsp, 32"
  end

(* Platform-specific helper: emit print for a float *)
let emit_print_float cg =
  if is_linux then begin
    (* Linux: sprintf to buffer, then write syscall *)
    asm cg "    movq xmm0, rax";
    asm cg "    push rax";
    asm cg "    mov rdi, 64";
    asm cg "    call _coco_alloc";
    asm cg "    pop rdx";
    asm cg "    movq xmm0, rdx";
    asm cg "    push rax";
    asm cg "    lea rsi, [rel fmt_float_bare]";
    asm cg "    mov rdi, rax";
    asm cg "    mov rax, 1";
    asm cg "    call sprintf";
    asm cg "    pop rdi";
    (* strlen *)
    asm cg "    push rdi";
    asm cg "    call strlen";
    asm cg "    mov rdx, rax";
    asm cg "    pop rdi";
    (* write *)
    asm cg "    mov rax, 1";
    asm cg "    mov rsi, rdi";
    asm cg "    mov rdi, 1";
    asm cg "    syscall";
    (* write newline *)
    asm cg "    mov rax, 1";
    asm cg "    mov rdi, 1";
    asm cg "    lea rsi, [rel newline]";
    asm cg "    mov rdx, 1";
    asm cg "    syscall"
  end else begin
    (* Windows: printf *)
    asm cg "    movq xmm1, rax";
    asm cg "    mov rdx, rax";
    asm cg "    lea rcx, [rel fmt_float]";
    asm cg "    sub rsp, 32";
    asm cg "    call printf";
    asm cg "    add rsp, 32"
  end

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
  | Ast.Call ("tofloat", _) -> TFloat
  | Ast.Call ("len", _) -> TInt
  | Ast.Call ("sqrt", _) -> TFloat
  | Ast.Call ("floor", _) -> TInt
  | Ast.Call ("ceil", _) -> TInt
  | Ast.Call ("pow", _) -> TFloat
  | Ast.Call ("sin", _) -> TFloat
  | Ast.Call ("cos", _) -> TFloat
  | Ast.Call ("tan", _) -> TFloat
  | Ast.Call ("fmod", _) -> TFloat
  | Ast.Call ("substr", _) -> TStr
  | Ast.Call ("char_at", _) -> TStr
  | Ast.Call ("char_code", _) -> TInt
  | Ast.Call ("from_char_code", _) -> TStr
  | Ast.Call ("index_of", _) -> TInt
  | Ast.Call ("read_file", _) -> TStr
  | Ast.Call ("write_file", _) -> TInt
  | Ast.Call ("append_file", _) -> TInt
  | Ast.Call ("file_exists", _) -> TInt
  | Ast.Call ("trim", _) -> TStr
  | Ast.Call ("upper", _) -> TStr
  | Ast.Call ("lower", _) -> TStr
  | Ast.Call ("replace", _) -> TStr
  | Ast.Call ("starts_with", _) -> TInt
  | Ast.Call ("ends_with", _) -> TInt
  | Ast.Call (name, _) when Hashtbl.mem Class.classes name -> TObj name
  | Ast.Call (name, _) when Hashtbl.mem cg.func_ret name -> Hashtbl.find cg.func_ret name
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
  let builtins = ["print";"exit";"halt";"exec";"input";"len";"tostring";"toint";"push";"pop";
    "sqrt";"floor";"ceil";"pow";"sin";"cos";"tan";"fmod";
    "substr";"char_at";"char_code";"from_char_code";"index_of";"tofloat"] in
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
    let lty = infer_type cg left in
    compile_expr cg left;
    asm cg "    push rax";
    compile_expr cg right;
    asm cg "    mov rcx, rax";
    asm cg "    pop rax";
    (match op, lty with
     | (Ast.Eq | Ast.Neq), TStr ->
       asm cg "    push rcx";
       asm cg "    push rax";
       asm cg "    mov rdx, rcx";
       asm cg "    mov rcx, rax";
       asm cg "    mov rbx, rsp";
       asm cg "    and rsp, -16";
       asm cg "    sub rsp, 32";
       asm cg "    call strcmp";
       asm cg "    mov rsp, rbx";
       asm cg "    add rsp, 16";
       if op = Ast.Eq then begin
         asm cg "    cmp rax, 0";
         asm cg "    sete al";
         asm cg "    movzx rax, al"
       end else begin
         asm cg "    cmp rax, 0";
         asm cg "    setne al";
         asm cg "    movzx rax, al"
       end
     | _ -> emit_binop cg op)
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
    let size = (n + 1) * 8 in
    if is_linux then begin
      asmf cg "    mov rdi, %d" size;
      asm cg "    call _coco_alloc"
    end else begin
      asmf cg "    mov rcx, %d" size;
      asm cg "    sub rsp, 32";
      asm cg "    call _coco_alloc";
      asm cg "    add rsp, 32"
    end;
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
    if is_linux then begin
      asm cg "    mov rdi, 1024";
      asm cg "    call _coco_alloc"
    end else begin
      asm cg "    sub rsp, 32";  (* shadow space *)
      asm cg "    mov rcx, 1024";
      asm cg "    call _coco_alloc";
      asm cg "    add rsp, 32"
    end;
    asm cg "    pop r8";         (* left str *)
    asm cg "    pop r9";         (* right str *)
    if is_linux then begin
      asm cg "    mov rdi, rax";   (* buffer *)
      asm cg "    push rax";       (* save buffer ptr for result *)
      asm cg "    lea rsi, [rel fmt_concat]";
      asm cg "    mov rdx, r8";
      asm cg "    mov rcx, r9";
      asm cg "    xor rax, rax";
      asm cg "    call sprintf";
      asm cg "    pop rax"         (* buffer ptr is our result *)
    end else begin
      asm cg "    mov rcx, rax";   (* buffer *)
      asm cg "    push rax";       (* save buffer ptr for result *)
      asm cg "    lea rdx, [rel fmt_concat]";
      asm cg "    sub rsp, 32";
      asm cg "    call sprintf";
      asm cg "    add rsp, 32";
      asm cg "    pop rax"         (* buffer ptr is our result *)
    end

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
    func_ret = cg.func_ret;
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
    let size = ncap * 8 in
    if is_linux then begin
      asmf cg "    mov rdi, %d" size;
      asm cg "    call _coco_alloc"
    end else begin
      asmf cg "    mov rcx, %d" size;
      asm cg "    sub rsp, 32";
      asm cg "    call _coco_alloc";
      asm cg "    add rsp, 32"
    end;
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
  if is_linux then begin
    asm cg "    mov rdi, 16";
    asm cg "    call _coco_alloc"
  end else begin
    asm cg "    mov rcx, 16";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    add rsp, 32"
  end;
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
  if is_linux then begin
    asm cg "    mov rdi, 256";
    asm cg "    call _coco_alloc"
  end else begin
    asm cg "    mov rcx, 256";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    add rsp, 32"
  end;
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
    if is_linux then begin
      let regs = [| "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9" |] in
      for i = 0 to min n 6 - 1 do
        asmf cg "    mov %s, [rsp+%d]" regs.(i) (i * 8)
      done;
      asmf cg "    call %s" (mangle (cls_name ^ "_init"));
      asmf cg "    add rsp, %d" (n * 8)
    end else begin
      let regs = [| "rcx"; "rdx"; "r8"; "r9" |] in
      let shadow = max n 4 * 8 in
      asmf cg "    sub rsp, %d" shadow;
      for i = 0 to n - 1 do
        if i < 4 then
          asmf cg "    mov %s, [rsp+%d]" regs.(i) (shadow + i * 8)
      done;
      asmf cg "    call %s" (mangle (cls_name ^ "_init"));
      asmf cg "    add rsp, %d" (shadow + n * 8)
    end;
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
  else if name = "sqrt" then
    builtin_sqrt cg args
  else if name = "floor" then
    builtin_floor cg args
  else if name = "ceil" then
    builtin_ceil cg args
  else if name = "pow" then
    builtin_pow cg args
  else if name = "sin" then
    builtin_sin cg args
  else if name = "cos" then
    builtin_cos cg args
  else if name = "tan" then
    builtin_tan cg args
  else if name = "fmod" then
    builtin_fmod cg args
  else if name = "tofloat" then
    builtin_tofloat cg args
  else if name = "substr" then
    builtin_substr cg args
  else if name = "char_at" then
    builtin_char_at cg args
  else if name = "char_code" then
    builtin_char_code cg args
  else if name = "from_char_code" then
    builtin_from_char_code cg args
  else if name = "index_of" then
    builtin_index_of cg args
  else if name = "read_file" then
    builtin_read_file cg args
  else if name = "write_file" then
    builtin_write_file cg args
  else if name = "append_file" then
    builtin_append_file cg args
  else if name = "file_exists" then
    builtin_file_exists cg args
  else if name = "split" then
    builtin_split cg args
  else if name = "trim" then
    builtin_trim cg args
  else if name = "replace" then
    builtin_replace cg args
  else if name = "upper" then
    builtin_upper cg args
  else if name = "lower" then
    builtin_lower cg args
  else if name = "starts_with" then
    builtin_starts_with cg args
  else if name = "ends_with" then
    builtin_ends_with cg args
  else if name = "map" then
    builtin_map cg args
  else if name = "filter" then
    builtin_filter cg args
  else if name = "sort" then
    builtin_sort cg args
  else begin
    let n = List.length args in
    (* evaluate all args and push to stack first —
       avoid clobbering args in nested calls, learned this the hard way with clamp() *)
    List.iter (fun arg ->
      compile_expr cg arg;
      asm cg "    push rax"
    ) (List.rev args);
    (* pop into the right registers based on platform *)
    if is_linux then begin
      (* Linux System V AMD64: rdi, rsi, rdx, rcx, r8, r9 *)
      let regs = [| "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9" |] in
      for i = 0 to min n 6 - 1 do
        asmf cg "    pop %s" regs.(i)
      done;
      if n > 6 then
        asmf cg "    add rsp, %d" ((n - 6) * 8);
      asm cg "    xor rax, rax";  (* for varargs functions *)
      asmf cg "    call %s" (mangle name)
    end else begin
      (* Windows x64: rcx, rdx, r8, r9 + shadow space *)
      let regs = [| "rcx"; "rdx"; "r8"; "r9" |] in
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
  end

and builtin_print cg args =
  match args with
  | [] -> failwith "print takes at least 1 argument"
  | _ ->
    List.iter (fun arg ->
      let ty = infer_type cg arg in
      compile_expr cg arg;
      match ty with
      | TFloat -> emit_print_float cg
      | TStr -> emit_print_str cg
      | TInt | TObj _ | TClosure | TArray _ -> emit_print_int cg
    ) args

and builtin_exit cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    if is_linux then begin
      (* Linux: exit syscall #60 *)
      asm cg "    mov rdi, rax";
      asm cg "    mov rax, 60";
      asm cg "    syscall"
    end else begin
      (* Windows: call exit() *)
      asm cg "    sub rsp, 32";
      asm cg "    mov rcx, rax";
      asm cg "    call exit";
      asm cg "    add rsp, 32"
    end
  | [] ->
    if is_linux then begin
      asm cg "    xor rdi, rdi";
      asm cg "    mov rax, 60";
      asm cg "    syscall"
    end else begin
      asm cg "    sub rsp, 32";
      asm cg "    xor rcx, rcx";
      asm cg "    call exit";
      asm cg "    add rsp, 32"
    end
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
    if is_linux then begin
      (* Linux: allocate buffer, then read syscall *)
      asm cg "    mov rdi, 1024";
      asm cg "    call _coco_alloc";
      asm cg "    push rax";
      (* read(0, buf, 1023) *)
      asm cg "    mov rdi, 0";
      asm cg "    mov rsi, rax";
      asm cg "    mov rdx, 1023";
      asm cg "    xor rax, rax";
      asm cg "    syscall";
      (* remove trailing newline if present *)
      asm cg "    pop rdi";
      asm cg "    push rdi";
      asm cg "    add rdi, rax";
      asm cg "    dec rdi";
      asm cg "    cmp byte [rdi], 10";
      asm cg "    jne .no_newline";
      asm cg "    mov byte [rdi], 0";
      asm cg ".no_newline:";
      asm cg "    pop rax"
    end else begin
      (* Windows: scanf *)
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
    end
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

(* math builtins — all use C math library via xmm0/xmm1 *)
and builtin_sqrt cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    movq xmm0, rax";
    asm cg "    sqrtsd xmm0, xmm0";
    asm cg "    movq rax, xmm0"
  | _ -> failwith "sqrt takes 1 argument"

and builtin_floor cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    movq xmm0, rax";
    asm cg "    roundsd xmm0, xmm0, 1";
    asm cg "    cvttsd2si rax, xmm0"
  | _ -> failwith "floor takes 1 argument"

and builtin_ceil cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    movq xmm0, rax";
    asm cg "    roundsd xmm0, xmm0, 2";
    asm cg "    cvttsd2si rax, xmm0"
  | _ -> failwith "ceil takes 1 argument"

and builtin_pow cg args =
  match args with
  | [base_; exp_] ->
    compile_expr cg base_;
    asm cg "    push rax";
    compile_expr cg exp_;
    asm cg "    movq xmm1, rax";
    asm cg "    pop rcx";
    asm cg "    movq xmm0, rcx";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call pow";
    asm cg "    mov rsp, rbx";
    asm cg "    movq rax, xmm0"
  | _ -> failwith "pow takes 2 arguments (base, exponent)"

and builtin_sin cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    movq xmm0, rax";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call sin";
    asm cg "    mov rsp, rbx";
    asm cg "    movq rax, xmm0"
  | _ -> failwith "sin takes 1 argument"

and builtin_cos cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    movq xmm0, rax";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call cos";
    asm cg "    mov rsp, rbx";
    asm cg "    movq rax, xmm0"
  | _ -> failwith "cos takes 1 argument"

and builtin_tan cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    movq xmm0, rax";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call tan";
    asm cg "    mov rsp, rbx";
    asm cg "    movq rax, xmm0"
  | _ -> failwith "tan takes 1 argument"

and builtin_fmod cg args =
  match args with
  | [a; b] ->
    compile_expr cg a;
    asm cg "    push rax";
    compile_expr cg b;
    asm cg "    movq xmm1, rax";
    asm cg "    pop rcx";
    asm cg "    movq xmm0, rcx";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call fmod";
    asm cg "    mov rsp, rbx";
    asm cg "    movq rax, xmm0"
  | _ -> failwith "fmod takes 2 arguments"

and builtin_tofloat cg args =
  match args with
  | [arg] ->
    compile_expr cg arg;
    asm cg "    cvtsi2sd xmm0, rax";
    asm cg "    movq rax, xmm0"
  | _ -> failwith "tofloat takes 1 argument"

(* string builtins *)
and builtin_substr cg args =
  match args with
  | [str; start; length] ->
    compile_expr cg length;
    asm cg "    push rax";
    compile_expr cg start;
    asm cg "    push rax";
    compile_expr cg str;
    asm cg "    push rax";
    (* alloc len+1 bytes for result *)
    asm cg "    mov rcx, [rsp+16]";
    asm cg "    inc rcx";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    mov rsp, rbx";
    asm cg "    push rax";
    (* memcpy(dest, src+start, len) *)
    asm cg "    mov rcx, rax";
    asm cg "    mov rdx, [rsp+8]";
    asm cg "    add rdx, [rsp+16]";
    asm cg "    mov r8, [rsp+24]";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call memcpy";
    asm cg "    mov rsp, rbx";
    (* null-terminate *)
    asm cg "    pop rax";
    asm cg "    mov rcx, [rsp+16]";
    asm cg "    mov byte [rax+rcx], 0";
    asm cg "    add rsp, 24"
  | _ -> failwith "substr takes 3 arguments (string, start, length)"

and builtin_char_at cg args =
  match args with
  | [str; idx] ->
    compile_expr cg idx;
    asm cg "    push rax";
    compile_expr cg str;
    asm cg "    pop rcx";
    asm cg "    push rax";
    asm cg "    push rcx";
    (* alloc 2 bytes *)
    asm cg "    mov rcx, 2";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rcx";
    asm cg "    pop rdx";
    asm cg "    movzx r8, byte [rdx+rcx]";
    asm cg "    mov [rax], r8b";
    asm cg "    mov byte [rax+1], 0"
  | _ -> failwith "char_at takes 2 arguments (string, index)"

and builtin_char_code cg args =
  match args with
  | [str] ->
    compile_expr cg str;
    asm cg "    movzx rax, byte [rax]"
  | _ -> failwith "char_code takes 1 argument (string)"

and builtin_from_char_code cg args =
  match args with
  | [code] ->
    compile_expr cg code;
    asm cg "    push rax";
    asm cg "    mov rcx, 2";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rcx";
    asm cg "    mov [rax], cl";
    asm cg "    mov byte [rax+1], 0"
  | _ -> failwith "from_char_code takes 1 argument (char code)"

and builtin_index_of cg args =
  match args with
  | [haystack; needle] ->
    compile_expr cg needle;
    asm cg "    push rax";
    compile_expr cg haystack;
    asm cg "    push rax";
    (* strstr(haystack, needle) *)
    asm cg "    mov rcx, rax";
    asm cg "    mov rdx, [rsp+8]";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call strstr";
    asm cg "    mov rsp, rbx";
    asm cg "    test rax, rax";
    let not_found = next_label cg "indexof_nf" in
    let done_ = next_label cg "indexof_done" in
    asmf cg "    jz %s" not_found;
    asm cg "    pop rcx";
    asm cg "    sub rax, rcx";
    asm cg "    add rsp, 8";
    asmf cg "    jmp %s" done_;
    asmf cg "%s:" not_found;
    asm cg "    add rsp, 16";
    asm cg "    mov rax, -1";
    asmf cg "%s:" done_
  | _ -> failwith "index_of takes 2 arguments (haystack, needle)"

(* File I/O builtins *)
and builtin_read_file cg args =
  match args with
  | [path] ->
    compile_expr cg path;
    if is_linux then begin
      asm cg "    mov rdi, rax";
      asm cg "    lea rsi, [rel mode_r]";
      asm cg "    call fopen";
      asm cg "    test rax, rax";
      let err_lbl = next_label cg "read_err" in
      let ok_lbl = next_label cg "read_ok" in
      asmf cg "    jz %s" err_lbl;
      asm cg "    push rax";  (* save file ptr *)
      (* fseek(f, 0, SEEK_END) *)
      asm cg "    mov rdi, rax";
      asm cg "    xor rsi, rsi";
      asm cg "    mov rdx, 2";
      asm cg "    call fseek";
      (* ftell(f) *)
      asm cg "    mov rdi, [rsp]";
      asm cg "    call ftell";
      asm cg "    push rax";  (* save size *)
      (* fseek(f, 0, SEEK_SET) *)
      asm cg "    mov rdi, [rsp+8]";
      asm cg "    xor rsi, rsi";
      asm cg "    xor rdx, rdx";
      asm cg "    call fseek";
      (* malloc(size + 1) *)
      asm cg "    mov rdi, [rsp]";
      asm cg "    inc rdi";
      asm cg "    call malloc";
      asm cg "    push rax";  (* save buffer *)
      (* fread(buf, 1, size, f) *)
      asm cg "    mov rdi, rax";
      asm cg "    mov rsi, 1";
      asm cg "    mov rdx, [rsp+8]";
      asm cg "    mov rcx, [rsp+16]";
      asm cg "    call fread";
      (* null terminate *)
      asm cg "    mov rax, [rsp]";
      asm cg "    mov rcx, [rsp+8]";
      asm cg "    mov byte [rax+rcx], 0";
      (* fclose(f) *)
      asm cg "    mov rdi, [rsp+16]";
      asm cg "    call fclose";
      asm cg "    pop rax";  (* buffer *)
      asm cg "    add rsp, 16";
      asmf cg "    jmp %s" ok_lbl;
      asmf cg "%s:" err_lbl;
      asm cg "    xor rax, rax";  (* return nil on error *)
      asmf cg "%s:" ok_lbl
    end else begin
      asm cg "    mov rcx, rax";
      asm cg "    lea rdx, [rel mode_r]";
      asm cg "    sub rsp, 32";
      asm cg "    call fopen";
      asm cg "    add rsp, 32";
      asm cg "    test rax, rax";
      let err_lbl = next_label cg "read_err" in
      let ok_lbl = next_label cg "read_ok" in
      asmf cg "    jz %s" err_lbl;
      asm cg "    push rax";
      asm cg "    mov rcx, rax";
      asm cg "    xor rdx, rdx";
      asm cg "    mov r8, 2";
      asm cg "    sub rsp, 32";
      asm cg "    call fseek";
      asm cg "    add rsp, 32";
      asm cg "    mov rcx, [rsp]";
      asm cg "    sub rsp, 32";
      asm cg "    call ftell";
      asm cg "    add rsp, 32";
      asm cg "    push rax";
      asm cg "    mov rcx, [rsp+8]";
      asm cg "    xor rdx, rdx";
      asm cg "    xor r8, r8";
      asm cg "    sub rsp, 32";
      asm cg "    call fseek";
      asm cg "    add rsp, 32";
      asm cg "    mov rcx, [rsp]";
      asm cg "    inc rcx";
      asm cg "    sub rsp, 32";
      asm cg "    call malloc";
      asm cg "    add rsp, 32";
      asm cg "    push rax";
      asm cg "    mov rcx, rax";
      asm cg "    mov rdx, 1";
      asm cg "    mov r8, [rsp+8]";
      asm cg "    mov r9, [rsp+16]";
      asm cg "    sub rsp, 32";
      asm cg "    call fread";
      asm cg "    add rsp, 32";
      asm cg "    mov rax, [rsp]";
      asm cg "    mov rcx, [rsp+8]";
      asm cg "    mov byte [rax+rcx], 0";
      asm cg "    mov rcx, [rsp+16]";
      asm cg "    sub rsp, 32";
      asm cg "    call fclose";
      asm cg "    add rsp, 32";
      asm cg "    pop rax";
      asm cg "    add rsp, 16";
      asmf cg "    jmp %s" ok_lbl;
      asmf cg "%s:" err_lbl;
      asm cg "    xor rax, rax";
      asmf cg "%s:" ok_lbl
    end
  | _ -> failwith "read_file takes 1 argument (path)"

and builtin_write_file cg args =
  match args with
  | [path; content] ->
    compile_expr cg content;
    asm cg "    push rax";
    compile_expr cg path;
    if is_linux then begin
      asm cg "    mov rdi, rax";
      asm cg "    lea rsi, [rel mode_w]";
      asm cg "    call fopen";
      asm cg "    test rax, rax";
      let err_lbl = next_label cg "write_err" in
      let ok_lbl = next_label cg "write_ok" in
      asmf cg "    jz %s" err_lbl;
      asm cg "    mov rdi, [rsp]";
      asm cg "    mov rsi, rax";
      asm cg "    call fputs";
      asm cg "    pop rdx";
      asm cg "    push rax";
      asm cg "    mov rdi, rsi";
      asm cg "    call fclose";
      asm cg "    pop rax";
      asm cg "    mov rax, 1";  (* return 1 on success *)
      asmf cg "    jmp %s" ok_lbl;
      asmf cg "%s:" err_lbl;
      asm cg "    add rsp, 8";
      asm cg "    xor rax, rax";  (* return 0 on error *)
      asmf cg "%s:" ok_lbl
    end else begin
      asm cg "    mov rcx, rax";
      asm cg "    lea rdx, [rel mode_w]";
      asm cg "    sub rsp, 32";
      asm cg "    call fopen";
      asm cg "    add rsp, 32";
      asm cg "    test rax, rax";
      let err_lbl = next_label cg "write_err" in
      let ok_lbl = next_label cg "write_ok" in
      asmf cg "    jz %s" err_lbl;
      asm cg "    push rax";
      asm cg "    mov rcx, [rsp+8]";
      asm cg "    mov rdx, rax";
      asm cg "    sub rsp, 32";
      asm cg "    call fputs";
      asm cg "    add rsp, 32";
      asm cg "    mov rcx, [rsp]";
      asm cg "    sub rsp, 32";
      asm cg "    call fclose";
      asm cg "    add rsp, 32";
      asm cg "    add rsp, 16";
      asm cg "    mov rax, 1";
      asmf cg "    jmp %s" ok_lbl;
      asmf cg "%s:" err_lbl;
      asm cg "    add rsp, 8";
      asm cg "    xor rax, rax";
      asmf cg "%s:" ok_lbl
    end
  | _ -> failwith "write_file takes 2 arguments (path, content)"

and builtin_append_file cg args =
  match args with
  | [path; content] ->
    compile_expr cg content;
    asm cg "    push rax";
    compile_expr cg path;
    if is_linux then begin
      asm cg "    mov rdi, rax";
      asm cg "    lea rsi, [rel mode_a]";
      asm cg "    call fopen";
      asm cg "    test rax, rax";
      let err_lbl = next_label cg "append_err" in
      let ok_lbl = next_label cg "append_ok" in
      asmf cg "    jz %s" err_lbl;
      asm cg "    mov rdi, [rsp]";
      asm cg "    mov rsi, rax";
      asm cg "    call fputs";
      asm cg "    pop rdx";
      asm cg "    push rax";
      asm cg "    mov rdi, rsi";
      asm cg "    call fclose";
      asm cg "    pop rax";
      asm cg "    mov rax, 1";
      asmf cg "    jmp %s" ok_lbl;
      asmf cg "%s:" err_lbl;
      asm cg "    add rsp, 8";
      asm cg "    xor rax, rax";
      asmf cg "%s:" ok_lbl
    end else begin
      asm cg "    mov rcx, rax";
      asm cg "    lea rdx, [rel mode_a]";
      asm cg "    sub rsp, 32";
      asm cg "    call fopen";
      asm cg "    add rsp, 32";
      asm cg "    test rax, rax";
      let err_lbl = next_label cg "append_err" in
      let ok_lbl = next_label cg "append_ok" in
      asmf cg "    jz %s" err_lbl;
      asm cg "    push rax";
      asm cg "    mov rcx, [rsp+8]";
      asm cg "    mov rdx, rax";
      asm cg "    sub rsp, 32";
      asm cg "    call fputs";
      asm cg "    add rsp, 32";
      asm cg "    mov rcx, [rsp]";
      asm cg "    sub rsp, 32";
      asm cg "    call fclose";
      asm cg "    add rsp, 32";
      asm cg "    add rsp, 16";
      asm cg "    mov rax, 1";
      asmf cg "    jmp %s" ok_lbl;
      asmf cg "%s:" err_lbl;
      asm cg "    add rsp, 8";
      asm cg "    xor rax, rax";
      asmf cg "%s:" ok_lbl
    end
  | _ -> failwith "append_file takes 2 arguments (path, content)"

and builtin_file_exists cg args =
  match args with
  | [path] ->
    compile_expr cg path;
    if is_linux then begin
      asm cg "    mov rdi, rax";
      asm cg "    xor rsi, rsi";
      asm cg "    call access";
      asm cg "    test rax, rax";
      asm cg "    setz al";
      asm cg "    movzx rax, al"
    end else begin
      asm cg "    mov rcx, rax";
      asm cg "    xor rdx, rdx";
      asm cg "    sub rsp, 32";
      asm cg "    call _access";
      asm cg "    add rsp, 32";
      asm cg "    test rax, rax";
      asm cg "    setz al";
      asm cg "    movzx rax, al"
    end
  | _ -> failwith "file_exists takes 1 argument (path)"

(* String utility builtins *)
and builtin_split cg args =
  match args with
  | [str; delim] ->
    (* Simple implementation: returns array of substrings *)
    compile_expr cg delim;
    asm cg "    push rax";
    compile_expr cg str;
    asm cg "    push rax";
    (* Count occurrences to know array size *)
    if is_linux then begin
      asm cg "    mov rdi, 256";
      asm cg "    call _coco_alloc"
    end else begin
      asm cg "    mov rcx, 256";
      asm cg "    sub rsp, 32";
      asm cg "    call _coco_alloc";
      asm cg "    add rsp, 32"
    end;
    asm cg "    mov qword [rax], 0";  (* placeholder length *)
    asm cg "    add rax, 8";
    asm cg "    add rsp, 16"
  | _ -> failwith "split takes 2 arguments (string, delimiter)"

and builtin_trim cg args =
  match args with
  | [str] ->
    compile_expr cg str;
    asm cg "    push rax";
    (* Skip leading whitespace *)
    let start_loop = next_label cg "trim_start" in
    asmf cg "%s:" start_loop;
    asm cg "    movzx rcx, byte [rax]";
    asm cg "    cmp rcx, 32";  (* space *)
    asm cg "    je .trim_skip";
    asm cg "    cmp rcx, 9";   (* tab *)
    asm cg "    je .trim_skip";
    asm cg "    cmp rcx, 10";  (* newline *)
    asm cg "    je .trim_skip";
    asm cg "    cmp rcx, 13";  (* carriage return *)
    asm cg "    jne .trim_start_done";
    asm cg ".trim_skip:";
    asm cg "    inc rax";
    asmf cg "    jmp %s" start_loop;
    asm cg ".trim_start_done:";
    asm cg "    push rax";
    (* Find end *)
    asm cg "    mov rcx, rax";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call strlen";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rdi";
    asm cg "    add rax, rdi";
    asm cg "    dec rax";
    (* Trim trailing whitespace *)
    let end_loop = next_label cg "trim_end" in
    asmf cg "%s:" end_loop;
    asm cg "    cmp rax, rdi";
    asm cg "    jl .trim_end_done";
    asm cg "    movzx rcx, byte [rax]";
    asm cg "    cmp rcx, 32";
    asm cg "    je .trim_end_skip";
    asm cg "    cmp rcx, 9";
    asm cg "    je .trim_end_skip";
    asm cg "    cmp rcx, 10";
    asm cg "    je .trim_end_skip";
    asm cg "    cmp rcx, 13";
    asm cg "    jne .trim_end_done";
    asm cg ".trim_end_skip:";
    asm cg "    dec rax";
    asmf cg "    jmp %s" end_loop;
    asm cg ".trim_end_done:";
    asm cg "    sub rax, rdi";
    asm cg "    inc rax";
    asm cg "    push rax";
    asm cg "    push rdi";
    (* Allocate result *)
    asm cg "    mov rcx, [rsp]";
    asm cg "    inc rcx";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rsi";
    asm cg "    pop r8";
    asm cg "    push rax";
    (* Copy *)
    asm cg "    mov rcx, rax";
    asm cg "    mov rdx, rsi";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call memcpy";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rax";
    asm cg "    mov rcx, r8";
    asm cg "    mov byte [rax+rcx], 0";
    asm cg "    add rsp, 8"
  | _ -> failwith "trim takes 1 argument (string)"

and builtin_replace cg args =
  match args with
  | [str; _old_s; _new_s] ->
    (* Simplified: just return original for now, full impl would be complex *)
    compile_expr cg str
  | _ -> failwith "replace takes 3 arguments (string, old, new)"

and builtin_upper cg args =
  match args with
  | [str] ->
    compile_expr cg str;
    asm cg "    push rax";
    asm cg "    mov rcx, rax";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call strlen";
    asm cg "    mov rsp, rbx";
    asm cg "    inc rax";
    asm cg "    push rax";
    asm cg "    mov rcx, rax";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rcx";
    asm cg "    pop rsi";
    asm cg "    push rax";
    let loop_lbl = next_label cg "upper_loop" in
    asmf cg "%s:" loop_lbl;
    asm cg "    movzx rdx, byte [rsi]";
    asm cg "    test rdx, rdx";
    asm cg "    jz .upper_done";
    asm cg "    cmp rdx, 97";  (* 'a' *)
    asm cg "    jl .upper_copy";
    asm cg "    cmp rdx, 122"; (* 'z' *)
    asm cg "    jg .upper_copy";
    asm cg "    sub rdx, 32";  (* to uppercase *)
    asm cg ".upper_copy:";
    asm cg "    mov [rax], dl";
    asm cg "    inc rax";
    asm cg "    inc rsi";
    asmf cg "    jmp %s" loop_lbl;
    asm cg ".upper_done:";
    asm cg "    mov byte [rax], 0";
    asm cg "    pop rax"
  | _ -> failwith "upper takes 1 argument (string)"

and builtin_lower cg args =
  match args with
  | [str] ->
    compile_expr cg str;
    asm cg "    push rax";
    asm cg "    mov rcx, rax";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call strlen";
    asm cg "    mov rsp, rbx";
    asm cg "    inc rax";
    asm cg "    push rax";
    asm cg "    mov rcx, rax";
    asm cg "    mov rbx, rsp";
    asm cg "    and rsp, -16";
    asm cg "    sub rsp, 32";
    asm cg "    call _coco_alloc";
    asm cg "    mov rsp, rbx";
    asm cg "    pop rcx";
    asm cg "    pop rsi";
    asm cg "    push rax";
    let loop_lbl = next_label cg "lower_loop" in
    asmf cg "%s:" loop_lbl;
    asm cg "    movzx rdx, byte [rsi]";
    asm cg "    test rdx, rdx";
    asm cg "    jz .lower_done";
    asm cg "    cmp rdx, 65";  (* 'A' *)
    asm cg "    jl .lower_copy";
    asm cg "    cmp rdx, 90";  (* 'Z' *)
    asm cg "    jg .lower_copy";
    asm cg "    add rdx, 32";  (* to lowercase *)
    asm cg ".lower_copy:";
    asm cg "    mov [rax], dl";
    asm cg "    inc rax";
    asm cg "    inc rsi";
    asmf cg "    jmp %s" loop_lbl;
    asm cg ".lower_done:";
    asm cg "    mov byte [rax], 0";
    asm cg "    pop rax"
  | _ -> failwith "lower takes 1 argument (string)"

and builtin_starts_with cg args =
  match args with
  | [str; prefix] ->
    compile_expr cg prefix;
    asm cg "    push rax";
    compile_expr cg str;
    asm cg "    pop rdx";
    let loop_lbl = next_label cg "sw_loop" in
    let yes_lbl = next_label cg "sw_yes" in
    let no_lbl = next_label cg "sw_no" in
    asmf cg "%s:" loop_lbl;
    asm cg "    movzx rcx, byte [rdx]";
    asm cg "    test rcx, rcx";
    asmf cg "    jz %s" yes_lbl;
    asm cg "    movzx r8, byte [rax]";
    asm cg "    cmp rcx, r8";
    asmf cg "    jne %s" no_lbl;
    asm cg "    inc rax";
    asm cg "    inc rdx";
    asmf cg "    jmp %s" loop_lbl;
    asmf cg "%s:" yes_lbl;
    asm cg "    mov rax, 1";
    let done_lbl = next_label cg "sw_done" in
    asmf cg "    jmp %s" done_lbl;
    asmf cg "%s:" no_lbl;
    asm cg "    xor rax, rax";
    asmf cg "%s:" done_lbl
  | _ -> failwith "starts_with takes 2 arguments (string, prefix)"

and builtin_ends_with cg args =
  match args with
  | [str; suffix] ->
    compile_expr cg suffix;
    asm cg "    push rax";
    compile_expr cg str;
    asm cg "    push rax";
    (* Get lengths *)
    if is_linux then begin
      asm cg "    mov rdi, rax";
      asm cg "    call strlen";
      asm cg "    push rax";
      asm cg "    mov rdi, [rsp+16]";
      asm cg "    call strlen"
    end else begin
      asm cg "    mov rcx, rax";
      asm cg "    sub rsp, 32";
      asm cg "    call strlen";
      asm cg "    add rsp, 32";
      asm cg "    push rax";
      asm cg "    mov rcx, [rsp+16]";
      asm cg "    sub rsp, 32";
      asm cg "    call strlen";
      asm cg "    add rsp, 32"
    end;
    asm cg "    pop rcx";  (* str len *)
    asm cg "    cmp rax, rcx";
    let no_lbl = next_label cg "ew_no" in
    let yes_lbl = next_label cg "ew_yes" in
    asmf cg "    jg %s" no_lbl;
    asm cg "    pop rdi";  (* str *)
    asm cg "    add rdi, rcx";
    asm cg "    sub rdi, rax";
    asm cg "    pop rsi";  (* suffix *)
    if is_linux then begin
      asm cg "    call strcmp"
    end else begin
      asm cg "    mov rcx, rdi";
      asm cg "    mov rdx, rsi";
      asm cg "    sub rsp, 32";
      asm cg "    call strcmp";
      asm cg "    add rsp, 32"
    end;
    asm cg "    test rax, rax";
    asm cg "    setz al";
    asm cg "    movzx rax, al";
    let done_lbl = next_label cg "ew_done" in
    asmf cg "    jmp %s" done_lbl;
    asmf cg "%s:" no_lbl;
    asm cg "    add rsp, 16";
    asm cg "    xor rax, rax";
    asmf cg "%s:" yes_lbl;
    asmf cg "%s:" done_lbl
  | _ -> failwith "ends_with takes 2 arguments (string, suffix)"

(* Array utility builtins - simplified implementations *)
and builtin_map cg args =
  match args with
  | [arr; _func] ->
    (* For now, just return the array - full impl needs closure calling *)
    compile_expr cg arr
  | _ -> failwith "map takes 2 arguments (array, function)"

and builtin_filter cg args =
  match args with
  | [arr; _func] ->
    compile_expr cg arr
  | _ -> failwith "filter takes 2 arguments (array, function)"

and builtin_sort cg args =
  match args with
  | [arr] ->
    compile_expr cg arr
  | _ -> failwith "sort takes 1 argument (array)"

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
  List.iter (fun p ->
    let key = f.name ^ "." ^ p in
    match Hashtbl.find_opt cg.func_ret key with
    | Some t -> cg.type_env <- (p, t) :: cg.type_env
    | None -> ()
  ) f.params;
  let mangled = mangle f.name in
  asmf cg "global %s" mangled;
  asmf cg "%s:" mangled;
  asm cg "    push rbp";
  asm cg "    mov rbp, rsp";
  (* bind parameters from registers to stack slots.
     Windows x64: rcx, rdx, r8, r9
     Linux System V AMD64: rdi, rsi, rdx, rcx, r8, r9 *)
  let regs = if is_linux then
    [| "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9" |]
  else
    [| "rcx"; "rdx"; "r8"; "r9" |] in
  List.iteri (fun i name ->
    alloc_local cg name;
    let off = lookup_var cg name in
    if i < Array.length regs then asmf cg "    mov [rbp-%d], %s" off regs.(i)
  ) f.params;
  List.iter (compile_stmt cg) f.body;
  (* default return 0 if control falls through *)
  if is_linux && f.name = "main" then begin
    (* On Linux, main should call exit syscall instead of returning *)
    asm cg "    xor rdi, rdi";
    asm cg "    mov rax, 60";
    asm cg "    syscall"
  end else begin
    asm cg "    xor rax, rax";
    asm cg "    mov rsp, rbp";
    asm cg "    pop rbp";
    asm cg "    ret"
  end;
  asm cg "";
  (* emit any lambda functions that were created inside this function *)
  List.iter (fun lam_code -> Buffer.add_string cg.buf lam_code) cg.pending_lambdas;
  cg.pending_lambdas <- []

(* encode a string for NASM db directives, emitting non-printable
   and special chars as raw byte values instead of inside quotes *)
let encode_nasm_string str =
  if String.length str = 0 then "0" (* empty string: just the null terminator *)
  else begin
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
  end

let preregister_func_types cg (prog : Ast.program) =
  let func_params : (string, string list) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (f : Ast.func_def) ->
    Hashtbl.replace func_params f.name f.params
  ) prog.functions;
  let rec scan_call_expr cg = function
    | Ast.Call (name, args) ->
      (match Hashtbl.find_opt func_params name with
       | Some params ->
         List.iteri (fun i arg ->
           if i < List.length params then begin
             let param = List.nth params i in
             let key = name ^ "." ^ param in
             let ty = infer_type cg arg in
             if ty <> TInt then
               Hashtbl.replace cg.func_ret key ty
           end
         ) args
       | None -> ());
      List.iter (scan_call_expr cg) args
    | Ast.BinOp (_, a, b) -> scan_call_expr cg a; scan_call_expr cg b
    | Ast.UnaryOp (_, e) -> scan_call_expr cg e
    | Ast.Index (a, b) -> scan_call_expr cg a; scan_call_expr cg b
    | Ast.TableLit es -> List.iter (scan_call_expr cg) es
    | Ast.FieldGet (e, _) -> scan_call_expr cg e
    | Ast.MethodCall (e, _, args) -> scan_call_expr cg e; List.iter (scan_call_expr cg) args
    | _ -> ()
  in
  let rec scan_call_stmt cg = function
    | Ast.LocalDecl (_, e) | Ast.Assign (_, e) | Ast.ExprStmt e | Ast.Return e ->
      scan_call_expr cg e
    | Ast.IfStmt (c, t, f) ->
      scan_call_expr cg c;
      List.iter (scan_call_stmt cg) t;
      List.iter (scan_call_stmt cg) f
    | Ast.WhileStmt (c, b) ->
      scan_call_expr cg c; List.iter (scan_call_stmt cg) b
    | Ast.ForStmt (_, s, e, b) ->
      scan_call_expr cg s; scan_call_expr cg e;
      List.iter (scan_call_stmt cg) b
    | Ast.ForEach (_, e, b) ->
      scan_call_expr cg e; List.iter (scan_call_stmt cg) b
    | _ -> ()
  in
  List.iter (fun (f : Ast.func_def) ->
    let local_cg = { cg with type_env = [] } in
    List.iter (fun p ->
      let key = f.name ^ "." ^ p in
      match Hashtbl.find_opt cg.func_ret key with
      | Some t -> local_cg.type_env <- (p, t) :: local_cg.type_env
      | None -> ()
    ) f.params;
    let scan_with_env stmt =
      (match stmt with
       | Ast.LocalDecl (name, e) ->
         let ty = infer_type local_cg e in
         local_cg.type_env <- (name, ty) :: local_cg.type_env
       | Ast.Assign (name, e) ->
         let ty = infer_type local_cg e in
         local_cg.type_env <- (name, ty) :: local_cg.type_env
       | _ -> ());
      scan_call_stmt local_cg stmt
    in
    List.iter scan_with_env f.body
  ) prog.functions;
  let scan_func cg (f : Ast.func_def) =
    let local_cg = { cg with type_env = [] } in
    List.iter (fun p ->
      let key = f.name ^ "." ^ p in
      let ty = match Hashtbl.find_opt cg.func_ret key with
        | Some t -> t | None -> TInt in
      local_cg.type_env <- (p, ty) :: local_cg.type_env
    ) f.params;
    let rec scan_stmt = function
      | Ast.LocalDecl (name, e) ->
        let ty = infer_type local_cg e in
        local_cg.type_env <- (name, ty) :: local_cg.type_env;
        None
      | Ast.Assign (name, e) ->
        let ty = infer_type local_cg e in
        local_cg.type_env <- (name, ty) :: local_cg.type_env;
        None
      | Ast.Return e -> Some (infer_type local_cg e)
      | Ast.IfStmt (_, t, f) ->
        let t_ty = List.find_map scan_stmt t in
        if t_ty <> None then t_ty
        else List.find_map scan_stmt f
      | Ast.WhileStmt (_, b) -> List.find_map scan_stmt b
      | Ast.ForStmt (_, _, _, b) -> List.find_map scan_stmt b
      | Ast.ForEach (_, _, b) -> List.find_map scan_stmt b
      | _ -> None
    in
    List.find_map scan_stmt f.body
  in
  List.iter (fun (f : Ast.func_def) ->
    match scan_func cg f with
    | Some ty -> Hashtbl.replace cg.func_ret f.name ty
    | None -> ()
  ) prog.functions

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
  (* external symbols we link against *)
  if is_linux then begin
    (* Linux: only need C library functions, no printf for basic print *)
    asm cg "extern sprintf";
    asm cg "extern strlen";
    asm cg "extern atoi";
    asm cg "extern sqrt";
    asm cg "extern floor";
    asm cg "extern ceil";
    asm cg "extern pow";
    asm cg "extern sin";
    asm cg "extern cos";
    asm cg "extern tan";
    asm cg "extern fmod";
    asm cg "extern strstr";
    asm cg "extern memcpy";
    asm cg "extern strcmp";
    asm cg "extern malloc";
    asm cg "extern free";
    asm cg "extern system";
    asm cg "extern fopen";
    asm cg "extern fclose";
    asm cg "extern fread";
    asm cg "extern fwrite";
    asm cg "extern fputs";
    asm cg "extern fseek";
    asm cg "extern ftell";
    asm cg "extern access"
  end else begin
    (* Windows: msvcrt / ucrt *)
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
    asm cg "extern sqrt";
    asm cg "extern floor";
    asm cg "extern ceil";
    asm cg "extern pow";
    asm cg "extern sin";
    asm cg "extern cos";
    asm cg "extern tan";
    asm cg "extern fmod";
    asm cg "extern strstr";
    asm cg "extern memcpy";
    asm cg "extern strcmp";
    asm cg "extern fopen";
    asm cg "extern fclose";
    asm cg "extern fread";
    asm cg "extern fwrite";
    asm cg "extern fputs";
    asm cg "extern fseek";
    asm cg "extern ftell";
    asm cg "extern _access"
  end;
  asm cg "";
  asm cg "section .text";
  asm cg "";
  Gc.emit_allocator (asm cg);
  preregister_func_types cg prog;
  preregister_field_types cg prog;
  (List.iter (compile_function cg) prog.functions;
  (* data section: format strings + interned literals *)
  asm cg "section .data";
  if is_linux then begin
    asm cg "    newline db 10";
    asm cg "    fmt_int_bare db \"%d\", 0";
    asm cg "    fmt_float_bare db \"%f\", 0";
    asm cg "    fmt_concat db \"%s%s\", 0";
    asm cg "    mode_r db \"r\", 0";
    asm cg "    mode_w db \"w\", 0";
    asm cg "    mode_a db \"a\", 0"
  end else begin
    asm cg "    fmt_int db \"%d\", 10, 0";
    asm cg "    fmt_str db \"%s\", 10, 0";
    asm cg "    fmt_concat db \"%s%s\", 0";
    asm cg "    fmt_input db \"%1023[^\", 10, \"]\", 0";
    asm cg "    fmt_float db \"%f\", 10, 0";
    asm cg "    fmt_int_bare db \"%d\", 0";
    asm cg "    mode_r db \"r\", 0";
    asm cg "    mode_w db \"w\", 0";
    asm cg "    mode_a db \"a\", 0"
  end;
  Gc.emit_arena_data (asm cg);
  List.iter (fun (str, label) ->
    asmf cg "    %s db %s, 0" label (encode_nasm_string str)
  ) cg.strtab;
  List.iter (fun (f, label) ->
    let s = Printf.sprintf "%.17g" f in
    let s = if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
      then s else s ^ ".0" in
    asmf cg "    %s dq %s" label s
  ) cg.ftab;
  Buffer.contents cg.buf : string)

(* backward compat alias — compiler.ml calls Codegen.gen_program *)
let gen_program = compile_program
