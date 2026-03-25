(* codegen.ml — CocoScript x86-64 backend
   Targets NASM + Windows x64 calling convention (rcx, rdx, r8, r9).
   We emit raw asm text into a buffer, then flush to .asm file.
   see: https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention *)

type valtype = TInt | TStr | TFloat

(* codegen state — threaded through everything *)
type context = {
  buf : Buffer.t;
  mutable lbl_seq : int;
  mutable locals : (string * int) list;
  mutable sp_off : int;
  mutable strtab : (string * string) list;
  mutable ftab : (float * string) list;
  mutable type_env : (string * valtype) list;
}

let create () = {
  buf = Buffer.create 4096;
  lbl_seq = 0;
  locals = [];
  sp_off = 0;
  strtab = [];
  ftab = [];
  type_env = [];
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

(* very rough type inference — just enough for print formatting.
   doesn't handle user-defined types yet, but works for the basics *)
let infer_type cg (expr : Ast.expr) =
  match expr with
  | Ast.StringLit _ -> TStr
  | Ast.FloatLit _ -> TFloat
  | Ast.BinOp (Concat, _, _) -> TStr
  | Ast.Call ("input", _) -> TStr
  | Ast.Ident name ->
    (match List.assoc_opt name cg.type_env with
     | Some t -> t
     | None -> TInt)
  | _ -> TInt

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
    asmf cg "    mov rcx, %d" (n * 8);
    asm cg "    sub rsp, 32"; (* shadow space per win64 abi *)
    asm cg "    call malloc";
    asm cg "    add rsp, 32";
    asm cg "    push rax";
    List.iteri (fun i elem ->
      compile_expr cg elem;
      asm cg "    pop rcx";
      asmf cg "    mov [rcx+%d], rax" (i * 8);
      asm cg "    push rcx"
    ) elems;
    asm cg "    pop rax"

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
    asm cg "    call malloc";
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

(* dispatch calls — builtins are handled inline, user fns go through
   the normal win64 calling convention path *)
and compile_call cg name args =
  if name = "print" then
    builtin_print cg args
  else if name = "exit" then
    builtin_exit cg args
  else if name = "halt" then
    builtin_halt cg args
  else if name = "exec" then
    builtin_exec cg args
  else if name = "input" then
    builtin_input cg args
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
  | [arg] ->
    let ty = infer_type cg arg in
    compile_expr cg arg;
    asm cg "    sub rsp, 32"; (* shadow space per win64 abi *)
    (match ty with
    | TFloat ->
      asm cg "    movq xmm1, rax";
      asm cg "    mov rdx, rax";
      asm cg "    lea rcx, [rel fmt_float]"
    | TStr ->
      asm cg "    mov rdx, rax";
      asm cg "    lea rcx, [rel fmt_str]"
    | TInt ->
      asm cg "    mov rdx, rax";
      asm cg "    lea rcx, [rel fmt_int]");
    asm cg "    call printf";
    asm cg "    add rsp, 32"
  | _ -> failwith "print takes exactly 1 argument"

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
    asm cg "    call malloc";
    asm cg "    add rsp, 32";
    asm cg "    push rax";
    asm cg "    lea rcx, [rel fmt_input]";
    asm cg "    mov rdx, rax";
    asm cg "    sub rsp, 32";
    asm cg "    call scanf";
    asm cg "    add rsp, 32";
    asm cg "    pop rax"
  | _ -> failwith "input takes no arguments"


let rec compile_stmt cg (stmt : Ast.stmt) =
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
    asmf cg "%s:" start_lbl;
    compile_expr cg cond;
    asm cg "    cmp rax, 0";
    asmf cg "    je near %s" end_lbl;
    List.iter (compile_stmt cg) body;
    asmf cg "    jmp near %s" start_lbl;
    asmf cg "%s:" end_lbl
  | Ast.ForStmt (name, start, finish, body) ->
    compile_expr cg start;
    alloc_local cg name;
    let off = lookup_var cg name in
    asmf cg "    mov [rbp-%d], rax" off;
    let start_lbl = next_label cg "for" in
    let end_lbl = next_label cg "endfor" in
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
    asmf cg "%s:" end_lbl
  | Ast.Return expr ->
    compile_expr cg expr;
    asm cg "    mov rsp, rbp";
    asm cg "    pop rbp";
    asm cg "    ret"
  | Ast.ExprStmt expr -> compile_expr cg expr
  | Ast.Include _ -> () (* handled earlier in the pipeline *)
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
  (* reset per-function state *)
  cg.locals <- [];
  cg.sp_off <- 0;
  cg.type_env <- [];
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
  asm cg ""

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
  asm cg "";
  asm cg "section .text";
  asm cg "";
  List.iter (compile_function cg) prog.functions;
  (* data section: format strings + interned literals *)
  asm cg "section .data";
  asm cg "    fmt_int db \"%d\", 10, 0";
  asm cg "    fmt_str db \"%s\", 10, 0";
  asm cg "    fmt_concat db \"%s%s\", 0";
  asm cg "    fmt_input db \"%s\", 0";
  asm cg "    fmt_float db \"%f\", 10, 0";
  List.iter (fun (str, label) ->
    asmf cg "    %s db \"%s\", 0" label str
  ) cg.strtab;
  List.iter (fun (f, label) ->
    asmf cg "    %s dq %.17g" label f
  ) cg.ftab;
  Buffer.contents cg.buf

(* backward compat alias — compiler.ml calls Codegen.gen_program *)
let gen_program = compile_program
