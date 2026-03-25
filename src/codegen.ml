(* x86-64 assembly generator targeting NASM + Windows *)

type valtype = TInt | TStr | TFloat

type context = {
  buf : Buffer.t;
  mutable label_count : int;
  mutable locals : (string * int) list;
  mutable stack_offset : int;
  mutable string_lits : (string * string) list;
  mutable float_lits : (float * string) list;
  mutable local_types : (string * valtype) list;
}

let create () = {
  buf = Buffer.create 4096;
  label_count = 0;
  locals = [];
  stack_offset = 0;
  string_lits = [];
  float_lits = [];
  local_types = [];
}

let emit ctx s = Buffer.add_string ctx.buf s; Buffer.add_char ctx.buf '\n'
let emitf ctx fmt = Printf.ksprintf (emit ctx) fmt

let fresh_label ctx prefix =
  ctx.label_count <- ctx.label_count + 1;
  Printf.sprintf "%s_%d" prefix ctx.label_count

let add_string_lit ctx str =
  match List.assoc_opt str ctx.string_lits with
  | Some label -> label
  | None ->
    let label = fresh_label ctx "str" in
    ctx.string_lits <- (str, label) :: ctx.string_lits;
    label

let add_float_lit ctx f =
  match List.assoc_opt f ctx.float_lits with
  | Some label -> label
  | None ->
    let label = fresh_label ctx "flt" in
    ctx.float_lits <- (f, label) :: ctx.float_lits;
    label

let push_local ctx name =
  ctx.stack_offset <- ctx.stack_offset + 8;
  ctx.locals <- (name, ctx.stack_offset) :: ctx.locals;
  emitf ctx "    sub rsp, 8"

let get_local ctx name =
  match List.assoc_opt name ctx.locals with
  | Some off -> off
  | None -> failwith (Printf.sprintf "undefined variable: %s" name)

let mangle name =
  if name = "main" then "main" else "_coco_" ^ name

let infer_type ctx (expr : Ast.expr) =
  match expr with
  | Ast.StringLit _ -> TStr
  | Ast.FloatLit _ -> TFloat
  | Ast.BinOp (Concat, _, _) -> TStr
  | Ast.Call ("input", _) -> TStr
  | Ast.Ident name ->
    (match List.assoc_opt name ctx.local_types with
     | Some t -> t
     | None -> TInt)
  | _ -> TInt

let rec gen_expr ctx (expr : Ast.expr) =
  match expr with
  | Ast.IntLit n ->
    emitf ctx "    mov rax, %d" n
  | Ast.BoolLit true ->
    emit ctx "    mov rax, 1"
  | Ast.BoolLit false ->
    emit ctx "    mov rax, 0"
  | Ast.Nil ->
    emit ctx "    xor rax, rax"
  | Ast.StringLit s ->
    let label = add_string_lit ctx s in
    emitf ctx "    lea rax, [rel %s]" label
  | Ast.Ident name ->
    let off = get_local ctx name in
    emitf ctx "    mov rax, [rbp-%d]" off
  | Ast.BinOp (op, left, right) ->
    gen_expr ctx left;
    emit ctx "    push rax";
    gen_expr ctx right;
    emit ctx "    mov rcx, rax";
    emit ctx "    pop rax";
    gen_binop ctx op
  | Ast.UnaryOp (Neg, e) ->
    gen_expr ctx e;
    emit ctx "    neg rax"
  | Ast.UnaryOp (Not, e) ->
    gen_expr ctx e;
    emit ctx "    cmp rax, 0";
    emit ctx "    sete al";
    emit ctx "    movzx rax, al"
  | Ast.Call (name, args) ->
    gen_call ctx name args
  | Ast.FloatLit f ->
    let label = add_float_lit ctx f in
    emitf ctx "    movsd xmm0, [rel %s]" label;
    emit ctx "    movq rax, xmm0"
  | Ast.Index (arr, idx) ->
    gen_expr ctx arr;
    emit ctx "    push rax";
    gen_expr ctx idx;
    emit ctx "    mov rcx, rax";
    emit ctx "    pop rax";
    emit ctx "    mov rax, [rax+rcx*8]"
  | Ast.TableLit elems ->
    let n = List.length elems in
    emitf ctx "    mov rcx, %d" (n * 8);
    emit ctx "    sub rsp, 32";
    emit ctx "    call malloc";
    emit ctx "    add rsp, 32";
    emit ctx "    push rax";
    List.iteri (fun i elem ->
      gen_expr ctx elem;
      emit ctx "    pop rcx";
      emitf ctx "    mov [rcx+%d], rax" (i * 8);
      emit ctx "    push rcx"
    ) elems;
    emit ctx "    pop rax"

and gen_binop ctx (op : Ast.binop) =
  match op with
  | Add -> emit ctx "    add rax, rcx"
  | Sub -> emit ctx "    sub rax, rcx"
  | Mul -> emit ctx "    imul rax, rcx"
  | Div -> emit ctx "    cqo"; emit ctx "    idiv rcx"
  | Mod -> emit ctx "    cqo"; emit ctx "    idiv rcx"; emit ctx "    mov rax, rdx"
  | Eq -> emit ctx "    cmp rax, rcx"; emit ctx "    sete al"; emit ctx "    movzx rax, al"
  | Neq -> emit ctx "    cmp rax, rcx"; emit ctx "    setne al"; emit ctx "    movzx rax, al"
  | Lt -> emit ctx "    cmp rax, rcx"; emit ctx "    setl al"; emit ctx "    movzx rax, al"
  | Gt -> emit ctx "    cmp rax, rcx"; emit ctx "    setg al"; emit ctx "    movzx rax, al"
  | Lte -> emit ctx "    cmp rax, rcx"; emit ctx "    setle al"; emit ctx "    movzx rax, al"
  | Gte -> emit ctx "    cmp rax, rcx"; emit ctx "    setge al"; emit ctx "    movzx rax, al"
  | And -> emit ctx "    and rax, rcx"
  | Or -> emit ctx "    or rax, rcx"
  | Concat ->
    (* rax = left str, rcx = right str *)
    emit ctx "    push rcx";
    emit ctx "    push rax";
    emit ctx "    sub rsp, 32";
    emit ctx "    mov rcx, 1024";
    emit ctx "    call malloc";
    emit ctx "    add rsp, 32";
    emit ctx "    pop r8";         (* left str *)
    emit ctx "    pop r9";         (* right str *)
    emit ctx "    mov rcx, rax";   (* buffer *)
    emit ctx "    push rax";       (* save buffer ptr for result *)
    emit ctx "    lea rdx, [rel fmt_concat]";
    emit ctx "    sub rsp, 32";
    emit ctx "    call sprintf";
    emit ctx "    add rsp, 32";
    emit ctx "    pop rax"         (* buffer ptr is our result *)

and gen_call ctx name args =
  (* Windows x64 calling convention: rcx, rdx, r8, r9, then stack *)
  if name = "print" then
    gen_print ctx args
  else if name = "exit" then
    gen_exit ctx args
  else if name = "halt" then
    gen_halt ctx args
  else if name = "exec" then
    gen_exec ctx args
  else if name = "input" then
    gen_input ctx args
  else begin
    let regs = [| "rcx"; "rdx"; "r8"; "r9" |] in
    let n = List.length args in
    (* evaluate all args and push to stack to avoid clobbering *)
    List.iter (fun arg ->
      gen_expr ctx arg;
      emit ctx "    push rax"
    ) (List.rev args);
    (* pop into registers *)
    let shadow = max n 4 * 8 in
    emitf ctx "    sub rsp, %d" shadow;
    for i = 0 to n - 1 do
      if i < 4 then begin
        emitf ctx "    mov %s, [rsp+%d]" regs.(i) (shadow + i * 8)
      end
    done;
    emitf ctx "    call %s" (mangle name);
    emitf ctx "    add rsp, %d" (shadow + n * 8)
  end

and gen_print ctx args =
  match args with
  | [arg] ->
    let ty = infer_type ctx arg in
    gen_expr ctx arg;
    emit ctx "    sub rsp, 32";
    (match ty with
    | TFloat ->
      emit ctx "    movq xmm1, rax";
      emit ctx "    mov rdx, rax";
      emit ctx "    lea rcx, [rel fmt_float]"
    | TStr ->
      emit ctx "    mov rdx, rax";
      emit ctx "    lea rcx, [rel fmt_str]"
    | TInt ->
      emit ctx "    mov rdx, rax";
      emit ctx "    lea rcx, [rel fmt_int]");
    emit ctx "    call printf";
    emit ctx "    add rsp, 32"
  | _ -> failwith "print takes exactly 1 argument"

and gen_exit ctx args =
  match args with
  | [arg] ->
    gen_expr ctx arg;
    emit ctx "    sub rsp, 32";
    emit ctx "    mov rcx, rax";
    emit ctx "    call exit";
    emit ctx "    add rsp, 32"
  | [] ->
    emit ctx "    sub rsp, 32";
    emit ctx "    xor rcx, rcx";
    emit ctx "    call exit";
    emit ctx "    add rsp, 32"
  | _ -> failwith "exit takes 0 or 1 argument"

and gen_halt ctx args =
  match args with
  | [] ->
    let label = add_string_lit ctx "pause" in
    (* align stack to 16 bytes before call *)
    emit ctx "    mov rbx, rsp";
    emit ctx "    and rsp, -16";
    emit ctx "    sub rsp, 32";
    emitf ctx "    lea rcx, [rel %s]" label;
    emit ctx "    call system";
    emit ctx "    mov rsp, rbx"
  | _ -> failwith "halt takes no arguments"

and gen_exec ctx args =
  match args with
  | [arg] ->
    gen_expr ctx arg;
    emit ctx "    mov rbx, rsp";
    emit ctx "    and rsp, -16";
    emit ctx "    sub rsp, 32";
    emit ctx "    mov rcx, rax";
    emit ctx "    call system";
    emit ctx "    mov rsp, rbx"
  | _ -> failwith "exec takes exactly 1 argument"

and gen_input ctx args =
  match args with
  | [] ->
    emit ctx "    mov rcx, 1024";
    emit ctx "    sub rsp, 32";
    emit ctx "    call malloc";
    emit ctx "    add rsp, 32";
    emit ctx "    push rax";
    emit ctx "    lea rcx, [rel fmt_input]";
    emit ctx "    mov rdx, rax";
    emit ctx "    sub rsp, 32";
    emit ctx "    call scanf";
    emit ctx "    add rsp, 32";
    emit ctx "    pop rax"
  | _ -> failwith "input takes no arguments"

let rec gen_stmt ctx (stmt : Ast.stmt) =
  match stmt with
  | Ast.LocalDecl (name, expr) ->
    let ty = infer_type ctx expr in
    ctx.local_types <- (name, ty) :: ctx.local_types;
    gen_expr ctx expr;
    push_local ctx name;
    let off = get_local ctx name in
    emitf ctx "    mov [rbp-%d], rax" off
  | Ast.Assign (name, expr) ->
    gen_expr ctx expr;
    let off = get_local ctx name in
    emitf ctx "    mov [rbp-%d], rax" off
  | Ast.IfStmt (cond, then_body, else_body) ->
    let else_label = fresh_label ctx "else" in
    let end_label = fresh_label ctx "endif" in
    gen_expr ctx cond;
    emit ctx "    cmp rax, 0";
    emitf ctx "    je near %s" else_label;
    List.iter (gen_stmt ctx) then_body;
    emitf ctx "    jmp near %s" end_label;
    emitf ctx "%s:" else_label;
    List.iter (gen_stmt ctx) else_body;
    emitf ctx "%s:" end_label
  | Ast.WhileStmt (cond, body) ->
    let start_label = fresh_label ctx "while" in
    let end_label = fresh_label ctx "endwhile" in
    emitf ctx "%s:" start_label;
    gen_expr ctx cond;
    emit ctx "    cmp rax, 0";
    emitf ctx "    je near %s" end_label;
    List.iter (gen_stmt ctx) body;
    emitf ctx "    jmp near %s" start_label;
    emitf ctx "%s:" end_label
  | Ast.ForStmt (name, start, finish, body) ->
    gen_expr ctx start;
    push_local ctx name;
    let off = get_local ctx name in
    emitf ctx "    mov [rbp-%d], rax" off;
    let start_label = fresh_label ctx "for" in
    let end_label = fresh_label ctx "endfor" in
    emitf ctx "%s:" start_label;
    emitf ctx "    mov rax, [rbp-%d]" off;
    emit ctx "    push rax";
    gen_expr ctx finish;
    emit ctx "    mov rcx, rax";
    emit ctx "    pop rax";
    emit ctx "    cmp rax, rcx";
    emitf ctx "    jg near %s" end_label;
    List.iter (gen_stmt ctx) body;
    emitf ctx "    mov rax, [rbp-%d]" off;
    emit ctx "    inc rax";
    emitf ctx "    mov [rbp-%d], rax" off;
    emitf ctx "    jmp near %s" start_label;
    emitf ctx "%s:" end_label
  | Ast.Return expr ->
    gen_expr ctx expr;
    emit ctx "    mov rsp, rbp";
    emit ctx "    pop rbp";
    emit ctx "    ret"
  | Ast.ExprStmt expr -> gen_expr ctx expr
  | Ast.Include _ -> ()
  | Ast.IndexAssign (arr, idx, value) ->
    gen_expr ctx arr;
    emit ctx "    push rax";
    gen_expr ctx idx;
    emit ctx "    push rax";
    gen_expr ctx value;
    emit ctx "    mov rdx, rax";
    emit ctx "    pop rcx";
    emit ctx "    pop rax";
    emit ctx "    mov [rax+rcx*8], rdx"

let gen_func ctx (f : Ast.func_def) =
  ctx.locals <- [];
  ctx.stack_offset <- 0;
  ctx.local_types <- [];
  let mangled = mangle f.name in
  emitf ctx "global %s" mangled;
  emitf ctx "%s:" mangled;
  emit ctx "    push rbp";
  emit ctx "    mov rbp, rsp";
  let regs = [| "rcx"; "rdx"; "r8"; "r9" |] in
  List.iteri (fun i name ->
    push_local ctx name;
    let off = get_local ctx name in
    if i < 4 then emitf ctx "    mov [rbp-%d], %s" off regs.(i)
  ) f.params;
  List.iter (gen_stmt ctx) f.body;
  emit ctx "    xor rax, rax";
  emit ctx "    mov rsp, rbp";
  emit ctx "    pop rbp";
  emit ctx "    ret";
  emit ctx ""

let gen_program (prog : Ast.program) =
  let ctx = create () in
  emit ctx "bits 64";
  emit ctx "default rel";
  emit ctx "";
  emit ctx "extern printf";
  emit ctx "extern exit";
  emit ctx "extern malloc";
  emit ctx "extern free";
  emit ctx "extern system";
  emit ctx "extern sprintf";
  emit ctx "extern scanf";
  emit ctx "";
  emit ctx "section .text";
  emit ctx "";
  List.iter (gen_func ctx) prog.functions;
  emit ctx "section .data";
  emit ctx "    fmt_int db \"%d\", 10, 0";
  emit ctx "    fmt_str db \"%s\", 10, 0";
  emit ctx "    fmt_concat db \"%s%s\", 0";
  emit ctx "    fmt_input db \"%s\", 0";
  emit ctx "    fmt_float db \"%f\", 10, 0";
  List.iter (fun (str, label) ->
    emitf ctx "    %s db \"%s\", 0" label str
  ) ctx.string_lits;
  List.iter (fun (f, label) ->
    emitf ctx "    %s dq %.17g" label f
  ) ctx.float_lits;
  Buffer.contents ctx.buf
