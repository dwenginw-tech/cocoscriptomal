(* gc_marksweep.ml - Mark and sweep garbage collector for CocoScript *)

(* Object header layout (8 bytes):
   [0-3]: size in bytes
   [4]: mark bit (0 = white, 1 = black)
   [5]: type tag (0=int, 1=string, 2=array, 3=object, 4=closure)
   [6-7]: reserved *)

let is_linux = Sys.os_type = "Unix"

(* GC state *)
let gc_heap_start = "_gc_heap_start"
let gc_heap_ptr = "_gc_heap_ptr"
let gc_heap_end = "_gc_heap_end"
let gc_roots = "_gc_roots"
let gc_root_count = "_gc_root_count"
let gc_threshold = "_gc_threshold"

let heap_size = 8 * 1024 * 1024  (* 8MB heap *)
let initial_threshold = 1024 * 1024  (* Trigger GC at 1MB *)

(* Type tags *)
let tag_int = 0
let tag_string = 1
let tag_array = 2
let tag_object = 3
let tag_closure = 4

(* Emit the GC initialization code *)
let emit_gc_init asm =
  asm "global _gc_init";
  asm "_gc_init:";
  asm "    push rbp";
  asm "    mov rbp, rsp";
  
  if is_linux then begin
    asm (Printf.sprintf "    mov rdi, %d" heap_size);
    asm "    call malloc";
    asm (Printf.sprintf "    mov [rel %s], rax" gc_heap_start);
    asm (Printf.sprintf "    mov [rel %s], rax" gc_heap_ptr);
    asm "    mov rdx, rax";
    asm (Printf.sprintf "    add rdx, %d" heap_size);
    asm (Printf.sprintf "    mov [rel %s], rdx" gc_heap_end);
    asm (Printf.sprintf "    mov qword [rel %s], %d" gc_threshold initial_threshold);
    asm (Printf.sprintf "    mov qword [rel %s], 0" gc_root_count)
  end else begin
    asm (Printf.sprintf "    mov rcx, %d" heap_size);
    asm "    sub rsp, 32";
    asm "    call malloc";
    asm "    add rsp, 32";
    asm (Printf.sprintf "    mov [rel %s], rax" gc_heap_start);
    asm (Printf.sprintf "    mov [rel %s], rax" gc_heap_ptr);
    asm "    mov rdx, rax";
    asm (Printf.sprintf "    add rdx, %d" heap_size);
    asm (Printf.sprintf "    mov [rel %s], rdx" gc_heap_end);
    asm (Printf.sprintf "    mov qword [rel %s], %d" gc_threshold initial_threshold);
    asm (Printf.sprintf "    mov qword [rel %s], 0" gc_root_count)
  end;
  
  asm "    pop rbp";
  asm "    ret";
  asm ""

(* Mark phase - recursively mark all reachable objects *)
let emit_gc_mark asm =
  asm "global _gc_mark";
  asm "_gc_mark:";
  asm "    push rbp";
  asm "    mov rbp, rsp";
  asm "    push rbx";
  asm "    push r12";
  asm "    push r13";
  
  if is_linux then
    asm "    mov rbx, rdi"  (* object pointer in rbx *)
  else
    asm "    mov rbx, rcx";  (* object pointer in rbx *)
  
  (* Check if pointer is in heap *)
  asm (Printf.sprintf "    mov rax, [rel %s]" gc_heap_start);
  asm "    cmp rbx, rax";
  asm "    jb .mark_done";
  asm (Printf.sprintf "    mov rax, [rel %s]" gc_heap_end);
  asm "    cmp rbx, rax";
  asm "    jae .mark_done";
  
  (* Check if already marked *)
  asm "    mov al, byte [rbx+4]";
  asm "    test al, al";
  asm "    jnz .mark_done";
  
  (* Mark object *)
  asm "    mov byte [rbx+4], 1";
  
  (* Get type tag *)
  asm "    movzx rax, byte [rbx+5]";
  
  (* Handle different types *)
  asm (Printf.sprintf "    cmp rax, %d" tag_string);
  asm "    je .mark_done";  (* strings have no pointers *)
  
  asm (Printf.sprintf "    cmp rax, %d" tag_array);
  asm "    je .mark_array";
  
  asm (Printf.sprintf "    cmp rax, %d" tag_object);
  asm "    je .mark_object";
  
  asm (Printf.sprintf "    cmp rax, %d" tag_closure);
  asm "    je .mark_closure";
  
  asm "    jmp .mark_done";
  
  (* Mark array elements *)
  asm ".mark_array:";
  asm "    mov r12, [rbx+8]";  (* length *)
  asm "    lea r13, [rbx+16]";  (* first element *)
  asm ".mark_array_loop:";
  asm "    test r12, r12";
  asm "    jz .mark_done";
  asm "    mov rdi, [r13]";
  if is_linux then
    asm "    call _gc_mark"
  else begin
    asm "    mov rcx, rdi";
    asm "    sub rsp, 32";
    asm "    call _gc_mark";
    asm "    add rsp, 32"
  end;
  asm "    add r13, 8";
  asm "    dec r12";
  asm "    jmp .mark_array_loop";
  
  (* Mark object fields *)
  asm ".mark_object:";
  asm "    mov r12d, dword [rbx]";  (* size *)
  asm "    shr r12, 3";  (* convert to field count *)
  asm "    lea r13, [rbx+8]";
  asm ".mark_object_loop:";
  asm "    test r12, r12";
  asm "    jz .mark_done";
  asm "    mov rdi, [r13]";
  if is_linux then
    asm "    call _gc_mark"
  else begin
    asm "    mov rcx, rdi";
    asm "    sub rsp, 32";
    asm "    call _gc_mark";
    asm "    add rsp, 32"
  end;
  asm "    add r13, 8";
  asm "    dec r12";
  asm "    jmp .mark_object_loop";
  
  (* Mark closure environment *)
  asm ".mark_closure:";
  asm "    mov rdi, [rbx+16]";  (* env pointer *)
  if is_linux then
    asm "    call _gc_mark"
  else begin
    asm "    mov rcx, rdi";
    asm "    sub rsp, 32";
    asm "    call _gc_mark";
    asm "    add rsp, 32"
  end;
  
  asm ".mark_done:";
  asm "    pop r13";
  asm "    pop r12";
  asm "    pop rbx";
  asm "    pop rbp";
  asm "    ret";
  asm ""

(* Sweep phase - free unmarked objects and compact *)
let emit_gc_sweep asm =
  asm "global _gc_sweep";
  asm "_gc_sweep:";
  asm "    push rbp";
  asm "    mov rbp, rsp";
  asm "    push rbx";
  asm "    push r12";
  asm "    push r13";
  
  asm (Printf.sprintf "    mov rbx, [rel %s]" gc_heap_start);  (* current *)
  asm (Printf.sprintf "    mov r12, [rel %s]" gc_heap_ptr);    (* end *)
  asm "    mov r13, rbx";  (* write pointer for compaction *)
  
  asm ".sweep_loop:";
  asm "    cmp rbx, r12";
  asm "    jae .sweep_done";
  
  (* Get object size *)
  asm "    mov eax, dword [rbx]";
  asm "    add eax, 8";  (* include header *)
  
  (* Check mark bit *)
  asm "    mov cl, byte [rbx+4]";
  asm "    test cl, cl";
  asm "    jz .sweep_free";
  
  (* Object is alive - clear mark and copy if needed *)
  asm "    mov byte [rbx+4], 0";
  asm "    cmp rbx, r13";
  asm "    je .sweep_skip_copy";
  
  (* Copy object to compacted position *)
  asm "    push rax";
  asm "    push rbx";
  if is_linux then begin
    asm "    mov rdi, r13";
    asm "    mov rsi, rbx";
    asm "    mov rdx, rax";
    asm "    call memcpy"
  end else begin
    asm "    mov rcx, r13";
    asm "    mov rdx, rbx";
    asm "    mov r8, rax";
    asm "    sub rsp, 32";
    asm "    call memcpy";
    asm "    add rsp, 32"
  end;
  asm "    pop rbx";
  asm "    pop rax";
  
  asm ".sweep_skip_copy:";
  asm "    add r13, rax";  (* advance write pointer *)
  asm "    add rbx, rax";  (* advance read pointer *)
  asm "    jmp .sweep_loop";
  
  asm ".sweep_free:";
  (* Object is dead - skip it *)
  asm "    add rbx, rax";
  asm "    jmp .sweep_loop";
  
  asm ".sweep_done:";
  (* Update heap pointer *)
  asm (Printf.sprintf "    mov [rel %s], r13" gc_heap_ptr);
  
  asm "    pop r13";
  asm "    pop r12";
  asm "    pop rbx";
  asm "    pop rbp";
  asm "    ret";
  asm ""

(* Main GC collection routine *)
let emit_gc_collect asm =
  asm "global _gc_collect";
  asm "_gc_collect:";
  asm "    push rbp";
  asm "    mov rbp, rsp";
  
  (* Mark phase - mark all roots *)
  asm (Printf.sprintf "    mov rcx, [rel %s]" gc_root_count);
  asm "    test rcx, rcx";
  asm "    jz .collect_sweep";
  
  asm (Printf.sprintf "    lea rbx, [rel %s]" gc_roots);
  asm ".collect_mark_loop:";
  asm "    mov rdi, [rbx]";
  if is_linux then
    asm "    call _gc_mark"
  else begin
    asm "    mov rcx, rdi";
    asm "    sub rsp, 32";
    asm "    call _gc_mark";
    asm "    add rsp, 32"
  end;
  asm "    add rbx, 8";
  asm "    dec rcx";
  asm "    jnz .collect_mark_loop";
  
  (* Sweep phase *)
  asm ".collect_sweep:";
  if is_linux then
    asm "    call _gc_sweep"
  else begin
    asm "    sub rsp, 32";
    asm "    call _gc_sweep";
    asm "    add rsp, 32"
  end;
  
  (* Update threshold *)
  asm (Printf.sprintf "    mov rax, [rel %s]" gc_heap_ptr);
  asm (Printf.sprintf "    sub rax, [rel %s]" gc_heap_start);
  asm "    shl rax, 1";  (* threshold = 2 * current_size *)
  asm (Printf.sprintf "    mov [rel %s], rax" gc_threshold);
  
  asm "    pop rbp";
  asm "    ret";
  asm ""

(* Allocate with GC - checks threshold and triggers collection *)
let emit_gc_alloc asm =
  asm "global _gc_alloc";
  asm "_gc_alloc:";
  asm "    push rbp";
  asm "    mov rbp, rsp";
  asm "    push rbx";
  asm "    push r12";
  
  if is_linux then
    asm "    mov r12, rdi"  (* size *)
  else
    asm "    mov r12, rcx";  (* size *)
  
  (* Align size *)
  asm "    add r12, 15";
  asm "    and r12, -16";
  
  (* Check if GC needed *)
  asm (Printf.sprintf "    mov rax, [rel %s]" gc_heap_ptr);
  asm (Printf.sprintf "    sub rax, [rel %s]" gc_heap_start);
  asm (Printf.sprintf "    cmp rax, [rel %s]" gc_threshold);
  asm "    jb .alloc_no_gc";
  
  (* Trigger GC *)
  if is_linux then
    asm "    call _gc_collect"
  else begin
    asm "    sub rsp, 32";
    asm "    call _gc_collect";
    asm "    add rsp, 32"
  end;
  
  asm ".alloc_no_gc:";
  (* Allocate from heap *)
  asm (Printf.sprintf "    mov rax, [rel %s]" gc_heap_ptr);
  asm "    mov rbx, rax";
  asm "    add rax, r12";
  asm "    add rax, 8";  (* header size *)
  asm (Printf.sprintf "    cmp rax, [rel %s]" gc_heap_end);
  asm "    ja .alloc_oom";
  
  asm (Printf.sprintf "    mov [rel %s], rax" gc_heap_ptr);
  
  (* Initialize header *)
  asm "    mov dword [rbx], r12d";  (* size *)
  asm "    mov byte [rbx+4], 0";    (* mark = 0 *)
  asm "    mov byte [rbx+5], 0";    (* type = int *)
  asm "    mov word [rbx+6], 0";    (* reserved *)
  
  asm "    lea rax, [rbx+8]";  (* return pointer past header *)
  
  asm "    pop r12";
  asm "    pop rbx";
  asm "    pop rbp";
  asm "    ret";
  
  asm ".alloc_oom:";
  (* Out of memory *)
  if is_linux then begin
    asm "    mov rdi, 1";
    asm "    mov rax, 60";
    asm "    syscall"
  end else begin
    asm "    mov rcx, 1";
    asm "    sub rsp, 32";
    asm "    call exit";
    asm "    add rsp, 32"
  end;
  asm ""

(* Emit all GC functions *)
let emit_gc_runtime asm =
  emit_gc_init asm;
  emit_gc_mark asm;
  emit_gc_sweep asm;
  emit_gc_collect asm;
  emit_gc_alloc asm

(* Emit GC data section *)
let emit_gc_data asm =
  asm (Printf.sprintf "    %s dq 0" gc_heap_start);
  asm (Printf.sprintf "    %s dq 0" gc_heap_ptr);
  asm (Printf.sprintf "    %s dq 0" gc_heap_end);
  asm (Printf.sprintf "    %s dq 0" gc_threshold);
  asm (Printf.sprintf "    %s dq 0" gc_root_count);
  asm (Printf.sprintf "    %s times 1024 dq 0" gc_roots)  (* max 1024 roots *)

