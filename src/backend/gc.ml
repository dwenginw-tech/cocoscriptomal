let arena_size = 1024 * 1024

let alloc_fn = "_coco_alloc"
let arena_ptr = "_coco_arena_ptr"
let arena_end = "_coco_arena_end"

(* _coco_alloc: rcx = size, returns pointer in rax.
   bump-allocates from 1MB arenas, calls malloc for new blocks. *)
let emit_allocator asm =
  asm (Printf.sprintf "global %s" alloc_fn);
  asm (Printf.sprintf "%s:" alloc_fn);
  asm "    push rbp";
  asm "    mov rbp, rsp";
  asm "    add rcx, 7";
  asm "    and rcx, -8";
  asm (Printf.sprintf "    mov rax, [rel %s]" arena_ptr);
  asm "    add rax, rcx";
  asm (Printf.sprintf "    cmp rax, [rel %s]" arena_end);
  asm "    ja .new_arena";
  asm (Printf.sprintf "    mov rdx, [rel %s]" arena_ptr);
  asm (Printf.sprintf "    mov [rel %s], rax" arena_ptr);
  asm "    mov rax, rdx";
  asm "    pop rbp";
  asm "    ret";
  asm ".new_arena:";
  asm "    push rcx";
  asm (Printf.sprintf "    mov rcx, %d" arena_size);
  asm "    sub rsp, 32";
  asm "    call malloc";
  asm "    add rsp, 32";
  asm "    pop rcx";
  asm (Printf.sprintf "    mov [rel %s], rax" arena_ptr);
  asm "    mov rdx, rax";
  asm (Printf.sprintf "    add rdx, %d" arena_size);
  asm (Printf.sprintf "    mov [rel %s], rdx" arena_end);
  asm "    mov rdx, rax";
  asm "    add rax, rcx";
  asm (Printf.sprintf "    mov [rel %s], rax" arena_ptr);
  asm "    mov rax, rdx";
  asm "    pop rbp";
  asm "    ret";
  asm ""

let emit_arena_data asm =
  asm (Printf.sprintf "    %s dq 0" arena_ptr);
  asm (Printf.sprintf "    %s dq 0" arena_end)
