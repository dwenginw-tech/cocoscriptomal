let is_linux = Sys.os_type = "Unix"

let emit_quicksort asm =
  asm "global _coco_quicksort";
  asm "_coco_quicksort:";
  asm "    push rbp";
  asm "    mov rbp, rsp";
  asm "    push rbx";
  asm "    push r12";
  asm "    push r13";
  asm "    push r14";
  asm "    push r15";
  
  if is_linux then begin
    asm "    mov rbx, rdi";
    asm "    mov r12, rsi";
    asm "    mov r13, rdx"
  end else begin
    asm "    mov rbx, rcx";
    asm "    mov r12, rdx";
    asm "    mov r13, r8"
  end;
  
  asm "    cmp r12, r13";
  asm "    jge .qs_done";
  
  asm "    mov r14, r12";
  asm "    mov r15, r13";
  asm "    mov rax, [rbx+r13*8]";
  asm "    push rax";
  
  asm ".qs_partition:";
  asm "    cmp r14, r15";
  asm "    jge .qs_partition_done";
  
  asm ".qs_left:";
  asm "    mov rax, [rbx+r14*8]";
  asm "    cmp rax, [rsp]";
  asm "    jge .qs_right";
  asm "    inc r14";
  asm "    jmp .qs_left";
  
  asm ".qs_right:";
  asm "    mov rax, [rbx+r15*8]";
  asm "    cmp rax, [rsp]";
  asm "    jle .qs_swap";
  asm "    dec r15";
  asm "    jmp .qs_right";
  
  asm ".qs_swap:";
  asm "    mov rax, [rbx+r14*8]";
  asm "    mov rcx, [rbx+r15*8]";
  asm "    mov [rbx+r14*8], rcx";
  asm "    mov [rbx+r15*8], rax";
  asm "    inc r14";
  asm "    dec r15";
  asm "    jmp .qs_partition";
  
  asm ".qs_partition_done:";
  asm "    pop rax";
  
  if is_linux then begin
    asm "    push r13";
    asm "    mov rdi, rbx";
    asm "    mov rsi, r12";
    asm "    mov rdx, r15";
    asm "    call _coco_quicksort";
    asm "    pop r13";
    asm "    mov rdi, rbx";
    asm "    mov rsi, r14";
    asm "    mov rdx, r13";
    asm "    call _coco_quicksort"
  end else begin
    asm "    push r13";
    asm "    mov rcx, rbx";
    asm "    mov rdx, r12";
    asm "    mov r8, r15";
    asm "    sub rsp, 32";
    asm "    call _coco_quicksort";
    asm "    add rsp, 32";
    asm "    pop r13";
    asm "    mov rcx, rbx";
    asm "    mov rdx, r14";
    asm "    mov r8, r13";
    asm "    sub rsp, 32";
    asm "    call _coco_quicksort";
    asm "    add rsp, 32"
  end;
  
  asm ".qs_done:";
  asm "    pop r15";
  asm "    pop r14";
  asm "    pop r13";
  asm "    pop r12";
  asm "    pop rbx";
  asm "    pop rbp";
  asm "    ret";
  asm ""
