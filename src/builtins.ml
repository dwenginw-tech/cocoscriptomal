(* built-in function implementations in assembly *)

let print_func = {|
global _coco_print
_coco_print:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov rdx, rcx
    lea rcx, [rel fmt_int]
    call printf
    add rsp, 32
    pop rbp
    ret
|}

let print_str_func = {|
global _coco_print_str
_coco_print_str:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov rdx, rcx
    lea rcx, [rel fmt_str]
    call printf
    add rsp, 32
    pop rbp
    ret
|}
