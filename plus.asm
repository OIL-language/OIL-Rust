[BITS 64]
global _start
section .text
_start:
    call main
    mov rdi, rax
    mov rax, 60
    syscall
main:
    enter 10, 0
    mov word [rsp + 2], 1
    mov ax, word [rsp + 2]
    mov word [rsp + 4], ax
    mov ax, word [rsp + 2]
    mov word [rsp + 8], ax
    add word [rsp + 8], 1
    mov ax, word [rsp + 8]
    mov word [rsp + 2], ax
    mov ax, word [rsp + 2]
    mov word [rsp + 6], ax
    mov ax, word [rsp + 6]
    add word [rsp + 4], ax
    mov ax, word [rsp + 4]
    mov word [rsp + 0], ax
    mov rax, [rsp]
    leave
    ret
