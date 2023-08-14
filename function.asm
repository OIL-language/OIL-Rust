[BITS 64]
global _start
section .text
print:
    enter 0, 0
    mov rax, 1
    mov rdi, 1
    mov rsi, [rbp + 16]
    mov rdx, [rbp + 24]
    syscall
    leave
    ret
_start:
    call main
    mov rdi, rax
    mov rax, 60
    syscall
main:
    enter 24, 0
    mov qword [rsp + 8], 0
.L0:
    mov rax, [rsp + 8]
    cmp rax, 3
    sete byte [rsp + 16]
    and byte [rsp + 16], 0x1
    xor byte [rsp + 16], 0x1
    mov al, [rsp + 16]
    test al, al
    jz .L1
    push qword [rsp + 8]
    push str_0
    call print
    add rsp, 16
    push 1
    push str_1
    call print
    add rsp, 16
    add qword [rsp + 8], 1
    jmp .L0
.L1:
    mov qword [rsp + 0], 0
    mov rax, [rsp + 0]
    leave
    ret
@main:
    enter 8, 0
    push qword [rsp + 0]
    call main
    pop qword [rsp + 0]
    add rsp, 0
    mov rax, [rsp + 0]
    leave
    ret
section .data
str_0: db 72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33
str_1: db 10
