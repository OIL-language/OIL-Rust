[BITS 64]
global _start
section .text
malloc:
    enter 0, 0
    mov rax, 0x9        ; syscall mmap
    mov rdi, 0x0        ; addr chosed by kernel
    mov rsi, [rbp + 16] ; length
    mov rdx, 0x3        ; PROT_READ | PROT_WRITE
    mov r10, 0x22       ; MAP_ANONYMOUS | MAP_PRIVATE
    mov r8, -1          ; no file descriptor
    mov r9, 0           ; no offset
    syscall
    cmp rax, -1    ; MAP_FAILED
    mov rbx, 0x0   ; null
    mov qword [rbp + 24], rax
    leave
    ret
free:
    enter 0, 0
    mov rax, 0xb        ; syscall munmap
    mov rdi, [rbp + 24] ; addr
    mov rsi, [rbp + 16] ; length
    syscall
    leave
    ret
print:
    enter 0, 0
    mov rax, 0x1        ; syscall write
    mov rdi, 0x1        ; stdout
    mov rsi, [rbp + 24] ; text
    mov rdx, [rbp + 16] ; length
    syscall
    leave
    ret
_start:
    sub rsp, 0x8
    call @main
    mov rax, 0x3c ; syscall exit
    pop rdi       ; exit code
    syscall
strlen:
    enter 48, 0
    mov rax, 0
    mov qword [rbp - 16], rax
.L0:
    mov rax, qword [rbp + 16]
    mov rcx, qword [rbp - 16]
    mov bl, [rax + rcx]
    mov byte [rbp - 32], bl
    mov al, byte [rbp - 32]
    cmp al, 0
    setg byte [rbp - 24]
    mov al, byte [rbp - 24]
    test al, al
    jz .L1
    mov rax, qword [rbp - 16]
    mov qword [rbp - 40], rax
    mov rax, 1
    add qword [rbp - 40], rax
    mov rax, qword [rbp - 40]
    mov qword [rbp - 16], rax
    jmp .L0
.L1:
    mov rax, qword [rbp - 16]
    mov qword [rbp - 8], rax
    mov rax, qword [rbp - 8]
    mov qword [rbp + 24], rax
    leave
    ret
print_len:
    enter 16, 0
    mov rax, qword [rbp - 8]
    push rax
    mov rax, qword [rbp + 16]
    push rax
    call strlen
    add rsp, 8
    pop rax
    mov qword [rbp - 8], rax
    mov rax, qword [rbp + 16]
    push rax
    mov rax, qword [rbp - 8]
    push rax
    call print
    add rsp, 16
    leave
    ret
strcpy:
    enter 56, 0
    mov rax, 0
    mov qword [rbp - 8], rax
.L0:
    mov rax, qword [rbp - 8]
    cmp rax, qword [rbp + 16]
    setl byte [rbp - 16]
    mov al, byte [rbp - 16]
    test al, al
    jz .L1
    mov rax, qword [rbp + 24]
    mov rcx, qword [rbp - 8]
    mov bl, [rax + rcx]
    mov byte [rbp - 24], bl
    mov rax, qword [rbp + 32]
    mov bl, byte [rbp - 24]
    mov rcx, qword [rbp - 8]
    mov [rax + rcx], bl
    mov rax, qword [rbp - 8]
    mov qword [rbp - 32], rax
    mov rax, 1
    add qword [rbp - 32], rax
    mov rax, qword [rbp - 32]
    mov qword [rbp - 8], rax
    jmp .L0
.L1:
    leave
    ret
realloc:
    enter 40, 0
    mov rax, qword [rbp - 16]
    push rax
    mov rax, qword [rbp + 16]
    push rax
    call malloc
    add rsp, 8
    pop rax
    mov qword [rbp - 16], rax
    mov rax, qword [rbp - 16]
    mov qword [rbp - 8], rax
    mov rax, qword [rbp - 8]
    push rax
    mov rbx, [rbp + 32]
    mov rax, qword [rbx]
    push rax
    mov rax, qword [rbp + 24]
    push rax
    call strcpy
    add rsp, 24
    mov rbx, [rbp + 32]
    mov rax, qword [rbx]
    push rax
    mov rax, qword [rbp + 24]
    push rax
    call free
    add rsp, 16
    mov rax, qword [rbp + 32]
    mov rbx, qword [rbp - 8]
    mov [rax], rbx
    leave
    ret
DString_from:
    enter 88, 0
    mov rax, qword [rbp - 56]
    push rax
    mov rax, qword [rbp + 16]
    push rax
    call strlen
    add rsp, 8
    pop rax
    mov qword [rbp - 56], rax
    mov rax, qword [rbp - 56]
    mov qword [rbp - 32 - 8], rax
    mov rax, 1
    mov qword [rbp - 32 - 16], rax
.L0:
    mov rax, qword [rbp - 32 - 16]
    cmp rax, qword [rbp - 32 - 8]
    setl byte [rbp - 64]
    mov al, byte [rbp - 64]
    test al, al
    jz .L1
    mov rax, qword [rbp - 32 - 16]
    mov qword [rbp - 72], rax
    mov rax, qword [rbp - 72]
    mov rbx, 2
    mul rbx
    mov qword [rbp - 72], rax
    mov rax, qword [rbp - 72]
    mov qword [rbp - 32 - 16], rax
    jmp .L0
.L1:
    mov rax, qword [rbp - 80]
    push rax
    mov rax, qword [rbp - 32 - 16]
    push rax
    call malloc
    add rsp, 8
    pop rax
    mov qword [rbp - 80], rax
    mov rax, qword [rbp - 80]
    mov qword [rbp - 32 - 0], rax
    mov rax, qword [rbp - 32 - 0]
    push rax
    mov rax, qword [rbp + 16]
    push rax
    mov rax, qword [rbp - 32 - 8]
    push rax
    call strcpy
    add rsp, 24
    mov rax, qword [rbp - 32 - 0]
    mov qword [rbp - 8 - 0], rax
    mov rax, qword [rbp - 32 - 8]
    mov qword [rbp - 8 - 8], rax
    mov rax, qword [rbp - 32 - 16]
    mov qword [rbp - 8 - 16], rax
    mov rax, qword [rbp - 8 - 0]
    mov qword [rbp + 40 - 0], rax
    mov rax, qword [rbp - 8 - 8]
    mov qword [rbp + 40 - 8], rax
    mov rax, qword [rbp - 8 - 16]
    mov qword [rbp + 40 - 16], rax
    leave
    ret
DString_pop:
    enter 24, 0
    mov rbx, [rbp + 16]
    mov rax, qword [rbx - 8]
    cmp rax, 0
    sete byte [rbp - 8]
    mov al, byte [rbp - 8]
    test al, al
    jz .L0
    mov rax, str_0
    push rax
    call print_len
    add rsp, 8
    jmp .L1
.L0:
.L1:
    mov rbx, [rbp + 16]
    mov rax, qword [rbx - 8]
    mov qword [rbp - 16], rax
    mov rax, 1
    sub qword [rbp - 16], rax
    mov rcx, [rbp + 16]
    mov rax, qword [rbp - 16]
    mov qword [rcx - 8], rax
    leave
    ret
DString_append:
    enter 56, 0
    mov rbx, [rbp + 24]
    mov rcx, [rbp + 24]
    mov rax, qword [rbx - 8]
    cmp rax, qword [rcx - 16]
    setge byte [rbp - 8]
    mov al, byte [rbp - 8]
    test al, al
    jz .L0
    mov rbx, [rbp + 24]
    mov rax, qword [rbx - 16]
    mov qword [rbp - 16], rax
    mov rbx, [rbp + 24]
    mov rax, qword [rbx - 16]
    mov qword [rbp - 24], rax
    mov rax, qword [rbp - 24]
    mov rbx, 2
    mul rbx
    mov qword [rbp - 24], rax
    mov rcx, [rbp + 24]
    mov rax, qword [rbp - 24]
    mov qword [rcx - 16], rax
    mov rbx, [rbp + 24]
    lea rax, [rbx - 0]
    mov qword [rbp - 32], rax
    mov rax, qword [rbp - 32]
    push rax
    mov rax, qword [rbp - 16]
    push rax
    mov rbx, [rbp + 24]
    mov rax, qword [rbx - 16]
    push rax
    call realloc
    add rsp, 24
    jmp .L1
.L0:
.L1:
    mov rdi, [rbp + 24]
    mov rax, qword [rdi - 0]
    mov bl, byte [rbp + 16]
    mov r8, [rbp + 24]
    mov rcx, qword [r8 - 8]
    mov [rax + rcx], bl
    mov rbx, [rbp + 24]
    mov rax, qword [rbx - 8]
    mov qword [rbp - 40], rax
    mov rax, 1
    add qword [rbp - 40], rax
    mov rcx, [rbp + 24]
    mov rax, qword [rbp - 40]
    mov qword [rcx - 8], rax
    leave
    ret
DString_free:
    enter 8, 0
    mov rbx, [rbp + 16]
    mov rax, qword [rbx - 0]
    push rax
    mov rbx, [rbp + 16]
    mov rax, qword [rbx - 16]
    push rax
    call free
    add rsp, 16
    leave
    ret
@main:
    enter 112, 0
    mov rax, qword [rbp - 40 - 16]
    push rax
    mov rax, qword [rbp - 40 - 8]
    push rax
    mov rax, qword [rbp - 40 - 0]
    push rax
    mov rax, str_1
    push rax
    call DString_from
    add rsp, 8
    pop rax
    mov qword [rbp - 40 - 16], rax
    pop rax
    mov qword [rbp - 40 - 8], rax
    pop rax
    mov qword [rbp - 40 - 0], rax
    mov rax, qword [rbp - 40 - 0]
    mov qword [rbp - 16 - 0], rax
    mov rax, qword [rbp - 40 - 8]
    mov qword [rbp - 16 - 8], rax
    mov rax, qword [rbp - 40 - 16]
    mov qword [rbp - 16 - 16], rax
    mov rax, 0
    mov qword [rbp - 64], rax
.L0:
    mov rax, qword [rbp - 64]
    cmp rax, 1000000
    setl byte [rbp - 72]
    mov al, byte [rbp - 72]
    test al, al
    jz .L1
    lea rax, [rbp - 16]
    mov qword [rbp - 80], rax
    mov rax, qword [rbp - 80]
    push rax
    mov al, 65
    push rax
    call DString_append
    add rsp, 16
    lea rax, [rbp - 16]
    mov qword [rbp - 88], rax
    mov rax, qword [rbp - 88]
    push rax
    mov al, 66
    push rax
    call DString_append
    add rsp, 16
    lea rax, [rbp - 16]
    mov qword [rbp - 96], rax
    mov rax, qword [rbp - 96]
    push rax
    mov al, 67
    push rax
    call DString_append
    add rsp, 16
    mov rax, qword [rbp - 64]
    mov qword [rbp - 104], rax
    mov rax, 1
    add qword [rbp - 104], rax
    mov rax, qword [rbp - 104]
    mov qword [rbp - 64], rax
    jmp .L0
.L1:
    mov rax, qword [rbp - 16 - 0]
    push rax
    mov rax, qword [rbp - 16 - 8]
    push rax
    call print
    add rsp, 16
    lea rax, [rbp - 16]
    mov qword [rbp - 112], rax
    mov rax, qword [rbp - 112]
    push rax
    call DString_free
    add rsp, 8
    mov rax, 0
    mov qword [rbp - 8], rax
    mov rax, qword [rbp - 8]
    mov qword [rbp + 16], rax
    leave
    ret
section .data
str_0: db 69, 114, 114, 111, 114, 44, 32, 116, 114, 105, 101, 100, 32, 116, 111, 32, 112, 111, 112, 32, 102, 114, 111, 109, 32, 101, 109, 112, 116, 121, 32, 115, 116, 114, 105, 110, 103, 0
str_1: db 72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33, 10, 0
