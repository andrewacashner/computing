;---------------------------------------------------------------
; First NASM program of my own
; Add two numbers and return the sum as the exit value
; Check the exit value in bash with `echo "$?"`
; To compile, link, run, and check exit value:
;     nasm -felf64 add.asm && ld add.o && ./a.out ; echo "$?"
;
; 2017/01/27
;----------------------------------------------------------------

        global  _start

        section .text
_start:
        mov     ecx, 3
        add     ecx, 7
        ; exit with value of rcx
        mov     eax, 60
        xor     rdi, rcx
        syscall
