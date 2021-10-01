	.file	"hellofolks.c"
	.text
	.globl	names
	.section	.rodata
.LC0:
	.string	"Joseph"
.LC1:
	.string	"Jos\303\251"
.LC2:
	.string	"Josephine"
.LC3:
	.string	"Giuseppe"
.LC4:
	.string	"Iosephus"
.LC5:
	.string	"Josefina"
	.data
	.align 32
	.type	names, @object
	.size	names, 56
names:
	.quad	.LC0
	.quad	.LC1
	.quad	.LC2
	.quad	.LC3
	.quad	.LC4
	.quad	.LC5
	.quad	.LC2
	.section	.rodata
.LC6:
	.string	"Hello, %s!\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	$0, -4(%rbp)
	jmp	.L2
.L3:
	movl	-4(%rbp), %eax
	cltq
	movq	names(,%rax,8), %rax
	movq	%rax, %rsi
	movl	$.LC6, %edi
	movl	$0, %eax
	call	printf
	addl	$1, -4(%rbp)
.L2:
	movl	-4(%rbp), %eax
	cltq
	movq	names(,%rax,8), %rax
	testq	%rax, %rax
	jne	.L3
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (GNU) 11.2.1 20210728 (Red Hat 11.2.1-1)"
	.section	.note.GNU-stack,"",@progbits
