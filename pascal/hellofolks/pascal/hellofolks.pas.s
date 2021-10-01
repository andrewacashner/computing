	.file "hellofolks.pas"
# Begin asmlist al_procedures

.section .text.n_main
	.balign 16,0x90
.globl	PASCALMAIN
	.type	PASCALMAIN,@function
PASCALMAIN:
.globl	main
	.type	main,@function
main:
.Lc1:
	pushq	%rbp
.Lc3:
.Lc4:
	movq	%rsp,%rbp
.Lc5:
	leaq	-160(%rsp),%rsp
	movq	%rbx,-152(%rbp)
	movq	%r12,-144(%rbp)
	movq	%r13,-136(%rbp)
	call	fpc_initializeunits
	movq	$0,-104(%rbp)
	leaq	-24(%rbp),%rdx
	leaq	-88(%rbp),%rsi
	movl	$1,%edi
	call	fpc_pushexceptaddr
	movq	%rax,%rdi
	call	fpc_setjmp
	movslq	%eax,%rdx
	movq	%rdx,-96(%rbp)
	testl	%eax,%eax
	jne	.Lj8
	movq	TC_$P$HELLOFOLKS_$$_FOLKS,%rdi
	call	fpc_dynarray_high
	movq	%rax,%rbx
	cmpl	$0,%ebx
	jge	.Lj9
	jmp	.Lj10
.Lj9:
	movl	$-1,%r12d
	.balign 8,0x90
.Lj11:
	leal	1(%r12d),%eax
	movl	%eax,%r12d
	movq	$U_$P$HELLOFOLKS_$$_NAME,%rdi
	movq	TC_$P$HELLOFOLKS_$$_FOLKS,%rdx
	movslq	%r12d,%rax
	movq	(%rdx,%rax,8),%rsi
	call	fpc_unicodestr_assign
	call	fpc_get_output
	movq	%rax,%r13
	leaq	-104(%rbp),%rdi
	call	fpc_unicodestr_decr_ref
	movq	$.Ld9,%rax
	movq	%rax,-128(%rbp)
	movq	U_$P$HELLOFOLKS_$$_NAME,%rax
	movq	%rax,-120(%rbp)
	movq	$.Ld10,%rax
	movq	%rax,-112(%rbp)
	leaq	-128(%rbp),%rsi
	movq	$2,%rdx
	leaq	-104(%rbp),%rdi
	call	fpc_unicodestr_concat_multi
	movq	-104(%rbp),%rdx
	movq	%r13,%rsi
	movl	$0,%edi
	call	fpc_write_text_unicodestr
	call	fpc_iocheck
	movq	%r13,%rdi
	call	fpc_writeln_end
	call	fpc_iocheck
	cmpl	%r12d,%ebx
	jle	.Lj13
	jmp	.Lj11
.Lj13:
.Lj10:
.Lj8:
	call	fpc_popaddrstack
	leaq	-104(%rbp),%rdi
	call	fpc_unicodestr_decr_ref
	movq	-96(%rbp),%rax
	testq	%rax,%rax
	je	.Lj7
	call	fpc_reraise
.Lj7:
	call	fpc_do_exit
	movq	-152(%rbp),%rbx
	movq	-144(%rbp),%r12
	movq	-136(%rbp),%r13
	movq	%rbp,%rsp
	popq	%rbp
	ret
.Lc2:
.Le0:
	.size	main, .Le0 - main

.section .text.n_p$hellofolks_$$_init_implicit$
	.balign 16,0x90
.globl	INIT$_$P$HELLOFOLKS
	.type	INIT$_$P$HELLOFOLKS,@function
INIT$_$P$HELLOFOLKS:
.globl	P$HELLOFOLKS_$$_init_implicit$
	.type	P$HELLOFOLKS_$$_init_implicit$,@function
P$HELLOFOLKS_$$_init_implicit$:
.Lc6:
	pushq	%rbp
.Lc8:
.Lc9:
	movq	%rsp,%rbp
.Lc10:
	movq	%rbp,%rsp
	popq	%rbp
	ret
.Lc7:
.Le1:
	.size	P$HELLOFOLKS_$$_init_implicit$, .Le1 - P$HELLOFOLKS_$$_init_implicit$

.section .text.n_p$hellofolks_$$_finalize_implicit$
	.balign 16,0x90
.globl	PASCALFINALIZE
	.type	PASCALFINALIZE,@function
PASCALFINALIZE:
.globl	FINALIZE$_$P$HELLOFOLKS
	.type	FINALIZE$_$P$HELLOFOLKS,@function
FINALIZE$_$P$HELLOFOLKS:
.globl	P$HELLOFOLKS_$$_finalize_implicit$
	.type	P$HELLOFOLKS_$$_finalize_implicit$,@function
P$HELLOFOLKS_$$_finalize_implicit$:
.Lc11:
	pushq	%rbp
.Lc13:
.Lc14:
	movq	%rsp,%rbp
.Lc15:
	movq	$RTTI_$P$HELLOFOLKS_$$_def00000003,%rax
	movq	%rax,%rsi
	movq	$TC_$P$HELLOFOLKS_$$_FOLKS,%rdi
	call	fpc_finalize
	movq	$U_$P$HELLOFOLKS_$$_NAME,%rdi
	call	fpc_unicodestr_decr_ref
	movq	%rbp,%rsp
	popq	%rbp
	ret
.Lc12:
.Le2:
	.size	P$HELLOFOLKS_$$_finalize_implicit$, .Le2 - P$HELLOFOLKS_$$_finalize_implicit$

.section .text
# End asmlist al_procedures
# Begin asmlist al_globals

.section .bss
	.balign 8
	.type U_$P$HELLOFOLKS_$$_NAME,@object
	.size U_$P$HELLOFOLKS_$$_NAME,8
U_$P$HELLOFOLKS_$$_NAME:
	.zero 8

.section .data.n_INITFINAL
	.balign 8
.globl	INITFINAL
	.type	INITFINAL,@object
INITFINAL:
	.quad	2,0
	.quad	INIT$_$SYSTEM
	.quad	0
	.quad	INIT$_$P$HELLOFOLKS
	.quad	FINALIZE$_$P$HELLOFOLKS
.Le3:
	.size	INITFINAL, .Le3 - INITFINAL

.section .data.n_FPC_THREADVARTABLES
	.balign 8
.globl	FPC_THREADVARTABLES
	.type	FPC_THREADVARTABLES,@object
FPC_THREADVARTABLES:
	.long	1
	.quad	THREADVARLIST_$SYSTEM$indirect
.Le4:
	.size	FPC_THREADVARTABLES, .Le4 - FPC_THREADVARTABLES

.section .data.n_FPC_RESOURCESTRINGTABLES
	.balign 8
.globl	FPC_RESOURCESTRINGTABLES
	.type	FPC_RESOURCESTRINGTABLES,@object
FPC_RESOURCESTRINGTABLES:
	.quad	0
.Le5:
	.size	FPC_RESOURCESTRINGTABLES, .Le5 - FPC_RESOURCESTRINGTABLES

.section .data.n_FPC_WIDEINITTABLES
	.balign 8
.globl	FPC_WIDEINITTABLES
	.type	FPC_WIDEINITTABLES,@object
FPC_WIDEINITTABLES:
	.quad	0
.Le6:
	.size	FPC_WIDEINITTABLES, .Le6 - FPC_WIDEINITTABLES

.section .data.n_FPC_RESSTRINITTABLES
	.balign 8
.globl	FPC_RESSTRINITTABLES
	.type	FPC_RESSTRINITTABLES,@object
FPC_RESSTRINITTABLES:
	.quad	0
.Le7:
	.size	FPC_RESSTRINITTABLES, .Le7 - FPC_RESSTRINITTABLES

.section .fpc.n_version
	.balign 16
	.type	__fpc_ident,@object
__fpc_ident:
	.ascii	"FPC 3.2.2 [2021/06/03] for x86_64 - Linux"
.Le8:
	.size	__fpc_ident, .Le8 - __fpc_ident

.section .data.n___stklen
	.balign 8
.globl	__stklen
	.type	__stklen,@object
__stklen:
	.quad	8388608
.Le9:
	.size	__stklen, .Le9 - __stklen

.section .data.n___heapsize
	.balign 8
.globl	__heapsize
	.type	__heapsize,@object
__heapsize:
	.quad	0
.Le10:
	.size	__heapsize, .Le10 - __heapsize

.section .data.n___fpc_valgrind
	.balign 8
.globl	__fpc_valgrind
	.type	__fpc_valgrind,@object
__fpc_valgrind:
	.byte	0
.Le11:
	.size	__fpc_valgrind, .Le11 - __fpc_valgrind

.section .data.n_FPC_RESLOCATION
	.balign 8
.globl	FPC_RESLOCATION
	.type	FPC_RESLOCATION,@object
FPC_RESLOCATION:
	.quad	0
.Le12:
	.size	FPC_RESLOCATION, .Le12 - FPC_RESLOCATION
# End asmlist al_globals
# Begin asmlist al_const

.section .rodata.n_.Ld2
	.balign 8
.Ld2$strlab:
	.short	1200,2
	.long	0
	.quad	-1,6
.Ld2:
	.short	74,111,115,101,112,104,0
.Le13:
	.size	.Ld2$strlab, .Le13 - .Ld2$strlab

.section .rodata.n_.Ld2
	.balign 8
.Ld3$strlab:
	.short	1200,2
	.long	0
	.quad	-1,5
.Ld3:
	.short	74,111,115,195,169,0
.Le14:
	.size	.Ld3$strlab, .Le14 - .Ld3$strlab

.section .rodata.n_.Ld2
	.balign 8
.Ld4$strlab:
	.short	1200,2
	.long	0
	.quad	-1,9
.Ld4:
	.short	74,111,115,101,112,104,105,110,101,0
.Le15:
	.size	.Ld4$strlab, .Le15 - .Ld4$strlab

.section .rodata.n_.Ld2
	.balign 8
.Ld5$strlab:
	.short	1200,2
	.long	0
	.quad	-1,8
.Ld5:
	.short	71,105,117,115,101,112,112,101,0
.Le16:
	.size	.Ld5$strlab, .Le16 - .Ld5$strlab

.section .rodata.n_.Ld2
	.balign 8
.Ld6$strlab:
	.short	1200,2
	.long	0
	.quad	-1,8
.Ld6:
	.short	73,111,115,101,112,104,117,115,0
.Le17:
	.size	.Ld6$strlab, .Le17 - .Ld6$strlab

.section .rodata.n_.Ld2
	.balign 8
.Ld7$strlab:
	.short	1200,2
	.long	0
	.quad	-1,8
.Ld7:
	.short	74,111,115,101,102,105,110,97,0
.Le18:
	.size	.Ld7$strlab, .Le18 - .Ld7$strlab

.section .rodata.n_.Ld2
	.balign 8
.Ld8$strlab:
	.short	1200,2
	.long	0
	.quad	-1,9
.Ld8:
	.short	74,111,115,101,112,104,105,110,101,0
.Le19:
	.size	.Ld8$strlab, .Le19 - .Ld8$strlab

.section .data.n_.Ld1
	.balign 8
.Ld1$dynarrlab:
	.quad	-1,6
.Ld1:
	.quad	.Ld2
	.quad	.Ld3
	.quad	.Ld4
	.quad	.Ld5
	.quad	.Ld6
	.quad	.Ld7
	.quad	.Ld8
.Le20:
	.size	.Ld1$dynarrlab, .Le20 - .Ld1$dynarrlab
# End asmlist al_const
# Begin asmlist al_typedconsts

.section .data.n_TC_$P$HELLOFOLKS_$$_FOLKS
	.balign 8
	.type	TC_$P$HELLOFOLKS_$$_FOLKS,@object
TC_$P$HELLOFOLKS_$$_FOLKS:
	.quad	.Ld1
.Le21:
	.size	TC_$P$HELLOFOLKS_$$_FOLKS, .Le21 - TC_$P$HELLOFOLKS_$$_FOLKS

.section .rodata.n_.Ld9
	.balign 8
.Ld9$strlab:
	.short	1200,2
	.long	0
	.quad	-1,7
.Ld9:
	.short	72,101,108,108,111,44,32,0
.Le22:
	.size	.Ld9$strlab, .Le22 - .Ld9$strlab

.section .rodata.n_.Ld10
	.balign 8
.Ld10$strlab:
	.short	1200,2
	.long	0
	.quad	-1,1
.Ld10:
	.short	33,0
.Le23:
	.size	.Ld10$strlab, .Le23 - .Ld10$strlab
# End asmlist al_typedconsts
# Begin asmlist al_rtti

.section .data.n_RTTI_$P$HELLOFOLKS_$$_def00000003
	.balign 8
.globl	RTTI_$P$HELLOFOLKS_$$_def00000003
	.type	RTTI_$P$HELLOFOLKS_$$_def00000003,@object
RTTI_$P$HELLOFOLKS_$$_def00000003:
	.byte	21,0
	.quad	8
	.quad	RTTI_$SYSTEM_$$_UNICODESTRING$indirect
	.long	258
	.quad	RTTI_$SYSTEM_$$_UNICODESTRING$indirect
	.byte	10
	.ascii	"hellofolks"
.Le24:
	.size	RTTI_$P$HELLOFOLKS_$$_def00000003, .Le24 - RTTI_$P$HELLOFOLKS_$$_def00000003

.section .data.n_RTTI_$P$HELLOFOLKS_$$_def00000004
	.balign 8
.globl	RTTI_$P$HELLOFOLKS_$$_def00000004
	.type	RTTI_$P$HELLOFOLKS_$$_def00000004,@object
RTTI_$P$HELLOFOLKS_$$_def00000004:
	.byte	12,0
	.quad	24,3
	.quad	RTTI_$SYSTEM_$$_UNICODESTRING$indirect
	.byte	1
	.quad	RTTI_$SYSTEM_$$_LONGINT$indirect
.Le25:
	.size	RTTI_$P$HELLOFOLKS_$$_def00000004, .Le25 - RTTI_$P$HELLOFOLKS_$$_def00000004
# End asmlist al_rtti
# Begin asmlist al_indirectglobals

.section .data.n_RTTI_$P$HELLOFOLKS_$$_def00000003
	.balign 8
.globl	RTTI_$P$HELLOFOLKS_$$_def00000003$indirect
	.type	RTTI_$P$HELLOFOLKS_$$_def00000003$indirect,@object
RTTI_$P$HELLOFOLKS_$$_def00000003$indirect:
	.quad	RTTI_$P$HELLOFOLKS_$$_def00000003
.Le26:
	.size	RTTI_$P$HELLOFOLKS_$$_def00000003$indirect, .Le26 - RTTI_$P$HELLOFOLKS_$$_def00000003$indirect

.section .data.n_RTTI_$P$HELLOFOLKS_$$_def00000004
	.balign 8
.globl	RTTI_$P$HELLOFOLKS_$$_def00000004$indirect
	.type	RTTI_$P$HELLOFOLKS_$$_def00000004$indirect,@object
RTTI_$P$HELLOFOLKS_$$_def00000004$indirect:
	.quad	RTTI_$P$HELLOFOLKS_$$_def00000004
.Le27:
	.size	RTTI_$P$HELLOFOLKS_$$_def00000004$indirect, .Le27 - RTTI_$P$HELLOFOLKS_$$_def00000004$indirect
# End asmlist al_indirectglobals
# Begin asmlist al_dwarf_frame

.section .debug_frame
.Lc16:
	.long	.Lc18-.Lc17
.Lc17:
	.long	-1
	.byte	1
	.byte	0
	.uleb128	1
	.sleb128	-4
	.byte	16
	.byte	12
	.uleb128	7
	.uleb128	8
	.byte	5
	.uleb128	16
	.uleb128	2
	.balign 4,0
.Lc18:
	.long	.Lc20-.Lc19
.Lc19:
	.quad	.Lc16
	.quad	.Lc1
	.quad	.Lc2-.Lc1
	.byte	4
	.long	.Lc3-.Lc1
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc4-.Lc3
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc5-.Lc4
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc20:
	.long	.Lc22-.Lc21
.Lc21:
	.quad	.Lc16
	.quad	.Lc6
	.quad	.Lc7-.Lc6
	.byte	4
	.long	.Lc8-.Lc6
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc9-.Lc8
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc10-.Lc9
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc22:
	.long	.Lc24-.Lc23
.Lc23:
	.quad	.Lc16
	.quad	.Lc11
	.quad	.Lc12-.Lc11
	.byte	4
	.long	.Lc13-.Lc11
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc14-.Lc13
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc15-.Lc14
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc24:
# End asmlist al_dwarf_frame
.section .note.GNU-stack,"",%progbits

