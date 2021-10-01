.section .data
.align 8
.align 1
.Lu1Aj_srt:
	.quad	stg_SRT_2_info
	.quad	base_GHCziBase_zpzp_closure
	.quad	ghczmprim_GHCziCString_unpackCStringzh_closure
	.quad	0
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1zX_str:
	.asciz "!"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.Ls1zt_info)+0
.Ls1zt_info:
.Lc1zY:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1zZ
.Lc1A0:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1zX_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1zZ:
	jmp *-16(%r13)
	.size .Ls1zt_info, .-.Ls1zt_info
.section .text
.align 8
.align 8
	.quad	1
	.long	16
	.long	.Lu1Aj_srt-(.Ls1zu_info)+0
.Ls1zu_info:
.Lc1A1:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1A2
.Lc1A3:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1A5
.Lc1A4:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 16(%rbx),%rax
	movq $.Ls1zt_info,-8(%r12)
	leaq -8(%r12),%rbx
	movq %rbx,%rsi
	movq %rax,%r14
	movl $base_GHCziBase_zpzp_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.Lc1A5:
	movq $16,904(%r13)
.Lc1A2:
	jmp *-16(%r13)
	.size .Ls1zu_info, .-.Ls1zu_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1Aa_str:
	.asciz "Hello, "
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.Ls1zs_info)+0
.Ls1zs_info:
.Lc1Ab:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1Ac
.Lc1Ad:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1Aa_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1Ac:
	jmp *-16(%r13)
	.size .Ls1zs_info, .-.Ls1zs_info
.section .text
.align 8
.align 8
	.quad	4294967301
	.quad	0
	.long	14
	.long	.Lu1Aj_srt-(.LrrJ_info)+0
.LrrJ_info:
.Lc1Ae:
.Lc1Ag:
	addq $40,%r12
	cmpq 856(%r13),%r12
	ja .Lc1Ai
.Lc1Ah:
	movq $.Ls1zu_info,-32(%r12)
	movq %r14,-16(%r12)
	leaq -32(%r12),%rax
	movq $.Ls1zs_info,-8(%r12)
	leaq -8(%r12),%rbx
	movq %rax,%rsi
	movq %rbx,%r14
	movl $base_GHCziBase_zpzp_closure,%ebx
	jmp stg_ap_pp_fast
.Lc1Ai:
	movq $40,904(%r13)
.Lc1Af:
	movl $.LrrJ_closure,%ebx
	jmp *-8(%r13)
	.size .LrrJ_info, .-.LrrJ_info
.section .data
.align 8
.align 1
.LrrJ_closure:
	.quad	.LrrJ_info
	.quad	0
.section .data
.align 8
.align 1
.Lu1BU_srt:
	.quad	stg_SRT_4_info
	.quad	ghczmprim_GHCziCString_unpackCStringUtf8zh_closure
	.quad	ghczmprim_GHCziCString_unpackCStringzh_closure
	.quad	base_GHCziBase_map_closure
	.quad	.LrrJ_closure
	.quad	0
.section .data
.align 8
.align 1
.Lu1BV_srt:
	.quad	stg_SRT_3_info
	.quad	base_GHCziBase_zd_closure
	.quad	base_DataziOldList_unlines_closure
	.quad	.Lu1BU_srt
	.quad	0
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1AN_str:
	.asciz "Josephine"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.Ls1zB_info)+0
.Ls1zB_info:
.Lc1AO:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1AP
.Lc1AQ:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1AN_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1AP:
	jmp *-16(%r13)
	.size .Ls1zB_info, .-.Ls1zB_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1AW_str:
	.asciz "Josefina"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.Ls1zA_info)+0
.Ls1zA_info:
.Lc1AX:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1AY
.Lc1AZ:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1AW_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1AY:
	jmp *-16(%r13)
	.size .Ls1zA_info, .-.Ls1zA_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1B5_str:
	.asciz "Iosephus"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.Ls1zz_info)+0
.Ls1zz_info:
.Lc1B6:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1B7
.Lc1B8:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1B5_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1B7:
	jmp *-16(%r13)
	.size .Ls1zz_info, .-.Ls1zz_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1Be_str:
	.asciz "Giuseppe"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.Ls1zy_info)+0
.Ls1zy_info:
.Lc1Bf:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1Bg
.Lc1Bh:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1Be_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1Bg:
	jmp *-16(%r13)
	.size .Ls1zy_info, .-.Ls1zy_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1Bn_str:
	.asciz "Josephine"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.Ls1zx_info)+0
.Ls1zx_info:
.Lc1Bo:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1Bp
.Lc1Bq:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1Bn_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1Bp:
	jmp *-16(%r13)
	.size .Ls1zx_info, .-.Ls1zx_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1Bw_str:
	.asciz "José"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringUtf8zh_closure-(.Ls1zw_info)+0
.Ls1zw_info:
.Lc1Bx:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1By
.Lc1Bz:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1Bw_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringUtf8zh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1By:
	jmp *-16(%r13)
	.size .Ls1zw_info, .-.Ls1zw_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1BF_str:
	.asciz "Joseph"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.Ls1zv_info)+0
.Ls1zv_info:
.Lc1BG:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1BH
.Lc1BI:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c1BF_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1BH:
	jmp *-16(%r13)
	.size .Ls1zv_info, .-.Ls1zv_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.Lu1BU_srt-(.Ls1zJ_info)+0
.Ls1zJ_info:
.Lc1BK:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1BL
.Lc1BM:
	addq $280,%r12
	cmpq 856(%r13),%r12
	ja .Lc1BO
.Lc1BN:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $.Ls1zB_info,-272(%r12)
	leaq -272(%r12),%rax
	movq $ghczmprim_GHCziTypes_ZC_con_info,-256(%r12)
	movq %rax,-248(%r12)
	movq $ghczmprim_GHCziTypes_ZMZN_closure+1,-240(%r12)
	leaq -254(%r12),%rax
	movq $.Ls1zA_info,-232(%r12)
	leaq -232(%r12),%rbx
	movq $ghczmprim_GHCziTypes_ZC_con_info,-216(%r12)
	movq %rbx,-208(%r12)
	movq %rax,-200(%r12)
	leaq -214(%r12),%rax
	movq $.Ls1zz_info,-192(%r12)
	leaq -192(%r12),%rbx
	movq $ghczmprim_GHCziTypes_ZC_con_info,-176(%r12)
	movq %rbx,-168(%r12)
	movq %rax,-160(%r12)
	leaq -174(%r12),%rax
	movq $.Ls1zy_info,-152(%r12)
	leaq -152(%r12),%rbx
	movq $ghczmprim_GHCziTypes_ZC_con_info,-136(%r12)
	movq %rbx,-128(%r12)
	movq %rax,-120(%r12)
	leaq -134(%r12),%rax
	movq $.Ls1zx_info,-112(%r12)
	leaq -112(%r12),%rbx
	movq $ghczmprim_GHCziTypes_ZC_con_info,-96(%r12)
	movq %rbx,-88(%r12)
	movq %rax,-80(%r12)
	leaq -94(%r12),%rax
	movq $.Ls1zw_info,-72(%r12)
	leaq -72(%r12),%rbx
	movq $ghczmprim_GHCziTypes_ZC_con_info,-56(%r12)
	movq %rbx,-48(%r12)
	movq %rax,-40(%r12)
	leaq -54(%r12),%rax
	movq $.Ls1zv_info,-32(%r12)
	leaq -32(%r12),%rbx
	movq $ghczmprim_GHCziTypes_ZC_con_info,-16(%r12)
	movq %rbx,-8(%r12)
	movq %rax,(%r12)
	leaq -14(%r12),%rax
	movq %rax,%rsi
	movl $.LrrJ_closure+1,%r14d
	movl $base_GHCziBase_map_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.Lc1BO:
	movq $280,904(%r13)
.Lc1BL:
	jmp *-16(%r13)
	.size .Ls1zJ_info, .-.Ls1zJ_info
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.Lu1BV_srt-(.Ls1zK_info)+0
.Ls1zK_info:
.Lc1BP:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1BQ
.Lc1BR:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1BT
.Lc1BS:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1AE
.Lc1AD:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movq $.Ls1zJ_info,-8(%r12)
	leaq -8(%r12),%rax
	movq %rax,%rsi
	movl $base_DataziOldList_unlines_closure,%r14d
	movl $base_GHCziBase_zd_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.Lc1AE:
	jmp *(%rbx)
.Lc1BT:
	movq $16,904(%r13)
.Lc1BQ:
	jmp *-16(%r13)
	.size .Ls1zK_info, .-.Ls1zK_info
.section .data
.align 8
.align 1
.Ls1zK_closure:
	.quad	.Ls1zK_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.Lu1CI_srt:
	.quad	stg_SRT_3_info
	.quad	base_GHCziBase_zd_closure
	.quad	base_SystemziIO_putStr_closure
	.quad	.Ls1zK_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.Lu1CI_srt-(Main_main_info)+0
.globl Main_main_info
.type Main_main_info, @function
Main_main_info:
.Lc1CF:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1CG
.Lc1CH:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1CE
.Lc1CD:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $.Ls1zK_closure,%esi
	movl $base_SystemziIO_putStr_closure,%r14d
	movl $base_GHCziBase_zd_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.Lc1CE:
	jmp *(%rbx)
.Lc1CG:
	jmp *-16(%r13)
	.size Main_main_info, .-Main_main_info
.section .data
.align 8
.align 1
.globl Main_main_closure
.type Main_main_closure, @object
Main_main_closure:
	.quad	Main_main_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.Lu1CY_srt:
	.quad	stg_SRT_2_info
	.quad	base_GHCziTopHandler_runMainIO_closure
	.quad	Main_main_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.Lu1CY_srt-(ZCMain_main_info)+0
.globl ZCMain_main_info
.type ZCMain_main_info, @function
ZCMain_main_info:
.Lc1CV:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1CW
.Lc1CX:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1CU
.Lc1CT:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $Main_main_closure,%r14d
	movl $base_GHCziTopHandler_runMainIO_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_p_fast
.Lc1CU:
	jmp *(%rbx)
.Lc1CW:
	jmp *-16(%r13)
	.size ZCMain_main_info, .-ZCMain_main_info
.section .data
.align 8
.align 1
.globl ZCMain_main_closure
.type ZCMain_main_closure, @object
ZCMain_main_closure:
	.quad	ZCMain_main_info
	.quad	0
	.quad	0
	.quad	0
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.Lr1mu_bytes:
	.asciz "main"
.section .data
.align 8
.align 1
.Lr1mP_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	.Lr1mu_bytes
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.Lr1mQ_bytes:
	.asciz "Main"
.section .data
.align 8
.align 1
.Lr1mR_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	.Lr1mQ_bytes
.section .data
.align 8
.align 1
.globl Main_zdtrModule_closure
.type Main_zdtrModule_closure, @object
Main_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	.Lr1mP_closure+1
	.quad	.Lr1mR_closure+1
	.quad	3
.section .note.GNU-stack,"",@progbits
.ident "GHC 8.8.4"


