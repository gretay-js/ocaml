	.file ""
	.section .rodata.cst8,"a",@progbits
	.align	16
caml_negf_mask:
	.quad	0x8000000000000000
	.quad	0
	.align	16
caml_absf_mask:
	.quad	0x7fffffffffffffff
	.quad	-1
	.data
	.globl	camlReg_availability_set__data_begin
camlReg_availability_set__data_begin:
	.text
	.globl	camlReg_availability_set__code_begin
camlReg_availability_set__code_begin:
	.data
	.globl	camlReg_availability_set__gc_roots
camlReg_availability_set__gc_roots:
	.quad	0
	.text
	.align	16
	.globl	camlReg_availability_set__print_483
camlReg_availability_set__print_483:
	.cfi_startproc
.L101:
	movq	%rax, %rsi
	movq	%rbx, %rdx
	movq	24(%rdi), %rax
	movq	%rsi, %rbx
	movq	%rdx, %rdi
	.file	1	"asmcomp/debug/reg_availability_set.ml"
	.loc	1	105	8
	jmp	camlReg_with_debug_info__print_786@PLT
	.cfi_endproc
	.type camlReg_availability_set__print_483,@function
	.size camlReg_availability_set__print_483,. - camlReg_availability_set__print_483
	.text
	.align	16
	.globl	camlReg_availability_set__pp_print_list_495
camlReg_availability_set__pp_print_list_495:
	.cfi_startproc
.L103:
	movq	%rax, %rdx
	movq	%rbx, %rsi
	movq	24(%rdi), %rbx
	movq	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a104$2c36$2d$2d76$5d_295_closure@GOTPCREL(%rip), %rax
	movq	%rdx, %rdi
	.loc	1	104	6
	jmp	camlStdlib__format__pp_print_list_inner_2847@PLT
	.cfi_endproc
	.type camlReg_availability_set__pp_print_list_495,@function
	.size camlReg_availability_set__pp_print_list_495,. - camlReg_availability_set__pp_print_list_495
	.text
	.align	16
	.globl	camlReg_availability_set__inter_14
camlReg_availability_set__inter_14:
	.loc	1	23	10
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L107:
	cmpq	$1, %rax
	je	.L105
	cmpq	$1, %rbx
	je	.L106
	.loc	1	27	15
	movq	(%rbx), %rdi
	.loc	1	27	4
	movq	(%rax), %rbx
	.loc	1	29	18
.L108:
	subq	$40, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L109
	leaq	8(%r15), %rax
	movq	$4343, -8(%rax)
	movq	caml_curry2@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$5, 8(%rax)
	movq	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a29$2c18$2d$2d1306$5d_24@GOTPCREL(%rip), %rsi
	movq	%rsi, 16(%rax)
	movq	%rdi, 24(%rax)
	movq	$1, %rdi
	.loc	1	29	6
	call	camlStdlib__set__fold_866@PLT
.L104:
	movq	%rax, %rbx
	.loc	1	60	4
.L111:
	subq	$16, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L112
	leaq	8(%r15), %rax
	movq	$1024, -8(%rax)
	movq	%rbx, (%rax)
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
	.align	4
.L106:
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
	.align	4
.L105:
	movq	%rbx, %rax
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
.L112:
	call	caml_call_gc@PLT
.L113:
	jmp	.L111
.L109:
	call	caml_call_gc@PLT
.L110:
	jmp	.L108
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.type camlReg_availability_set__inter_14,@function
	.size camlReg_availability_set__inter_14,. - camlReg_availability_set__inter_14
	.text
	.align	16
	.globl	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a29$2c18$2d$2d1306$5d_24
camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a29$2c18$2d$2d1306$5d_24:
	.loc	1	29	18
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L124:
	movq	%rax, (%rsp)
	movq	%rbx, 16(%rsp)
	.cfi_adjust_cfa_offset 16
	subq	$16, %rsp
	movq	%r14, (%rsp)
	leaq	.L123(%rip), %r14
	movq	%r14, 8(%rsp)
	movq	%rsp, %r14
	.file	2	"asmcomp/debug/reg_with_debug_info.ml"
	.loc	2	102	12
	movq	(%rax), %rbx
	movq	24(%rdi), %rax
	.loc	1	30	16
	call	camlReg_with_debug_info__find_reg_exn_680@PLT
.L114:
	popq	%r14
	.cfi_adjust_cfa_offset -8
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	jmp	.L117
	.align	4
.L123:
	movq	caml_exn_Not_found@GOTPCREL(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.L122
	movq	16(%rsp), %rax
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	ret
	.cfi_adjust_cfa_offset 24
	.align	4
.L122:
	call	caml_raise_exn@PLT
.L125:
	.align	4
.L117:
	movq	(%rsp), %rbx
	.loc	2	129	19
	movq	8(%rbx), %rbx
	.loc	2	129	19
	movq	8(%rax), %rdi
	cmpq	$1, %rbx
	je	.L120
	cmpq	$1, %rdi
	je	.L119
	.loc	1	45	31
	movq	(%rbx), %rax
	movq	%rax, 8(%rsp)
	.loc	1	46	34
	movq	(%rdi), %rbx
	.loc	1	47	19
	call	camlReg_with_debug_info__compare_54@PLT
.L115:
	cmpq	$1, %rax
	jne	.L121
	.loc	1	48	18
.L126:
	subq	$16, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L127
	leaq	8(%r15), %rbx
	movq	$1024, -8(%rbx)
	movq	8(%rsp), %rax
	movq	%rax, (%rbx)
	jmp	.L118
	.align	4
.L121:
	movq	$1, %rbx
	jmp	.L118
	.align	4
.L120:
	cmpq	$1, %rdi
	jne	.L119
	movq	$1, %rbx
	jmp	.L118
	.align	4
.L119:
	movq	$1, %rbx
.L118:
	.loc	2	88	2
.L129:
	subq	$24, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L130
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	(%rsp), %rdi
	.loc	2	102	12
	movq	(%rdi), %rdi
	movq	%rdi, (%rax)
	movq	%rbx, 8(%rax)
	movq	camlReg_with_debug_info__Pmakeblock_1094@GOTPCREL(%rip), %rbx
	movq	24(%rbx), %rdi
	movq	16(%rsp), %rbx
	.loc	1	56	12
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	camlStdlib__set__add_169@PLT
	.cfi_adjust_cfa_offset 24
.L130:
	call	caml_call_gc@PLT
.L131:
	jmp	.L129
.L127:
	call	caml_call_gc@PLT
.L128:
	jmp	.L126
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a29$2c18$2d$2d1306$5d_24,@function
	.size camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a29$2c18$2d$2d1306$5d_24,. - camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a29$2c18$2d$2d1306$5d_24
	.text
	.align	16
	.globl	camlReg_availability_set__equal_118
camlReg_availability_set__equal_118:
	.loc	1	62	10
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L135:
	cmpq	$1, %rax
	je	.L134
	cmpq	$1, %rbx
	je	.L133
	.loc	1	66	14
	movq	(%rbx), %rbx
	.loc	1	65	24
	movq	(%rax), %rax
	movq	camlReg_with_debug_info__Pmakeblock_1094@GOTPCREL(%rip), %rdi
	movq	80(%rdi), %rdi
	movq	24(%rdi), %rdi
	.file	3	"set.ml"
	.loc	3	324	6
	call	camlStdlib__set__compare_758@PLT
.L132:
	.loc	3	324	6
	cmpq	$1, %rax
	sete	%al
	movzbq	%al, %rax
	.loc	3	324	6
	salq	$1, %rax
	.loc	3	324	6
	incq	%rax
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
	.align	4
.L134:
	cmpq	$1, %rbx
	jne	.L133
	movq	$3, %rax
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
	.align	4
.L133:
	movq	$1, %rax
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.type camlReg_availability_set__equal_118,@function
	.size camlReg_availability_set__equal_118,. - camlReg_availability_set__equal_118
	.text
	.align	16
	.globl	camlReg_availability_set__canonicalise_140
camlReg_availability_set__canonicalise_140:
	.loc	1	68	17
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L140:
	cmpq	$1, %rax
	je	.L139
	movq	%rax, (%rsp)
	movq	camlStdlib__hashtbl__create_inner_2077@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	movq	$85, %rbx
	movq	$1, %rax
	.file	4	"hashtbl.ml"
	.loc	4	593	20
	call	camlStdlib__hashtbl__create_inner_248@PLT
.L136:
	movq	%rax, %rdi
	movq	%rdi, 8(%rsp)
	movq	(%rsp), %rax
	.loc	1	71	4
	movq	(%rax), %rbx
	.loc	1	73	16
.L141:
	subq	$32, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L142
	leaq	8(%r15), %rax
	movq	$3319, -8(%rax)
	movq	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a73$2c16$2d$2d815$5d_156@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	%rdi, 16(%rax)
	.loc	1	73	4
	call	camlStdlib__set__iter_847@PLT
.L137:
	movq	$1, %rdi
	movq	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a93$2c21$2d$2d91$5d_243_closure@GOTPCREL(%rip), %rax
	movq	8(%rsp), %rbx
	.loc	1	93	6
	call	camlStdlib__hashtbl__fold_1155@PLT
.L138:
	movq	%rax, %rbx
	.loc	1	98	4
.L144:
	subq	$16, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L145
	leaq	8(%r15), %rax
	movq	$1024, -8(%rax)
	movq	%rbx, (%rax)
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	ret
	.cfi_adjust_cfa_offset 24
	.align	4
.L139:
	movq	$1, %rax
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	ret
	.cfi_adjust_cfa_offset 24
.L145:
	call	caml_call_gc@PLT
.L146:
	jmp	.L144
.L142:
	call	caml_call_gc@PLT
.L143:
	jmp	.L141
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlReg_availability_set__canonicalise_140,@function
	.size camlReg_availability_set__canonicalise_140,. - camlReg_availability_set__canonicalise_140
	.text
	.align	16
	.globl	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a73$2c16$2d$2d815$5d_156
camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a73$2c16$2d$2d815$5d_156:
	.loc	1	73	16
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L157:
	.loc	2	129	19
	movq	8(%rax), %rdi
	cmpq	$1, %rdi
	je	.L151
	.loc	1	76	10
	movq	(%rdi), %rdi
	.loc	2	34	25
	movq	(%rdi), %rdi
	.file	5	"typing/ident.ml"
	.loc	5	51	20
	movq	(%rdi), %rsi
	cmpq	$1, %rsi
	jne	.L156
	movq	$1, %rax
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	ret
	.cfi_adjust_cfa_offset 24
	.align	4
.L156:
	movq	%rdi, 8(%rsp)
	movq	%rbx, (%rsp)
	movq	%rax, 16(%rsp)
	.cfi_adjust_cfa_offset 16
	subq	$16, %rsp
	movq	%r14, (%rsp)
	leaq	.L155(%rip), %r14
	movq	%r14, 8(%rsp)
	movq	%rsp, %r14
	movq	16(%rbx), %rax
	movq	%rdi, %rbx
	.loc	1	79	18
	call	camlIdent__find_2679@PLT
.L147:
	popq	%r14
	.cfi_adjust_cfa_offset -8
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	jmp	.L152
	.align	4
.L155:
	movq	caml_exn_Not_found@GOTPCREL(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.L154
	movq	(%rsp), %rax
	movq	16(%rax), %rax
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rdi
	.loc	1	80	37
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	camlIdent__add_2526@PLT
	.cfi_adjust_cfa_offset 24
	.align	4
.L154:
	call	caml_raise_exn@PLT
.L158:
	.align	4
.L152:
	movq	16(%rsp), %rbx
	.loc	2	103	17
	movq	(%rbx), %rbx
	.loc	2	103	17
	movq	24(%rbx), %rbx
	.loc	2	103	17
	movq	(%rax), %rax
	.loc	2	103	17
	movq	24(%rax), %rax
	testb	$1, %bl
	jne	.L153
	movzbq	-8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.L153
	testb	$1, %al
	jne	.L153
	movzbq	-8(%rax), %rax
	testq	%rax, %rax
	jne	.L153
	movq	(%rsp), %rax
	movq	16(%rax), %rax
	movq	8(%rsp), %rbx
	.loc	1	87	16
	call	camlIdent__remove_2604@PLT
.L149:
	movq	(%rsp), %rax
	movq	16(%rax), %rax
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rdi
	.loc	1	88	16
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	camlIdent__add_2526@PLT
	.cfi_adjust_cfa_offset 24
	.align	4
.L153:
	movq	$1, %rax
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	ret
	.cfi_adjust_cfa_offset 24
	.align	4
.L151:
	movq	$1, %rax
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	ret
	.cfi_adjust_cfa_offset 24
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a73$2c16$2d$2d815$5d_156,@function
	.size camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a73$2c16$2d$2d815$5d_156,. - camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a73$2c16$2d$2d815$5d_156
	.text
	.align	16
	.globl	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a93$2c21$2d$2d91$5d_243
camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a93$2c21$2d$2d91$5d_243:
	.loc	1	93	21
	.cfi_startproc
.L160:
	movq	%rbx, %rax
	movq	%rdi, %rbx
	movq	camlReg_with_debug_info__Pmakeblock_1094@GOTPCREL(%rip), %rdi
	movq	24(%rdi), %rdi
	.loc	1	94	10
	jmp	camlStdlib__set__add_169@PLT
	.cfi_endproc
	.type camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a93$2c21$2d$2d91$5d_243,@function
	.size camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a93$2c21$2d$2d91$5d_243,. - camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a93$2c21$2d$2d91$5d_243
	.text
	.align	16
	.globl	camlReg_availability_set__print_272
camlReg_availability_set__print_272:
	.loc	1	100	10
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L166:
	cmpq	$1, %rdi
	je	.L165
	movq	%rbx, 8(%rsp)
	movq	%rax, (%rsp)
	.loc	1	102	4
	movq	(%rdi), %rbx
	movq	$1, %rax
	.loc	3	389	6
	call	camlStdlib__set__elements_aux_1019@PLT
.L162:
	movq	%rax, 16(%rsp)
.L167:
	subq	$80, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L168
	leaq	8(%r15), %rax
	movq	$4343, -8(%rax)
	movq	caml_curry2@GOTPCREL(%rip), %rbx
	movq	%rbx, (%rax)
	movq	$5, 8(%rax)
	movq	camlReg_availability_set__print_483@GOTPCREL(%rip), %rdi
	movq	%rdi, 16(%rax)
	movq	(%rsp), %rdi
	movq	%rdi, 24(%rax)
	leaq	40(%rax), %rdi
	movq	%rdi, (%rsp)
	movq	$4343, -8(%rdi)
	movq	%rbx, (%rdi)
	movq	$5, 8(%rdi)
	movq	camlReg_availability_set__pp_print_list_495@GOTPCREL(%rip), %rbx
	movq	%rbx, 16(%rdi)
	movq	%rax, 24(%rdi)
	movq	camlReg_availability_set__const_block_293@GOTPCREL(%rip), %rdi
	movq	camlStdlib__format__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fstdlib$2fformat$2eml$3a1343$2c27$2d$2d33$5d_3454_closure@GOTPCREL(%rip), %rax
	movq	8(%rsp), %rbx
	.file	6	"format.ml"
	.loc	6	1343	18
	call	camlStdlib__format__kfprintf_3406@PLT
.L163:
	movq	%rax, %rdi
	movq	(%rsp), %rax
	movq	16(%rsp), %rbx
	.loc	1	103	4
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
	.align	4
.L165:
	movq	camlReg_availability_set__const_block_282@GOTPCREL(%rip), %rdi
	movq	camlStdlib__format__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fstdlib$2fformat$2eml$3a1343$2c27$2d$2d33$5d_3454_closure@GOTPCREL(%rip), %rax
	.loc	6	1343	18
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	camlStdlib__format__kfprintf_3406@PLT
	.cfi_adjust_cfa_offset 24
.L168:
	call	caml_call_gc@PLT
.L169:
	jmp	.L167
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlReg_availability_set__print_272,@function
	.size camlReg_availability_set__print_272,. - camlReg_availability_set__print_272
	.text
	.align	16
	.globl	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a104$2c36$2d$2d76$5d_295
camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a104$2c36$2d$2d76$5d_295:
	.loc	1	104	36
	.cfi_startproc
.L171:
	movq	%rax, %rbx
	movq	camlReg_availability_set__const_block_305@GOTPCREL(%rip), %rdi
	movq	camlStdlib__format__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fstdlib$2fformat$2eml$3a1343$2c27$2d$2d33$5d_3454_closure@GOTPCREL(%rip), %rax
	.loc	6	1343	18
	jmp	camlStdlib__format__kfprintf_3406@PLT
	.cfi_endproc
	.type camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a104$2c36$2d$2d76$5d_295,@function
	.size camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a104$2c36$2d$2d76$5d_295,. - camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a104$2c36$2d$2d76$5d_295
	.data
	.quad	4087
	.globl	camlReg_availability_set__inter_343
camlReg_availability_set__inter_343:
	.globl	camlReg_availability_set__inter_14_closure
camlReg_availability_set__inter_14_closure:
	.quad	caml_curry2
	.quad	5
	.quad	camlReg_availability_set__inter_14
	.data
	.quad	4087
	.globl	camlReg_availability_set__equal_344
camlReg_availability_set__equal_344:
	.globl	camlReg_availability_set__equal_118_closure
camlReg_availability_set__equal_118_closure:
	.quad	caml_curry2
	.quad	5
	.quad	camlReg_availability_set__equal_118
	.data
	.quad	3063
	.globl	camlReg_availability_set__canonicalise_345
camlReg_availability_set__canonicalise_345:
	.globl	camlReg_availability_set__canonicalise_140_closure
camlReg_availability_set__canonicalise_140_closure:
	.quad	camlReg_availability_set__canonicalise_140
	.quad	3
	.data
	.quad	4087
	.globl	camlReg_availability_set__set_of_closures_346
camlReg_availability_set__set_of_closures_346:
	.globl	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a93$2c21$2d$2d91$5d_243_closure
camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a93$2c21$2d$2d91$5d_243_closure:
	.quad	caml_curry3
	.quad	7
	.quad	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a93$2c21$2d$2d91$5d_243
	.data
	.quad	4087
	.globl	camlReg_availability_set__print_347
camlReg_availability_set__print_347:
	.globl	camlReg_availability_set__print_272_closure
camlReg_availability_set__print_272_closure:
	.quad	caml_curry3
	.quad	7
	.quad	camlReg_availability_set__print_272
	.data
	.quad	4087
	.globl	camlReg_availability_set__set_of_closures_348
camlReg_availability_set__set_of_closures_348:
	.globl	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a104$2c36$2d$2d76$5d_295_closure
camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a104$2c36$2d$2d76$5d_295_closure:
	.quad	caml_curry2
	.quad	5
	.quad	camlReg_availability_set__anon_fn$5b$2fusr$2flocal$2fhome$2fgyorsh$2fdev$2focaml$2fasmcomp$2fdebug$2freg_availability_set$2eml$3a104$2c36$2d$2d76$5d_295
	.data
	.quad	4864
	.globl	camlReg_availability_set
camlReg_availability_set:
	.quad	camlReg_availability_set__inter_14_closure
	.quad	camlReg_availability_set__canonicalise_140_closure
	.quad	camlReg_availability_set__equal_118_closure
	.quad	camlReg_availability_set__print_272_closure
	.data
	.data
	.data
	.data
	.data
	.data
	.data
	.quad	2816
	.globl	camlReg_availability_set__const_block_305
camlReg_availability_set__const_block_305:
	.quad	camlReg_availability_set__const_block_303
	.quad	camlReg_availability_set__const_string_304
	.data
	.quad	2044
	.globl	camlReg_availability_set__const_string_304
camlReg_availability_set__const_string_304:
	.ascii	",@ "
	.space	4
	.byte	4
	.data
	.quad	2828
	.globl	camlReg_availability_set__const_block_303
camlReg_availability_set__const_block_303:
	.quad	89
	.quad	camlReg_availability_set__const_block_302
	.data
	.quad	2833
	.globl	camlReg_availability_set__const_block_302
camlReg_availability_set__const_block_302:
	.quad	camlReg_availability_set__const_block_301
	.quad	1
	.data
	.quad	3840
	.globl	camlReg_availability_set__const_block_301
camlReg_availability_set__const_block_301:
	.quad	camlReg_availability_set__const_string_300
	.quad	3
	.quad	1
	.data
	.quad	2044
	.globl	camlReg_availability_set__const_string_300
camlReg_availability_set__const_string_300:
	.ascii	"@ "
	.space	5
	.byte	5
	.data
	.quad	2816
	.globl	camlReg_availability_set__const_block_293
camlReg_availability_set__const_block_293:
	.quad	camlReg_availability_set__const_block_291
	.quad	camlReg_availability_set__const_string_292
	.data
	.quad	2044
	.globl	camlReg_availability_set__const_string_292
camlReg_availability_set__const_string_292:
	.ascii	"{%a}"
	.space	3
	.byte	3
	.data
	.quad	2828
	.globl	camlReg_availability_set__const_block_291
camlReg_availability_set__const_block_291:
	.quad	247
	.quad	camlReg_availability_set__const_block_290
	.data
	.quad	1807
	.globl	camlReg_availability_set__const_block_290
camlReg_availability_set__const_block_290:
	.quad	camlReg_availability_set__const_block_289
	.data
	.quad	2828
	.globl	camlReg_availability_set__const_block_289
camlReg_availability_set__const_block_289:
	.quad	251
	.quad	1
	.data
	.quad	2816
	.globl	camlReg_availability_set__const_block_282
camlReg_availability_set__const_block_282:
	.quad	camlReg_availability_set__const_block_280
	.quad	camlReg_availability_set__const_string_281
	.data
	.quad	3068
	.globl	camlReg_availability_set__const_string_281
camlReg_availability_set__const_string_281:
	.ascii	"<unreachable>"
	.space	2
	.byte	2
	.data
	.quad	2827
	.globl	camlReg_availability_set__const_block_280
camlReg_availability_set__const_block_280:
	.quad	camlReg_availability_set__const_string_279
	.quad	1
	.data
	.quad	3068
	.globl	camlReg_availability_set__const_string_279
camlReg_availability_set__const_string_279:
	.ascii	"<unreachable>"
	.space	2
	.byte	2
	.text
	.align	16
	.globl	camlReg_availability_set__entry
camlReg_availability_set__entry:
	.cfi_startproc
.L172:
	movq	$1, %rax
	ret
	.cfi_endproc
	.type camlReg_availability_set__entry,@function
	.size camlReg_availability_set__entry,. - camlReg_availability_set__entry
	.data
	.text
	.globl	camlReg_availability_set__code_end
camlReg_availability_set__code_end:
	.data
				/* relocation table start */
	.align	8
				/* relocation table end */
	.data
	.quad	0
	.globl	camlReg_availability_set__data_end
camlReg_availability_set__data_end:
	.quad	0
	.align	8
	.globl	camlReg_availability_set__frametable
camlReg_availability_set__frametable:
	.quad	20
	.quad	.L163
	.word	33
	.word	2
	.word	0
	.word	16
	.align	8
	.quad	.L173
	.quad	.L169
	.word	32
	.word	3
	.word	0
	.word	8
	.word	16
	.align	8
	.quad	.L162
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L175
	.quad	.L149
	.word	33
	.word	3
	.word	0
	.word	8
	.word	16
	.align	8
	.quad	.L177
	.quad	.L158
	.word	32
	.word	0
	.align	8
	.quad	.L147
	.word	49
	.word	3
	.word	16
	.word	24
	.word	32
	.align	8
	.quad	.L178
	.quad	.L146
	.word	32
	.word	1
	.word	3
	.align	8
	.quad	.L138
	.word	33
	.word	0
	.align	8
	.quad	.L179
	.quad	.L137
	.word	33
	.word	1
	.word	8
	.align	8
	.quad	.L180
	.quad	.L143
	.word	32
	.word	3
	.word	3
	.word	5
	.word	8
	.align	8
	.quad	.L136
	.word	33
	.word	1
	.word	0
	.align	8
	.quad	.L181
	.quad	.L132
	.word	17
	.word	0
	.align	8
	.quad	.L183
	.quad	.L131
	.word	32
	.word	3
	.word	0
	.word	3
	.word	16
	.align	8
	.quad	.L128
	.word	32
	.word	3
	.word	0
	.word	8
	.word	16
	.align	8
	.quad	.L115
	.word	33
	.word	3
	.word	0
	.word	8
	.word	16
	.align	8
	.quad	.L185
	.quad	.L125
	.word	32
	.word	0
	.align	8
	.quad	.L114
	.word	49
	.word	2
	.word	16
	.word	32
	.align	8
	.quad	.L186
	.quad	.L113
	.word	16
	.word	1
	.word	3
	.align	8
	.quad	.L104
	.word	17
	.word	0
	.align	8
	.quad	.L187
	.quad	.L110
	.word	16
	.word	2
	.word	3
	.word	5
	.align	8
	.align	8
.L183:
	.long	(.L188 - .) + 1275068416
	.long	1327200
	.quad	.L184
	.align	8
.L181:
	.long	(.L189 - .) + -1409286144
	.long	2429248
	.quad	.L182
	.align	8
.L182:
	.long	(.L190 - .) + -1409286144
	.long	295296
	.quad	0
	.align	8
.L176:
	.long	(.L190 - .) + -1879048192
	.long	434272
	.quad	0
	.align	8
.L187:
	.long	(.L190 - .) + -67108864
	.long	118895
	.quad	0
	.align	8
.L173:
	.long	(.L191 - .) + -1811939328
	.long	5501216
	.quad	.L174
	.align	8
.L178:
	.long	(.L190 - .) + -872415232
	.long	323872
	.quad	0
	.align	8
.L185:
	.long	(.L190 - .) + 0
	.long	192817
	.quad	0
	.align	8
.L184:
	.long	(.L190 - .) + -939524096
	.long	270752
	.quad	0
	.align	8
.L180:
	.long	(.L190 - .) + 134217728
	.long	299085
	.quad	0
	.align	8
.L174:
	.long	(.L190 - .) + -67108864
	.long	421954
	.quad	0
	.align	8
.L186:
	.long	(.L190 - .) + -536870912
	.long	123136
	.quad	0
	.align	8
.L175:
	.long	(.L188 - .) + 1543503872
	.long	1593440
	.quad	.L176
	.align	8
.L177:
	.long	(.L190 - .) + -872415232
	.long	356608
	.quad	0
	.align	8
.L179:
	.long	(.L190 - .) + 402653184
	.long	381026
	.quad	0
.L190:
	.ascii	"asmcomp/debug/reg_availability_set.ml\0"
	.align	8
.L191:
	.ascii	"format.ml\0"
	.align	8
.L189:
	.ascii	"hashtbl.ml\0"
	.align	8
.L188:
	.ascii	"set.ml\0"
	.align	8
	.section .note.GNU-stack,"",%progbits
