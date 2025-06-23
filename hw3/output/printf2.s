	.text
	.file	"printf2.ll"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	format@GOTPCREL(%rip), %rdi
	callq	printf@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	format,@object                  # @format
	.data
	.globl	format
format:
	.asciz	"test alignment"
	.size	format, 15

	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym format
