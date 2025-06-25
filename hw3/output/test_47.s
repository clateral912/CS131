	.data
	.globl	format
format:
	.asciz	"test alignment"
	.text
main:
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$6, %rsp