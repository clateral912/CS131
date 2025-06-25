	.data
	.globl	format
format:
	.asciz	"test alignment"
	.text
main:
	movq	%rdi, -32(%rbp)
	movq	%rsi, -40(%rbp)
	subq	$5, %rsp