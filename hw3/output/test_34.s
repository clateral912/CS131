	.data
	.globl	gbl
gbl:
	.quad	1
	.quad	2
	.quad	3
	.quad	4
	.quad	5
	.quad	6
	.quad	7
	.text
main:
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$6, %rsp