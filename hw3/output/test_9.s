	.text
main:
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	subq	$3, %rsp