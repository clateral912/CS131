	.text
main:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	subq	$4, %rsp