	.text
foo:
	movq	%rdi, -16(%rbp)
	subq	$2, %rsp
	.text
main:
	movq	%rdi, -32(%rbp)
	movq	%rsi, -40(%rbp)
	subq	$5, %rsp