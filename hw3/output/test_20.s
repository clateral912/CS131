	.text
foo:
	movq	%rdi, -16(%rbp)
	subq	$2, %rsp
	.text
main:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	subq	$4, %rsp