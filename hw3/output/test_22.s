	.text
foo:
	movq	%rdi, -24(%rbp)
	subq	$3, %rsp
	.text
main:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	subq	$4, %rsp