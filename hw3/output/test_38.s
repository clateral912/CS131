	.text
factorial:
	movq	%rdi, -64(%rbp)
	subq	$8, %rsp
	.text
main:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	subq	$4, %rsp