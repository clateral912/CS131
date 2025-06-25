	.text
main:
	movq	%rdi, -56(%rbp)
	movq	%rsi, -64(%rbp)
	subq	$8, %rsp