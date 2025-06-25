	.text
main:
	movq	%rdi, -64(%rbp)
	movq	%rsi, -72(%rbp)
	subq	$9, %rsp