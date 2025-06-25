	.text
foo:
	movq	%rdi, -48(%rbp)
	movq	%rsi, -56(%rbp)
	subq	$7, %rsp
	.text
main:
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$6, %rsp