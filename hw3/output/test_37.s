	.text
factorial:
	movq	%rdi, -152(%rbp)
	subq	$19, %rsp
	.text
main:
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$6, %rsp