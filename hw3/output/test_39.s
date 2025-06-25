	.text
factorial:
	movq	%rdi, -152(%rbp)
	subq	$19, %rsp
	.text
factorial2:
	movq	%rdi, -152(%rbp)
	subq	$19, %rsp
	.text
main:
	movq	%rdi, -56(%rbp)
	movq	%rsi, -64(%rbp)
	subq	$8, %rsp