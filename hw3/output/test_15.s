	.text
f1:
	movq	%rdi, -48(%rbp)
	subq	$6, %rsp
	.text
f2:
	movq	%rdi, -48(%rbp)
	subq	$6, %rsp
	.text
main:
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$6, %rsp