	.text
foo:
	subq	$1, %rsp
	.text
bar:
	subq	$1, %rsp
	.text
main:
	movq	%rdi, -128(%rbp)
	movq	%rsi, -136(%rbp)
	subq	$17, %rsp