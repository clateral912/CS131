	.text
bar:
	movq	%rdi, -72(%rbp)
	movq	%rsi, -80(%rbp)
	movq	%rdx, -88(%rbp)
	movq	%rcx, -96(%rbp)
	movq	%r8 , -104(%rbp)
	movq	%r9 , -112(%rbp)
	movq	16(%rbp), %rax
	movq	%rax, -120(%rbp)
	movq	24(%rbp), %rax
	movq	%rax, -128(%rbp)
	subq	$16, %rsp
	.text
foo:
	movq	%rdi, -24(%rbp)
	subq	$3, %rsp
	.text
main:
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	subq	$4, %rsp