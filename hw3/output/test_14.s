	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$48, %rsp
	subq	$8, %rsp
	movq	%rsp, -32(%rbp)
	movq	$17, %rax
	movq	-32(%rbp), %r10
	movq	%r10, (%rax)
	movq	-32(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	