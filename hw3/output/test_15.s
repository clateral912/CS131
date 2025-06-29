	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -64(%rbp)
	movq	%rsi, -72(%rbp)
	subq	$72, %rsp
	subq	$8, %rsp
	movq	%rsp, -56(%rbp)
	movq	$17, %rax
	movq	-56(%rbp), %r10
	movq	%r10, (%rax)
	subq	$8, %rsp
	movq	%rsp, -40(%rbp)
	movq	-56(%rbp), %rax
	movq	-40(%rbp), %r10
	movq	%r10, (%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	