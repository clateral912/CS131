	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -32(%rbp)
	movq	%rsi, -40(%rbp)
	subq	$40, %rsp
	subq	$8, %rsp
	movq	%rsp, -24(%rbp)
	movq	$0, %rax
	movq	-24(%rbp), %r10
	movq	%r10, (%rax)
	movq	$42, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	