	.text
	.globl	foo
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -48(%rbp)
	movq	%rsi, -56(%rbp)
	subq	$56, %rsp
	subq	$8, %rsp
	movq	%rsp, -40(%rbp)
	movq	-56(%rbp), %r11
	movq	-48(%rbp), %rcx
	addq	%rcx, %r11
	movq	%r11, -32(%rbp)
	movq	-40(%rbp), %rax
	movq	-32(%rbp), %r10
	movq	%r10, (%rax)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$48, %rsp
	movq	$ll_callback.foo, %rdi
	callq	ll_callback
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	callq	ll_ltoa
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	callq	ll_puts
	movq	%rax, -16(%rbp)
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	