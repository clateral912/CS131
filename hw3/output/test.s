	.text
	.globl	factorial
factorial:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -152(%rbp)
	subq	$152, %rsp
	subq	$8, %rsp
	movq	%rsp, -144(%rbp)
	subq	$8, %rsp
	movq	%rsp, -136(%rbp)
	movq	-144(%rbp), %rax
	movq	-152(%rbp), %r10
	movq	%r10, (%rax)
	movq	-136(%rbp), %rax
	movq	$1, %r10
	movq	%r10, (%rax)
	jmp	factorial.start
	.text
factorial.start:
	movq	-144(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	$0, %r10
	movq	-24(%rbp), %r11
	cmpq	%r10, %r11
	setg	-16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$0, %rax
	jne	factorial.then
	jmp	factorial.end
	.text
factorial.then:
	movq	-136(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -88(%rbp)
	movq	-144(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -80(%rbp)
	movq	-80(%rbp), %r11
	movq	-88(%rbp), %rcx
	imulq	%rcx, %r11
	movq	%r11, -72(%rbp)
	movq	-136(%rbp), %rax
	movq	-72(%rbp), %r10
	movq	%r10, (%rax)
	movq	-144(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -56(%rbp)
	movq	$1, %rcx
	movq	-56(%rbp), %r11
	subq	%rcx, %r11
	movq	%r11, -48(%rbp)
	movq	-144(%rbp), %rax
	movq	-48(%rbp), %r10
	movq	%r10, (%rax)
	jmp	factorial.start
	.text
factorial.end:
	movq	-136(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %rax
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
	subq	$8, %rsp
	movq	%rsp, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	$0, %r10
	movq	%r10, (%rax)
	movq	$5, %rdi
	callq	factorial
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	