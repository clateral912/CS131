	.text
	.globl	f1
f1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -48(%rbp)
	subq	$48, %rsp
	jmp	f1.start
	.text
f1.start:
	movq	$10, %r10
	movq	-48(%rbp), %r11
	cmpq	%r10, %r11
	setg	-16(%rbp)
	shlq	$24, -16(%rbp)
	shrq	$24, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$0, %rax
	jne	f1.then
	jmp	f1.end
	.text
f1.then:
	movq	$1, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
f1.end:
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	f2
f2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -48(%rbp)
	subq	$48, %rsp
	jmp	f2.start
	.text
f2.start:
	movq	$10, %r10
	movq	-48(%rbp), %r11
	cmpq	%r10, %r11
	setg	-16(%rbp)
	shlq	$24, -16(%rbp)
	shrq	$24, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpq	$0, %rax
	jne	f2.then
	jmp	f2.end
	.text
f2.then:
	movq	$1, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
f2.end:
	movq	$0, %rax
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
	movq	$0, %rdi
	callq	f1
	movq	%rax, -32(%rbp)
	movq	$15, %rdi
	callq	f2
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %r11
	movq	-32(%rbp), %rcx
	addq	%rcx, %r11
	movq	%r11, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	