	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$48, %rsp
	movq	$0, %r10
	movq	$3, %r11
	cmpq	%r10, %r11
	setl	-32(%rbp)
	movq	-32(%rbp), %rax
	cmpq	$0, %rax
	jne	then
	jmp	else
	.text
main.then:
	movq	$7, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
main.else:
	movq	$9, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	