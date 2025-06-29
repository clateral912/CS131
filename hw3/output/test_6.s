	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$48, %rsp
	movq	$12, %r11
	movq	$5, %rcx
	addq	%rcx, %r11
	movq	%r11, -32(%rbp)
	jmp	next
	.text
main.next:
	jmp	end
	.text
main.end:
	movq	-32(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	