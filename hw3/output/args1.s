	.data
	.globl	toofew
toofew:
	.asciz	"argc < 3"
	.data
	.globl	toomany
toomany:
	.asciz	"argc > 3"
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -144(%rbp)
	movq	%rsi, -152(%rbp)
	subq	$152, %rsp
	movq	$3, %r10
	movq	-144(%rbp), %r11
	cmpq	%r10, %r11
	setl	-136(%rbp)
	andq	$1, -136(%rbp)
	movq	-136(%rbp), %rax
	cmpq	$0, %rax
	jne	main.few
	jmp	main.else
	.text
main.few:
	movq	$0, %r9 
	addq	$0, %r9 
	addq	$0, %r9 
	leaq	toofew(%rip), %r11
	addq	%r9 , %r11
	movq	%r11, %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	callq	ll_puts
	movq	%rax, -16(%rbp)
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
main.else:
	movq	$3, %r10
	movq	-144(%rbp), %r11
	cmpq	%r10, %r11
	setg	-40(%rbp)
	andq	$1, -40(%rbp)
	movq	-40(%rbp), %rax
	cmpq	$0, %rax
	jne	main.many
	jmp	main.right
	.text
main.many:
	movq	$0, %r9 
	addq	$0, %r9 
	addq	$0, %r9 
	leaq	toomany(%rip), %r11
	addq	%r9 , %r11
	movq	%r11, %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rdi
	callq	ll_puts
	movq	%rax, -56(%rbp)
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
main.right:
	# %3 = getelementptr i8*, i8** %argv, i32 1
	movq	$0, %r9 
	addq	$1, %r9 
	movq	-152(%rbp), %r11
	addq	%r9 , %r11
	movq	%r11, %rax
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rax
	# %4 = load i8*, i8** %3
	movq	(%rax), %rax
	movq	%rax, -112(%rbp)
	# %5 = getelementptr i8*, i8** %argv, i32 2
	movq	$0, %r9 
	addq	$2, %r9 
	movq	-152(%rbp), %r11
	addq	%r9 , %r11
	movq	%r11, %rax
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %rax
	# %6 = load i8*, i8** %5
	movq	(%rax), %rax
	movq	%rax, -96(%rbp)
	# %7 = call i8* @ll_strcat(i8* %4, i8* %6)
	movq	-112(%rbp), %rdi
	movq	-96(%rbp), %rsi
	callq	ll_strcat
	movq	%rax, -88(%rbp)
	# call void @ll_puts(i8* %7)
	movq	-88(%rbp), %rdi
	callq	ll_puts
	movq	%rax, -80(%rbp)
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	