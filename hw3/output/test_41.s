	.data
	.globl	gstr
gstr:
	.asciz	"hello, world!"
	.text
main:
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	subq	$6, %rsp