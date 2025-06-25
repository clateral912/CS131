	.data
	.globl	toofew
toofew:
	.asciz	"argc < 3"
	.data
	.globl	toomany
toomany:
	.asciz	"argc > 3"
	.text
main:
	movq	%rdi, -144(%rbp)
	movq	%rsi, -152(%rbp)
	subq	$19, %rsp