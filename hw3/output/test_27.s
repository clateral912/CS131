	.data
	.globl	gint
gint:
	.quad	42
	.data
	.globl	v1
v1:
	.quad	0
	.quad	gint
	.data
	.globl	v2
v2:
	.quad	1
	.quad	0
	.data
	.globl	gstr
gstr:
	.asciz	"hello, world!"
	.text
main:
	movq	%rdi, -64(%rbp)
	movq	%rsi, -72(%rbp)
	subq	$9, %rsp
	.text
foo:
	movq	%rdi, -40(%rbp)
	subq	$5, %rsp