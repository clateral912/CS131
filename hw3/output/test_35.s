	.data
	.globl	hd
hd:
	.quad	1
	.quad	md
	.data
	.globl	md
md:
	.quad	2
	.quad	tl
	.data
	.globl	tl
tl:
	.quad	3
	.quad	0
	.text
main:
	movq	%rdi, -80(%rbp)
	movq	%rsi, -88(%rbp)
	subq	$11, %rsp