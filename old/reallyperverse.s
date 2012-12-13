	.file	"reallyperverse.c"
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
.globl main
	.type	main,@function
main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	andl	$-16, %esp
	movl	$0, %eax
	subl	%eax, %esp
	movl	$0, -4(%ebp)
	movl	-4(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	$.LC0, (%esp)
	jmp	.data
	cmp	%eax, %ebx
	jz	foo
	call	printf
	leal	-4(%ebp), %eax
	addl	$3, (%eax)
	movl	-4(%ebp), %eax
	cmp	%eax, %ebx
	jnz	foo
	movl	%eax, 4(%esp)
	movl	$.LC0, (%esp)
	call	printf
	movl	-4(%ebp), %eax
	addl	$7, %eax
	movl	%eax, 4(%esp)
	movl	$.LC0, (%esp)
	call	printf
	movl	-4(%ebp), %ecx
	cmp	%eax, %ebx
	jz	foo
	movl	%ecx, %eax
	leal	0(,%eax,8), %edx
	subl	%ecx, %edx
	leal	-4(%ebp), %eax
	subl	%edx, (%eax)
	movl	-4(%ebp), %eax
	movl	%eax, 4(%esp)
	cmp	%eax, %ebx
	jnz	foo
	movl	$.LC0, (%esp)
	call	printf
	movl	-4(%ebp), %eax
	movl	%eax, 4(%esp)
	leal	-4(%ebp), %eax
	decl	(%eax)
	movl	$.LC0, (%esp)
	cmp	%eax, %ebx
	jz	foo
	call	printf
	leal	-4(%ebp), %eax
	decl	(%eax)
	movl	-4(%ebp), %eax
	movl	%eax, 4(%esp)
	cmp	%eax, %ebx
	jnz	foo
	movl	$.LC0, (%esp)
	call	printf
	movl	$0, %eax
	leave
	ret
.Lfe1:
	.size	main,.Lfe1-main
	.section	.rodata
.LC1:
	.string	"%s\n"
	.text
.globl foo
	.type	foo,@function
foo:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	%eax, (0x34)
	movl	8(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	$.LC1, (%esp)
	call	printf
	leave
	ret
.Lfe2:
	.size	foo,.Lfe2-foo
	.ident	"GCC: (GNU) 3.2.3"
