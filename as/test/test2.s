	.file	"z.c"
	.text
	.globl	foo
	.type	foo, @function
foo:
	movei 1,33
	popj 017,
	.size	foo, .-foo
	.ident	"GCC: (GNU) 4.3.0.- for XKL-2 (XKL LLC, Kirkland, WA, USA)  Built 2013-08-15 23:03 +0200 on porter by mikpe"
	.section	.note.GNU-stack,"",@progbits
