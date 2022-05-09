.text
.globl _main
_main:
	push %rbp
	mov %rsp, %rbp
	sub $16, %rsp
# span (23, 27)
mov $1092616192, %eax # float 10
movq %rax, %xmm0
	popq %rbp
	ret
