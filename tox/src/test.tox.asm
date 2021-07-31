.text
.type add, @function
add:
	push %rbp
	mov %rsp, %rbp
	sub $16, %rsp
	popq %rbp
	ret
.text
.type main, @function
main:
	push %rbp
	mov %rsp, %rbp
	sub $0, %rsp
	popq %rbp
	ret
