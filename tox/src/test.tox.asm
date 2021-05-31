
.text # code segment
.globl _main # label at start of compiled static main
_main:
        pushq   %rbp
        movq    %rsp, %rbp
        call    main
        popq    %rbp
        ret

main:
    pushq %rbp
    movq %rsp, %rbp
    movq $10,%rax
	pushq %rax
	movq $2,%rax
	popq %rdx
	mulq %rdx
	pushq %rax
	movq $5,%rax
	popq %rdx
	addq %rdx,%rax
	popq %rbp
	ret
