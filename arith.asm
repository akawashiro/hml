	.data
NL:
	.asciiz "\n"
	.text
	.globl main
main:
	li $t0,-24
	addu $sp,$sp,$t0
	li $t0,1
	li $t1,2
	li $t2,3
	mul $t3,$t1,$t2
	li $t1,4
	li $t2,5
	mul $t4,$t1,$t2
	addu $t1,$t3,$t4
	addu $t3,$t0,$t1
	sw $a0,20($sp)
	move $a0,$t3
	li $v0,1
	syscall
	lw $a0,20($sp)
	li $v0,10
	syscall
