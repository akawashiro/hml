	.data
NL:
	.asciiz "\n"
	.text
	.globl main
main:
	li $t0,-52
	addu $sp,$sp,$t0
	li $t0,1
	li $t1,2
	li $t2,3
	mul $t3,$t1,$t2
	li $t1,4
	li $t2,5
	mul $t4,$t1,$t2
	li $t1,3
	li $t2,4
	li $t5,5
	li $t6,6
	mul $t7,$t5,$t6
	li $t5,6
	li $t6,8
	mul $t8,$t5,$t6
	li $t5,10
	li $t6,11
	li $t9,12
	li $v0,13
	sw $v0,40($sp)
	lw $v1,40($sp)
	addu $v0,$t9,$v1
	sw $v0,44($sp)
	lw $v1,44($sp)
	addu $t9,$t6,$v1
	addu $t6,$t5,$t9
	addu $t5,$t8,$t6
	addu $t8,$t7,$t5
	addu $t7,$t2,$t8
	addu $t2,$t1,$t7
	addu $t1,$t4,$t2
	addu $t2,$t3,$t1
	addu $t7,$t0,$t2
	sw $a0,48($sp)
	move $a0,$t7
	li $v0,1
	syscall
	lw $a0,48($sp)
	li $v0,10
	syscall
	li $v0,10
	syscall
