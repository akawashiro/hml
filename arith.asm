	.data
NL:
	.asciiz "\n"
	.text
	.globl main
main:
	li $t0,-8
	addu $sp,$sp,$t0
	li $t0,1
	li $t0,2
	li $t0,3
	mul $t0,$t0,$t0
	li $t0,4
	li $t0,5
	mul $t0,$t0,$t0
	li $t0,3
	li $t0,4
	li $t0,5
	li $t0,6
	mul $t0,$t0,$t0
	li $t0,6
	li $t0,8
	mul $t0,$t0,$t0
	li $t0,10
	li $t0,11
	li $t0,12
	li $t0,13
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	sw $a0,4($sp)
	move $a0,$t0
	li $v0,1
	syscall
	lw $a0,4($sp)
	li $v0,10
	syscall
	li $v0,10
	syscall
