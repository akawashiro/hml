	.data
NL:
	.asciiz "\n"
	.text
	.globl main
main:
	li $t0,-8
	addu $sp,$sp,$t0
	li $t0,2
	li $t0,10
	slt $t0,$t0,$t0
	move $t0,$t0
	beqz $t0,_if_label_0
	j _if_label_1
_if_label_0:
	li $t0,10
	move $t0,$t0
	sw $a0,4($sp)
	move $a0,$t0
	li $v0,1
	syscall
	lw $a0,4($sp)
	li $v0,10
	syscall
_if_label_1:
	li $t0,20
	li $t0,3
	mul $t0,$t0,$t0
	li $t0,5
	li $t0,5
	li $t0,1
	li $t0,1
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	addu $t0,$t0,$t0
	move $t0,$t0
	sw $a0,4($sp)
	move $a0,$t0
	li $v0,1
	syscall
	lw $a0,4($sp)
	li $v0,10
	syscall
	li $v0,10
	syscall
