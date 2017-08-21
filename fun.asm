	.data
NL:
	.asciiz "\n"
	.text
	.globl main
f_0:
	li $t0,-16
	addu $sp,$sp,$t0
	li $t0,10
	addu $t1,$a0,$t0
	move $v0,$t1
	li $t0,16
	addu $sp,$sp,$t0
	jr $ra
main:
	li $t0,-8
	addu $sp,$sp,$t0
	li $t0,10
	sw $a0,4($sp)
	sw $t0,0($sp)
	move $a0,$t0
	jal f_0
	lw $a0,4($sp)
	lw $t0,0($sp)
	move $t0,$v0
	sw $a0,4($sp)
	move $a0,$t0
	li $v0,1
	syscall
	lw $a0,4($sp)
	li $v0,10
	syscall
