	.data
NL:
	.asciiz "\n"
	.text
	.globl main
fun_f_0:
	li $t0,-20
	addu $sp,$sp,$t0
	move $t0,$a0
	move $t1,$a1
	li $t1,10
	addu $t1,$t0,$t1
	move $v0,$t1
	li $t0,20
	addu $sp,$sp,$t0
	jr $ra
main:
	li $t0,-8
	addu $sp,$sp,$t0
	li $t0,10
	sw $a0,4($sp)
	sw $t0,0($sp)
	move $a0,$t0
	jal fun_f_0
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
	li $v0,10
	syscall
