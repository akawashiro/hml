	.data
NL:
	.asciiz "\n"
	.text
	.globl main
fun_f_1:
	li $t0,-28
	addu $sp,$sp,$t0
	move $t0,$a0
	move $t1,$a1
	move $t2,$a2
	addu $t2,$t1,$t0
	move $v0,$t2
	li $t0,28
	addu $sp,$sp,$t0
	jr $ra
main:
	li $t0,-12
	addu $sp,$sp,$t0
	li $t0,2
	move $t0,$t0
	li $t0,10
	sw $a0,4($sp)
	sw $a1,8($sp)
	sw $t0,0($sp)
	move $a0,$t0
	move $a1,$t0
	jal fun_f_1
	lw $a0,4($sp)
	lw $a1,8($sp)
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
