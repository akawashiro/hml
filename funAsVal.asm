	.data
NL:
	.asciiz "\n"
	.text
	.globl main
fun_f_0:
	li $t0,-28
	addu $sp,$sp,$t0
	move $t0,$a0
	move $t1,$a1
	move $t2,$a2
	addu $t2,$t0,$t1
	move $v0,$t2
	li $t0,28
	addu $sp,$sp,$t0
	jr $ra
fun_g_1:
	li $t0,-20
	addu $sp,$sp,$t0
	move $t0,$a0
	move $t1,$a1
	li $t1,1
	li $t1,2
	sw $a0,4($sp)
	sw $a1,12($sp)
	sw $ra,16($sp)
	sw $t0,0($sp)
	sw $t1,8($sp)
	move $a0,$t1
	move $a1,$t1
	jalr $t0,$ra
	lw $a0,4($sp)
	lw $a1,12($sp)
	lw $ra,16($sp)
	lw $t0,0($sp)
	lw $t1,8($sp)
	move $t1,$v0
	move $v0,$t1
	li $t0,20
	addu $sp,$sp,$t0
	jr $ra
main:
	li $t0,-8
	addu $sp,$sp,$t0
	sw $a0,4($sp)
	sw $t0,0($sp)
	la $a0,fun_f_0
	jal fun_g_1
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
