	.data
NL:
	.asciiz "\n"
	.text
	.globl main
fun_f_0:
	li $t0,-16
	addu $sp,$sp,$t0
	addu $t0,$a0,$a1
	move $v0,$t0
	li $t0,16
	addu $sp,$sp,$t0
	jr $ra
fun_g_1:
	li $t0,-16
	addu $sp,$sp,$t0
	li $t0,1
	sw $a0,4($sp)
	sw $a1,8($sp)
	sw $ra,12($sp)
	sw $t0,0($sp)
	move $a0,$a0
	move $a1,$t0
	jal fun_f_0
	lw $a0,4($sp)
	lw $a1,8($sp)
	lw $ra,12($sp)
	lw $t0,0($sp)
	move $t0,$v0
	move $v0,$t0
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
