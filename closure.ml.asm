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
	addu $t0,$t1,$t0
	move $v0,$t0
	li $t0,28
	addu $sp,$sp,$t0
	jr $ra
main:
	li $t0,-16
	addu $sp,$sp,$t0
	li $t0,2
	move $t1,$t0
	li $t0,10
	sw $a0,8($sp)
	sw $a1,12($sp)
	sw $t0,0($sp)
	sw $t1,4($sp)
	move $a0,$t0
	move $a1,$t1
	jal fun_f_1
	lw $a0,8($sp)
	lw $a1,12($sp)
	lw $t0,0($sp)
	lw $t1,4($sp)
	move $t0,$v0
	sw $a0,8($sp)
	move $a0,$t0
	li $v0,1
	syscall
	lw $a0,8($sp)
	li $v0,10
	syscall
	li $v0,10
	syscall
