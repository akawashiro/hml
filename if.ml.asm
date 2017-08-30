	.data
NL:
	.asciiz "\n"
	.text
	.globl main
main:
	li $t0,-28
	addu $sp,$sp,$t0
	li $t0,2
	li $t1,10
	slt $t2,$t0,$t1
	move $t0,$t2
	beqz $t0,_if_label_0
	j _if_label_1
_if_label_0:
	li $t0,10
	move $t2,$t0
	sw $a0,12($sp)
	move $a0,$t2
	li $v0,1
	syscall
	lw $a0,12($sp)
	li $v0,10
	syscall
_if_label_1:
	li $t0,20
	li $t2,3
	mul $t1,$t0,$t2
	li $t0,5
	li $t2,5
	li $t3,1
	li $t4,1
	addu $t5,$t3,$t4
	addu $t3,$t2,$t5
	addu $t2,$t0,$t3
	addu $t0,$t1,$t2
	move $t2,$t0
	sw $a0,12($sp)
	move $a0,$t2
	li $v0,1
	syscall
	lw $a0,12($sp)
	li $v0,10
	syscall
	li $v0,10
	syscall
