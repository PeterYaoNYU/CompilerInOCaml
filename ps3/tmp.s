	.text
	.align	2
	.globl printInt
	.globl fish_main
	.globl main

main:
	move $s8, $31
	jal fish_main
	move $31, $s8
	move $a0, $2
	j printInt

printInt:
	add $t0, $v0, $zero
	li $v0, 1
	syscall
	add $v0, $t0, $zero
	jr $ra

fish_main:
	li	$2, 0x0
	la	$8, var_x
	sw	$2, 0($8)
	li	$2, 0x1
	la	$8, var_a
	sw	$2, 0($8)
	j L1
L2:
	li	$2, 0x1
	la	$8, var_b
	sw	$2, 0($8)
	j L7
L8:
	li	$2, 0x1
	la	$8, var_c
	sw	$2, 0($8)
	j L13
L14:
	la	$2, var_x
	lw	$2, 0($2)
	la	$8, T20
	sw	$2, 0($8)
	la	$2, var_c
	lw	$2, 0($2)
	la	$8, T20
	lw	$8, 0($8)
	add	$2, $2, $8
	la	$8, var_x
	sw	$2, 0($8)
	la	$2, var_c
	lw	$2, 0($2)
	la	$8, T18
	sw	$2, 0($8)
	li	$2, 0x1
	la	$8, T18
	lw	$8, 0($8)
	add	$2, $2, $8
	la	$8, var_c
	sw	$2, 0($8)
L13:
	la	$2, var_c
	lw	$2, 0($2)
	la	$8, T15
	sw	$2, 0($8)
	la	$2, var_b
	lw	$2, 0($2)
	la	$8, T15
	lw	$8, 0($8)
	slt	$2, $2, $8
	xori	$2, $2, 0x1
	bne	$2, $0, L14
	la	$2, var_b
	lw	$2, 0($2)
	la	$8, T12
	sw	$2, 0($8)
	li	$2, 0x1
	la	$8, T12
	lw	$8, 0($8)
	add	$2, $2, $8
	la	$8, var_b
	sw	$2, 0($8)
L7:
	la	$2, var_b
	lw	$2, 0($2)
	la	$8, T9
	sw	$2, 0($8)
	la	$2, var_a
	lw	$2, 0($2)
	la	$8, T9
	lw	$8, 0($8)
	slt	$2, $2, $8
	xori	$2, $2, 0x1
	bne	$2, $0, L8
	la	$2, var_a
	lw	$2, 0($2)
	la	$8, T6
	sw	$2, 0($8)
	li	$2, 0x1
	la	$8, T6
	lw	$8, 0($8)
	add	$2, $2, $8
	la	$8, var_a
	sw	$2, 0($8)
L1:
	la	$2, var_a
	lw	$2, 0($2)
	la	$8, T3
	sw	$2, 0($8)
	li	$2, 0xA
	la	$8, T3
	lw	$8, 0($8)
	slt	$2, $2, $8
	xori	$2, $2, 0x1
	bne	$2, $0, L2
	la	$2, var_x
	lw	$2, 0($2)
	jr	$31


	.data
	.align 0
T10:	.word 0
T11:	.word 0
T12:	.word 0
T15:	.word 0
T16:	.word 0
T17:	.word 0
T18:	.word 0
T19:	.word 0
T20:	.word 0
T21:	.word 0
T22:	.word 0
T23:	.word 0
T24:	.word 0
T3:	.word 0
T4:	.word 0
T5:	.word 0
T6:	.word 0
T9:	.word 0
var_a:	.word 0
var_b:	.word 0
var_c:	.word 0
var_x:	.word 0

