	.text
	.align	2
	.globl main
	j main
add_func:
	addi	$29, $29, 0xFFFFFFA8
	sw	$30, 84($29)
	sw	$31, 80($29)
	addi	$30, $29, 0x58
	lw	$9, 0($30)
	sw	$9, -12($30)
	lw	$9, 4($30)
	sw	$9, -16($30)
	li	$2, 0x0
	sw	$2, -20($30)
	li	$2, 0x0
	sw	$2, -24($30)
	li	$2, 0x0
	sw	$2, -28($30)
	li	$2, 0x0
	sw	$2, -32($30)
	li	$2, 0x0
	sw	$2, -36($30)
	li	$2, 0x0
	sw	$2, -40($30)
	li	$2, 0x0
	sw	$2, -44($30)
	li	$2, 0x0
	sw	$2, -48($30)
	lw	$2, -12($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x0
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	slt	$2, $8, $2
	bne	$2, $0, L5
	lw	$2, -12($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x3
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	slt	$2, $2, $8
	bne	$2, $0, L5
	li	$2, 0x0
	j L6
L5:
	li	$2, 0x1
L6:
	bne	$2, $0, L3
	lw	$2, -16($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x0
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	slt	$2, $8, $2
	bne	$2, $0, L3
	li	$2, 0x0
	j L4
L3:
	li	$2, 0x1
L4:
	bne	$2, $0, L1
	lw	$2, -16($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x3
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	slt	$2, $2, $8
	bne	$2, $0, L1
	li	$2, 0x0
	j L2
L1:
	li	$2, 0x1
L2:
	beq	$2, $0, L7
	li	$2, 0x0
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x1
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	sub	$2, $8, $2
	j L8
L7:
	li	$2, 0x0
L8:
	lw	$2, -12($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x2
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	slt	$2, $8, $2
	xori	$2, $2, 0x1
	beq	$2, $0, L9
	li	$2, 0x1
	sw	$2, -20($30)
	j L10
L9:
	li	$2, 0x0
	sw	$2, -20($30)
L10:
	lw	$2, -12($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x2
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	lw	$2, -20($30)
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	mul	$2, $2, $8
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	sub	$2, $8, $2
	sw	$2, -28($30)
	lw	$2, -16($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x2
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	slt	$2, $8, $2
	xori	$2, $2, 0x1
	beq	$2, $0, L11
	li	$2, 0x1
	sw	$2, -24($30)
	j L12
L11:
	li	$2, 0x0
	sw	$2, -24($30)
L12:
	lw	$2, -16($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x2
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	lw	$2, -24($30)
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	mul	$2, $2, $8
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	sub	$2, $8, $2
	sw	$2, -32($30)
	lw	$2, -28($30)
	bne	$2, $0, L15
	lw	$2, -32($30)
	bne	$2, $0, L15
	li	$2, 0x0
	j L16
L15:
	li	$2, 0x1
L16:
	beq	$2, $0, L13
	lw	$2, -28($30)
	beq	$2, $0, L19
	lw	$2, -32($30)
	beq	$2, $0, L19
	li	$2, 0x1
	j L20
L19:
	li	$2, 0x0
L20:
	beq	$2, $0, L17
	li	$2, 0x0
	j L18
L17:
	li	$2, 0x1
L18:
	beq	$2, $0, L13
	li	$2, 0x1
	j L14
L13:
	li	$2, 0x0
L14:
	beq	$2, $0, L21
	li	$2, 0x1
	sw	$2, -36($30)
	j L22
L21:
	li	$2, 0x0
	sw	$2, -36($30)
L22:
	lw	$2, -28($30)
	beq	$2, $0, L23
	lw	$2, -32($30)
	beq	$2, $0, L23
	li	$2, 0x1
	j L24
L23:
	li	$2, 0x0
L24:
	beq	$2, $0, L25
	li	$2, 0x1
	sw	$2, -40($30)
	j L26
L25:
	li	$2, 0x0
	sw	$2, -40($30)
L26:
	lw	$2, -20($30)
	beq	$2, $0, L35
	lw	$2, -24($30)
	beq	$2, $0, L35
	li	$2, 0x1
	j L36
L35:
	li	$2, 0x0
L36:
	beq	$2, $0, L33
	lw	$2, -40($30)
	beq	$2, $0, L33
	li	$2, 0x1
	j L34
L33:
	li	$2, 0x0
L34:
	bne	$2, $0, L31
	lw	$2, -20($30)
	beq	$2, $0, L39
	lw	$2, -24($30)
	beq	$2, $0, L41
	li	$2, 0x0
	j L42
L41:
	li	$2, 0x1
L42:
	beq	$2, $0, L39
	li	$2, 0x1
	j L40
L39:
	li	$2, 0x0
L40:
	beq	$2, $0, L37
	lw	$2, -40($30)
	beq	$2, $0, L43
	li	$2, 0x0
	j L44
L43:
	li	$2, 0x1
L44:
	beq	$2, $0, L37
	li	$2, 0x1
	j L38
L37:
	li	$2, 0x0
L38:
	bne	$2, $0, L31
	li	$2, 0x0
	j L32
L31:
	li	$2, 0x1
L32:
	bne	$2, $0, L29
	lw	$2, -20($30)
	beq	$2, $0, L49
	li	$2, 0x0
	j L50
L49:
	li	$2, 0x1
L50:
	beq	$2, $0, L47
	lw	$2, -24($30)
	beq	$2, $0, L47
	li	$2, 0x1
	j L48
L47:
	li	$2, 0x0
L48:
	beq	$2, $0, L45
	lw	$2, -40($30)
	beq	$2, $0, L51
	li	$2, 0x0
	j L52
L51:
	li	$2, 0x1
L52:
	beq	$2, $0, L45
	li	$2, 0x1
	j L46
L45:
	li	$2, 0x0
L46:
	bne	$2, $0, L29
	li	$2, 0x0
	j L30
L29:
	li	$2, 0x1
L30:
	bne	$2, $0, L27
	lw	$2, -20($30)
	beq	$2, $0, L57
	li	$2, 0x0
	j L58
L57:
	li	$2, 0x1
L58:
	beq	$2, $0, L55
	lw	$2, -24($30)
	beq	$2, $0, L59
	li	$2, 0x0
	j L60
L59:
	li	$2, 0x1
L60:
	beq	$2, $0, L55
	li	$2, 0x1
	j L56
L55:
	li	$2, 0x0
L56:
	beq	$2, $0, L53
	lw	$2, -40($30)
	beq	$2, $0, L53
	li	$2, 0x1
	j L54
L53:
	li	$2, 0x0
L54:
	bne	$2, $0, L27
	li	$2, 0x0
	j L28
L27:
	li	$2, 0x1
L28:
	beq	$2, $0, L61
	li	$2, 0x1
	sw	$2, -44($30)
	j L62
L61:
	li	$2, 0x0
	sw	$2, -44($30)
L62:
	lw	$2, -20($30)
	beq	$2, $0, L67
	lw	$2, -24($30)
	beq	$2, $0, L67
	li	$2, 0x1
	j L68
L67:
	li	$2, 0x0
L68:
	bne	$2, $0, L65
	lw	$2, -20($30)
	beq	$2, $0, L69
	lw	$2, -40($30)
	beq	$2, $0, L69
	li	$2, 0x1
	j L70
L69:
	li	$2, 0x0
L70:
	bne	$2, $0, L65
	li	$2, 0x0
	j L66
L65:
	li	$2, 0x1
L66:
	bne	$2, $0, L63
	lw	$2, -24($30)
	beq	$2, $0, L71
	lw	$2, -40($30)
	beq	$2, $0, L71
	li	$2, 0x1
	j L72
L71:
	li	$2, 0x0
L72:
	bne	$2, $0, L63
	li	$2, 0x0
	j L64
L63:
	li	$2, 0x1
L64:
	beq	$2, $0, L73
	li	$2, 0x1
	sw	$2, -48($30)
	j L74
L73:
	li	$2, 0x0
	sw	$2, -48($30)
L74:
	li	$2, 0x4
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	lw	$2, -48($30)
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	mul	$2, $2, $8
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x2
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	lw	$2, -44($30)
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	mul	$2, $2, $8
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	add	$2, $2, $8
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	lw	$2, -36($30)
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	add	$2, $2, $8
end_add:
	lw	$30, 84($29)
	lw	$31, 80($29)
	addi	$29, $29, 0x58
	jr	$31
main:
	addi	$29, $29, 0xFFFFFFA8
	sw	$30, 84($29)
	sw	$31, 80($29)
	addi	$30, $29, 0x58
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	li	$2, 0x2
	add	$4, $2, $0
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x1
	add	$5, $2, $0
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	jal add_func
	lw	$0, 0($29)
	addi	$29, $29, 0x4
	lw	$0, 0($29)
	addi	$29, $29, 0x4
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	add	$4, $2, $0
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	li	$2, 0x1
	add	$4, $2, $0
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x1
	add	$5, $2, $0
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	jal add_func
	lw	$0, 0($29)
	addi	$29, $29, 0x4
	lw	$0, 0($29)
	addi	$29, $29, 0x4
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	add	$5, $2, $0
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	jal add_func
	lw	$0, 0($29)
	addi	$29, $29, 0x4
	lw	$0, 0($29)
	addi	$29, $29, 0x4
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	add	$4, $2, $0
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	jal printInt
	lw	$0, 0($29)
	addi	$29, $29, 0x4
	lw	$8, 0($29)
	addi	$29, $29, 0x4
end_main:
	lw	$30, 84($29)
	lw	$31, 80($29)
	addi	$29, $29, 0x58
	jr	$31


	.data
	.align 0

#
# below here is the print debugging support code
#
	
.data
_spaceString: .asciiz " "
_newlineString: .asciiz "\n"

.text
.globl printInt     # int reg -> unit
.globl printSpace   # unit    -> unit
.globl printNewline # unit    -> unit

printInt: # int reg->unit
	                  # The syscall takes its argument in $a0
   add $t0, $v0, $zero    # since this function does not return anything, it should probably preserve $v0
   li $v0, 1              # print_int syscall
   syscall
   add $v0, $t0, $zero    # restore $v0 
jr $ra


printSpace: # unit->unit
add $t0, $v0, $zero
la $a0, _spaceString      # address of string to print
li $v0, 4                 # system call code for print_str
syscall                   # print the string
add $v0, $t0, $zero
jr $ra

printNewline: # unit->unit
add $t0, $v0, $zero
la $a0, _newlineString    # address of string to print
li $v0, 4                 # system call code for print_str
syscall                   # print the string
add $v0, $t0, $zero
jr $ra
