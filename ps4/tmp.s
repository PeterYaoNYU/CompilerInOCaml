	.text
	.align	2
	.globl main
	j main
main:
	addi	$29, $29, 0xFFFFFFA8
	sw	$30, 84($29)
	sw	$31, 80($29)
	addi	$30, $29, 0x58
	li	$2, 0x0
	sw	$2, -12($30)
	li	$2, 0x0
	sw	$2, -16($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	lw	$2, -12($30)
	add	$4, $2, $0
	jal printInt
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	lw	$2, -16($30)
	add	$4, $2, $0
	jal printInt
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	lw	$2, -12($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0xA
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	slt	$2, $8, $2
	add	$4, $2, $0
	jal printInt
	lw	$8, 0($29)
	addi	$29, $29, 0x4
L1:
	lw	$2, -12($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0xA
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	slt	$2, $8, $2
	beq	$2, $0, L2
	lw	$2, -16($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	lw	$2, -12($30)
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	add	$2, $2, $8
	sw	$2, -16($30)
	lw	$2, -12($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x1
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	add	$2, $2, $8
	sw	$2, -12($30)
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	lw	$2, -16($30)
	add	$4, $2, $0
	jal printInt
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	lw	$2, -12($30)
	add	$4, $2, $0
	jal printInt
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	j L1
L2:
	lw	$2, -16($30)
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
