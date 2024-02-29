	.text
	.align	2
	.globl main
	j main
main:
	addi	$29, $29, 0xFFFFFFA8
	sw	$30, 84($29)
	sw	$31, 80($29)
	addi	$30, $29, 0x58
	addi	$29, $29, 0xFFFFFFFC
	sw	$8, 0($29)
	li	$2, 0xC
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x4
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	sub	$2, $8, $2
	add	$4, $2, $0
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	jal printInt
	lw	$0, 0($29)
	addi	$29, $29, 0x4
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	li	$2, 0xC
	addi	$29, $29, 0xFFFFFFFC
	sw	$2, 0($29)
	li	$2, 0x4
	lw	$8, 0($29)
	addi	$29, $29, 0x4
	sub	$2, $8, $2
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
