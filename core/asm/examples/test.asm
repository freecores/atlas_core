; -------------------------------------------------------------------------------------------------
; ATLAS ASSEMBLER test/example program file, May 2013
; -------------------------------------------------------------------------------------------------

; -> for all further lexical explanations see the rules given between the '>' and '<' symbol
; -> this is a comment, initiated with >;<
; -> all constant numbers must have an '#'-prefix
; -> assembler is not case-sensitive!


; User defintions
; -------------------------------------------------------------------------------------------------
; user-defined definitions for registers (r0, .., r1), cp_registers (c0, .., c7), constants ('#...')
; definitions: >.equ<> <>alias_name<> <>real_name/constant<
; do not use a definition name also as label name!
; .equ definitions must be at the beginning of a program (before actual code)
.equ cnt  r0              ; counter register
.equ data r3              ; data transfer register
.equ LR   r7              ; r7 is always the link register
.equ dec_val        #1    ; constant/immediate alias used for decrement
.equ msr_xint0_en   #13   ; this is the MSR's external interrupt 0 enable flag
.equ msr_xint1_en   #14   ; this is the MSR's external interrupt 1 enable flag
.equ data_area_size #4    ; space area alias
.equ bytes          #4    ; number of bytes to transfer within the demo program
.equ some_data      #1289 ; just some data for memory init


; Exception vector table
; -------------------------------------------------------------------------------------------------
; This table MUST be the first logical instructions within
; a program ( block 0) due to fixed exception vectors!

; labels: >lable_name: letters, numbers and '_' symbols<>:<
; only one label can point to one specific location (so no redundant labels!!!)
reset_vec:		b init          ; destination after reset, always starting in system mode
x_int0_vec:		b x_int_handler ; external interrupt 0 interrupt handler
x_int1_vec:		b x_int_handler ; external interrupt 1 interrupt handler
cmd_err_vec:	b cmd_err_vec   ; undefined instruction / illegal msr/reg/cp access
swi_vec:		b swi_handler   ; software interrupt exception handler


; Exception handlers (system mode)
; -------------------------------------------------------------------------------------------------
swi_handler: ; SOFTWARE INTERRUPT HANDLER
; instructions
; >cmd mnemonic<> <>OP1<>,<> <>OP2<>,<> < ...
			dec  LR, LR, #2 ; restore WORD (!!!) address of calling instruction
			ldr  r0, LR, +#0, pre ; load the instruction, which caused the exception
			clr  r1 ; load tag mask
			ldih r1, #252
			bic  r0, r0, r1	; isolate syscall tag in r0
			
				ldil r4, #low[test_mem_data_table]
				ldih r4, #high[test_mem_data_table]
				ldr r4, r4, +#0, pre

nirvana:	b nirvana ; this is the end, my friend...


x_int_handler: ; EXTERNAL INTERRUPT HANDLER
            ; do nothing in this exception handlers but return
			dec LR, LR, #2 ; restore WORD (!!!) address of interrupted instruction
			b skip_dummy_mem_area ; skip dummy mem area

; reserve memory (fill with zeros)
; reserve: >.space<> <>#<>integer value = number of zero words / defintion<
dummy_mem_area:
			.space #4 ; insert 4 * x"0000" = "NOP"

skip_dummy_mem_area:
			reti LR ; return from interrupt handler routine, re-enable global xint_en and resume operation in user_mode at [r7]


; Main program
; -------------------------------------------------------------------------------------------------
init:		ldil r0, #xlow[main_usr]  ; get extended (upper 16-bit of 32-bit) low address byte of label "main_usr" - just a demo, it is 0 here
			ldih r0, #xhigh[main_usr] ; get extended (upper 16-bit of 32-bit) high address byte of label "main_usr" - just a demo, it is 0 here
			ldil r0, #low[main_usr]   ; get low address byte of label "main_usr"
			ldih r0, #high[main_usr]  ; get high address byte of label "main_usr"
			
			ldsr r1                   ; copy machine status register to r1
			sbr  r1, r1, msr_xint0_en ; enable external interrupt 0 mask, no '#'-prefix when using a constant definition!
			stsr r1                   ; store r1 to machine status register

			gtui r0 ; enable external interrupts and resume operation in user mode at [r0]

.space #4 ; just some space (4*x"0000") to isolate user program...


; Here starts the user program (of course in user mode^^)
; -------------------------------------------------------------------------------------------------
main_usr:	ldil cnt, #low[bytes] ; loading a sign extended constant definition -> number of bytes to transfer

			ldil r1, #low[test_mem_data_table] ; load address of constant table (see below
			ldih r1, #high[test_mem_data_table]

			ldil r2, #low[data_area] ; get address of label 'data_area' - store data there
			ldih r2, #high[data_area]

			ldil r4, #low[test_mem_data_table] ; swap with first data table entry
			ldih r4, #high[test_mem_data_table]
			ldil r5, #low[#2222] ; dummy data
			ldih r5, #high[#2222]
			swp r5, r4, r5 ; swap memory[r4] with r5

			ldil data, #0xAB ; hex init
			ldil data, #0b11001100 ; bin init
			clr data ; clear data transfer register

			staf #1, usr_flags

loop1:		bl load_subroutine ; branch to 'load_subroutine' and link (save return address to link register = r7)
			bl store_subroutine
			decs cnt, cnt, dec_val ; decrement 'cnt' by 'dec_val' and set alu flags corresponding to the result ('s'-appendix)
			bne loop1			
			syscall #129 ; terminate by system call with tag '129'

store_subroutine: ; just some subroutine testing
			str data, r2, +#2, post, ! ; store 'data' at [r2] and set r2=r2+2 afterwards
			ret LR ; 'ret' and 'stpc' are equal

load_subroutine: ; and again some subroutine testing
			ldr data, r1, +#2, post, ! ; load 'data' from [r1] and set r1=r1+2 afterwards
			ret LR ; return to location stored in the link register LR (=r7)


; this is a pseudo data area
; -------------------------------------------------------------------------------------------------
data_area:	.space data_area_size ; insert 'data_area_size' * x"0000" = "NOP"
			.space #32 ; just some room
; direct memory initialization
; word init: >.dw<> <>#<>integer value (max (2^16)-1)<
test_mem_data_table:
.dw #23432
.dw #234
.dw #14
.dw some_data ; you can also use definitions here
.dw [test_mem_data_table] ; here, the absolute address (inside a page) of branch label "test_mem_data_table" will be placed
.stringz "This is a zero-terminated character string"

.include "other_stuff.asm" ; include (AT THIS POINT) the file "other_stuff.asm"
