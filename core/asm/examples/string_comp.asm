; *************************************************************************************************
; Compare two character strings
; 21th of March, 2013
; by Stephan Nolting
; *************************************************************************************************

; Register defintions
; -------------------------------------------------------------------------------------------------
.equ cnt   r0
.equ s1    r1
.equ s2    r2
.equ temp1 r3
.equ temp2 r4
.equ equal r5

; Constants defintions
; -------------------------------------------------------------------------------------------------
.equ upper_border  #123
.equ lower_border  #96
.equ byte_mask     #255
.equ distance      #32
.equ string_length #23


; Exception vector table
; -------------------------------------------------------------------------------------------------
reset_vec:		b main
x_int0_vec:		b x_int0_vec ; not used
x_int1_vec:		b x_int1_vec ; not used
swi_vec:		b swi_vec ; not used


; Main program
; -------------------------------------------------------------------------------------------------
main:		ldil s1,  #low[string_1]
			ldih s1,  #high[string_1]
			ldil s2,  #low[string_2]
			ldih s2,  #high[string_2]
			ldil cnt, string_length
			
			clr equal ; not equal
			cdp #1, c0, c0, #2 ; direct memory access - bypass cache

compare_loop:
			ldr  temp1, s1, +#2, post, !
			ldr  temp2, s2, +#2, post, !
			cmp  temp1, temp2
			bne  not_equal
			decs cnt, cnt, #1
			bne  compare_loop

			ldil equal, #255 ; equal!

not_equal:	b not_equal


; Memory data area
; -------------------------------------------------------------------------------------------------
string_1: .stringz "To boldly go, where no man has gone before..."
string_2: .stringz "To boldly go, where no man has gone before..."
