; *************************************************************************************************
; Count leading zeros of a register
; 27th of May, 2013
; by Stephan Nolting
; *************************************************************************************************

; Register defintions
; -------------------------------------------------------------------------------------------------


; Constants defintions
; -------------------------------------------------------------------------------------------------


; Exception vector table
; -------------------------------------------------------------------------------------------------
reset_vec:		b main
x_int0_vec:		b x_int0_vec ; not used
x_int1_vec:		b x_int1_vec ; not used
cmd_err_vec:	b cmd_err_vec ; not used
swi_vec:		b swi_vec ; not used


; Main program
; -------------------------------------------------------------------------------------------------
main:		ldil r0, #0b11000110 ; load low value using binary format
			ldih r0, #0b00000101 ; load high value using binary format

			ldil r1, #16 ; if register is zero -> 16 leading zeros

			teq r0, r0
			beq end ; skip counting when register is not zero


			clr  r1 ; reset counter
loop:		sfts r0, r0, #lsl ; shift msb of r0 into carry flag
			bcs  end ; terminate when shifted bit is one
			inc  r1, r1, #1 ; increment counter
			b    loop

end:		b end