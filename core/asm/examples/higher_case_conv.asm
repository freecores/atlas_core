; *************************************************************************************************
; Convert a character string to higher-case letters
; 21th of March, 2013
; by Stephan Nolting
; *************************************************************************************************

; Register defintions
; -------------------------------------------------------------------------------------------------
.equ cnt   r0
.equ d_in  r1
.equ d_out r2
.equ data  r3
.equ temp  r4
.equ copy  r5
.equ sp    r6

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
main:		ldil d_in,  #low[string_input]
			ldih d_in,  #high[string_input]
			ldil d_out, #low[string_output]
			ldih d_out, #high[string_output]
			ldil sp,    #low[stack_begin]
			ldih sp,    #high[stack_begin]

			ldil cnt, string_length
			
			cdp #1, c0, c0, #2

conversion_loop:
			ldr data, d_in, +#2, post, !

			; low byte
			ldil temp, #low[byte_mask]
			ldih temp, #high[byte_mask]
			sft  temp, temp, #swp
			bic  copy, data, temp
			
			ldil temp, upper_border
			cmp  copy, temp
			bge  skip_low_byte
			ldil temp, lower_border
			cmp  copy, temp
			ble  skip_low_byte

			ldil temp, distance
			sub copy, copy, temp

skip_low_byte:
			str copy, sp, +#2, post, ! ; push

			; high byte
			ldil temp, #low[byte_mask]
			ldih temp, #high[byte_mask]
			bic  copy, data, temp
			sft  copy, copy, #swp
			
			ldil temp, upper_border
			cmp  copy, temp
			bge  skip_high_byte
			ldil temp, lower_border
			cmp  copy, temp
			ble  skip_high_byte

			ldil temp, distance
			sub copy, copy, temp

skip_high_byte:
			sft copy, copy, #swp
			ldr data, sp, -#2, pre, ! ; pop
			orr data, data, copy

			str  data, d_out, +#2, post, !

			decs cnt, cnt, #1
			bne  conversion_loop

end:		b end


; Memory data area
; -------------------------------------------------------------------------------------------------
string_input:  .string "To boldly go, where no man has gone before..."
string_output: .space #23

.space #32
stack_begin: .dw #0
