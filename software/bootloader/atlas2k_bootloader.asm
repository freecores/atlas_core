; *****************************************************************************************************************
; *****************************************************************************************************************
; ATLAS 2K Bootloader
;
; Bootloader MEM (ROM) pages starting at 0x8000
;
; Options:
;  -> load image via UART
;  -> execute image in memory (page: 0x0000, address: 0x0000)
;  -> load image from SPI EEPROM
;  -> burn image to SPI EEPROM
;  -> make hex dump from memory (any page)
;
; Boot configuration via CP1.COM_0_CORE.SYS_IN (bits 1 downto 0):
;  -> "00": Launch console
;  -> "01": Boot from UART
;  -> "10": Boot from SPI EEPROM (CS0)
;  -> "11": Boot from memory (page: 0x0000, address: 0x0000)
;
; SPI-EEPROM at CS0 (boot EEPROM) must have 16-bit data address (!) -> max size is 64kB - 16 bytes
; => e.g. 25LCD512
;
; Terminal configuration (CP1.COM_0_CORE.UART): 2400-8-N-1
; This bootloader does not need any RAM at all, thus the baud rate must be low
; to allow burning of the EEPROM 'on the fly'.
;
; For version info see the string section.
;
; 'Global variables'
;  usr_r0: Image size
;  usr_r1: Image checksum
;  usr_r2: Image name (0,1)
;  usr_r3: Image name (2,3)
;  usr_r4: Image name (4,5)
;  usr_r5: Image name (6,7)
;  usr_r6: Image name (8,9)
;  usr_r7: Checksum computation
; *****************************************************************************************************************
; *****************************************************************************************************************

; -----------------------------------------------------------------------------
; Processor register definitions
; -----------------------------------------------------------------------------
.equ sp           r6 ; stack pointer
.equ lr           r7 ; link register

; -----------------------------------------------------------------------------
; CPU function cores
; -----------------------------------------------------------------------------
.equ sys0_core    c0
.equ sys1_core    c1
.equ com0_core    c2
.equ com1_core    c3

; -----------------------------------------------------------------------------
; MSR bits
; -----------------------------------------------------------------------------
.equ msr_gxint_en #11 ; global external interrupt enable
.equ msr_xint0_en #12 ; enable external interrupt line 0
.equ msr_xint1_en #13 ; enable external interrupt line 1

; -----------------------------------------------------------------------------
; Configuration constans
; -----------------------------------------------------------------------------
.equ uart_baud_c #2400  ; com0_core.UART default baud rate
; keep the baud rate low - eeprom programming is done without buffering!


; *****************************************************************************************************************
; Exception Vector Table
; *****************************************************************************************************************

reset_vec:		b reset
x_int0_vec:		b boot_irq_error					; fatal error
x_int1_vec:		b boot_irq_error					; fatal error
cmd_err_vec:	b boot_irq_error					; fatal error
swi_vec:		b boot_irq_error					; fatal error


; *****************************************************************************************************************
; Interrupt service routine - this is fatal!
; *****************************************************************************************************************
boot_irq_error:
            ; restore bootloader page
            ldil  r0, #0x00							; = 0x8000
            ldih  r0, #0x80
            mcr   #1, sys1_core, r0, #2				; set d-page

            ; set alarm lights
            ldih  r0, #0b10011001
            mcr   #1, com0_core, r0, #7				; set system output

            ; print error message
            ldil  r2, low[string_err_irq]
            ldih  r2, high[string_err_irq]
            bl    uart_print_br__

            b     #+0								; freeze


; *****************************************************************************************************************
; Main Program
; *****************************************************************************************************************
reset:		; set mmu pages
            mrc   #1, r0, sys1_core, #1				; get sys i-page
            mcr   #1, sys1_core, r0, #0
            mcr   #1, sys1_core, r0, #2

            ; disable lfsr and timer
            ldil  r0, #0x00
            mcr   #1, sys0_core, r0, #3				; clear timer threshold - disable timer
            mcr   #1, sys0_core, r0, #6				; clear lfsr polynomial register - disable lfsr

            ; setup IRQ controller (for network adapter)
            ldih  r0, #0b00000010                   ; network adapter - channel 1
            mcr   #1, sys0_core, r0, #0				; set irq mask register
            ldil  r0, #0xff
            mcr   #1, sys0_core, r0, #1				; set irq config register - all rising edge
            mrc   #1, r0, sys0_core, #0				; ack pending IRQs

            ; init MSR
            ldil  r1, #0x00
            ldih  r1, #0xF8							; sys_mode, prv_sys_mode, g_irq_en, int1_en, int0_en
            stsr  r1

            ; setup Wishbone bus controller
            ldil  r0, #0b00110000                   ; bus error and timeout error IRQ enable
            ldih  r0, #0                            ; burst size = 1 word
            mcr   #1, com1_core, r0, #0				; set WB ctrl reg
            ldil  r0, #0x02                         ; offset = 1 word
            mcr   #1, com1_core, r0, #3				; set WB address offset reg
            ldil  r0, #100                          ; timeout = 100 cycles
            mcr   #1, com1_core, r0, #5				; set WB timeout reg

            ; alive LED
            ldih  r0, #0x01
            mcr   #1, com0_core, r0, #7				; set system output

            ; get system clock frequency
            mrc   #1, r0, sys1_core, #7				; clock low
            mrc   #1, r1, sys1_core, #7				; clock high

            ; baud rate
            ldil  r2, low[uart_baud_c]
            ldih  r2, high[uart_baud_c]
            ldil  r3, #15
            add   r2, r2, r3						; +15 to compensate UART's latency

            ; compute and set UART prescaler
            clr   r3
            clr   r4
main_baud_loop:
            subs  r0, r0, r2						; do bad division...
            sbcs  r1, r1, r3
            bmi   #+3
            inc   r4, r4, #1
            b     main_baud_loop
            mcr   #1, com0_core, r4, #1				; set UART prescaler

            ; activate UART core
            mrc   #1, r0, com0_core, #2				; com ctrl reg
            sbr   r0, r0, #6						; UART EN
            mcr   #1, com0_core, r0, #2				; com ctrl reg

            ; print intro
            bl    uart_linebreak__
            bl    uart_linebreak__
            ldil  r2, low[string_intro0]
            ldih  r2, high[string_intro0]
            bl    uart_print_br__

            ; print boot page
            ldil  r2, low[string_intro3]
            ldih  r2, high[string_intro3]
            bl    uart_print__
            mrc   #1, r4, sys1_core, #1				; get sys i-page
            bl    print_hex_string__
            bl    uart_linebreak__

            ; print clock speed
            ldil  r2, low[string_intro4]
            ldih  r2, high[string_intro4]
            bl    uart_print__
            mrc   #1, r5, sys1_core, #7				; clock low
            mrc   #1, r4, sys1_core, #7				; clock high
            bl    print_hex_string__				; print clock high
            mov   r4, r5
            bl    print_hex_string__				; print clock low
            bl    uart_linebreak__

            ; check boot config switches
            mrc   #1, r0, com0_core, #7				; parallel input
            ldil  r1, #0x03							; bit mask
            and   r0, r0, r1

            ; option selection
            ldil  r6, #'0'
            add   r6, r6, r0						; add ascii offset to selection
            b     console_selector


; -----------------------------------------------------------------------------------
; Terminal console
; -----------------------------------------------------------------------------------
start_console:
            ; print menu
            bl    uart_linebreak__
            ldil  r2, low[string_menu_hd]
            ldih  r2, high[string_menu_hd]
            bl    uart_print_br__
            ldil  r2, low[string_menu0]
            ldih  r2, high[string_menu0]
            bl    uart_print_br__
            ldil  r2, low[string_menup]
            ldih  r2, high[string_menup]
            bl    uart_print_br__
            ldil  r2, low[string_menud]
            ldih  r2, high[string_menud]
            bl    uart_print_br__
            ldil  r2, low[string_menur]
            ldih  r2, high[string_menur]
            bl    uart_print_br__
            ldil  r2, low[string_menuw]
            ldih  r2, high[string_menuw]
            bl    uart_print_br__

console_input:
            ldil  r2, low[string_menux]
            ldih  r2, high[string_menux]
            bl    uart_print__						; print prompt

            ; process unser input
console_wait:
            bl    uart_receivebyte__				; wait for user input
            mov   r6, r0							; backup for selector
            mov   r1, r6							; backup for echo
            bl    uart_sendbyte__					; echo selection
            bl    uart_linebreak__

            ; go to submenu
console_selector:
            ldil  r1, #'0'
            cmp   r1, r6
            beq   start_console						; restart console

            ldil  r1, #'1'
            cmp   r1, r6
            beq   boot_uart							; boot from uart

            ldil  r1, #'2'
            cmp   r1, r6
            beq   boot_eeprom						; boot from eeprom

            ldil  r1, #'3'
            cmp   r1, r6
            beq   boot_memory						; boot from memory

            ldil  r5, low[burn_eeprom]
            ldih  r5, high[burn_eeprom]
            ldil  r1, #'p'
            cmp   r1, r6
            rbaeq r5								; program eeprom

            ldil  r5, low[mem_dump]
            ldih  r5, high[mem_dump]
            ldil  r1, #'d'
            cmp   r1, r6
            rbaeq r5								; ram dump

            ldil  r5, low[wb_dump]
            ldih  r5, high[wb_dump]
            ldil  r1, #'w'
            cmp   r1, r6
            rbaeq r5								; ram dump

            ldil  r1, #'r'
            cmp   r1, r6
            bne   console_input						; invalid input

            ; do 'hard' reset
            clr   r0
            gt    r0


; -----------------------------------------------------------------------------------
; Booting from memory
; -----------------------------------------------------------------------------------
boot_memory:
            ldil  r2, low[string_booting]
            ldih  r2, high[string_booting]
            bl    uart_print_br__

            ; print no image info on start-up
            clr   r0
            stub  r2, r0

            b     start_image


; -----------------------------------------------------------------------------------
; Intermediate Brach Stops - Stop 2
; -----------------------------------------------------------------------------------
uart_print_br__:		b uart_print_br_
uart_print__:			b uart_print_
uart_linebreak__:		b uart_linebreak_
uart_sendbyte__:		b uart_sendbyte_
uart_receivebyte__:		b uart_receivebyte_
print_hex_string__:     b print_hex_string_


; -----------------------------------------------------------------------------------
; Booting from SPI EEPROM
; -----------------------------------------------------------------------------------
boot_eeprom:
            ; intro
            ldil  r2, low[string_booting]
            ldih  r2, high[string_booting]
            bl    uart_print_br_

            ; get signature
            ldil  r2, #0
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            sft   r5, r3, #swp
            ldil  r2, #1
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            orr   r5, r5, r3
            ldil  r0, #0xFE
            ldih  r0, #0xCA
            cmp   r0, r5
            bne   signature_err_

            ; get size
            ldil  r2, #2
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            sft   r5, r3, #swp
            ldil  r2, #3
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            orr   r5, r5, r3
            stub  r0, r5

            ; get checksum
            ldil  r2, #4
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            sft   r5, r3, #swp
            ldil  r2, #5
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            orr   r5, r5, r3
            stub  r1, r5

            ; get image name
            ldil  r2, #6
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            sft   r5, r3, #swp
            ldil  r2, #7
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            orr   r5, r5, r3
            stub  r2, r5

            ldil  r2, #8
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            sft   r5, r3, #swp
            ldil  r2, #9
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            orr   r5, r5, r3
            stub  r3, r5

            ldil  r2, #10
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            sft   r5, r3, #swp
            ldil  r2, #11
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            orr   r5, r5, r3
            stub  r4, r5

            ldil  r2, #12
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            sft   r5, r3, #swp
            ldil  r2, #13
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            orr   r5, r5, r3
            stub  r5, r5

            ldil  r2, #14
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            sft   r5, r3, #swp
            ldil  r2, #15
            bl    spi_eeprom_read_byte__			; read byte from eeprom
            orr   r5, r5, r3
            stub  r6, r5

            ; download program
            ldil  r6, #0							; base address MEMORY = 0x0000
            mcr   #1, sys1_core, r6, #2				; set system d-page
            stub  r7, r6							; init checksum computation

boot_eeprom_loop:
            ldil  r0, #16
            add   r2, r6, r0						; high = base EEPROM = base MEM +16
            bl    spi_eeprom_read_byte__			; get high-byte
            sft   r5, r3, #swp						; swap bytes

            ldil  r0, #17
            add   r2, r6, r0						; low = base EEPROM = base MEM +17
            bl    spi_eeprom_read_byte__			; get low-byte
            orr   r5, r5, r3						; construct word

            str   r5, r6, +#2, post, !				; save to data mem

            ; update checksum
            ldub  r0, r7
            eor   r0, r0, r5
            stub  r7, r0

            ldub  r0, r0
            cmp   r6, r0							; done?
            bne   boot_eeprom_loop

            b     download_completed


; -----------------------------------------------------------------------------------
; Booting from UART
; -----------------------------------------------------------------------------------
boot_uart:	ldil  r2, low[string_booting]
            ldih  r2, high[string_booting]
            bl    uart_print_br_
            ldil  r2, low[string_boot_wimd]
            ldih  r2, high[string_boot_wimd]
            bl    uart_print_br_

            ; check signature (2 byte)
            bl    uart_receivebyte_					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte_					; get low-byte
            orr   r1, r1, r0						; construct word
            ldil  r0, #0xFE
            ldih  r0, #0xCA
            cmp   r1, r0							; signature test
            bne   signature_err_

            ; get program size (2 byte)
            bl    uart_receivebyte_					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte_					; get low-byte
            orr   r1, r1, r0						; construct word
            sft   r1, r1, #lsl						; #bytes = 2*#words
            stub  r0, r1

            ; get checksum (2 byte)
            bl    uart_receivebyte_					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte_					; get low-byte
            orr   r1, r1, r0						; construct word
            stub  r1, r1

            ; get image name (10 byte)
            bl    uart_receivebyte_					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte_					; get low-byte
            orr   r1, r1, r0						; construct word
            stub  r2, r1
            bl    uart_receivebyte_					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte_					; get low-byte
            orr   r1, r1, r0						; construct word
            stub  r3, r1
            bl    uart_receivebyte_					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte_					; get low-byte
            orr   r1, r1, r0						; construct word
            stub  r4, r1
            bl    uart_receivebyte_					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte_					; get low-byte
            orr   r1, r1, r0						; construct word
            stub  r5 r1
            bl    uart_receivebyte_					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte_					; get low-byte
            orr   r1, r1, r0						; construct word
            stub  r6, r1

            ; init download
            clr   r5								; address = 0x00000000
            mcr   #1, sys1_core, r5, #2				; set system d-page
            stub  r7, r5							; init checksum computation

            ; downloader
uart_downloader:
            bl    uart_receivebyte_					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte_					; get low-byte
            orr   r1, r1, r0						; construct word
            str   r1, r5, +#2, post, !				; save to data mem

            ; update checksum
            ldub  r0, r7
            eor   r0, r0, r1
            stub  r7, r0

            ldub  r0, r0
            cmp   r5, r0							; done?
            bne   uart_downloader

download_completed:
            ; re-init system d page
            mrc   #1, r0, sys1_core, #1				; get sys i-page
            mcr   #1, sys1_core, r0, #2				; reset system d-page

            ; download completed
            ldil  r2, low[string_done]
            ldih  r2, high[string_done]
            bl    uart_print_br_

            ; transfer done - check checksum
            ldub  r0, r7
            ldub  r1, r1
            cmp   r0, r1
            beq   start_image

            ; checksum error!
            ldil  r2, low[string_err_check]
            ldih  r2, high[string_err_check]
            bl    uart_print_br_
            ldil  r2, low[string_err_res]
            ldih  r2, high[string_err_res]
            bl    uart_print_br_
            bl    uart_receivebyte_					; wait for any key input
            clr   r0
            gt    r0								; restart


; -----------------------------------------------------------------------------------
; Intermediate Brach Stops - Stop 1
; -----------------------------------------------------------------------------------
uart_print_br_:			b uart_print_br
uart_print_:			b uart_print
uart_linebreak_:		b uart_linebreak
uart_sendbyte_:			b uart_sendbyte
uart_receivebyte_:		b uart_receivebyte
spi_eeprom_read_byte__:	b spi_eeprom_read_byte_
signature_err_:         b signature_err
console_input_:         b console_input
print_hex_string_:      b print_hex_string0


; -----------------------------------------------------------------------------------
; Start image from memory
; -----------------------------------------------------------------------------------
start_image:
            ldil  r2, low[string_start_im]
            ldih  r2, high[string_start_im]
            bl    uart_print

            ; print image name
            ldubs r1, r2
            beq   start_image_no_text				; print image info?

            ldil  r1, #34							;'"'
            bl    uart_sendbyte
            ldub  r1, r2
            sft   r1, r1, #swp
            bl    uart_sendbyte
            sft   r1, r1, #swp
            bl    uart_sendbyte
            ldub  r1, r3
            sft   r1, r1, #swp
            bl    uart_sendbyte
            sft   r1, r1, #swp
            bl    uart_sendbyte
            ldub  r1, r4
            sft   r1, r1, #swp
            bl    uart_sendbyte
            sft   r1, r1, #swp
            bl    uart_sendbyte
            ldub  r1, r5
            sft   r1, r1, #swp
            bl    uart_sendbyte
            sft   r1, r1, #swp
            bl    uart_sendbyte
            ldub  r1, r6
            sft   r1, r1, #swp
            bl    uart_sendbyte
            sft   r1, r1, #swp
            bl    uart_sendbyte
            ldil  r1, #34							;'"'
            bl    uart_sendbyte
            bl    uart_linebreak

            ; print checksum
            ldil  r2, low[string_checksum]
            ldih  r2, high[string_checksum]
            bl    uart_print
            ldub  r4, r7							; print computed checksum
            bl    print_hex_string0

            ; some final line breaks
start_image_no_text:
            bl    uart_linebreak
            bl    uart_linebreak

            CLR   R0								; ZERO!

            ; re-init MSR
            sbr   r3, r0, #14						; prv_sys_mode
            sbr   r3, r3, #15						; sys_mode
            stsr  r3

            ; clear alive LED
            mcr   #1, com0_core, r0, #7				; set system output

            ; set IRQ base address
            mcr   #1, sys1_core, r0, #0

            ; set mmu pages, address: 0x0000
            mcr   #1, sys1_core, r0, #0
            mcr   #1, sys1_core, r0, #3
            mcr   #1, sys1_core, r0, #4
            mcr   #1, sys1_core, r0, #2				; d-page - set first
            mcr   #1, sys1_core, r0, #1				; i-page
            gt    r0								; start image at 0x0000 & 0x0000


; -----------------------------------------------------------------------------------
; RAM page dump via console
; -----------------------------------------------------------------------------------
mem_dump:	ldil  r2, low[string_edpage]
            ldih  r2, high[string_edpage]
            bl    uart_print

            ; get d-page
            bl    receive_hex_word

            ; wait for enter/abort
mem_dump_wait:
            bl    uart_receivebyte
            ldil  r1, #0x0D							; CR - enter
            cmp   r0, r1
            beq   mem_dump_continue
            ldil  r1, #0x08							; Backspace - abort
            cmp   r0, r1
            bne   mem_dump_wait
            bl    uart_linebreak
            b     console_input_					; restart terminal prompt

mem_dump_continue:
            mcr   #1, sys1_core, r4, #2				; set d-page
            bl    uart_linebreak

            ; hex word loop
            ldil  r5, #0x00							; reset byte counter
mem_dump_loop:
            ; display address?
            ldil  r0, #0x0F
            ands  r0, r5, r0
            bne   mem_dump_loop_2

            ; display!
            bl    uart_linebreak
            ldil  r1, #36							; '$'
            bl    uart_sendbyte
            mov   r4, r5
            bl    print_hex_string					; print 4hex address
            ldil  r1, #58							; ':'
            bl    uart_sendbyte
            ldil  r1, #32							; ' '
            bl    uart_sendbyte

mem_dump_loop_2:
            ldr   r4, r5, +#2, post, !				; get one word

            ; display hex data
            ldil  r1, #32							; ' '
            bl    uart_sendbyte
            bl    print_hex_string					; print 4hex data

            ; print ASCII data?
            ldil  r0, #0x0F
            ands  r0, r5, r0
            bne   mem_dump_loop_3

            ; display!
            ldil  r1, #32							; ' '
            bl    uart_sendbyte
            bl    uart_sendbyte
            ldil  r0, #16
            sub   r4, r5, r0
            ldil  r0, #0xF0
            and   r4, r4, r0
mem_dump_ascii:
            ldr   r1, r4, +#1, post, !				; get one byte
            sft   r1, r1, #swp
            ldih  r1, #0x00							; clear high byte
            ldil  r0, #32							; ' '
            cmp   r1, r0							; is ASCII command?
            bls   #+2
            ldil  r1, #46							; '.'
            bl    uart_sendbyte
            ldil  r1, #0x0F
            and   r0, r1, r4
            teq   r0, r1
            bne   mem_dump_ascii

            ; user console interrupt?
mem_dump_loop_3:
            mrc   #1, r0, com0_core, #0				; get uart RTX register
            stb   r0, #15							; copy uart rx_ready flag to T-flag
            bts   mem_dump_end

            ; check pointer
            ldil  r3, #0xFE							; last address
            teq   r3, r5
            bne   mem_dump_loop

mem_dump_end:
            bl    uart_receivebyte					; wait for enter
            clr   r0
            gt    r0								; restart bootloader


; -----------------------------------------------------------------------------------
; Intermediate Brach Stops - Stop 0
; -----------------------------------------------------------------------------------
spi_eeprom_read_byte_:	b spi_eeprom_read_byte0
print_hex_string0:      b print_hex_string


; -----------------------------------------------------------------------------------
; Burn SPI EEPROM
; -----------------------------------------------------------------------------------
burn_eeprom:
            ; disable write protection
            ldil  r0, #1							; CS0
            mcr   #1, com0_core, r0, #4				; set CS
            ldil  r0, 0b01010000					; UART EN, auto CS, MSB first, mode 0
            ldih  r0, 0b00111111					; prsc 3, length = 16 bit
            mcr   #1, com0_core, r0, #2				; SPI config

            ldil  r0, #0x00
            ldih  r0, #0x01							; write status reg
            bl    spi_trans							; iniatiate transmission
    
            ; we are ready! - waiting for image data...
            ldil  r2, low[string_prog_eep]
            ldih  r2, high[string_prog_eep]
            bl    uart_print_br
            ldil  r2, low[string_boot_wimd]
            ldih  r2, high[string_boot_wimd]
            bl    uart_print_br

            ; get signature
            bl    uart_receivebyte					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte					; get low-byte
            orr   r0, r1, r0						; construct word
            ldil  r1, #0xFE
            ldih  r1, #0xCA
            cmp   r0, r1
            bne   signature_err

            ; write signature (2 bytes)
            ldil  r2, #0
            ldil  r3, #0xCA
            bl    spi_eeprom_write_byte
            ldil  r2, #1
            ldil  r3, #0xFE
            bl    spi_eeprom_write_byte

            ; get image size
            bl    uart_receivebyte					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte					; get low-byte
            orr   r5, r1, r0						; construct word
            sft   r5, r5, #lsl						; #bytes = 2*#words
            stub  r0, r5

            ; write image size (2 bytes)
            ldil  r2, #2
            sft   r3, r5, #swp
            bl    spi_eeprom_write_byte
            ldil  r2, #3
            mov   r3, r5
            bl    spi_eeprom_write_byte

            ; get checksum
            bl    uart_receivebyte					; get high-byte
            sft   r1, r0, #swp						; swap bytes
            bl    uart_receivebyte					; get low-byte
            orr   r5, r1, r0						; construct word
            stub  r1, r5

            ; write checksum (2 bytes)
            ldil  r2, #4
            sft   r3, r5, #swp
            bl    spi_eeprom_write_byte
            ldil  r2, #5
            mov   r3, r5
            bl    spi_eeprom_write_byte

            ; write image name (10 bytes)
            ldil  r2, #6							; base address
burn_eeprom_image_name:
            bl    uart_receivebyte					; get byte
            mov   r3, r0
            bl    spi_eeprom_write_byte
            inc   r2, r2, #1
            ldil  r0, #16							; end address
            cmp   r2, r0
            bne   burn_eeprom_image_name

            ; write image data
            ldil  r2, #16							; base address
            clr   r5								; byte counter
burn_eeprom_image_data:
            bl    uart_receivebyte					; get byte
            mov   r3, r0
            bl    spi_eeprom_write_byte
            inc   r2, r2, #1
            ldub  r0, r0							; get absolute size
            inc   r5, r5, #1
            cmp   r5, r0
            bne   burn_eeprom_image_data

            ; set global write protection
            ldil  r0, #1							; CS0
            mcr   #1, com0_core, r0, #4				; set CS
            ldil  r0, 0b01010000					; UART EN, auto CS, MSB first, mode 0
            ldih  r0, 0b00111111					; prsc 3, length = 16 bit
            mcr   #1, com0_core, r0, #2				; SPI config

            ldil  r0, #0x0C							; protect all
            ldih  r0, #0x01							; status reg write
            bl    spi_trans							; iniatiate transmission

            ; we are done!
            ldil  r2, low[string_done]
            ldih  r2, high[string_done]
            bl    uart_print_br

            ; return to main console
            ldil  r5, low[start_console]
            ldih  r5, high[start_console]
            gt    r5


; -----------------------------------------------------------------------------------
; Signature error
; -----------------------------------------------------------------------------------
signature_err:
            ldil  r2, low[string_err_image]
            ldih  r2, high[string_err_image]
            bl    uart_print_br
            ldil  r2, low[string_err_res]
            ldih  r2, high[string_err_res]
            bl    uart_print_br
            bl    uart_receivebyte
            clr   r0
            gt    r0								; restart


; *****************************************************************************************************************
; Communication subroutines
; *****************************************************************************************************************

; -----------------------------------------------------------------------------------
; Intermediate Brach Stops
; -----------------------------------------------------------------------------------
spi_eeprom_read_byte0:	b spi_eeprom_read_byte


; --------------------------------------------------------------------------------------------------------
; Print char-string (bytes) via CP1.COM_0.UART and send linebreak
; Arguments: r2 = address of string (string must be zero-terminated!)
; Results: -
; Used registers: r0, r1, r2, r3, r4, lr
uart_print_br:
; --------------------------------------------------------------------------------------------------------
            ldil  r3, #0xFF
            mov   r4, lr
            b     uart_print_loop


; --------------------------------------------------------------------------------------------------------
; Print char-string (bytes) via CP1.COM_0.UART
; Arguments: r2 = address of string (string must be zero-terminated!)
; Results: -
; Used registers: r0, r1, r2, r3, r4, lr
uart_print:
; --------------------------------------------------------------------------------------------------------
            clr   r3
            mov   r4, lr
            
uart_print_loop:
            ldr   r0, r2, +#1, post, !				; get one string byte
            ldil  r1, #0x00							; upper byte mask
            ldih  r1, #0xFF
            and   r1, r0, r1
            sfts  r1, r1, #swp						; swap bytes and test if zero
            beq   uart_print_loop_end
            bl    uart_sendbyte
            b     uart_print_loop

uart_print_loop_end:
            mov   lr, r4
            teq   r3, r3							; do linebreak?
            rbaeq lr
;			b     uart_linebreak


; --------------------------------------------------------------------------------------------------------
; Print linebreak
; Arguments: -
; Results: -
; Used registers: r0, r1, r2, lr
uart_linebreak:
; --------------------------------------------------------------------------------------------------------
            mov   r2, lr
            ldil  r1, #0x0D							; carriage return
            bl    uart_sendbyte
            ldil  r1, #0x0A							; line feed
            mov   lr, r2
;			b     uart_sendbyte


; --------------------------------------------------------------------------------------------------------
; Print char (byte) via CP1.COM_0.UART
; Arguments: r1 = char (low byte)
; Results: -
; Used registers: r0, r1
uart_sendbyte:
; --------------------------------------------------------------------------------------------------------
            mrc  #1, r0, com0_core, #2				; get com control register
            stb  r0, #5								; copy uart tx_busy flag to T-flag
            bts  uart_sendbyte						; still set, keep on waiting
            mcr  #1, com0_core, r1, #0				; send data
            ret  lr


; --------------------------------------------------------------------------------------------------------
; Receive a byte via CP1.COM_0.UART
; Arguments: -
; Results: r0 (low byte)
; Used registers: r0
uart_receivebyte:
; --------------------------------------------------------------------------------------------------------
            mrc   #1, r0, com0_core, #0				; get uart status/data register
            stbi  r0, #15							; copy inverted uart rx_ready flag to T-flag
            bts   uart_receivebyte					; nothing received, keep on waiting
            ldih  r0, #0x00							; clear upper byte
            ret   lr


; --------------------------------------------------------------------------------------------------------
; Reads 16 bit data as 4x hex chars via UART (and echo them)
; Arguments: -
; Results:
;  r4 = data
; Used registers: r0, r1, r2, r3, r4, lr
receive_hex_word:
; --------------------------------------------------------------------------------------------------------
            mov   r2, lr							; backup link regsiter
            ldil  r4, #0							; clear data register
            ldil  r3, #4							; number of chars

receive_hex_word_loop:
            bl    uart_receivebyte					; get one char

            ; convert to higher case
            ldil  r1, #'F'
            cmp   r0, r1
            bmi   #+3								; skip decrement
            ldil  r1, #32							; -> to lower case
            sub   r0, r0, r1

            ; is valid?
            ldil  r1, #'0'
            cmp   r0, r1
            bmi   receive_hex_word_loop				; if less than '0'

            ldil  r1, #'F'
            cmp   r1, r0
            bmi   receive_hex_word_loop				; if higher than 'F'

            ldil  r1, #'9'
            cmp   r1, r0
            bls   receive_hex_word_echo				; if less than '9'

            ldil  r1, #'A'
            cmp   r0, r1
            bhi   receive_hex_word_loop				; if less than 'A'

            ; echo char
receive_hex_word_echo:
            mov   r1, r0
            bl    uart_sendbyte

            ; do conversion
            ldil  r0, #'0'
            sub   r1, r1, r0
            ldil  r0, #9
            cmp   r0, r1
            bls   #+2								; '0'..'9' -> ok
            dec   r1, r1, #7						; 'A' - '0' - 10 = 7 -> 'A'..'F' -> ok

            ; save conversion data
            sft   r4, r4, #rol
            sft   r4, r4, #rol
            sft   r4, r4, #rol
            sft   r4, r4, #rol
            orr   r4, r4, r1

            ; loop controller
            decs  r3, r3, #1
            bne   receive_hex_word_loop

            ret   r2								; return


; --------------------------------------------------------------------------------------------------------
; Prints 16bit data as 4x char hex value
; Arguments:
;  r4 = data
; Results: -
; Used registers: r0, r1, r2, r3, r4, r6, lr
print_hex_string:
; --------------------------------------------------------------------------------------------------------
            mov   r6, lr							; backup link regiiter

            ; char 3
            sft   r2, r4, #rol
            sft   r2, r2, #rol
            sft   r2, r2, #rol
            sft   r2, r2, #rol
            bl    conv_hex_comp
            bl    uart_sendbyte

            ; char 2
            sft   r2, r4, #swp
            bl    conv_hex_comp
            bl    uart_sendbyte

            ; char 1
            sft   r2, r4, #lsr
            sft   r2, r2, #lsr
            sft   r2, r2, #lsr
            sft   r2, r2, #lsr
            bl    conv_hex_comp
            bl    uart_sendbyte

            ; char 0
            mov   r2, r4
            bl    conv_hex_comp
            bl    uart_sendbyte

            ret   r6

; compute hex-char from 4-bit value of r2, result in r1
conv_hex_comp:	ldil r1, #0x0f					; mask for lowest 4 bit
                and  r2, r2, r1

                ldil r1, #9
                cmp  r1, r2
                bcs  #+3

                ldil r1, #48					; this is a '0'
                b    #+2
                ldil r1, #55					; this is an 'A'-10
                add  r1, r1, r2					; resulting char in lower byte
                ret  lr


; --------------------------------------------------------------------------------------------------------
; Starts SPI transmission and waits for transmission to finish
; Arguments:
;  r0 = TX data
; Results:
;  r0 = RX data
; Used registers: r0
spi_trans:
; --------------------------------------------------------------------------------------------------------
            mcr   #1, com0_core, r0, #3				; set SPI data - start transfer

            ; wait for end
            mrc   #1, r0, com0_core, #2				; get status reg
            stb   r0, #3							; busy flag
            bts   #-2								; still set?

            mrc   #1, r0, com0_core, #3				; get received data

            ret   lr


; --------------------------------------------------------------------------------------------------------
; Writes 1 byte to serial EEPROM @ CS0
; Arguments:
;  r2 = Address word
;  r3 = Data byte (low part)
; Results: -
; Used registers: r0, r1, r2, r3, lr
spi_eeprom_write_byte:
; --------------------------------------------------------------------------------------------------------
            mov   r1, lr							; save link register

            ; set write-enable latch
            ; -------------------------------------------
            ldil  r0, 0b01010000					; UART EN, auto CS, MSB first, mode 0
            ldih  r0, 0b00110111					; prsc 3, length = 8 bit
            mcr   #1, com0_core, r0, #2				; SPI config
            ldil  r0, #1							; CS0
            mcr   #1, com0_core, r0, #4				; config CS
            ldil  r0, #0x06							; write enable command
            bl    spi_trans							; iniatiate transmission

            ; check status reg
            ; -------------------------------------------
            ldil  r0, 0b01010000					; UART EN, auto CS, MSB first, mode 0
            ldih  r0, 0b00111111					; prsc 3, length = 16 bit
            mcr   #1, com0_core, r0, #2				; SPI config
            ldil  r0, #0x00
            ldih  r0, #0x05							; read SREG command
            bl    spi_trans							; iniatiate transmission

            ; check WIP flag
            stb   r0, #1							; WEL flag
            bts   spi_eeprom_write

            ; EEPROM ACCESS ERROR!
            ldil  r2, low[string_err_eep]
            ldih  r2, high[string_err_eep]
            bl    uart_print_br
            ldil  r2, low[string_err_res]
            ldih  r2, high[string_err_res]
            bl    uart_print_br
            bl    uart_receivebyte
            clr   r0
            gt    r0								; restart

spi_eeprom_write:
            ; send write instruction and
            ; high address byte (16 bit trans)
            ; -------------------------------------------
            ldil  r0, 0b01000000					; UART EN, manual CS, MSB first, mode 0
            ldih  r0, 0b00111111					; prsc 3, length = 16 bit
            mcr   #1, com0_core, r0, #2				; SPI config

            ldil  r0, #1							; CS0
            mcr   #1, com0_core, r0, #4				; pre-assert CS

            sft   r0, r2, #swp						; swap high address byte to low byte
            ldih  r0, #0x02							; write command
            bl    spi_trans							; iniatiate transmission

            ; send low address byte and
            ; send data byte (16 bit trans)
            ; -------------------------------------------
            mov   lr, r2							; copy address
            ldih  lr, #0x00							; clear high part
            sft   lr, lr, #swp						; swap bytes
            mov   r0, r3							; copy data byte
            ldih  r0, #0x00							; clear high data byte
            orr   r0, r0, lr						; merge low address and data byte
            bl    spi_trans							; iniatiate transmission

            clr   r0
            mcr   #1, com0_core, r0, #4				; de-assert CS

            ; wait for write command to finish
            ; 16 bit transfer
            ; -------------------------------------------
            ldil  r0, 0b01010000					; UART EN, auto CS, MSB first, mode 0
            ldih  r0, 0b00111111					; prsc 3, length = 16 bit
            mcr   #1, com0_core, r0, #2				; SPI config
            ldil  r0, #1							; CS0
            mcr   #1, com0_core, r0, #4				; set CS

spi_eeprom_write_byte_bsy:
            ldil  r0, #0x00
            ldih  r0, #0x05							; read SREG command
            bl    spi_trans							; iniatiate transmission

            ; check WIP flag
            stb   r0, #0							; WIP flag
            bts   spi_eeprom_write_byte_bsy

            ret   r1


; --------------------------------------------------------------------------------------------------------
; Reads 1 byte from serial EEPROM @ CS0
; Arguments:
;  r2 = Address word
; Results:
;  r3 = Data byte (low part)
; Used registers: r0, r1, r2, r3, lr
spi_eeprom_read_byte:
; --------------------------------------------------------------------------------------------------------
            mov   r1, lr							; save link register

            ; config SPI
            ldil  r0, 0b01000000					; UART EN, manual CS, MSB first, mode 0
            ldih  r0, 0b00111111					; prsc 3, length = 16 bit
            mcr   #1, com0_core, r0, #2				; SPI config
            ldil  r0, #1							; CS0
            mcr   #1, com0_core, r0, #4				; pre-assert CS

            ; send read instruction and
            ; high address (16 bit trans)
            ; -------------------------------------------
            sft   r0, r2, #swp						; swap high address byte to low byte
            ldih  r0, #0x03							; read command
            bl    spi_trans							; iniatiate transmission

            ; send low address byte and
            ; read data byte (16 bit trans)
            ; -------------------------------------------
            mov   r0, r2							; copy address
            ldih  r0, #0x00							; data tranfer dummy
            sft   r0, r0, #swp						; swap data and address bytes
            bl    spi_trans							; iniatiate transmission

            clr   r3
            mcr   #1, com0_core, r3, #4				; deassert CS
            mov   r3, r0
            ldih  r3, #0x00							; clear high byte

            ret   r1


; -----------------------------------------------------------------------------------
; Wisbone Dump
; -----------------------------------------------------------------------------------
wb_dump:    ldil  r2, low[string_ewbadr]
            ldih  r2, high[string_ewbadr]
            bl    uart_print

            ; get and set address (32-bit)
            bl    receive_hex_word
            mcr   #1, com1_core, r4, #2             ; set high part of base address
            bl    receive_hex_word
            mcr   #1, com1_core, r4, #1             ; set low part of base address

wb_dump_wait:
            ; execute?
            bl    uart_receivebyte
            ldil  r1, #0x0D							; CR - enter
            cmp   r0, r1
            beq   wb_dump_proceed

            ; abort?
            ldil  r1, #0x08							; Backspace - abort
            cmp   r0, r1
            beq   wb_dump_end
            b     wb_dump_wait

            ; download word from wishbone net
wb_dump_proceed:
            bl    wb_read_word
            mov   r6, r0

            ; print it
            ldil  r2, low[string_data]
            ldih  r2, high[string_data]
            bl    uart_print
            mov   r4, r6
            bl    print_hex_string

            ; return to main console
wb_dump_end:
            bl    uart_linebreak
            ldil  r5, low[console_input]
            ldih  r5, high[console_input]
            gt    r5


; --------------------------------------------------------------------------------------------------------
; Reads 1 word from the Wishbone network (base address must be set before, auto address increment)
; Arguments: -
; Results:
;  r0 = data
; Used registers: r0 ,lr
wb_read_word:
; --------------------------------------------------------------------------------------------------------
            cdp   #1, com1_core, com1_core, #0      ; initiate read-transfer
            mrc   #1, r0, com1_core, #0				; get WB status reg
            stb   r0, #6                            ; busy flag -> t-flag
            bts   #-2                               ; repeat until data is ready

            mrc   #1, r0, com1_core, #4				; get data
            ret   lr


; *****************************************************************************************************************
; ROM: Text strings
; *****************************************************************************************************************

string_intro0:    .stringz "Atlas-2K Bootloader - V20140410\nby Stephan Nolting, stnolting@gmail.com\nwww.opencores.org/project,atlas_core\n"
string_intro3:    .stringz "Bootloader @ 0x"
string_intro4:    .stringz "Clock (Hz): 0x"

string_booting:   .stringz "Booting..."
string_prog_eep:  .stringz "Burning EEPROM"
string_boot_wimd: .stringz "Waiting for image data..."
string_start_im:  .stringz "Starting image "
string_done:      .stringz "Download completed!"
string_edpage:    .stringz "Enter page (4hex): 0x"
string_ewbadr:    .stringz "Enter addr (8hex): 0x"
string_checksum:  .stringz "Checksum: 0x"
string_data:      .stringz "\n-> 0x"

string_menu_hd:   .stringz "cmd/boot-switch"
string_menu0:     .stringz " 0/'00': Restart console\n 1/'01': Boot from UART\n 2/'10': Boot from EEPROM\n 3/'11': Boot from memory"
string_menup:     .stringz " p: Burn EEPROM"
string_menud:     .stringz " d: RAM dump"
string_menur:     .stringz " r: Reset"
string_menuw:     .stringz " w: WB dump"
string_menux:     .stringz "cmd:> "

string_err_image: .stringz "IMAGE ERR!"
string_err_irq:   .stringz "\nIRQ ERR!"
string_err_check: .stringz "CHECKSUM ERR!"
string_err_eep:   .stringz "SPI/EEPROM ERR!"
string_err_res:   .stringz "Press any key"
