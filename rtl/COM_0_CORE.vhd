-- #########################################################
-- #   << ATLAS Project - Communication Controller 0 >>    #
-- # ***************************************************** #
-- #  -> UART (RXD, TXD)                                   #
-- #  -> SPI (8 channels)                                  #
-- #  -> Parallel IO  (16 in, 16 out)                      #
-- #  -> System IO (8 in, 8 out)                           #
-- # ***************************************************** #
-- #  Last modified: 11.05.2014                            #
-- # ***************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany            #
-- #########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity COM_0_CORE is
	port	(
-- ###############################################################################################
-- ##           Host Interface                                                                  ##
-- ###############################################################################################

				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(15 downto 0); -- write data
				DAT_O           : out std_logic_vector(15 downto 0); -- read data

-- ###############################################################################################
-- ##           Interrupt Lines                                                                 ##
-- ###############################################################################################

				UART_RX_IRQ_O   : out std_logic; -- UART IRQ "data available"
				UART_TX_IRQ_O   : out std_logic; -- UART IRQ "sending done"
				SPI_IRQ_O       : out std_logic; -- SPI IRQ "transfer done"
				PIO_IRQ_O       : out std_logic; -- PIO input pin change irq

-- ###############################################################################################
-- ##           Communication Lines                                                             ##
-- ###############################################################################################

				UART_TXD_O      : out std_logic; -- UART serial output
				UART_RXD_I      : in  std_logic; -- UART serial input
				SPI_MOSI_O      : out std_logic_vector(07 downto 0); -- serial data out
				SPI_MISO_I      : in  std_logic_vector(07 downto 0); -- serial data in
				SPI_SCK_O       : out std_logic_vector(07 downto 0); -- serial clock out
				SPI_CS_O        : out std_logic_vector(07 downto 0); -- chip select (low active)
				PIO_IN_I        : in  std_logic_vector(15 downto 0); -- parallel input
				PIO_OUT_O       : out std_logic_vector(15 downto 0); -- parallel output
				SYS_IO_I        : in  std_logic_vector(07 downto 0); -- system input
				SYS_IO_O        : out std_logic_vector(07 downto 0)  -- system output
			);
end COM_0_CORE;

architecture COM_0_CORE_BEHAV of COM_0_CORE is

	-- Module Addresses --
	constant uart_rtx_sd_reg_c      : std_logic_vector(02 downto 0) := "000"; -- R/W: UART RTX data + status flags
	constant uart_prsc_reg_c        : std_logic_vector(02 downto 0) := "001"; -- R/W: UART prescaler register
	constant com_ctrl_reg_c         : std_logic_vector(02 downto 0) := "010"; -- R/W: COM control register
	constant spi_data_reg_c         : std_logic_vector(02 downto 0) := "011"; -- R/W: SPI RTX data register
	constant spi_cs_reg_c           : std_logic_vector(02 downto 0) := "100"; -- R/W: SPI chip select register
	constant pio_in_reg_c           : std_logic_vector(02 downto 0) := "101"; -- R:   PIO input register
	constant pio_out_reg_c          : std_logic_vector(02 downto 0) := "110"; -- R/W: PIO output register
	constant sys_io_reg_c           : std_logic_vector(02 downto 0) := "111"; -- R/W: System parallel in/out

	-- CTRL Register --
	constant spi_cr_dir_flag_c      : natural :=  0; -- R/W: 0: MSB first, 1: LSB first
	constant spi_cr_cpol_c          : natural :=  1; -- R/W: clock polarity, 1: idle '1' clock, 0: idle '0' clock
	constant spi_cr_cpha_c          : natural :=  2; -- R/W: edge offset: 0: first edge, 1: second edge
	constant spi_cr_bsy_c           : natural :=  3; -- R:   transceiver is busy when '1'
	constant spi_cr_auto_cs_c       : natural :=  4; -- R/W: Auto apply CS when '1'
	constant uart_tx_busy_c         : natural :=  5; -- R:   UART transmitter is busy
	constant uart_en_c              : natural :=  6; -- R/W: UART enable
	constant uart_ry_ovf_c          : natural :=  7; -- R:   UART Rx overflow corruption
	constant spi_cr_ln_lsb_c        : natural :=  8; -- R/W: data length lsb
	constant spi_cr_ln_msb_c        : natural := 11; -- R/W: data length msb
	constant spi_cr_prsc_lsb_c      : natural := 12; -- R/W: SPI clock prescaler lsb
	constant spi_cr_prsc_msb_c      : natural := 15; -- R/W: SPI clock prescaler msb

	-- UART Control Flags (UART RTX REG) --
	constant uart_rx_ready_c        : natural := 15; -- R: Data received

	-- UART Registers --
	signal UART_RX_REG              : std_logic_vector(07 downto 0);
	signal UART_PRSC_REG            : std_logic_vector(15 downto 0);

	-- UART Transceiver --
	signal UART_RX_SYNC             : std_logic_vector(03 downto 0);
	signal UART_TX_BSY_FLAG         : std_logic;
    signal UART_DCOR_FLAG           : std_logic;
	signal UART_RX_BSY_FLAG         : std_logic;
	signal UART_TX_SREG             : std_logic_vector(09 downto 0);
	signal UART_RX_SREG             : std_logic_vector(09 downto 0);
	signal UART_TX_BIT_CNT          : std_logic_vector(03 downto 0);
	signal UART_RX_BIT_CNT          : std_logic_vector(03 downto 0);
	signal UART_TX_BAUD_CNT         : std_logic_vector(15 downto 0);
	signal UART_RX_BAUD_CNT         : std_logic_vector(15 downto 0);
	signal UART_RX_READY            : std_logic;
	signal UART_RX_READY_SYNC       : std_logic;

	-- SPI Registers --
	signal SPI_TX_REG               : std_logic_vector(15 downto 0);
	signal SPI_RX_REG               : std_logic_vector(15 downto 0);
	signal SPI_RX_REG_NXT           : std_logic_vector(15 downto 0);
	signal SPI_CS_REG               : std_logic_vector(07 downto 0);
	signal COM_CONFIG_REG           : std_logic_vector(15 downto 0);

	-- SPI Transceiver --
	signal SPI_IN_BUF               : std_logic_vector(01 downto 0);
	signal SPI_MOSI_NXT             : std_logic;
	signal SPI_SCK_NXT              : std_logic;
	signal SPI_MOSI_FF              : std_logic;
	signal SPI_CS_FF                : std_logic_vector(07 downto 0);
	signal SPI_CS_FF_NXT            : std_logic_vector(07 downto 0);
	signal SPI_IRQ                  : std_logic;

	-- SPI Arbiter --
	type   spi_arb_state_type is (IDLE, START_TRANS, TRANSMIT_0, TRANSMIT_1, FINISH);
	signal SPI_ARB_STATE            : spi_arb_state_type;
	signal SPI_ARB_STATE_NXT        : spi_arb_state_type;
	signal SPI_BIT_CNT              : std_logic_vector(04 downto 0);
	signal SPI_BIT_CNT_NXT          : std_logic_vector(04 downto 0);
	signal SPI_RX_SFT               : std_logic_vector(15 downto 0); -- rx shift registers
	signal SPI_RX_SFT_NXT           : std_logic_vector(15 downto 0); -- rx shift registers
	signal SPI_TX_SFT               : std_logic_vector(15 downto 0); -- tx shift registers
	signal SPI_TX_SFT_NXT           : std_logic_vector(15 downto 0); -- tx shift registers
	signal SPI_PRSC_CNT             : std_logic_vector(15 downto 0);
	signal SPI_PRSC_CNT_NXT         : std_logic_vector(15 downto 0);
	signal SPI_BUSY_FLAG            : std_logic;
	signal SPI_BUSY_FLAG_NXT        : std_logic;
	signal SPI_SCK_FF               : std_logic;
	signal SPI_MISO                 : std_logic;

	-- PIO Registers --
	signal PIO_OUT_DATA             : std_logic_vector(15 downto 0);
	signal PIO_IN_DATA              : std_logic_vector(15 downto 0);
	signal PIO_SYNC                 : std_logic_vector(15 downto 0);
	signal SYS_IO_I_FF              : std_logic_vector(07 downto 0);
	signal SYS_IO_O_FF              : std_logic_vector(07 downto 0);

begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					UART_PRSC_REG  <= (others => '0');
					COM_CONFIG_REG <= (others => '0');
					SPI_TX_REG     <= (others => '0');
					SPI_CS_REG     <= (others => '0');
					PIO_IN_DATA    <= (others => '0');
					PIO_OUT_DATA   <= (others => '0');
					PIO_SYNC       <= (others => '0');
					SYS_IO_O_FF    <= (others => '0');
					SYS_IO_I_FF    <= (others => '0');
				elsif (ICE_I = '1') then -- interface enable
					if (W_EN_I = '1') then -- register update
						case (ADR_I) is
							when uart_prsc_reg_c => UART_PRSC_REG  <= DAT_I;
							when com_ctrl_reg_c  => COM_CONFIG_REG <= DAT_I;
							when spi_data_reg_c  => SPI_TX_REG     <= DAT_I;
							when spi_cs_reg_c    => SPI_CS_REG     <= DAT_I(07 downto 00);
							when pio_out_reg_c   => PIO_OUT_DATA   <= DAT_I;
							when sys_io_reg_c    => SYS_IO_O_FF    <= DAT_I(15 downto 08);
							when others          => NULL;
						end case;
					end if;
				end if;
				PIO_SYNC    <= PIO_IN_DATA;
				PIO_IN_DATA <= PIO_IN_I; -- pio input
				SYS_IO_I_FF <= SYS_IO_I;
			end if;
		end process W_ACC;

		-- Output --
		PIO_OUT_O <= PIO_OUT_DATA;
		SYS_IO_O <= SYS_IO_O_FF;

		-- PIO Input pin change IRQ --
		PIO_IRQ_O <= '0' when (PIO_SYNC = PIO_IN_DATA) else '1';



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, UART_TX_BSY_FLAG, UART_RX_READY, UART_RX_REG, UART_PRSC_REG, COM_CONFIG_REG, SYS_IO_O_FF,
		               SPI_BUSY_FLAG, SPI_CS_REG, SPI_RX_REG, PIO_OUT_DATA, PIO_IN_DATA, SYS_IO_I_FF, UART_DCOR_FLAG)
		begin
			case (ADR_I) is
				when uart_rtx_sd_reg_c => DAT_O                  <= (others => '0');
				                          DAT_O(7 downto 0)      <= UART_RX_REG;
				                          DAT_O(uart_rx_ready_c) <= UART_RX_READY;
				when uart_prsc_reg_c   => DAT_O <= UART_PRSC_REG;
				when com_ctrl_reg_c    => DAT_O                  <= COM_CONFIG_REG;
				                          DAT_O(spi_cr_bsy_c)    <= SPI_BUSY_FLAG;
				                          DAT_O(uart_tx_busy_c)  <= UART_TX_BSY_FLAG;
				                          DAT_O(uart_ry_ovf_c)   <= UART_DCOR_FLAG;
				when spi_data_reg_c    => DAT_O <= SPI_RX_REG;
				when spi_cs_reg_c      => DAT_O <= x"00" & SPI_CS_REG;
				when pio_in_reg_c      => DAT_O <= PIO_IN_DATA;
				when pio_out_reg_c     => DAT_O <= PIO_OUT_DATA;
				when sys_io_reg_c      => DAT_O <= SYS_IO_O_FF & SYS_IO_I_FF;
				when others            => DAT_O <= x"0000";
			end case;
		end process R_ACC;



	-- UART Flag Arbiter -----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		UART_FLAG_CTRL: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					UART_RX_READY      <= '0';
					UART_RX_READY_SYNC <= '0';
                    UART_DCOR_FLAG     <= '0';
				else
					-- Ready flag and corruption flag --
					UART_RX_READY_SYNC <= UART_RX_BSY_FLAG;
					if (UART_RX_READY = '1') and (R_EN_I = '1') and (ADR_I = uart_rtx_sd_reg_c) and (ICE_I = '1') then
						UART_RX_READY  <= '0';
                        UART_DCOR_FLAG <= '0';
					elsif (UART_RX_READY_SYNC = '1') and (UART_RX_BSY_FLAG = '0') then -- falling edge
						UART_RX_READY  <= '1';
                        UART_DCOR_FLAG <= UART_RX_READY;
					end if;
				end if;
			end if;
		end process UART_FLAG_CTRL;

		-- Interrupt output --
		UART_RX_IRQ_O <= UART_RX_READY;
		UART_TX_IRQ_O <= not UART_TX_BSY_FLAG;



	-- Transmitter Unit ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		UART_TRANSMITTER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					UART_TX_BSY_FLAG <= '0';
					UART_TX_SREG     <= (others => '1');
					UART_TX_BIT_CNT  <= (others => '0');
					UART_TX_BAUD_CNT <= (others => '0');
				else
					-- UART disabled
					if (COM_CONFIG_REG(uart_en_c) = '0') then
						UART_TX_BSY_FLAG <= '0';
						UART_TX_SREG     <= (others => '1');
						UART_TX_BIT_CNT  <= (others => '0');
						UART_TX_BAUD_CNT <= (others => '0');

					-- UART TX register --
					elsif (UART_TX_BSY_FLAG = '0') then
						UART_TX_BIT_CNT  <= "1010"; -- 10 bits
						UART_TX_BAUD_CNT <= UART_PRSC_REG;
						if (W_EN_I = '1') and (ADR_I = uart_rtx_sd_reg_c) then
							UART_TX_BSY_FLAG <= '1';
							UART_TX_SREG     <= '1' & DAT_I(7 downto 0) & '0'; -- stopbit & data & startbit
						end if;
					else
						if (UART_TX_BAUD_CNT = x"0000") then
							UART_TX_BAUD_CNT <= UART_PRSC_REG;
							if (UART_TX_BIT_CNT /= "0000") then
								UART_TX_SREG    <= '1' & UART_TX_SREG(9 downto 1);
								UART_TX_BIT_CNT <= std_logic_vector(unsigned(UART_TX_BIT_CNT) - 1);
							else
								UART_TX_BSY_FLAG <= '0'; -- done
							end if;
						else
							UART_TX_BAUD_CNT <= std_logic_vector(unsigned(UART_TX_BAUD_CNT) - 1);
						end if;
					end if;
				end if;
			end if;
		end process UART_TRANSMITTER;

		-- Transmitter output --
		UART_TXD_O <= UART_TX_SREG(0);



	-- UART Receiver Unit ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		UART_RECEIVER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					UART_RX_BSY_FLAG <= '0';
					UART_RX_SREG     <= (others => '0');
					UART_RX_BIT_CNT  <= (others => '0');
					UART_RX_BAUD_CNT <= (others => '0');
					UART_RX_SYNC     <= (others => '1');
					UART_RX_REG      <= (others => '0');
				else
					-- Synchronizer --
					if (COM_CONFIG_REG(uart_en_c) = '1') then
						UART_RX_SYNC <= UART_RXD_I & UART_RX_SYNC(3 downto 1);
					end if;

					-- UART disabled --
					if (COM_CONFIG_REG(uart_en_c) = '0') then
						UART_RX_BSY_FLAG <= '0';
						UART_RX_SREG     <= (others => '0');
						UART_RX_BIT_CNT  <= (others => '0');
						UART_RX_BAUD_CNT <= (others => '0');
						UART_RX_SYNC     <= (others => '1');
						UART_RX_REG      <= (others => '0');
					
					-- RX shift reg --
					elsif (UART_RX_BSY_FLAG = '0') then
						UART_RX_BIT_CNT  <= "1001"; -- 9 bits (startbit + 8 data bits)
						UART_RX_BAUD_CNT <= '0' & UART_PRSC_REG(15 downto 1); -- half baud rate, sample in middle
						if (UART_RX_SYNC(1 downto 0) = "01") then -- start 'bit' detected (falling logical edge)
							UART_RX_BSY_FLAG <= '1';
						end if;
					else
						if (UART_RX_BAUD_CNT = x"0000") then
							UART_RX_BAUD_CNT <= UART_PRSC_REG;
							if (UART_RX_BIT_CNT /= "0000") then
								UART_RX_SREG    <= UART_RX_SYNC(0) & UART_RX_SREG(9 downto 1);
								UART_RX_BIT_CNT <= std_logic_vector(unsigned(UART_RX_BIT_CNT) - 1);
							else
								UART_RX_BSY_FLAG <= '0'; -- done
								UART_RX_REG      <= UART_RX_SREG(9 downto 2);
							end if;
						else
							UART_RX_BAUD_CNT <= std_logic_vector(unsigned(UART_RX_BAUD_CNT) - 1);
						end if;
					end if;
				end if;
			end if;
		end process UART_RECEIVER;



	-- SPI Transceiver Unit --------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		SPI_ARB_SYNC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					SPI_ARB_STATE  <= IDLE;
					SPI_RX_SFT     <= (others => '0');
					SPI_TX_SFT     <= (others => '0');
					SPI_BIT_CNT    <= (others => '0');
					SPI_PRSC_CNT   <= (others => '0');
					SPI_RX_REG     <= (others => '0');
					SPI_SCK_FF     <= '0';
					SPI_MOSI_FF    <= '0';
					SPI_IN_BUF     <= "00";
					SPI_CS_FF      <= (others => '1');
					SPI_BUSY_FLAG  <= '0';
					SPI_IRQ_O      <= '0';
				else
					SPI_ARB_STATE  <= SPI_ARB_STATE_NXT;
					SPI_RX_SFT     <= SPI_RX_SFT_NXT;
					SPI_TX_SFT     <= SPI_TX_SFT_NXT;
					SPI_BIT_CNT    <= SPI_BIT_CNT_NXT;
					SPI_PRSC_CNT   <= SPI_PRSC_CNT_NXT;
					SPI_RX_REG     <= SPI_RX_REG_NXT;
					SPI_SCK_FF     <= SPI_SCK_NXT;
					SPI_MOSI_FF    <= SPI_MOSI_NXT;
					SPI_IN_BUF     <= SPI_IN_BUF(0) & SPI_MISO;
					if (COM_CONFIG_REG(spi_cr_auto_cs_c) = '1') then -- auto apply chip select
						SPI_CS_FF  <= SPI_CS_FF_NXT;
					else -- manually apply chip select
						SPI_CS_FF  <= not SPI_CS_REG;
					end if;
					SPI_BUSY_FLAG  <= SPI_BUSY_FLAG_NXT;
					SPI_IRQ_O      <= SPI_IRQ;
				end if;
			end if;
		end process SPI_ARB_SYNC;



		SPI_ARB_COMB: process(SPI_ARB_STATE, COM_CONFIG_REG, SPI_RX_SFT, SPI_TX_SFT, SPI_BIT_CNT, SPI_PRSC_CNT, SPI_IN_BUF,
		                      SPI_RX_REG, SPI_MOSI_FF, SPI_CS_FF, SPI_CS_REG, SPI_TX_REG, W_EN_I, ADR_I, SPI_BUSY_FLAG, ICE_I)
			variable prsc_match_v : std_logic;
		begin
			-- Defaults --
			SPI_ARB_STATE_NXT <= SPI_ARB_STATE; -- arbiter state
			SPI_RX_SFT_NXT    <= SPI_RX_SFT;    -- rx shift register
			SPI_TX_SFT_NXT    <= SPI_TX_SFT;    -- tx shift register
			SPI_BIT_CNT_NXT   <= SPI_BIT_CNT;   -- bit counter
			SPI_PRSC_CNT_NXT  <= SPI_PRSC_CNT;  -- SPI clock prescaler
			SPI_RX_REG_NXT    <= SPI_RX_REG;    -- complete received data
			SPI_SCK_NXT       <= COM_CONFIG_REG(spi_cr_cpol_c); -- clock polarity
			SPI_MOSI_NXT      <= SPI_MOSI_FF;   -- serial data output
			SPI_BUSY_FLAG_NXT <= SPI_BUSY_FLAG; -- busy flag
			SPI_IRQ           <= '0';           -- no interrupt
			prsc_match_v      := SPI_PRSC_CNT(to_integer(unsigned(COM_CONFIG_REG(spi_cr_prsc_msb_c downto spi_cr_prsc_lsb_c)))); -- prescaler match

			-- State machine --
			case (SPI_ARB_STATE) is -- IDLE, START_TRANS, TRANSMIT, END_TRANS

				when IDLE => -- Wait for transmitter init
					SPI_CS_FF_NXT     <= (others => '1'); -- deselct all slaves
					SPI_BIT_CNT_NXT   <= (others => '0');
					SPI_RX_SFT_NXT    <= (others => '0');
					SPI_PRSC_CNT_NXT  <= (others => '0');
					SPI_MOSI_NXT      <= '0';
					SPI_SCK_NXT       <= COM_CONFIG_REG(spi_cr_cpol_c); -- idle clk polarity
					if (W_EN_I = '1') and (ADR_I = spi_data_reg_c) and (ICE_I = '1') then
						SPI_ARB_STATE_NXT <= START_TRANS;
						SPI_BUSY_FLAG_NXT <= '1';
					end if;

				when START_TRANS => -- Apply slave select signal
					SPI_TX_SFT_NXT    <= SPI_TX_REG;
					SPI_CS_FF_NXT     <= not SPI_CS_REG;
					SPI_ARB_STATE_NXT <= TRANSMIT_0;

				when TRANSMIT_0 => -- first half of bit transmission
					SPI_CS_FF_NXT    <= SPI_CS_FF; -- keep CS alive
					SPI_PRSC_CNT_NXT <= std_logic_vector(unsigned(SPI_PRSC_CNT) + 1);
					SPI_SCK_NXT      <= COM_CONFIG_REG(spi_cr_cpol_c) xor COM_CONFIG_REG(spi_cr_cpha_c);
					if (COM_CONFIG_REG(spi_cr_dir_flag_c) = '0') then -- MSB first
						SPI_MOSI_NXT <= SPI_TX_SFT(to_integer(unsigned(COM_CONFIG_REG(spi_cr_ln_msb_c downto spi_cr_ln_lsb_c))));
					else -- LSB first
						SPI_MOSI_NXT <= SPI_TX_SFT(0);
					end if;
					if (prsc_match_v = '1') then -- first half completed
						SPI_ARB_STATE_NXT <= TRANSMIT_1;
						SPI_PRSC_CNT_NXT  <= (others => '0');
					end if;

				when TRANSMIT_1 => -- second half of bit transmission
					SPI_CS_FF_NXT    <= SPI_CS_FF; -- keep CS alive
					SPI_PRSC_CNT_NXT <= std_logic_vector(unsigned(SPI_PRSC_CNT) + 1);
					SPI_SCK_NXT      <= not (COM_CONFIG_REG(spi_cr_cpol_c) xor COM_CONFIG_REG(spi_cr_cpha_c));
					if (prsc_match_v = '1') then -- second half completed
						SPI_BIT_CNT_NXT  <= std_logic_vector(unsigned(SPI_BIT_CNT) + 1);
						SPI_PRSC_CNT_NXT <= (others => '0');
						if (COM_CONFIG_REG(spi_cr_dir_flag_c) = '0') then -- MSB first
							SPI_TX_SFT_NXT <= SPI_TX_SFT(14 downto 0) & '0'; -- left shift
							SPI_RX_SFT_NXT <= SPI_RX_SFT(14 downto 0) & SPI_IN_BUF(1); -- left shift
						else -- LSB first
							SPI_TX_SFT_NXT <= '0' & SPI_TX_SFT(15 downto 1); -- right shift
							SPI_RX_SFT_NXT <= SPI_IN_BUF(1) & SPI_TX_SFT(15 downto 1); -- right shift
						end if;
						if (to_integer(unsigned(SPI_BIT_CNT)) = to_integer(unsigned(COM_CONFIG_REG(spi_cr_ln_msb_c downto spi_cr_ln_lsb_c)))) then
							SPI_ARB_STATE_NXT <= FINISH;
						else
							SPI_ARB_STATE_NXT <= TRANSMIT_0;
						end if;
					end if;

				when FINISH => -- finish transfer
					SPI_CS_FF_NXT     <= SPI_CS_FF; -- keep CS alive
					SPI_BUSY_FLAG_NXT <= '0';
					SPI_RX_REG_NXT    <= SPI_RX_SFT;
					SPI_MOSI_NXT      <= '0';
					SPI_IRQ           <= '1'; -- IRQ tick
					SPI_ARB_STATE_NXT <= IDLE;

			end case;
		end process SPI_ARB_COMB;


		-- SPI IO Interface --
		SPI_IO: process(SPI_CS_FF, SPI_MOSI_FF, SPI_SCK_FF, SPI_MISO_I)
			variable spi_miso_bus_v : std_logic_vector(7 downto 0);
			variable spi_miso_v     : std_logic;
		begin
			spi_miso_bus_v := SPI_MISO_I and (not SPI_CS_FF);
			spi_miso_v := '0';
			for i in 0 to 7 loop -- for all channels
				SPI_MOSI_O(i) <= SPI_MOSI_FF and (not SPI_CS_FF(i));
				SPI_CS_O(i)   <= SPI_CS_FF(i);
				SPI_SCK_O(i)  <= SPI_SCK_FF;
				spi_miso_v    := spi_miso_v or spi_miso_bus_v(i);
			end loop;
			SPI_MISO <= spi_miso_v;
		end process SPI_IO;



end COM_0_CORE_BEHAV;
