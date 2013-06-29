-- #########################################################
-- #   << ATLAS Project - Multi Purpose IO Controller >>   #
-- #     << Serial Peripheral Interface Controller >>      #
-- # ***************************************************** #
-- #  Configurable SPI Controller.                         #
-- #  SPI_clk = HOST_clk * 1/(2+2*(2^config_prsc))         #
-- # ***************************************************** #
-- #  Last modified: 06.06.2013                            #
-- # ***************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany            #
-- #########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity SPI_CORE is
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
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				IRQ_O           : out std_logic; -- interrupt request

-- ###############################################################################################
-- ##           Peripheral Interface                                                            ##
-- ###############################################################################################

				-- SPI Port --
				SPI_CS_O        : out std_logic_vector(15 downto 0); -- slave select
				SPI_SCK_O       : out std_logic; -- serial master clock
				SPI_MOSI_O      : out std_logic; -- serial data output
				SPI_MISO_I      : in  std_logic  -- serial data input
			);
end SPI_CORE;

architecture SPI_CORE_STRUCTURE of SPI_CORE is

	-- Module Addresses --
	constant rtxdata_c              : std_logic_vector(2 downto 0) := "000";
	constant cs_reg_c               : std_logic_vector(2 downto 0) := "001";
	constant config_c               : std_logic_vector(2 downto 0) := "010";

	-- Configuration Register --
	constant cr_spi_en_c            : natural :=  0; -- enable SPI core
	constant cr_dir_flag_c          : natural :=  1; -- R/W: 0: MSB first, 1: LSB first
	constant cr_cpol_c              : natural :=  2; -- R/W: clock polarity, 1: idle '1' clock, 0: idle '0' clock
	constant cr_cpha_c              : natural :=  3; -- R/W: edge offset: 0: first edge, 1: second edge
	constant cr_css_c               : natural :=  4; -- R/W: CS active state: 0: low active, 1: high active
	constant cr_bsy_c               : natural :=  5; -- R:   transceiver is busy when '1'
	constant cr_inten_c             : natural :=  6; -- R/W: Transfer done interrupt enable
	constant cr_ln_lsb_c            : natural :=  7; -- R/W: data length lsb
	constant cr_ln_msb_c            : natural := 10; -- R/W: data length msb
	constant cr_prsc_lsb_c          : natural := 11; -- R/W: SPI clock prescaler lsb
	constant cr_prsc_msb_c          : natural := 14; -- R/W: SPI clock prescaler msb
--	constant reserved               : natural := 15; -- R/W: reserved

	-- Registers --
	signal TX_REG                   : std_logic_vector(data_width_c-1 downto 0);
	signal RX_REG, RX_REG_NXT       : std_logic_vector(data_width_c-1 downto 0);
	signal CS_REG                   : std_logic_vector(data_width_c-1 downto 0);
	signal CONFIG_REG               : std_logic_vector(data_width_c-1 downto 0);

	-- Transceiver --
	signal IN_BUF                   : std_logic_vector(01 downto 0);
	signal MOSI_NXT                 : std_logic;
	signal SCK_NXT                  : std_logic;
	signal MOSI_FF                  : std_logic;
	signal CS_FF, CS_FF_NXT         : std_logic_vector(data_width_c-1 downto 0);
	signal IRQ                      : std_logic;

	-- Arbiter --
	type   arb_state_type is (IDLE, START_TRANS, TRANSMIT_0, TRANSMIT_1);
	signal ARB_STATE, ARB_STATE_NXT : arb_state_type;
	signal BIT_CNT,   BIT_CNT_NXT   : std_logic_vector(04 downto 0);
	signal RX_SFT,    RX_SFT_NXT    : std_logic_vector(data_width_c-1 downto 0); -- rx shift registers
	signal TX_SFT,    TX_SFT_NXT    : std_logic_vector(data_width_c-1 downto 0); -- tx shift registers
	signal PRSC_CNT,  PRSC_CNT_NXT  : std_logic_vector(data_width_c-1 downto 0);
	signal BUSY_FLAG, BUSY_FLAG_NXT : std_logic;

begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					TX_REG     <= (others => '0');
					CS_REG     <= (others => '0');
					CONFIG_REG <= (others => '0');
				elsif (ICE_I = '1') then -- Register update
					if (W_EN_I = '1') then
						case (ADR_I) is
							when rtxdata_c => TX_REG     <= DAT_I;
							when cs_reg_c  => CS_REG     <= DAT_I;
							when config_c  => CONFIG_REG <= DAT_I;
							when others    => NULL;
						end case;
					end if;
				end if;
			end if;
		end process W_ACC;



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, RX_REG, CS_REG, CONFIG_REG, BUSY_FLAG)
		begin
			case (ADR_I) is
				when rtxdata_c => DAT_O <= RX_REG;
				when cs_reg_c  => DAT_O <= CS_REG;
				when config_c  => DAT_O <= CONFIG_REG;
				                  DAT_O(cr_bsy_c) <= BUSY_FLAG;
				when others    => DAT_O <= x"0000";
			end case;
		end process R_ACC;



	-- Transceiver Unit ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		SPI_ARB_SYNC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					ARB_STATE  <= IDLE;
					RX_SFT     <= (others => '0');
					TX_SFT     <= (others => '0');
					BIT_CNT    <= (others => '0');
					PRSC_CNT   <= (others => '0');
					RX_REG     <= (others => '0');
					SPI_SCK_O  <= '0';
					MOSI_FF    <= '0';
					IN_BUF     <= "00";
					CS_FF      <= (others => '1');
					BUSY_FLAG  <= '0';
					IRQ_O      <= '0';
				else
					ARB_STATE  <= ARB_STATE_NXT;
					RX_SFT     <= RX_SFT_NXT;
					TX_SFT     <= TX_SFT_NXT;
					BIT_CNT    <= BIT_CNT_NXT;
					PRSC_CNT   <= PRSC_CNT_NXT;
					RX_REG     <= RX_REG_NXT;
					SPI_SCK_O  <= SCK_NXT;
					MOSI_FF    <= MOSI_NXT;
					IN_BUF     <= IN_BUF(0) & SPI_MISO_I;
					CS_FF      <= CS_FF_NXT;
					BUSY_FLAG  <= BUSY_FLAG_NXT;
					IRQ_O      <= IRQ;
				end if;
			end if;
		end process SPI_ARB_SYNC;

		-- Output --
		SPI_MOSI_O <= MOSI_FF;
		SPI_CS_O   <= CS_FF;


		SPI_ARB_COMB: process(ARB_STATE, CONFIG_REG, RX_SFT, TX_SFT, BIT_CNT, PRSC_CNT, IN_BUF, RX_REG, MOSI_FF, CS_FF, CS_REG, TX_REG, W_EN_I, ADR_I, BUSY_FLAG, ICE_I)
			variable prsc_match_v : std_logic;
		begin
			-- Defaults --
			ARB_STATE_NXT <= ARB_STATE; -- arbiter state
			RX_SFT_NXT    <= RX_SFT;    -- rx shift register
			TX_SFT_NXT    <= TX_SFT;    -- tx shift register
			BIT_CNT_NXT   <= BIT_CNT;   -- bit counter
			PRSC_CNT_NXT  <= PRSC_CNT; -- SPI clock prescaler
			RX_REG_NXT    <= RX_REG;    -- complete received data
			SCK_NXT       <= CONFIG_REG(cr_cpol_c); -- clock polarity
			MOSI_NXT      <= MOSI_FF;   -- serial data output
			BUSY_FLAG_NXT <= BUSY_FLAG; -- busy flag
			IRQ           <= '0';       -- no interrupt
			prsc_match_v  := PRSC_CNT(to_integer(unsigned(CONFIG_REG(cr_prsc_msb_c downto cr_prsc_lsb_c)))); -- prescaler match
			for i in 0 to 15 loop
				CS_FF_NXT(i) <= not CONFIG_REG(cr_css_c); -- deselct all slaves
			end loop;

			-- State machine --
			case (ARB_STATE) is -- IDLE, START_TRANS, TRANSMIT, END_TRANS);

				when IDLE => -- Wait for transmitter init
					BIT_CNT_NXT   <= (others => '0');
					RX_SFT_NXT    <= (others => '0');
					PRSC_CNT_NXT  <= (others => '0');
					MOSI_NXT      <= '0';
					TX_SFT_NXT    <= TX_REG;
					if (W_EN_I = '1') and (ADR_I = cs_reg_c) and (ICE_I = '1') then
						ARB_STATE_NXT <= START_TRANS;
						BUSY_FLAG_NXT <= '1';
					end if;

				when START_TRANS => -- Apply slave select signal
					if (CONFIG_REG(cr_css_c) = '0') then -- low active CS
						CS_FF_NXT <= not CS_REG;
					else -- high active CS
						CS_FF_NXT <= CS_REG;
					end if;
					ARB_STATE_NXT <= TRANSMIT_0;

				when TRANSMIT_0 => -- first half of bit transmission
					CS_FF_NXT    <= CS_FF; -- keep CS alive
					PRSC_CNT_NXT <= std_logic_vector(unsigned(PRSC_CNT) + 1);
					SCK_NXT      <= CONFIG_REG(cr_cpol_c) xor CONFIG_REG(cr_cpha_c);
					if (CONFIG_REG(cr_dir_flag_c) = '0') then -- MSB first
						MOSI_NXT <= TX_SFT(to_integer(unsigned(CONFIG_REG(cr_ln_msb_c downto cr_ln_lsb_c))));
					else -- LSB first
						MOSI_NXT <= TX_SFT(0);
					end if;
					if (prsc_match_v = '1') then -- first half completed
						ARB_STATE_NXT <= TRANSMIT_1;
						PRSC_CNT_NXT <= (others => '0');
					end if;

				when TRANSMIT_1 => -- second half of bit transmission
					CS_FF_NXT    <= CS_FF; -- keep CS alive
					PRSC_CNT_NXT <= std_logic_vector(unsigned(PRSC_CNT) + 1);
					SCK_NXT      <= not (CONFIG_REG(cr_cpol_c) xor CONFIG_REG(cr_cpha_c));
					if (CONFIG_REG(cr_dir_flag_c) = '0') then -- MSB first
						MOSI_NXT <= TX_SFT(to_integer(unsigned(CONFIG_REG(cr_ln_msb_c downto cr_ln_lsb_c))));
					else -- LSB first
						MOSI_NXT <= TX_SFT(0);
					end if;
					if (prsc_match_v = '1') then -- second half completed
						BIT_CNT_NXT   <= std_logic_vector(unsigned(BIT_CNT) + 1);
						PRSC_CNT_NXT <= (others => '0');
						if (CONFIG_REG(cr_dir_flag_c) = '0') then -- MSB first
							TX_SFT_NXT <= TX_SFT(14 downto 0) & '0'; -- left shift
							RX_SFT_NXT <= RX_SFT(14 downto 0) & IN_BUF(1); -- left shift
						else -- LSB first
							TX_SFT_NXT <= '0' & TX_SFT(15 downto 1); -- right shift
							RX_SFT_NXT <= IN_BUF(1) & TX_SFT(15 downto 1); -- right shift
						end if;
						if (to_integer(unsigned(BIT_CNT)) = (to_integer(unsigned(CONFIG_REG(cr_ln_msb_c downto cr_ln_lsb_c))) + 1)) then
							BUSY_FLAG_NXT <= '0';
							RX_REG_NXT    <= RX_SFT;
							MOSI_NXT      <= '0';
							ARB_STATE_NXT <= IDLE;
							IRQ           <= CONFIG_REG(cr_inten_c);
						else
							ARB_STATE_NXT <= TRANSMIT_0;
						end if;
					end if;

			end case;
		end process SPI_ARB_COMB;



end SPI_CORE_STRUCTURE;
