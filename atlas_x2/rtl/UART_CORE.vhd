-- #########################################################
-- #   << ATLAS Project - Multi Purpose IO Controller >>   #
-- #   << Universal Asynchronous Receiver Transceiver >>   #
-- # ***************************************************** #
-- #  Configurable UART Core.                              #
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

entity UART_CORE is
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
				RX_IRQ_O        : out std_logic; -- received byte interrupt
				TX_IRQ_O        : out std_logic; -- transmitter ready interrupt

-- ###############################################################################################
-- ##           Peripheral Interface                                                            ##
-- ###############################################################################################

				-- UART Port --
				TXD_O           : out std_logic; -- serial data output
				RXD_I           : in  std_logic  -- serial data input
			);
end UART_CORE;

architecture UART_CORE_STRUCTURE of UART_CORE is

	-- Module Addresses --
	constant rtx_data_c             : std_logic_vector(2 downto 0) := "000";
	constant status_c               : std_logic_vector(2 downto 0) := "001";
	constant baud_c                 : std_logic_vector(2 downto 0) := "010";

	-- Configuration Register --
	constant cr_rx_int_en_c         : natural :=  0; -- R/W: enable received-byte interrupt
	constant cr_tx_int_en_c         : natural :=  1; -- R/W: enable trasmitted-byte interrupt
	constant cr_tx_bsy_c            : natural :=  2; -- R:   transmitter is busy when one
	constant cr_rx_rdy_c            : natural :=  3; -- R:   received char when one

	-- Registers --
	signal RX_REG                   : std_logic_vector(07 downto 0);
	signal BAUD_REG                 : std_logic_vector(data_width_c-1 downto 0);
	signal RX_INT_EN, TX_INT_EN     : std_logic;

	-- Transceiver --
	signal RX_SYNC                  : std_logic_vector(03 downto 0);
	signal TX_BSY_FLAG, RX_BSY_FLAG : std_logic;
	signal TX_SREG, RX_SREG         : std_logic_vector(09 downto 0);
	signal TX_BIT_CNT, RX_BIT_CNT   : std_logic_vector(03 downto 0);
	signal TX_BAUD_CNT, RX_BAUD_CNT : std_logic_vector(data_width_c-1 downto 0);
	signal RX_READY, RX_READY_SYNC  : std_logic;


begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					BAUD_REG  <= (others => '0');
					RX_INT_EN <= '0';
					TX_INT_EN <= '0';
				elsif (ICE_I = '1') then -- interface enable -- Register update
					if (W_EN_I = '1') then
						case (ADR_I) is
							when status_c => RX_INT_EN <= DAT_I(cr_rx_int_en_c);
							                 TX_INT_EN <= DAT_I(cr_tx_int_en_c);
							when baud_c   => BAUD_REG  <= DAT_I;
							when others   => NULL;
						end case;
					end if;
				end if;
			end if;
		end process W_ACC;



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, RX_REG, BAUD_REG, RX_INT_EN, TX_INT_EN, TX_BSY_FLAG, RX_READY)
		begin
			case (ADR_I) is
				when rtx_data_c => DAT_O                 <= x"00" & RX_REG;
				when status_c   => DAT_O                 <= x"0000";
				                   DAT_O(cr_rx_int_en_c) <= RX_INT_EN;
				                   DAT_O(cr_tx_int_en_c) <= TX_INT_EN;
				                   DAT_O(cr_tx_bsy_c)    <= TX_BSY_FLAG;
				                   DAT_O(cr_rx_rdy_c)    <= RX_READY;
				when baud_c     => DAT_O                 <= BAUD_REG;
				when others     => DAT_O                 <= x"0000";
			end case;
		end process R_ACC;



	-- Flag Arbiter ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		FLAG_CTRL: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					RX_READY      <= '0';
					RX_READY_SYNC <= '0';
				else
					-- Ready Flag --
					RX_READY_SYNC <= RX_BSY_FLAG;
					if (R_EN_I = '1') and (ADR_I = rtx_data_c) and (ICE_I = '1') then
						RX_READY <= '0';
					elsif (RX_READY_SYNC = '1') and (RX_BSY_FLAG = '0') then -- falling edge
						RX_READY <= '1';
					end if;
				end if;
			end if;
		end process FLAG_CTRL;

		-- Interrupt Generator --
		RX_IRQ_O <= RX_INT_EN and RX_READY;
		TX_IRQ_O <= TX_INT_EN and (not TX_BSY_FLAG);



	-- Transmitter Unit ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		UART_TRANSMITTER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					TX_BSY_FLAG <= '0';
					TX_SREG     <= (others => '1');
					TX_BIT_CNT  <= (others => '0');
					TX_BAUD_CNT <= (others => '0');
				else
					if (TX_BSY_FLAG = '0') then
						TX_BIT_CNT  <= "1010"; -- 10 bits
						TX_BAUD_CNT <= BAUD_REG;
						if (W_EN_I = '1') and (ADR_I = rtx_data_c) then
							TX_BSY_FLAG <= '1';
							TX_SREG     <= '1' & DAT_I(7 downto 0) & '0'; -- stopbit & data & startbit
						end if;
					else
						if (TX_BAUD_CNT = x"0000") then
							TX_BAUD_CNT <= BAUD_REG;
							if (TX_BIT_CNT /= "0000") then
								TX_SREG    <= '1' & TX_SREG(9 downto 1);
								TX_BIT_CNT <= std_logic_vector(unsigned(TX_BIT_CNT) - 1);
							else
								TX_BSY_FLAG <= '0'; -- done
							end if;
						else
							TX_BAUD_CNT <= std_logic_vector(unsigned(TX_BAUD_CNT) - 1);
						end if;
					end if;
				end if;
			end if;
		end process UART_TRANSMITTER;

		-- Transmitter output --
		TXD_O <= TX_SREG(0);



	-- Receiver Unit ---------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		UART_RECEIVER: process(CLK_I, BAUD_REG)
			variable half_baud_v : std_logic_vector(15 downto 0);
		begin
			half_baud_v :='0' &  BAUD_REG(15 downto 1);
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					RX_BSY_FLAG <= '0';
					RX_SREG     <= (others => '0');
					RX_BIT_CNT  <= (others => '0');
					RX_BAUD_CNT <= (others => '0');
					RX_SYNC     <= (others => '1');
					RX_REG      <= (others => '0');
				else
					-- Synchronizer --
					RX_SYNC <= RXD_I & RX_SYNC(3 downto 1);

					-- RX shift reg --
					if (RX_BSY_FLAG = '0') then
						RX_BIT_CNT  <= "1001"; -- 9 bits
						RX_BAUD_CNT <= half_baud_v;
						if (RX_SYNC(1 downto 0) = "01") then -- start 'bit' detected
							RX_BSY_FLAG <= '1';
						end if;
					else
						if (RX_BAUD_CNT = x"0000") then
							RX_BAUD_CNT <= BAUD_REG;
							if (RX_BIT_CNT /= "0000") then
								RX_SREG    <= RX_SYNC(0) & RX_SREG(9 downto 1);
								RX_BIT_CNT <= std_logic_vector(unsigned(RX_BIT_CNT) - 1);
							else
								RX_BSY_FLAG <= '0'; -- done
								RX_REG      <= RX_SREG(9 downto 2);
							end if;
						else
							RX_BAUD_CNT <= std_logic_vector(unsigned(RX_BAUD_CNT) - 1);
						end if;
					end if;
				end if;
			end if;
		end process UART_RECEIVER;



end UART_CORE_STRUCTURE;
