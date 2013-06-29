-- #########################################################
-- #   << ATLAS Project - Multi Purpose IO Controller >>   #
-- #              << Interrupt Controller >>               #
-- # ***************************************************** #
-- #  8 Channel Interrupt Controller                       #
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

entity INT_CORE is
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

				-- Interrupt lines --
				INT_REQ_I       : in  std_logic_vector(07 downto 0)  -- request lines
			);
end INT_CORE;

architecture INT_CORE_STRUCTURE of INT_CORE is

	-- Module Addresses --
	constant source_reg_c      : std_logic_vector(2 downto 0) := "000"; -- R: ID of last activated interrupt
	constant mask_reg_c        : std_logic_vector(2 downto 0) := "001"; -- R/W: enable IRQ-lines
	constant type_0_reg_c      : std_logic_vector(2 downto 0) := "010"; -- R/W: '1': level triggered, '0': edge triggered
	constant type_1_reg_c      : std_logic_vector(2 downto 0) := "011"; -- R/W: '1': high level/rising edge, '0': low level/falling edge

	-- Registers --
	signal MASK_REG            : std_logic_vector(07 downto 0);
	signal SOURCE_REG          : std_logic_vector(02 downto 0);
	signal TYPE_0_REG          : std_logic_vector(07 downto 0);
	signal TYPE_1_REG          : std_logic_vector(07 downto 0);

	-- Internals --
	signal INT_SYNC_0          : std_logic_vector(07 downto 0);
	signal INT_SYNC_1          : std_logic_vector(07 downto 0);
	signal RAW_INT_REQ         : std_logic_vector(07 downto 0);
	signal IRQ_BUF             : std_logic_vector(07 downto 0);
	signal IRQ_ID              : std_logic_vector(02 downto 0);
	signal IRQ_ACK_MASK        : std_logic_vector(07 downto 0);
	signal IRQ_ACK_MASK_FF     : std_logic_vector(07 downto 0);
	signal IRQ_LOCK            : std_logic;

begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					MASK_REG   <= (others => '0');
					TYPE_0_REG <= (others => '0');
					TYPE_1_REG <= (others => '0');
					INT_SYNC_0 <= (others => '0');
					INT_SYNC_1 <= (others => '0');
				elsif (ICE_I = '1') then -- interface enable
					-- Write update --
					if (W_EN_I = '1') then
						case (ADR_I) is
							when mask_reg_c   => MASK_REG   <= DAT_I(7 downto 0);
							when type_0_reg_c => TYPE_0_REG <= DAT_I(7 downto 0);
							when type_1_reg_c => TYPE_1_REG <= DAT_I(7 downto 0);
							when others       => NULL;
						end case;
					end if;

					-- Synchronize inputs --
					INT_SYNC_1 <= INT_SYNC_0;
					INT_SYNC_0 <= INT_REQ_I;
				end if;
			end if;
		end process W_ACC;



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, MASK_REG, SOURCE_REG, TYPE_0_REG, TYPE_1_REG)
		begin
			case (ADR_I) is
				when mask_reg_c   => DAT_O <= "00000000" & MASK_REG;
				when source_reg_c => DAT_O <= "0000000000000" & SOURCE_REG;
				when type_0_reg_c => DAT_O <= "00000000" & TYPE_0_REG;
				when type_1_reg_c => DAT_O <= "00000000" & TYPE_1_REG;
				when others       => DAT_O <= x"0000";
			end case;
		end process R_ACC;



	-- Interrupt Detector ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		DETECTOR: process(MASK_REG, TYPE_0_REG, TYPE_1_REG, INT_SYNC_0, INT_SYNC_1)
		begin
			-- Edge/Level detector --
			RAW_INT_REQ <= (others => '0');
			for i in 0 to 7 loop
				if (MASK_REG(i) = '1') then -- channel enabled
					if (TYPE_0_REG(i) = '1') then -- level triggered
						RAW_INT_REQ(i) <= TYPE_1_REG(i) xnor INT_SYNC_0(i);
					else -- edge triggered
						if (TYPE_1_REG(i) = '1') then -- rising edge
							RAW_INT_REQ(i) <= INT_SYNC_0(i) and (not INT_SYNC_1(i));
						else -- falling edge
							RAW_INT_REQ(i) <= (not INT_SYNC_0(i)) and INT_SYNC_1(i);
						end if;
					end if;
				end if;
			end loop;
		end process DETECTOR;



	-- Interrupt Request Buffer ----------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		IRQ_BUFFER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					IRQ_BUF         <= (others => '0');
					SOURCE_REG      <= (others => '0');
					IRQ_ACK_MASK_FF <= (others => '0');
					IRQ_LOCK        <= '0';
				else
					if (IRQ_LOCK = '0') then -- store ID and mask until ACK
						IRQ_ACK_MASK_FF <= IRQ_ACK_MASK;
						SOURCE_REG      <= IRQ_ID;
					end if;
					if (R_EN_I = '1') and (ADR_I = source_reg_c) then -- ack on source reg read
						IRQ_BUF  <= (IRQ_BUF or RAW_INT_REQ) and (not IRQ_ACK_MASK_FF);
						IRQ_LOCK <= '0';
					else
						IRQ_BUF  <= IRQ_BUF or RAW_INT_REQ;
						if (to_integer(unsigned(IRQ_BUF)) /= 0) then
							IRQ_LOCK <= '1';
						end if;
					end if;
				end if;
			end if;
		end process IRQ_BUFFER;

		-- IRQ signal to host --
		IRQ_O <= IRQ_LOCK;



	-- Priority Encoder ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		PR_ENC: process(IRQ_BUF)
		begin
			IRQ_ID       <= (others => '0');
			IRQ_ACK_MASK <= (others => '0');
			for i in 0 to 7 loop
				if (IRQ_BUF(i) = '1') then
					IRQ_ID          <= std_logic_vector(to_unsigned(i,3));
					IRQ_ACK_MASK(i) <= '1';
					exit;
				end if;
			end loop;
		end process PR_ENC;



end INT_CORE_STRUCTURE;
