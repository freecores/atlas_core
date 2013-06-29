-- #########################################################
-- #   << ATLAS Project - Multi Purpose IO Controller >>   #
-- #      << High Precision General Purpose Timer >>       #
-- # ***************************************************** #
-- #  General purpose 16-bit timer with 16-bit prescaler   #
-- #  and 16-bit threshold value.                          # 
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

entity TIMER_CORE is
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
				IRQ_O           : out std_logic  -- interrupt request
			);
end TIMER_CORE;

architecture TIMER_CORE_STRUCTURE of TIMER_CORE is

	-- Module Addresses --
	constant count_reg_c       : std_logic_vector(2 downto 0) := "000";
	constant thresh_reg_c      : std_logic_vector(2 downto 0) := "001";
	constant prsc_reg_c        : std_logic_vector(2 downto 0) := "010";

	-- Internal Registers --
	signal COUNT_REG           : std_logic_vector(data_width_c-1 downto 0);
	signal THRESH_REG          : std_logic_vector(data_width_c-1 downto 0);
	signal PRSC_REG            : std_logic_vector(data_width_c-1 downto 0);
	signal PRSC_CNT            : std_logic_vector(data_width_c-1 downto 0);

begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I, PRSC_REG, PRSC_CNT, THRESH_REG)
			variable prsc_match_c : std_logic;
			variable thres_zero_c : std_logic;
		begin
			-- Prescaler match --
			prsc_match_c := '0';
			if (PRSC_REG = PRSC_CNT) then
				prsc_match_c := '1';
			end if;

			-- Threshold zero test --
			thres_zero_c := '0';
			if (THRESH_REG = x"0000") then
				thres_zero_c := '1';
			end if;

			-- Sync update --
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					COUNT_REG  <= (others => '0');
					THRESH_REG <= (others => '0');
					PRSC_REG   <= (others => '0');
					PRSC_CNT   <= (others => '0');
				elsif (W_EN_I = '1') and (ICE_I = '1') then -- Reg update
					PRSC_CNT <= (others => '0');
					case (ADR_I) is
						when count_reg_c  => COUNT_REG  <= DAT_I;
						when thresh_reg_c => THRESH_REG <= DAT_I;
						when prsc_reg_c   => PRSC_REG   <= DAT_I;
						when others       => NULL;
					end case;
				else
					-- Prescaler increment --
					if (prsc_match_c = '1') or (thres_zero_c = '1') then
						PRSC_CNT <= (others => '0');
					else
						PRSC_CNT <= std_logic_vector(unsigned(PRSC_CNT) + 1);
					end if;

					-- Counter increment --
					if (COUNT_REG = THRESH_REG) then
						COUNT_REG <= (others => '0');
					elsif (thres_zero_c = '0') and (prsc_match_c = '1') then
						COUNT_REG <= std_logic_vector(unsigned(COUNT_REG) + 1);
					end if;
				end if;
			end if;
		end process W_ACC;



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, COUNT_REG, THRESH_REG, PRSC_REG)
		begin
			case (ADR_I) is
				when count_reg_c  => DAT_O <= COUNT_REG;
				when thresh_reg_c => DAT_O <= THRESH_REG;
				when prsc_reg_c   => DAT_O <= PRSC_REG;
				when others       => DAT_O <= x"0000";
			end case;
		end process R_ACC;



	-- Interrupt Generator ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		INT_TOGGLE: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					IRQ_O <= '0';
				elsif (COUNT_REG = THRESH_REG) and (THRESH_REG /= x"0000") then
					IRQ_O <= '1';
				else
					IRQ_O <= '0';
				end if;
			end if;
		end process INT_TOGGLE;



end TIMER_CORE_STRUCTURE;
