-- #########################################################
-- #   << ATLAS Project - Multi Purpose IO Controller >>   #
-- #         << Linear Feedback Shift Register >>          #
-- # ***************************************************** #
-- #  16-bit random generator with configurable            #
-- #  polynomial mask.                                     #
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

entity LFSR_CORE is
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

				-- External Noise Source --
				RND_I           : in  std_logic
			);
end LFSR_CORE;

architecture LFSR_CORE_STRUCTURE of LFSR_CORE is

	-- Module Addresses --
	constant lfsr_reg_c        : std_logic_vector(2 downto 0) := "000";
	constant poly_reg_c        : std_logic_vector(2 downto 0) := "001";
	constant ctrl_reg_c        : std_logic_vector(2 downto 0) := "010";

	-- Control reg --
	constant ctrl_en_c         : natural := 0; -- enable LFSR when '1'
	constant ctrl_up_c         : natural := 1; -- '1' new value after read access, '0' free running mode

	-- Registers --
	signal LFSR, POLY          : std_logic_vector(data_width_c-1 downto 0);
	signal CTRL                : std_logic_vector(01 downto 0);
	signal LFSR_DATA           : std_logic;
	signal NOISE_SYNC          : std_logic_vector(01 downto 0);
begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					LFSR       <= (others => '0');
					POLY       <= (others => '0');
					CTRL       <= (others => '0');
					NOISE_SYNC <= (others => '0');
				else
					if (W_EN_I = '1') and (ICE_I = '1') then -- Write update
						case (ADR_I) is
							when lfsr_reg_c => LFSR <= DAT_I;
							when poly_reg_c => POLY <= DAT_I;
							when ctrl_reg_c => CTRL <= DAT_I(1 downto 0);
							when others     => NULL;
						end case;
					elsif (CTRL(ctrl_en_c) = '1') and (LFSR /= x"0000") then -- LFSR update enabled
						if (CTRL(ctrl_up_c) = '1') then
							if (R_EN_I = '1') and (ADR_I = lfsr_reg_c) and (ICE_I = '1') then
								LFSR <= LFSR(14 downto 0) & LFSR_DATA;
							end if;
						else
							LFSR <= LFSR(14 downto 0) & LFSR_DATA;
						end if;
					end if;

					-- External Noise Sync
					NOISE_SYNC <= NOISE_SYNC(0) & RND_I;
				end if;
			end if;
		end process W_ACC;



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, LFSR, POLY, CTRL)
		begin
			case (ADR_I) is
				when lfsr_reg_c => DAT_O <= LFSR;
				when poly_reg_c => DAT_O <= POLY;
				when ctrl_reg_c => DAT_O <= x"000" & "00" & CTRL;
				when others     => DAT_O <= x"0000";
			end case;
		end process R_ACC;



	-- LFSR Update -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		LFSR_UPDATE: process(LFSR, POLY, NOISE_SYNC)
			variable lfsr_tmp_v  : std_logic_vector(15 downto 0);
			variable lfsr_d_in_v : std_logic;
		begin
			lfsr_tmp_v  := LFSR and POLY;
			lfsr_d_in_v := NOISE_SYNC(1);
			for i in 0 to 15 loop
				lfsr_d_in_v := lfsr_d_in_v xor lfsr_tmp_v(i);
			end loop;
			LFSR_DATA <= lfsr_d_in_v;
		end process LFSR_UPDATE;


end LFSR_CORE_STRUCTURE;
