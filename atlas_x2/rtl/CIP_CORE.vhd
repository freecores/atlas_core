-- #########################################################
-- #   << ATLAS Project - Multi Purpose IO Controller >>   #
-- #                << Custom IP Module >>                 #
-- # ***************************************************** #
-- #  This design dummy can be used to implement a user-   #
-- #  specified IP module.                                 #
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

entity CIP_CORE is
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
end CIP_CORE;

architecture CIP_CORE_STRUCTURE of CIP_CORE is

	-- Registers --
	type  dummy_rf_t is array (0 to 7) of std_logic_vector(data_width_c-1 downto 0);
	signal DUMMY_RF : dummy_rf_t;

begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					DUMMY_RF <= (others => (others => '0'));
				elsif (ICE_I = '1') then -- interface enable
					-- Register update --
					if (W_EN_I = '1') then
						case (ADR_I) is
							when "000"  => DUMMY_RF(0) <= DAT_I;
							when "001"  => DUMMY_RF(1) <= DAT_I;
							when "010"  => DUMMY_RF(2) <= DAT_I;
							when "011"  => DUMMY_RF(3) <= DAT_I;
							when "100"  => DUMMY_RF(4) <= DAT_I;
							when "101"  => DUMMY_RF(5) <= DAT_I;
							when "110"  => DUMMY_RF(6) <= DAT_I;
							when "111"  => DUMMY_RF(7) <= DAT_I;
							when others => NULL;
						end case;
					end if;
				end if;
			end if;
		end process W_ACC;



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, DUMMY_RF)
		begin
			case (ADR_I) is
				when "000"  => DAT_O <= DUMMY_RF(0);
				when "001"  => DAT_O <= DUMMY_RF(1);
				when "010"  => DAT_O <= DUMMY_RF(2);
				when "011"  => DAT_O <= DUMMY_RF(3);
				when "100"  => DAT_O <= DUMMY_RF(4);
				when "101"  => DAT_O <= DUMMY_RF(5);
				when "110"  => DAT_O <= DUMMY_RF(6);
				when "111"  => DAT_O <= DUMMY_RF(7);
				when others => DAT_O <= x"0000";
			end case;
		end process R_ACC;



	-- Module Interrupt Request ----------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		IRQ_O <= '0';



end CIP_CORE_STRUCTURE;
