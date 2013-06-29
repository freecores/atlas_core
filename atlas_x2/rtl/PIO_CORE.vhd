-- #########################################################
-- #   << ATLAS Project - Multi Purpose IO Controller >>   #
-- #              << Parallel IO Controller >>             #
-- # ***************************************************** #
-- #  16-bit in/out parallel IO controller.                #
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

entity PIO_CORE is
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

				-- Parallel IO --
				PIO_OUT_O       : out std_logic_vector(data_width_c-1 downto 0); -- parallel output
				PIO_IN_I        : in  std_logic_vector(data_width_c-1 downto 0)  -- parallel input
			);
end PIO_CORE;

architecture PIO_CORE_STRUCTURE of PIO_CORE is

	-- Module Addresses --
	constant out_reg_c        : std_logic_vector(2 downto 0) := "000";
	constant in_reg_c         : std_logic_vector(2 downto 0) := "001";
	constant int_en_c         : std_logic_vector(2 downto 0) := "010";
	constant int_conf0_c      : std_logic_vector(2 downto 0) := "011";
	constant int_conf1_c      : std_logic_vector(2 downto 0) := "100";
	constant int_source_c     : std_logic_vector(2 downto 0) := "101";

	-- Registers --
	signal OUT_DATA           : std_logic_vector(data_width_c-1 downto 0);
	signal IN_SYNC            : std_logic_vector(data_width_c-1 downto 0);
	signal IRQ_SYNC           : std_logic_vector(data_width_c-1 downto 0);
	signal INT_EN_REG         : std_logic_vector(data_width_c-1 downto 0);
	signal INT_SOURCE_REG     : std_logic_vector(data_width_c-1 downto 0);
	signal INT_CONF0_REG      : std_logic_vector(data_width_c-1 downto 0);
	signal INT_CONF1_REG      : std_logic_vector(data_width_c-1 downto 0);
	signal RAW_INT_REQ        : std_logic_vector(data_width_c-1 downto 0);

begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					OUT_DATA       <= (others => '0');
					IN_SYNC        <= (others => '0');
					IRQ_SYNC       <= (others => '0');
					INT_EN_REG     <= (others => '0');
					INT_CONF0_REG  <= (others => '0');
					INT_CONF1_REG  <= (others => '0');
				else
					if (ICE_I = '1') then -- interface enable
						-- Register update --
						if (W_EN_I = '1') then
							case (ADR_I) is
								when out_reg_c   => OUT_DATA      <= DAT_I;
								when int_en_c    => INT_EN_REG    <= DAT_I;
								when int_conf0_c => INT_CONF0_REG <= DAT_I;
								when int_conf1_c => INT_CONF1_REG <= DAT_I;
								when others => NULL;
							end case;
						end if;
					end if;
					-- Sync Input --
					IN_SYNC  <= PIO_IN_I;
					IRQ_SYNC <= IN_SYNC;
				end if;
			end if;
		end process W_ACC;

		-- Output --
		PIO_OUT_O <= OUT_DATA;



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, OUT_DATA, IN_SYNC, INT_EN_REG, INT_CONF0_REG, INT_CONF1_REG, INT_SOURCE_REG)
		begin
			case (ADR_I) is
				when out_reg_c    => DAT_O <= OUT_DATA;
				when in_reg_c     => DAT_O <= IN_SYNC;
				when int_en_c     => DAT_O <= INT_EN_REG;
				when int_conf0_c  => DAT_O <= INT_CONF0_REG;
				when int_conf1_c  => DAT_O <= INT_CONF1_REG;
				when int_source_c => DAT_O <= INT_SOURCE_REG;
				when others       => DAT_O <= x"0000";
			end case;
		end process R_ACC;



	-- Interrupt Detector ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		DETECTOR: process(INT_EN_REG, INT_CONF0_REG, INT_CONF1_REG, IN_SYNC, IRQ_SYNC)
		begin
			-- Edge/Level detector --
			RAW_INT_REQ <= (others => '0');
			for i in 0 to 15 loop
				if (INT_EN_REG(i) = '1') then -- channel enabled
					if (INT_CONF0_REG(i) = '1') then -- level triggered
						RAW_INT_REQ(i) <= INT_CONF1_REG(i) xnor IN_SYNC(i);
					else -- edge triggered
						if (INT_CONF1_REG(i) = '1') then -- rising edge
							RAW_INT_REQ(i) <= IN_SYNC(i) and (not IRQ_SYNC(i));
						else -- falling edge
							RAW_INT_REQ(i) <= (not IN_SYNC(i)) and IRQ_SYNC(i);
						end if;
					end if;
				end if;
			end loop;
		end process DETECTOR;



	-- Interrupt Generator ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		IRQ_GEN: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					IRQ_O          <= '0';
					INT_SOURCE_REG <= (others => '0');
				else
					-- INT Source --
					if (R_EN_I = '1') and (ADR_I = int_source_c) then
						INT_SOURCE_REG <= (others => '0');
					else
						INT_SOURCE_REG <= INT_SOURCE_REG or RAW_INT_REQ;
					end if;

					-- IRQ --
					if (INT_SOURCE_REG /= x"0000") then
						IRQ_O <= '1';
					else
						IRQ_O <= '0';
					end if;
				end if;
			end if;
		end process IRQ_GEN;



end PIO_CORE_STRUCTURE;
