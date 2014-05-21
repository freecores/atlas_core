-- ########################################################
-- #        << ATLAS Project - Data Write-Back >>         #
-- # **************************************************** #
-- #  Data write back selector for register file input.   #
-- # **************************************************** #
-- #  Last modified: 08.05.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity WB_UNIT is
	port	(
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################

				CLK_I           : in  std_logic; -- global clock line
				CE_I            : in  std_logic; -- clock enable
				RST_I           : in  std_logic; -- global reset line, sync, high-active

-- ###############################################################################################
-- ##           Function Control                                                                ##
-- ###############################################################################################

				WB_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- wb stage control

-- ###############################################################################################
-- ##           Data Input                                                                      ##
-- ###############################################################################################

				MEM_WB_DAT_I    : in  std_logic_vector(data_width_c-1 downto 0); -- memory read data
				ALU_WB_DAT_I    : in  std_logic_vector(data_width_c-1 downto 0); -- alu read data
				MEM_ADR_FB_I    : in  std_logic_vector(data_width_c-1 downto 0); -- memory address feedback

-- ###############################################################################################
-- ##           Data Output                                                                     ##
-- ###############################################################################################

				WB_DATA_O       : out std_logic_vector(data_width_c-1 downto 0); -- write back data
				WB_FWD_O        : out std_logic_vector(fwd_width_c-1  downto 0)  -- WB stage forwarding path
			);
end WB_UNIT;

architecture WB_STRUCTURE of WB_UNIT is

	-- Pipeline register --
	signal ALU_FF         : std_logic_vector(data_width_c-1 downto 0);

	-- Write-Back Source Select --
	signal WB_DATA_INT    : std_logic_vector(data_width_c-1 downto 0);

	-- Aligned Mem Data --
	signal MEM_ADR_FB     : std_logic_vector(data_width_c-1 downto 0);
	signal MEM_WB_DAT_INT : std_logic_vector(data_width_c-1 downto 0);

begin

	-- Pipeline Register -----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		PIPE_REG: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					ALU_FF     <= (others => '0');
					MEM_ADR_FB <= (others => '0');
				elsif (CE_I = '1') then
					ALU_FF     <= ALU_WB_DAT_I;
					MEM_ADR_FB <= MEM_ADR_FB_I;
				end if;
			end if;
		end process PIPE_REG;



	-- Data Alignment --------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		DAT_ALIGN: process(MEM_ADR_FB, MEM_WB_DAT_I)
			variable dat_end_v : std_logic_vector(data_width_c-1 downto 0);
		begin
			-- Endianness converter --
			if (big_endian_c = false) then
				dat_end_v := MEM_WB_DAT_I(data_width_c/2-1 downto 0) & MEM_WB_DAT_I(data_width_c-1 downto data_width_c/2);
			else
				dat_end_v := MEM_WB_DAT_I;
			end if;

			-- Unaligned access? --
			if (word_mode_en_c = false) then -- byte-addressed memory
				if (MEM_ADR_FB(0) = '1') then -- swap bytes
					MEM_WB_DAT_INT <= dat_end_v(data_width_c/2-1 downto 0) & dat_end_v(data_width_c-1 downto data_width_c/2);
				else
					MEM_WB_DAT_INT <= dat_end_v;
				end if;
			else -- word-addressed memory
				MEM_WB_DAT_INT <= dat_end_v;
			end if;
		end process DAT_ALIGN;



	-- Module Data Output ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		-- route mem data if valid mem-read-access
		WB_DATA_INT <= MEM_WB_DAT_INT when (WB_CTRL_BUS_I(ctrl_rd_mem_acc_c) = '1') else ALU_FF;
		WB_DATA_O   <= WB_DATA_INT;



	-- Forwarding Path Output ------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------

		-- forwarding data --
		WB_FWD_O(fwd_dat_msb_c downto fwd_dat_lsb_c) <= WB_DATA_INT;

		-- destination address --
		WB_FWD_O(fwd_adr_3_c downto fwd_adr_0_c) <= WB_CTRL_BUS_I(ctrl_rd_3_c downto ctrl_rd_0_c);

		-- valid forwarding --
		WB_FWD_O(fwd_en_c) <= WB_CTRL_BUS_I(ctrl_wb_en_c);




end WB_STRUCTURE;
