-- ########################################################
-- #        << ATLAS Project - Data Register File >>      #
-- # **************************************************** #
-- #  Main data register file, organized in two bank,     #
-- #  separated for each operating mode. Each bank holds  #
-- #  8 16-bit data registers.                            #
-- # **************************************************** #
-- #  Last modified: 09.03.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity REG_FILE is
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
				OF_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- of stage control

-- ###############################################################################################
-- ##           Data Input                                                                      ##
-- ###############################################################################################

				WB_DATA_I       : in  std_logic_vector(data_width_c-1 downto 0); -- write back data
				IMMEDIATE_I     : in  std_logic_vector(data_width_c-1 downto 0); -- immediates
				PC_1D_I         : in  std_logic_vector(data_width_c-1 downto 0); -- pc 1x delayed
				WB_FWD_I        : in  std_logic_vector(fwd_width_c-1  downto 0); -- WB stage forwarding path

-- ###############################################################################################
-- ##           Data Output                                                                     ##
-- ###############################################################################################

				OP_A_DATA_O     : out std_logic_vector(data_width_c-1 downto 0); -- operand A output
				OP_B_DATA_O     : out std_logic_vector(data_width_c-1 downto 0); -- operand B output
				OP_C_DATA_O     : out std_logic_vector(data_width_c-1 downto 0)  -- operand C output
			);
end REG_FILE;

architecture RF_STRUCTURE of REG_FILE is

	-- Register File --
	type   reg_file_mem_type is array (2*8-1 downto 0) of std_logic_vector(data_width_c-1 downto 0);
	signal REG_FILE_MEM      : reg_file_mem_type := (others => (others => '0'));

	-- Operand Multiplexer --
	signal OP_A_INT          : std_logic_vector(data_width_c-1 downto 0);
	signal OP_B_INT          : std_logic_vector(data_width_c-1 downto 0);

begin

	-- Data Register File ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		DATA_REGISTER_FILE: process(CLK_I)
		begin
			-- sync write access --
			if rising_edge(CLK_I) then
				if (WB_CTRL_BUS_I(ctrl_wb_en_c) = '1') and (CE_I = '1') then -- valid write back
					REG_FILE_MEM(to_integer(unsigned(WB_CTRL_BUS_I(ctrl_rd_3_c downto ctrl_rd_0_c)))) <= WB_DATA_I;
				end if;
			end if;
		end process DATA_REGISTER_FILE;



	-- Operand Fetch Forwarding Unit -----------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		OF_FWD: process(WB_FWD_I, OF_CTRL_BUS_I, REG_FILE_MEM)
		begin
			-- operand A forwarding --
			if (WB_FWD_I(fwd_en_c) = '1') and (OF_CTRL_BUS_I(ctrl_ra_3_c downto ctrl_ra_0_c) = WB_FWD_I(fwd_adr_3_c downto fwd_adr_0_c)) then
				OP_A_INT <= WB_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c);
			else
				OP_A_INT <= REG_FILE_MEM(to_integer(unsigned(OF_CTRL_BUS_I(ctrl_ra_3_c downto ctrl_ra_0_c))));
			end if;

			-- operand B forwarding --
			if (WB_FWD_I(fwd_en_c) = '1') and (OF_CTRL_BUS_I(ctrl_rb_3_c downto ctrl_rb_0_c) = WB_FWD_I(fwd_adr_3_c downto fwd_adr_0_c)) then
				OP_B_INT <= WB_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c);
			else
				OP_B_INT <= REG_FILE_MEM(to_integer(unsigned(OF_CTRL_BUS_I(ctrl_rb_3_c downto ctrl_rb_0_c))));
			end if;
		end process OF_FWD;



	-- Operand Multiplexer ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		OP_A_DATA_O <= PC_1D_I     when (OF_CTRL_BUS_I(ctrl_ra_is_pc_c)  = '1') else OP_A_INT;
		OP_B_DATA_O <= IMMEDIATE_I when (OF_CTRL_BUS_I(ctrl_rb_is_imm_c) = '1') else OP_B_INT;
		OP_C_DATA_O <= OP_B_INT;



end RF_STRUCTURE;
