-- ########################################################
-- #      << ATLAS Project - Memory Access System >>      #
-- # **************************************************** #
-- #  This unit generates all neccessary signals for the  #
-- #  data memory interface. Furthermore, internal data   #
-- #  switching networks are located here.                #
-- # **************************************************** #
-- #  Last modified: 14.03.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity MEM_ACC is
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

				MA_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- ma stage control

-- ###############################################################################################
-- ##           Data Input                                                                      ##
-- ###############################################################################################

				ALU_RES_I       : in  std_logic_vector(data_width_c-1 downto 0); -- alu result
				MAC_RES_I       : in  std_logic_vector(data_width_c-1 downto 0); -- mac result
				ADR_BASE_I      : in  std_logic_vector(data_width_c-1 downto 0); -- op_a bypass
				DATA_BP_I       : in  std_logic_vector(data_width_c-1 downto 0); -- op_b bypass
				CP_DATA_I       : in  std_logic_vector(data_width_c-1 downto 0); -- coprocessor rd data
				RD_MSR_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data msr

				WB_FWD_I        : in  std_logic_vector(fwd_width_c-1  downto 0); -- WB stage forwarding path

-- ###############################################################################################
-- ##           Data Output                                                                     ##
-- ###############################################################################################

				DATA_O          : out std_logic_vector(data_width_c-1 downto 0); -- data output
				MEM_ADR_FB_O    : out std_logic_vector(data_width_c-1 downto 0); -- memory address feedback

				MA_FWD_O        : out std_logic_vector(fwd_width_c-1  downto 0); -- MA stage forwarding path

-- ###############################################################################################
-- ##           Memory (w) Interface                                                            ##
-- ###############################################################################################

				MEM_ADR_O       : out std_logic_vector(data_width_c-1 downto 0); -- address output
				MEM_DAT_O       : out std_logic_vector(data_width_c-1 downto 0); -- write data output
				MEM_RW_O        : out std_logic -- read write
			);
end MEM_ACC;

architecture MA_STRUCTURE of MEM_ACC is

	-- Pipeline register --
	signal ALU_RES_FF       : std_logic_vector(data_width_c-1 downto 0);
	signal MAC_RES_FF       : std_logic_vector(data_width_c-1 downto 0);
	signal ADR_BASE_FF      : std_logic_vector(data_width_c-1 downto 0);
	signal DATA_BP_FF       : std_logic_vector(data_width_c-1 downto 0);

	-- ALU data buffer --
	signal ALU_RES_BUF      : std_logic_vector(data_width_c-1 downto 0);

	-- Internal signals --
	signal DATA_BP_INT      : std_logic_vector(data_width_c-1 downto 0);
	signal ALU_MAC_DAT      : std_logic_vector(data_width_c-1 downto 0);
	signal SYS_CP_R_DAT     : std_logic_vector(data_width_c-1 downto 0);
	signal SYS_CP_ALU_R_DAT : std_logic_vector(data_width_c-1 downto 0);

begin

	-- Pipeline Register -----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		PIPE_REG: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					ALU_RES_FF  <= (others => '0');
					MAC_RES_FF  <= (others => '0');
					ADR_BASE_FF <= (others => '0');
					DATA_BP_FF  <= (others => '0');
					ALU_RES_BUF <= (others => '0');
				elsif (CE_I = '1') then
					ALU_RES_FF  <= ALU_RES_I;
					MAC_RES_FF  <= MAC_RES_I;
					ADR_BASE_FF <= ADR_BASE_I;
					DATA_BP_FF  <= DATA_BP_I;
					ALU_RES_BUF <= ALU_RES_FF;
				end if;
			end if;
		end process PIPE_REG;



	-- Memory Access Forwarding Unit -----------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MA_FWD: process(WB_FWD_I, MA_CTRL_BUS_I, DATA_BP_FF)
		begin
			-- Memory write data (OP_B) forwarding --
			if (WB_FWD_I(fwd_en_c) = '1') and (MA_CTRL_BUS_I(ctrl_mcyc_c) = '0') and (MA_CTRL_BUS_I(ctrl_rb_3_c downto ctrl_rb_0_c) = WB_FWD_I(fwd_adr_3_c downto fwd_adr_0_c)) then
				DATA_BP_INT <= WB_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- WB stage
			else
				DATA_BP_INT <= DATA_BP_FF;
			end if;
		end process MA_FWD;



	-- Memory Address Generator and Data Alignment ---------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_MEM_ACC: process(MA_CTRL_BUS_I, ALU_RES_BUF, ADR_BASE_FF, ALU_RES_FF, DATA_BP_INT)
			variable mem_adr_v : std_logic_vector(data_width_c-1 downto 0);
			variable dat_end_v : std_logic_vector(data_width_c-1 downto 0);
		begin
			-- address origin --
			if (MA_CTRL_BUS_I(ctrl_mem_daa_c) = '1') then
				mem_adr_v := ALU_RES_BUF; -- use delayed address
			elsif (MA_CTRL_BUS_I(ctrl_mem_bpba_c) = '1') then
				mem_adr_v := ADR_BASE_FF; -- use bypassed address
			else
				mem_adr_v := ALU_RES_FF;
			end if;
			MEM_ADR_FB_O <= mem_adr_v; -- data alignment address
			MEM_ADR_O    <= mem_adr_v; -- memory address output

			-- Endianness converter --
			if (big_endian_c = true) then
				dat_end_v := DATA_BP_INT(data_width_c/2-1 downto 0) & DATA_BP_INT(data_width_c-1 downto data_width_c/2);
			else
				dat_end_v := DATA_BP_INT;
			end if;

			-- data alignment --
			if (word_mode_en_c = false) then -- byte-addressed memory
				if (mem_adr_v(0) = '1') then -- unaligned? -> swap bytes
					MEM_DAT_O <= dat_end_v(data_width_c/2-1 downto 0) & dat_end_v(data_width_c-1 downto data_width_c/2);
				else -- aligned
					MEM_DAT_O <= dat_end_v;
				end if;
			else -- word-addressed memory
				MEM_DAT_O <= dat_end_v;
			end if;
		end process W_MEM_ACC;

		-- R/W Control --
		MEM_RW_O <= MA_CTRL_BUS_I(ctrl_mem_wr_c) and MA_CTRL_BUS_I(ctrl_en_c);



	-- Stage Data Multiplexer ------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		no_mac_mul_units: -- syntheszie no MAC and no MUL unit
			if (build_mul_c = false) and (build_mac_c = false) generate
				ALU_MAC_DAT <= ALU_RES_FF;
			end generate no_mac_mul_units;
		synhesize_mac_mul_units: -- synthesize MAC and/or MUL unit
			if (build_mul_c = true) or (build_mac_c = true) generate
				ALU_MAC_DAT <= MAC_RES_FF when (MA_CTRL_BUS_I(ctrl_use_mac_c) = '1') else ALU_RES_FF;
			end generate synhesize_mac_mul_units;

		no_cp_present: -- no coprocessors present
			if (cp0_present_c = false) and (cp1_present_c = false) generate
				SYS_CP_R_DAT <= RD_MSR_I;
			end generate no_cp_present;
		cp_present: -- at least one coprocessor is present
			if (cp0_present_c = true) or (cp1_present_c = true) generate
				SYS_CP_R_DAT <= CP_DATA_I when (MA_CTRL_BUS_I(ctrl_rd_cp_acc_c) = '1') else RD_MSR_I;
			end generate cp_present;

		-- Multiplexers --
		SYS_CP_ALU_R_DAT <= SYS_CP_R_DAT when (MA_CTRL_BUS_I(ctrl_cp_msr_rd_c) = '1') else ALU_MAC_DAT;
		DATA_O           <= DATA_BP_FF   when (MA_CTRL_BUS_I(ctrl_link_c)      = '1') else SYS_CP_ALU_R_DAT;



	-- Forwarding Path Output ------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------

		-- forwarding data --
		MA_FWD_O(fwd_dat_msb_c downto fwd_dat_lsb_c) <= SYS_CP_ALU_R_DAT;

		-- destination address --
		MA_FWD_O(fwd_adr_3_c downto fwd_adr_0_c) <= MA_CTRL_BUS_I(ctrl_rd_3_c downto ctrl_rd_0_c);

		-- valid forwarding --
		MA_FWD_O(fwd_en_c) <= MA_CTRL_BUS_I(ctrl_wb_en_c);




end MA_STRUCTURE;
