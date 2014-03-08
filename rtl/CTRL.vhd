-- ########################################################
-- #        << ATLAS Project - CPU Control Spine >>       #
-- # **************************************************** #
-- #  Main control system, generating control signals     #
-- #  for each pipeline stage.                            #
-- # **************************************************** #
-- #  Last modified: 25.01.2014                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity CTRL is
	port	(
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################

				CLK_I           : in  std_logic; -- global clock line
				CE_I            : in  std_logic; -- clock enable
				RST_I           : in  std_logic; -- global reset line, sync, high-active

-- ###############################################################################################
-- ##           Decoder Interface                                                               ##
-- ###############################################################################################

				OP_DEC_CTRL_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- decoder ctrl lines
				MULTI_CYC_O     : out std_logic; -- multi-cycle indicator
				MULTI_CYC_REQ_I : in  std_logic; -- multi-cycle request
				INSTR_I         : in  std_logic_vector(data_width_c-1 downto 0); -- instruction input
				INSTR_REG_O     : out std_logic_vector(data_width_c-1 downto 0); -- instruction register

-- ###############################################################################################
-- ##           Control Lines                                                                   ##
-- ###############################################################################################

				OF_CTRL_BUS_O   : out std_logic_vector(ctrl_width_c-1 downto 0); -- of stage control
				EX_CTRL_BUS_O   : out std_logic_vector(ctrl_width_c-1 downto 0); -- ex stage control
				MA_CTRL_BUS_O   : out std_logic_vector(ctrl_width_c-1 downto 0); -- ma stage control
				WB_CTRL_BUS_O   : out std_logic_vector(ctrl_width_c-1 downto 0); -- wb stage control

-- ###############################################################################################
-- ##           Function Control                                                                ##
-- ###############################################################################################

				VALID_BRANCH_I  : in  std_logic; -- valid branch detected
				EXC_TAKEN_I     : in  std_logic; -- exception taken
				WAKE_UP_I       : in  std_logic; -- wake up from sleep
				EXC_POS_O       : out std_logic; -- exception would be possible
				STOP_PC_O       : out std_logic; -- freeze program counter
				IR_UPDATE_EN_O  : out std_logic  -- enable instruction reg update
			);
end CTRL;

architecture CTRL_STRUCTURE of CTRL is

	-- Pipeline register --
	signal EX_CTRL_FF       : std_logic_vector(ctrl_width_c-1 downto 0);
	signal EX_CTRL_BUF      : std_logic_vector(ctrl_width_c-1 downto 0);
	signal MA_CTRL_FF       : std_logic_vector(ctrl_width_c-1 downto 0);
	signal WB_CTRL_FF       : std_logic_vector(ctrl_width_c-1 downto 0);

	-- Branch arbiter --
	signal DIS_CYCLE_FF     : std_logic;
	signal DIS_CYCLE        : std_logic;

	-- Instruction Fetch Arbiter --
	signal DIS_IF           : std_logic;
	signal MEM_DEPENDECY    : std_logic;
	signal MULTI_CYC_FF     : std_logic;

	-- System enable/Start-up control --
	signal SYS_ENABLE       : std_logic;
	signal START_FF         : std_logic;
	signal SLEEP_FLAG       : std_logic;

	-- EX LDDD --
--	signal EX_A_MA_FWD      : std_logic;
--	signal EX_A_WB_FWD      : std_logic;
--	signal EX_B_MA_FWD      : std_logic;
--	signal EX_B_WB_FWD      : std_logic;
--	signal EX_C_WB_FWD      : std_logic;

begin

	-- System Enable-FF ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		SYSTEM_ENABLE: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					START_FF    <= '0';
					SLEEP_FLAG  <= '0';
				elsif (CE_I = '1') then
					START_FF <= '1'; -- pretty amazing, huh? ;)
					if (OP_DEC_CTRL_I(ctrl_sleep_c) = '1') then
						SLEEP_FLAG <= '1'; -- go to sleep
					elsif (WAKE_UP_I = '1') then
						SLEEP_FLAG <= '0'; -- wake up
					end if;
				end if;
			end if;
		end process SYSTEM_ENABLE;

		-- Enable control --
		SYS_ENABLE <= (not SLEEP_FLAG) and START_FF;



	-- Stage 0: Pipeline Flow Arbiter ----------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		FLOW_ARBITER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					MULTI_CYC_FF    <= '0';
					DIS_CYCLE_FF    <= '0';
				elsif (CE_I = '1') then
					MULTI_CYC_FF <= MULTI_CYC_REQ_I;
					if (VALID_BRANCH_I = '1') then
						DIS_CYCLE_FF <= '1'; -- one additional cycle for branches and system / ext interrupts
					elsif (DIS_CYCLE_FF = '1') and (MULTI_CYC_REQ_I = '0') then -- hold when multi-cycle op required
						DIS_CYCLE_FF <= '0';
					end if;
				end if;
			end if;
		end process FLOW_ARBITER;

		-- Multi cycle outut --
		MULTI_CYC_O <= MULTI_CYC_FF;


		-- Instruction Register --
		--------------------------
		I_REG: process(CLK_I, INSTR_I)
		begin
			--if rising_edge(CLK_I) then
			--	if (RST_I = '1') then
			--		INSTR_REG_O <= (others => '0');
			--	elsif (CE_I = '1') and (((not DIS_IF) and (not MEM_DEPENDECY)) = '1') then
					INSTR_REG_O <= INSTR_I;
			--	end if;
			--end if;
		end process I_REG;


		-- Temporal Data Dependency Detector for Memory-Load Operations --
		---------------------------------------------------------------------
		T_DDD: process(OP_DEC_CTRL_I, EX_CTRL_FF)
			variable a_match_v, b_match_v : std_logic;
		begin
			-- Operand A dependency? --
			a_match_v := '0';
			if ((OP_DEC_CTRL_I(ctrl_ra_3_c downto ctrl_ra_0_c) = EX_CTRL_FF(ctrl_rd_3_c downto ctrl_rd_0_c)) and (OP_DEC_CTRL_I(ctrl_ra_is_pc_c) = '0')) then
				a_match_v := '1';
			end if;

			-- Operand B dependency? --
			b_match_v := '0';
			if ((OP_DEC_CTRL_I(ctrl_rb_3_c downto ctrl_rb_0_c) = EX_CTRL_FF(ctrl_rd_3_c downto ctrl_rd_0_c)) and (OP_DEC_CTRL_I(ctrl_rb_is_imm_c) = '0')) then
				b_match_v := '1';
			end if;

			-- Memory load dependency? --
			MEM_DEPENDECY <= EX_CTRL_FF(ctrl_en_c) and EX_CTRL_FF(ctrl_rd_wb_c) and EX_CTRL_FF(ctrl_mem_acc_c) and (not EX_CTRL_FF(ctrl_mem_wr_c)) and (a_match_v or b_match_v);
		end process T_DDD;


		-- Disable Control --
		-- Branch / Exception: Disable next 2 cycles
		-- Mem-load dependency: Insert 1 dummy cycle
		branch_slots: -- highly experimental!!!
			if (branch_slots_en_c = true) generate
				DIS_CYCLE <= '1' when (MEM_DEPENDECY = '1') or (SYS_ENABLE = '0') else '0';
			end generate branch_slots;
		no_branch_slots:
			if (branch_slots_en_c = false) generate
				DIS_CYCLE <= '1' when (DIS_CYCLE_FF = '1') or (VALID_BRANCH_I = '1') or (MEM_DEPENDECY = '1') or (SYS_ENABLE = '0') else '0';
			end generate no_branch_slots;
		DIS_IF         <= MULTI_CYC_REQ_I or SLEEP_FLAG;
		STOP_PC_O      <= DIS_IF or MEM_DEPENDECY;
		IR_UPDATE_EN_O <= (not DIS_IF) and (not MEM_DEPENDECY);



	-- Stage 1: Operand Fetch ------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		OF_CTRL_BUS_O <= OP_DEC_CTRL_I;


--		-- Local Data Dependency Detector for EX Stage (pre-processed in OF-Stage) --
--		EX_A_MA_FWD <= '1' when ((OP_DEC_CTRL_I(ctrl_ra_3_c downto ctrl_ra_0_c) = EX_CTRL_BUF(ctrl_rd_3_c downto ctrl_rd_0_c)) and (OP_DEC_CTRL_I(ctrl_ra_is_pc_c)  = '0')) else '0';
--		EX_A_WB_FWD <= '1' when ((OP_DEC_CTRL_I(ctrl_ra_3_c downto ctrl_ra_0_c) = MA_CTRL_FF(ctrl_rd_3_c  downto ctrl_rd_0_c)) and (OP_DEC_CTRL_I(ctrl_ra_is_pc_c)  = '0')) else '0';
--		EX_B_MA_FWD <= '1' when ((OP_DEC_CTRL_I(ctrl_rb_3_c downto ctrl_rb_0_c) = EX_CTRL_BUF(ctrl_rd_3_c downto ctrl_rd_0_c)) and (OP_DEC_CTRL_I(ctrl_rb_is_imm_c) = '0')) else '0';
--		EX_B_WB_FWD <= '1' when ((OP_DEC_CTRL_I(ctrl_rb_3_c downto ctrl_rb_0_c) = MA_CTRL_FF(ctrl_rd_3_c  downto ctrl_rd_0_c)) and (OP_DEC_CTRL_I(ctrl_rb_is_imm_c) = '0')) else '0';
--		EX_C_WB_FWD <= '1' when ((OP_DEC_CTRL_I(ctrl_rb_3_c downto ctrl_rb_0_c) = MA_CTRL_FF(ctrl_rd_3_c  downto ctrl_rd_0_c))) else '0';



	-- Stage 2: Execution ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		EX_STAGE: process (CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					EX_CTRL_FF <= (others => '0');
				elsif (CE_I = '1') then
					EX_CTRL_FF                    <= OP_DEC_CTRL_I;
					EX_CTRL_FF(ctrl_en_c)         <= OP_DEC_CTRL_I(ctrl_en_c) and (not DIS_CYCLE);
					EX_CTRL_FF(ctrl_mcyc_c)       <= MULTI_CYC_FF; -- un-interruptable multi-cycle operation?
--					EX_CTRL_FF(ctrl_a_ex_ma_fw_c) <= EX_A_MA_FWD;
--					EX_CTRL_FF(ctrl_a_ex_wb_fw_c) <= EX_A_WB_FWD;
--					EX_CTRL_FF(ctrl_b_ex_ma_fw_c) <= EX_B_MA_FWD;
--					EX_CTRL_FF(ctrl_b_ex_wb_fw_c) <= EX_B_WB_FWD;
--					EX_CTRL_FF(ctrl_c_ex_wb_fw_c) <= EX_C_WB_FWD;
				end if;
			end if;
		end process EX_STAGE;


		-- Exception insertion system --
		EXC_INSERTION: process (EX_CTRL_FF, EXC_TAKEN_I)
		begin
			EX_CTRL_BUF <= EX_CTRL_FF;
			if (EXC_TAKEN_I = '1') then -- is exception? - insert link register and invalidate current operation
				EX_CTRL_BUF(ctrl_rd_3_c downto ctrl_rd_0_c) <= system_mode_c & link_reg_adr_c; -- save to sys link reg
				EX_CTRL_BUF(ctrl_en_c)   <= '0'; -- disable it all
				EX_CTRL_BUF(ctrl_link_c) <= '1'; -- link return address
			end if;
		end process EXC_INSERTION;

		-- Output --
		EX_CTRL_BUS_O <= EX_CTRL_BUF;
		EXC_POS_O     <= EX_CTRL_FF(ctrl_en_c) and (not EX_CTRL_FF(ctrl_mcyc_c)); -- exception would be possible and no in-interuptable OP



	-- Stage 3: Memory Access ------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MA_STAGE: process (CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					MA_CTRL_FF <= (others => '0');
				elsif (CE_I = '1') then
					MA_CTRL_FF <= EX_CTRL_BUF;
					-- some pre-processing to shorten critical path --
					if (VALID_BRANCH_I = '0') and (EX_CTRL_BUF(ctrl_branch_c) = '1') then -- unfullfilled branch
						MA_CTRL_FF(ctrl_wb_en_c) <= EXC_TAKEN_I; -- IRQs may process anyway
					else
						MA_CTRL_FF(ctrl_wb_en_c) <= (EX_CTRL_BUF(ctrl_en_c) and EX_CTRL_BUF(ctrl_rd_wb_c)) or EXC_TAKEN_I; -- valid reg data write-back
					end if;
					MA_CTRL_FF(ctrl_rd_cp_acc_c) <=  EX_CTRL_BUF(ctrl_cp_acc_c) and (not EX_CTRL_BUF(ctrl_cp_wr_c)); -- cp read-back
					MA_CTRL_FF(ctrl_cp_msr_rd_c) <= (EX_CTRL_BUF(ctrl_cp_acc_c) and (not EX_CTRL_BUF(ctrl_cp_wr_c))) or (EX_CTRL_BUF(ctrl_msr_rd_c)); -- cp or msr read access
				end if;
			end if;
		end process MA_STAGE;

		-- Output --
		MA_CTRL_BUS_O <= MA_CTRL_FF;



	-- Stage 4: Write Back ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		WB_STAGE: process (CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					WB_CTRL_FF <= (others => '0');
				elsif (CE_I = '1') then
					WB_CTRL_FF <= MA_CTRL_FF;
					-- some pre-processing to shorten critical path --
					WB_CTRL_FF(ctrl_rd_mem_acc_c) <= MA_CTRL_FF(ctrl_mem_acc_c) and (not MA_CTRL_FF(ctrl_mem_wr_c)); -- valid memory read-back
				end if;
			end if;
		end process WB_STAGE;

		-- Output --
		WB_CTRL_BUS_O <= WB_CTRL_FF;




end CTRL_STRUCTURE;
