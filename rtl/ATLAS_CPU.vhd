-- ########################################################
-- #         << ATLAS Project - Atlas CPU Core >>         #
-- # **************************************************** #
-- #  This is the top entity of the CPU core.             #
-- #  All submodules are instantiated here.               #
-- # **************************************************** #
-- #  Last modified: 23.03.2014                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity ATLAS_CPU is
	port	(
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################

				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				CE_I            : in  std_logic; -- clock enable, high-active

-- ###############################################################################################
-- ##           Instruction Interface                                                           ##
-- ###############################################################################################

				INSTR_ADR_O     : out std_logic_vector(data_width_c-1 downto 0); -- instruction byte adr
				INSTR_DAT_I     : in  std_logic_vector(data_width_c-1 downto 0); -- instruction input
				INSTR_EN_O      : out std_logic; -- allow IR update

-- ###############################################################################################
-- ##           Data Interface                                                                  ##
-- ###############################################################################################

				-- Memory Arbitration --
				SYS_MODE_O      : out std_logic; -- current operating mode
				SYS_INT_O       : out std_logic; -- interrupt processing

				-- Memory System --
				MEM_REQ_O       : out std_logic; -- mem access in next cycle
				MEM_RW_O        : out std_logic; -- read write
				MEM_ADR_O       : out std_logic_vector(data_width_c-1 downto 0); -- data byte adr
				MEM_DAT_O       : out std_logic_vector(data_width_c-1 downto 0); -- write data
				MEM_DAT_I       : in  std_logic_vector(data_width_c-1 downto 0); -- read data

-- ###############################################################################################
-- ##           Coprocessor Interface                                                           ##
-- ###############################################################################################

				USR_CP_EN_O     : out std_logic; -- access to cp0
				SYS_CP_EN_O     : out std_logic; -- access to cp1
				CP_OP_O         : out std_logic; -- data transfer/processing
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(cp_cmd_width_c-1 downto 0); -- register addresses / cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data cp0 OR cp1

-- ###############################################################################################
-- ##           External Interrupt Lines                                                        ##
-- ###############################################################################################

				EXT_INT_0_I     : in  std_logic; -- external interrupt request 0
				EXT_INT_1_I     : in  std_logic  -- external interrupt request 1
			);
end ATLAS_CPU;

architecture ATLAS_CPU_BEHAV of ATLAS_CPU is

	-- Global Nets  --
	signal G_CLK         : std_logic; -- global clock line
	signal G_RST         : std_logic; -- global reset line
	signal G_CE          : std_logic; -- global clock enable

	-- Control Lines --
	signal OF_CTRL       : std_logic_vector(ctrl_width_c-1 downto 0);
	signal EX_CTRL       : std_logic_vector(ctrl_width_c-1 downto 0);
	signal MA_CTRL       : std_logic_vector(ctrl_width_c-1 downto 0);
	signal WB_CTRL       : std_logic_vector(ctrl_width_c-1 downto 0);

	-- Forwarding Paths --
	signal MA_FWD        : std_logic_vector(fwd_width_c-1  downto 0);
	signal WB_FWD        : std_logic_vector(fwd_width_c-1  downto 0);

	-- Data Lines --
	signal WB_DATA       : std_logic_vector(data_width_c-1 downto 0); -- write back data
	signal OP_A_DATA     : std_logic_vector(data_width_c-1 downto 0); -- operand A data
	signal OP_B_DATA     : std_logic_vector(data_width_c-1 downto 0); -- operand B data
	signal OP_C_DATA     : std_logic_vector(data_width_c-1 downto 0); -- operand C data
	signal BP_A_DATA     : std_logic_vector(data_width_c-1 downto 0); -- operand A bypass
	signal BP_C_DATA     : std_logic_vector(data_width_c-1 downto 0); -- operand C bypass
	signal ALU_RES       : std_logic_vector(data_width_c-1 downto 0); -- alu result
	signal MUL_RES       : std_logic_vector(2*data_width_c-1 downto 0); -- mul result
	signal IMMEDIATE     : std_logic_vector(data_width_c-1 downto 0); -- immediate value
	signal T_FLAG        : std_logic; -- transfer flag
	signal MA_DATA       : std_logic_vector(data_width_c-1 downto 0); -- ma stage result
	signal MEM_ADR_FB    : std_logic_vector(data_width_c-1 downto 0); -- mem adr feedback

	-- Program Counter --
	signal PC_1D         : std_logic_vector(data_width_c-1 downto 0); -- 1x delayed pc
	signal STOP_PC       : std_logic; -- freeze PC

	-- Flag Stuff --
	signal ALU_FLAG_I    : std_logic_vector(flag_bus_width_c-1 downto 0); -- alu flag input
	signal ALU_FLAG_O    : std_logic_vector(flag_bus_width_c-1 downto 0); -- alu flag output
	signal MSR_W_DATA    : std_logic_vector(data_width_c-1 downto 0); -- msr write data
	signal MSR_R_DATA    : std_logic_vector(data_width_c-1 downto 0); -- msr read data

	-- Control Signals --
	signal VALID_BRANCH  : std_logic; -- taken branch
	signal EXC_POS       : std_logic; -- exception would be possible
	signal EXC_TAKEN     : std_logic; -- async interrupt taken
	signal WAKE_UP_CALL  : std_logic; -- wake up from sleep
	signal MODE          : std_logic; -- current operating mode
	signal MODE_FF       : std_logic; -- delayed current operating mode
    signal COND_TRUE     : std_logic; -- condition is true

	-- Opcode Decoder --
	signal OP_CTRL       : std_logic_vector(ctrl_width_c-1 downto 0); -- decoder contorl output
	signal MULTI_CYC     : std_logic; -- multi-cycle indicator
	signal MULTI_CYC_REQ : std_logic; -- multi-cycle reqest
	signal INSTR_REG     : std_logic_vector(data_width_c-1 downto 0); -- instruction register

	-- Coprocessor --
	signal CP_PTC        : std_logic; -- user coprocessor protection

begin

	-- Global Signals --------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		G_CLK <= CLK_I;
		G_CE  <= CE_I;
		G_RST <= RST_I;



	-- Opcode Decoder --------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		Opcode_Decoder: OP_DEC
			port map (
						-- Decoder Interface Input --
						INSTR_I         => INSTR_REG,      -- instruction input
						INSTR_ADR_I     => PC_1D,          -- corresponding address
						T_FLAG_I        => T_FLAG,         -- T-Flag input
						M_FLAG_I        => MODE_FF,        -- Mode flag input
						MULTI_CYC_I     => MULTI_CYC,      -- multi-cycle indicator
						CP_PTC_I        => CP_PTC,         -- coprocessor protection

						-- Decoder Interface Output --
						MULTI_CYC_REQ_O => MULTI_CYC_REQ,  -- multi-cycle reqest
						CTRL_O          => OP_CTRL,        -- decoder ctrl lines
						IMM_O           => IMMEDIATE       -- immediate
					);



	-- Control System --------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		Control_Spine: CTRL
			port map (
						-- Global Control --
						CLK_I           => G_CLK,          -- global clock line
						CE_I            => G_CE,           -- clock enable
						RST_I           => G_RST,          -- global reset line, sync, high-active

						-- Decoder Interface --
						OP_DEC_CTRL_I   => OP_CTRL,        -- decoder ctrl lines
						MULTI_CYC_O     => MULTI_CYC,      -- multi-cycle indicator
						MULTI_CYC_REQ_I => MULTI_CYC_REQ,  -- multi-cycle request
						INSTR_I         => INSTR_DAT_I,    -- instruction input
						INSTR_REG_O     => INSTR_REG,      -- instruction register

						-- Control Lines --
						OF_CTRL_BUS_O   => OF_CTRL,        -- of stage control
						EX_CTRL_BUS_O   => EX_CTRL,        -- ex stage control
						MA_CTRL_BUS_O   => MA_CTRL,        -- ma stage control
						WB_CTRL_BUS_O   => WB_CTRL,        -- wb stage control

						-- Function Control --
                        COND_TRUE_I     => COND_TRUE,      -- condition is true
						VALID_BRANCH_I  => VALID_BRANCH,   -- valid branch detected
						EXC_TAKEN_I     => EXC_TAKEN,      -- excation execute
						WAKE_UP_I       => WAKE_UP_CALL,   -- wake up from sleep
						EXC_POS_O       => EXC_POS,        -- exception would be possible
						STOP_PC_O       => STOP_PC,        -- freeze program counter
						IR_UPDATE_EN_O  => INSTR_EN_O      -- enable instruction reg update
					);



	-- Machine Status System -------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		System_Reg: SYS_REG
			port map (
						-- Global Control --
						CLK_I           => G_CLK,          -- global clock line
						CE_I            => G_CE,           -- clock enable
						RST_I           => G_RST,          -- global reset line, asyc

						-- Function Control --
						EX_CTRL_BUS_I   => EX_CTRL,        -- ex stage control
						MA_CTRL_BUS_I   => MA_CTRL,        -- ma stage control
						EXT_INT_REQ0_I  => EXT_INT_0_I,    -- external interrupt request 0
						EXT_INT_REQ1_I  => EXT_INT_1_I,    -- external interrupt request 1

						-- Data Input --
						FLAG_BUS_I      => ALU_FLAG_O,     -- flag input
						EXC_POS_I       => EXC_POS,        -- exception would be possible
						STOP_PC         => STOP_PC,        -- freeze pc
						PC_DATA_I       => ALU_RES,        -- PC write data
						MSR_DATA_I      => MSR_W_DATA,     -- MSR write data

						-- Data Output --
						FLAG_BUS_O      => ALU_FLAG_I,     -- flag output
						VALID_BRANCH_O  => VALID_BRANCH,   -- valid branch detected
						EXC_EXECUTED_O  => EXC_TAKEN,      -- executed exception
						WAKE_UP_O       => WAKE_UP_CALL,   -- wake-up signal
						RD_MSR_O        => MSR_R_DATA,     -- read data msr
						PC_O            => INSTR_ADR_O,    -- pc output
						PC_1D_O         => PC_1D,          -- pc 1x delayed
						CP_PTC_O        => CP_PTC,         -- coprocessor protection
                        COND_TRUE_O     => COND_TRUE,      -- condition is true
						MODE_O          => MODE,           -- current mode
						MODE_FF_O       => MODE_FF         -- delayed current mode
					);

			-- Control Lines --
			SYS_MODE_O <= MODE; -- current operating mode
			SYS_INT_O  <= EXC_TAKEN; -- exception taken



	-- OF Stage --------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		Operand_Fetch: REG_FILE
			port map (
						-- Global Control --
						CLK_I           => G_CLK,          -- global clock line
						CE_I            => G_CE,           -- clock enable
						RST_I           => G_RST,          -- global reset line, sync, high-active

						-- Function Control --
						WB_CTRL_BUS_I   => WB_CTRL,        -- wb stage control
						OF_CTRL_BUS_I   => OF_CTRL,        -- of stage control

						-- Data Input --
						WB_DATA_I       => WB_DATA,        -- write back data
						IMMEDIATE_I     => IMMEDIATE,      -- immediates
						PC_1D_I         => PC_1D,          -- pc 1x delayed
						WB_FWD_I        => WB_FWD,         -- WB stage forwarding path

						-- Data Output --
						OP_A_DATA_O     => OP_A_DATA,      -- operand A output
						OP_B_DATA_O     => OP_B_DATA,      -- operand B output
						OP_C_DATA_O     => OP_C_DATA       -- operand C output
					);



	-- EX Stage --------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		Executor: ALU
			port map (
						-- Global Control --
						CLK_I           => G_CLK,          -- global clock line
						CE_I            => G_CE,           -- clock enable
						RST_I           => G_RST,          -- global reset line, sync, high-active

						-- Function Control --
						EX_CTRL_BUS_I   => EX_CTRL,        -- stage control
						FLAG_BUS_I      => ALU_FLAG_I,     -- flag input

						-- Data Input --
						OP_A_I          => OP_A_DATA,      -- operand A input
						OP_B_I          => OP_B_DATA,      -- operand B input
						OP_C_I          => OP_C_DATA,      -- operand C input
						PC_1D_I         => PC_1D,          -- 1x delayed PC
						MA_FWD_I        => MA_FWD,         -- MA stage forwarding path
						WB_FWD_I        => WB_FWD,         -- WB stage forwarding path

						-- Data Output --
						FLAG_BUS_O      => ALU_FLAG_O,     -- flag output
						MASK_T_FLAG_O   => T_FLAG,         -- T-Flag for mask generation
						MSR_DATA_O      => MSR_W_DATA,     -- MSR write data
						ALU_RES_O       => ALU_RES,        -- ALU result
						MUL_RES_O       => MUL_RES,        -- MUL result
						BP_OPA_O        => BP_A_DATA,      -- operand A bypass
						BP_OPC_O        => BP_C_DATA,      -- operand C bypass

						-- Coprocessor Interface --
						CP_CP0_EN_O     => USR_CP_EN_O,    -- access to cp0
						CP_CP1_EN_O     => SYS_CP_EN_O,    -- access to cp1
						CP_OP_O         => CP_OP_O,        -- data transfer/operation
						CP_RW_O         => CP_RW_O,        -- read/write access
						CP_CMD_O        => CP_CMD_O,       -- register addresses / cmd
						CP_DAT_O        => CP_DAT_O,       -- write data

						-- Memory access --
						MEM_REQ_O       => MEM_REQ_O       -- data memory access request for next cycle
					);



	-- MA Stage --------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		Memory_Access: MEM_ACC
			port map (
						-- Global Control --
						CLK_I           => G_CLK,          -- global clock line
						CE_I            => G_CE,           -- clock enable
						RST_I           => G_RST,          -- global reset line, asyc

						-- Function Control --
						MA_CTRL_BUS_I   => MA_CTRL,        -- ma stage control

						-- Data Input --
						ALU_RES_I       => ALU_RES,        -- alu result
						MUL_RES_I       => MUL_RES,        -- mul result
						ADR_BASE_I      => BP_A_DATA,      -- op_a bypass
						DATA_BP_I       => BP_C_DATA,      -- op_b bypass
						CP_DATA_I       => CP_DAT_I,       -- coprocessor rd data
						RD_MSR_I        => MSR_R_DATA,     -- read data msr
						WB_FWD_I        => WB_FWD,         -- WB stage forwarding path

						-- Data Output --
						DATA_O          => MA_DATA,        -- data output
						MEM_ADR_FB_O    => MEM_ADR_FB,     -- memory address feedback
						MA_FWD_O        => MA_FWD,         -- MA stage forwarding path

						-- Memory (w) Interface --
						MEM_ADR_O       => MEM_ADR_O,      -- address output
						MEM_DAT_O       => MEM_DAT_O,      -- write data output
						MEM_RW_O        => MEM_RW_O        -- read write
					);



	-- WB Stage --------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		Write_Back: WB_UNIT
			port map (
						-- Global Control --
						CLK_I           => G_CLK,          -- global clock line
						CE_I            => G_CE,           -- clock enable
						RST_I           => G_RST,          -- global reset line, sync, high-active

						-- Function Control --
						WB_CTRL_BUS_I   => WB_CTRL,        -- wb stage control

						-- Data Input --
						MEM_WB_DAT_I    => MEM_DAT_I,      -- memory read data
						ALU_WB_DAT_I    => MA_DATA,        -- alu read data
						MEM_ADR_FB_I    => MEM_ADR_FB,     -- memory address feedback

						-- Data Output --
						WB_DATA_O       => WB_DATA,        -- write back data
						WB_FWD_O        => WB_FWD          -- WB stage forwarding path
					);



end ATLAS_CPU_BEHAV;
