-- ########################################################
-- #         << ATLAS Project - Atlas Processor >>        #
-- # **************************************************** #
-- #  This is the top entity of the Atlas Processor.      #
-- #  The design features the CPU itself, a Wishbone-     #
-- #  compatible bus unit incorporating a cache and a     #
-- #  memory management unit, implemented as system CP.   #
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

entity ATLAS_PROCESSOR is
-- ###############################################################################################
-- ##           Configuration                                                                   ##
-- ###############################################################################################
	generic (
				UC_AREA_BEGIN_G : std_logic_vector(bus_adr_width_c-1 downto 0) := x"FF000000"; -- begin of uncached area
				UC_AREA_END_G   : std_logic_vector(bus_adr_width_c-1 downto 0) := x"FFFFFFFF"  -- end of uncached area
			);
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################
	port	(
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active

-- ###############################################################################################
-- ##           Coprocessor Interface                                                           ##
-- ###############################################################################################

				CP_EN_O         : out std_logic; -- access to cp0
				CP_OP_O         : out std_logic; -- data transfer/processing
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(8 downto 0); -- register addresses / cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data cp0

-- ###############################################################################################
-- ##           External Interrupt Line                                                         ##
-- ###############################################################################################

				IRQ_I           : in  std_logic;  -- external interrupt request

-- ###############################################################################################
-- ##           Wishbone Bus Interface                                                          ##
-- ###############################################################################################

				WB_ADR_O        : out std_logic_vector(bus_adr_width_c-1 downto 0); -- address
				WB_CTI_O        : out std_logic_vector(02 downto 0); -- cycle type
				WB_SEL_O        : out std_logic_vector(01 downto 0); -- byte select
				WB_TGC_O        : out std_logic;                     -- cycle tag
				WB_DATA_O       : out std_logic_vector(data_width_c-1 downto 0); -- data out
				WB_DATA_I       : in  std_logic_vector(data_width_c-1 downto 0); -- data in
				WB_WE_O         : out std_logic;                     -- read/write
				WB_CYC_O        : out std_logic;                     -- cycle
				WB_STB_O        : out std_logic;                     -- strobe
				WB_ACK_I        : in  std_logic;                     -- acknowledge
				WB_HALT_I       : in  std_logic                      -- halt bus transaction
			);
end ATLAS_PROCESSOR;

architecture ATLAS_PROCESSOR_STRUCTURE of ATLAS_PROCESSOR is

	-- Global Control --
	signal HALT        : std_logic; -- halt system
	signal SYS_MODE    : std_logic; -- current processor mode
	signal SYS_INT     : std_logic; -- processing exception
	signal SYS_IRQ     : std_logic; -- system interrupt

	-- Instruction Interface --
	signal INSTR_ADR   : std_logic_vector(data_width_c-1 downto 0); -- instruction address
	signal INSTR_DATA  : std_logic_vector(data_width_c-1 downto 0); -- opcode
	signal INSTR_EN    : std_logic; -- enable pseudo-IR
	signal XTND_I_ADR  : std_logic_vector(bus_adr_width_c-1 downto 0); -- extended instruction address

	-- Data Interface --
	signal MEM_REQ     : std_logic; -- memory request
	signal MEM_RW      : std_logic; -- read/write access
	signal MEM_R_DATA  : std_logic_vector(data_width_c-1 downto 0); -- read data
	signal MEM_W_DATA  : std_logic_vector(data_width_c-1 downto 0); -- write data
	signal MEM_ADR     : std_logic_vector(data_width_c-1 downto 0); -- memory address
	signal XTND_D_ADR  : std_logic_vector(bus_adr_width_c-1 downto 0); -- extended data address

	-- Coprocessor Signals --
	signal USR_CP_EN   : std_logic; -- access user coprocessor
	signal SYS_CP_EN   : std_logic; -- access system coprocessor
	signal CP_OP       : std_logic; -- transfer/data processing
	signal CP_RW       : std_logic; -- read/write access
	signal CP_CMD      : std_logic_vector(8 downto 0); -- register addresses / cmd
	signal CP_W_DATA   : std_logic_vector(data_width_c-1 downto 0); -- write data
	signal SYS_CP_DRB  : std_logic_vector(data_width_c-1 downto 0); -- system coprocessor data readback
	signal CP_DATA_RB  : std_logic_vector(data_width_c-1 downto 0); -- coprocessor data readback

	-- Bus Unit Control --
	signal BUS_ERROR   : std_logic; -- error during bus transaction
	signal CACHE_SYNC  : std_logic; -- cache is sync
	signal CLR_CACHE   : std_logic; -- clear cache
	signal FLS_CACHE   : std_logic; -- flush cache
	signal DIR_ACC     : std_logic; -- force direct access
	signal D_PAGE      : std_logic_vector(15 downto 0); -- data page
	signal I_PAGE      : std_logic_vector(15 downto 0); -- instruction page

begin

	-- Atlas CPU -------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		processor_core: ATLAS_CORE
			port map (
						-- Global Control --
						CLK_I           => CLK_I,      -- global clock line
						RST_I           => RST_I,      -- global reset line, sync, high-active
						HOLD_I          => HALT,       -- stops core when high

						-- Instruction Interface --
						INSTR_ADR_O     => INSTR_ADR,  -- instruction byte adr
						INSTR_DAT_I     => INSTR_DATA, -- instruction input
						INSTR_EN_O      => INSTR_EN,   -- allow IR update

						-- Data Interface --
						SYS_MODE_O      => SYS_MODE,   -- current operating mode
						SYS_INT_O       => SYS_INT,    -- interrupt processing
						MEM_REQ_O       => MEM_REQ,    -- mem access in next cycle
						MEM_RW_O        => MEM_RW,     -- read write
						MEM_ADR_O       => MEM_ADR,    -- data byte adr
						MEM_DAT_O       => MEM_W_DATA, -- write data
						MEM_DAT_I       => MEM_R_DATA, -- read data

						-- Coprocessor Interface --
						USR_CP_EN_O     => USR_CP_EN,  -- access to cp0
						SYS_CP_EN_O     => SYS_CP_EN,  -- access to cp1
						CP_OP_O         => CP_OP,      -- data transfer/processing
						CP_RW_O         => CP_RW,      -- read/write access
						CP_CMD_O        => CP_CMD,     -- register addresses / cmd
						CP_DAT_O        => CP_W_DATA,  -- write data
						CP_DAT_I        => CP_DATA_RB, -- read data cp0 OR cp1

						-- Interrupt Lines --
						EXT_INT_0_I     => IRQ_I,      -- external interrupt
						EXT_INT_1_I     => SYS_IRQ     -- internal interrupt
					);

		-- Coprocessor Data Read-Back --
		CP_DATA_RB <= SYS_CP_DRB or CP_DAT_I;



	-- System Coprocessor ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		memory_management_unit: MMU
			port map (
						-- Global Control --
						CLK_I           => CLK_I,      -- global clock line
						RST_I           => RST_I,      -- global reset line, sync, high-active
						HALT_I          => HALT,       -- inverted clock enable

						-- Processor Interface --
						CP_EN_I         => SYS_CP_EN,  -- access coprocessor
						CP_OP_I         => CP_OP,      -- data transfer/processing
						CP_RW_I         => CP_RW,      -- read/write access
						CP_CMD_I        => CP_CMD,     -- register addresses / cmd
						CP_DAT_I        => CP_W_DATA,  -- write data
						CP_DAT_O        => SYS_CP_DRB, -- read data
						SYS_MODE_I      => SYS_MODE,   -- current operating mode
						INT_EXE_I       => SYS_INT,    -- interrupt beeing executed
						MMU_IRQ_O       => SYS_IRQ,    -- mmu interrupt request

						-- Bus Unit Interface --
						CACHE_ERROR_I   => BUS_ERROR,  -- bus access error
						CACHE_SYNC_I    => CACHE_SYNC, -- cache is sync
						CACHE_CLR_O     => CLR_CACHE,  -- reload cache
						CACHE_FLUSH_O   => FLS_CACHE,  -- synchronize cache with mem
						MEM_DIR_ACC_O   => DIR_ACC,    -- direct access (bypass cache)
						MEM_IP_ADR_O    => I_PAGE,     -- instruction page
						MEM_DP_ADR_O    => D_PAGE      -- data page
					);

		XTND_I_ADR <= I_PAGE & INSTR_ADR; -- address extension
		XTND_D_ADR <= D_PAGE & MEM_ADR;   -- address extension



	-- Cache and Bus Unit ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		bus_unit: BUS_INTERFACE
			generic map (
							-- Configuration --
							UC_AREA_BEGIN_G => UC_AREA_BEGIN_G, -- begin of uncached area
							UC_AREA_END_G   => UC_AREA_END_G    -- end of uncached area
						)
			port map (
						-- Global Control --
						CLK_I            => CLK_I,      -- core clock, all triggering on rising edge
						RST_I            => RST_I,      -- global reset, high active, sync

						-- Instruction Interface --
						INSTR_ADR_I      => XTND_I_ADR, -- instruction byte address
						INSTR_DAT_O      => INSTR_DATA, -- current opcode
						INSTR_EN_I       => INSTR_EN,   -- allow pseudo-IR update

						-- Data Interface --
						MEM_REQ_I        => MEM_REQ,    -- access in next cycle
						MEM_RW_I         => MEM_RW,     -- read/write access
						MEM_ADR_I        => XTND_D_ADR, -- data byte address
						MEM_DAT_I        => MEM_W_DATA, -- write data
						MEM_DAT_O        => MEM_R_DATA, -- read data

						-- Arbitration --
						SYS_MODE_I       => SYS_MODE,   -- current processor mode
						HALT_O           => HALT,       -- stop processor
						ERROR_O          => BUS_ERROR,  -- bus access error
						CACHE_SYNC_O     => CACHE_SYNC, -- cache is sync
						CLR_CACHE_I      => CLR_CACHE,  -- reload cache
						FLUSH_CACHE_I    => FLS_CACHE,  -- synchronize cache with mem
						DIR_ACC_I        => DIR_ACC,    -- force direct access

						-- Wishbone Bus --
						WB_ADR_O         => WB_ADR_O,   -- address
						WB_CTI_O         => WB_CTI_O,   -- cycle type
						WB_SEL_O         => WB_SEL_O,   -- byte select
						WB_TGC_O         => WB_TGC_O,   -- cycle tag
						WB_DATA_O        => WB_DATA_O,  -- data out
						WB_DATA_I        => WB_DATA_I,  -- data in
						WB_WE_O          => WB_WE_O,    -- read/write
						WB_CYC_O         => WB_CYC_O,   -- cycle
						WB_STB_O         => WB_STB_O,   -- strobe
						WB_ACK_I         => WB_ACK_I,   -- acknowledge
						WB_HALT_I        => WB_HALT_I   -- halt bus transaction
					);



end ATLAS_PROCESSOR_STRUCTURE;
