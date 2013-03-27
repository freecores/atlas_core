-- ########################################################
-- #          << ATLAS Project - Atlas MICRO >>           #
-- # **************************************************** #
-- #  This is the top entity of the CPU-only              #
-- #  implementation. The design instatiates the CPU      #
-- #  itself and incorporates a configurable shared/      #
-- #  distributed data/instruction memory/memories.       #
-- # **************************************************** #
-- #  Last modified: 26.03.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity ATLAS_MICRO is
-- ###############################################################################################
-- ##           Configuration                                                                   ##
-- ###############################################################################################
	generic	(
				LOG2_MEM_SIZE_G : natural := 9;   -- memory address width (mem size)
				SHARED_MEM_G    : boolean := TRUE; -- shared/distributed data/instruction memories
				BOOT_ADDRESS_G  : std_logic_vector(data_width_c-1 downto 0) := x"0000"  -- boot address
			);
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################
	port	(
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				HALT_I          : in  std_logic; -- halt processor when high
				SYS_MODE_O      : out std_logic; -- current processor operating mode

-- ###############################################################################################
-- ##           Coprocessor Interface                                                           ##
-- ###############################################################################################

				USR_CP_EN_O     : out std_logic; -- access to cp0
				SYS_CP_EN_O     : out std_logic; -- access to cp1
				CP_OP_O         : out std_logic; -- data transfer/processing
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(8 downto 0); -- register addresses/cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data (OR-ed)

-- ###############################################################################################
-- ##           External Interrupt Line                                                         ##
-- ###############################################################################################

				IRQ0_I          : in  std_logic; -- external interrupt request line 0
				IRQ1_I          : in  std_logic  -- external interrupt request line 1
			);
end ATLAS_MICRO;

architecture ATLAS_MICROCONTROLLER of ATLAS_MICRO is

	-- Instruction Interface --
	signal INSTR_ADR   : std_logic_vector(data_width_c-1 downto 0); -- instruction address
	signal INSTR_DATA  : std_logic_vector(data_width_c-1 downto 0); -- opcode
	signal INSTR_EN    : std_logic; -- enable pseudo-IR

	-- Data Interface --
	signal MEM_REQ     : std_logic; -- memory request
	signal MEM_REQ_FF  : std_logic; -- memory request
	signal MEM_RW      : std_logic; -- read/write access
	signal MEM_ADR     : std_logic_vector(data_width_c-1 downto 0); -- memory address
	signal MEM_R_DATA  : std_logic_vector(data_width_c-1 downto 0); -- read data
	signal MEM_W_DATA  : std_logic_vector(data_width_c-1 downto 0); -- write data

	-- Memory Type --
	type MEM_FILE_T is array (0 to (2**LOG2_MEM_SIZE_G)-1) of std_logic_vector(data_width_c-1 downto 0);

	-- INIT MEMORY IMAGE X --
	-- Shared memory: Use this memory image for initializing the DATA and INSTRUCTION memory
	-- Separated memories: Use this memory image for initializing the INSTRUCTION memory
	-----------------------------------------------------------------------------------------
	signal MEM_FILE_X : MEM_FILE_T :=
	(
		others => x"0000" -- Place here the init.vhd file content
	);
	-----------------------------------------------------------------------------------------

	-- INIT MEMORY IMAGE Y --
	-- Shared memory: For this case, this component is not used!
	-- Separated memories: Use this memory image for optionally initializing the DATA memory
	-----------------------------------------------------------------------------------------
	signal MEM_FILE_Y : MEM_FILE_T :=
	(
		others => x"0000" -- Place here the init.vhd file content
	);
	-----------------------------------------------------------------------------------------

begin

	-- Atlas CPU -------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		processor_core: ATLAS_CORE
			generic map (
						BOOT_ADDRESS_G  => BOOT_ADDRESS_G(data_width_c-1 downto 0) -- boot address for reset
						)
			port map (
						-- Global Control --
						CLK_I           => CLK_I,       -- global clock line
						RST_I           => RST_I,       -- global reset line, sync, high-active
						HOLD_I          => HALT_I,      -- stops core when high

						-- Instruction Interface --
						INSTR_ADR_O     => INSTR_ADR,   -- instruction byte adr
						INSTR_DAT_I     => INSTR_DATA,  -- instruction input
						INSTR_EN_O      => INSTR_EN,    -- allow IR update

						-- Data Interface --
						SYS_MODE_O      => SYS_MODE_O,  -- current operating mode
						SYS_INT_O       => open,        -- interrupt processing
						MEM_REQ_O       => MEM_REQ,     -- mem access in next cycle
						MEM_RW_O        => MEM_RW,      -- read write
						MEM_ADR_O       => MEM_ADR,     -- data byte adr
						MEM_DAT_O       => MEM_W_DATA,  -- write data
						MEM_DAT_I       => MEM_R_DATA,  -- read data

						-- Coprocessor Interface --
						USR_CP_EN_O     => USR_CP_EN_O, -- access to cp0
						SYS_CP_EN_O     => SYS_CP_EN_O, -- access to cp1
						CP_OP_O         => CP_OP_O,     -- data transfer/processing
						CP_RW_O         => CP_RW_O,     -- read/write access
						CP_CMD_O        => CP_CMD_O,    -- register addresses / cmd
						CP_DAT_O        => CP_DAT_O,    -- write data
						CP_DAT_I        => CP_DAT_I,    -- read data cp0 OR cp1

						-- Interrupt Lines --
						EXT_INT_0_I     => IRQ0_I,      -- external interrupt line 0
						EXT_INT_1_I     => IRQ1_I       -- internal interrupt line 1
					);



	-- Access Request Buffer -------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		ACC_BUFFER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					MEM_REQ_FF <= '0';
				elsif (HALT_I = '0') then
					MEM_REQ_FF <= MEM_REQ;
				end if;
			end if;
		end process ACC_BUFFER;



	-- Internal Memory -------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		INT_MEMORY: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (SHARED_MEM_G = TRUE) then -- Shared I/D-Memory
				-- --------------------------------------------------------------
					if (MEM_REQ_FF = '1') then -- valid access
						if (MEM_RW = '1') then -- write data access
							MEM_FILE_X(to_integer(unsigned(MEM_ADR(LOG2_MEM_SIZE_G downto 1)))) <= MEM_W_DATA;
						else -- read data access
							MEM_R_DATA <= MEM_FILE_X(to_integer(unsigned(MEM_ADR(LOG2_MEM_SIZE_G downto 1))));
						end if;
					end if;
				else -- Separated I/D-Memories
				-- --------------------------------------------------------------
					if (MEM_REQ_FF = '1') then -- valid access
						if (MEM_RW = '1') then -- write data access
							MEM_FILE_Y(to_integer(unsigned(MEM_ADR(LOG2_MEM_SIZE_G downto 1)))) <= MEM_W_DATA;
						else -- read data access
							MEM_R_DATA <= MEM_FILE_Y(to_integer(unsigned(MEM_ADR(LOG2_MEM_SIZE_G downto 1))));
						end if;
					end if;
				end if;
				-- Instruction access
				if (INSTR_EN = '1') then -- instruction update
					INSTR_DATA <= MEM_FILE_X(to_integer(unsigned(INSTR_ADR(LOG2_MEM_SIZE_G downto 1))));
				end if;
			end if;
		end process INT_MEMORY;



end ATLAS_MICROCONTROLLER;
