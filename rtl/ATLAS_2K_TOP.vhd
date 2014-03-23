-- ########################################################
-- #       << ATLAS Project - ATLAS 2k Processor >>       #
-- # **************************************************** #
-- #  This is the top entity oth ATLAS 2k processor.      #
-- #  See the core's data sheet for more information.     #
-- # **************************************************** #
-- #  Last modified: 22.03.2014                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity ATLAS_2K_TOP is
-- ###############################################################################################
-- ##           Configuration                                                                   ##
-- ###############################################################################################
	generic (
				CLK_SPEED_G     : std_logic_vector(31 downto 0) := x"00000000" -- clock speed (in Hz)
			);
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################
	port	(
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				CE_I            : in  std_logic; -- global clock enable, high active

-- ###############################################################################################
-- ##           Coprocessor Interface                                                           ##
-- ###############################################################################################

				CP_EN_O         : out std_logic; -- access to cp0
				CP_ICE_O        : out std_logic; -- cp interface clock enable
				CP_OP_O         : out std_logic; -- data transfer/processing
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(08 downto 0); -- register addresses / cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data cp0

-- ###############################################################################################
-- ##           Memory Interface                                                                ##
-- ###############################################################################################

				MEM_I_PAGE_O    : out std_logic_vector(data_width_c-1 downto 0); -- instruction page
				MEM_I_ADR_O     : out std_logic_vector(data_width_c-1 downto 0); -- instruction adr
				MEM_I_EN_O      : out std_logic; -- IR update
				MEM_I_DAT_I     : in  std_logic_vector(data_width_c-1 downto 0); -- instruction input

				MEM_D_EN_O      : out std_logic; -- access enable
				MEM_D_RW_O      : out std_logic; -- read/write
				MEM_D_PAGE_O    : out std_logic_vector(data_width_c-1 downto 0); -- data page
				MEM_D_ADR_O     : out std_logic_vector(data_width_c-1 downto 0); -- data adr
				MEM_D_DAT_O     : out std_logic_vector(data_width_c-1 downto 0); -- data out
				MEM_D_DAT_I     : in  std_logic_vector(data_width_c-1 downto 0); -- data in

				CRITICAL_IRQ_I  : in  std_logic; -- critical error IRQ

-- ###############################################################################################
-- ##           IO Interface                                                                    ##
-- ###############################################################################################

				-- UART --
				UART_RXD_I      : in  std_logic; -- UART receiver input
				UART_TXD_O      : out std_logic; -- UART transmitter output

				-- SPI --
				SPI_MOSI_O      : out std_logic_vector(07 downto 0); -- serial data out
				SPI_MISO_I      : in  std_logic_vector(07 downto 0); -- serial data in
				SPI_SCK_O       : out std_logic_vector(07 downto 0); -- serial clock out
				SPI_CS_O        : out std_logic_vector(07 downto 0); -- chip select (low active)

				-- Parallel IO --
				PIO_OUT_O       : out std_logic_vector(15 downto 0); -- parallel output
				PIO_IN_I        : in  std_logic_vector(15 downto 0); -- parallel input

				-- System IO --
				SYS_OUT_O       : out std_logic_vector(07 downto 0); -- system output
				SYS_IN_I        : in  std_logic_vector(07 downto 0); -- system input

				-- IRQs --
				IRQ_I           : in  std_logic_vector(01 downto 0); -- IRQs

-- ###############################################################################################
-- ##          Wishbone Bus                                                                     ##
-- ###############################################################################################

                WB_CLK_O        : out std_logic; -- bus clock
                WB_RST_O        : out std_logic; -- bus reset, sync, high active
				WB_ADR_O        : out std_logic_vector(31 downto 0); -- address
				WB_SEL_O        : out std_logic_vector(01 downto 0); -- byte select
				WB_DATA_O       : out std_logic_vector(15 downto 0); -- data out
				WB_DATA_I       : in  std_logic_vector(15 downto 0); -- data in
				WB_WE_O         : out std_logic; -- read/write
				WB_CYC_O        : out std_logic; -- cycle enable
				WB_STB_O        : out std_logic; -- strobe
				WB_ACK_I        : in  std_logic; -- acknowledge
                WB_ERR_I        : in  std_logic  -- bus error
			);
end ATLAS_2K_TOP;

architecture ATLAS_2K_TOP_BEHAV of ATLAS_2K_TOP is

	-- Global Control --
	signal SYS_MODE      : std_logic; -- current processor mode
	signal SYS_INT_EXE   : std_logic; -- processing IRQ

	-- Coprocessor Signals --
	signal USR_CP_EN     : std_logic; -- access user coprocessor
	signal SYS_CP_EN     : std_logic; -- access system coprocessor
	signal CP_OP         : std_logic; -- transfer/data processing
	signal CP_RW         : std_logic; -- read/write access
	signal CP_CMD        : std_logic_vector(08 downto 0); -- register addresses / cmd
	signal CP_W_DATA     : std_logic_vector(data_width_c-1 downto 0); -- write data
	signal SYS_CP_DRB    : std_logic_vector(data_width_c-1 downto 0); -- system coprocessor data readback
	signal CP_DATA_RB    : std_logic_vector(data_width_c-1 downto 0); -- coprocessor data readback

	-- CPU Bus --
	signal CPU_D_REQ     : std_logic; -- data access request
	signal CPU_D_RW      : std_logic; -- read/write access
	signal CPU_D_ADR     : std_logic_vector(data_width_c-1 downto 0); -- access address
	signal CPU_D_W_DATA  : std_logic_vector(data_width_c-1 downto 0); -- write data
	signal CPU_D_R_DATA  : std_logic_vector(data_width_c-1 downto 0); -- read data
	signal CPU_I_EN      : std_logic; -- instruction reg enable
	signal CPU_I_ADR     : std_logic_vector(data_width_c-1 downto 0); -- instruction address
	signal CPU_I_DATA    : std_logic_vector(data_width_c-1 downto 0); -- instruction word
	signal CP_DAT_I_SYNC : std_logic_vector(data_width_c-1 downto 0); -- external input sync

	-- MMU --
	signal I_PAGE        : std_logic_vector(data_width_c-1 downto 0); -- instruction page
	signal D_PAGE        : std_logic_vector(data_width_c-1 downto 0); -- data page

	-- Boot Mem --
	signal BOOT_I_ADR    : std_logic_vector(15 downto 0); -- instruction adr
	signal BOOT_I_EN     : std_logic; -- IR update
	signal BOOT_I_DAT    : std_logic_vector(15 downto 0); -- instruction out
	signal BOOT_D_EN     : std_logic; -- access enable
	signal BOOT_D_RW     : std_logic; -- read/write
	signal BOOT_D_ADR    : std_logic_vector(15 downto 0); -- data adr
	signal BOOT_D_DAT_O  : std_logic_vector(15 downto 0); -- data in
	signal BOOT_D_DAT_I  : std_logic_vector(15 downto 0); -- data out

	-- IRQ Lines --
	signal SYS_CP_IRQ    : std_logic; -- IRQ from system coprocessor

begin

	-- Atlas CPU Core --------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		cpu_core: ATLAS_CPU
			port map (
						-- Global Control --
						CLK_I           => CLK_I,          -- global clock line
						RST_I           => RST_I,          -- global reset line, sync, high-active
						CE_I            => CE_I,           -- clock enable

						-- Instruction Interface --
						INSTR_ADR_O     => CPU_I_ADR,      -- instruction byte adr
						INSTR_DAT_I     => CPU_I_DATA,     -- instruction input
						INSTR_EN_O      => CPU_I_EN,       -- allow IR update

						-- Data Interface --
						SYS_MODE_O      => SYS_MODE,       -- current operating mode
						SYS_INT_O       => SYS_INT_EXE,    -- interrupt processing
						MEM_REQ_O       => CPU_D_REQ,      -- mem access in next cycle
						MEM_RW_O        => CPU_D_RW,       -- read write
						MEM_ADR_O       => CPU_D_ADR,      -- data byte adr
						MEM_DAT_O       => CPU_D_W_DATA,   -- write data
						MEM_DAT_I       => CPU_D_R_DATA,   -- read data

						-- Coprocessor Interface --
						USR_CP_EN_O     => USR_CP_EN,      -- access to cp0
						SYS_CP_EN_O     => SYS_CP_EN,      -- access to cp1
						CP_OP_O         => CP_OP,          -- data transfer/processing
						CP_RW_O         => CP_RW,          -- read/write access
						CP_CMD_O        => CP_CMD,         -- register addresses / cmd
						CP_DAT_O        => CP_W_DATA,      -- write data
						CP_DAT_I        => CP_DATA_RB,     -- read data cp0 OR cp1

						-- Interrupt Lines --
						EXT_INT_0_I     => CRITICAL_IRQ_I, -- critical error irq
						EXT_INT_1_I     => SYS_CP_IRQ      -- sys cp irq
					);

		-- External CP data in sync --
		CP_DAT_IN_SYNC: process (CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					CP_DAT_I_SYNC <= (others => '0');
				elsif (CE_I = '1') then
					if (USR_CP_EN = '1') then
						CP_DAT_I_SYNC <= CP_DAT_I;
					else
						CP_DAT_I_SYNC <= (others => '0');
					end if;
				end if;
			end if;
		end process CP_DAT_IN_SYNC;

		-- External Coprocessor Interface --
		CP_EN_O    <= USR_CP_EN;
		CP_OP_O    <= CP_OP;
		CP_RW_O    <= CP_RW;
		CP_CMD_O   <= CP_CMD;
		CP_DAT_O   <= CP_W_DATA;
		CP_DATA_RB <= SYS_CP_DRB or CP_DAT_I_SYNC;
		CP_ICE_O   <= CE_I;



	-- System Coprocessor ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		system_coprocessor: SYSTEM_CP
			generic map	(
						CLOCK_SPEED_G   => CLK_SPEED_G     -- clock speed in Hz
						)
			port map (
						-- Global Control --
						CLK_I           => CLK_I,          -- global clock line
						RST_I           => RST_I,          -- global reset line, sync, high-active
						ICE_I           => CE_I,           -- interface clock enable, high-active

						-- Processor Interface --
						CP_EN_I         => SYS_CP_EN,      -- access coprocessor
						CP_OP_I         => CP_OP,          -- data transfer/processing
						CP_RW_I         => CP_RW,          -- read/write access
						CP_CMD_I        => CP_CMD,         -- register addresses / cmd
						CP_DAT_I        => CP_W_DATA,      -- write data
						CP_DAT_O        => SYS_CP_DRB,     -- read data
						CP_IRQ_O        => SYS_CP_IRQ,     -- unit interrupt request

						SYS_MODE_I      => SYS_MODE,       -- current operating mode
						INT_EXE_I       => SYS_INT_EXE,    -- interrupt beeing executed

						-- Memory Interface --
						MEM_IP_ADR_O    => I_PAGE,         -- instruction page
						MEM_DP_ADR_O    => D_PAGE,         -- data page

						-- IO Interface --
						UART_RXD_I      => UART_RXD_I,     -- receiver input
						UART_TXD_O      => UART_TXD_O,     -- UART transmitter output
						SPI_SCK_O       => SPI_SCK_O,      -- serial clock output
						SPI_MOSI_O      => SPI_MOSI_O,     -- serial data output
						SPI_MISO_I      => SPI_MISO_I,     -- serial data input
						SPI_CS_O        => SPI_CS_O,       -- device select
						PIO_OUT_O       => PIO_OUT_O,      -- parallel output
						PIO_IN_I        => PIO_IN_I,       -- parallel input

						-- System IO --
						SYS_OUT_O       => SYS_OUT_O,      -- system parallel output
						SYS_IN_I        => SYS_IN_I,       -- system parallel input

						-- IRQ Lines --
						IRQ_I           => IRQ_I,          -- external IRQs

                        -- Wishbone Bus --
                        WB_CLK_O        => WB_CLK_O,       -- bus clock
                        WB_RST_O        => WB_RST_O,       -- bus reset, sync, high active
                        WB_ADR_O        => WB_ADR_O,       -- address
                        WB_SEL_O        => WB_SEL_O,       -- byte select
                        WB_DATA_O       => WB_DATA_O,      -- data out
                        WB_DATA_I       => WB_DATA_I,      -- data in
                        WB_WE_O         => WB_WE_O,        -- read/write
                        WB_CYC_O        => WB_CYC_O,       -- cycle enable
                        WB_STB_O        => WB_STB_O,       -- strobe
                        WB_ACK_I        => WB_ACK_I,       -- acknowledge
                        WB_ERR_I        => WB_ERR_I        -- bus error
					);


	-- Memory Gate -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		memory_gate: MEM_GATE
			port map (
						-- Host Interface --
						CLK_I           => CLK_I,          -- global clock line
						RST_I           => RST_I,          -- global reset line, sync, high-active

						I_ADR_I         => CPU_I_ADR,      -- instruction adr
						I_EN_I          => CPU_I_EN,       -- IR update
						I_DAT_O         => CPU_I_DATA,     -- instruction out
						D_REQ_I         => CPU_D_REQ,      -- request access in next cycle
						D_RW_I          => CPU_D_RW,       -- read/write
						D_ADR_I         => CPU_D_ADR,      -- data adr
						D_DAT_I         => CPU_D_W_DATA,   -- data in
						D_DAT_O         => CPU_D_R_DATA,   -- data out
						MEM_IP_ADR_I    => I_PAGE,         -- instruction page
						MEM_DP_ADR_I    => D_PAGE,         -- data page

						-- Boot ROM Interface --
						BOOT_I_ADR_O    => BOOT_I_ADR,     -- instruction adr
						BOOT_I_EN_O     => BOOT_I_EN,      -- IR update
						BOOT_I_DAT_I    => BOOT_I_DAT,     -- instruction out
						BOOT_D_EN_O     => BOOT_D_EN,      -- access enable
						BOOT_D_RW_O     => BOOT_D_RW,      -- read/write
						BOOT_D_ADR_O    => BOOT_D_ADR,     -- data adr
						BOOT_D_DAT_O    => BOOT_D_DAT_O,   -- data in
						BOOT_D_DAT_I    => BOOT_D_DAT_I,   -- data out

						-- Memory Interface --
						MEM_I_PAGE_O    => MEM_I_PAGE_O,   -- instruction page
						MEM_I_ADR_O     => MEM_I_ADR_O,    -- instruction adr
						MEM_I_EN_O      => MEM_I_EN_O,     -- IR update
						MEM_I_DAT_I     => MEM_I_DAT_I,    -- instruction out
						MEM_D_EN_O      => MEM_D_EN_O,     -- access enable
						MEM_D_RW_O      => MEM_D_RW_O,     -- read/write
						MEM_D_PAGE_O    => MEM_D_PAGE_O,   -- instruction page
						MEM_D_ADR_O     => MEM_D_ADR_O,    -- data adr
						MEM_D_DAT_O     => MEM_D_DAT_O,    -- data in
						MEM_D_DAT_I     => MEM_D_DAT_I     -- data out
					);



	-- Bootloader Memory -----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		bootloader_mem: BOOT_MEM
			port map (
						-- Host Interface --
						CLK_I           => CLK_I,          -- global clock line
						I_ADR_I         => BOOT_I_ADR,     -- instruction adr
						I_EN_I          => BOOT_I_EN,      -- IR update
						I_DAT_O         => BOOT_I_DAT,     -- instruction out
						D_EN_I          => BOOT_D_EN,      -- access enable
						D_RW_I          => BOOT_D_RW,      -- read/write
						D_ADR_I         => BOOT_D_ADR,     -- data adr
						D_DAT_I         => BOOT_D_DAT_O,   -- data in
						D_DAT_O         => BOOT_D_DAT_I    -- data out
					);



end ATLAS_2K_TOP_BEHAV;
