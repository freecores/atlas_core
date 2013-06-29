-- ########################################################
-- #         << ATLAS Project - Atlas Processor >>        #
-- # **************************************************** #
-- #  Top entity of the Atlas-SoC-Processor               #
-- #  Modules:                                            #
-- #   - Atlas Processor                                  #
-- #     - Atlas CPU                                      #
-- #     - MMU                                            #
-- #     - Bus Interface + Cache                          #
-- #   - Multi IO Controller                              #
-- #     - Simple UART                                    #
-- #     - SPI Controller for up to 16 devices            #
-- #     - 16-bit Parallel I/O Controller                 #
-- #     - Custom IP module slot                          #
-- #     - 8 Channel PWM Controller                       #
-- #     - 16+16-bit Timer                                #
-- #     - Linear Feedback Shift Register                 #
-- #     - 8 Channel Interrupt Controller                 #
-- # **************************************************** #
-- #  Last modified: 31.05.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity ATLAS_X2 is
-- ###############################################################################################
-- ##           Configuration                                                                   ##
-- ###############################################################################################
	generic (
				UC_AREA_BEGIN_G : std_logic_vector(bus_adr_width_c-1 downto 0) := x"FF000000"; -- begin of uncached area
				UC_AREA_END_G   : std_logic_vector(bus_adr_width_c-1 downto 0) := x"FFFFFFFF"; -- end of uncached area
				BOOT_ADDRESS_G  : std_logic_vector(bus_adr_width_c-1 downto 0) := x"00000000"; -- boot address
				SYNTH_SPI_G     : boolean := true; -- synthesize SPI core
				SYNTH_CIP_G     : boolean := true; -- synthesize custom IP
				SYNTH_UART_G    : boolean := true; -- synthesize UART core
				SYNTH_PIO_G     : boolean := true; -- synthesize parallel IO core
				SYNTH_PWM_G     : boolean := true; -- synthesize PWM core
				SYNTH_INT_G     : boolean := true; -- synthesize interrupt controller
				SYNTH_LFSR_G    : boolean := true; -- synthesize LFSR
				SYNTH_TIME_G    : boolean := true  -- synthesize timer
			);
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################
	port	(
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active

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
				WB_HALT_I       : in  std_logic;                     -- halt bus transaction

-- ###############################################################################################
-- ##           External Interfaces                                                             ##
-- ###############################################################################################

				-- UART --
				UART_RXD_I      : in  std_logic; -- receiver input
				UART_TXD_O      : out std_logic; -- UART transmitter output

				-- SPI --
				SPI_CLK_O       : out std_logic; -- serial clock output
				SPI_MOSI_O      : out std_logic; -- serial data output
				SPI_MISO_I      : in  std_logic; -- serial data input
				SPI_CS_O        : out std_logic_vector(15 downto 0); -- device select

				-- Parallel IO --
				PIO_OUT_O       : out std_logic_vector(15 downto 0); -- parallel output
				PIO_IN_I        : in  std_logic_vector(15 downto 0); -- parallel input

				-- PWM --
				PWM_O           : out std_logic_vector(07 downto 0); -- pwm channels

				-- IRQs --
				IRQ_I           : in  std_logic_vector(01 downto 0); -- SoC-internal IRQs

				-- LFSR --
				RND_I           : in  std_logic  -- external random feed
			);
end ATLAS_X2;

architecture ATLAS_X2_STRUCTURE of ATLAS_X2 is

	-- Atlas Processor -----------------------------------------------------------------------------------
	---------------------------------------------------------------------------------------------------------
	component ATLAS_PROCESSOR
	generic (
				-- Module Configuration --
				UC_AREA_BEGIN_G : std_logic_vector(bus_adr_width_c-1 downto 0) := x"FF000000"; -- begin of uncached area
				UC_AREA_END_G   : std_logic_vector(bus_adr_width_c-1 downto 0) := x"FFFFFFFF"; -- end of uncached area
				BOOT_ADDRESS_G  : std_logic_vector(bus_adr_width_c-1 downto 0) := x"00000000"  -- boot address
			);
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				-- Coprocessor Interface --
				CP_EN_O         : out std_logic; -- access to cp0
				CP_ICE_O        : out std_logic; -- cp interface enable
				CP_OP_O         : out std_logic; -- data transfer/processing
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(8 downto 0); -- register addresses / cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data cp0
				-- External Interrupt Line --
				IRQ_I           : in  std_logic;  -- external interrupt request
				-- Wishbone Bus Interface --
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
	end component;

	-- Multi IO Controller -------------------------------------------------------------------------------
	---------------------------------------------------------------------------------------------------------
	component MULTI_IO_CTRL
	generic	(
				-- Module Configuration --
				SYNTH_SPI_G     : boolean := true; -- synthesize SPI core
				SYNTH_CIP_G     : boolean := true; -- synthesize custom IP
				SYNTH_UART_G    : boolean := true; -- synthesize UART core
				SYNTH_PIO_G     : boolean := true; -- synthesize parallel IO core
				SYNTH_PWM_G     : boolean := true; -- synthesize PWM core
				SYNTH_INT_G     : boolean := true; -- synthesize interrupt controller
				SYNTH_LFSR_G    : boolean := true; -- synthesize LFSR
				SYNTH_TIME_G    : boolean := true  -- synthesize timer
			);
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				-- Processor Interface --
				CP_EN_I         : in  std_logic; -- access coprocessor
				CP_OP_I         : in  std_logic; -- data transfer/processing
				CP_RW_I         : in  std_logic; -- read/write access
				CP_CMD_I        : in  std_logic_vector(cp_cmd_width_c-1 downto 0); -- register addresses / cmd
				CP_DAT_I        : in  std_logic_vector(data_width_c-1   downto 0); -- write data
				CP_DAT_O        : out std_logic_vector(data_width_c-1   downto 0); -- read data
				CP_IRQ_O        : out std_logic; -- unit interrupt request
				-- UART --
				UART_RXD_I      : in  std_logic; -- receiver input
				UART_TXD_O      : out std_logic; -- UART transmitter output
				-- SPI --
				SPI_CLK_O       : out std_logic; -- serial clock output
				SPI_MOSI_O      : out std_logic; -- serial data output
				SPI_MISO_I      : in  std_logic; -- serial data input
				SPI_CS_O        : out std_logic_vector(15 downto 0); -- device select
				-- Parallel IO --
				PIO_OUT_O       : out std_logic_vector(15 downto 0); -- parallel output
				PIO_IN_I        : in  std_logic_vector(15 downto 0); -- parallel input
				-- PWM --
				PWM_O           : out std_logic_vector(07 downto 0); -- pwm channels
				-- IRQs --
				IRQ_I           : in  std_logic_vector(01 downto 0); -- SoC-internal IRQs
				-- LFSR --
				RND_I           : in  std_logic  -- external random feed
			);
	end component;

	-- PIO Core Connection --
	signal IRQ_LINE : std_logic; -- cp interrupt
	signal CP_EN    : std_logic; -- access to cp
	signal CP_OP    : std_logic; -- data transfer/processing
	signal CP_RW    : std_logic; -- read/write access
	signal CP_CMD   : std_logic_vector(cp_cmd_width_c-1 downto 0); -- register addresses / cmd
	signal CP_DAT_W : std_logic_vector(data_width_c-1   downto 0); -- write data
	signal CP_DAT_R : std_logic_vector(data_width_c-1   downto 0); -- read data
	signal CP_ICE   : std_logic; -- interface clock enable

begin

	-- Atlas Processor -------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	atlas_system_core: ATLAS_PROCESSOR
		generic map (
						-- Module Configuration --
						UC_AREA_BEGIN_G => UC_AREA_BEGIN_G, -- begin of uncached area
						UC_AREA_END_G   => UC_AREA_END_G,   -- end of uncached area
						BOOT_ADDRESS_G  => BOOT_ADDRESS_G   -- boot address
					)
		port map (
						-- Global Control --
						CLK_I           => CLK_I,           -- global clock line
						RST_I           => RST_I,           -- global reset line, sync, high-active

						-- Coprocessor Interface --
						CP_EN_O         => CP_EN,           -- access to cp0
						CP_ICE_O        => CP_ICE,          -- cp interface enable
						CP_OP_O         => CP_OP,           -- data transfer/processing
						CP_RW_O         => CP_RW,           -- read/write access
						CP_CMD_O        => CP_CMD,          -- register addresses / cmd
						CP_DAT_O        => CP_DAT_W,        -- write data
						CP_DAT_I        => CP_DAT_R,        -- read data cp0

						-- External Interrupt Line --
						IRQ_I           => IRQ_LINE,        -- external interrupt request

						-- Wishbone Bus Interface --
						WB_ADR_O        => WB_ADR_O,        -- address
						WB_CTI_O        => WB_CTI_O,        -- cycle type
						WB_SEL_O        => WB_SEL_O,        -- byte select
						WB_TGC_O        => WB_TGC_O,        -- cycle tag
						WB_DATA_O       => WB_DATA_O,       -- data out
						WB_DATA_I       => WB_DATA_I,       -- data in
						WB_WE_O         => WB_WE_O,         -- read/write
						WB_CYC_O        => WB_CYC_O,        -- cycle
						WB_STB_O        => WB_STB_O,        -- strobe
						WB_ACK_I        => WB_ACK_I,        -- acknowledge
						WB_HALT_I       => WB_HALT_I        -- halt bus transaction
				);



	-- Multi IO/Peripheral Core ----------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	system_extension_core: MULTI_IO_CTRL
		generic	map (
						-- Module Configuration --
						SYNTH_SPI_G     => SYNTH_SPI_G,     -- synthesize SPI core
						SYNTH_CIP_G     => SYNTH_CIP_G,     -- synthesize custom IP module
						SYNTH_UART_G    => SYNTH_UART_G,    -- synthesize UART core
						SYNTH_PIO_G     => SYNTH_PIO_G,     -- synthesize parallel IO core
						SYNTH_PWM_G     => SYNTH_PWM_G,     -- synthesize PWM core
						SYNTH_INT_G     => SYNTH_INT_G,     -- synthesize interrupt controller
						SYNTH_LFSR_G    => SYNTH_LFSR_G,    -- synthesize LFSR
						SYNTH_TIME_G    => SYNTH_TIME_G     -- synthesize timer
					)
		port map (
						-- Global Control --
						CLK_I           => CLK_I,           -- global clock line
						RST_I           => RST_I,           -- global reset line, sync, high-active
						ICE_I           => CP_ICE,          -- interface clock enable, high-active

						-- Processor Interface --
						CP_EN_I         => CP_EN,           -- access coprocessor
						CP_OP_I         => CP_OP,           -- data transfer/processing
						CP_RW_I         => CP_RW,           -- read/write access
						CP_CMD_I        => CP_CMD,          -- register addresses / cmd
						CP_DAT_I        => CP_DAT_W,        -- write data
						CP_DAT_O        => CP_DAT_R,        -- read data
						CP_IRQ_O        => IRQ_LINE,        -- unit interrupt request

						-- UART --
						UART_RXD_I      => UART_RXD_I,      -- receiver input
						UART_TXD_O      => UART_TXD_O,      -- UART transmitter output

						-- SPI --
						SPI_CLK_O       => SPI_CLK_O,       -- serial clock output
						SPI_MOSI_O      => SPI_MOSI_O,      -- serial data output
						SPI_MISO_I      => SPI_MISO_I,      -- serial data input
						SPI_CS_O        => SPI_CS_O,        -- device select

						-- Parallel IO --
						PIO_OUT_O       => PIO_OUT_O,       -- parallel output
						PIO_IN_I        => PIO_IN_I,        -- parallel input

						-- PWM --
						PWM_O           => PWM_O,           -- pwm channels

						-- IRQs --
						IRQ_I           => IRQ_I,           -- SoC-internal IRQs

						-- LFSR --
						RND_I           => RND_I            -- external random feed
			);



end ATLAS_X2_STRUCTURE;
