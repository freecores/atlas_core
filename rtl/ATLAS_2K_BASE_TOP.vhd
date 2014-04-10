-- #########################################################
-- #          << ATLAS Project - Basic System >>           #
-- # ***************************************************** #
-- #  This is the top entity of a simple implementation    #
-- #  of the ATLAS 2k and a compatible memory component.   #
-- #                                                       #
-- #  The number of pages as well as the page size can be  #
-- #  configured via constant in the 'USER CONFIGURATION'  #
-- #  section. Both values must be a number of 2!          #
-- #  Also, the frequency of the 'CLK_I' signal must be    #
-- #  declared in this section (in Hz).                    #
-- #                                                       #
-- # ***************************************************** #
-- #  Last modified: 09.04.2014                            #
-- # ***************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany            #
-- #########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity ATLAS_2K_BASE_TOP is
	port	(
-- ###############################################################################################
-- ##           Global Signals                                                                  ##
-- ###############################################################################################

				CLK_I           : in  std_logic; -- global clock line
				RSTN_I          : in  std_logic; -- global reset line, low-active

-- ###############################################################################################
-- ##           IO Interface                                                                    ##
-- ###############################################################################################

				-- UART --
				UART_RXD_I      : in  std_logic; -- receiver input
				UART_TXD_O      : out std_logic; -- UART transmitter output

				-- SPI --
				SPI_MOSI_O      : out std_logic_vector(07 downto 0); -- serial data out
				SPI_MISO_I      : in  std_logic_vector(07 downto 0); -- serial data in
				SPI_SCK_O       : out std_logic_vector(07 downto 0); -- serial clock out
				SPI_CS_O        : out std_logic_vector(07 downto 0); -- chip select (low active)

				-- PIO --
				PIO_OUT_O       : out std_logic_vector(15 downto 0); -- parallel output
				PIO_IN_I        : in  std_logic_vector(15 downto 0); -- parallel input

				-- System IO --
				SYS_OUT_O       : out std_logic_vector(07 downto 0); -- system output
				SYS_IN_I        : in  std_logic_vector(07 downto 0); -- system input

-- ###############################################################################################
-- ##           Wishbone Bus                                                                    ##
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
end ATLAS_2K_BASE_TOP;

architecture ATLAS_2K_BASE_TOP_STRUCTURE of ATLAS_2K_BASE_TOP is

  -- Component: Atlas-2K Processor ----------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component ATLAS_2K_TOP
	generic (
				CLK_SPEED_G     : std_logic_vector(31 downto 0) := (others => '0') -- clock speed (in Hz)
			);
	port	(
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				CE_I            : in  std_logic; -- global clock enable, high active
				CP_EN_O         : out std_logic; -- access to cp0
				CP_ICE_O        : out std_logic; -- cp interface clock enable
				CP_OP_O         : out std_logic; -- data transfer/processing
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(08 downto 0); -- register addresses / cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data cp0
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
				UART_RXD_I      : in  std_logic; -- receiver input
				UART_TXD_O      : out std_logic; -- UART transmitter output
				SPI_MOSI_O      : out std_logic_vector(07 downto 0); -- serial data out
				SPI_MISO_I      : in  std_logic_vector(07 downto 0); -- serial data in
				SPI_SCK_O       : out std_logic_vector(07 downto 0); -- serial clock out
				SPI_CS_O        : out std_logic_vector(07 downto 0); -- chip select (low active)
				PIO_OUT_O       : out std_logic_vector(15 downto 0); -- parallel output
				PIO_IN_I        : in  std_logic_vector(15 downto 0); -- parallel input
				SYS_OUT_O       : out std_logic_vector(07 downto 0); -- system parallel output
				SYS_IN_I        : in  std_logic_vector(07 downto 0); -- system parallel input
				IRQ_I           : in  std_logic; -- IRQ
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
  end component;

  -- RAM ------------------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component INT_RAM
	generic	(
				MEM_SIZE_G      : natural := 256 -- memory size in words
			);
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				I_ADR_I         : in  std_logic_vector(31 downto 0); -- instruction adr
				I_EN_I          : in  std_logic; -- IR update
				I_DAT_O         : out std_logic_vector(15 downto 0); -- instruction out
				D_EN_I          : in  std_logic; -- access enable
				D_RW_I          : in  std_logic; -- read/write
				D_ADR_I         : in  std_logic_vector(31 downto 0); -- data adr
				D_DAT_I         : in  std_logic_vector(15 downto 0); -- data in
				D_DAT_O         : out std_logic_vector(15 downto 0)  -- data out
			);
  end component;

-- *** USER CONFIGURATION ***
-- ***********************************************************************************************
	constant clk_speed_c    : std_logic_vector(31 downto 0) := x"02FAF080"; -- clock speed in Hz
	constant num_pages_c    : natural := 4; -- number of pages (must be a power of 2)
	constant page_size_c    : natural := 4096; -- page size in bytes (must be a power of 2)
-- ***********************************************************************************************

	-- Internals... -
	constant ram_size_c     : natural := num_pages_c*page_size_c; -- internal RAM size in bytes
	constant ld_pg_size_c   : natural := log2(page_size_c); -- page select address width
	constant ld_num_pg_c    : natural := log2(num_pages_c); -- page size address width

	-- Global Signals --
	signal G_CLK            : std_logic;
	signal G_RST            : std_logic;

	-- Memory Interface --
	signal I_ADR,   D_ADR   : std_logic_vector(data_width_c-1 downto 0);
	signal I_PAGE,  D_PAGE  : std_logic_vector(data_width_c-1 downto 0);
	signal I_EN,    D_EN    : std_logic;
	signal D_RW             : std_logic;
	signal I_DAT_O, D_DAT_O : std_logic_vector(data_width_c-1 downto 0);
	signal D_DAT_I          : std_logic_vector(data_width_c-1 downto 0);
	signal MEM_D_ADR        : std_logic_vector(31 downto 0);
	signal MEM_I_ADR        : std_logic_vector(31 downto 0);

	-- IRQ --
	signal CRITICAL_IRQ     : std_logic;

begin

	-- Clock/Reset -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		G_RST <= not RSTN_I;
		G_CLK <= CLK_I;



	-- Core ------------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		the_core_of_the_problem: ATLAS_2K_TOP
			generic map (
							CLK_SPEED_G => clk_speed_c     -- clock speed (in Hz)
						)
			port map (
						CLK_I           => G_CLK,          -- global clock line
						RST_I           => G_RST,          -- global reset line, sync, high-active
						CE_I            => '1',            -- global clock enable, high active

						CP_EN_O         => open,           -- access to cp0
						CP_ICE_O        => open,           -- cp interface clock enable
						CP_OP_O         => open,           -- data transfer/processing
						CP_RW_O         => open,           -- read/write access
						CP_CMD_O        => open,           -- register addresses / cmd
						CP_DAT_O        => open,           -- write data
						CP_DAT_I        => x"0000",        -- read data cp0

						MEM_I_PAGE_O    => I_PAGE,         -- instruction page
						MEM_I_ADR_O     => I_ADR,          -- instruction adr
						MEM_I_EN_O      => I_EN,           -- IR update
						MEM_I_DAT_I     => I_DAT_O,        -- instruction input
						MEM_D_EN_O      => D_EN,           -- access enable
						MEM_D_RW_O      => D_RW,           -- read/write
						MEM_D_PAGE_O    => D_PAGE,         -- data page
						MEM_D_ADR_O     => D_ADR,          -- data adr
						MEM_D_DAT_O     => D_DAT_I,        -- data out
						MEM_D_DAT_I     => D_DAT_O,        -- data in
						CRITICAL_IRQ_I  => CRITICAL_IRQ,   -- critical error IRQ

						UART_RXD_I      => UART_RXD_I,     -- receiver input
						UART_TXD_O      => UART_TXD_O,     -- UART transmitter output

						SPI_SCK_O       => SPI_SCK_O,      -- serial clock output
						SPI_MOSI_O      => SPI_MOSI_O,     -- serial data output
						SPI_MISO_I      => SPI_MISO_I,     -- serial data input
						SPI_CS_O        => SPI_CS_O,       -- device select - low-active

						PIO_OUT_O       => PIO_OUT_O,      -- parallel output
						PIO_IN_I        => PIO_IN_I,       -- parallel input

						SYS_OUT_O       => SYS_OUT_O,      -- system parallel output
						SYS_IN_I        => SYS_IN_I,       -- system parallel input

						IRQ_I           => '0',            -- IRQ - not used here

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



	-- Memory Mapping --------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MEMORY_MAPPING: process(I_PAGE, D_PAGE, I_ADR, D_ADR)
		begin
			-- default --
			MEM_I_ADR <= (others => '0');
			MEM_D_ADR <= (others => '0');

			-- page address --
			MEM_I_ADR(ld_pg_size_c-1 downto 0) <= I_ADR(ld_pg_size_c-1 downto 0);
			MEM_D_ADR(ld_pg_size_c-1 downto 0) <= D_ADR(ld_pg_size_c-1 downto 0);

			-- page number --
			MEM_I_ADR((ld_pg_size_c+ld_num_pg_c)-1 downto ld_pg_size_c) <= I_PAGE(ld_num_pg_c-1 downto 0);
			MEM_D_ADR((ld_pg_size_c+ld_num_pg_c)-1 downto ld_pg_size_c) <= D_PAGE(ld_num_pg_c-1 downto 0);
		end process MEMORY_MAPPING;



	-- Internal RAM ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		internal_ram: INT_RAM
			generic	map (
							MEM_SIZE_G  => ram_size_c      -- memory size in bytes
						)
			port map (
						-- Host Interface --
						CLK_I           => G_CLK,          -- global clock line
						I_ADR_I         => MEM_I_ADR,      -- instruction adr
						I_EN_I          => I_EN,           -- IR update
						I_DAT_O         => I_DAT_O,        -- instruction out
						D_EN_I          => D_EN,           -- access enable
						D_RW_I          => D_RW,           -- read/write
						D_ADR_I         => MEM_D_ADR,      -- data adr
						D_DAT_I         => D_DAT_I,        -- data in
						D_DAT_O         => D_DAT_O         -- data out
					);



	-- User Section ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		CRITICAL_IRQ <= '0';


end ATLAS_2K_BASE_TOP_STRUCTURE;
