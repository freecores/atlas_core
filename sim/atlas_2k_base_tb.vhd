library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity atlas_2k_base_tb is
end atlas_2k_base_tb;

architecture atlas_2k_base_tb_structure of atlas_2k_base_tb is

  -- Component: Atlas-2K Processor ----------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component ATLAS_2K_BASE_TOP
	port	(
				-- Globals --
				CLK_I           : in  std_logic; -- global clock line
				RSTN_I          : in  std_logic; -- global reset line, low-active

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
				SYS_IN_I        : in  std_logic_vector(07 downto 0)  -- system input
			);
  end component;

	-- Global Signals --
	signal CLK_GEN          : std_logic := '0';
	signal RSTN_GEN         : std_logic := '0';

	-- IO --
	signal RXD, TXD         : std_logic;                     -- UART
	signal PIO_OUT, PIO_IN  : std_logic_vector(15 downto 0); -- PIO
	signal BOOT_C_IN        : std_logic_vector(07 downto 0); -- Boot/sys condfig
	signal BOOT_C_OUT       : std_logic_vector(07 downto 0); -- Boot/sys status
	signal SPI_MISO         : std_logic_vector(07 downto 0); -- SPI master out slave in
	signal SPI_MOSI         : std_logic_vector(07 downto 0); -- SPI master in slave out
	signal SPI_CSN          : std_logic_vector(07 downto 0); -- SPI chip select (low-active)
	signal SPI_SCK          : std_logic_vector(07 downto 0); -- SPI master clock out

begin

	-- Clock/Reset Generator -------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		CLK_GEN  <= not CLK_GEN after 10 ns; -- 50Mhz
		RSTN_GEN <= '0', '1' after 35 ns;



	-- Processor Core --------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		DUT: ATLAS_2K_BASE_TOP
			port map (
						-- Globals --
						CLK_I           => CLK_GEN,      -- global clock line
						RSTN_I          => RSTN_GEN,     -- global reset line, low-active

						-- UART --
						UART_RXD_I      => RXD,          -- receiver input
						UART_TXD_O      => TXD,          -- UART transmitter output

						-- SPI --
						SPI_MOSI_O      => SPI_MOSI,     -- serial data out
						SPI_MISO_I      => SPI_MISO,     -- serial data in
						SPI_SCK_O       => SPI_SCK,      -- serial clock out
						SPI_CS_O        => SPI_CSN,      -- chip select (low active)

						-- PIO --
						PIO_OUT_O       => PIO_OUT,      -- parallel output
						PIO_IN_I        => PIO_IN,       -- parallel input

						-- System IO --
						SYS_OUT_O       => BOOT_C_OUT,   -- system output
						SYS_IN_I        => BOOT_C_IN     -- system input
					);

		-- BOOT CONFIG --
		BOOT_C_IN(7 downto 2) <= "000000"; -- unused
		BOOT_C_IN(1 downto 0) <= "11"; -- BOOT FROM INTERNAL MEMORY!!!



	-- Stimulus --------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		STIMULUS: process
		begin
			-- all idle --
			RXD      <= '1'; -- idle
			SPI_MISO <= "00000000";
			PIO_IN   <= x"0000";
			wait;
		end process STIMULUS;



end atlas_2k_base_tb_structure;
