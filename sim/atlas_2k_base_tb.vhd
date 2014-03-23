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
				SYS_IN_I        : in  std_logic_vector(07 downto 0); -- system input

                -- Wishbone Bus --
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

    -- Wishbone Bus --
    signal WB_CLK, WB_RST   : std_logic;
	signal WB_ADR           : std_logic_vector(31 downto 0); -- address
	signal WB_SEL           : std_logic_vector(01 downto 0); -- byte select
	signal WB_DATA_O        : std_logic_vector(15 downto 0); -- data out
	signal WB_DATA_I        : std_logic_vector(15 downto 0); -- data in
	signal WB_WE            : std_logic; -- read/write
	signal WB_CYC           : std_logic; -- cycle enable
	signal WB_STB           : std_logic; -- strobe
	signal WB_ACK           : std_logic; -- acknowledge
    signal WB_ERR           : std_logic; -- bus error

    -- Wishbone Dummy Memory --
    constant wm_mem_size_c : natural := 256; -- BYTE
	constant log2_mem_size_c : natural := log2(wm_mem_size_c/2); -- address width
	signal   WB_ACK_BUF : std_logic;
	type     mem_file_t is array (0 to (wm_mem_size_c/2)-1) of std_logic_vector(15 downto 0);
	signal   MEM_FILE : mem_file_t := (others => (others => '0'));

begin

	-- Stimulus --------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		STIMULUS: process
		begin
			-- all idle --
			RXD      <= '1'; -- idle
			SPI_MISO <= "00000000";
			PIO_IN   <= x"0000";
            WB_ERR   <= '0';
			wait;
		end process STIMULUS;



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
						SYS_IN_I        => BOOT_C_IN,    -- system input

                        -- Wishbone Bus --
                        WB_CLK_O        => WB_CLK,       -- bus clock
                        WB_RST_O        => WB_RST,       -- bus reset, sync, high active
                        WB_ADR_O        => WB_ADR,       -- address
                        WB_SEL_O        => WB_SEL,       -- byte select
                        WB_DATA_O       => WB_DATA_O,    -- data out
                        WB_DATA_I       => WB_DATA_I,    -- data in
                        WB_WE_O         => WB_WE,        -- read/write
                        WB_CYC_O        => WB_CYC,       -- cycle enable
                        WB_STB_O        => WB_STB,       -- strobe
                        WB_ACK_I        => WB_ACK,       -- acknowledge
                        WB_ERR_I        => WB_ERR        -- bus error
					);

		-- BOOT CONFIG --
		BOOT_C_IN(7 downto 2) <= "000000"; -- unused
		BOOT_C_IN(1 downto 0) <= "11"; -- BOOT FROM INTERNAL MEMORY!!!



	-- WB Memory -------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		WB_MEM_FILE_ACCESS: process(WB_CLK)
		begin
			if falling_edge(WB_CLK) then

				--- Data Read/Write ---
				if (WB_STB = '1') and (WB_CYC = '1') then
					if (WB_WE = '1') then
						MEM_FILE(to_integer(unsigned(WB_ADR(log2_mem_size_c downto 1)))) <= WB_DATA_O;
					end if;
					WB_DATA_I <= MEM_FILE(to_integer(unsigned(WB_ADR(log2_mem_size_c downto 1))));
				end if;

				--- ACK Control ---
				if (WB_RST = '1') then
					WB_ACK_BUF <= '0';
				else
					WB_ACK_BUF <= WB_CYC and WB_STB;
				end if;

			end if;
		end process WB_MEM_FILE_ACCESS;

		--- ACK Signal ---
		WB_ACK <= WB_ACK_BUF and WB_CYC;



end atlas_2k_base_tb_structure;
