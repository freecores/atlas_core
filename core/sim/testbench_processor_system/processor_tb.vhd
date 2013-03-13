library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity processor_tb is
end processor_tb;

architecture processor_tb_structure of processor_tb is

  -- Component: Atlas Processor -------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component ATLAS_PROCESSOR
	generic (
				-- Configuration --
				UC_AREA_BEGIN_G : std_logic_vector(bus_adr_width_c-1 downto 0) := x"FF000000"; -- begin of uncached area
				UC_AREA_END_G   : std_logic_vector(bus_adr_width_c-1 downto 0) := x"FFFFFFFF"  -- end of uncached area
			);
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active

				-- Coprocesor Interface --
				CP_EN_O         : out std_logic; -- access to cp0
				CP_OP_O         : out std_logic; -- data transfer/processing
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(8 downto 0); -- register addresses / cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data cp0

				-- External Interrupt Line --
				IRQ_I           : in  std_logic;  -- external interrupt request

				-- Wishbone Interface --
				WB_ADR_O        : out std_logic_vector(bus_adr_width_c-1 downto 0); -- address
				WB_CTI_O        : out std_logic_vector(02 downto 0); -- cycle type
				WB_SEL_O        : out std_logic_vector(01 downto 0); -- byte select
				WB_TGC_O        : out std_logic_vector(wb_tag_size_c-1 downto 0); -- cycle tag
				WB_DATA_O       : out std_logic_vector(data_width_c-1 downto 0); -- data out
				WB_DATA_I       : in  std_logic_vector(data_width_c-1 downto 0); -- data in
				WB_WE_O         : out std_logic;                     -- read/write
				WB_CYC_O        : out std_logic;                     -- cycle
				WB_STB_O        : out std_logic;                     -- strobe
				WB_ACK_I        : in  std_logic;                     -- acknowledge
				WB_HALT_I       : in  std_logic                      -- halt bus transaction
			);
  end component;

  -- Component: Test Memory -----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component TEST_MEM
	generic	(
				MEM_SIZE      : natural := 256;  -- memory cells
				LOG2_MEM_SIZE : natural := 8;    -- log2(memory cells)
				OUTPUT_GATE   : boolean := FALSE -- output and-gate, might be necessary for some bus systems
			);
	port	(
				-- Wishbone Bus --
				WB_CLK_I      : in  std_logic; -- memory master clock
				WB_RST_I      : in  std_logic; -- high active sync reset
				WB_CTI_I      : in  std_logic_vector(02 downto 0); -- cycle indentifier
				WB_TGC_I      : in  std_logic_vector(wb_tag_size_c-1 downto 0); -- cycle tag
				WB_ADR_I      : in  std_logic_vector(31 downto 0); -- adr in
				WB_DATA_I     : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				WB_DATA_O     : out std_logic_vector(data_width_c-1 downto 0); -- read data
				WB_WE_I       : in  std_logic; -- write enable
				WB_CYC_I      : in  std_logic; -- valid cycle
				WB_STB_I      : in  std_logic; -- valid strobe
				WB_ACK_O      : out std_logic; -- acknowledge
				WB_HALT_O     : out std_logic; -- throttle master
				WB_ERR_O      : out std_logic  -- abnormal cycle termination
			);
  end component;

	-- Global Signals --
	signal G_CLK       : std_logic := '0';
	signal G_RST       : std_logic := '0';
	signal XINT        : std_logic := '0';

	-- Bus --
	signal WB_ADR_O    : std_logic_vector(31 downto 0); -- address
	signal WB_CTI_O    : std_logic_vector(02 downto 0); -- cycle type
	signal WB_SEL_O    : std_logic_vector(01 downto 0); -- byte select
	signal WB_TGC_O    : std_logic_vector(wb_tag_size_c-1 downto 0); -- cycle tag
	signal WB_DATA_O   : std_logic_vector(data_width_c-1 downto 0); -- data out
	signal WB_DATA_I   : std_logic_vector(data_width_c-1 downto 0); -- data in
	signal WB_WE_O     : std_logic;                     -- read/write
	signal WB_CYC_O    : std_logic;                     -- cycle
	signal WB_STB_O    : std_logic;                     -- strobe
	signal WB_ACK_I    : std_logic;                     -- acknowledge
	signal WB_HALT_I   : std_logic;                     -- halt bus transaction

begin

	-- Clock/Reset Generator -------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		G_CLK <= not G_CLK after 5 ns;
		G_RST <= '1', '0' after 25 ns;


	-- Stimulus --------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		STIMULUS: process
		begin
			XINT <= '0';
			wait for 905 ns;
			wait for 2500 ns;
			XINT <= '1';
			wait for 10 ns; -- just enough to get attention ^^
			XINT <= '0';
			wait for 40000 ns; -- wait for the end
		end process STIMULUS;


	-- Core ------------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		the_core_of_the_problem: ATLAS_PROCESSOR
			generic map (
						-- Configuration --
						UC_AREA_BEGIN_G => x"FF000000", -- begin of uncached area
						UC_AREA_END_G   => x"FFFFFFFF"  -- end of uncached area
					)
			port map (
						-- Global Control --
						CLK_I           => G_CLK,       -- global clock line
						RST_I           => G_RST,       -- global reset line, sync, high-active

						-- Coprocesor Interface --
						CP_EN_O         => open,        -- access to cp0
						CP_OP_O         => open,        -- data transfer/processing
						CP_RW_O         => open,        -- read/write access
						CP_CMD_O        => open,        -- register addresses / cmd
						CP_DAT_O        => open,        -- write data
						CP_DAT_I        => x"0000",     -- read data cp0

						-- External Interrupt Line --
						IRQ_I           => XINT,        -- external interrupt request

						-- Wishbone Interface --
						WB_ADR_O        => WB_ADR_O,    -- address
						WB_CTI_O        => WB_CTI_O,    -- cycle type
						WB_SEL_O        => WB_SEL_O,    -- byte select
						WB_TGC_O        => WB_TGC_O,    -- cycle tag
						WB_DATA_O       => WB_DATA_O,   -- data out
						WB_DATA_I       => WB_DATA_I,   -- data in
						WB_WE_O         => WB_WE_O,     -- read/write
						WB_CYC_O        => WB_CYC_O,    -- cycle
						WB_STB_O        => WB_STB_O,    -- strobe
						WB_ACK_I        => WB_ACK_I,    -- acknowledge
						WB_HALT_I       => WB_HALT_I    -- halt bus transaction
					);



	-- Memory ----------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		test_memory: TEST_MEM
			port map (
				-- Wishbone Bus --
						WB_CLK_I        => G_CLK, -- memory master clock
						WB_RST_I        => G_RST, -- high active sync reset
						WB_CTI_I        => WB_CTI_O, -- cycle indentifier
						WB_TGC_I        => WB_TGC_O, -- cycle tag
						WB_ADR_I        => WB_ADR_O, -- adr in
						WB_DATA_I       => WB_DATA_O, -- write data
						WB_DATA_O       => WB_DATA_I, -- read data
						WB_WE_I         => WB_WE_O, -- write enable
						WB_CYC_I        => WB_CYC_O, -- valid cycle
						WB_STB_I        => WB_STB_O, -- valid strobe
						WB_ACK_O        => WB_ACK_I, -- acknowledge
						WB_HALT_O       => WB_HALT_I, -- throttle master
						WB_ERR_O        => open -- abnormal cycle termination
					);




end processor_tb_structure;
