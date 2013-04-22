-- ########################################################
-- #         << ATLAS Project - WB Test Memory >>         #
-- # **************************************************** #
-- #  Wishbone-compatible demo memory.                    #
-- # **************************************************** #
-- #  Last modified: 01.04.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity TEST_MEM is
	generic	(
				MEM_SIZE_G    : natural := 256 -- memory size in bytes
			);
	port	(
				-- Wishbone Bus --
				WB_CLK_I      : in  std_logic; -- memory master clock
				WB_RST_I      : in  std_logic; -- high active sync reset
				WB_CTI_I      : in  std_logic_vector(02 downto 0); -- cycle indentifier
				WB_TGC_I      : in  std_logic; -- cycle tag
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
end TEST_MEM;

architecture TEST_MEM_STRUCTURE of TEST_MEM is

	--- Internal Constants --
	constant LOG2_MEM_SIZE : natural := log2(MEM_SIZE_G); -- address width

	--- Buffer ---
	signal WB_ACK_O_INT : std_logic;
	signal WB_DATA_INT  : std_logic_vector(data_width_c-1 downto 0);

	--- Memory Type ---
	type MEM_FILE_TYPE is array (0 to MEM_SIZE_G-1) of std_logic_vector(data_width_c-1 downto 0);

	--- INIT MEMORY IMAGE ---
	------------------------------------------------------
	signal MEM_FILE : MEM_FILE_TYPE :=
	(
		-- This is where you have to place the "init.vhd" file content --
	);
	------------------------------------------------------

begin

	-- Memory Access and Handshake -------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MEM_FILE_ACCESS: process(WB_CLK_I)
		begin
			if rising_edge(WB_CLK_I) then

				--- Data Read/Write ---
				if (WB_STB_I = '1') then
					if (WB_WE_I = '1') then
						MEM_FILE(to_integer(unsigned(WB_ADR_I(LOG2_MEM_SIZE downto 1)))) <= WB_DATA_I;
					else
						WB_DATA_INT <= MEM_FILE(to_integer(unsigned(WB_ADR_I(LOG2_MEM_SIZE downto 1))));
					end if;
				end if;

				--- ACK Control ---
				if (WB_RST_I = '1') then
					WB_ACK_O_INT <= '0';
				else
					WB_ACK_O_INT <= WB_STB_I;
				end if;

			end if;
		end process MEM_FILE_ACCESS;

		--- Output Gate ---
		WB_DATA_O <= WB_DATA_INT;

		--- ACK Signal ---
		WB_ACK_O  <= WB_ACK_O_INT and WB_CYC_I;

		--- Throttle ---
		WB_HALT_O <= '0'; -- yeay, we're at full speed!

		--- Error ---
		WB_ERR_O  <= '0'; -- nothing can go wrong ;)



end TEST_MEM_STRUCTURE;
