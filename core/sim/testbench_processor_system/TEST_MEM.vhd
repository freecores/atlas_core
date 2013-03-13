-- ########################################################
-- #         << ATLAS Project - WB Test Memory >>         #
-- # **************************************************** #
-- #  Wishbone-compatible demo memory.                    #
-- # **************************************************** #
-- #  Last modified: 13.03.2013                           #
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
end TEST_MEM;

architecture TEST_MEM_STRUCTURE of TEST_MEM is

	--- Buffer ---
	signal WB_ACK_O_INT : std_logic;
	signal WB_DATA_INT  : std_logic_vector(data_width_c-1 downto 0);

	--- Memory Type ---
	type MEM_FILE_TYPE is array (0 to MEM_SIZE-1) of std_logic_vector(data_width_c-1 downto 0);

	--- INIT MEMORY IMAGE ---
	------------------------------------------------------
	signal MEM_FILE : MEM_FILE_TYPE :=
	(
000000 => x"bc1a", -- B
000001 => x"bc12", -- B
000002 => x"bc11", -- B
000003 => x"bc01", -- B
000004 => x"07f2", -- DEC
000005 => x"5078", -- LDR
000006 => x"2891", -- CLR
000007 => x"ccfc", -- LDIH
000008 => x"3001", -- BIC
000009 => x"c642", -- LDIL
000010 => x"ca00", -- LDIH
000011 => x"50c8", -- LDR
000012 => x"54c8", -- STR
000013 => x"e400", -- CDP
000014 => x"ec00", -- MRC
000015 => x"dc83", -- STBI
000016 => x"b9fe", -- BTS
000017 => x"e401", -- CDP
000018 => x"bc00", -- B
000019 => x"07f2", -- DEC
000020 => x"bc05", -- B
000021 => x"0000", -- NOP
000022 => x"0000", -- NOP
000023 => x"0000", -- NOP
000024 => x"0000", -- NOP
000025 => x"37f2", -- RETI
000026 => x"c000", -- LDIL
000027 => x"c800", -- LDIH
000028 => x"c04c", -- LDIL
000029 => x"c800", -- LDIH
000030 => x"1880", -- LDSR
000031 => x"d49d", -- SBR
000032 => x"1c01", -- STSR
000033 => x"3403", -- GTXI
000034 => x"0000", -- NOP
000035 => x"0000", -- NOP
000036 => x"0000", -- NOP
000037 => x"0000", -- NOP
000038 => x"c47f", -- LDIL
000039 => x"c004", -- LDIL
000040 => x"c800", -- LDIH
000041 => x"c4c2", -- LDIL
000042 => x"c880", -- LDIH
000043 => x"c17a", -- LDIL
000044 => x"c900", -- LDIH
000045 => x"c642", -- LDIL
000046 => x"ca00", -- LDIH
000047 => x"c6ae", -- LDIL
000048 => x"ca88", -- LDIH
000049 => x"62c5", -- SWP
000050 => x"29b3", -- CLR
000051 => x"1c71", -- STAF
000052 => x"be07", -- BL
000053 => x"be04", -- BL
000054 => x"0409", -- DECS
000055 => x"85fd", -- BNE
000056 => x"fc81", -- SYSCALL
000057 => x"7daa", -- STR
000058 => x"37f0", -- RET
000059 => x"799a", -- LDR
000060 => x"37f0", -- RET
000061 => x"0000", -- NOP
000062 => x"0000", -- NOP
000063 => x"0000", -- NOP
000064 => x"0000", -- NOP
000065 => x"0000", -- NOP
000066 => x"0000", -- NOP
000067 => x"0000", -- NOP
000068 => x"0000", -- NOP
000069 => x"0000", -- NOP
000070 => x"0000", -- NOP
000071 => x"0000", -- NOP
000072 => x"0000", -- NOP
000073 => x"0000", -- NOP
000074 => x"0000", -- NOP
000075 => x"0000", -- NOP
000076 => x"0000", -- NOP
000077 => x"0000", -- NOP
000078 => x"0000", -- NOP
000079 => x"0000", -- NOP
000080 => x"0000", -- NOP
000081 => x"0000", -- NOP
000082 => x"0000", -- NOP
000083 => x"0000", -- NOP
000084 => x"0000", -- NOP
000085 => x"0000", -- NOP
000086 => x"0000", -- NOP
000087 => x"0000", -- NOP
000088 => x"0000", -- NOP
000089 => x"0000", -- NOP
000090 => x"0000", -- NOP
000091 => x"0000", -- NOP
000092 => x"0000", -- NOP
000093 => x"0000", -- NOP
000094 => x"0000", -- NOP
000095 => x"0000", -- NOP
000096 => x"0000", -- NOP
000097 => x"5b88", -- 23432
000098 => x"00ea", -- 234
000099 => x"000e", -- 14
000100 => x"0509", -- 1289
others => x"0000"  -- NOP
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
		WB_DATA_O <= WB_DATA_INT when (OUTPUT_GATE = FALSE) or ((OUTPUT_GATE = TRUE) and (WB_STB_I = '1')) else (others => '0');

		--- ACK Signal ---
		WB_ACK_O  <= WB_ACK_O_INT and WB_CYC_I;

		--- Throttle ---
		WB_HALT_O <= '0'; -- yeay, we're at full speed!

		--- Error ---
		WB_ERR_O  <= '0'; -- nothing can go wrong ;)



end TEST_MEM_STRUCTURE;
