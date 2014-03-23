-- ########################################################
-- #            << ATLAS Project - Demo RAM >>            #
-- # **************************************************** #
-- #  Core-compatible example RAM component.              #
-- # **************************************************** #
-- #  Last modified: 02.03.2014                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity INT_RAM is
	generic	(
				MEM_SIZE_G      : natural := 256 -- memory size in bytes
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
end INT_RAM;

architecture INT_RAM_STRUCTURE of INT_RAM is

	-- Internal Constants --
	constant log2_mem_size_c : natural := log2(MEM_SIZE_G/2); -- address width

	-- Memory Type --
	type int_mem_file_t is array (0 to (MEM_SIZE_G/2)-1) of std_logic_vector(data_width_c-1 downto 0);

--	======================================================================
	signal MEM_FILE : int_mem_file_t;   -- use this for implementation
--	signal MEM_FILE : int_mem_file_t := -- use this for simulation only
--	(
--        others => x"0000"  -- NOP
--	);
--	======================================================================

	-- RAM attribute to inhibit bypass-logic - ALTERA ONLY! --
	attribute ramstyle : string;
	attribute ramstyle of MEM_FILE : signal is "no_rw_check";

begin

	-- Memory Access and Handshake -------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MEM_FILE_ACCESS: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				-- Data Read(/Write) --
				if (D_EN_I = '1') then -- valid access
					if (D_RW_I = '1') then -- write data access
						if (word_mode_en_c = true) then
							MEM_FILE(to_integer(unsigned(D_ADR_I(log2_mem_size_c-1 downto 0)))) <= D_DAT_I;
						else
							MEM_FILE(to_integer(unsigned(D_ADR_I(log2_mem_size_c downto 1)))) <= D_DAT_I;
						end if;
					end if;
					if (word_mode_en_c = true) then
						D_DAT_O <= MEM_FILE(to_integer(unsigned(D_ADR_I(log2_mem_size_c-1 downto 0))));
					else
						D_DAT_O <= MEM_FILE(to_integer(unsigned(D_ADR_I(log2_mem_size_c downto 1))));
					end if;
				end if;
				-- Instruction Read --
				if (I_EN_I = '1') then
					if (word_mode_en_c = true) then
						I_DAT_O <= MEM_FILE(to_integer(unsigned(I_ADR_I(log2_mem_size_c-1 downto 0))));
					else
						I_DAT_O <= MEM_FILE(to_integer(unsigned(I_ADR_I(log2_mem_size_c downto 1))));
					end if;
				end if;
			end if;
		end process MEM_FILE_ACCESS;



end INT_RAM_STRUCTURE;
