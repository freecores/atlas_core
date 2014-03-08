-- ########################################################
-- #         << ATLAS Project - Memory Gateway >>         #
-- # **************************************************** #
-- #  Gateway between CPU instruction/data interface and  #
-- #  bootloader ROM / memory/IO bus system.              #
-- # **************************************************** #
-- #  Last modified: 08.03.2014                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity MEM_GATE is
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active

				I_ADR_I         : in  std_logic_vector(15 downto 0); -- instruction adr
				I_EN_I          : in  std_logic; -- IR update
				I_DAT_O         : out std_logic_vector(15 downto 0); -- instruction out
				D_REQ_I         : in  std_logic; -- request access in next cycle
				D_RW_I          : in  std_logic; -- read/write
				D_ADR_I         : in  std_logic_vector(15 downto 0); -- data adr
				D_DAT_I         : in  std_logic_vector(15 downto 0); -- data in
				D_DAT_O         : out std_logic_vector(15 downto 0); -- data out
				MEM_IP_ADR_I    : in  std_logic_vector(15 downto 0); -- instruction page
				MEM_DP_ADR_I    : in  std_logic_vector(15 downto 0); -- data page

				-- Boot ROM Interface --
				BOOT_I_ADR_O    : out std_logic_vector(15 downto 0); -- instruction adr
				BOOT_I_EN_O     : out std_logic; -- IR update
				BOOT_I_DAT_I    : in  std_logic_vector(15 downto 0); -- instruction out
				BOOT_D_EN_O     : out std_logic; -- access enable
				BOOT_D_RW_O     : out std_logic; -- read/write
				BOOT_D_ADR_O    : out std_logic_vector(15 downto 0); -- data adr
				BOOT_D_DAT_O    : out std_logic_vector(15 downto 0); -- data in
				BOOT_D_DAT_I    : in  std_logic_vector(15 downto 0); -- data out

				-- Memory Interface --
				MEM_I_PAGE_O    : out std_logic_vector(15 downto 0); -- instruction page
				MEM_I_ADR_O     : out std_logic_vector(15 downto 0); -- instruction adr
				MEM_I_EN_O      : out std_logic; -- IR update
				MEM_I_DAT_I     : in  std_logic_vector(15 downto 0); -- instruction out
				MEM_D_EN_O      : out std_logic; -- access enable
				MEM_D_RW_O      : out std_logic; -- read/write
				MEM_D_PAGE_O    : out std_logic_vector(15 downto 0); -- data page
				MEM_D_ADR_O     : out std_logic_vector(15 downto 0); -- data adr
				MEM_D_DAT_O     : out std_logic_vector(15 downto 0); -- data in
				MEM_D_DAT_I     : in  std_logic_vector(15 downto 0)  -- data out
			);
end MEM_GATE;

architecture MEM_GATE_BEHAV of MEM_GATE is

	-- local signals --
	signal MEM_DACC_FF : std_logic;
	signal D_GATE_SEL  : std_logic;
	signal I_GATE_SEL  : std_logic;

begin

	-- Gateway ---------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MEM_ACC_FLAG: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					MEM_DACC_FF <= '0';
				else
					MEM_DACC_FF <= D_REQ_I;
				end if;
			end if;
		end process MEM_ACC_FLAG;

		-- Switch --
		I_GATE_SEL   <= '1' when (MEM_IP_ADR_I(15) = boot_page_c(15)) else '0';
		D_GATE_SEL   <= '1' when (MEM_DP_ADR_I(15) = boot_page_c(15)) else '0';

		-- Bootloader ROM --
		BOOT_I_EN_O  <= I_EN_I when (I_GATE_SEL = '1') else '0';
		BOOT_I_ADR_O <= I_ADR_I;
		BOOT_D_EN_O  <= MEM_DACC_FF when (D_GATE_SEL = '1') else '0';
		BOOT_D_ADR_O <= D_ADR_I when (MEM_DACC_FF = '1') else (others => '0'); -- to reduce switching activity
		BOOT_D_DAT_O <= (others => '0'); -- boot MEM is read-only
		BOOT_D_RW_O  <= D_RW_I;

		-- Memory System --
		MEM_I_EN_O   <= I_EN_I when (I_GATE_SEL = '0') else '0';
		MEM_I_PAGE_O <= '0' & MEM_IP_ADR_I(14 downto 0);
		MEM_I_ADR_O  <= I_ADR_I;
		MEM_D_EN_O   <= MEM_DACC_FF when (D_GATE_SEL = '0') else '0';
		MEM_D_PAGE_O <= '0' & MEM_DP_ADR_I(14 downto 0);
		MEM_D_ADR_O  <= D_ADR_I when (MEM_DACC_FF = '1') else (others => '0'); -- to reduce switching activity
		MEM_D_DAT_O  <= D_DAT_I when (MEM_DACC_FF = '1') else (others => '0'); -- to reduce switching activity
		MEM_D_RW_O   <= D_RW_I;

		-- CPU --
		I_DAT_O      <= BOOT_I_DAT_I when (I_GATE_SEL = '1') else MEM_I_DAT_I;
		D_DAT_O      <= BOOT_D_DAT_I when (D_GATE_SEL = '1') else MEM_D_DAT_I;



end MEM_GATE_BEHAV;
