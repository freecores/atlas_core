library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity micro_tb is
end micro_tb;

architecture micro_tb_structure of micro_tb is

  -- Component: Atlas Micro -----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component ATLAS_MICRO
	generic	(
				-- Configuration --
				MEM_SIZE_G      : natural := 1024; -- memory size in bytes
				SHARED_MEM_G    : boolean := TRUE; -- shared/distributed data/instruction memories
				BOOT_ADDRESS_G  : std_logic_vector(data_width_c-1 downto 0) := x"0000"  -- boot address
			);
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				SYS_MODE_O      : out std_logic; -- current processor operating mode

				-- Coprocesor Interface --
				USR_CP_EN_O     : out std_logic; -- access to cp0
				SYS_CP_EN_O     : out std_logic; -- access to cp1
				CP_OP_O         : out std_logic; -- data transfer/processing
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(8 downto 0); -- register addresses/cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data (OR-ed)

				-- External Interrupt Lines --
				IRQ0_I          : in  std_logic; -- external interrupt request line 0
				IRQ1_I          : in  std_logic  -- external interrupt request line 1
			);
  end component;

	-- Global Signals --
	signal G_CLK       : std_logic := '0';
	signal G_RST       : std_logic := '0';
	signal XINT0       : std_logic := '0';
	signal XINT1       : std_logic := '0';

begin

	-- Clock/Reset Generator -------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		G_CLK <= not G_CLK after 10 ns;
		G_RST <= '1', '0' after 35 ns;


	-- Stimulus --------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		STIMULUS: process
		begin
			XINT0 <= '0';
			XINT1 <= '0';
			wait for 40000 ns; -- wait for the end
		end process STIMULUS;


	-- Core ------------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		the_core_of_the_problem: ATLAS_MICRO
			generic map (
						-- Configuration --
						MEM_SIZE_G      => 1024,        -- internal memory size in bytes
						SHARED_MEM_G    => TRUE,        -- shared data/instruction memories
						BOOT_ADDRESS_G  => x"0000"      -- boot address
					)
			port map (
						-- Global Control --
						CLK_I           => G_CLK,       -- global clock line
						RST_I           => G_RST,       -- global reset line, sync, high-active
						SYS_MODE_O      => open,        -- current processor operating mode

						-- Coprocesor Interface --
						USR_CP_EN_O     => open,        -- access to cp0
						SYS_CP_EN_O     => open,        -- access to cp1
						CP_OP_O         => open,        -- data transfer/processing
						CP_RW_O         => open,        -- read/write access
						CP_CMD_O        => open,        -- register addresses/cmd
						CP_DAT_O        => open,        -- write data
						CP_DAT_I        => x"0000",     -- read data (OR-ed)

						-- External Interrupt Lines --
						IRQ0_I          => XINT0,       -- external interrupt request line 0
						IRQ1_I          => XINT1        -- external interrupt request line 1
					);



end micro_tb_structure;
