-- ########################################################
-- #             << ATLAS Project - MMU >>                #
-- # **************************************************** #
-- #  The memory management unit allows to extend the     #
-- #  accessible memory/IO space to up to 4GB.            #
-- #  Base registers generate the most significant 16-bit #
-- #  of a true 32-bit addressable system.                #
-- # **************************************************** #
-- #  Last modified: 05.06.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity MMU is
-- ################################################################################################################
-- ##       Boot Page for Reset                                                                                  ##
-- ################################################################################################################
	generic (
				BOOT_PAGE_G     : std_logic_vector(data_width_c-1 downto 0) := (others => '0') -- boot address
			);
	port	(
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################

				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				HALT_I          : in  std_logic; -- inverted clock enable

-- ###############################################################################################
-- ##           Processor Interface                                                             ##
-- ###############################################################################################

				CP_EN_I         : in  std_logic; -- access coprocessor
				CP_OP_I         : in  std_logic; -- data transfer/processing
				CP_RW_I         : in  std_logic; -- read/write access
				CP_CMD_I        : in  std_logic_vector(cp_cmd_width_c-1 downto 0); -- register addresses / cmd
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- read data
				SYS_MODE_I      : in  std_logic; -- current operating mode
				INT_EXE_I       : in  std_logic; -- interrupt beeing executed
				MMU_IRQ_O       : out std_logic; -- mmu interrupt request

-- ###############################################################################################
-- ##           Bus Unit Interface                                                              ##
-- ###############################################################################################

				CACHE_ERROR_I   : in  std_logic; -- bus access error
				CACHE_SYNC_I    : in  std_logic; -- cache is sync
				CACHE_CLR_O     : out std_logic; -- reload cache
				CACHE_FLUSH_O   : out std_logic; -- synchronize cache with mem
				MEM_DIR_ACC_O   : out std_logic; -- direct access (bypass cache)
				MEM_IP_ADR_O    : out std_logic_vector(data_width_c-1 downto 0); -- instruction page
				MEM_DP_ADR_O    : out std_logic_vector(data_width_c-1 downto 0)  -- data page
			);
end MMU;

architecture MMU_STRUCTURE of MMU is

	-- Registers --
	signal MMU_CTRL             : std_logic_vector(15 downto 0); -- r0: control register
	signal MMU_SCRATCH          : std_logic_vector(15 downto 0); -- r1: scratch register
	signal MMU_SYS_I_PAGE       : std_logic_vector(15 downto 0); -- r2: system mode instruction page
	signal MMU_SYS_D_PAGE       : std_logic_vector(15 downto 0); -- r3: system mode data page
	signal MMU_USR_I_PAGE       : std_logic_vector(15 downto 0); -- r4: user mode instruction page
	signal MMU_USR_D_PAGE       : std_logic_vector(15 downto 0); -- r5: user mode data page
	signal MMU_I_PAGE_LINK      : std_logic_vector(15 downto 0); -- r6: instruction link page
	signal MMU_D_PAGE_LINK      : std_logic_vector(15 downto 0); -- r7: data link page

	-- Control register bits --
	constant mmu_ctrl_cflush_c  : natural := 0; -- w: flush cache
	constant mmu_ctrl_cclr_c    : natural := 1; -- w: clear cache
	constant mmu_ctrl_da_c      : natural := 2; -- r/w: direct access
	constant mmu_ctrl_csync_c   : natural := 3; -- r: cache is sync
	constant mmu_ctrl_bus_err_c : natural := 4; -- r/w: bus error interrupt/ack
	constant mmu_ctrl_ccx_en_c  : natural := 5; -- r/w: enable automatic page switch on irq

	-- Commands --
	-- applied on any register
	constant cmd_flush_cache_c  : std_logic_vector(2 downto 0) := "000"; -- 0: flush cache
	constant cmd_clear_cache_c  : std_logic_vector(2 downto 0) := "001"; -- 1: reload cache
	constant cmd_en_dir_acc_c   : std_logic_vector(2 downto 0) := "010"; -- 2: enable direct access
	constant cmd_dis_dir_acc_c  : std_logic_vector(2 downto 0) := "011"; -- 3: disable diect access
	constant cmd_ack_bus_err_c  : std_logic_vector(2 downto 0) := "100"; -- 4: acknowledge bus error interupt
	constant cmd_link_copy_c    : std_logic_vector(2 downto 0) := "101"; -- 5: reload last accessed system pages

begin

	-- MMU Register Update ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MMU_REG_UP: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					MMU_CTRL        <= (others => '0');
					MMU_SCRATCH     <= (others => '0');
					MMU_SYS_I_PAGE  <= BOOT_PAGE_G;
					MMU_SYS_D_PAGE  <= BOOT_PAGE_G;
					MMU_USR_I_PAGE  <= (others => '0');
					MMU_USR_D_PAGE  <= (others => '0');
					MMU_I_PAGE_LINK <= (others => '0');
					MMU_D_PAGE_LINK <= (others => '0');
					MEM_IP_ADR_O    <= BOOT_PAGE_G;
					MEM_DP_ADR_O    <= BOOT_PAGE_G;
					CP_DAT_O        <= (others => '0');
				else--if (HALT_I = '0') then

					-- Defaults --
					CP_DAT_O <= (others => '0');
					MMU_CTRL(mmu_ctrl_csync_c)   <= CACHE_SYNC_I;
					MMU_CTRL(mmu_ctrl_bus_err_c) <= MMU_CTRL(mmu_ctrl_bus_err_c) or CACHE_ERROR_I;

					-- Exception Processing ----------------------------------------------------------
					-- ----------------------------------------------------------------------------------
					if (INT_EXE_I = '1') and (MMU_CTRL(mmu_ctrl_ccx_en_c) = '1') then
						MMU_SYS_I_PAGE              <= (others => '0');  -- i-page zero
						MMU_SYS_D_PAGE              <= (others => '0');  -- d-page zero
						MEM_IP_ADR_O                <= (others => '0');  -- i-page zero
						MEM_DP_ADR_O                <= (others => '0');  -- d-page zero
						MMU_I_PAGE_LINK             <= MMU_SYS_I_PAGE;   -- save current sys i-page
						MMU_D_PAGE_LINK             <= MMU_SYS_D_PAGE;   -- save current sys d-page

					-- Data Transfer / CMD Procesing -------------------------------------------------
					-- ----------------------------------------------------------------------------------
					elsif (CP_EN_I = '1') then
						if (CP_OP_I = '1') then -- Data Transfer
						-- --------------------------------------------------------
							if (CP_RW_I = '1') then -- valid write
								case (CP_CMD_I(cp_op_a_msb_c downto cp_op_a_lsb_c)) is
									when "000"  => MMU_CTRL        <= CP_DAT_I; -- control register
									when "001"  => MMU_SCRATCH     <= CP_DAT_I; -- scratch register
									when "010"  => MMU_SYS_I_PAGE  <= CP_DAT_I; -- system instruction page
									when "011"  => MMU_SYS_D_PAGE  <= CP_DAT_I; -- system data page
									when "100"  => MMU_USR_I_PAGE  <= CP_DAT_I; -- user instruction page
									when "101"  => MMU_USR_D_PAGE  <= CP_DAT_I; -- user data page
									when "110"  => MMU_I_PAGE_LINK <= CP_DAT_I; -- instruction page link
									when "111"  => MMU_D_PAGE_LINK <= CP_DAT_I; -- data page link
									when others => NULL; -- do nothing
								end case;
							else -- valid read
								case (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c)) is
									when "000"  => CP_DAT_O <= MMU_CTRL;        -- control register
									when "001"  => CP_DAT_O <= MMU_SCRATCH;     -- scratch register
									when "010"  => CP_DAT_O <= MMU_SYS_I_PAGE;  -- system instruction page
									when "011"  => CP_DAT_O <= MMU_SYS_D_PAGE;  -- system data page
									when "100"  => CP_DAT_O <= MMU_USR_I_PAGE;  -- user instruction page
									when "101"  => CP_DAT_O <= MMU_USR_D_PAGE;  -- user data page
									when "110"  => CP_DAT_O <= MMU_I_PAGE_LINK; -- instruction page link
									when "111"  => CP_DAT_O <= MMU_D_PAGE_LINK; -- data page link
									when others => CP_DAT_O <= (others => '0'); -- dummy output
								end case;
							end if;
						else -- Command Processing
						-- --------------------------------------------------------
							case CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c) is
								when cmd_flush_cache_c => MMU_CTRL(mmu_ctrl_cflush_c)  <= '1'; -- flush cache
								when cmd_clear_cache_c => MMU_CTRL(mmu_ctrl_cclr_c)    <= '1'; -- clear cache
								when cmd_en_dir_acc_c  => MMU_CTRL(mmu_ctrl_da_c)      <= '1'; -- enable direct access
								when cmd_dis_dir_acc_c => MMU_CTRL(mmu_ctrl_da_c)      <= '0'; -- disable direct access
								when cmd_ack_bus_err_c => MMU_CTRL(mmu_ctrl_bus_err_c) <= '0'; -- ack bus error
								when cmd_link_copy_c   => MMU_SYS_I_PAGE               <= MMU_I_PAGE_LINK; -- reload old i-page
								                          MMU_SYS_D_PAGE               <= MMU_D_PAGE_LINK; -- reload old d-page
								when others => NULL; -- undefined operation
							end case;
						end if;

					-- Auto Update -------------------------------------------------------------------
					-- ----------------------------------------------------------------------------------
					else
						-- Control Register --
						MMU_CTRL(mmu_ctrl_csync_c)   <= CACHE_SYNC_I;
						MMU_CTRL(mmu_ctrl_cflush_c)  <= '0'; -- auto clear cache flush bit
						MMU_CTRL(mmu_ctrl_cclr_c)    <= '0'; -- auto clear cache clear bit
						MMU_CTRL(mmu_ctrl_bus_err_c) <= MMU_CTRL(mmu_ctrl_bus_err_c) or CACHE_ERROR_I;

						-- Page update --
						if (SYS_MODE_I = user_mode_c) then
							MEM_IP_ADR_O <= MMU_USR_I_PAGE;
							MEM_DP_ADR_O <= MMU_USR_D_PAGE;
						else
							MEM_IP_ADR_O <= MMU_SYS_I_PAGE;
							MEM_DP_ADR_O <= MMU_SYS_D_PAGE;
						end if;
					end if;

				end if;
			end if;
		end process MMU_REG_UP;

		-- Interrupt Request --
		MMU_IRQ_O     <= MMU_CTRL(mmu_ctrl_bus_err_c);

		-- Cache control --
		CACHE_CLR_O   <= MMU_CTRL(mmu_ctrl_cclr_c);
		CACHE_FLUSH_O <= MMU_CTRL(mmu_ctrl_cflush_c);
		MEM_DIR_ACC_O <= MMU_CTRL(mmu_ctrl_da_c);



end MMU_STRUCTURE;
