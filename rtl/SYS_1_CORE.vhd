-- #########################################################
-- #       << ATLAS Project - System Controller 1 >>       #
-- # ***************************************************** #
-- #  -> Memory Management Unit                            #
-- #  -> Clock Information                                 #
-- # ***************************************************** #
-- #  Last modified: 07.05.2014                            #
-- # ***************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany            #
-- #########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity SYS_1_CORE is
-- ###############################################################################################
-- ##       Clock Speed Configuration                                                           ##
-- ###############################################################################################
	generic (
				CLK_SPEED_G     : std_logic_vector(31 downto 0) := (others => '0') -- clock speed (in Hz)
			);
	port	(
-- ###############################################################################################
-- ##           Host Interface                                                                  ##
-- ###############################################################################################

				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(15 downto 0); -- write data
				DAT_O           : out std_logic_vector(15 downto 0); -- read data

				SYS_MODE_I      : in  std_logic; -- current operating mode
				INT_EXE_I       : in  std_logic; -- interrupt beeing executed

-- ###############################################################################################
-- ##           Memory Interface                                                                ##
-- ###############################################################################################

				MEM_IP_ADR_O    : out std_logic_vector(15 downto 0); -- instruction page
				MEM_DP_ADR_O    : out std_logic_vector(15 downto 0)  -- data page
			);
end SYS_1_CORE;

architecture SYS_1_CORE_BEHAV of SYS_1_CORE is

	-- Register addresses --
	constant mmu_irq_base_c     : std_logic_vector(02 downto 0) := "000"; -- R/W: base page for IRQs
	constant mmu_sys_i_page_c   : std_logic_vector(02 downto 0) := "001"; -- R/W: system mode I page
	constant mmu_sys_d_page_c   : std_logic_vector(02 downto 0) := "010"; -- R/W: system mode D page
	constant mmu_usr_i_page_c   : std_logic_vector(02 downto 0) := "011"; -- R/W: user mode I page
	constant mmu_usr_d_page_c   : std_logic_vector(02 downto 0) := "100"; -- R/W: user mode D page
	constant mmu_i_page_link_c  : std_logic_vector(02 downto 0) := "101"; -- R:   linked i page
	constant mmu_d_page_link_c  : std_logic_vector(02 downto 0) := "110"; -- R:   linked d page
	constant mmu_sys_info_c     : std_logic_vector(02 downto 0) := "111"; -- R:   system info
	-- Sys info register (uses auto-pointer):
	-- 1st read access: clock speed LOW
	-- 2nd read access: clock speed HIGH

	-- Registers --
	signal MMU_IRQ_BASE         : std_logic_vector(15 downto 0);
	signal MMU_SYS_I_PAGE       : std_logic_vector(15 downto 0);
	signal MMU_SYS_D_PAGE       : std_logic_vector(15 downto 0);
	signal MMU_USR_I_PAGE       : std_logic_vector(15 downto 0);
	signal MMU_USR_D_PAGE       : std_logic_vector(15 downto 0);
	signal MMU_I_PAGE_LINK      : std_logic_vector(15 downto 0);
	signal MMU_D_PAGE_LINK      : std_logic_vector(15 downto 0);

	-- Buffers / Local signals --
	signal I_SYS_TMP, I_USR_TMP : std_logic_vector(15 downto 0);
	signal D_SYS_TMP, D_USR_TMP : std_logic_vector(15 downto 0);
	signal MODE_BUF             : std_logic_vector(01 downto 0);
	signal SYS_INFO             : std_logic_vector(15 downto 0);
	signal SYS_INFO_ADR         : std_logic_vector(01 downto 0);

begin

	-- MMU Register Update ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MMU_REG_UP: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					MMU_IRQ_BASE    <= start_page_c; -- (others => '0');
					MMU_SYS_I_PAGE  <= start_page_c;
					MMU_SYS_D_PAGE  <= start_page_c;
					MMU_USR_I_PAGE  <= start_page_c; -- (others => '0');
					MMU_USR_D_PAGE  <= start_page_c; -- (others => '0');
					MMU_I_PAGE_LINK <= start_page_c; -- (others => '0');
					MMU_D_PAGE_LINK <= start_page_c; -- (others => '0');
					I_SYS_TMP       <= start_page_c;
					D_SYS_TMP       <= start_page_c;
					I_USR_TMP       <= start_page_c; -- (others => '0');
					D_USR_TMP       <= start_page_c; -- (others => '0');
					MODE_BUF        <= system_mode_c & system_mode_c; -- start in system mode
				elsif (ICE_I = '1') then

					-- Auto update --
					MODE_BUF  <= MODE_BUF(0) & SYS_MODE_I;
					I_SYS_TMP <= MMU_SYS_I_PAGE;
					D_SYS_TMP <= MMU_SYS_D_PAGE;
					I_USR_TMP <= MMU_USR_I_PAGE;
					D_USR_TMP <= MMU_USR_D_PAGE;

					-- Exception Processing ----------------------------------------------------------
					-- ----------------------------------------------------------------------------------
					if (INT_EXE_I = '1') then
						MMU_SYS_I_PAGE <= MMU_IRQ_BASE; -- system-mode base page for IRQs
						MMU_SYS_D_PAGE <= MMU_IRQ_BASE; -- system-mode base page for IRQs
						I_SYS_TMP      <= MMU_IRQ_BASE; -- system-mode base page for IRQs
						D_SYS_TMP      <= MMU_IRQ_BASE; -- system-mode base page for IRQs
						if (MODE_BUF(1) = user_mode_c) then -- we were in USR mode
							MMU_I_PAGE_LINK <= I_USR_TMP; -- save current sys i-page
							MMU_D_PAGE_LINK <= D_USR_TMP; -- save current sys d-page
						else -- we were in SYS mode
							MMU_I_PAGE_LINK <= I_SYS_TMP; -- save current sys i-page
							MMU_D_PAGE_LINK <= D_SYS_TMP; -- save current sys d-page
						end if;

					-- Data Transfer -----------------------------------------------------------------
					-- ----------------------------------------------------------------------------------
					elsif (W_EN_I = '1') then -- valid write
						case (ADR_I) is
							when mmu_irq_base_c    => MMU_IRQ_BASE    <= DAT_I; -- system-mode base page
							when mmu_sys_i_page_c  => MMU_SYS_I_PAGE  <= DAT_I; -- system instruction page
							when mmu_sys_d_page_c  => MMU_SYS_D_PAGE  <= DAT_I; -- system data page
							when mmu_usr_i_page_c  => MMU_USR_I_PAGE  <= DAT_I; -- user instruction page
							when mmu_usr_d_page_c  => MMU_USR_D_PAGE  <= DAT_I; -- user data page
--							when mmu_i_page_link_c => MMU_I_PAGE_LINK <= DAT_I; -- instruction page link
--							when mmu_d_page_link_c => MMU_D_PAGE_LINK <= DAT_I; -- data page link
							when others            => NULL; -- do nothing
						end case;
					end if;
				end if;
			end if;
		end process MMU_REG_UP;

		-- Page Output --
		MEM_IP_ADR_O <= I_USR_TMP when (SYS_MODE_I = user_mode_c) else I_SYS_TMP;
		MEM_DP_ADR_O <= D_USR_TMP when (SYS_MODE_I = user_mode_c) else D_SYS_TMP;



	-- MMU Read Access -------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, MMU_IRQ_BASE, MMU_SYS_I_PAGE, MMU_SYS_D_PAGE, MMU_USR_I_PAGE,
		               MMU_USR_D_PAGE, MMU_I_PAGE_LINK, MMU_D_PAGE_LINK, SYS_INFO)
		begin
			case (ADR_I) is
				when mmu_irq_base_c    => DAT_O <= MMU_IRQ_BASE;    -- system-mode base page
				when mmu_sys_i_page_c  => DAT_O <= MMU_SYS_I_PAGE;  -- system instruction page
				when mmu_sys_d_page_c  => DAT_O <= MMU_SYS_D_PAGE;  -- system data page
				when mmu_usr_i_page_c  => DAT_O <= MMU_USR_I_PAGE;  -- user instruction page
				when mmu_usr_d_page_c  => DAT_O <= MMU_USR_D_PAGE;  -- user data page
				when mmu_i_page_link_c => DAT_O <= MMU_I_PAGE_LINK; -- instruction page link
				when mmu_d_page_link_c => DAT_O <= MMU_D_PAGE_LINK; -- data page link
				when mmu_sys_info_c    => DAT_O <= SYS_INFO;        -- system info
				when others            => DAT_O <= (others => '0'); -- dummy output
			end case;
		end process R_ACC;



	-- System Info Output Control --------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		SYS_INFO_CTRL: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					SYS_INFO_ADR <= (others => '0');
				elsif (R_EN_I = '1') and (ICE_I = '1') and (ADR_I = mmu_sys_info_c) then
					SYS_INFO_ADR <= std_logic_vector(unsigned(SYS_INFO_ADR) + 1);
				end if;
			end if;
		end process SYS_INFO_CTRL;

		-- Output selector --
		SYS_INFO_OUT: process(SYS_INFO_ADR)
		begin
			case (SYS_INFO_ADR) is
				when "00" => SYS_INFO <= CLK_SPEED_G(15 downto 00);
				when "01" => SYS_INFO <= CLK_SPEED_G(31 downto 16);
				when "10" => SYS_INFO <= CLK_SPEED_G(15 downto 00);
				when "11" => SYS_INFO <= CLK_SPEED_G(31 downto 16);
				when others => SYS_INFO <= (others => '0');
			end case;
		end process SYS_INFO_OUT;



end SYS_1_CORE_BEHAV;
