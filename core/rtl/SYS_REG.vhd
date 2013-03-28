-- ########################################################
-- #        << ATLAS Project - System Registers >>        #
-- # **************************************************** #
-- #  The main system register (MSR & PC) are located     #
-- #  here. Also the context control and interrupt        #
-- #  processing circuits are implemented within this     #
-- #  unit.                                               #
-- # **************************************************** #
-- #  Last modified: 26.03.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity SYS_REG is
-- ################################################################################################################
-- ##       Boot Address for Reset                                                                               ##
-- ################################################################################################################
	generic (
				BOOT_ADDRESS_G  : std_logic_vector(data_width_c-1 downto 0) := (others => '0') -- boot address
			);
	port	(
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################

				CLK_I           : in  std_logic; -- global clock line
				CE_I            : in  std_logic; -- clock enable
				RST_I           : in  std_logic; -- global reset line, sync, high-active

-- ###############################################################################################
-- ##           Function Control                                                                ##
-- ###############################################################################################

				EX_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- ex stage control
				MA_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- ma stage control
				EXT_INT_REQ0_I  : in  std_logic; -- external interrupt request 0
				EXT_INT_REQ1_I  : in  std_logic; -- external interrupt request 1

-- ###############################################################################################
-- ##           Data Input                                                                      ##
-- ###############################################################################################

				FLAG_BUS_I      : in  std_logic_vector(flag_bus_width_c-1 downto 0); -- flag input
				EXC_POS_I       : in  std_logic; -- exception would be possible
				STOP_PC         : in  std_logic; -- freeze pc
				PC_DATA_I       : in  std_logic_vector(data_width_c-1 downto 0); -- PC write data
				MSR_DATA_I      : in  std_logic_vector(data_width_c-1 downto 0); -- MSR write data

-- ###############################################################################################
-- ##           Data Output                                                                     ##
-- ###############################################################################################

				FLAG_BUS_O      : out std_logic_vector(flag_bus_width_c-1 downto 0); -- flag output
				VALID_BRANCH_O  : out std_logic; -- valid branch detected
				EXC_EXECUTED_O  : out std_logic; -- executed executed
				RD_MSR_O        : out std_logic_vector(data_width_c-1 downto 0); -- read data msr
				PC_O            : out std_logic_vector(data_width_c-1 downto 0); -- pc output
				PC_1D_O         : out std_logic_vector(data_width_c-1 downto 0); -- pc 1x delayed
				CP_PTC_O        : out std_logic; -- user coprocessor protection
				MODE_O          : out std_logic  -- current operating mode
			);
end SYS_REG;

architecture SR_STRUCTURE of SYS_REG is

	-- System Register --
	signal SYS_REG_PC      : std_logic_vector(data_width_c-1 downto 0);
	signal SYS_REG_MSR     : std_logic_vector(data_width_c-1 downto 0);
	signal PC_1D           : std_logic_vector(data_width_c-1 downto 0);

	-- Branch system --
	signal VALID_BRANCH    : std_logic;

	-- Interrupt System --
	signal INT_REQ         : std_logic;
	signal INT_VECTOR      : std_logic_vector(1 downto 0);
	signal XINT_SYNC       : std_logic_vector(1 downto 0);
	signal XINT_0_TAKEN    : std_logic;
	signal XINT_1_TAKEN    : std_logic;

begin

	-- External Interrupt Sychronizer ----------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		XI_SYNCHRONIZER: process(CLK_I)
			variable valid_int_req_v : std_logic;
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					XINT_SYNC <= (others => '0');
				elsif (CE_I = '1') then
					XINT_SYNC(0) <= EXT_INT_REQ0_I;
					XINT_SYNC(1) <= EXT_INT_REQ1_I;
				end if;
			end if;
		end process XI_SYNCHRONIZER;



	-- Exception Priority System ---------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		EXC_SYS: process(EX_CTRL_BUS_I, XINT_SYNC, EXC_POS_I, SYS_REG_MSR)
			variable xint0_valid_v : std_logic;
			variable xint1_valid_v : std_logic;
		begin
			-- external interrupt enable --
			-- => external int is possible AND int source is enabled AND global ints are enabled
			xint0_valid_v := EXC_POS_I and SYS_REG_MSR(msr_xint0_en_c) and SYS_REG_MSR(msr_xint_en_c);
			xint1_valid_v := EXC_POS_I and SYS_REG_MSR(msr_xint1_en_c) and SYS_REG_MSR(msr_xint_en_c);

			-- exception priority list and encoding --
			if ((xint0_valid_v = '1') and (XINT_SYNC(0) = '1')) then -- external interrupt 0
				INT_REQ    <= '1';
				INT_VECTOR <= "01"; -- ext int 0 vector
			elsif ((xint1_valid_v = '1') and (XINT_SYNC(1) = '1')) then -- external interrupt 1
				INT_REQ    <= '1';
				INT_VECTOR <= "10"; -- ext int 1 vector
			elsif ((EXC_POS_I = '1') and (EX_CTRL_BUS_I(ctrl_syscall_c) = '1')) then
			-- software interrupt: system call // msr/coprocessor access violation // undefined instruction
				INT_REQ    <= '1';
				INT_VECTOR <= "11"; -- sw int vector
			else -- no exception
				INT_REQ    <= '0';
				INT_VECTOR <= "00"; -- irrelevant [hw reset vector]
			end if;
		end process EXC_SYS;

		-- output to cycle manager --
		EXC_EXECUTED_O <= INT_REQ;



	-- System Register Update ------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		SR_UPDATE: process(CLK_I, SYS_REG_MSR, EX_CTRL_BUS_I)
			variable m_msr_acc_v : std_logic_vector(2 downto 0);
		begin
			-- manual msr access mode --
			m_msr_acc_v := SYS_REG_MSR(msr_mode_flag_c) & EX_CTRL_BUS_I(ctrl_msr_am_1_c downto ctrl_msr_am_0_c);

			-- sync update --
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					SYS_REG_PC                   <= BOOT_ADDRESS_G; -- boot address
					SYS_REG_MSR                  <= (others => '0');
					SYS_REG_MSR(msr_mode_flag_c) <= system_mode_c; -- we're the king after reset
				elsif (CE_I = '1') then -- clock enable

					-- Exception MSR Access -------------------------------------------------
					if (INT_REQ = '1') then -- switch to system mode
						SYS_REG_MSR(msr_mode_flag_c) <= system_mode_c; -- goto sytem mode
						SYS_REG_MSR(msr_xint_en_c)   <= '0'; -- clear global xint enable flag

					-- Manual MSR Access ----------------------------------------------------
					elsif (EX_CTRL_BUS_I(ctrl_en_c) = '1') then -- valid operation
						if (EX_CTRL_BUS_I(ctrl_msr_wr_c) = '1') then -- write operation
							case (m_msr_acc_v) is
								when "100" => -- system mode: full access
									SYS_REG_MSR <= MSR_DATA_I;
								when "101" => -- system mode: access all ALU flags
									SYS_REG_MSR(msr_usr_z_flag_c) <= MSR_DATA_I(msr_usr_z_flag_c);
									SYS_REG_MSR(msr_usr_c_flag_c) <= MSR_DATA_I(msr_usr_c_flag_c);
									SYS_REG_MSR(msr_usr_o_flag_c) <= MSR_DATA_I(msr_usr_o_flag_c);
									SYS_REG_MSR(msr_usr_n_flag_c) <= MSR_DATA_I(msr_usr_n_flag_c);
									SYS_REG_MSR(msr_usr_t_flag_c) <= MSR_DATA_I(msr_usr_t_flag_c);
									SYS_REG_MSR(msr_sys_z_flag_c) <= MSR_DATA_I(msr_sys_z_flag_c);
									SYS_REG_MSR(msr_sys_c_flag_c) <= MSR_DATA_I(msr_sys_c_flag_c);
									SYS_REG_MSR(msr_sys_o_flag_c) <= MSR_DATA_I(msr_sys_o_flag_c);
									SYS_REG_MSR(msr_sys_n_flag_c) <= MSR_DATA_I(msr_sys_n_flag_c);
									SYS_REG_MSR(msr_sys_t_flag_c) <= MSR_DATA_I(msr_sys_t_flag_c);
								when "110" => -- system mode: only access system ALU flags
									SYS_REG_MSR(msr_sys_z_flag_c) <= MSR_DATA_I(msr_sys_z_flag_c);
									SYS_REG_MSR(msr_sys_c_flag_c) <= MSR_DATA_I(msr_sys_c_flag_c);
									SYS_REG_MSR(msr_sys_o_flag_c) <= MSR_DATA_I(msr_sys_o_flag_c);
									SYS_REG_MSR(msr_sys_n_flag_c) <= MSR_DATA_I(msr_sys_n_flag_c);
									SYS_REG_MSR(msr_sys_t_flag_c) <= MSR_DATA_I(msr_sys_t_flag_c);
								when others => -- system/user mode: only access user ALU flags
									SYS_REG_MSR(msr_usr_z_flag_c) <= MSR_DATA_I(msr_usr_z_flag_c);
									SYS_REG_MSR(msr_usr_c_flag_c) <= MSR_DATA_I(msr_usr_c_flag_c);
									SYS_REG_MSR(msr_usr_o_flag_c) <= MSR_DATA_I(msr_usr_o_flag_c);
									SYS_REG_MSR(msr_usr_n_flag_c) <= MSR_DATA_I(msr_usr_n_flag_c);
									SYS_REG_MSR(msr_usr_t_flag_c) <= MSR_DATA_I(msr_usr_t_flag_c);								
							end case;

					-- Automatic MSR Access -------------------------------------------------
						elsif (EX_CTRL_BUS_I(ctrl_ctx_down_c) = '1') then -- context down switch?
							SYS_REG_MSR(msr_mode_flag_c) <= user_mode_c; -- go down to user mode
							if (SYS_REG_MSR(msr_mode_flag_c) = system_mode_c) then -- only in system mode!
								SYS_REG_MSR(msr_xint_en_c) <= EX_CTRL_BUS_I(ctrl_re_xint_c); -- auto re-enable global x_ints
							end if;
						else -- auto-update
							if (SYS_REG_MSR(msr_mode_flag_c) = user_mode_c) then -- user mode auto alu flag update
								if(EX_CTRL_BUS_I(ctrl_fupdate_c) = '1') then -- allow auto update of ALU flags
									SYS_REG_MSR(msr_usr_z_flag_c) <= FLAG_BUS_I(flag_z_c);
									SYS_REG_MSR(msr_usr_c_flag_c) <= FLAG_BUS_I(flag_c_c);
									SYS_REG_MSR(msr_usr_o_flag_c) <= FLAG_BUS_I(flag_o_c);
									SYS_REG_MSR(msr_usr_n_flag_c) <= FLAG_BUS_I(flag_n_c);
								end if;
								if (EX_CTRL_BUS_I(ctrl_tf_store_c) = '1') then -- allow user mode update of T-flag
									SYS_REG_MSR(msr_usr_t_flag_c) <= FLAG_BUS_I(flag_t_c);
								end if;
							else -- system mode auto alu flag update
								if(EX_CTRL_BUS_I(ctrl_fupdate_c) = '1') then -- allow system mode auto update of ALU flags
									SYS_REG_MSR(msr_sys_z_flag_c) <= FLAG_BUS_I(flag_z_c);
									SYS_REG_MSR(msr_sys_c_flag_c) <= FLAG_BUS_I(flag_c_c);
									SYS_REG_MSR(msr_sys_o_flag_c) <= FLAG_BUS_I(flag_o_c);
									SYS_REG_MSR(msr_sys_n_flag_c) <= FLAG_BUS_I(flag_n_c);
								end if;
								if (EX_CTRL_BUS_I(ctrl_tf_store_c) = '1') then -- allow system mode update of T-flag
									SYS_REG_MSR(msr_sys_t_flag_c) <= FLAG_BUS_I(flag_t_c);
								end if;
							end if;
						end if;
					end if;

					-- Exception PC Access --------------------------------------------------
					if (INT_REQ = '1') then
						SYS_REG_PC <= (others => '0');
						SYS_REG_PC(2 downto 1) <= INT_VECTOR;

					-- Manual/Branch PC Access ----------------------------------------------
					elsif (VALID_BRANCH = '1') or ((EX_CTRL_BUS_I(ctrl_en_c) = '1') and (EX_CTRL_BUS_I(ctrl_ctx_down_c) = '1')) then -- valid automatic/manual update/goto user mode
						SYS_REG_PC <= PC_DATA_I;

					-- Automatic PC Update --------------------------------------------------
					elsif (STOP_PC = '0') then -- update instruction address
						if (word_mode_en_c = false) then -- byte-addressed memory
							SYS_REG_PC <= Std_Logic_Vector(unsigned(SYS_REG_PC) + 2); -- byte increment
						else -- word-addressed memory
							SYS_REG_PC <= Std_Logic_Vector(unsigned(SYS_REG_PC) + 1); -- word increment
						end if;
					end if;

				end if;
			end if;
		end process SR_UPDATE;



	-- MSR Flag Output -------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		FLAG_BUS_O(flag_z_c) <= SYS_REG_MSR(msr_usr_z_flag_c) when (SYS_REG_MSR(msr_mode_flag_c) = user_mode_c) else SYS_REG_MSR(msr_sys_z_flag_c);
		FLAG_BUS_O(flag_c_c) <= SYS_REG_MSR(msr_usr_c_flag_c) when (SYS_REG_MSR(msr_mode_flag_c) = user_mode_c) else SYS_REG_MSR(msr_sys_c_flag_c);
		FLAG_BUS_O(flag_o_c) <= SYS_REG_MSR(msr_usr_o_flag_c) when (SYS_REG_MSR(msr_mode_flag_c) = user_mode_c) else SYS_REG_MSR(msr_sys_o_flag_c);
		FLAG_BUS_O(flag_n_c) <= SYS_REG_MSR(msr_usr_n_flag_c) when (SYS_REG_MSR(msr_mode_flag_c) = user_mode_c) else SYS_REG_MSR(msr_sys_n_flag_c);
		FLAG_BUS_O(flag_t_c) <= SYS_REG_MSR(msr_usr_t_flag_c) when (SYS_REG_MSR(msr_mode_flag_c) = user_mode_c) else SYS_REG_MSR(msr_sys_t_flag_c);

		-- Special Flag output --
		CP_PTC_O <= SYS_REG_MSR(msr_usr_cp_ptc_c); -- user coprocessor protection
		MODE_O   <= SYS_REG_MSR(msr_mode_flag_c); -- current operating mode



	-- MSR Data-Read Access --------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MSR_RD_ACC: process(MA_CTRL_BUS_I, SYS_REG_MSR)
			variable msr_r_mode_v : std_logic_vector(2 downto 0);
		begin
			msr_r_mode_v := SYS_REG_MSR(msr_mode_flag_c) & MA_CTRL_BUS_I(ctrl_msr_am_1_c downto ctrl_msr_am_0_c);
			RD_MSR_O <= (others => '0');
			case (msr_r_mode_v) is
				when "100" => -- system mode: full read access
					RD_MSR_O <= SYS_REG_MSR;
				when "101" => -- system mode: only read all ALU flags
					RD_MSR_O(msr_sys_z_flag_c) <= SYS_REG_MSR(msr_sys_z_flag_c);
					RD_MSR_O(msr_sys_c_flag_c) <= SYS_REG_MSR(msr_sys_c_flag_c);
					RD_MSR_O(msr_sys_o_flag_c) <= SYS_REG_MSR(msr_sys_o_flag_c);
					RD_MSR_O(msr_sys_n_flag_c) <= SYS_REG_MSR(msr_sys_n_flag_c);
					RD_MSR_O(msr_sys_t_flag_c) <= SYS_REG_MSR(msr_sys_t_flag_c);
					RD_MSR_O(msr_usr_z_flag_c) <= SYS_REG_MSR(msr_usr_z_flag_c);
					RD_MSR_O(msr_usr_c_flag_c) <= SYS_REG_MSR(msr_usr_c_flag_c);
					RD_MSR_O(msr_usr_o_flag_c) <= SYS_REG_MSR(msr_usr_o_flag_c);
					RD_MSR_O(msr_usr_n_flag_c) <= SYS_REG_MSR(msr_usr_n_flag_c);
					RD_MSR_O(msr_usr_t_flag_c) <= SYS_REG_MSR(msr_usr_t_flag_c);
				when "110" => -- system mode: only read system ALU flags
					RD_MSR_O(msr_sys_z_flag_c) <= SYS_REG_MSR(msr_sys_z_flag_c);
					RD_MSR_O(msr_sys_c_flag_c) <= SYS_REG_MSR(msr_sys_c_flag_c);
					RD_MSR_O(msr_sys_o_flag_c) <= SYS_REG_MSR(msr_sys_o_flag_c);
					RD_MSR_O(msr_sys_n_flag_c) <= SYS_REG_MSR(msr_sys_n_flag_c);
					RD_MSR_O(msr_sys_t_flag_c) <= SYS_REG_MSR(msr_sys_t_flag_c);
				when others => -- system/user mode: only read user ALU flags
					RD_MSR_O(msr_usr_z_flag_c) <= SYS_REG_MSR(msr_usr_z_flag_c);
					RD_MSR_O(msr_usr_c_flag_c) <= SYS_REG_MSR(msr_usr_c_flag_c);
					RD_MSR_O(msr_usr_o_flag_c) <= SYS_REG_MSR(msr_usr_o_flag_c);
					RD_MSR_O(msr_usr_n_flag_c) <= SYS_REG_MSR(msr_usr_n_flag_c);
					RD_MSR_O(msr_usr_t_flag_c) <= SYS_REG_MSR(msr_usr_t_flag_c);
			end case;
		end process MSR_RD_ACC;



	-- PC Output -------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		PC_DELAY: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					PC_1D <= (others => '0');
				elsif (STOP_PC = '0') and (CE_I = '1') then
					PC_1D <= SYS_REG_PC;
				end if;
			end if;
		end process PC_DELAY;

		-- PC outputs --
		PC_O    <= SYS_REG_PC; -- direct output
		PC_1D_O <= PC_1D;      -- 1x delayed



	-- Branch Detector -------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		BRANCH_DETECTOR: process(EX_CTRL_BUS_I, SYS_REG_MSR, INT_REQ)
			variable z_v, c_v, o_v, n_v, t_v : std_logic;
			variable valid_v                 : std_logic;
			variable valid_branch_v          : std_logic;
			variable manual_branch_v         : std_logic;
		begin

			-- flag isolation --
			if (SYS_REG_MSR(msr_mode_flag_c) = user_mode_c) then -- user mode
				z_v := SYS_REG_MSR(msr_usr_z_flag_c);
				c_v := SYS_REG_MSR(msr_usr_c_flag_c);
				o_v := SYS_REG_MSR(msr_usr_o_flag_c);
				n_v := SYS_REG_MSR(msr_usr_n_flag_c);
				t_v := SYS_REG_MSR(msr_usr_t_flag_c);
			else -- system mode
				z_v := SYS_REG_MSR(msr_sys_z_flag_c);
				c_v := SYS_REG_MSR(msr_sys_c_flag_c);
				o_v := SYS_REG_MSR(msr_sys_o_flag_c);
				n_v := SYS_REG_MSR(msr_sys_n_flag_c);
				t_v := SYS_REG_MSR(msr_sys_t_flag_c);
			end if;

			-- condition check --
			case (EX_CTRL_BUS_I(ctrl_cond_3_c downto ctrl_cond_0_c)) is
				when cond_eq_c => valid_v := z_v;                          -- equal
				when cond_ne_c => valid_v := not z_v;                      -- not equal
				when cond_cs_c => valid_v := c_v;                          -- unsigned higher or same
				when cond_cc_c => valid_v := not c_v;                      -- unsigned lower
				when cond_mi_c => valid_v := n_v;                          -- negative
				when cond_pl_c => valid_v := not n_v;                      -- positive or zero
				when cond_os_c => valid_v := o_v;                          -- overflow
				when cond_oc_c => valid_v := not o_v;                      -- no overflow
				when cond_hi_c => valid_v := c_v and (not z_v);            -- unisgned higher
				when cond_ls_c => valid_v := (not c_v) and z_v;            -- unsigned lower or same
				when cond_ge_c => valid_v := n_v xnor o_v;                 -- greater than or equal
				when cond_lt_c => valid_v := n_v xor o_v;                  -- less than
				when cond_gt_c => valid_v := (not z_v) and (n_v xnor o_v); -- greater than
				when cond_le_c => valid_v := z_v or (n_v xor o_v);         -- less than or equal
				when cond_ts_c => valid_v := t_v;                          -- transfer set
				when cond_al_c => valid_v := '1';                          -- always
				when others    => valid_v := '0';                          -- undefined
			end case;

			-- Manual branch? --
			manual_branch_v := EX_CTRL_BUS_I(ctrl_pc_wr_c);

			-- Valid branch command? --
			valid_branch_v := EX_CTRL_BUS_I(ctrl_en_c) and ((EX_CTRL_BUS_I(ctrl_branch_c) and valid_v) or manual_branch_v);

			-- Output to cycle arbiter --
			VALID_BRANCH   <= valid_branch_v;-- or INT_REQ; -- internal signal, no INT_REQ since it is redundant
			VALID_BRANCH_O <= valid_branch_v or INT_REQ; -- external signal

		end process BRANCH_DETECTOR;




end SR_STRUCTURE;
