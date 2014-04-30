-- ########################################################
-- #    << ATLAS Project - Arithmetical/Logical Unit >>   #
-- # **************************************************** #
-- #  The main data processing is done here. Also the CP  #
-- #  interface emerges from this unit.                   #
-- # **************************************************** #
-- #  Last modified: 30.04.2014                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity ALU is
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

				EX_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- stage control
				FLAG_BUS_I      : in  std_logic_vector(flag_bus_width_c-1 downto 0); -- flag input

-- ###############################################################################################
-- ##           Data Input                                                                      ##
-- ###############################################################################################

				OP_A_I          : in  std_logic_vector(data_width_c-1 downto 0); -- operand A input
				OP_B_I          : in  std_logic_vector(data_width_c-1 downto 0); -- operand B input
				OP_C_I          : in  std_logic_vector(data_width_c-1 downto 0); -- operand C input

				PC_1D_I         : in  std_logic_vector(data_width_c-1 downto 0); -- 1x delayed PC

				MA_FWD_I        : in  std_logic_vector(fwd_width_c-1  downto 0); -- MA stage forwarding path
				WB_FWD_I        : in  std_logic_vector(fwd_width_c-1  downto 0); -- WB stage forwarding path

-- ###############################################################################################
-- ##           Data Output                                                                     ##
-- ###############################################################################################

				FLAG_BUS_O      : out std_logic_vector(flag_bus_width_c-1 downto 0); -- flag output
				MASK_T_FLAG_O   : out std_logic; -- T-Flag for mask generation

				MSR_DATA_O      : out std_logic_vector(data_width_c-1 downto 0); -- MSR write data
				ALU_RES_O       : out std_logic_vector(data_width_c-1 downto 0); -- ALU result
				MUL_RES_O       : out std_logic_vector(2*data_width_c-1 downto 0); -- MUL result
				BP_OPA_O        : out std_logic_vector(data_width_c-1 downto 0); -- operand A bypass
				BP_OPC_O        : out std_logic_vector(data_width_c-1 downto 0); -- operand C bypass

				CP_CP0_EN_O     : out std_logic; -- access to cp0
				CP_CP1_EN_O     : out std_logic; -- access to cp1
				CP_OP_O         : out std_logic; -- data transfer/operation
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(cp_cmd_width_c-1 downto 0); -- register addresses / cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data

				MEM_REQ_O       : out std_logic -- data memory access request for next cycle
			);
end ALU;

architecture ALU_STRUCTURE of ALU is

	-- Pipeline register --
	signal OP_A_FF      : std_logic_vector(data_width_c-1 downto 0);
	signal OP_B_FF      : std_logic_vector(data_width_c-1 downto 0);
	signal OP_C_FF      : std_logic_vector(data_width_c-1 downto 0);

	-- Functional Units Output --
	signal FU_ARITH_RES : std_logic_vector(data_width_c-1 downto 0);
	signal FU_ARITH_FLG : std_logic_vector(1 downto 0); -- overflow & carry
	signal FU_LOGIC_RES : std_logic_vector(data_width_c-1 downto 0);
	signal FU_LOGIC_FLG : std_logic_vector(1 downto 0);
	signal FU_SHIFT_RES : std_logic_vector(data_width_c-1 downto 0);
	signal FU_SHIFT_FLG : std_logic_vector(1 downto 0);

	-- Internal data lines  --
	signal OP_A_INT     : std_logic_vector(data_width_c-1 downto 0);
	signal OP_B_INT     : std_logic_vector(data_width_c-1 downto 0);
	signal OP_C_INT     : std_logic_vector(data_width_c-1 downto 0);
	signal ALU_RES_INT  : std_logic_vector(data_width_c-1 downto 0);
	signal T_FLAG_FUNC  : std_logic;
	signal PARITY_BIT   : std_logic;
	signal TRANSF_INT   : std_logic;
	signal SEL_BIT      : std_logic;
	signal INV_BIT      : std_logic;
	signal IS_ZERO      : std_logic;
	signal EXTND_ZERO   : std_logic;

	-- Multiplier --
	signal MUL_OP_A     : std_logic_vector(data_width_c-1 downto 0);
	signal MUL_OP_B     : std_logic_vector(data_width_c-1 downto 0);

begin

	-- Pipeline Register -----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		PIPE_REG: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					OP_A_FF <= (others => '0');
					OP_B_FF <= (others => '0');
					op_c_ff <= (others => '0');
				elsif (CE_I = '1') then
					OP_A_FF <= OP_A_I;
					OP_B_FF <= OP_B_I;
					OP_C_FF <= OP_C_I;
				end if;
			end if;
		end process PIPE_REG;



	-- Execution Forwarding Unit ---------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		EX_FWD: process(MA_FWD_I, WB_FWD_I, EX_CTRL_BUS_I, OP_A_FF, OP_B_FF, OP_C_FF)
			variable op_a_ma_match_v : std_logic;
			variable op_b_ma_match_v : std_logic;
			variable op_a_wb_match_v : std_logic;
			variable op_b_wb_match_v : std_logic;
			variable op_c_wb_match_v : std_logic;
			variable op_a_tmp_v      : std_logic_vector(data_width_c-1 downto 0);
		begin

			-- Data from early stages -> higher priority than data from later stages
			-- No forwarding when OP_A is the PC
			-- No forwarding when OP_B is an immediate

			-- Local data dependency detectors --
			op_a_ma_match_v := '0';
			if (MA_FWD_I(fwd_en_c) = '1') and (EX_CTRL_BUS_I(ctrl_ra_is_pc_c) = '0')  and (EX_CTRL_BUS_I(ctrl_ra_3_c downto ctrl_ra_0_c) = MA_FWD_I(fwd_adr_3_c downto fwd_adr_0_c)) then
				op_a_ma_match_v := '1';
			end if;
			op_a_wb_match_v := '0';
			if (WB_FWD_I(fwd_en_c) = '1') and (EX_CTRL_BUS_I(ctrl_ra_is_pc_c) = '0')  and (EX_CTRL_BUS_I(ctrl_ra_3_c downto ctrl_ra_0_c) = WB_FWD_I(fwd_adr_3_c downto fwd_adr_0_c)) then
				op_a_wb_match_v := '1';
			end if;

			op_b_ma_match_v := '0';
			if (MA_FWD_I(fwd_en_c) = '1') and (EX_CTRL_BUS_I(ctrl_rb_is_imm_c) = '0') and (EX_CTRL_BUS_I(ctrl_rb_3_c downto ctrl_rb_0_c) = MA_FWD_I(fwd_adr_3_c downto fwd_adr_0_c)) then
				op_b_ma_match_v := '1';
			end if;
			op_b_wb_match_v := '0';
			if (WB_FWD_I(fwd_en_c) = '1') and (EX_CTRL_BUS_I(ctrl_rb_is_imm_c) = '0') and (EX_CTRL_BUS_I(ctrl_rb_3_c downto ctrl_rb_0_c) = WB_FWD_I(fwd_adr_3_c downto fwd_adr_0_c)) then
				op_b_wb_match_v := '1';
			end if;

			op_c_wb_match_v := '0';
			if (WB_FWD_I(fwd_en_c) = '1') and (EX_CTRL_BUS_I(ctrl_rb_3_c downto ctrl_rb_0_c) = WB_FWD_I(fwd_adr_3_c downto fwd_adr_0_c)) then
				op_c_wb_match_v := '1';
			end if;
			
--			op_a_ma_match_v := MA_FWD_I(fwd_en_c) and EX_CTRL_BUS_I(ctrl_a_ex_ma_fw_c);
--			op_a_wb_match_v := WB_FWD_I(fwd_en_c) and EX_CTRL_BUS_I(ctrl_a_ex_wb_fw_c);
--			op_b_ma_match_v := MA_FWD_I(fwd_en_c) and EX_CTRL_BUS_I(ctrl_b_ex_ma_fw_c);
--			op_b_wb_match_v := WB_FWD_I(fwd_en_c) and EX_CTRL_BUS_I(ctrl_b_ex_wb_fw_c);
--			op_c_wb_match_v := WB_FWD_I(fwd_en_c) and EX_CTRL_BUS_I(ctrl_c_ex_wb_fw_c);

			-- OP A Gating --
			if (EX_CTRL_BUS_I(ctrl_en_c) = '1') then
				-- OP A Forwarding --
				if (op_a_ma_match_v = '1') then
					op_a_tmp_v := MA_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- MA stage
				elsif (op_a_wb_match_v = '1') then
					op_a_tmp_v := WB_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- WB stage
				else
					op_a_tmp_v := OP_A_FF;
				end if;
			else
				op_a_tmp_v := (others => '0');
			end if;

			-- OP A Mask Unit --
			OP_A_INT <= op_a_tmp_v;
			if (EX_CTRL_BUS_I(ctrl_clr_ha_c) = '1') then -- clear high half word
				OP_A_INT(data_width_c-1 downto data_width_c/2) <= (others => '0');
			end if;
			if (EX_CTRL_BUS_I(ctrl_clr_la_c) = '1') then -- clear low half word
				OP_A_INT(data_width_c/2-1 downto 0) <= (others => '0');
			end if;

			-- OP B Gating --
			if (EX_CTRL_BUS_I(ctrl_en_c) = '1') then
				-- OP B Forwarding --
				if (op_b_ma_match_v = '1') then
					OP_B_INT <= MA_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- MA stage
				elsif (op_b_wb_match_v = '1') then
					OP_B_INT <= WB_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- WB stage
				else
					OP_B_INT <= OP_B_FF;
				end if;
			else
				OP_B_INT <= (others => '0');
			end if;

			-- OP C Forwarding --
			if (op_c_wb_match_v = '1') then
				OP_C_INT <= WB_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- WB stage
			else
				OP_C_INT <= OP_C_FF;
			end if;

		end process EX_FWD;



	-- Functional Unit: Arithmetic Core --------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		FU_ARITHMETIC_CORE: process(EX_CTRL_BUS_I, OP_A_INT, OP_B_INT, FLAG_BUS_I)
			variable op_a_v, op_b_v   : std_logic_vector(data_width_c-1 downto 0);
			variable cflag_v          : std_logic;
			variable add_a_v, add_b_v : std_logic_vector(data_width_c downto 0);
			variable add_cf_in_v      : std_logic_vector(0 downto 0);
			variable adder_c_sel_v    : std_logic;
			variable adder_carry_in_v : std_logic;
			variable adder_tmp_v      : std_logic_vector(data_width_c downto 0);
		begin

			-- Operand Insulation --
			op_a_v  := (others => '0');
			op_b_v  := (others => '0');
			cflag_v := '0';
			if (EX_CTRL_BUS_I(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) = alu_adc_c) or
			(EX_CTRL_BUS_I(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) = alu_sbc_c) then
				op_a_v  := OP_A_INT;
				op_b_v  := OP_B_INT;
				cflag_v := FLAG_BUS_I(flag_c_c);
			end if;

			-- ADD/SUB Select --
			if (EX_CTRL_BUS_I(ctrl_alu_cf_opt_c) = '0') then -- propagate carry_in
				adder_c_sel_v := cflag_v;
			else -- invert carry_in
				adder_c_sel_v := not cflag_v;
			end if;
			add_a_v := '0' & op_a_v;
			adder_carry_in_v := adder_c_sel_v and EX_CTRL_BUS_I(ctrl_alu_usec_c);
			case (EX_CTRL_BUS_I(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c)) is
				when alu_sbc_c => -- (+OP_A) + (-OP_B) {+ (-CARRY)}
					add_b_v        := '0' & (not op_b_v);
					add_cf_in_v(0) := not adder_carry_in_v;
				when alu_adc_c => -- (+OP_A) + (+OP_B) {+ (+CARRY)}
					add_b_v        := '0' & op_b_v;
					add_cf_in_v(0) := adder_carry_in_v;
				when others => -- other function set, adder irrelevant
					add_b_v        := '0' & op_b_v;
					add_cf_in_v(0) := adder_carry_in_v;
			end case;

			-- adder core --
			adder_tmp_v  := std_logic_vector(unsigned(add_a_v) + unsigned(add_b_v) + unsigned(add_cf_in_v(0 downto 0)));
			FU_ARITH_RES <= adder_tmp_v(data_width_c-1 downto 0); -- result, MSB of adder_tmp_v is CARRY bit
			
			-- adder flag carry output logic --
			case (EX_CTRL_BUS_I(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c)) is
				when alu_adc_c => -- add
					FU_ARITH_FLG(0) <= adder_tmp_v(data_width_c);
				when alu_sbc_c => -- sub
					FU_ARITH_FLG(0) <= not adder_tmp_v(data_width_c);
				when others => -- other function set, adder irrelevant
					FU_ARITH_FLG(0) <= adder_tmp_v(data_width_c);
			end case;

			-- Arithmetic overflow flag --
			FU_ARITH_FLG(1) <= ((not add_a_v(data_width_c-1)) and (not add_b_v(data_width_c-1)) and (    adder_tmp_v(data_width_c-1))) or
							((    add_a_v(data_width_c-1)) and (    add_b_v(data_width_c-1)) and (not adder_tmp_v(data_width_c-1)));
		end process FU_ARITHMETIC_CORE;



	-- Functional Unit: Shifter Core -----------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		FU_SHIFTER_CORE: process(EX_CTRL_BUS_I, OP_A_INT, OP_B_FF, FLAG_BUS_I)
			variable op_a_v, op_b_v   : std_logic_vector(data_width_c-1 downto 0);
			variable cflag_v          : std_logic;
			variable shifter_dat_v    : std_logic_vector(data_width_c-1 downto 0);
			variable shifter_carry_v  : std_logic;
			variable shifter_ovf_v    : std_logic;
		begin

			-- Operand Insulation --
			op_a_v  := (others => '0');
			op_b_v  := (others => '0');
			cflag_v := '0';
			if (EX_CTRL_BUS_I(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) = alu_sft_c) then
				op_a_v  := OP_A_INT;
				op_b_v  := OP_B_FF;
				cflag_v := FLAG_BUS_I(flag_c_c);
			end if;

			-- Shifter Core --
			case (op_b_v(2 downto 0)) is
				when sft_asr_c => -- arithmetical right shift
					shifter_dat_v   := op_a_v(data_width_c-1) & op_a_v(data_width_c-1 downto 1);
					FU_SHIFT_FLG(0) <= op_a_v(0);
				when sft_rol_c => -- rotate left
					shifter_dat_v   := op_a_v(data_width_c-2 downto 0) & op_a_v(data_width_c-1);
					FU_SHIFT_FLG(0) <= op_a_v(data_width_c-1);
				when sft_ror_c => -- rotate right
					shifter_dat_v   := op_a_v(0) & op_a_v(data_width_c-1 downto 1);
					FU_SHIFT_FLG(0) <= op_a_v(0);
				when sft_lsl_c => -- logical shift left
					shifter_dat_v   := op_a_v(data_width_c-2 downto 0) & '0';
					FU_SHIFT_FLG(0) <= op_a_v(data_width_c-1);
				when sft_lsr_c => -- logical shift right
					shifter_dat_v   := '0' & op_a_v(data_width_c-1 downto 1);
					FU_SHIFT_FLG(0) <= op_a_v(0);
				when sft_rlc_c => -- rotate left through carry
					shifter_dat_v   := op_a_v(data_width_c-2 downto 0) & cflag_v;
					FU_SHIFT_FLG(0) <= op_a_v(data_width_c-1);
				when sft_rrc_c => -- rotate right through carry
					shifter_dat_v   := cflag_v & op_a_v(data_width_c-1 downto 1);
					FU_SHIFT_FLG(0) <= op_a_v(0);
				when others    => -- swap halfwords (sft_swp_c)
					shifter_dat_v   := op_a_v(data_width_c/2-1 downto 0) & op_a_v(data_width_c-1 downto data_width_c/2);
					FU_SHIFT_FLG(0) <= op_a_v(data_width_c-1);
			end case;
			FU_SHIFT_RES <= shifter_dat_v;

			-- Overflow flag --
			FU_SHIFT_FLG(1) <= op_a_v(data_width_c-1) xor shifter_dat_v(data_width_c-1);

		end process FU_SHIFTER_CORE;



	-- Functional Unit: Logical Core -----------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		FU_LOGIC_CORE: process(EX_CTRL_BUS_I, OP_A_INT, OP_B_INT, FLAG_BUS_I)
		begin
			-- Keep Flags --
			FU_LOGIC_FLG(0) <= FLAG_BUS_I(flag_c_c);
			FU_LOGIC_FLG(1) <= FLAG_BUS_I(flag_o_c);

			-- Logic Function --
			case (EX_CTRL_BUS_I(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c)) is
				when alu_and_c  => FU_LOGIC_RES <= OP_A_INT and OP_B_INT;
				when alu_nand_c => FU_LOGIC_RES <= OP_A_INT nand OP_B_INT;
				when alu_orr_c  => FU_LOGIC_RES <= OP_A_INT or OP_B_INT;
				when alu_eor_c  => FU_LOGIC_RES <= OP_A_INT xor OP_B_INT;
				when alu_bic_c  => FU_LOGIC_RES <= OP_A_INT and (not OP_B_INT);
				when others     => FU_LOGIC_RES    <= (others => '0');
								FU_LOGIC_FLG(0) <= '0';
								FU_LOGIC_FLG(1) <= '0';
			end case;
		end process FU_LOGIC_CORE;



	-- Function Selector -----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		-- Data result --
		ALU_RES_INT <= FU_ARITH_RES or FU_SHIFT_RES or FU_LOGIC_RES;

		-- Carry Flag --
		FLAG_BUS_O(flag_c_c) <= FU_ARITH_FLG(0) or FU_SHIFT_FLG(0) or FU_LOGIC_FLG(0);

		-- Overflow Flag --
		FLAG_BUS_O(flag_o_c) <= FU_ARITH_FLG(1) or FU_SHIFT_FLG(1) or FU_LOGIC_FLG(0);



	-- Parity Computation ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		PARITY_GEN: process(OP_A_INT)
			variable par_v : std_logic;
		begin
			par_v := '0';
			for i in 0 to data_width_c-1 loop
				par_v := par_v xor OP_A_INT(i);
			end loop;
			PARITY_BIT <= par_v;
		end process PARITY_GEN;



	-- Additional Flag Computation -------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------

		-- Zero detector --
		-- Ladies and gentleman, the critical path!
		IS_ZERO              <= '1' when (to_integer(unsigned(ALU_RES_INT)) = 0) else '0'; -- zero detector
		EXTND_ZERO           <= (FLAG_BUS_I(flag_z_c) and IS_ZERO) when (EX_CTRL_BUS_I(ctrl_alu_zf_opt_c) = '0') else (FLAG_BUS_I(flag_z_c) or IS_ZERO); -- extended zero detector
		FLAG_BUS_O(flag_z_c) <= EXTND_ZERO when (EX_CTRL_BUS_I(ctrl_alu_usez_c) = '1') else IS_ZERO; -- (extended) zero flag

		-- Negative flag --
		FLAG_BUS_O(flag_n_c) <= ALU_RES_INT(data_width_c-1); -- negative flag

		-- T-Flag update --
		SEL_BIT              <= OP_A_INT(to_integer(unsigned(OP_B_INT(3 downto 0)))); -- selected bit
		T_FLAG_FUNC          <= PARITY_BIT when (EX_CTRL_BUS_I(ctrl_get_par_c) = '1') else SEL_BIT; -- parity or selected bit
		INV_BIT              <= (not T_FLAG_FUNC) when (EX_CTRL_BUS_I(ctrl_tf_inv_c) = '1') else T_FLAG_FUNC; -- invert bit?
		TRANSF_INT           <= INV_BIT when (EX_CTRL_BUS_I(ctrl_tf_store_c) = '1') else FLAG_BUS_I(flag_t_c); -- transfer flag
		FLAG_BUS_O(flag_t_c) <= TRANSF_INT;

		-- T-Flag for mask generation (this is some kind of forwarding to the opcode decoder) --
		MASK_T_FLAG_O        <= TRANSF_INT when (EX_CTRL_BUS_I(ctrl_en_c) = '1') and (EX_CTRL_BUS_I(ctrl_tf_store_c) = '1') else FLAG_BUS_I(flag_t_c);



	-- Mltiplier Kernel ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		-- Operand gating --
		MUL_OP_A <= OP_A_INT when ((build_mul_c = true) and (EX_CTRL_BUS_I(ctrl_use_mul_c) = '1')) else (others => '0');
		MUL_OP_B <= OP_B_INT when ((build_mul_c = true) and (EX_CTRL_BUS_I(ctrl_use_mul_c) = '1')) else (others => '0');

		-- Multiplier core --
		MUL_BUFFER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					MUL_RES_O <= (others => '0');
				elsif (CE_I = '1') then
					MUL_RES_O <= std_logic_vector(unsigned(MUL_OP_A) * unsigned(MUL_OP_B));
				end if;
			end if;
		end process MUL_BUFFER;



	-- Module Data Output ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------

		-- Coprocessor Interface --
		CP_CP0_EN_O <= EX_CTRL_BUS_I(ctrl_en_c) and EX_CTRL_BUS_I(ctrl_cp_acc_c) and (not EX_CTRL_BUS_I(ctrl_cp_id_c)); -- cp 0 access
		CP_CP1_EN_O <= EX_CTRL_BUS_I(ctrl_en_c) and EX_CTRL_BUS_I(ctrl_cp_acc_c) and      EX_CTRL_BUS_I(ctrl_cp_id_c);  -- cp 1 access
		CP_OP_O     <= EX_CTRL_BUS_I(ctrl_cp_trans_c); -- data transfer / cp operation
		CP_DAT_O    <= OP_A_INT; -- data output
		CP_RW_O     <= EX_CTRL_BUS_I(ctrl_cp_wr_c); -- read/write transfer
		CP_CMD_O(cp_op_a_msb_c downto cp_op_a_lsb_c) <= EX_CTRL_BUS_I(ctrl_cp_rd_2_c  downto ctrl_cp_rd_0_c)  when (EX_CTRL_BUS_I(ctrl_cp_acc_c) = '1') else (others => '0');  -- cp destination / op A reg
		CP_CMD_O(cp_op_b_msb_c downto cp_op_b_lsb_c) <= EX_CTRL_BUS_I(ctrl_cp_ra_2_c  downto ctrl_cp_ra_0_c)  when (EX_CTRL_BUS_I(ctrl_cp_acc_c) = '1') else (others => '0');  -- cp op B reg
		CP_CMD_O(cp_cmd_msb_c  downto cp_cmd_lsb_c)  <= EX_CTRL_BUS_I(ctrl_cp_cmd_2_c downto ctrl_cp_cmd_0_c) when (EX_CTRL_BUS_I(ctrl_cp_acc_c) = '1') else (others => '0'); -- cp command

		-- Data Output --
		MSR_DATA_O  <= OP_B_INT;    -- MSR write data
		ALU_RES_O   <= ALU_RES_INT; -- ALU result
		BP_OPA_O    <= OP_A_INT;    -- operand A bypass out (address base for mem access)

		-- Link_address/mem_w_data port --
		BP_OPC_O    <= PC_1D_I when (EX_CTRL_BUS_I(ctrl_link_c) = '1') else OP_C_INT; -- operand C bypass out (data for mem write access) or link address

		-- Memory system --
		MEM_REQ_O   <= EX_CTRL_BUS_I(ctrl_en_c) and EX_CTRL_BUS_I(ctrl_mem_acc_c); -- mem access in next cycle



end ALU_STRUCTURE;
