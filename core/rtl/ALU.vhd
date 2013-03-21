-- ########################################################
-- #    << ATLAS Project - Arithmetical/Logical Unit >>   #
-- # **************************************************** #
-- #  The main data processing is done here. Also the CP  #
-- #  interface emerges from this unit.                   #
-- # **************************************************** #
-- #  Last modified: 18.03.2013                           #
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
				PC_DATA_O       : out std_logic_vector(data_width_c-1 downto 0); -- PC write data
				ALU_RES_O       : out std_logic_vector(data_width_c-1 downto 0); -- ALU result
				MAC_RES_O       : out std_logic_vector(data_width_c-1 downto 0); -- MAC result
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
	signal OP_A_FF     : std_logic_vector(data_width_c-1 downto 0);
	signal OP_B_FF     : std_logic_vector(data_width_c-1 downto 0);
	signal OP_C_FF     : std_logic_vector(data_width_c-1 downto 0);

	-- Internal data lines  --
	signal OP_A_INT    : std_logic_vector(data_width_c-1 downto 0);
	signal OP_B_INT    : std_logic_vector(data_width_c-1 downto 0);
	signal OP_C_INT    : std_logic_vector(data_width_c-1 downto 0);
	signal ALU_RES_INT : std_logic_vector(data_width_c-1 downto 0);
	signal TRANSF_INT  : std_logic;
	signal SEL_BIT     : std_logic;
	signal INV_BIT     : std_logic;
	signal IS_ZERO     : std_logic;
	signal EXTND_ZERO  : std_logic;

	-- Multiplier --
	signal MAC_BUF     : std_logic_vector(data_width_c-1 downto 0);

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

			-- operand A forwarding --
			if (op_a_ma_match_v = '1') then
				OP_A_INT <= MA_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- MA stage
			elsif (op_a_wb_match_v = '1') then
				OP_A_INT <= WB_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- WB stage
			else
				OP_A_INT <= OP_A_FF;
			end if;

			-- operand B forwarding --
			if (op_b_ma_match_v = '1') then
				OP_B_INT <= MA_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- MA stage
			elsif (op_b_wb_match_v = '1') then
				OP_B_INT <= WB_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- WB stage
			else
				OP_B_INT <= OP_B_FF;
			end if;

			-- operand C forwarding --
			if (op_c_wb_match_v = '1') then
				OP_C_INT <= WB_FWD_I(fwd_dat_msb_c downto fwd_dat_lsb_c); -- WB stage
			else
				OP_C_INT <= OP_C_FF;
			end if;

		end process EX_FWD;



	-- ALU / Shifter Logic ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		ALU_KERNEL: process(EX_CTRL_BUS_I, OP_A_INT, OP_B_INT, OP_B_FF, FLAG_BUS_I)
			variable add_a_v, add_b_v : std_logic_vector(data_width_c downto 0);
			variable add_cf_in_v      : std_logic_vector(0 downto 0);
			variable adder_carry_in_v : std_logic;
			variable adder_tmp_v      : std_logic_vector(data_width_c downto 0);
			variable adder_res_v      : std_logic_vector(data_width_c-1 downto 0);
			variable adder_car_v      : std_logic;
			variable adder_ovf_v      : std_logic;
			variable shifter_dat_v    : std_logic_vector(data_width_c-1 downto 0);
			variable shifter_carry_v  : std_logic;
			variable shifter_ovf_v    : std_logic;
			variable mask_data_v      : std_logic_vector(data_width_c-1 downto 0);
		begin

			-- ADDER / SUBTRACTOR --
			-- ==============================================
			add_a_v := '0' & OP_A_INT;
			adder_carry_in_v := FLAG_BUS_I(flag_c_c) and EX_CTRL_BUS_I(ctrl_alu_usec_c);
			case (EX_CTRL_BUS_I(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c)) is
				when alu_sbc_c => -- (+OP_A) + (-OP_B) {+ (-CARRY)}
					add_b_v        := '0' & (not OP_B_INT);
					add_cf_in_v(0) := not adder_carry_in_v;
				when alu_adc_c => -- (+OP_A) + (+OP_B) {+ (+CARRY)}
					add_b_v        := '0' & OP_B_INT;
					add_cf_in_v(0) := adder_carry_in_v;
				when others => -- other function set, adder irrelevant
					add_b_v        := '0' & OP_B_INT;
					add_cf_in_v(0) := adder_carry_in_v;
			end case;

			-- adder core --
			adder_tmp_v := std_logic_vector(unsigned(add_a_v) + unsigned(add_b_v) + unsigned(add_cf_in_v(0 downto 0)));
			adder_res_v := adder_tmp_v(data_width_c-1 downto 0); -- result, MSB of adder_tmp_v is CARRY bit
			
			-- adder flag carry output logic --
			case (EX_CTRL_BUS_I(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c)) is
				when alu_adc_c => -- add
					adder_car_v := adder_tmp_v(data_width_c);
				when alu_sbc_c => -- sub
					adder_car_v := not adder_tmp_v(data_width_c);
				when others => -- other function set, adder irrelevant
					adder_car_v := adder_tmp_v(data_width_c);
			end case;

			-- Arithmetica overflow flag --
			adder_ovf_v := ((not add_a_v(data_width_c-1)) and (not add_b_v(data_width_c-1)) and (    adder_tmp_v(data_width_c-1))) or
					       ((    add_a_v(data_width_c-1)) and (    add_b_v(data_width_c-1)) and (not adder_tmp_v(data_width_c-1)));


			-- SHIFTER --
			-- ==============================================
			case (op_b_ff(2 downto 0)) is
				when sft_asr_c => -- arithmetical right shift
					shifter_dat_v   := OP_A_INT(data_width_c-1) & OP_A_INT(data_width_c-1 downto 1);
					shifter_carry_v := OP_A_INT(0);
				when sft_rol_c => -- rotate left
					shifter_dat_v   := OP_A_INT(data_width_c-2 downto 0) & OP_A_INT(data_width_c-1);
					shifter_carry_v := OP_A_INT(data_width_c-1);
				when sft_ror_c => -- rotate right
					shifter_dat_v   := OP_A_INT(0) & OP_A_INT(data_width_c-1 downto 1);
					shifter_carry_v := OP_A_INT(0);
				when sft_lsl_c => -- logical shift left
					shifter_dat_v   := OP_A_INT(data_width_c-2 downto 0) & '0';
					shifter_carry_v := OP_A_INT(data_width_c-1);
				when sft_lsr_c => -- logical shift right
					shifter_dat_v   := '0' & OP_A_INT(data_width_c-1 downto 1);
					shifter_carry_v := OP_A_INT(0);
				when sft_rlc_c => -- rotate left through carry
					shifter_dat_v   := OP_A_INT(data_width_c-2 downto 0) & FLAG_BUS_I(flag_c_c);
					shifter_carry_v := OP_A_INT(data_width_c-1);
				when sft_rrc_c => -- rotate right through carry
					shifter_dat_v   := FLAG_BUS_I(flag_c_c) & OP_A_INT(data_width_c-1 downto 1);
					shifter_carry_v := OP_A_INT(0);
				when others    => -- swap halfwords (sft_swp_c)
					shifter_dat_v   := OP_A_INT(data_width_c/2-1 downto 0) & OP_A_INT(data_width_c-1 downto data_width_c/2);
					shifter_carry_v := OP_A_INT(data_width_c-1);
			end case;

			-- Overflow flag --
			shifter_ovf_v := OP_A_INT(data_width_c-1) xor shifter_dat_v(data_width_c-1);


			-- MASK UNIT for immediate loading --
			-- ==============================================
			mask_data_v := OP_A_INT;
			if (EX_CTRL_BUS_I(ctrl_clr_ha_c) = '1') then -- clear high half word
				mask_data_v(15 downto 08) := (others => '0');
			end if;
			if (EX_CTRL_BUS_I(ctrl_clr_la_c) = '1') then -- clear low half word
				mask_data_v(07 downto 00) := (others => '0');
			end if;


			-- FUNCTION SELECTOR --
			-- ==============================================
			FLAG_BUS_O(flag_c_c) <= FLAG_BUS_I(flag_c_c); -- keep carry
			FLAG_BUS_O(flag_o_c) <= '0'; -- clear overflow flag
			ALU_RES_INT          <= adder_res_v;

			-- elementary function control --
			case (EX_CTRL_BUS_I(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c)) is
				when alu_and_c => -- logical and
					ALU_RES_INT <= mask_data_v and OP_B_INT; -- mask irrelevant
				when alu_orr_c => -- logical or
					ALU_RES_INT <= mask_data_v or OP_B_INT; -- use mask data for immediate loading
				when alu_eor_c => -- logical xor
					ALU_RES_INT <= mask_data_v xor OP_B_INT; -- mask irrelevant
				when alu_nand_c => -- logical nand
					ALU_RES_INT <= mask_data_v nand OP_B_INT; -- mask irrelevant
				when alu_bic_c => -- bit clear (a and (not b))
					ALU_RES_INT <= OP_A_INT and (not OP_B_INT);
				when alu_sft_c => -- shift (alu_sft_c)
					FLAG_BUS_O(flag_c_c) <= shifter_carry_v;
					FLAG_BUS_O(flag_o_c) <= shifter_ovf_v;
					ALU_RES_INT          <= shifter_dat_v;
				when alu_adc_c => -- add
					FLAG_BUS_O(flag_c_c) <= adder_car_v;
					FLAG_BUS_O(flag_o_c) <= adder_ovf_v;
					ALU_RES_INT <= adder_res_v;
				when alu_sbc_c => -- sub/cmp/cpc
					FLAG_BUS_O(flag_c_c) <= adder_car_v;
					FLAG_BUS_O(flag_o_c) <= adder_ovf_v;
					ALU_RES_INT <= adder_res_v;
				when others => -- undefined
					NULL;
			end case;

		end process ALU_KERNEL;

		-- Zero detector - Ladies and gentleman, the critical path! --
		IS_ZERO              <= '1' when (to_integer(unsigned(ALU_RES_INT)) = 0) else '0'; -- zero detector
		EXTND_ZERO           <= FLAG_BUS_I(flag_z_c) and IS_ZERO; -- extended zero detector
		FLAG_BUS_O(flag_z_c) <= EXTND_ZERO when (EX_CTRL_BUS_I(ctrl_alu_usez_c) = '1') else IS_ZERO; -- zero flag

		-- Negative flag --
		FLAG_BUS_O(flag_n_c) <= ALU_RES_INT(data_width_c-1); -- negative flag

		-- T-Flag update --
		SEL_BIT    <= OP_A_INT(to_integer(unsigned(EX_CTRL_BUS_I(ctrl_bit_3_c downto ctrl_bit_0_c))));
		INV_BIT    <= (not SEL_BIT) when (EX_CTRL_BUS_I(ctrl_tf_inv_c) = '1') else SEL_BIT; -- invert bit?
		TRANSF_INT <= INV_BIT when (EX_CTRL_BUS_I(ctrl_tf_store_c) = '1') else FLAG_BUS_I(flag_t_c); -- transfer flag
		FLAG_BUS_O(flag_t_c) <= TRANSF_INT;

		-- T-Flag for mask generation (yeah, this is some kind of forwarding) --
		MASK_T_FLAG_O <= TRANSF_INT when (EX_CTRL_BUS_I(ctrl_en_c) = '1') and (EX_CTRL_BUS_I(ctrl_tf_store_c) = '1') else FLAG_BUS_I(flag_t_c);



	-- Multiplier ------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MAC_BUFFER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					MAC_BUF <= (others => '0');
				elsif (CE_I = '1') then
					if (EX_CTRL_BUS_I(ctrl_load_mac_c) = '1') and (EX_CTRL_BUS_I(ctrl_en_c) = '1') then -- load mac buffer
						MAC_BUF <= OP_C_I;
					else
						MAC_BUF <= (others => '0');
					end if;
				end if;
			end if;
		end process MAC_BUFFER;



		MULTIPLIER: process(EX_CTRL_BUS_I, OP_A_INT, OP_B_INT, MAC_BUF)
			variable mul_res_v : std_logic_vector(2*data_width_c-1 downto 0);
			variable mac_ofs_v : std_logic_vector(data_width_c-1 downto 0);
		begin
			-- Multiplier --
			if (build_mul_c = true) then
				mul_res_v := std_logic_vector(unsigned(OP_A_INT) * unsigned(OP_B_INT));
			else
				mul_res_v := (others => '0');
			end if;

			-- Offset --
			if (EX_CTRL_BUS_I(ctrl_use_offs_c) = '1') and (build_mac_c = true) then
				mac_ofs_v := MAC_BUF;
			else
				mac_ofs_v := (others => '0');
			end if;

			-- Accumulate --
			if (build_mac_c = true) then
				MAC_RES_O <= std_logic_vector(unsigned(mul_res_v(data_width_c-1 downto 0)) + unsigned(mac_ofs_v));
			else
				MAC_RES_O <= mul_res_v(data_width_c-1 downto 0);
			end if;
		end process MULTIPLIER;



	-- Module Data Output ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------

		-- Coprocessor Interface --
		CP_CP0_EN_O <= EX_CTRL_BUS_I(ctrl_en_c) and EX_CTRL_BUS_I(ctrl_cp_acc_c) and (not EX_CTRL_BUS_I(ctrl_cp_id_c)); -- cp 0 access
		CP_CP1_EN_O <= EX_CTRL_BUS_I(ctrl_en_c) and EX_CTRL_BUS_I(ctrl_cp_acc_c) and      EX_CTRL_BUS_I(ctrl_cp_id_c);  -- cp 1 access
		CP_OP_O     <= EX_CTRL_BUS_I(ctrl_cp_trans_c); -- data transfer / cp operation
		CP_DAT_O    <= OP_A_INT; -- data output
		CP_RW_O     <= EX_CTRL_BUS_I(ctrl_cp_wr_c); -- read/write transfer
		CP_CMD_O(cp_op_a_msb_c downto cp_op_a_lsb_c) <= EX_CTRL_BUS_I(ctrl_cp_rd_2_c  downto ctrl_cp_rd_0_c);  -- cp destination / op A reg
		CP_CMD_O(cp_op_b_msb_c downto cp_op_b_lsb_c) <= EX_CTRL_BUS_I(ctrl_cp_ra_2_c  downto ctrl_cp_ra_0_c);  -- cp op B reg
		CP_CMD_O(cp_cmd_msb_c  downto cp_cmd_lsb_c)  <= EX_CTRL_BUS_I(ctrl_cp_cmd_2_c downto ctrl_cp_cmd_0_c); -- cp command

		-- Data Output --
		MSR_DATA_O  <= OP_B_INT;    -- MSR write data
		PC_DATA_O   <= ALU_RES_INT; -- PC write data
		ALU_RES_O   <= ALU_RES_INT; -- ALU result
		BP_OPA_O    <= OP_A_INT;    -- operand A bypass out (address base for mem access)

		-- Link_address/mem_w_data port --
		BP_OPC_O    <= PC_1D_I when (EX_CTRL_BUS_I(ctrl_link_c) = '1') else OP_C_INT; -- operand C bypass out (data for mem write access) or link address

		-- Memory system --
		MEM_REQ_O   <= EX_CTRL_BUS_I(ctrl_en_c) and EX_CTRL_BUS_I(ctrl_mem_acc_c); -- mem access in next cycle



end ALU_STRUCTURE;
