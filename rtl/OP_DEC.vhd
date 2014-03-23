-- ########################################################
-- #         << ATLAS Project - OpCode Decoder >>         #
-- # **************************************************** #
-- #  OpCode decoding unit.                               #
-- # **************************************************** #
-- #  Last modified: 23.03.2014                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity OP_DEC is
	port	(
-- ###############################################################################################
-- ##           Decoder Interface Input                                                         ##
-- ###############################################################################################

				INSTR_I         : in  std_logic_vector(data_width_c-1 downto 0); -- instruction input
				INSTR_ADR_I     : in  std_logic_vector(data_width_c-1 downto 0); -- corresponding address
				T_FLAG_I        : in  std_logic; -- T-Flag input
				M_FLAG_I        : in  std_logic; -- Mode flag input
				MULTI_CYC_I     : in  std_logic; -- multi-cycle indicator
				CP_PTC_I        : in  std_logic; -- user coprocessor protection

-- ###############################################################################################
-- ##           Decoder Interface Output                                                        ##
-- ###############################################################################################

				MULTI_CYC_REQ_O : out std_logic; -- multi-cycle reqest
				CTRL_O          : out std_logic_vector(ctrl_width_c-1 downto 0); -- decoder ctrl lines
				IMM_O           : out std_logic_vector(data_width_c-1 downto 0)  -- immediate
			);
end OP_DEC;

architecture OP_DEC_STRUCTURE of OP_DEC is

	-- formated instruction --
	signal INSTR_INT : std_logic_vector(15 downto 0);

begin

	-- Data Format Converter -------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		DATA_CONV: process(INSTR_I, INSTR_ADR_I)
			variable instr_sel_v : std_logic_vector(31 downto 0);
			variable instr_tmp_v : std_logic_vector(15 downto 0);
		begin
			instr_sel_v := (others => '0');
			for i in 0 to data_width_c-1 loop
				instr_sel_v(i) := INSTR_I(i);
			end loop;
			if (data_width_c = 16) then -- 16-bit mode
				instr_tmp_v := instr_sel_v(15 downto 0);
			else -- 32-bit mode
				if (INSTR_ADR_I(1) = '0') then
					instr_tmp_v := instr_sel_v(15 downto 0);
				else
					instr_tmp_v := instr_sel_v(31 downto 16);
				end if;
			end if;
			if (big_endian_c = true) then -- endian converter
				INSTR_INT <= instr_tmp_v(7 downto 0) & instr_tmp_v(15 downto 8);
			else
				INSTR_INT <= instr_tmp_v;
			end if;
		end process DATA_CONV;



	-- Opcode Decoder --------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		OPCODE_DECODER: process(INSTR_INT, MULTI_CYC_I, T_FLAG_I, M_FLAG_I, CP_PTC_I)
			variable mem_acc_temp_v  : std_logic_vector(3 downto 0);
			variable redundant_reg_v : std_logic;
		begin

			-- defaults --
			IMM_O                                      <= (others => '0');                  -- zero immediate
			MULTI_CYC_REQ_O                            <= '0';                              -- no multi-cycle operation
			CTRL_O                                     <= (others => '0');                  -- all signals disabled
			CTRL_O(ctrl_en_c)                          <= '1';                              -- but we're enabled ^^
			CTRL_O(ctrl_cp_id_c)                       <= INSTR_INT(10);                    -- coprocessor ID
			CTRL_O(ctrl_ra_3_c   downto ctrl_ra_0_c)   <= M_FLAG_I & INSTR_INT(6 downto 4); -- operand A register
			CTRL_O(ctrl_rb_3_c   downto ctrl_rb_0_c)   <= M_FLAG_I & INSTR_INT(2 downto 0); -- operand B register
			CTRL_O(ctrl_rd_3_c   downto ctrl_rd_0_c)   <= M_FLAG_I & INSTR_INT(9 downto 7); -- destination register
			CTRL_O(ctrl_cond_3_c downto ctrl_cond_0_c) <= INSTR_INT(13 downto 10);          -- branch condition

			-- both operands have same addresses --
			redundant_reg_v := '0';
			if (INSTR_INT(6 downto 4) = INSTR_INT(2 downto 0)) then
				redundant_reg_v := '1';
			end if;

			-- decoder --
			case (INSTR_INT(15 downto 14)) is

				when "00" => -- Class 0: ALU data processing // Bank / PC / MSR transfer
				-- ==============================================================================
					CTRL_O(ctrl_rd_wb_c)   <= '1'; -- allow write back
					CTRL_O(ctrl_fupdate_c) <= INSTR_INT(3); -- flag update
					IMM_O(2 downto 0)      <= INSTR_INT(2 downto 0); -- 3-bit immediate
					if (INSTR_INT(13 downto 10) = fs_inc_c) or (INSTR_INT(13 downto 10) = fs_dec_c) or (INSTR_INT(13 downto 10) = fs_sft_c) then
						CTRL_O(ctrl_rb_is_imm_c) <= '1'; -- yes, this is an immediate
					end if;
					-- mapping to alu elementary operation --
					case (INSTR_INT(13 downto 10)) is

						when fs_orr_c => -- logical or // load from user bank register if redundant
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_orr_c; -- logical or
							if (redundant_reg_v = '1') then -- user bank load
								CTRL_O(ctrl_ra_3_c) <= user_mode_c; -- load from user bank
								CTRL_O(ctrl_rb_3_c) <= user_mode_c; -- load from user bank
								if (M_FLAG_I = user_mode_c) then -- unauthorized access
									CTRL_O(ctrl_cmd_err_c) <= '1'; -- access violation - cmd_err trap
								end if;
							end if;

						when fs_and_c => -- logical and // store to user bank register if redundant
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_and_c; -- logical and
							if (redundant_reg_v = '1') then -- user bank store
								CTRL_O(ctrl_rd_3_c) <= user_mode_c; -- store to user bank
								if (M_FLAG_I = user_mode_c) then -- unauthorized access
									CTRL_O(ctrl_cmd_err_c) <= '1'; -- access violation - cmd_err trap
								end if;
							end if;

						when fs_cmp_c => -- compare by sbtraction // load from msr if s = 0
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_sbc_c; -- compare by subtraction
							CTRL_O(ctrl_rd_wb_c) <= '0'; -- disable write back
							CTRL_O(ctrl_msr_am_1_c) <= INSTR_INT(6);
							CTRL_O(ctrl_msr_am_0_c) <= INSTR_INT(5);
							if (INSTR_INT(3) = '0') then -- load from MSR
								if (INSTR_INT(6 downto 5) /= "11") and (M_FLAG_I = user_mode_c) then
									CTRL_O(ctrl_cmd_err_c) <= '1'; -- access violation - cmd_err trap
								end if;
								CTRL_O(ctrl_msr_rd_c) <= '1'; -- read msr
								CTRL_O(ctrl_rd_wb_c)  <= '1'; -- re-enable write back
							end if;

						when fs_cpx_c => -- extended compare with flags // store to msr if s = 0
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_sbc_c; -- compare by subtraction with flags
							CTRL_O(ctrl_alu_usec_c) <= '1'; -- use carry input
							CTRL_O(ctrl_alu_usez_c) <= '1'; -- use zero input
							CTRL_O(ctrl_rd_wb_c)    <= '0'; -- disable write back
							CTRL_O(ctrl_msr_am_1_c) <= INSTR_INT(6); -- only for MSR immediate write access
							CTRL_O(ctrl_msr_am_0_c) <= INSTR_INT(5); -- only for MSR immediate write access
							IMM_O(msr_sys_z_flag_c) <= INSTR_INT(0); -- only for MSR immediate write access
							IMM_O(msr_usr_z_flag_c) <= INSTR_INT(0); -- only for MSR immediate write access
							IMM_O(msr_sys_c_flag_c) <= INSTR_INT(1); -- only for MSR immediate write access
							IMM_O(msr_usr_c_flag_c) <= INSTR_INT(1); -- only for MSR immediate write access
							IMM_O(msr_sys_o_flag_c) <= INSTR_INT(2); -- only for MSR immediate write access
							IMM_O(msr_usr_o_flag_c) <= INSTR_INT(2); -- only for MSR immediate write access
							IMM_O(msr_sys_n_flag_c) <= INSTR_INT(7); -- only for MSR immediate write access
							IMM_O(msr_usr_n_flag_c) <= INSTR_INT(7); -- only for MSR immediate write access
							IMM_O(msr_sys_t_flag_c) <= INSTR_INT(8); -- only for MSR immediate write access
							IMM_O(msr_usr_t_flag_c) <= INSTR_INT(8); -- only for MSR immediate write access
							if (INSTR_INT(3) = '0') then -- store to MSR
								if ((M_FLAG_I = user_mode_c) and (INSTR_INT(6 downto 5) /= "11")) then
									CTRL_O(ctrl_cmd_err_c) <= '1'; -- access violation -> cmd_err trap
								end if;
								if(MULTI_CYC_I = '0') then
									CTRL_O(ctrl_msr_wr_c)    <= '1'; -- write msr
									MULTI_CYC_REQ_O          <= '1'; -- we need a dummy cycle afterwards
									CTRL_O(ctrl_rb_is_imm_c) <= INSTR_INT(4); -- store immediate
								else
									CTRL_O(ctrl_en_c) <= '0'; -- insert empty cycle
								end if;
							end if;

						when fs_tst_c => -- compare by logical xor // load from pc if s = 0
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_eor_c; -- compare by logical xor
							CTRL_O(ctrl_rd_wb_c) <= '0'; -- disable write back
							if (INSTR_INT(3) = '0') then -- load from PC
								CTRL_O(ctrl_ra_is_pc_c) <= '1'; -- read pc
								CTRL_O(ctrl_rb_is_imm_c) <= INSTR_INT(3); -- this is an immediate
								CTRL_O(ctrl_rd_wb_c)  <= '1'; -- re-enable write back
							end if;

						when fs_teq_c => -- compare by logical and // store to pc if s = 0
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_and_c; -- compare by logical and
							CTRL_O(ctrl_rd_3_c downto ctrl_rd_0_c) <= M_FLAG_I & link_reg_adr_c; -- link register
							CTRL_O(ctrl_rd_wb_c) <= '0'; -- disable write back
							if (INSTR_INT(3) = '0') then -- store to PC
								if ((M_FLAG_I = user_mode_c) and ((INSTR_INT(1 downto 0) /= "00") or (INSTR_INT(7) = '1'))) then
									CTRL_O(ctrl_cmd_err_c) <= '1'; -- access violation - cmd_err trap
								end if;
								CTRL_O(ctrl_pc_wr_c)                           <= '1'; -- write pc
								CTRL_O(ctrl_rb_is_imm_c)                       <= '1'; -- this is an immediate
								IMM_O                                          <= (others => '0'); -- zero
								CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_orr_c; -- logical or with 0
								CTRL_O(ctrl_ctx_down_c)                        <= INSTR_INT(0); -- goto user mode when bit 0 = '1'
								CTRL_O(ctrl_re_xint_c)                         <= INSTR_INT(1); -- re-enable global xint flag
								CTRL_O(ctrl_link_c)                            <= INSTR_INT(2); -- link
								CTRL_O(ctrl_rd_wb_c)                           <= INSTR_INT(2); -- allow write back for linking
								CTRL_O(ctrl_restsm_c)                          <= INSTR_INT(7); -- restore saved mode
							end if;

						when fs_inc_c | fs_add_c => -- immediate addition // addition
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_adc_c;

						when fs_dec_c => -- immediate subtraction
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_sbc_c;

						when fs_sub_c => -- subtraction
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_sbc_c;
							if (redundant_reg_v = '1') then -- SUB instruction with Ra = Rb: Rd = 0 - Ra (NEG Rd, Ra)
								CTRL_O(ctrl_clr_la_c) <= '1'; -- set low byte of A to 0
								CTRL_O(ctrl_clr_ha_c) <= '1'; -- set high byte of A to 0
							end if;

						when fs_adc_c => -- addition with carry
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_adc_c;
							CTRL_O(ctrl_alu_usec_c) <= '1'; -- use carry input

						when fs_sbc_c => -- subtraction with carry
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_sbc_c;
							CTRL_O(ctrl_alu_usec_c) <= '1'; -- use carry input
							if (redundant_reg_v = '1') then -- SBC instruction with Ra = Rb: Rd = 0 - Ra - C (NEC Rd, Ra)
								CTRL_O(ctrl_clr_la_c) <= '1'; -- set low byte of A to 0
								CTRL_O(ctrl_clr_ha_c) <= '1'; -- set high byte of A to 0
							end if;

						when fs_eor_c => -- logical xor
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_eor_c;

						when fs_nand_c => -- logical not-and
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_nand_c;

						when fs_bic_c => -- bit clear
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_bic_c;

						when fs_sft_c => -- shift operation
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_sft_c;

						when others => -- undefined
							NULL; -- use defaults

					end case;


				when "01" => -- Class 1: Memory Access
				-- ==============================================================================
					IMM_O(2 downto 0) <= INSTR_INT(2 downto 0); -- immediate offset
					if (INSTR_INT(12) = '1') then
						CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_adc_c; -- add index
					else
						CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_sbc_c; -- sub index
					end if;
					mem_acc_temp_v := INSTR_INT(10) & INSTR_INT(3) & INSTR_INT(13) & INSTR_INT(11); -- L,I,P,W
					case (mem_acc_temp_v) is

						when "0000" | "0100" => -- load, imm/reg offset, pre, no wb
							CTRL_O(ctrl_mem_acc_c)   <= '1'; -- this is a memory access
							CTRL_O(ctrl_rb_is_imm_c) <= INSTR_INT(3); -- this is an immediate
							CTRL_O(ctrl_rd_wb_c)     <= '1'; -- allow data write back

						when "0001" | "0101" => -- load, imm/reg offset, pre, do wb
							CTRL_O(ctrl_rb_is_imm_c) <= INSTR_INT(3); -- this is an immediate
							CTRL_O(ctrl_rd_wb_c)     <= '1'; -- allow data write back
							if (MULTI_CYC_I = '0') then -- fist cycle: ADD/SUB R_base, R_base, Offset
								CTRL_O(ctrl_rd_3_c downto ctrl_rd_0_c) <= M_FLAG_I & INSTR_INT(6 downto 4); -- base adr
								MULTI_CYC_REQ_O      <= '1'; -- prepare second cycle
							else -- second cycle: LD R_data, [R_base]
								CTRL_O(ctrl_mem_acc_c)  <= '1'; -- this is a memory access
								CTRL_O(ctrl_mem_bpba_c) <= '1'; -- use bypassed adr from prev cycle
							end if;

						when "0011" | "0111" => -- load, imm/reg offset, post, do wb
							CTRL_O(ctrl_rb_is_imm_c) <= INSTR_INT(3); -- this is an immediate
							CTRL_O(ctrl_rd_wb_c)     <= '1'; -- allow data write back
							if (MULTI_CYC_I = '0') then -- fist cycle: LD R_data, [R_base]
								CTRL_O(ctrl_mem_acc_c)  <= '1'; -- this is a memory access
								CTRL_O(ctrl_mem_bpba_c) <= '1'; -- use bypassed adr from prev cycle
								MULTI_CYC_REQ_O         <= '1'; -- prepare second cycle
							else -- second cycle: ADD/SUB R_base, R_base, Offset
								CTRL_O(ctrl_rd_3_c downto ctrl_rd_0_c) <= M_FLAG_I & INSTR_INT(6 downto 4); -- base adr
							end if;

						when "1000" | "1001" => -- store, reg offset, pre, (no) wb
							if (MULTI_CYC_I = '0') then -- fist cycle: ADD/SUB R_base, R_base, R_offset
								CTRL_O(ctrl_rd_3_c downto ctrl_rd_0_c) <= M_FLAG_I & INSTR_INT(6 downto 4); -- base adr
								CTRL_O(ctrl_rd_wb_c)   <= INSTR_INT(11); -- write back base?
								MULTI_CYC_REQ_O        <= '1'; -- prepare second cycle
							else -- second cycle: ST R_data, [R_base]
								CTRL_O(ctrl_rb_3_c downto ctrl_rb_0_c) <= M_FLAG_I & INSTR_INT(9 downto 7); -- store data
								CTRL_O(ctrl_mem_daa_c) <= '1'; -- use delayed adr from prev cycle
								CTRL_O(ctrl_mem_acc_c) <= '1'; -- this is a memory access
								CTRL_O(ctrl_mem_wr_c)  <= '1'; -- write access
							end if;

						when "1011" =>  -- store, reg offset, post, do wb
							if (MULTI_CYC_I = '0') then -- fist cycle: ST R_data, [R_base]
								CTRL_O(ctrl_rb_3_c downto ctrl_rb_0_c) <= M_FLAG_I & INSTR_INT(9 downto 7); -- store data
								CTRL_O(ctrl_mem_bpba_c) <= '1'; -- use bypassed adr from prev cycle
								CTRL_O(ctrl_mem_acc_c)  <= '1'; -- this is a memory access
								CTRL_O(ctrl_mem_wr_c)   <= '1'; -- write access
								MULTI_CYC_REQ_O         <= '1'; -- prepare second cycle
							else -- second cycle: ADD/SUB R_base, R_base, R_offset
								CTRL_O(ctrl_rd_3_c downto ctrl_rd_0_c) <= M_FLAG_I & INSTR_INT(6 downto 4); -- base adr
								CTRL_O(ctrl_rd_wb_c)    <= '1'; -- write back base
							end if;

						when "1100" | "1101" | "1111" => -- store, imm offset, pre/post, (no) wb
							CTRL_O(ctrl_rd_3_c downto ctrl_rd_0_c) <= M_FLAG_I & INSTR_INT(6 downto 4); -- base adr
							CTRL_O(ctrl_rb_3_c downto ctrl_rb_0_c) <= M_FLAG_I & INSTR_INT(9 downto 7); -- store data
							CTRL_O(ctrl_rb_is_imm_c) <= '1'; -- this is an immediate
							CTRL_O(ctrl_mem_acc_c)   <= '1'; -- this is a memory access
							CTRL_O(ctrl_mem_wr_c)    <= '1'; -- write access
							CTRL_O(ctrl_mem_bpba_c)  <= INSTR_INT(13); -- use bypassed adr base
							CTRL_O(ctrl_rd_wb_c)     <= INSTR_INT(11); -- write back base

						-- Data Swap Operations R_b => M[R_a] => R_d --------------------------------
						when "0010" | "0110" | "1010" | "1110" => -- load/store, imm/reg offset, post, no wb [REDUNDANT!]
							CTRL_O(ctrl_mem_acc_c)   <= '1'; -- this is a memory access
							CTRL_O(ctrl_mem_bpba_c)  <= '1'; -- use bypassed adr from prev cycle
							CTRL_O(ctrl_rb_is_imm_c) <= '1'; -- this is an immediate (pseudo)
							if (MULTI_CYC_I = '0') then -- first cycle: LD R_d, [R_a]
								CTRL_O(ctrl_rd_wb_c)  <= '1'; -- write back base
								MULTI_CYC_REQ_O       <= '1'; -- prepare second cycle
							else -- second cycle: ST R_b, [R_a]
								CTRL_O(ctrl_mem_wr_c) <= '1'; -- write access
							end if;

						when others => -- undefined
							NULL; -- wayne ^^

					end case;


				when "10" => -- Class 2: Branch and Link
				-- ==============================================================================
					CTRL_O(ctrl_branch_c)    <= '1'; -- this is a branch
					CTRL_O(ctrl_link_c)      <= INSTR_INT(9); -- link?
					CTRL_O(ctrl_ra_is_pc_c)  <= '1'; -- operand A is the pc
					CTRL_O(ctrl_rb_is_imm_c) <= '1'; -- operand B is an immediate
					CTRL_O(ctrl_rd_wb_c)     <= INSTR_INT(9); -- allow write back for linking
					CTRL_O(ctrl_rd_3_c     downto ctrl_rd_0_c)     <= M_FLAG_I & link_reg_adr_c; -- link register
					CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_adc_c; -- add offset (without carry)
					if (word_mode_en_c = false) then -- byte addressing mode
						IMM_O(9 downto 0) <= INSTR_INT(8 downto 0) & '0'; -- offset = offset * 2 (byte offset)
						for i in 10 to data_width_c-1 loop
							IMM_O(i) <= INSTR_INT(8); -- sign extension
						end loop;
					else -- word addressing mode
						IMM_O(8 downto 0) <= INSTR_INT(8 downto 0); -- offset = offset (word offset)
						for i in 9 to data_width_c-1 loop
							IMM_O(i) <= INSTR_INT(8); -- sign extension
						end loop;
					end if;


				when "11" => -- Class 3: Sub Classes
				-- ==============================================================================
					case (INSTR_INT(13 downto 12)) is

						when "00" => -- Class 3a: Load Immediate
						-- --------------------------------------------------------------------------------
							CTRL_O(ctrl_rd_wb_c)                           <= '1'; -- allow write back
							CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_orr_c; -- logical OR
							CTRL_O(ctrl_ra_3_c downto ctrl_ra_0_c)         <= M_FLAG_I & INSTR_INT(9 downto 7); -- op A = source & destination
							CTRL_O(ctrl_rb_is_imm_c)                       <= '1'; -- B is an immediate
							if (INSTR_INT(11) = '0') then -- load and expand low part
								CTRL_O(ctrl_clr_la_c) <= '1'; -- set low byte of A to 0
								IMM_O(7 downto 0) <= INSTR_INT(10) & INSTR_INT(6 downto 0);
								if (ldil_sign_ext_c = true) then -- use sign extension
									for i in 8 to data_width_c-1 loop -- sign extension
										IMM_O(i) <= INSTR_INT(10);
									end loop;
									CTRL_O(ctrl_clr_ha_c) <= '1'; -- set high byte of A to 0
								end if;
							else -- load high part
								IMM_O(15 downto 8) <= INSTR_INT(10) & INSTR_INT(6 downto 0);
								CTRL_O(ctrl_clr_ha_c) <= '1'; -- set high byte of A to 0
							end if;	
							

						when "01" => -- Class 3b: Bit Transfer
						-- --------------------------------------------------------------------------------
							CTRL_O(ctrl_rb_is_imm_c) <= '1'; -- B is an immediate
							case (INSTR_INT(11 downto 10)) is
								when "00" => -- modifiy bit -> clear bit
									IMM_O(to_integer(unsigned(INSTR_INT(3 downto 0)))) <= '1';
									CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c)     <= alu_bic_c; -- bit clear
									CTRL_O(ctrl_rd_wb_c)                               <= '1'; -- allow write back
								when "01" => -- modify bit -> set bit
									IMM_O(to_integer(unsigned(INSTR_INT(3 downto 0)))) <= '1';
									CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c)     <= alu_orr_c; -- logical or
									CTRL_O(ctrl_rd_wb_c)                               <= '1'; -- allow write back
								when "10" => -- T-flag transfer, load from T
									IMM_O(to_integer(unsigned(INSTR_INT(3 downto 0)))) <= '1';
									CTRL_O(ctrl_rd_wb_c)                               <= '1'; -- allow write back
									if (T_FLAG_I = '0') then
										CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_bic_c; -- bit clear
									else
										CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_orr_c; -- logical or
									end if;
								when others => -- "11" -- T-flag transfer, store to T
									IMM_O(3 downto 0)        <= INSTR_INT(3 downto 0);
									CTRL_O(ctrl_rb_is_imm_c) <= not INSTR_INT(9); -- B is an immediate or REG
									CTRL_O(ctrl_tf_store_c)  <= '1'; -- store to t-flag
									CTRL_O(ctrl_tf_inv_c)    <= INSTR_INT(7); -- invert bit to be transfered to T-flag
									CTRL_O(ctrl_get_par_c)   <= INSTR_INT(8); -- get parity bit of OP_A
							end case;


						when "10" => -- Class 3c: Coprocessor Access
						-- --------------------------------------------------------------------------------
							CTRL_O(ctrl_cp_acc_c)   <= '1'; -- this is a cp access
                            CTRL_O(ctrl_cp_trans_c) <= INSTR_INT(11); -- data transfer/access
							if (INSTR_INT(11) = '1') then -- data transfer
								CTRL_O(ctrl_cp_wr_c)    <= INSTR_INT(3); -- read / write
								CTRL_O(ctrl_rd_wb_c)    <= not INSTR_INT(3); -- allow write back
							end if;
							if (M_FLAG_I = user_mode_c) then -- access violation?
								if ((CP_PTC_I = '1') and (INSTR_INT(10) = '0')) or (INSTR_INT(10) = '1') then -- unauthorized acces?
									CTRL_O(ctrl_cmd_err_c) <= '1'; -- access violation/undefined instruction - cmd_err trap
								end if;
							end if;


						when others => -- Class 3d: Sub Sub Classes
						-- ==============================================================================
							case (INSTR_INT(11 downto 10)) is

								when "00" => -- Class 3c0: Multiply-and-Accumulate
								-- --------------------------------------------------------------------------------
									CTRL_O(ctrl_rd_wb_c) <= '1'; -- allow write back
									if (INSTR_INT(3) = '1') then -- MAC
										if (build_mac_c = true) then -- unit present?
											if (MULTI_CYC_I = '0') then -- fist cycle: load MAC buffer
												CTRL_O(ctrl_rb_3_c downto ctrl_rb_0_c) <= M_FLAG_I & INSTR_INT(9 downto 7); -- mac offset
												CTRL_O(ctrl_load_mac_c) <= '1'; -- load mac buffer
												MULTI_CYC_REQ_O         <= '1'; -- prepare second cycle
											else -- second cycle: MUL operation with offset = MAC
												CTRL_O(ctrl_use_offs_c) <= '1'; -- use loaded offset
												CTRL_O(ctrl_use_mac_c)  <= '1'; -- use mac unit
											end if;
										else -- not present
											CTRL_O(ctrl_cmd_err_c) <= '1'; -- invalid instruction - cmd_err trap
										end if;
									else -- MUL
										if (build_mul_c = true) then -- unit present?
											CTRL_O(ctrl_use_mac_c) <= '1'; -- use mac unit
										else -- not present
											CTRL_O(ctrl_cmd_err_c) <= '1'; -- invalid instruction - cmd_err trap
										end if;
									end if;


								when "01" => -- Class 3c1: Special (Sleep, Reg-based branch)
								-- --------------------------------------------------------------------------------
									if (INSTR_INT(9) = '0') then -- SLEEP mode
                                        if (M_FLAG_I = user_mode_c) then -- access violation?
                                            CTRL_O(ctrl_cmd_err_c) <= '1'; -- access violation - cmd_err trap
                                        else
                                            CTRL_O(ctrl_sleep_c)   <= '1'; -- go to sleep
                                        end if;
									elsif (reg_branches_en_c = true) then -- register-based branches enabled
										CTRL_O(ctrl_cond_3_c   downto ctrl_cond_0_c)   <= INSTR_INT(6 downto 3); -- branch condition
										CTRL_O(ctrl_rd_3_c     downto ctrl_rd_0_c)     <= M_FLAG_I & link_reg_adr_c; -- link register
										CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_adc_c; -- add offset (without carry)
										CTRL_O(ctrl_branch_c)   <= '1'; -- this is a branch
										CTRL_O(ctrl_link_c)     <= INSTR_INT(7); -- link?
										CTRL_O(ctrl_rd_wb_c)    <= INSTR_INT(7); -- allow write back for linking
										CTRL_O(ctrl_ra_is_pc_c) <= '1'; -- operand A is the pc
										CTRL_O(ctrl_clr_la_c)   <= INSTR_INT(8); -- set low byte of A to 0
										CTRL_O(ctrl_clr_ha_c)   <= INSTR_INT(8); -- set high byte of A to 0
									else
										CTRL_O(ctrl_cmd_err_c)  <= '1'; -- undefined instruction - cmd_err trap
									end if;


								when "10" => -- Class 3c2: Conditional MOVE = if (COND=TRUE) then Rd <= Rb
								-- --------------------------------------------------------------------------------
                                    if (cond_moves_en_c = true) then -- conditional moves enabled
                                        CTRL_O(ctrl_cond_3_c   downto ctrl_cond_0_c)   <= INSTR_INT(6 downto 3); -- branch condition
                                        CTRL_O(ctrl_alu_fs_2_c downto ctrl_alu_fs_0_c) <= alu_orr_c; -- logical OR
                                        CTRL_O(ctrl_rd_wb_c)   <= '1'; -- allow write back
                                        CTRL_O(ctrl_clr_la_c)  <= '1'; -- set low byte of A to 0
                                        CTRL_O(ctrl_clr_ha_c)  <= '1'; -- set high byte of A to 0
                                        CTRL_O(ctrl_cond_wb_c) <= '1'; -- is conditional write back
									else
										CTRL_O(ctrl_cmd_err_c)  <= '1'; -- undefined instruction - cmd_err trap
									end if;


								when others => -- Class 3c3: System Call with 10-bit tag
								-- --------------------------------------------------------------------------------
									CTRL_O(ctrl_syscall_c) <= '1'; -- is system call

							end case;

					end case;


				when others => -- undefined
				-- ==============================================================================
					NULL; -- wayne...


			end case;
		
		end process OPCODE_DECODER;




end OP_DEC_STRUCTURE;
