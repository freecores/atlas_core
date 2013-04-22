-- ########################################################
-- #         << ATLAS Project - System Package >>         #
-- # **************************************************** #
-- #  All architecture configurations, options, signal    #
-- #  definitions and components are listed here.         #
-- # **************************************************** #
-- #  Last modified: 17.04.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package atlas_core_package is

  -- Architecture Configuration -------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
    constant big_endian_c           : boolean := false; -- use little/big endian memory system
	constant cp0_present_c          : boolean := false; -- coprocessor 0 (usr cp) present?
	constant cp1_present_c          : boolean := true;  -- coprocessor 1 (sys cp) present?
	constant build_mul_c            : boolean := true;  -- build a dedicated MUL unit
	constant build_mac_c            : boolean := false; -- build a dedicated MAC unit
	constant ldil_sign_ext_c        : boolean := true;  -- use sign extension when loading low byte
	constant log2_cache_pages_c     : natural := 2;     -- address bits to specify number of cache pages, max 5
	constant log2_cache_page_size_c : natural := 5;     -- address bits to specify cache page size (in words)
	constant max_bus_latency_c      : natural := (2**log2_cache_page_size_c)/2; -- max wb bus cycle latency
	constant word_mode_en_c         : boolean := false; -- use word-addressed memory system instead of byte-addressed

	---- DO NOT CHANGE ANYTHING BELOW UNLESS YOU REALLY KNOW WHAT YOU ARE DOING! ----

  -- Architecture Constants -----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	constant data_width_c      : natural := 16; -- processing data width
	constant data_bytes_c      : natural := data_width_c/8; -- processing data width in bytes
	constant align_lsb_c       : natural := data_bytes_c/2; -- lsb of adr word boundary
	constant cache_pages_c     : natural := 2**log2_cache_pages_c; -- number of cache pages, max 32
	constant cache_page_size_c : natural := 2**log2_cache_page_size_c; -- size of cache page in words
	constant bus_adr_width_c   : natural := 32; -- wishbone bus address width
	constant link_reg_adr_c    : std_logic_vector(2 downto 0) := "111"; -- link reg for calls
	constant stack_pnt_adr_c   : std_logic_vector(2 downto 0) := "110"; -- stack pointer
	constant user_mode_c       : std_logic := '0'; -- user mode indicator
	constant system_mode_c     : std_logic := '1'; -- system mode indicator
	constant branch_slots_en_c : boolean := false; -- use branch delay slots (highly experimental!!!)


  -- Wishbone Bus Constants -----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	constant wb_classic_cyc_c  : std_logic_vector(2 downto 0) := "000"; -- classic cycle
	constant wb_con_bst_cyc_c  : std_logic_vector(2 downto 0) := "001"; -- constant address burst
	constant wb_inc_bst_cyc_c  : std_logic_vector(2 downto 0) := "010"; -- incrementing address burst
	constant wb_end_cyc_cyc_c  : std_logic_vector(2 downto 0) := "111"; -- burst end


  -- Machine Status Register ----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	constant msr_usr_z_flag_c  : natural := 0;  -- user mode zero flag
	constant msr_usr_c_flag_c  : natural := 1;  -- user mode carry flag
	constant msr_usr_o_flag_c  : natural := 2;  -- user mode overflow flag
	constant msr_usr_n_flag_c  : natural := 3;  -- user mode negative flag
	constant msr_usr_t_flag_c  : natural := 4;  -- user mode transfer flag
	constant msr_sys_z_flag_c  : natural := 5;  -- system mode zero flag
	constant msr_sys_c_flag_c  : natural := 6;  -- system mode carry flag
	constant msr_sys_o_flag_c  : natural := 7;  -- system mode overflow flag
	constant msr_sys_n_flag_c  : natural := 8;  -- system mode negative flag
	constant msr_sys_t_flag_c  : natural := 9;  -- system mode transfer flag
	constant msr_usr_cp_ptc_c  : natural := 10; -- user coprocessor protected
	constant msr_xint_en_c     : natural := 11; -- enable external interrupts (global)
	constant msr_xint0_en_c    : natural := 12; -- enable external interrupt 0
	constant msr_xint1_en_c    : natural := 13; -- enable external interrupt 1
	constant msr_svd_mode_c    : natural := 14; -- saved operating mode
	constant msr_mode_flag_c   : natural := 15; -- system ('1') / user ('0') mode


  -- Forwarding Bus -------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	constant fwd_en_c          : natural := 0;  -- valid register signal
	constant fwd_adr_0_c       : natural := 1;  -- address bit 0
	constant fwd_adr_1_c       : natural := 2;  -- address bit 1
	constant fwd_adr_2_c       : natural := 3;  -- address bit 2
	constant fwd_adr_3_c       : natural := 4;  -- address bit 3 (bank select)
	constant fwd_dat_lsb_c     : natural := 5;  -- forwarding data lsb
	constant fwd_dat_msb_c     : natural := 5+data_width_c-1; -- forwarding data msb
	constant fwd_width_c       : natural := 5+data_width_c;   -- size of forwarding bus


  -- Flag Bus -------------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	constant flag_z_c          : natural := 0;  -- user mode zero flag
	constant flag_c_c          : natural := 1;  -- user mode carry flag
	constant flag_o_c          : natural := 2;  -- user mode overflow flag
	constant flag_n_c          : natural := 3;  -- user mode negative flag
	constant flag_t_c          : natural := 4;  -- user mode transfer flag
	constant flag_bus_width_c  : natural := 5;  -- size of flag bus

	-- Freude, schöner Götterfunken,
	-- Tochter aus Elysium,
	-- Wir betreten feuertrunken,
	-- Himmlische, dein Heiligthum!
	-- Deine Zauber binden wieder
	-- Was die Mode streng geteilt;
	-- Alle Menschen werden Brüder,
	-- Wo dein sanfter Flügel weilt.


  -- Main Control Bus -----------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	-- Global Control --
	constant ctrl_en_c         : natural := 0;  -- valid cycle
	constant ctrl_mcyc_c       : natural := 1;  -- un-interruptable/atomic operation

	-- Operand A Register --
	constant ctrl_ra_is_pc_c   : natural := 2;  -- operand register A is the PC
	constant ctrl_clr_ha_c     : natural := 3;  -- set higher half word of A to 0 (@ 16 bit)
	constant ctrl_clr_la_c     : natural := 4;  -- set lower half word of A to 0 (@ 16 bit)
	constant ctrl_ra_0_c       : natural := 5;  -- operand register A adr bit 0
	constant ctrl_ra_1_c       : natural := 6;  -- operand register A adr bit 1
	constant ctrl_ra_2_c       : natural := 7;  -- operand register A adr bit 2
	constant ctrl_ra_3_c       : natural := 8;  -- operand register A adr bit 3 (bank select)

	-- Operand B Register --
	constant ctrl_rb_is_imm_c  : natural := 9;  -- operand register B is an immediate
	constant ctrl_rb_0_c       : natural := 10; -- operand register B adr bit 0
	constant ctrl_rb_1_c       : natural := 11; -- operand register B adr bit 1
	constant ctrl_rb_2_c       : natural := 12; -- operand register B adr bit 2
	constant ctrl_rb_3_c       : natural := 13; -- operand register B adr bit 3 (bank select)

	-- Destiantion Register --
	constant ctrl_rd_wb_c      : natural := 14; -- register write back request
	constant ctrl_rd_0_c       : natural := 15; -- register destination adr bit 0
	constant ctrl_rd_1_c       : natural := 16; -- register destination adr bit 1
	constant ctrl_rd_2_c       : natural := 17; -- register destination adr bit 2
	constant ctrl_rd_3_c       : natural := 18; -- register destination adr bit 3 (bank select)

	-- ALU Control --
	constant ctrl_alu_fs_0_c   : natural := 19; -- alu function set bit 0
	constant ctrl_alu_fs_1_c   : natural := 20; -- alu function set bit 1
	constant ctrl_alu_fs_2_c   : natural := 21; -- alu function set bit 2
	constant ctrl_alu_usec_c   : natural := 22; -- alu use MSR(carry_flag)
	constant ctrl_alu_usez_c   : natural := 23; -- alu use MSR(zero_flag)
	constant ctrl_fupdate_c    : natural := 24; -- msr flag update enable

	-- Bit Manipulation --
	constant ctrl_tf_store_c   : natural := 25; -- store bit to t-flag
	constant ctrl_tf_inv_c     : natural := 26; -- invert bit to be store in t-flag
	constant ctrl_get_par_c    : natural := 27; -- get parity bit
	constant ctrl_bit_0_c      : natural := 28; -- bit index bit 0
	constant ctrl_bit_1_c      : natural := 29; -- bit index bit 1
	constant ctrl_bit_2_c      : natural := 30; -- bit index bit 2
	constant ctrl_bit_3_c      : natural := 31; -- bit index bit 3

	-- System Register Access --
	constant ctrl_msr_wr_c     : natural := 32; -- write to mcr
	constant ctrl_msr_rd_c     : natural := 33; -- read from mcr
	constant ctrl_pc_wr_c      : natural := 34; -- write pc

	-- Branch/Context Control --
	constant ctrl_cond_0_c     : natural := 35; -- condition code bit 0
	constant ctrl_cond_1_c     : natural := 36; -- condition code bit 1
	constant ctrl_cond_2_c     : natural := 37; -- condition code bit 2
	constant ctrl_cond_3_c     : natural := 38; -- condition code bit 3
	constant ctrl_branch_c     : natural := 39; -- is branch operation
	constant ctrl_link_c       : natural := 40; -- store old pc to lr
	constant ctrl_syscall_c    : natural := 41; -- is a system call
	constant ctrl_ctx_down_c   : natural := 42; -- go to user mode
	constant ctrl_restsm_c     : natural := 43; -- restore saved mode

	-- Memory Access --
	constant ctrl_mem_acc_c    : natural := 44; -- request d-mem access
	constant ctrl_mem_wr_c     : natural := 45; -- write to d-mem
	constant ctrl_mem_bpba_c   : natural := 46; -- use bypassed base address
	constant ctrl_mem_daa_c    : natural := 47; -- use delayed address

	-- Coprocessor Access --
	constant ctrl_cp_acc_c     : natural := 48; -- coprocessor operation
	constant ctrl_cp_trans_c   : natural := 49; -- coprocessor data transfer
	constant ctrl_cp_wr_c      : natural := 50; -- write to coprocessor
	constant ctrl_cp_id_c      : natural := 51; -- coprocessor id bit

	-- Multiply-and-Acuumulate Unit --
	constant ctrl_use_mac_c    : natural := 52; -- use MAC unit
	constant ctrl_load_mac_c   : natural := 53; -- load addition buffer for MAC
	constant ctrl_use_offs_c   : natural := 54; -- use loaded offset

--	-- EX Forwarding --
--	constant ctrl_a_ex_ma_fw_c : natural := 55;
--	constant ctrl_a_ex_wb_fw_c : natural := 56;
--	constant ctrl_b_ex_ma_fw_c : natural := 57;
--	constant ctrl_b_ex_wb_fw_c : natural := 58;
--	constant ctrl_c_ex_wb_fw_c : natural := 59;

	-- Bus Size --
	constant ctrl_width_c      : natural := 55; -- control bus size

	-- Progress Redefinitions --
	constant ctrl_wb_en_c      : natural := ctrl_rd_wb_c;   -- valid write back
	constant ctrl_rd_mem_acc_c : natural := ctrl_mem_acc_c; -- true mem_read
	constant ctrl_rd_cp_acc_c  : natural := ctrl_cp_acc_c;  -- true cp_read
	constant ctrl_cp_msr_rd_c  : natural := ctrl_msr_rd_c;  -- true cp or msr read access
	constant ctrl_cp_cmd_0_c   : natural := ctrl_rb_0_c;    -- coprocessor cmd bit 0
	constant ctrl_cp_cmd_1_c   : natural := ctrl_rb_1_c;    -- coprocessor cmd bit 1
	constant ctrl_cp_cmd_2_c   : natural := ctrl_rb_2_c;    -- coprocessor cmd bit 2
	constant ctrl_cp_ra_0_c    : natural := ctrl_ra_0_c;    -- coprocessor op A bit 0
	constant ctrl_cp_ra_1_c    : natural := ctrl_ra_1_c;    -- coprocessor op A bit 1
	constant ctrl_cp_ra_2_c    : natural := ctrl_ra_2_c;    -- coprocessor op A bit 2
	constant ctrl_cp_rd_0_c    : natural := ctrl_rd_0_c;    -- coprocessor op B / dest bit 0
	constant ctrl_cp_rd_1_c    : natural := ctrl_rd_1_c;    -- coprocessor op B / dest bit 1
	constant ctrl_cp_rd_2_c    : natural := ctrl_rd_2_c;    -- coprocessor op B / dest bit 2
	constant ctrl_re_xint_c    : natural := ctrl_rb_1_c;    -- re-enable ext interrupts (global)
	constant ctrl_msr_am_0_c   : natural := ctrl_ra_1_c;    -- MSR access mode bit 0
	constant ctrl_msr_am_1_c   : natural := ctrl_ra_2_c;    -- MSR access mode bit 1


  -- Coprocessor Control Bus ----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	constant cp_cmd_lsb_c      : natural := 0; -- command word lsb
	constant cp_cmd_msb_c      : natural := 2; -- command word msb
	constant cp_op_b_lsb_c     : natural := 3; -- operand B address lsb
	constant cp_op_b_msb_c     : natural := 5; -- operand B address msb
	constant cp_op_a_lsb_c     : natural := 6; -- operand A / destination address lsb
	constant cp_op_a_msb_c     : natural := 8; -- operand A / destination address msb
	constant cp_cmd_width_c    : natural := 9; -- bus size


  -- Condition Codes ------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	constant cond_eq_c         : std_logic_vector(3 downto 0) := "0000"; -- equal
	constant cond_ne_c         : std_logic_vector(3 downto 0) := "0001"; -- not equal
	constant cond_cs_c         : std_logic_vector(3 downto 0) := "0010"; -- unsigned higher or same
	constant cond_cc_c         : std_logic_vector(3 downto 0) := "0011"; -- unsigned lower
	constant cond_mi_c         : std_logic_vector(3 downto 0) := "0100"; -- negative
	constant cond_pl_c         : std_logic_vector(3 downto 0) := "0101"; -- positive or zero
	constant cond_os_c         : std_logic_vector(3 downto 0) := "0110"; -- overflow
	constant cond_oc_c         : std_logic_vector(3 downto 0) := "0111"; -- no overflow
	constant cond_hi_c         : std_logic_vector(3 downto 0) := "1000"; -- unsigned higher
	constant cond_ls_c         : std_logic_vector(3 downto 0) := "1001"; -- unsigned lower or same
	constant cond_ge_c         : std_logic_vector(3 downto 0) := "1010"; -- greater than or equal
	constant cond_lt_c         : std_logic_vector(3 downto 0) := "1011"; -- less than
	constant cond_gt_c         : std_logic_vector(3 downto 0) := "1100"; -- greater than
	constant cond_le_c         : std_logic_vector(3 downto 0) := "1101"; -- less than or equal
	constant cond_ts_c         : std_logic_vector(3 downto 0) := "1110"; -- transfer flag set
	constant cond_al_c         : std_logic_vector(3 downto 0) := "1111"; -- always


  -- ALU Function Select --------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	constant fs_inc_c          : std_logic_vector(3 downto 0) := "0000"; -- add immediate
	constant fs_dec_c          : std_logic_vector(3 downto 0) := "0001"; -- subtract immediate
	constant fs_add_c          : std_logic_vector(3 downto 0) := "0010"; -- add
	constant fs_adc_c          : std_logic_vector(3 downto 0) := "0011"; -- add with carry
	constant fs_sub_c          : std_logic_vector(3 downto 0) := "0100"; -- subtract
	constant fs_sbc_c          : std_logic_vector(3 downto 0) := "0101"; -- subtract with carry
	constant fs_cmp_c          : std_logic_vector(3 downto 0) := "0110"; -- compare (sub)
	constant fs_cpx_c          : std_logic_vector(3 downto 0) := "0111"; -- extende compare with flags (sbc)
	constant fs_and_c          : std_logic_vector(3 downto 0) := "1000"; -- logical and
	constant fs_orr_c          : std_logic_vector(3 downto 0) := "1001"; -- logical or
	constant fs_eor_c          : std_logic_vector(3 downto 0) := "1010"; -- logical xor
	constant fs_nand_c         : std_logic_vector(3 downto 0) := "1011"; -- logical nand
	constant fs_bic_c          : std_logic_vector(3 downto 0) := "1100"; -- bit clear
	constant fs_teq_c          : std_logic_vector(3 downto 0) := "1101"; -- compare by logical and
	constant fs_tst_c          : std_logic_vector(3 downto 0) := "1110"; -- compare by logical xor
	constant fs_sft_c          : std_logic_vector(3 downto 0) := "1111"; -- shift operation

	-- Pseudo Intructions --
	constant fs_ld_user_c      : std_logic_vector(3 downto 0) := fs_orr_c; -- load from user bank
	constant fs_st_user_c      : std_logic_vector(3 downto 0) := fs_and_c; -- store to user bank
	constant fs_ld_msr_c       : std_logic_vector(3 downto 0) := fs_cmp_c; -- load from msr
	constant fs_st_msr_c       : std_logic_vector(3 downto 0) := fs_cpx_c; -- store to msr
	constant fs_ld_pc_c        : std_logic_vector(3 downto 0) := fs_tst_c; -- load from pc
	constant fs_st_pc_c        : std_logic_vector(3 downto 0) := fs_teq_c; -- store to pc

	-- Elementary ALU Operations --
	constant alu_adc_c         : std_logic_vector(2 downto 0) := "000"; -- add with carry
	constant alu_sbc_c         : std_logic_vector(2 downto 0) := "001"; -- subtract with carry
	constant alu_and_c         : std_logic_vector(2 downto 0) := "010"; -- logical and
	constant alu_orr_c         : std_logic_vector(2 downto 0) := "011"; -- logical or
	constant alu_eor_c         : std_logic_vector(2 downto 0) := "100"; -- logical xor
	constant alu_nand_c        : std_logic_vector(2 downto 0) := "101"; -- logical nand
	constant alu_bic_c         : std_logic_vector(2 downto 0) := "110"; -- bit clear
	constant alu_sft_c         : std_logic_vector(2 downto 0) := "111"; -- shift operation


  -- Shifter Control ------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	constant sft_swp_c         : std_logic_vector(2 downto 0) := "000"; -- swap halfwords
	constant sft_asr_c         : std_logic_vector(2 downto 0) := "001"; -- arithemtical right shift
	constant sft_rol_c         : std_logic_vector(2 downto 0) := "010"; -- rotate left
	constant sft_ror_c         : std_logic_vector(2 downto 0) := "011"; -- rotate right
	constant sft_lsl_c         : std_logic_vector(2 downto 0) := "100"; -- logical shift left
	constant sft_lsr_c         : std_logic_vector(2 downto 0) := "101"; -- logical shift right
	constant sft_rlc_c         : std_logic_vector(2 downto 0) := "110"; -- rotate left through carry
	constant sft_rrc_c         : std_logic_vector(2 downto 0) := "111"; -- rotate right through carry


  -- Cool Stuff -----------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	-- S: Carrie Underwood - Thank God For The Hometowns
	-- M: Precious - Das Leben ist kostbar
	-- M: Mean Creek
	-- S: Mumford & Sons - Lover of the Light
	-- M: 127 Hours

  -- Functions ------------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	function log2(temp : natural) return natural; -- logarithm base 2


  -- Component: Data Register File ----------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component REG_FILE
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				CE_I            : in  std_logic; -- clock enable
				RST_I           : in  std_logic; -- global reset line, sync, high-active

				-- Function Control --
				WB_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- wb stage control
				OF_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- of stage control

				-- Data Input --
				WB_DATA_I       : in  std_logic_vector(data_width_c-1 downto 0); -- write back data
				IMMEDIATE_I     : in  std_logic_vector(data_width_c-1 downto 0); -- immediates
				PC_1D_I         : in  std_logic_vector(data_width_c-1 downto 0); -- pc 1x delayed
				WB_FWD_I        : in  std_logic_vector(fwd_width_c-1  downto 0); -- WB stage forwarding path

				-- Data Output --
				OP_A_DATA_O     : out std_logic_vector(data_width_c-1 downto 0); -- operand A output
				OP_B_DATA_O     : out std_logic_vector(data_width_c-1 downto 0); -- operand B output
				OP_C_DATA_O     : out std_logic_vector(data_width_c-1 downto 0)  -- operand C output
			);
  end component;


  -- Component: Arithmetic/Logic Unit -------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component ALU
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				CE_I            : in  std_logic; -- clock enable
				RST_I           : in  std_logic; -- global reset line, sync, high-active

				-- Function Control --
				EX_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- stage control
				FLAG_BUS_I      : in  std_logic_vector(flag_bus_width_c-1 downto 0); -- flag input

				-- Data Input --
				OP_A_I          : in  std_logic_vector(data_width_c-1 downto 0); -- operand A input
				OP_B_I          : in  std_logic_vector(data_width_c-1 downto 0); -- operand B input
				OP_C_I          : in  std_logic_vector(data_width_c-1 downto 0); -- operand C input
				PC_1D_I         : in  std_logic_vector(data_width_c-1 downto 0); -- 1x delayed PC
				MA_FWD_I        : in  std_logic_vector(fwd_width_c-1  downto 0); -- MA stage forwarding path
				WB_FWD_I        : in  std_logic_vector(fwd_width_c-1  downto 0); -- WB stage forwarding path

				-- Data Output --
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
  end component;


  -- Component: Machine Status System -------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component SYS_REG
	generic (
				BOOT_ADDRESS_G  : std_logic_vector(data_width_c-1 downto 0) := (others => '0') -- boot address
			  );
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				CE_I            : in  std_logic; -- clock enable
				RST_I           : in  std_logic; -- global reset line, asyc

				-- Function Control --
				EX_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- ex stage control
				MA_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- ma stage control
				EXT_INT_REQ0_I  : in  std_logic; -- external interrupt request 0
				EXT_INT_REQ1_I  : in  std_logic; -- external interrupt request 1

				-- Data Input --
				FLAG_BUS_I      : in  std_logic_vector(flag_bus_width_c-1 downto 0); -- flag input
				EXC_POS_I       : in  std_logic; -- external interrupt would be possible
				STOP_PC         : in  std_logic; -- freeze pc
				PC_DATA_I       : in  std_logic_vector(data_width_c-1 downto 0); -- PC write data
				MSR_DATA_I      : in  std_logic_vector(data_width_c-1 downto 0); -- MSR write data

				-- Data Output --
				FLAG_BUS_O      : out std_logic_vector(flag_bus_width_c-1 downto 0); -- flag output
				VALID_BRANCH_O  : out std_logic; -- valid branch detected
				EXC_EXECUTED_O  : out std_logic; -- executed exception
				RD_MSR_O        : out std_logic_vector(data_width_c-1 downto 0); -- read data msr
				PC_O            : out std_logic_vector(data_width_c-1 downto 0); -- pc output
				PC_1D_O         : out std_logic_vector(data_width_c-1 downto 0); -- pc 1x delayed
				CP_PTC_O        : out std_logic; -- user coprocessor protection
				MODE_O          : out std_logic  -- current operating mode
			);
  end component;


  -- Component: Memory Access Control -------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component MEM_ACC
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				CE_I            : in  std_logic; -- clock enable
				RST_I           : in  std_logic; -- global reset line, asyc

				-- Function Control --
				MA_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- ma stage control

				-- Data Input --
				ALU_RES_I       : in  std_logic_vector(data_width_c-1 downto 0); -- alu result
				MAC_RES_I       : in  std_logic_vector(data_width_c-1 downto 0); -- mac result
				ADR_BASE_I      : in  std_logic_vector(data_width_c-1 downto 0); -- op_a bypass
				DATA_BP_I       : in  std_logic_vector(data_width_c-1 downto 0); -- op_b bypass
				CP_DATA_I       : in  std_logic_vector(data_width_c-1 downto 0); -- coprocessor rd data
				RD_MSR_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data msr
				WB_FWD_I        : in  std_logic_vector(fwd_width_c-1  downto 0); -- WB stage forwarding path

				-- Data Output --
				DATA_O          : out std_logic_vector(data_width_c-1 downto 0); -- data output
				MEM_ADR_FB_O    : out std_logic_vector(data_width_c-1 downto 0); -- memory address feedback
				MA_FWD_O        : out std_logic_vector(fwd_width_c-1  downto 0); -- MA stage forwarding path

				-- Memory (w) Interface --
				MEM_ADR_O       : out std_logic_vector(data_width_c-1 downto 0); -- address output
				MEM_DAT_O       : out std_logic_vector(data_width_c-1 downto 0); -- write data output
				MEM_RW_O        : out std_logic  -- read write
			);
  end component;


  -- Component: Data Write Back Unit --------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component WB_UNIT
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				CE_I            : in  std_logic; -- clock enable
				RST_I           : in  std_logic; -- global reset line, sync, high-active

				-- Function Control --
				WB_CTRL_BUS_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- wb stage control

				-- Data Input --
				MEM_WB_DAT_I    : in  std_logic_vector(data_width_c-1 downto 0); -- memory read data
				ALU_WB_DAT_I    : in  std_logic_vector(data_width_c-1 downto 0); -- alu read data
				MEM_ADR_FB_I    : in  std_logic_vector(data_width_c-1 downto 0); -- memory address feedback

				-- Data Output --
				WB_DATA_O       : out std_logic_vector(data_width_c-1 downto 0); -- write back data
				WB_FWD_O        : out std_logic_vector(fwd_width_c-1  downto 0)  -- WB stage forwarding path
			);
  end component;


  -- Component: Control System --------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component CTRL
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				CE_I            : in  std_logic; -- clock enable
				RST_I           : in  std_logic; -- global reset line, sync, high-active

				-- Decoder Interface --
				OP_DEC_CTRL_I   : in  std_logic_vector(ctrl_width_c-1 downto 0); -- decoder ctrl lines
				MULTI_CYC_O     : out std_logic;                                 -- multi-cycle indicator
				MULTI_CYC_REQ_I : in  std_logic;                                 -- multi-cycle request

				-- Control Lines --
				OF_CTRL_BUS_O   : out std_logic_vector(ctrl_width_c-1 downto 0); -- of stage control
				EX_CTRL_BUS_O   : out std_logic_vector(ctrl_width_c-1 downto 0); -- ex stage control
				MA_CTRL_BUS_O   : out std_logic_vector(ctrl_width_c-1 downto 0); -- ma stage control
				WB_CTRL_BUS_O   : out std_logic_vector(ctrl_width_c-1 downto 0); -- wb stage control

				-- Function Control --
				VALID_BRANCH_I  : in  std_logic; -- valid branch detected
				EXC_TAKEN_I     : in  std_logic; -- exception taken
				EXC_POS_O       : out std_logic; -- exception would be possible
				STOP_PC_O       : out std_logic; -- freeze program counter
				IR_UPDATE_EN_O  : out std_logic  -- enable instruction reg update
			);
  end component;


  -- Component: Opcode Decoder --------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component OP_DEC
	port	(
				-- Decoder Interface Input --
				INSTR_I         : in  std_logic_vector(data_width_c-1 downto 0); -- instruction input
				T_FLAG_I        : in  std_logic;                    -- T-Flag input
				M_FLAG_I        : in  std_logic;                    -- Mode flag input
				MULTI_CYC_I     : in  std_logic;                    -- multi-cycle indicator
				CP_PTC_I        : in  std_logic;                    -- user coprocessor protection

				-- Decoder Interface Output --
				MULTI_CYC_REQ_O : out std_logic;                                 -- multi-cycle reqest
				CTRL_O          : out std_logic_vector(ctrl_width_c-1 downto 0); -- decoder ctrl lines
				IMM_O           : out std_logic_vector(data_width_c-1 downto 0)  -- immediate
			);
  end component;


  -- Component: Atlas CPU Core --------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component ATLAS_CORE
	generic (
				BOOT_ADDRESS_G  : std_logic_vector(data_width_c-1 downto 0) := (others => '0') -- boot address
			);
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				HOLD_I          : in  std_logic; -- stops core when high

				-- Instruction Interface --
				INSTR_ADR_O     : out std_logic_vector(data_width_c-1 downto 0); -- instruction byte adr
				INSTR_DAT_I     : in  std_logic_vector(data_width_c-1 downto 0); -- instruction input
				INSTR_EN_O      : out std_logic; -- allow IR update

				-- Memory Arbitration --
				SYS_MODE_O      : out std_logic; -- current operating mode
				SYS_INT_O       : out std_logic; -- interrupt processing

				-- Memory System --
				MEM_REQ_O       : out std_logic; -- mem access in next cycle
				MEM_RW_O        : out std_logic; -- read write
				MEM_ADR_O       : out std_logic_vector(data_width_c-1 downto 0); -- data byte adr
				MEM_DAT_O       : out std_logic_vector(data_width_c-1 downto 0); -- write data
				MEM_DAT_I       : in  std_logic_vector(data_width_c-1 downto 0); -- read data

				-- Coprocessor Interface --
				USR_CP_EN_O     : out std_logic; -- access to cp0
				SYS_CP_EN_O     : out std_logic; -- access to cp1
				CP_OP_O         : out std_logic; -- data transfer/processing
				CP_RW_O         : out std_logic; -- read/write access
				CP_CMD_O        : out std_logic_vector(cp_cmd_width_c-1 downto 0); -- register addresses / cmd
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- read data cp0 OR cp1

				-- External Interrupt Lines --
				EXT_INT_0_I     : in  std_logic; -- external interrupt request 0
				EXT_INT_1_I     : in  std_logic  -- external interrupt request 1
			);
  end component;


  -- Component: Bus Interface ---------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component BUS_INTERFACE
	generic (
				-- Configuration --
				UC_AREA_BEGIN_G  : std_logic_vector(bus_adr_width_c-1 downto 0); -- begin of uncached area
				UC_AREA_END_G    : std_logic_vector(bus_adr_width_c-1 downto 0)  -- end of uncached area
			);
	port (
				-- Global Control --
				CLK_I            : in  std_logic; -- core clock, all triggering on rising edge
				RST_I            : in  std_logic; -- global reset, high active, sync

				-- Instruction Interface --
				INSTR_ADR_I      : in  std_logic_vector(bus_adr_width_c-1 downto 0); -- instruction byte address
				INSTR_DAT_O      : out std_logic_vector(data_width_c-1 downto 0); -- current opcode
				INSTR_EN_I       : in  std_logic; -- allow pseudo-IR update

				-- Data Interface --
				MEM_REQ_I        : in  std_logic; -- access in next cycle
				MEM_RW_I         : in  std_logic; -- read/write access
				MEM_ADR_I        : in  std_logic_vector(bus_adr_width_c-1 downto 0); -- data byte address
				MEM_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				MEM_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- read data

				-- Arbitration --
				SYS_MODE_I       : in  std_logic; -- current processor mode
				HALT_O           : out std_logic; -- stop processor
				ERROR_O          : out std_logic; -- bus access error
				CACHE_SYNC_O     : out std_logic; -- cache is sync
				CLR_CACHE_I      : in  std_logic; -- reload cache
				FLUSH_CACHE_I    : in  std_logic; -- synchronize cache with mem
				DIR_ACC_I        : in  std_logic; -- force direct access

				-- Wishbone Bus --
				WB_ADR_O         : out std_logic_vector(bus_adr_width_c-1 downto 0); -- address
				WB_CTI_O         : out std_logic_vector(02 downto 0); -- cycle type
				WB_SEL_O         : out std_logic_vector(01 downto 0); -- byte select
				WB_TGC_O         : out std_logic;                     -- cycle tag
				WB_DATA_O        : out std_logic_vector(data_width_c-1 downto 0); -- data out
				WB_DATA_I        : in  std_logic_vector(data_width_c-1 downto 0); -- data in
				WB_WE_O          : out std_logic;                     -- read/write
				WB_CYC_O         : out std_logic;                     -- cycle
				WB_STB_O         : out std_logic;                     -- strobe
				WB_ACK_I         : in  std_logic;                     -- acknowledge
				WB_HALT_I        : in  std_logic                      -- halt bus transaction
			);
  end component;


  -- Component: Memory Management Unit ------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component MMU
	generic (
				BOOT_PAGE_G     : std_logic_vector(data_width_c-1 downto 0) := (others => '0') -- boot address
			);
	port	(
				-- Global Control --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				HALT_I          : in  std_logic; -- inverted clock enable

				-- Processor Interface --
				CP_EN_I         : in  std_logic; -- access coprocessor
				CP_OP_I         : in  std_logic; -- data transfer/processing
				CP_RW_I         : in  std_logic; -- read/write access
				CP_CMD_I        : in  std_logic_vector(cp_cmd_width_c-1 downto 0); -- register addresses / cmd
				CP_DAT_I        : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				CP_DAT_O        : out std_logic_vector(data_width_c-1 downto 0); -- read data
				SYS_MODE_I      : in  std_logic; -- current operating mode
				INT_EXE_I       : in  std_logic; -- interrupt beeing executed
				MMU_IRQ_O       : out std_logic; -- mmu interrupt request

				-- Bus Unit Interface --
				CACHE_ERROR_I   : in  std_logic; -- bus access error
				CACHE_SYNC_I    : in  std_logic; -- cache is sync
				CACHE_CLR_O     : out std_logic; -- reload cache
				CACHE_FLUSH_O   : out std_logic; -- synchronize cache with mem
				MEM_DIR_ACC_O   : out std_logic; -- direct access (bypass cache)
				MEM_IP_ADR_O    : out std_logic_vector(data_width_c-1 downto 0); -- instruction page
				MEM_DP_ADR_O    : out std_logic_vector(data_width_c-1 downto 0)  -- data page
			);
  end component;



end atlas_core_package;

package body atlas_core_package is

  -- Function: Logarithm Base 2 -------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
	function log2(temp : natural) return natural is
		variable result : natural;
	begin
		for i in 0 to integer'high loop
			if (2**i >= temp) then
				return i;
			end if;
		end loop;
		return 0;
	end function log2;



end atlas_core_package;
