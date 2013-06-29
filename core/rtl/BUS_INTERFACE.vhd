-- ########################################################
-- #         << ATLAS Project - Bus Interface >>          #
-- # **************************************************** #
-- #  This unit features a Wishbone-compatible bus        #
-- #  together with a fully-associative shared data /     #
-- #  instruction cache. The system is capable of         #
-- #  generating a true 32-bit wide address for the NoC.  #
-- # **************************************************** #
-- #  Last modified: 25.06.2013                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity BUS_INTERFACE is
-- ################################################################################################################
-- ##       Cached Area Configuration                                                                            ##
-- ################################################################################################################
	generic (
				UC_AREA_BEGIN_G  : std_logic_vector(bus_adr_width_c-1 downto 0); -- begin of uncached area
				UC_AREA_END_G    : std_logic_vector(bus_adr_width_c-1 downto 0)  -- end of uncached area
			);
-- ################################################################################################################
-- ##       Global Control                                                                                       ##
-- ################################################################################################################
	port (
				CLK_I            : in  std_logic; -- core clock, all triggering on rising edge
				RST_I            : in  std_logic; -- global reset, high active, sync

-- ################################################################################################################
-- ##       Processor Access                                                                                     ##
-- ################################################################################################################

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

-- ################################################################################################################
-- ##       Wishbone Bus                                                                                         ##
-- ################################################################################################################

				WB_ADR_O         : out std_logic_vector(bus_adr_width_c-1 downto 0); -- address
				WB_CTI_O         : out std_logic_vector(02 downto 0); -- cycle type
				WB_SEL_O         : out std_logic_vector(01 downto 0); -- byte select
				WB_TGC_O         : out std_logic; -- cycle tag
				WB_DATA_O        : out std_logic_vector(data_width_c-1 downto 0); -- data out
				WB_DATA_I        : in  std_logic_vector(data_width_c-1 downto 0); -- data in
				WB_WE_O          : out std_logic;                     -- read/write
				WB_CYC_O         : out std_logic;                     -- cycle
				WB_STB_O         : out std_logic;                     -- strobe
				WB_ACK_I         : in  std_logic;                     -- acknowledge
				WB_HALT_I        : in  std_logic                      -- halt bus transaction
			);
end BUS_INTERFACE;

architecture BUS_INTERFACE_STRUCTURE of BUS_INTERFACE is

	-- Arbiter --
	type   arb_state_type is (IDLE, ANALYSE, TRANSFER_PAGE, RE_SYNC_1, RE_SYNC_2, DIRECT_ACCESS, FLUSH);
	type   dir_flag_type  is (UP, DOWN); 
	signal ARB_STATE,   ARB_STATE_NXT   : arb_state_type;                                      -- main arbiter
	signal RET_STATE,   RET_STATE_NXT   : arb_state_type;                                      -- main arbiter return state
	signal BUS_DIR,     BUS_DIR_NXT     : dir_flag_type;                                       -- current transfer operation
	signal PAGE_PNT,    PAGE_PNT_NXT    : std_logic_vector(log2_cache_pages_c-1 downto 0);     -- page pointer
	signal TYPE_FLAG,   TYPE_FLAG_NXT   : std_logic;                                           -- data/instruction transfer
	signal FREEZE_FLAG, FREEZE_FLAG_NXT : std_logic;                                           -- disable cpu
	signal D_ACC_BUF,   D_ACC_BUF_NXT   : std_logic_vector(bus_adr_width_c-1 downto 0);        -- data access address buffer
	signal I_ACC_BUF,   I_ACC_BUF_NXT   : std_logic_vector(bus_adr_width_c-1 downto 0);        -- instruction access address buffer
	signal DIR_DAT_REQ                  : std_logic;                                           -- direct access request
	signal RND_GEN                      : std_logic_vector(5 downto 0);                        -- random generator
	signal TIMEOUT_CNT, TIMEOUT_CNT_NXT : std_logic_vector(log2_cache_page_size_c-1 downto 0); -- timeout counter
	signal SYNC_CNT,    SYNC_CNT_NXT    : std_logic_vector(1 downto 0);                        -- bus synchroization counter
	signal DATA_CNT,    DATA_CNT_NXT    : std_logic_vector(log2_cache_page_size_c downto 0);   -- data packet counter
	signal DA_RB_FF,    DA_RB_FF_NXT    : std_logic;                                           -- direct access readback

	-- Cache System --
	type   cache_mem_type is array(0 to (cache_pages_c*cache_page_size_c-1)) of std_logic_vector(data_width_c-1 downto 0);
	signal CACHE_MEM                    : cache_mem_type := (others => (others => '0'));
	signal CACHE_EN                     : std_logic;                                           -- valid access to cache (up/download, d-access)
	signal CACHE_RW                     : std_logic;                                           -- read/write (up/download, d-access)
	signal MEM_REQ_FF                   : std_logic;                                           -- processor requires d-mem access
	signal MEM_REQ_FF_FF                : std_logic;                                           -- processor requires d-mem access, signal buffer
	signal MEM_RW_FF                    : std_logic;                                           -- processor requires write access
	signal INST_EN_FF                   : std_logic;                                           -- instruction reg enable ff
	signal I_UPDATE                     : std_logic;                                           -- instruction reg enable
	signal D_ACC_DAT_BUF                : std_logic_vector(data_width_c-1 downto 0);           -- data write buffer
	signal CACHE_D_ADR                  : std_logic_vector(log2_cache_pages_c+log2_cache_page_size_c-1 downto 0); -- cache instr. word address
	signal CACHE_I_ADR                  : std_logic_vector(log2_cache_pages_c+log2_cache_page_size_c-1 downto 0); -- cache data word address
	signal CA_ADR_BUF,  CA_ADR_BUF_NXT  : std_logic_vector(log2_cache_pages_c+log2_cache_page_size_c-1 downto 0); -- word address cache adr buffer
	signal CACHE_DR_DATA                : std_logic_vector(data_width_c-1 downto 0);           -- cache read data (processor/bus unit)
	signal CACHE_DW_DATA                : std_logic_vector(data_width_c-1 downto 0);           -- cache write data (processor/bus unit)
	signal CACHE_I_MISS, CACHE_D_MISS   : std_logic; -- miss during instruction/data access    -- I/D miss access
	signal VALID_FLAG,   VALID_FLAG_NXT : std_logic_vector(cache_pages_c-1 downto 0);          -- page valid flag
	signal DIRTY_FLAG,   DIRTY_FLAG_NXT : std_logic_vector(cache_pages_c-1 downto 0);          -- page dirty flag
	signal CACHE_SYNC                   : std_logic;                                           -- cache is sync

	-- Page Selector --
	type   page_base_type is array(0 to cache_pages_c-1) of std_logic_vector(bus_adr_width_c-1 downto log2_cache_page_size_c+align_lsb_c);
	signal PAGE_BASE_ADR                : page_base_type := (others => (others => '0'));       -- page base address
	signal PAGE_BASE_ADR_NXT            : page_base_type := (others => (others => '0'));       -- page base address
	signal I_PAGE_SELECT, D_PAGE_SELECT : std_logic_vector(log2_cache_pages_c-1 downto 0);     -- page translator output
	signal I_PAGE_BUF, I_PAGE_BUF_NXT   : std_logic_vector(log2_cache_pages_c-1 downto 0);     -- last accessed i-page
	signal D_PAGE_BUF, D_PAGE_BUF_NXT   : std_logic_vector(log2_cache_pages_c-1 downto 0);     -- last accessed d-page
	signal NEW_ENTRY_PAGE               : std_logic_vector(log2_cache_pages_c-1 downto 0);     -- new cache enty address

	-- Wishbone Bus --
	signal WB_ADR_BUF, WB_ADR_BUF_NXT   : std_logic_vector(bus_adr_width_c-1 downto 0);        -- bus address
	signal WB_CYC_BUF, WB_CYC_BUF_NXT   : std_logic;                                           -- valid cycle
	signal WB_STB_BUF, WB_STB_BUF_NXT   : std_logic;                                           -- strobe
	signal WB_ACK_BUF                   : std_logic;                                           -- ack signal buffer
	signal WB_DO_BUF,  WB_DO_BUF_NXT    : std_logic_vector(data_width_c-1 downto 0);           -- data out
	signal WB_DI_BUF                    : std_logic_vector(data_width_c-1 downto 0);           -- data in
	signal WB_ACK_CNT, WB_ACK_CNT_NXT   : std_logic_vector(log2_cache_page_size_c downto 0);   -- ack counter

begin

	-- Control Arbiter (Sync) ------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		ARBITER_SYNC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					-- Arbiter --
					ARB_STATE       <= IDLE;
					RET_STATE       <= IDLE;
					BUS_DIR         <= DOWN;
					PAGE_PNT        <= (others => '0');
					TYPE_FLAG       <= '0';
					FREEZE_FLAG     <= '0';
					WB_ACK_CNT      <= (others => '0');
					D_ACC_BUF       <= (others => '0');
					I_ACC_BUF       <= (others => '0');
					TIMEOUT_CNT     <= (others => '0');
					SYNC_CNT        <= (others => '0');
					DATA_CNT        <= (others => '0');
					DA_RB_FF        <= '0';

					-- Processor --
					MEM_REQ_FF      <= '0';
					MEM_REQ_FF_FF   <= '0';
					MEM_RW_FF       <= '0';
					D_ACC_DAT_BUF   <= (others => '0');
					INST_EN_FF      <= '0';

					-- Cache --
					CA_ADR_BUF      <= (others => '0');
					VALID_FLAG      <= (others => '0');
					DIRTY_FLAG      <= (others => '0');
					PAGE_BASE_ADR   <= (others => (others => '0'));
					D_PAGE_BUF      <= (others => '0');
					I_PAGE_BUF      <= (others => '0');

					-- Wishbone Bus --
					WB_ADR_BUF      <= (others => '0');
					WB_DI_BUF       <= (others => '0');
					WB_DO_BUF       <= (others => '0');
					WB_CYC_BUF      <= '0';
					WB_STB_BUF      <= '0';
					WB_ACK_BUF      <= '0';

				else
					if (WB_HALT_I = '0') then -- this is where we freeze it all
						-- Arbiter --
						ARB_STATE       <= ARB_STATE_NXT;
						RET_STATE       <= RET_STATE_NXT;
						BUS_DIR         <= BUS_DIR_NXT;
						PAGE_PNT        <= PAGE_PNT_NXT;
						TYPE_FLAG       <= TYPE_FLAG_NXT;
						FREEZE_FLAG     <= FREEZE_FLAG_NXT;
						WB_ACK_CNT      <= WB_ACK_CNT_NXT;
						TIMEOUT_CNT     <= TIMEOUT_CNT_NXT;
						SYNC_CNT        <= SYNC_CNT_NXT;
						DATA_CNT        <= DATA_CNT_NXT;
						DA_RB_FF        <= DA_RB_FF_NXT;

						-- Processor --
						if (FREEZE_FLAG = '0') then
							I_ACC_BUF     <= INSTR_ADR_I;
							D_ACC_BUF     <= MEM_ADR_I;
							D_ACC_DAT_BUF <= MEM_DAT_I;
							MEM_REQ_FF    <= MEM_REQ_I;
							MEM_RW_FF     <= MEM_RW_I;
							MEM_REQ_FF_FF <= MEM_REQ_FF;
							INST_EN_FF    <= INSTR_EN_I;
						end if;

						-- Cache --
						CA_ADR_BUF      <= CA_ADR_BUF_NXT;
						VALID_FLAG      <= VALID_FLAG_NXT;
						DIRTY_FLAG      <= DIRTY_FLAG_NXT;
						PAGE_BASE_ADR   <= PAGE_BASE_ADR_NXT;
						D_PAGE_BUF      <= D_PAGE_BUF_NXT;
						I_PAGE_BUF      <= I_PAGE_BUF_NXT;
						
						-- Wishbone Bus --
						WB_ADR_BUF      <= WB_ADR_BUF_NXT;
						WB_DI_BUF       <= WB_DATA_I;
						WB_DO_BUF       <= WB_DO_BUF_NXT;
						WB_CYC_BUF      <= WB_CYC_BUF_NXT;
						WB_STB_BUF      <= WB_STB_BUF_NXT;
						WB_ACK_BUF      <= WB_ACK_I;
					end if;
				end if;
			end if;
		end process ARBITER_SYNC;

		-- Processor Output --
		HALT_O <= FREEZE_FLAG;

		-- Sync Bus Output --
		WB_ADR_O  <= WB_ADR_BUF;
		WB_DATA_O <= WB_DO_BUF;
		WB_CYC_O  <= WB_CYC_BUF;
		WB_STB_O  <= WB_STB_BUF;



	-- Control Arbiter (Async) -----------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		ARBITER_ASYNC: process(ARB_STATE, RET_STATE, DATA_CNT, PAGE_PNT, WB_ACK_CNT, D_PAGE_SELECT, DA_RB_FF, I_PAGE_SELECT, TYPE_FLAG, NEW_ENTRY_PAGE, BUS_DIR, TIMEOUT_CNT, SYNC_CNT, MEM_RW_FF, -- arbiter signals
		                       SYS_MODE_I, MEM_REQ_FF, MEM_REQ_FF_FF, MEM_ADR_I, MEM_DAT_I, MEM_RW_I, INST_EN_FF, INSTR_ADR_I, D_ACC_DAT_BUF, FREEZE_FLAG, I_ACC_BUF, D_ACC_BUF, DIR_DAT_REQ, INSTR_EN_I, CLR_CACHE_I, FLUSH_CACHE_I, -- processor signals
		                       CACHE_I_MISS, CACHE_D_MISS, CACHE_DR_DATA, CA_ADR_BUF, VALID_FLAG, DIRTY_FLAG, PAGE_BASE_ADR, D_PAGE_BUF, I_PAGE_BUF, CACHE_SYNC, DIRTY_FLAG_NXT, VALID_FLAG_NXT, -- cache signals
		                       WB_ADR_BUF, WB_CYC_BUF, WB_STB_BUF, WB_ACK_I, WB_ACK_BUF, WB_DI_BUF, WB_DO_BUF) -- bus signals
			variable modified_page_v : std_logic; -- assigned page is valid and dirty
		begin
			-- Arbiter Defaults --
			ARB_STATE_NXT       <= ARB_STATE;       -- arbiter state
			RET_STATE_NXT       <= RET_STATE;       -- return state
			PAGE_PNT_NXT        <= PAGE_PNT;        -- page pointer
			BUS_DIR_NXT         <= BUS_DIR;         -- transfer direction
			DATA_CNT_NXT        <= DATA_CNT;        -- transferred data counter
			WB_ACK_CNT_NXT      <= (others => '0'); -- bus acknowledge counte
			TYPE_FLAG_NXT       <= TYPE_FLAG;       -- transfer type
			FREEZE_FLAG_NXT     <= FREEZE_FLAG;     -- disable cpu flag
			DA_RB_FF_NXT        <= '0';             -- direct access readback flag
			SYNC_CNT_NXT        <= "00";            -- sync counter

			-- Cache Defaults --
			PAGE_BASE_ADR_NXT   <= PAGE_BASE_ADR;   -- cache pages base address
			CA_ADR_BUF_NXT      <= CA_ADR_BUF;      -- cache address
			D_PAGE_BUF_NXT      <= D_PAGE_BUF;      -- last accessed D page
			I_PAGE_BUF_NXT      <= I_PAGE_BUF;      -- last accessed I page
			modified_page_v     := DIRTY_FLAG_NXT(to_integer(unsigned(PAGE_PNT))) and VALID_FLAG_NXT(to_integer(unsigned(PAGE_PNT))); -- current page is dirty and valid

			-- Status Flags --
			VALID_FLAG_NXT      <= VALID_FLAG;      -- valid cache page
			DIRTY_FLAG_NXT      <= DIRTY_FLAG;      -- modified cache page

			-- Bus monitoring --
			TIMEOUT_CNT_NXT     <= (others => '0'); -- bus timeout
			ERROR_O             <= '0';

			-- Wishbone Bus Defaults --
			WB_ADR_BUF_NXT      <= WB_ADR_BUF;      -- address
			WB_DO_BUF_NXT       <= WB_DO_BUF;       -- data out
			WB_STB_BUF_NXT      <= '0';             -- strobe data
			WB_CYC_BUF_NXT      <= WB_CYC_BUF;      -- valid cycle
			WB_SEL_O            <= (others => '1'); -- full word transfer quantity

			-- Wishbone Bus Static Defaults --
			WB_CTI_O            <= wb_classic_cyc_c;
			WB_TGC_O            <= SYS_MODE_I;      -- cycle tag
			if (BUS_DIR = UP) and (ARB_STATE /= IDLE) then -- download/upload
				WB_WE_O <= '1'; -- bus write
			else
				WB_WE_O <= '0'; -- bus read
			end if;

			-- Static D-Cache Access --
			CACHE_D_ADR         <= D_PAGE_SELECT & MEM_ADR_I(log2_cache_page_size_c downto align_lsb_c); -- word address
			CACHE_EN            <= '0';
			CACHE_DW_DATA       <= MEM_DAT_I;
			CACHE_RW            <= MEM_RW_I;
			MEM_DAT_O           <= CACHE_DR_DATA;

			-- Static I-Cache Access --
			CACHE_I_ADR         <= I_PAGE_SELECT & INSTR_ADR_I(log2_cache_page_size_c downto align_lsb_c); -- word address
			I_UPDATE            <= '0';

			-- State Machine --
			case (ARB_STATE) is

				when IDLE => -- waiting for requests
				-------------------------------------------------------------------------------
					-- Static D-Cache Access --
					CACHE_EN        <= MEM_REQ_FF and (not DIR_DAT_REQ) and (not CACHE_D_MISS); -- no cache access for direct requests
					D_PAGE_BUF_NXT  <= D_PAGE_SELECT; -- last accessed page
					if (DA_RB_FF = '1') then -- load data from direct access
						MEM_DAT_O <= WB_DI_BUF;
					end if;

					-- Static I-Cache Access --
					I_UPDATE        <= INSTR_EN_I;
					I_PAGE_BUF_NXT  <= I_PAGE_SELECT; -- last accessed page

					-- Modified Cache Page --
					if ((MEM_RW_I and MEM_REQ_FF) = '1') and (CACHE_D_MISS = '0') and (DIR_DAT_REQ = '0') then
						DIRTY_FLAG_NXT(to_integer(unsigned(D_PAGE_SELECT))) <= '1'; -- page is dirty now
					end if;

					-- Resync cache with memory --
					if (CLR_CACHE_I = '1') then
						VALID_FLAG_NXT <= (others => '0'); -- invalidate all pages
						DIRTY_FLAG_NXT <= (others => '0'); -- invalidate all pages
					end if;

					-- Page pointer --
					PAGE_PNT_NXT <= NEW_ENTRY_PAGE;

					-- Transfer direction --
					if (MEM_RW_I = '1') then
						BUS_DIR_NXT <= UP;
					else
						BUS_DIR_NXT <= DOWN;
					end if;

					-- Conflict Detector --
					if ((MEM_REQ_FF and DIR_DAT_REQ) = '1') then -- direct access
						ARB_STATE_NXT   <= DIRECT_ACCESS;
						FREEZE_FLAG_NXT <= '1';
						WB_CYC_BUF_NXT  <= '1';
						WB_STB_BUF_NXT  <= '1';
						WB_ADR_BUF_NXT  <= MEM_ADR_I; -- direct address output
						WB_DO_BUF_NXT   <= MEM_DAT_I; -- direct data output
					elsif (CACHE_I_MISS = '1') then -- instruction miss access
						ARB_STATE_NXT   <= ANALYSE;
						FREEZE_FLAG_NXT <= '1';
						TYPE_FLAG_NXT   <= '1';
					elsif (CACHE_D_MISS = '1') then -- data miss access
						ARB_STATE_NXT   <= ANALYSE;
						FREEZE_FLAG_NXT <= '1';
						TYPE_FLAG_NXT   <= '0';
					elsif (FLUSH_CACHE_I = '1') then -- resync memory with cache
						ARB_STATE_NXT   <= FLUSH;
						PAGE_PNT_NXT    <= (others => '0');
						FREEZE_FLAG_NXT <= '1';
						TYPE_FLAG_NXT   <= '0';
					end if;


				when ANALYSE => -- is the to be replaced page dirty?
				-------------------------------------------------------------------------------
					-- Clear data counter --
					DATA_CNT_NXT <= (others => '0');

					-- Update cache base address (word!) --
					CA_ADR_BUF_NXT <= (others => '0');
					CA_ADR_BUF_NXT(log2_cache_pages_c+log2_cache_page_size_c-1 downto log2_cache_page_size_c) <= PAGE_PNT;

					-- Prepare transfer operation --
					if (PAGE_PNT /= I_PAGE_BUF) and (PAGE_PNT /= D_PAGE_BUF) then
						ARB_STATE_NXT  <= TRANSFER_PAGE;
						RET_STATE_NXT  <= ANALYSE;
						WB_CYC_BUF_NXT <= '1';
						WB_STB_BUF_NXT <= '1';
					else
						PAGE_PNT_NXT <= NEW_ENTRY_PAGE;
					end if;

					-- Upload modified page? --
					if (modified_page_v = '1') then
						BUS_DIR_NXT    <= UP; -- upload modified page
						WB_STB_BUF_NXT <= '0';
						WB_ADR_BUF_NXT <= (others => '0');
						WB_ADR_BUF_NXT(bus_adr_width_c-1 downto log2_cache_page_size_c+1) <= PAGE_BASE_ADR(to_integer(unsigned(PAGE_PNT)));
					else -- download new page
						BUS_DIR_NXT    <= DOWN; -- download new page
						if (TYPE_FLAG = '1') then -- new instruction page
							WB_ADR_BUF_NXT <= I_ACC_BUF;
							WB_ADR_BUF_NXT(log2_cache_page_size_c downto 0) <= (others => '0');
						else -- new data page
							WB_ADR_BUF_NXT <= D_ACC_BUF;
							WB_ADR_BUF_NXT(log2_cache_page_size_c downto 0) <= (others => '0');
						end if;
					end if;


				when FLUSH => -- synchronize cache -> mem (dirty pages writeback)
				-------------------------------------------------------------------------------
					-- Clear data counter --
					DATA_CNT_NXT <= (others => '0');

					-- Cache base address (word!) --
					CA_ADR_BUF_NXT <= (others => '0');
					CA_ADR_BUF_NXT(log2_cache_pages_c+log2_cache_page_size_c-1 downto log2_cache_page_size_c) <= PAGE_PNT;

					-- Prepare transfer operation --
					BUS_DIR_NXT    <= UP; -- upload modified page
					RET_STATE_NXT  <= FLUSH;
					WB_CYC_BUF_NXT <= '1';
					WB_ADR_BUF_NXT <= (others => '0');
					WB_ADR_BUF_NXT(bus_adr_width_c-1 downto log2_cache_page_size_c+1) <= PAGE_BASE_ADR(to_integer(unsigned(PAGE_PNT))); -- base address

					-- Page dirty? --
					if (CACHE_SYNC = '1') then -- cache is sync -> nothing to do
						ARB_STATE_NXT  <= RE_SYNC_1;
					elsif (modified_page_v = '1') then -- need upload?
						ARB_STATE_NXT  <= TRANSFER_PAGE;
						WB_STB_BUF_NXT <= '0';
					else
						PAGE_PNT_NXT <= std_logic_vector(unsigned(PAGE_PNT) + 1);
					end if;


				when TRANSFER_PAGE => -- upload/download page
				-------------------------------------------------------------------------------
					-- Static D-Cache Access --
					CACHE_D_ADR   <= CA_ADR_BUF;
					CACHE_DW_DATA <= WB_DI_BUF;
					WB_DO_BUF_NXT <= CACHE_DR_DATA;
					if (BUS_DIR = DOWN) then
						CACHE_RW <= '1';
					else
						CACHE_RW <= '0';
					end if;

					-- Sync control --
					SYNC_CNT_NXT <= SYNC_CNT(0) & '1';

					-- Bus control --
					WB_CTI_O <= wb_inc_bst_cyc_c;

					-- Update Base Address --
					PAGE_BASE_ADR_NXT(to_integer(unsigned(PAGE_PNT))) <= WB_ADR_BUF(bus_adr_width_c-1 downto log2_cache_page_size_c+1); -- new/old base address

					-- ACK Counter --
					if (WB_ACK_I = '1') then
						WB_ACK_CNT_NXT <= std_logic_vector(unsigned(WB_ACK_CNT) + 1);
					end if;

					-- Transfer --
					if (BUS_DIR = DOWN) then -- download page
					-- ------------------------------------------
						CACHE_RW <= '1'; -- write to cache
						-- Update Bus Address --
						if (to_integer(unsigned(DATA_CNT)) < cache_page_size_c-1) then
							WB_ADR_BUF_NXT <= std_logic_vector(unsigned(WB_ADR_BUF) + data_bytes_c); -- inc bus address pointer
							DATA_CNT_NXT   <= std_logic_vector(unsigned(DATA_CNT) + 1); -- inc data counter
							WB_STB_BUF_NXT <= '1'; -- strobe data
						else
							WB_CTI_O       <= wb_end_bst_cyc_c;
							WB_STB_BUF_NXT <= '0'; -- no more new strobes
						end if;
						-- Accept Data --
						if (WB_ACK_BUF = '1') then
							CACHE_EN       <= '1'; -- enable data write to cache
							CA_ADR_BUF_NXT <= std_logic_vector(unsigned(CA_ADR_BUF) + 1); -- inc cache address pointer
						end if;
					else -- upload page
					-- ------------------------------------------
						CACHE_RW <= '0'; -- read from cache
						-- Update Bus & Cache Address --
						if (to_integer(unsigned(DATA_CNT)) /= cache_page_size_c-1) then
							if (SYNC_CNT(1) = '1') then -- cycle delay
								WB_ADR_BUF_NXT <= std_logic_vector(unsigned(WB_ADR_BUF) + data_bytes_c); -- inc bus address pointer
								DATA_CNT_NXT   <= std_logic_vector(unsigned(DATA_CNT) + 1); -- inc data counter
							end if;
							WB_STB_BUF_NXT <= SYNC_CNT(0);
							CACHE_EN       <= '1'; -- enable data read from cache
							CA_ADR_BUF_NXT <= std_logic_vector(unsigned(CA_ADR_BUF) + 1); -- inc cache address pointer
						else
							WB_CTI_O       <= wb_end_bst_cyc_c;
							WB_STB_BUF_NXT <= '0'; -- no more new strobes
						end if;
					end if;

					-- Wait for all ACKs --
					if (to_integer(unsigned(WB_ACK_CNT)) = cache_page_size_c) then
						WB_CYC_BUF_NXT <= '0'; -- terminate cycle
						if (BUS_DIR = DOWN) then
							ARB_STATE_NXT <= RE_SYNC_1; -- yeay, transfer completed!
							VALID_FLAG_NXT(to_integer(unsigned(PAGE_PNT))) <= '1'; -- page is valid now
							if (TYPE_FLAG = '1') then
								I_PAGE_BUF_NXT <= PAGE_PNT;
							else
								D_PAGE_BUF_NXT <= PAGE_PNT;
							end if;
						else
							ARB_STATE_NXT <= RET_STATE; -- upload complete, redo download
							DIRTY_FLAG_NXT(to_integer(unsigned(PAGE_PNT))) <= '0'; -- page is sync now
						end if;
					end if;

					-- Timeout --
					if (WB_ACK_I = '1') then
						TIMEOUT_CNT_NXT <= (others => '0');
					else
						TIMEOUT_CNT_NXT <= std_logic_vector(unsigned(TIMEOUT_CNT)+1); -- timeout counter
					end if;
					if (to_integer(unsigned(TIMEOUT_CNT)) > max_bus_latency_c) then
						ARB_STATE_NXT <= RE_SYNC_1;
						WB_CYC_BUF_NXT <= '0';
						ERROR_O <= '1'; -- error!
					end if;


				when DIRECT_ACCESS => -- direct memory access
				-------------------------------------------------------------------------------
					-- WB Bus --
					WB_CTI_O <= wb_classic_cyc_c;
					WB_STB_BUF_NXT <= '1';

					-- Data flow --
					if (WB_ACK_I = '1') then
						ARB_STATE_NXT   <= IDLE;
						FREEZE_FLAG_NXT <= '0';
						WB_CYC_BUF_NXT  <= '0';
						WB_STB_BUF_NXT  <= '0';
					end if;
					if (BUS_DIR = DOWN) then
						DA_RB_FF_NXT <= '1'; -- load data in next cycle
					end if;

					-- I- Cache Resync --
					I_UPDATE    <= INST_EN_FF;
					CACHE_I_ADR <= I_PAGE_BUF & I_ACC_BUF(log2_cache_page_size_c downto align_lsb_c);

					-- Timeout --
					if (WB_ACK_I = '1') then
						TIMEOUT_CNT_NXT <= (others => '0');
					else
						TIMEOUT_CNT_NXT <= std_logic_vector(unsigned(TIMEOUT_CNT)+1); -- timeout counter
					end if;
					if (to_integer(unsigned(TIMEOUT_CNT)) > max_bus_latency_c) then
						ARB_STATE_NXT <= IDLE;
						WB_CYC_BUF_NXT <= '0';
						WB_STB_BUF_NXT <= '0';
						ERROR_O <= '1'; -- error!
					end if;


				when RE_SYNC_1 => -- re-synchronize instruction/data fetch
				-------------------------------------------------------------------------------
					-- D-Read Access --
					CACHE_RW      <= '0';
					CACHE_EN      <= '1';
					CACHE_D_ADR   <= D_PAGE_BUF & D_ACC_BUF(log2_cache_page_size_c downto align_lsb_c);

					-- Modified Cache Page --
					if ((MEM_REQ_FF_FF and MEM_RW_FF) = '1') and (RET_STATE /= FLUSH) then
						DIRTY_FLAG_NXT(to_integer(unsigned(D_PAGE_BUF))) <= '1'; -- page is dirty now
					end if;

					-- WB Bus --
					WB_CYC_BUF_NXT <= '0';
					WB_STB_BUF_NXT <= '0';

					-- Arbiter --
					ARB_STATE_NXT <= RE_SYNC_2;


				when RE_SYNC_2 => -- re-synchronize instruction/data fetch
				-------------------------------------------------------------------------------
					-- I-Access --
					I_UPDATE        <= INST_EN_FF;
					CACHE_I_ADR     <= I_PAGE_BUF & I_ACC_BUF(log2_cache_page_size_c downto align_lsb_c);

					-- D-Write Access --
					CACHE_RW        <= MEM_RW_FF;
					CACHE_EN        <= MEM_REQ_FF_FF;
					CACHE_D_ADR     <= D_PAGE_BUF & D_ACC_BUF(log2_cache_page_size_c downto align_lsb_c);
					CACHE_DW_DATA   <= D_ACC_DAT_BUF;

					-- Arbiter --
					ARB_STATE_NXT   <= IDLE;
					FREEZE_FLAG_NXT <= '0';

			end case;
		end process ARBITER_ASYNC;



	-- New Entry Address Generator -------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		PAGE_RANDOM_GEN: process(CLK_I)
		begin
			-- Sync Update --
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					RND_GEN <= (others => '0');
				else--elsif (FREEZE_FLAG = '0') and (ARB_STATE = IDLE) then
					if (RND_GEN = "111111") then
						RND_GEN <= RND_GEN(4 downto 0) & '0';
					else
						RND_GEN <= RND_GEN(4 downto 0) & (RND_GEN(5) xnor RND_GEN(4));
					end if;
				end if;
			end if;
		end process PAGE_RANDOM_GEN;

		-- Output --
		NEW_ENTRY_PAGE <= RND_GEN(log2_cache_pages_c-1 downto 0);


	-- HIT / MISS Detector and Page Translator -------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		CONTENT_DETECTOR: process(INSTR_ADR_I, MEM_ADR_I, PAGE_BASE_ADR, VALID_FLAG, INSTR_EN_I, MEM_REQ_FF, DIRTY_FLAG)
			variable i_hit_v, d_hit_v, sync_v : std_logic;
		begin
			-- Instruction access --
			i_hit_v := '0';
			I_PAGE_SELECT <= (others => '0');
			for i in 0 to cache_pages_c-1 loop
				if (PAGE_BASE_ADR(i) = INSTR_ADR_I(bus_adr_width_c-1 downto log2_cache_page_size_c+align_lsb_c)) and (VALID_FLAG(i) = '1') then
					I_PAGE_SELECT <= std_logic_vector(to_unsigned(i, log2_cache_pages_c));
					i_hit_v := '1';
					exit;
				end if;
			end loop;
			CACHE_I_MISS <= (not i_hit_v) and INSTR_EN_I; -- valid insruction fetch

			-- Data access --
			d_hit_v := '0';
			D_PAGE_SELECT <= (others => '0');
			for j in 0 to cache_pages_c-1 loop
				if (PAGE_BASE_ADR(j) = MEM_ADR_I(bus_adr_width_c-1 downto log2_cache_page_size_c+align_lsb_c)) and (VALID_FLAG(j) = '1') then
					D_PAGE_SELECT <= std_logic_vector(to_unsigned(j, log2_cache_pages_c));
					d_hit_v := '1';
					exit;
				end if;
			end loop;
			CACHE_D_MISS <= (not d_hit_v) and MEM_REQ_FF; -- valid access

			-- Sync detector --
			sync_v := '1';
			for k in 0 to cache_pages_c-1 loop
				if (VALID_FLAG(k) = '1') then
					sync_v := sync_v and (not DIRTY_FLAG(k));
				else
					sync_v := sync_v;
				end if;
			end loop;
			CACHE_SYNC   <= sync_v;
			CACHE_SYNC_O <= sync_v;
		end process CONTENT_DETECTOR;

		-- Direct Data Request --
		DIR_DAT_REQ <= '1' when ((unsigned(MEM_ADR_I) >= unsigned(UC_AREA_BEGIN_G)) and (unsigned(MEM_ADR_I) <= unsigned(UC_AREA_END_G))) or (DIR_ACC_I = '1') else '0';



	-- Cache Memory Access ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		CACHE_MEM_ACCESS: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (CACHE_EN = '1') and (WB_HALT_I = '0') then -- valid data access
					if (CACHE_RW = '1') then -- cache data write access
						CACHE_MEM(to_integer(unsigned(CACHE_D_ADR(log2_cache_pages_c+log2_cache_page_size_c-1 downto 0)))) <= CACHE_DW_DATA; -- word address!
					end if;
					CACHE_DR_DATA <= CACHE_MEM(to_integer(unsigned(CACHE_D_ADR(log2_cache_pages_c+log2_cache_page_size_c-1 downto 0)))); -- word address!
				end if;
				if (I_UPDATE = '1') then -- cache instruction read access
					INSTR_DAT_O <= CACHE_MEM(to_integer(unsigned(CACHE_I_ADR(log2_cache_pages_c+log2_cache_page_size_c-1 downto 0)))); -- word address!
				end if;
			end if;
		end process CACHE_MEM_ACCESS;



end BUS_INTERFACE_STRUCTURE;