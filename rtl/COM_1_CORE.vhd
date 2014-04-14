-- #########################################################
-- #   << ATLAS Project - Communication Controller 1 >>    #
-- # ***************************************************** #
-- #  - Wishbone Bus Adapter                               #
-- #    -> 32-bit address, 16-bit data                     #
-- #    -> Variable Length Burst-Transfers                 #
-- #    -> Bus access is pipelined                         #
-- # ***************************************************** #
-- #  Last modified: 10.04.2014                            #
-- # ***************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany            #
-- #########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity COM_1_CORE is
	port	(
-- ###############################################################################################
-- ##           Host Interface                                                                  ##
-- ###############################################################################################

				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
                CMD_EXE_I       : in  std_logic; -- execute command
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address/command
				DAT_I           : in  std_logic_vector(15 downto 0); -- write data
				DAT_O           : out std_logic_vector(15 downto 0); -- read data
                IRQ_O           : out std_logic; -- interrupt request

-- ###############################################################################################
-- ##           Wishbone Bus                                                                    ##
-- ###############################################################################################

                WB_CLK_O        : out std_logic; -- bus clock
                WB_RST_O        : out std_logic; -- bus reset, sync, high active
				WB_ADR_O        : out std_logic_vector(31 downto 0); -- address
				WB_SEL_O        : out std_logic_vector(01 downto 0); -- byte select
				WB_DATA_O       : out std_logic_vector(15 downto 0); -- data out
				WB_DATA_I       : in  std_logic_vector(15 downto 0); -- data in
				WB_WE_O         : out std_logic; -- read/write
				WB_CYC_O        : out std_logic; -- cycle enable
				WB_STB_O        : out std_logic; -- strobe
				WB_ACK_I        : in  std_logic; -- acknowledge
--              WB_HALT_I       : in  std_logic; -- halt transfer
                WB_ERR_I        : in  std_logic  -- bus error
			);
end COM_1_CORE;

architecture COM_1_CORE_BEHAV of COM_1_CORE is

	-- Module Addresses --
	constant ctrl_reg_c        : std_logic_vector(02 downto 0) := "000"; -- R/W: control register (see below)
	constant base_adr_l_reg_c  : std_logic_vector(02 downto 0) := "001"; -- R/W: base address low
	constant base_adr_h_reg_c  : std_logic_vector(02 downto 0) := "010"; -- R/W: base address high
	constant adr_offset_c      : std_logic_vector(02 downto 0) := "011"; -- R/W: address offset (2's comp)
	constant rtx_fifo_c        : std_logic_vector(02 downto 0) := "100"; -- R/W: Read/write FIFO
	constant timeout_val_c     : std_logic_vector(02 downto 0) := "101"; -- R/W: Bus timeout cycles

	-- Module Operations --
	constant cmd_init_rtrans_c : std_logic_vector(02 downto 0) := "000"; -- start READ transfer
	constant cmd_init_wtrans_c : std_logic_vector(02 downto 0) := "001"; -- start WRITE transfer

	-- CTRL Register Bits --
	constant done_irq_c        : natural :=  0; -- R:   Transfer done (interrupt) flag
	constant bus_err_irq_c     : natural :=  1; -- R:   Wishbone bus error (interrupt) flag
	constant timeout_irq_c     : natural :=  2; -- R:   Wishbone bus timeout (interrupt) flag
	constant done_irq_en_c     : natural :=  3; -- R/W: Allow IRQ for <transfer done>
	constant bus_err_en_irq_c  : natural :=  4; -- R/W: Allow IRQ for <bus error>
	constant timeout_en_irq_c  : natural :=  5; -- R/W: Allow IRQ for <bus timeout>
	constant busy_flag_c       : natural :=  6; -- R:   Transfer in progress (busy)
	constant dir_flag_c        : natural :=  7; -- R:   Direction of last transfer (1: write, 0: read)
	constant burst_size_lsb_c  : natural :=  8; -- R/W: Burst size LSB
	constant burst_size_msb_c  : natural := 15; -- R/W: Burst size MSB

    -- Config Regs --
    signal BASE_ADR            : std_logic_vector(31 downto 0); -- base address
    signal ADR_OFFSET          : std_logic_vector(15 downto 0); -- address offset (2's comp)
    signal TIMEOUT_VAL         : std_logic_vector(15 downto 0); -- timeout in cycles

	-- Arbiter --
    signal ARB_BUSY            : std_logic; -- arbiter busy flag
    signal DIR_CTRL            : std_logic; -- direction of current/last transfer (0:read, 1:write)
    signal BURST_SIZE          : std_logic_vector(log2(wb_fifo_size_c)-1 downto 0);
    signal ACK_CNT             : std_logic_vector(log2(wb_fifo_size_c)-1 downto 0);
    signal WB_ADR_OFFSET       : std_logic_vector(31 downto 0);
    signal TIMEOUT_CNT         : std_logic_vector(15 downto 0);

    -- IRQ System --
    signal BUS_ERR_IRQ_EN      : std_logic;
    signal TRANS_DONE_IRQ_EN   : std_logic;
    signal TIMEOUT_IRQ_EN      : std_logic;
    signal BUS_ERR_IRQ         : std_logic;
    signal TRANS_DONE_IRQ      : std_logic;
    signal TIMEOUT_IRQ         : std_logic;

    -- RTX FIFO --
    type   rtx_fifo_t is array (0 to wb_fifo_size_c-1) of std_logic_vector(15 downto 0);
    signal TX_FIFO, RX_FIFO    : rtx_fifo_t := (others => (others => '0'));
    signal RX_FIFO_R_PNT       : std_logic_vector(log2(wb_fifo_size_c)-1 downto 0);
    signal RX_FIFO_W_PNT       : std_logic_vector(log2(wb_fifo_size_c)-1 downto 0);
    signal TX_FIFO_R_PNT       : std_logic_vector(log2(wb_fifo_size_c)-1 downto 0);
    signal TX_FIFO_W_PNT       : std_logic_vector(log2(wb_fifo_size_c)-1 downto 0);

    -- WB Sync --
	signal WB_DATA_I_FF        : std_logic_vector(15 downto 0); -- data in buffer
	signal WB_ACK_FF           : std_logic; -- acknowledge buffer
    signal WB_ERR_FF           : std_logic; -- bus error
    signal WB_ADR              : std_logic_vector(31 downto 0);
    signal WB_ADR_BUF          : std_logic_vector(31 downto 0);
    signal WB_STB_BUF          : std_logic;
    signal WB_CYC_BUF          : std_logic;

begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					BASE_ADR          <= (others => '0');
                    BURST_SIZE        <= (others => '0');
                    ADR_OFFSET        <= (others => '0');
                    TIMEOUT_VAL       <= (others => '0');
                    BUS_ERR_IRQ_EN    <= '0';
                    TRANS_DONE_IRQ_EN <= '0';
                    TIMEOUT_IRQ_EN    <= '0';
				elsif (ICE_I = '1') then -- interface enable
					if (W_EN_I = '1') and (ARB_BUSY = '0') then -- register update only if not busy
						case (ADR_I) is
							when ctrl_reg_c =>
                                BURST_SIZE        <= DAT_I(burst_size_lsb_c+log2(wb_fifo_size_c)-1 downto burst_size_lsb_c);
                                BUS_ERR_IRQ_EN    <= DAT_I(bus_err_en_irq_c);
                                TRANS_DONE_IRQ_EN <= DAT_I(done_irq_en_c);
                                TIMEOUT_IRQ_EN    <= DAT_I(timeout_en_irq_c);
							when base_adr_l_reg_c => BASE_ADR(15 downto 00) <= DAT_I;
							when base_adr_h_reg_c => BASE_ADR(31 downto 16) <= DAT_I;
							when adr_offset_c     => ADR_OFFSET  <= DAT_I;
                            when timeout_val_c    => TIMEOUT_VAL <= DAT_I;
							when others => NULL;
						end case;
					end if;
				end if;
			end if;
		end process W_ACC;



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, BASE_ADR, ADR_OFFSET, ARB_BUSY, DIR_CTRL, BURST_SIZE, BUS_ERR_IRQ_EN,
                       TRANS_DONE_IRQ_EN, BUS_ERR_IRQ, TRANS_DONE_IRQ, RX_FIFO, RX_FIFO_R_PNT,
                       TIMEOUT_IRQ_EN, TIMEOUT_IRQ, TIMEOUT_VAL)
		begin
			case (ADR_I) is
				when ctrl_reg_c =>
                    DAT_O <= (others => '0');
				    DAT_O(busy_flag_c)      <= ARB_BUSY;
				    DAT_O(dir_flag_c)       <= DIR_CTRL;
				    DAT_O(bus_err_irq_c)    <= BUS_ERR_IRQ;
				    DAT_O(bus_err_en_irq_c) <= BUS_ERR_IRQ_EN;
				    DAT_O(done_irq_c)       <= TRANS_DONE_IRQ;
				    DAT_O(done_irq_en_c)    <= TRANS_DONE_IRQ_EN;
				    DAT_O(timeout_irq_c)    <= TIMEOUT_IRQ;
				    DAT_O(timeout_en_irq_c) <= TIMEOUT_IRQ_EN;
				    DAT_O(burst_size_lsb_c+log2(wb_fifo_size_c)-1 downto burst_size_lsb_c) <= BURST_SIZE;
				when base_adr_l_reg_c => DAT_O <= BASE_ADR(15 downto 00);
				when base_adr_h_reg_c => DAT_O <= BASE_ADR(31 downto 16);
				when adr_offset_c     => DAT_O <= ADR_OFFSET;
                when rtx_fifo_c       => DAT_O <= RX_FIFO(to_integer(unsigned(RX_FIFO_R_PNT)));
                when timeout_val_c    => DAT_O <= TIMEOUT_VAL;
				when others           => DAT_O <= x"0000";
			end case;
		end process R_ACC;



	-- Host FIFO Access ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		FIFO_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
                    TX_FIFO_W_PNT <= (others => '0');
                    RX_FIFO_R_PNT <= (others => '0');
				elsif (ICE_I = '1') then -- interface enabled
                    if (ADR_I = rtx_fifo_c) then -- fifo access
                        if ((W_EN_I and (ARB_BUSY nand DIR_CTRL)) = '1') then -- valid write to tx fifo?
                            TX_FIFO(to_integer(unsigned(TX_FIFO_W_PNT))) <= DAT_I;
                            if (TX_FIFO_W_PNT /= BURST_SIZE) then
                                TX_FIFO_W_PNT <= std_logic_vector(unsigned(TX_FIFO_W_PNT) + 1); -- inc tx fifo write pointer
                            else
                                TX_FIFO_W_PNT <= (others => '0');
                            end if;
                        end if;
                        if ((R_EN_I and (ARB_BUSY nand (not DIR_CTRL))) = '1') then -- valid read from RX fifo?
                            if (RX_FIFO_R_PNT /= BURST_SIZE) then
                                RX_FIFO_R_PNT <= std_logic_vector(unsigned(RX_FIFO_R_PNT) + 1); -- inc rx fifo read pointer
                            else
                                RX_FIFO_R_PNT <= (others => '0');
                            end if;
                        end if;
                    end if;
				end if;
			end if;
		end process FIFO_ACC;



	-- Address Offset --------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		ADR_OFFSET_COMP: process(ADR_OFFSET)
		begin
			WB_ADR_OFFSET(15 downto 0) <= ADR_OFFSET;
            for i in 16 to 31 loop -- sign extension
                WB_ADR_OFFSET(i) <= ADR_OFFSET(15);
            end loop;
		end process ADR_OFFSET_COMP;



	-- Interrupt Output ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		IRQ_O <= (BUS_ERR_IRQ and BUS_ERR_IRQ_EN) or
                 (TRANS_DONE_IRQ and TRANS_DONE_IRQ_EN) or
                 (TIMEOUT_IRQ and TIMEOUT_IRQ_EN); -- use edge trigger!



	-- Bus Synchronizer ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		BUS_SYNC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
                    WB_DATA_I_FF <= (others => '0');
                    WB_ACK_FF    <= '0';
                    WB_ERR_FF    <= '0';
				else
                    WB_DATA_I_FF <= WB_DATA_I;
                    WB_ACK_FF    <= WB_ACK_I;
                    WB_ERR_FF    <= WB_ERR_I;
				end if;
			end if;
		end process BUS_SYNC;

        -- Static output --
        WB_SEL_O <= (others => '1');
        WB_ADR_O <= WB_ADR;
        WB_CLK_O <= CLK_I;
        WB_RST_O <= RST_I;



	-- Bus Arbiter ------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		BUS_ARBITER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					DIR_CTRL       <= '0';
					ARB_BUSY       <= '0';
                    ACK_CNT        <= (others => '0');
                    TIMEOUT_CNT    <= (others => '0');
                    TX_FIFO_R_PNT  <= (others => '0');
                    RX_FIFO_W_PNT  <= (others => '0');
                    BUS_ERR_IRQ    <= '0';
                    TRANS_DONE_IRQ <= '0';
                    TIMEOUT_IRQ    <= '0';
					WB_DATA_O      <= (others => '0');
					WB_ADR         <= (others => '0');
					WB_ADR_BUF     <= (others => '0');
                    WB_CYC_O       <= '0';
                    WB_STB_O       <= '0';
                    WB_STB_BUF     <= '0';
                    WB_CYC_BUF     <= '0';
                    WB_WE_O        <= '0';
				else
                    -- IDLE MODE ------------------------------
                    if (ARB_BUSY = '0') then
                        ACK_CNT       <= (others => '0');
                        TIMEOUT_CNT   <= (others => '0');
                        ARB_BUSY      <= '0';
                        RX_FIFO_W_PNT <= (others => '0');
                        TX_FIFO_R_PNT <= (others => '0');
                        WB_ADR        <= (others => '0');
                        WB_ADR_BUF    <= BASE_ADR;
                        WB_STB_O      <= '0';
                        WB_STB_BUF    <= '0';
                        WB_CYC_O      <= '0';
                        WB_CYC_BUF    <= '0';

                        -- Interface --
                        if (ICE_I = '1') then
                            if (R_EN_I = '1') and (ADR_I = ctrl_reg_c) then -- read CTRL reg?
                                BUS_ERR_IRQ    <= '0';
                                TRANS_DONE_IRQ <= '0';
                                TIMEOUT_IRQ    <= '0';
                            end if;
                            if (CMD_EXE_I = '1') then -- execute transfer command?
                                if (ADR_I = cmd_init_rtrans_c) then
                                    DIR_CTRL       <= '0'; -- read transfer
                                    ARB_BUSY       <= '1'; -- start!
                                    BUS_ERR_IRQ    <= '0';
                                    TRANS_DONE_IRQ <= '0';
                                    TIMEOUT_IRQ    <= '0';
                                    WB_STB_BUF     <= '1';
                                    WB_CYC_BUF     <= '1';
                                elsif (ADR_I = cmd_init_wtrans_c) then
                                    DIR_CTRL       <= '1'; -- write transfer
                                    ARB_BUSY       <= '1'; -- start!
                                    BUS_ERR_IRQ    <= '0';
                                    TRANS_DONE_IRQ <= '0';
                                    TIMEOUT_IRQ    <= '0';
                                    WB_STB_BUF     <= '1';
                                    WB_CYC_BUF     <= '1';
                                end if;
                            end if;
                        end if;

                    -- TRANSFER IN PROGRESS -------------------
                    else --elsif (WB_HALT_I = '0') then
                        WB_WE_O     <= DIR_CTRL;
                        WB_ADR      <= WB_ADR_BUF;
                        WB_STB_O    <= WB_STB_BUF;
                        WB_CYC_O    <= WB_CYC_BUF;
                        TIMEOUT_CNT <= std_logic_vector(unsigned(TIMEOUT_CNT) + 1);

                        -- Read Transfer ------------------------
                        if (DIR_CTRL = '0') then
                            if (WB_ACK_FF = '1') then
                                RX_FIFO(to_integer(unsigned(RX_FIFO_W_PNT))) <= WB_DATA_I_FF;
                                RX_FIFO_W_PNT <= std_logic_vector(unsigned(RX_FIFO_W_PNT) + 1); -- inc rx fifo write pointer
                            end if;
                            if (RX_FIFO_W_PNT /= BURST_SIZE) then -- all transfered?
                                WB_ADR_BUF    <= std_logic_vector(unsigned(WB_ADR_BUF) + unsigned(WB_ADR_OFFSET)); -- ADR
                                WB_STB_BUF    <= '1';
                            else
                                WB_STB_BUF    <= '0';
                            end if;

                        -- Write Transfer -----------------------
                        else
                            WB_DATA_O <= TX_FIFO(to_integer(unsigned(TX_FIFO_R_PNT)));
                            if (TX_FIFO_R_PNT /= BURST_SIZE) then -- all transfered?
                                TX_FIFO_R_PNT <= std_logic_vector(unsigned(TX_FIFO_R_PNT) + 1); -- inc tx fifo read pointer
                                WB_ADR_BUF    <= std_logic_vector(unsigned(WB_ADR_BUF) + unsigned(WB_ADR_OFFSET)); -- ADR
                                WB_STB_BUF    <= '1';
                            else
                                WB_STB_BUF    <= '0';
                            end if;
                        end if;

                        -- ACK counter --
                        if (WB_ACK_FF = '1') then
                            if (ACK_CNT = BURST_SIZE) then -- yeay, finished!
                                WB_CYC_BUF     <= '0';
                                WB_CYC_O       <= '0';
                                ARB_BUSY       <= '0'; -- done
                                TRANS_DONE_IRQ <= '1';
                            else
                                ACK_CNT        <= std_logic_vector(unsigned(ACK_CNT) + 1);
                                WB_CYC_BUF     <= '1';
                            end if;
                        end if;

                        -- Bus Error/Timeout? --
                        if (WB_ERR_FF = '1') or (TIMEOUT_CNT = TIMEOUT_VAL) then
                            WB_CYC_O       <= '0';
                            WB_CYC_BUF     <= '0';
                            WB_STB_O       <= '0';
                            WB_STB_BUF     <= '0';
                            ARB_BUSY       <= '0'; -- terminate
                            TRANS_DONE_IRQ <= '0';
                            if (WB_ERR_FF = '1') then
                                BUS_ERR_IRQ <= '1';
                            else
                                TIMEOUT_IRQ <= '1';
                            end if;
                        end if;
                    end if;
				end if;
			end if;
		end process BUS_ARBITER;



end COM_1_CORE_BEHAV;
