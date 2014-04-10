-- ########################################################
-- #       << ATLAS Project - System Coprocessor >>       #
-- # **************************************************** #
-- #  Top entity of the system extension coprocessor.     #
-- # **************************************************** #
-- #  Last modified: 09.04.2014                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity SYSTEM_CP is
-- ###############################################################################################
-- ##           Module Configuration                                                            ##
-- ###############################################################################################
	generic	(
				CLOCK_SPEED_G   : std_logic_vector(31 downto 0) := x"00000000" -- clock speed in Hz
			);
	port	(
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################

				CLK_I           : in std_logic; -- global clock line
				RST_I           : in std_logic; -- global reset line, sync, high-active
				ICE_I           : in std_logic; -- interface clock enable, high-active

-- ###############################################################################################
-- ##           Processor Interface                                                             ##
-- ###############################################################################################

				CP_EN_I         : in  std_logic; -- access coprocessor
				CP_OP_I         : in  std_logic; -- data transfer/processing
				CP_RW_I         : in  std_logic; -- read/write access
				CP_CMD_I        : in  std_logic_vector(cp_cmd_width_c-1 downto 0); -- register addresses / cmd
				CP_DAT_I        : in  std_logic_vector(data_width_c-1   downto 0); -- write data
				CP_DAT_O        : out std_logic_vector(data_width_c-1   downto 0); -- read data
				CP_IRQ_O        : out std_logic; -- unit interrupt request

				SYS_MODE_I      : in  std_logic; -- current operating mode
				INT_EXE_I       : in  std_logic; -- interrupt beeing executed

-- ###############################################################################################
-- ##           Memory Interface                                                                ##
-- ###############################################################################################

				MEM_IP_ADR_O    : out std_logic_vector(15 downto 0); -- instruction page
				MEM_DP_ADR_O    : out std_logic_vector(15 downto 0); -- data page

-- ###############################################################################################
-- ##           Peripheral Communication Interface                                              ##
-- ###############################################################################################

				-- UART --
				UART_RXD_I      : in  std_logic; -- receiver input
				UART_TXD_O      : out std_logic; -- UART transmitter output

				-- SPI --
				SPI_MOSI_O      : out std_logic_vector(07 downto 0); -- serial data out
				SPI_MISO_I      : in  std_logic_vector(07 downto 0); -- serial data in
				SPI_SCK_O       : out std_logic_vector(07 downto 0); -- serial clock out
				SPI_CS_O        : out std_logic_vector(07 downto 0); -- chip select (low active)

				-- Parallel IO --
				PIO_OUT_O       : out std_logic_vector(15 downto 0); -- parallel output
				PIO_IN_I        : in  std_logic_vector(15 downto 0); -- parallel input

				-- System IO --
				SYS_OUT_O       : out std_logic_vector(07 downto 0); -- system output
				SYS_IN_I        : in  std_logic_vector(07 downto 0); -- system input

				-- IRQs --
				IRQ_I           : in  std_logic; -- IRQ

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
                WB_ERR_I        : in  std_logic  -- bus error
			);
end SYSTEM_CP;

architecture SYSTEM_CP_BEHAV of SYSTEM_CP is

	-- Module addresses --
	constant sys0_module_c      : std_logic_vector(1 downto 0) := "00";
	constant sys1_module_c      : std_logic_vector(1 downto 0) := "01";
	constant com0_module_c      : std_logic_vector(1 downto 0) := "10";
	constant com1_module_c      : std_logic_vector(1 downto 0) := "11";

	-- Module interface --
	type module_interface_t is record
		DATA_O                  : std_logic_vector(data_width_c-1 downto 0);
		W_EN                    : std_logic;
		R_EN                    : std_logic;
        CMD_EXE                 : std_logic;
	end record;

	signal SYS_0_MODULE         : module_interface_t;
	signal SYS_1_MODULE         : module_interface_t;
	signal COM_0_MODULE         : module_interface_t;
	signal COM_1_MODULE         : module_interface_t;

	-- Raw interrupt signals --
	signal INT_ASSIGN           : std_logic_vector(7 downto 0);
	signal TIMER_IRQ            : std_logic;
	signal UART_RX_IRQ          : std_logic;
	signal UART_TX_IRQ          : std_logic;
	signal SPI_IRQ              : std_logic;
	signal PIO_IRQ              : std_logic;
    signal WB_CORE_IRQ          : std_logic;

	-- Internals --
	signal READ_ACC             : std_logic; -- true read access
	signal CMD_EXE              : std_logic; -- true coprocessor command

begin

	-- Write Access Logic ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		CTRL_W_ACC: process(CP_EN_I, CP_RW_I, CP_OP_I, CP_CMD_I)
			variable valid_acc_v : std_logic;
		begin
			-- Valid Write Access? --
			valid_acc_v := CP_EN_I and CP_RW_I and CP_OP_I;

			-- Address Decoder --
			SYS_0_MODULE.W_EN <= '0';
			SYS_1_MODULE.W_EN <= '0';
			COM_0_MODULE.W_EN <= '0';
			COM_1_MODULE.W_EN <= '0';
			case (CP_CMD_I(cp_op_a_msb_c-1 downto cp_op_a_lsb_c)) is
				when sys0_module_c => SYS_0_MODULE.W_EN <= valid_acc_v;
				when sys1_module_c => SYS_1_MODULE.W_EN <= valid_acc_v;
				when com0_module_c => COM_0_MODULE.W_EN <= valid_acc_v;
				when com1_module_c => COM_1_MODULE.W_EN <= valid_acc_v;
				when others        => NULL;
			end case;
		end process CTRL_W_ACC;



	-- Read Access Logic -----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		CTRL_R_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					CP_DAT_O <= (others => '0');
				elsif (ICE_I = '1') then -- clock enabled
					if (READ_ACC = '1') then -- valid read
						case (CP_CMD_I(cp_op_b_msb_c-1 downto cp_op_b_lsb_c)) is
							when sys0_module_c => CP_DAT_O <= SYS_0_MODULE.DATA_O;
							when sys1_module_c => CP_DAT_O <= SYS_1_MODULE.DATA_O;
							when com0_module_c => CP_DAT_O <= COM_0_MODULE.DATA_O;
							when com1_module_c => CP_DAT_O <= COM_1_MODULE.DATA_O;
							when others        => CP_DAT_O <= (others => '0');
						end case;
					else
						CP_DAT_O <= (others => '0');
					end if;
				end if;
			end if;
		end process CTRL_R_ACC;

		-- Module Read Enable --
		READ_ACC <= CP_EN_I and (not CP_RW_I) and CP_OP_I; -- true read access
		SYS_0_MODULE.R_EN    <= READ_ACC when (CP_CMD_I(cp_op_b_msb_c-1 downto cp_op_b_lsb_c) = sys0_module_c) else '0';
		SYS_1_MODULE.R_EN    <= READ_ACC when (CP_CMD_I(cp_op_b_msb_c-1 downto cp_op_b_lsb_c) = sys1_module_c) else '0';
		COM_0_MODULE.R_EN    <= READ_ACC when (CP_CMD_I(cp_op_b_msb_c-1 downto cp_op_b_lsb_c) = com0_module_c) else '0';
		COM_1_MODULE.R_EN    <= READ_ACC when (CP_CMD_I(cp_op_b_msb_c-1 downto cp_op_b_lsb_c) = com1_module_c) else '0';

        -- Module Execute Command --
        CMD_EXE  <= CP_EN_I and (not CP_OP_I); -- true coprocessor command
		SYS_0_MODULE.CMD_EXE <= CMD_EXE  when (CP_CMD_I(cp_op_b_msb_c-1 downto cp_op_b_lsb_c) = sys0_module_c) else '0';
		SYS_1_MODULE.CMD_EXE <= CMD_EXE  when (CP_CMD_I(cp_op_b_msb_c-1 downto cp_op_b_lsb_c) = sys1_module_c) else '0';
		COM_0_MODULE.CMD_EXE <= CMD_EXE  when (CP_CMD_I(cp_op_b_msb_c-1 downto cp_op_b_lsb_c) = com0_module_c) else '0';
		COM_1_MODULE.CMD_EXE <= CMD_EXE  when (CP_CMD_I(cp_op_b_msb_c-1 downto cp_op_b_lsb_c) = com1_module_c) else '0';



	-- System Controller 0 ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		system_ctrl_0: SYS_0_CORE
		port map (
					-- Host interface --
					CLK_I           => CLK_I,               -- global clock line
					RST_I           => RST_I,               -- global reset line, sync, high-active
					ICE_I           => ICE_I,               -- interface clock enable, high-active
					W_EN_I          => SYS_0_MODULE.W_EN,   -- write enable
					R_EN_I          => SYS_0_MODULE.R_EN,   -- read enable
					ADR_I           => CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c), -- access address
					DAT_I           => CP_DAT_I,            -- write data
					DAT_O           => SYS_0_MODULE.DATA_O, -- read data

					-- IRQ lines --
					TIMER_IRQ_O     => TIMER_IRQ,           -- timer irq
					IRQ_I           => INT_ASSIGN,          -- irq input
					IRQ_O           => CP_IRQ_O             -- interrupt request to cpu
				);

		-- IRQ assignment --
		INT_ASSIGN(0) <= TIMER_IRQ;   -- high precision timer irq
		INT_ASSIGN(1) <= WB_CORE_IRQ; -- Wishbone Interface CTRL IRQ
		INT_ASSIGN(2) <= UART_RX_IRQ; -- UART data received irq
		INT_ASSIGN(3) <= UART_TX_IRQ; -- UART data send irq
		INT_ASSIGN(4) <= SPI_IRQ;     -- SPI transfer done irq
		INT_ASSIGN(5) <= PIO_IRQ;     -- PIO input change IRQ
		INT_ASSIGN(6) <= '0';         -- reserved
		INT_ASSIGN(7) <= IRQ_I;       -- 'external' irq



	-- System Controller 1 ---------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		system_ctrl_1: SYS_1_CORE
		generic map (
						CLK_SPEED_G => CLOCK_SPEED_G         -- clock speed (inHz)
					)
		port map (
					-- Host interface --
					CLK_I           => CLK_I,               -- global clock line
					RST_I           => RST_I,               -- global reset line, sync, high-active
					ICE_I           => ICE_I,               -- interface clock enable, high-active
					W_EN_I          => SYS_1_MODULE.W_EN,   -- write enable
					R_EN_I          => SYS_1_MODULE.R_EN,   -- read enable
					ADR_I           => CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c), -- access address
					DAT_I           => CP_DAT_I,            -- write data
					DAT_O           => SYS_1_MODULE.DATA_O, -- read data

					-- CPU-special --
					SYS_MODE_I      => SYS_MODE_I,          -- current operating mode
					INT_EXE_I       => INT_EXE_I,           -- interrupt beeing executed

					-- Memory Interface --
					MEM_IP_ADR_O    => MEM_IP_ADR_O,        -- instruction page
					MEM_DP_ADR_O    => MEM_DP_ADR_O         -- data page
				);



	-- Communication Controller 0 --------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		communication_ctrl_0: COM_0_CORE
		port map (
					-- Host interface --
					CLK_I           => CLK_I,               -- global clock line
					RST_I           => RST_I,               -- global reset line, sync, high-active
					ICE_I           => ICE_I,               -- interface clock enable, high-active
					W_EN_I          => COM_0_MODULE.W_EN,   -- write enable
					R_EN_I          => COM_0_MODULE.R_EN,   -- read enable
					ADR_I           => CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c), -- access address
					DAT_I           => CP_DAT_I,            -- write data
					DAT_O           => COM_0_MODULE.DATA_O, -- read data

					-- Interrupt Lines --
					UART_RX_IRQ_O   => UART_RX_IRQ,         -- UART IRQ "data available"
					UART_TX_IRQ_O   => UART_TX_IRQ,         -- UART IRQ "sending done"
					SPI_IRQ_O       => SPI_IRQ,             -- SPI IRQ "transfer done"
					PIO_IRQ_O       => PIO_IRQ,             -- PIO input pin change irq

					-- Peripheral Interface --
					UART_TXD_O      => UART_TXD_O,          -- UART transmitter
					UART_RXD_I      => UART_RXD_I,          -- UART receiver
					SPI_MOSI_O      => SPI_MOSI_O,          -- SPI master out slave in
					SPI_MISO_I      => SPI_MISO_I,          -- SPI master in slave out
					SPI_SCK_O       => SPI_SCK_O,           -- SPI clock out
					SPI_CS_O        => SPI_CS_O,            -- SPI chip select
					PIO_IN_I        => PIO_IN_I,            -- parallel input
					PIO_OUT_O       => PIO_OUT_O,           -- parallel output
					SYS_IO_I        => SYS_IN_I,            -- system input
					SYS_IO_O        => SYS_OUT_O            -- system output
				);



	-- Communication Controller 1 --------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		communication_ctrl_1: COM_1_CORE
		port map (
					-- Host interface --
                    WB_CLK_O        => WB_CLK_O,            -- bus clock
                    WB_RST_O        => WB_RST_O,            -- bus reset, sync, high active
					CLK_I           => CLK_I,               -- global clock line
					RST_I           => RST_I,               -- global reset line, sync, high-active
					ICE_I           => ICE_I,               -- interface clock enable, high-active
					W_EN_I          => COM_1_MODULE.W_EN,   -- write enable
					R_EN_I          => COM_1_MODULE.R_EN,   -- read enable
                    CMD_EXE_I       => COM_1_MODULE.CMD_EXE,-- execute command
                    ADR_I           => CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c), -- access address/command
					DAT_I           => CP_DAT_I,            -- write data
					DAT_O           => COM_1_MODULE.DATA_O, -- read data
                    IRQ_O           => WB_CORE_IRQ,         -- interrupt request

                    -- Wishbone Bus --
                    WB_ADR_O        => WB_ADR_O,            -- address
                    WB_SEL_O        => WB_SEL_O,            -- byte select
                    WB_DATA_O       => WB_DATA_O,           -- data out
                    WB_DATA_I       => WB_DATA_I,           -- data in
                    WB_WE_O         => WB_WE_O,             -- read/write
                    WB_CYC_O        => WB_CYC_O,            -- cycle enable
                    WB_STB_O        => WB_STB_O,            -- strobe
                    WB_ACK_I        => WB_ACK_I,            -- acknowledge
                    WB_ERR_I        => WB_ERR_I             -- bus error
				);



end SYSTEM_CP_BEHAV;
