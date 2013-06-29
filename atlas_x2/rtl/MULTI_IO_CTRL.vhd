-- #########################################################
-- #   << ATLAS Project - Multi Purpose IO Controller >>   #
-- # ***************************************************** #
-- #  Multi purpose peripheral controller.                 #
-- #  Modules:                                             #
-- #   - Configurable UART                                 #
-- #   - SPI Controller for up to 16 devices               #
-- #   - Custom IP Slot                                    #
-- #   - 16-bit Parallel I/O Controller                    #
-- #   - 8 Channel PWM Controller                          #
-- #   - 16+16-bit Timer                                   #
-- #   - Linear Feedback Shift Register                    #
-- #   - 8 Channel Interrupt Controller                    #
-- # ***************************************************** #
-- #  Last modified: 06.06.2013                            #
-- # ***************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany            #
-- #########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity MULTI_IO_CTRL is
-- ###############################################################################################
-- ##           Module Configuration                                                            ##
-- ###############################################################################################
	generic	(
				SYNTH_SPI_G     : boolean := true; -- synthesize SPI core
				SYNTH_CIP_G     : boolean := true; -- synthesize custom IP
				SYNTH_UART_G    : boolean := true; -- synthesize UART core
				SYNTH_PIO_G     : boolean := true; -- synthesize parallel IO core
				SYNTH_PWM_G     : boolean := true; -- synthesize PWM core
				SYNTH_INT_G     : boolean := true; -- synthesize interrupt controller
				SYNTH_LFSR_G    : boolean := true; -- synthesize LFSR
				SYNTH_TIME_G    : boolean := true  -- synthesize timer
			);
	port	(
-- ###############################################################################################
-- ##           Global Control                                                                  ##
-- ###############################################################################################

				CLK_I           : in    std_logic; -- global clock line
				RST_I           : in    std_logic; -- global reset line, sync, high-active
				ICE_I           : in    std_logic; -- interface clock enable, high-active

-- ###############################################################################################
-- ##           Processor Interface                                                             ##
-- ###############################################################################################

				CP_EN_I         : in    std_logic; -- access coprocessor
				CP_OP_I         : in    std_logic; -- data transfer/processing
				CP_RW_I         : in    std_logic; -- read/write access
				CP_CMD_I        : in    std_logic_vector(cp_cmd_width_c-1 downto 0); -- register addresses / cmd
				CP_DAT_I        : in    std_logic_vector(data_width_c-1   downto 0); -- write data
				CP_DAT_O        : out   std_logic_vector(data_width_c-1   downto 0); -- read data
				CP_IRQ_O        : out   std_logic; -- unit interrupt request

-- ###############################################################################################
-- ##           Peripheral Communication Interface                                              ##
-- ###############################################################################################

				-- UART --
				UART_RXD_I      : in    std_logic; -- receiver input
				UART_TXD_O      : out   std_logic; -- UART transmitter output

				-- SPI --
				SPI_CLK_O       : out   std_logic; -- serial clock output
				SPI_MOSI_O      : out   std_logic; -- serial data output
				SPI_MISO_I      : in    std_logic; -- serial data input
				SPI_CS_O        : out   std_logic_vector(15 downto 0); -- device select

				-- Parallel IO --
				PIO_OUT_O       : out   std_logic_vector(15 downto 0); -- parallel output
				PIO_IN_I        : in    std_logic_vector(15 downto 0); -- parallel input

				-- PWM --
				PWM_O           : out   std_logic_vector(07 downto 0); -- pwm channels

				-- IRQs --
				IRQ_I           : in    std_logic_vector(01 downto 0); -- SoC-internal IRQs

				-- LFSR --
				RND_I           : in    std_logic  -- external random feed
			);
end MULTI_IO_CTRL;

architecture MULTI_IO_CTRL_STRUCTURE of MULTI_IO_CTRL is

	-- Modules --
	constant uart_module_c      : std_logic_vector(2 downto 0) := "000";
	constant spi_module_c       : std_logic_vector(2 downto 0) := "001";
	constant pwm_module_c       : std_logic_vector(2 downto 0) := "010";
	constant pio_module_c       : std_logic_vector(2 downto 0) := "011";
	constant cip_module_c       : std_logic_vector(2 downto 0) := "100";
	constant lfsr_module_c      : std_logic_vector(2 downto 0) := "101";
	constant timer_module_c     : std_logic_vector(2 downto 0) := "110";
	constant int_module_c       : std_logic_vector(2 downto 0) := "111";

	-- Module interface --
	type module_interface_t is record
		DATA_IN                 : std_logic_vector(data_width_c-1 downto 0);
		DATA_OUT                : std_logic_vector(data_width_c-1 downto 0);
		ADR                     : std_logic_vector(02 downto 0);
		W_EN                    : std_logic;
		R_EN                    : std_logic;
		IRQ                     : std_logic;
	end record;

	signal PIO_MODULE           : module_interface_t;
	signal PWM_MODULE           : module_interface_t;
	signal TMR_MODULE           : module_interface_t;
	signal RND_MODULE           : module_interface_t;
	signal INT_MODULE           : module_interface_t;
	signal URT_MODULE           : module_interface_t;
	signal CIP_MODULE           : module_interface_t;
	signal SPI_MODULE           : module_interface_t;

	-- Raw interrupt signals --
	signal INT_ASSIGN           : std_logic_vector(7 downto 0);

	-- Internals --
	signal READ_ACC             : std_logic; -- true read access

	-- I2C Controller --
	signal I2C_R_DATA           : std_logic_vector(7 downto 0);
	signal SCL_PAD_O, SDA_PAD_O : std_logic;
	signal SCL_PAD_I, SDA_PAD_I : std_logic;
	signal SCL_PADOE, SDA_PADOE : std_logic;

	-- UART --
	signal UART_RX_IRQ          : std_logic;
	signal UART_TX_IRQ          : std_logic;

  -- Component: Parallel IO Controller ------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component PIO_CORE
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				IRQ_O           : out std_logic; -- interrupt request

				-- Peripheral Interface --
				PIO_OUT_O       : out std_logic_vector(data_width_c-1 downto 0); -- parallel output
				PIO_IN_I        : in  std_logic_vector(data_width_c-1 downto 0)  -- parallel input
			);
  end component;

  -- Component: Pulse-Width-Controller Controller -------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component PWM_CORE
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				IRQ_O           : out std_logic; -- interrupt request

				-- Peripheral Interface --
				PWM_O           : out std_logic_vector(7 downto 0) -- pwm channels
			);
  end component;

  -- Component: Linear Feedback Shift Register ----------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component LFSR_CORE
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				IRQ_O           : out std_logic; -- interrupt request

				-- External Noise Source --
				RND_I           : in  std_logic
			);
  end component;

  -- Component: General Purpose Timer -------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component TIMER_CORE
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				IRQ_O           : out std_logic  -- interrupt request
			);
  end component;

  -- Interrupt Controller -------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component INT_CORE
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				IRQ_O           : out std_logic; -- interrupt request

				-- Interrupt lines --
				INT_REQ_I       : in  std_logic_vector(07 downto 0)  -- request lines
			);
  end component;

  -- Serial Peripheral Interface Controller -------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component SPI_CORE
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				IRQ_O           : out std_logic; -- interrupt request

				-- SPI Port --
				SPI_CS_O        : out std_logic_vector(15 downto 0); -- slave select
				SPI_SCK_O       : out std_logic; -- serial master clock
				SPI_MOSI_O      : out std_logic; -- serial data output
				SPI_MISO_I      : in  std_logic  -- serial data input
			);
  end component;

  -- UART Controller ------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component UART_CORE
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				RX_IRQ_O        : out std_logic; -- received byte interrupt
				TX_IRQ_O        : out std_logic; -- transmitter ready interrupt

				-- UART Port --
				TXD_O           : out std_logic; -- serial data output
				RXD_I           : in  std_logic  -- serial data input
			);
  end component;

  -- Custom IP Module -----------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  component CIP_CORE
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				RST_I           : in  std_logic; -- global reset line, sync, high-active
				ICE_I           : in  std_logic; -- interface clock enable, high-active
				W_EN_I          : in  std_logic; -- write enable
				R_EN_I          : in  std_logic; -- read enable
				ADR_I           : in  std_logic_vector(02 downto 0); -- access address
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				IRQ_O           : out std_logic  -- interrupt request
			);
  end component;

begin

	-- Write Access Logic ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		CTRL_W_ACC: process(CP_EN_I, CP_RW_I, CP_OP_I, CP_CMD_I)
			variable valid_acc_v : std_logic;
		begin
			-- Valid Write Access? --
			valid_acc_v := CP_EN_I and CP_RW_I and CP_OP_I;

			-- Address Decoder --
			PIO_MODULE.W_EN <= '0';
			PWM_MODULE.W_EN <= '0';
			TMR_MODULE.W_EN <= '0';
			RND_MODULE.W_EN <= '0';
			INT_MODULE.W_EN <= '0';
			URT_MODULE.W_EN <= '0';
			CIP_MODULE.W_EN <= '0';
			SPI_MODULE.W_EN <= '0';
			case (CP_CMD_I(cp_op_a_msb_c downto cp_op_a_lsb_c)) is
				when pwm_module_c   => PWM_MODULE.W_EN <= valid_acc_v;
				when pio_module_c   => PIO_MODULE.W_EN <= valid_acc_v;
				when timer_module_c => TMR_MODULE.W_EN <= valid_acc_v;
				when lfsr_module_c  => RND_MODULE.W_EN <= valid_acc_v;
				when int_module_c   => INT_MODULE.W_EN <= valid_acc_v;
				when uart_module_c  => URT_MODULE.W_EN <= valid_acc_v;
				when CIP_module_c   => CIP_MODULE.W_EN <= valid_acc_v;
				when spi_module_c   => SPI_MODULE.W_EN <= valid_acc_v;
				when others         => NULL;
			end case;
		end process CTRL_W_ACC;

		-- Module connection --
		PIO_MODULE.ADR     <= CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c);
		PIO_MODULE.DATA_IN <= CP_DAT_I;
		PWM_MODULE.ADR     <= CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c);
		PWM_MODULE.DATA_IN <= CP_DAT_I;
		TMR_MODULE.ADR     <= CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c);
		TMR_MODULE.DATA_IN <= CP_DAT_I;
		RND_MODULE.ADR     <= CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c);
		RND_MODULE.DATA_IN <= CP_DAT_I;
		INT_MODULE.ADR     <= CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c);
		INT_MODULE.DATA_IN <= CP_DAT_I;
		URT_MODULE.ADR     <= CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c);
		URT_MODULE.DATA_IN <= CP_DAT_I;
		CIP_MODULE.ADR     <= CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c);
		CIP_MODULE.DATA_IN <= CP_DAT_I;
		SPI_MODULE.ADR     <= CP_CMD_I(cp_cmd_msb_c downto cp_cmd_lsb_c);
		SPI_MODULE.DATA_IN <= CP_DAT_I;



	-- Read Access Logic -----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		CTRL_R_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					CP_DAT_O <= (others => '0');
				elsif ((CP_EN_I and CP_OP_I) = '1') and (CP_RW_I = '0') then -- valid read
					case (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c)) is
						when pwm_module_c   => CP_DAT_O <= PWM_MODULE.DATA_OUT;
						when pio_module_c   => CP_DAT_O <= PIO_MODULE.DATA_OUT;
						when timer_module_c => CP_DAT_O <= TMR_MODULE.DATA_OUT;
						when lfsr_module_c  => CP_DAT_O <= RND_MODULE.DATA_OUT;
						when int_module_c   => CP_DAT_O <= INT_MODULE.DATA_OUT;
						when uart_module_c  => CP_DAT_O <= URT_MODULE.DATA_OUT;
						when cip_module_c   => CP_DAT_O <= CIP_MODULE.DATA_OUT;
						when spi_module_c   => CP_DAT_O <= SPI_MODULE.DATA_OUT;
						when others         => CP_DAT_O <= (others => '0');
					end case;
				end if;
			end if;
		end process CTRL_R_ACC;

		-- Module connection --
		READ_ACC        <= CP_EN_I and (not CP_RW_I) and CP_OP_I; -- general read access
		PIO_MODULE.R_EN <= '1' when (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c) = pio_module_c)   and (READ_ACC = '1') else '0';
		PWM_MODULE.R_EN <= '1' when (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c) = pwm_module_c)   and (READ_ACC = '1') else '0';
		TMR_MODULE.R_EN <= '1' when (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c) = timer_module_c) and (READ_ACC = '1') else '0';
		RND_MODULE.R_EN <= '1' when (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c) = lfsr_module_c)  and (READ_ACC = '1') else '0';
		INT_MODULE.R_EN <= '1' when (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c) = int_module_c)   and (READ_ACC = '1') else '0';
		URT_MODULE.R_EN <= '1' when (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c) = uart_module_c)  and (READ_ACC = '1') else '0';
		CIP_MODULE.R_EN <= '1' when (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c) = cip_module_c)   and (READ_ACC = '1') else '0';
		SPI_MODULE.R_EN <= '1' when (CP_CMD_I(cp_op_b_msb_c downto cp_op_b_lsb_c) = spi_module_c)   and (READ_ACC = '1') else '0';



	-- Parallel IO Controller ------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	synthesize_pio_core:
		if (SYNTH_PIO_G = true) generate
			inst_pio_core: PIO_CORE
			port map (
						CLK_I           => CLK_I,               -- global clock line
						RST_I           => RST_I,               -- global reset line, sync, high-active
						ICE_I           => ICE_I,               -- interface clock enable, high-active
						W_EN_I          => PIO_MODULE.W_EN,     -- write enable
						R_EN_I          => PIO_MODULE.R_EN,     -- read enable
						ADR_I           => PIO_MODULE.ADR,      -- access address
						DAT_I           => PIO_MODULE.DATA_IN,  -- write data
						DAT_O           => PIO_MODULE.DATA_OUT, -- read data
						IRQ_O           => PIO_MODULE.IRQ,      -- interrupt request
						PIO_OUT_O       => PIO_OUT_O,           -- parallel output
						PIO_IN_I        => PIO_IN_I             -- parallel input
					);
		end generate;

	ignore_pio_core:
		if (SYNTH_PIO_G = false) generate
			PIO_MODULE.DATA_OUT <= (others => '0');
			PIO_MODULE.IRQ      <= '0';
			PIO_OUT_O           <= (others => '0');
		end generate;



	-- Pulse-Width-Modulation Controller -------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	synthesize_pwm_core:
		if (SYNTH_PWM_G = true) generate
			inst_pwm_core: PWM_CORE
			port map (
						CLK_I           => CLK_I,               -- global clock line
						RST_I           => RST_I,               -- global reset line, sync, high-active
						ICE_I           => ICE_I,               -- interface clock enable, high-active
						W_EN_I          => PWM_MODULE.W_EN,     -- write enable
						R_EN_I          => PWM_MODULE.R_EN,     -- read enable
						ADR_I           => PWM_MODULE.ADR,      -- access address
						DAT_I           => PWM_MODULE.DATA_IN,  -- write data
						DAT_O           => PWM_MODULE.DATA_OUT, -- read data
						IRQ_O           => open,                -- interrupt request
						PWM_O           => PWM_O                -- pwm output
					);
		end generate;
		PWM_MODULE.IRQ <= '0'; -- device has no interrupt

	ignore_pwm_core:
		if (SYNTH_PWM_G = false) generate
			PWM_MODULE.DATA_OUT <= (others => '0');
			PWM_O               <= (others => '0');
		end generate;



	-- Timer -----------------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	synthesize_timer:
		if (SYNTH_TIME_G = true) generate
			inst_timer: TIMER_CORE
			port map (
						CLK_I           => CLK_I,               -- global clock line
						RST_I           => RST_I,               -- global reset line, sync, high-active
						ICE_I           => ICE_I,               -- interface clock enable, high-active
						W_EN_I          => TMR_MODULE.W_EN,     -- write enable
						R_EN_I          => TMR_MODULE.R_EN,     -- read enable
						ADR_I           => TMR_MODULE.ADR,      -- access address
						DAT_I           => TMR_MODULE.DATA_IN,  -- write data
						DAT_O           => TMR_MODULE.DATA_OUT, -- read data
						IRQ_O           => TMR_MODULE.IRQ       -- interrupt request
					);
		end generate;

	ignore_timer:
		if (SYNTH_TIME_G = false) generate
			TMR_MODULE.DATA_OUT <= (others => '0');
			TMR_MODULE.IRQ      <= '0';
		end generate;



	-- Linear Feedback Shift Register ----------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	synthesize_lfsr:
		if (SYNTH_LFSR_G = true) generate
			inst_lfsr: LFSR_CORE
			port map (
						CLK_I           => CLK_I,               -- global clock line
						RST_I           => RST_I,               -- global reset line, sync, high-active
						ICE_I           => ICE_I,               -- interface clock enable, high-active
						W_EN_I          => RND_MODULE.W_EN,     -- write enable
						R_EN_I          => RND_MODULE.R_EN,     -- read enable
						ADR_I           => RND_MODULE.ADR,      -- access address
						DAT_I           => RND_MODULE.DATA_IN,  -- write data
						DAT_O           => RND_MODULE.DATA_OUT, -- read data
						IRQ_O           => open,                -- interrupt request
						RND_I           => RND_I                -- external noise source
					);
		end generate;
		RND_MODULE.IRQ <= '0'; -- device has no interrupt

	ignore_lfsr:
		if (SYNTH_LFSR_G = false) generate
			RND_MODULE.DATA_OUT <= (others => '0');
		end generate;



	-- Interrupt Controller --------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	synthesize_int_core:
		if (SYNTH_INT_G = true) generate
			inst_int_core: INT_CORE
			port map (
						CLK_I           => CLK_I,               -- global clock line
						RST_I           => RST_I,               -- global reset line, sync, high-active
						ICE_I           => ICE_I,               -- interface clock enable, high-active
						W_EN_I          => INT_MODULE.W_EN,     -- write enable
						R_EN_I          => INT_MODULE.R_EN,     -- read enable
						ADR_I           => INT_MODULE.ADR,      -- access address
						DAT_I           => INT_MODULE.DATA_IN,  -- write data
						DAT_O           => INT_MODULE.DATA_OUT, -- read data
						IRQ_O           => CP_IRQ_O,            -- interrupt request
						INT_REQ_I       => INT_ASSIGN           -- request lines
					);
		end generate;
		INT_MODULE.IRQ <= '0'; -- not used here

	ignore_int_core:
		if (SYNTH_INT_G = false) generate
			INT_MODULE.DATA_OUT <= (others => '0');
			CP_IRQ_O            <= '0';
		end generate;

		-- Interrupt Priority Assignment --
		INT_ASSIGN(0) <= IRQ_I(0);        -- SoC-internal IRQ 0
		INT_ASSIGN(1) <= TMR_MODULE.IRQ;  -- timer overflow interrupt 
		INT_ASSIGN(2) <= CIP_MODULE.IRQ;  -- CIP module
		INT_ASSIGN(3) <= UART_RX_IRQ;     -- UART byte received interrupt
		INT_ASSIGN(4) <= UART_TX_IRQ;     -- UART transmitter ready interrupt
		INT_ASSIGN(5) <= SPI_MODULE.IRQ;  -- SPI controller
		INT_ASSIGN(6) <= PIO_MODULE.IRQ;  -- parallel input controller pin interrupt
		INT_ASSIGN(7) <= IRQ_I(1);        -- SoC-internal IRQ 1



	-- Serial Peripheral Interface Controller --------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	synthesize_spi_core:
		if (SYNTH_SPI_G = true) generate
			inst_spi_core: SPI_CORE
			port map (
						CLK_I           => CLK_I,               -- global clock line
						RST_I           => RST_I,               -- global reset line, sync, high-active
						ICE_I           => ICE_I,               -- interface clock enable, high-active
						W_EN_I          => SPI_MODULE.W_EN,     -- write enable
						R_EN_I          => SPI_MODULE.R_EN,     -- read enable
						ADR_I           => SPI_MODULE.ADR,      -- access address
						DAT_I           => SPI_MODULE.DATA_IN,  -- write data
						DAT_O           => SPI_MODULE.DATA_OUT, -- read data
						IRQ_O           => SPI_MODULE.IRQ,      -- interrupt request
						SPI_CS_O        => SPI_CS_O,            -- slave select
						SPI_SCK_O       => SPI_CLK_O,           -- serial master clock
						SPI_MOSI_O      => SPI_MOSI_O,          -- serial data output
						SPI_MISO_I      => SPI_MISO_I           -- serial data input
					);
		end generate;

	ignore_spi_core:
		if (SYNTH_SPI_G = false) generate
			SPI_MODULE.DATA_OUT <= (others => '0');
			SPI_MODULE.IRQ      <= '0';
		end generate;



	-- Universal Asynchronous Receiver/Transmitter Controller ----------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	synthesize_UART_core:
		if (SYNTH_UART_G = true) generate
			inst_uart_core: UART_CORE
			port map (
						CLK_I           => CLK_I,               -- global clock line
						RST_I           => RST_I,               -- global reset line, sync, high-active
						ICE_I           => ICE_I,               -- interface clock enable, high-active
						W_EN_I          => URT_MODULE.W_EN,     -- write enable
						R_EN_I          => URT_MODULE.R_EN,     -- read enable
						ADR_I           => URT_MODULE.ADR,      -- access address
						DAT_I           => URT_MODULE.DATA_IN,  -- write data
						DAT_O           => URT_MODULE.DATA_OUT, -- read data
						RX_IRQ_O        => UART_RX_IRQ,         -- received byte interrupt
						TX_IRQ_O        => UART_TX_IRQ,         -- transmitter ready interrupt
						TXD_O           => UART_TXD_O,          -- serial data output
						RXD_I           => UART_RXD_I           -- serial data input
					);
		end generate;

	ignore_uart_core:
		if (SYNTH_UART_G = false) generate
			URT_MODULE.DATA_OUT <= (others => '0');
			UART_RX_IRQ         <= '0';
			UART_TX_IRQ         <= '0';
			UART_TXD_O          <= '1';
		end generate;



	-- Custom IP Module ------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
	synthesize_cip_core:
		if (SYNTH_CIP_G = true) generate
			inst_cip_core: CIP_CORE
			port map (
						CLK_I           => CLK_I,               -- global clock line
						RST_I           => RST_I,               -- global reset line, sync, high-active
						ICE_I           => ICE_I,               -- interface clock enable, high-active
						W_EN_I          => CIP_MODULE.W_EN,     -- write enable
						R_EN_I          => CIP_MODULE.R_EN,     -- read enable
						ADR_I           => CIP_MODULE.ADR,      -- access address
						DAT_I           => CIP_MODULE.DATA_IN,  -- write data
						DAT_O           => CIP_MODULE.DATA_OUT, -- read data
						IRQ_O           => CIP_MODULE.IRQ       -- interrupt request
					);
		end generate;

	ignore_cip_core:
		if (SYNTH_CIP_G = false) generate
			CIP_MODULE.DATA_OUT <= (others => '0');
			CIP_MODULE.IRQ      <= '0';
		end generate;



end MULTI_IO_CTRL_STRUCTURE;
