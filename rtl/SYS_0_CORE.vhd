-- #########################################################
-- #       << ATLAS Project - System Controller 0 >>       #
-- # ***************************************************** #
-- #  -> Interrupt Controller (8 channels)                 #
-- #  -> High Precision Timer (16+16 bit)                  #
-- #  -> Linear-Feedback Shift Register (16 bit)           #
-- # ***************************************************** #
-- #  Last modified: 29.01.2014                            #
-- # ***************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany            #
-- #########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity SYS_0_CORE is
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

-- ###############################################################################################
-- ##           Interrupt Lines                                                                 ##
-- ###############################################################################################

				TIMER_IRQ_O     : out std_logic; -- timer irq
				IRQ_I           : in  std_logic_vector(07 downto 0); -- irq input
				IRQ_O           : out std_logic  -- interrupt request
			);
end SYS_0_CORE;

architecture SYS_0_CORE_BEHAV of SYS_0_CORE is

	-- Module Addresses --
	constant irq_sm_reg_c      : std_logic_vector(02 downto 0) := "000"; -- R/W: Interrupt source and mask
	constant irq_conf_reg_c    : std_logic_vector(02 downto 0) := "001"; -- R/W: Interrupt type configuration
	-- lo byte: '1': level triggered, '0': edge triggered
	-- hi byte: '1': high level/rising edge, '0': low level/falling edge
	constant timer_cnt_reg_c   : std_logic_vector(02 downto 0) := "010"; -- R/W: Timer counter register
	constant timer_thr_reg_c   : std_logic_vector(02 downto 0) := "011"; -- R/W: Timer threshold register
	constant timer_prsc_reg_c  : std_logic_vector(02 downto 0) := "100"; -- R/W: Timer prescaler register
	constant lfsr_data_reg_c   : std_logic_vector(02 downto 0) := "101"; -- R/W: LFSR data register
	constant lfsr_poly_reg_c   : std_logic_vector(02 downto 0) := "110"; -- R/W: LFSR polynomial register
	-- bit 15: '0' new value after read access, '1' free running mode
	constant reserved_reg_c    : std_logic_vector(02 downto 0) := "111"; -- RESERVED

	-- IRQ Registers --
	signal IRQ_MASK_REG        : std_logic_vector(07 downto 0);
	signal IRQ_SOURCE_REG      : std_logic_vector(02 downto 0);
	signal IRQ_CONF_REG        : std_logic_vector(15 downto 0);

	-- Internals --
	signal IRQ_SYNC_0          : std_logic_vector(07 downto 0);
	signal IRQ_SYNC_1          : std_logic_vector(07 downto 0);
	signal IRQ_RAW_REQ         : std_logic_vector(07 downto 0);
	signal IRQ_BUF             : std_logic_vector(07 downto 0);
	signal IRQ_ID              : std_logic_vector(02 downto 0);
	signal IRQ_ACK_MASK        : std_logic_vector(07 downto 0);
	signal IRQ_ACK_MASK_FF     : std_logic_vector(07 downto 0);
	signal IRQ_LOCK            : std_logic;

	-- Timer Registers --
	signal TMR_CNT_REG         : std_logic_vector(15 downto 0);
	signal TMR_THR_REG         : std_logic_vector(15 downto 0);
	signal TMR_PRSC_REG        : std_logic_vector(15 downto 0);
	signal TMR_PRSC_CNT        : std_logic_vector(15 downto 0);

	-- Timer Signals --
	signal TMR_PRSC_MATCH      : std_logic;
	signal TMR_THRES_ZERO      : std_logic;

	-- LFSR Registers --
	signal LFSR_DATA           : std_logic_vector(15 downto 0);
	signal LFSR_POLY           : std_logic_vector(15 downto 0);
	signal LFSR_NEW            : std_logic_vector(15 downto 0);
	signal LFSR_NOISE          : std_logic;

begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					IRQ_MASK_REG <= (others => '0');
					IRQ_CONF_REG <= (others => '0');
					TMR_CNT_REG  <= (others => '0');
					TMR_THR_REG  <= (others => '0');
					TMR_PRSC_REG <= (others => '0');
					TMR_PRSC_CNT <= (others => '0');
					LFSR_DATA    <= (others => '0');
					LFSR_POLY    <= (others => '0');
					IRQ_SYNC_0   <= (others => '0');
					IRQ_SYNC_1   <= (others => '0');
				else
					-- IRQ write access --
					if (W_EN_I = '1') and (ICE_I = '1') and ((ADR_I = irq_sm_reg_c) or (ADR_I = irq_conf_reg_c)) then
						if (ADR_I = irq_sm_reg_c) then
							IRQ_MASK_REG <= DAT_I(15 downto 08);
						else -- (ADR_I = irq_conf_reg_c)
							IRQ_CONF_REG <= DAT_I;
						end if;
					end if;
					IRQ_SYNC_1 <= IRQ_SYNC_0;
					IRQ_SYNC_0 <= IRQ_I;

					-- TIMER write access --
					if (W_EN_I = '1') and (ICE_I = '1') and ((ADR_I = timer_cnt_reg_c) or (ADR_I = timer_thr_reg_c) or (ADR_I = timer_prsc_reg_c)) then
						TMR_PRSC_CNT <= (others => '0');
						if (ADR_I = timer_cnt_reg_c) then
							TMR_CNT_REG  <= DAT_I;
						elsif (ADR_I = timer_thr_reg_c) then
							TMR_THR_REG  <= DAT_I;
						else -- (ADR_I = timer_prsc_reg_c)
							TMR_PRSC_REG <= DAT_I;
						end if;
					else -- auto update
						if (TMR_PRSC_MATCH = '1') or (TMR_THRES_ZERO = '1') then -- prescaler increment
							TMR_PRSC_CNT <= (others => '0');
						else
							TMR_PRSC_CNT <= std_logic_vector(unsigned(TMR_PRSC_CNT) + 1);
						end if;
						if (TMR_CNT_REG = TMR_THR_REG) then -- counter increment
							TMR_CNT_REG <= (others => '0');
						elsif (TMR_THRES_ZERO = '0') and (TMR_PRSC_MATCH = '1') then
							TMR_CNT_REG <= std_logic_vector(unsigned(TMR_CNT_REG) + 1);
						end if;
					end if;

					-- LFSR write access --
					if (W_EN_I = '1') and (ICE_I = '1') and ((ADR_I = lfsr_data_reg_c) or (ADR_I = lfsr_poly_reg_c)) then
						if (ADR_I = lfsr_data_reg_c) then
							 LFSR_DATA <= DAT_I;
						else -- (ADR_I = lfsr_poly_reg_c)
							LFSR_POLY <= DAT_I;
						end if;
					else -- auto update
						if (LFSR_POLY(15) = '0') then -- access-update?
							if (R_EN_I = '1') and (ADR_I = lfsr_data_reg_c) and (ICE_I = '1') then
								LFSR_DATA <= LFSR_NEW;
							end if;
						else -- free-running mode
							LFSR_DATA <= LFSR_NEW;
						end if;
					end if;
				end if;
			end if;
		end process W_ACC;

		-- Timer prescaler match --
		TMR_PRSC_MATCH <= '1' when (TMR_PRSC_CNT = TMR_PRSC_REG) else '0';

		-- Timer threshold zero test --
		TMR_THRES_ZERO <= '1' when (TMR_THR_REG = x"0000") else '0';

		-- Timer IRQ --
		TIMER_IRQ_O    <= '1'  when ((TMR_CNT_REG = TMR_THR_REG) and (TMR_THRES_ZERO = '0')) else '0';



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, IRQ_MASK_REG, IRQ_CONF_REG, IRQ_SOURCE_REG, TMR_CNT_REG, TMR_THR_REG,
		               TMR_PRSC_REG, LFSR_DATA, LFSR_POLY)
		begin
			case (ADR_I) is
				when irq_sm_reg_c     => DAT_O <= IRQ_MASK_REG & "00000" & IRQ_SOURCE_REG;
				when irq_conf_reg_c   => DAT_O <= IRQ_CONF_REG;
				when timer_cnt_reg_c  => DAT_O <= TMR_CNT_REG;
				when timer_thr_reg_c  => DAT_O <= TMR_THR_REG;
				when timer_prsc_reg_c => DAT_O <= TMR_PRSC_REG;
				when lfsr_data_reg_c  => DAT_O <= LFSR_DATA;
				when lfsr_poly_reg_c  => DAT_O <= LFSR_POLY;
				when others           => DAT_O <= (others => '0');
			end case;
		end process R_ACC;



	-- Interrupt Detector ----------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		IRQ_DETECTOR: process(IRQ_MASK_REG, IRQ_CONF_REG, IRQ_SYNC_0, IRQ_SYNC_1)
		begin
			-- Edge/Level detector --
			IRQ_RAW_REQ <= (others => '0');
			for i in 0 to 7 loop
				if (IRQ_MASK_REG(i) = '1') then -- channel enabled
					if (IRQ_CONF_REG(i) = '1') then -- level triggered
						IRQ_RAW_REQ(i) <= IRQ_CONF_REG(i+8) xnor IRQ_SYNC_0(i);
					else -- edge triggered
						if (IRQ_CONF_REG(i+8) = '1') then -- rising edge
							IRQ_RAW_REQ(i) <= IRQ_SYNC_0(i) and (not IRQ_SYNC_1(i));
						else -- falling edge
							IRQ_RAW_REQ(i) <= (not IRQ_SYNC_0(i)) and IRQ_SYNC_1(i);
						end if;
					end if;
				end if;
			end loop;
		end process IRQ_DETECTOR;



	-- Interrupt Request Buffer ----------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		IRQ_BUFFER: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					IRQ_BUF         <= (others => '0');
					IRQ_SOURCE_REG  <= (others => '0');
					IRQ_ACK_MASK_FF <= (others => '0');
					IRQ_LOCK        <= '0';
				else
					if (IRQ_LOCK = '0') then -- store ID and mask until ACK
						IRQ_ACK_MASK_FF <= IRQ_ACK_MASK;
						IRQ_SOURCE_REG  <= IRQ_ID;
					end if;
					if (R_EN_I = '1') and (ADR_I = irq_sm_reg_c) then -- ack on source reg read
						IRQ_BUF  <= (IRQ_BUF or IRQ_RAW_REQ) and (not IRQ_ACK_MASK_FF);
						IRQ_LOCK <= '0'; -- ACK: remove lock
					else
						IRQ_BUF  <= IRQ_BUF or IRQ_RAW_REQ;
						if (IRQ_BUF /= x"00") then
							IRQ_LOCK <= '1';
						end if;
					end if;
				end if;
			end if;
		end process IRQ_BUFFER;

		-- IRQ signal to host --
		IRQ_O <= IRQ_LOCK;



	-- Interrupt Priority Encoder --------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		IRQ_PR_ENC: process(IRQ_BUF)
		begin
			IRQ_ID <= (others => '0');
			IRQ_ACK_MASK <= (others => '0');
			for i in 0 to 7 loop
				if (IRQ_BUF(i) = '1') then
					IRQ_ID <= std_logic_vector(to_unsigned(i,3));
					IRQ_ACK_MASK(i) <= '1';
					exit;
				end if;
			end loop;
		end process IRQ_PR_ENC;



	-- LFSR Update -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		LFSR_UPDATE: process(LFSR_DATA, LFSR_POLY, LFSR_NOISE)
		begin
			for i in 0 to 14 loop
				if (LFSR_POLY(i) = '1') then
					LFSR_NEW(i) <= LFSR_DATA(i+1) xor LFSR_DATA(0);
				else
					LFSR_NEW(i) <= LFSR_DATA(i+1);
				end if;
			end loop;
			LFSR_NEW(15) <= LFSR_DATA(0) xor LFSR_NOISE;
		end process LFSR_UPDATE;

		-- External noise input --
		LFSR_NOISE <= '0'; -- not used yet



end SYS_0_CORE_BEHAV;
