-- #########################################################
-- #   << ATLAS Project - Multi Purpose IO Controller >>   #
-- #        << Pulse-Width-Modulation Controller >>        #
-- # ***************************************************** #
-- #  8 PWM channels, each 8 bit deep.                     #
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

entity PWM_CORE is
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
				DAT_I           : in  std_logic_vector(data_width_c-1 downto 0); -- write data
				DAT_O           : out std_logic_vector(data_width_c-1 downto 0); -- read data
				IRQ_O           : out std_logic; -- interrupt request

-- ###############################################################################################
-- ##           Peripheral Interface                                                            ##
-- ###############################################################################################

				-- PWM Output --
				PWM_O           : out std_logic_vector(7 downto 0) -- pwm channels
			);
end PWM_CORE;

architecture PWM_CORE_STRUCTURE of PWM_CORE is

	-- Configuration --
	constant pwm_channels_c   : natural := 8; -- number of synthesized channels (max 8)
	constant pwm_clock_prsc_c : natural := 5; -- max PWM clock is 0.5*CLK/(2^pwm_clock_prsc_c)

	-- PWM counter --
	type pwm_cnt_t is array (0 to pwm_channels_c-1) of std_logic_vector(7 downto 0);
	signal PWM_CNT, PWM_CONF  : pwm_cnt_t;

	-- internal Buffer --
	signal CLK_DIV            : std_logic_vector(7 downto 0);
	signal PWM_CLK            : std_logic;

begin

	-- Write Access ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		W_ACC: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				if (RST_I = '1') then
					PWM_CONF <= (others => (others => '0'));
				elsif (ICE_I = '1') then
					if (W_EN_I = '1') then -- Duty-Cycle update
						case (ADR_I) is
							when "000"  => PWM_CONF(0) <= DAT_I(7 downto 0);
							when "001"  => PWM_CONF(1) <= DAT_I(7 downto 0);
							when "010"  => PWM_CONF(2) <= DAT_I(7 downto 0);
							when "011"  => PWM_CONF(3) <= DAT_I(7 downto 0);
							when "100"  => PWM_CONF(4) <= DAT_I(7 downto 0);
							when "101"  => PWM_CONF(5) <= DAT_I(7 downto 0);
							when "110"  => PWM_CONF(6) <= DAT_I(7 downto 0);
							when "111"  => PWM_CONF(7) <= DAT_I(7 downto 0);
							when others => NULL;
						end case;
					end if;
				end if;
			end if;
		end process W_ACC;

		-- No interrupts here --
		IRQ_O <= '0';



	-- Read Access -----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		R_ACC: process(ADR_I, PWM_CONF)
		begin
			case (ADR_I) is
				when "000"  => DAT_O <= x"00" & PWM_CONF(0);
				when "001"  => DAT_O <= x"00" & PWM_CONF(1);
				when "010"  => DAT_O <= x"00" & PWM_CONF(2);
				when "011"  => DAT_O <= x"00" & PWM_CONF(3);
				when "100"  => DAT_O <= x"00" & PWM_CONF(4);
				when "101"  => DAT_O <= x"00" & PWM_CONF(5);
				when "110"  => DAT_O <= x"00" & PWM_CONF(6);
				when "111"  => DAT_O <= x"00" & PWM_CONF(7);
				when others => DAT_O <= x"0000";
			end case;
		end process R_ACC;



	-- PWM Prescaler ---------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		CLK_DIVIDER: process(RST_I, CLK_I)
		begin
			if (RST_I = '1') then
				CLK_DIV <= (others => '0');
			elsif rising_edge(CLK_I) then
				CLK_DIV <= Std_Logic_Vector(unsigned(CLK_DIV) + 1);
			end if;
		end process CLK_DIVIDER;

		-- PWM_CLK is 0.5/(2^pwm_clock_prsc_c) of main CLK --
		PWM_CLK <= CLK_DIV(pwm_clock_prsc_c);



	-- PWM Counters ----------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		PWM_COUNTER_CTRL: process(RST_I, PWM_CLK)
		begin
			if (RST_I = '1') then
				PWM_CNT <= (others => (others => '0'));
				PWM_O   <= (others => '0');
			elsif rising_edge(PWM_CLK) then -- PWM counter
				PWM_O <= (others => '0');
				for i in 0 to pwm_channels_c-1 loop
					if (PWM_CONF(i) = x"00") then -- port inactive?
						PWM_O(i)   <= '0';
						PWM_CNT(i) <= (others => '0');
					elsif (PWM_CONF(i) = x"FF") then -- port always active?
						PWM_O(i)   <= '1';
						PWM_CNT(i) <= (others => '0');
					else
						PWM_CNT(i) <= Std_Logic_Vector(unsigned(PWM_CNT(i)) + 1);
						if (unsigned(PWM_CNT(i)) < unsigned(PWM_CONF(i))) then
							PWM_O(i) <= '1';
						else
							PWM_O(i) <= '0';
						end if;
					end if;
				end loop;
			end if;
		end process PWM_COUNTER_CTRL;



end PWM_CORE_STRUCTURE;
