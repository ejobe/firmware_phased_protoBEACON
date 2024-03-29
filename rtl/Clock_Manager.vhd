---------------------------------------------------------------------------------
-- Univ. of Chicago  
--    --KICP--
--
-- PROJECT:      phased-array trigger board
-- FILE:         clock_manager.vhd
-- AUTHOR:       e.oberla
-- EMAIL         ejo@uchicago.edu
-- DATE:         1/2016
--
-- DESCRIPTION:  clocks, top level manager
--
---------------------------------------------------------------------------------

library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity Clock_Manager is
	Port(
		Reset_i			:  in		std_logic;
		CLK0_i			:	in		std_logic;
		CLK1_i			:  in		std_logic;
		PLL_reset_i		:  in		std_logic;
		
		CLK_125MHz_o 	:  out	std_logic;
		CLK_31MHz_a_o		:  out	std_logic;  
		CLK_31MHz_b_o		:  inout	std_logic;  --//main logic clock
		CLK_10MHz_o		:	out	std_logic;
		CLK_1MHz_o		:  out	std_logic;
		CLK_1Hz_o		:  out	std_logic;
		CLK_10Hz_o		:  out	std_logic;
		CLK_1kHz_o		:	out	std_logic;
		CLK_100kHz_o	:	out	std_logic;
		CLK_120kHz_o	:	inout	std_logic;
		
		refresh_100Hz_o	:	out	std_logic;  --//refresh pulse derived from CLK_15MHz_o
		refresh_1Hz_o		:	out	std_logic;  --//refresh pulse derived from CLK_15MHz_o
		refresh_100mHz_o 	:	out	std_logic;	--//refresh pulse derived from CLK_15MHz_o every 10 s
		refresh_60Hz_o		:	out	std_logic;  --//refresh pulse derived from CLK_120kHz_o

		fpga_fastpllLock_o : inout std_logic;
		fpga_pllLock_o	:	inout	std_logic);  --lock signal from main PLL on fpga

end Clock_Manager;

architecture rtl of Clock_Manager is
	
	signal clk_1MHz_sig	: 	std_logic;
	signal clk_6MHz_sig	: 	std_logic;

	--//need to create a single pulse every Hz with width of 15 MHz clock period
	signal refresh_clk_counter_1Hz 	:	std_logic_vector(31 downto 0) := (others=>'0');
	signal refresh_clk_counter_100Hz 	:	std_logic_vector(31 downto 0) := (others=>'0');
	signal refresh_clk_counter_100mHz:	std_logic_vector(31 downto 0) := (others=>'0');
	signal refresh_clk_counter_60Hz:	std_logic_vector(31 downto 0) := (others=>'0');

	signal refresh_clk_1Hz				:	std_logic := '0';
	signal refresh_clk_100Hz			:	std_logic := '0';
	signal refresh_clk_100mHz			:	std_logic := '0';
	signal refresh_clk_60Hz			:	std_logic := '0';

	--//for 7.5 MHz
	--constant REFRESH_CLK_MATCH_1HZ 		: 	std_logic_vector(23 downto 0) := x"7270E0";  --//7.5e6
	--constant REFRESH_CLK_MATCH_100mHz 	: 	std_logic_vector(27 downto 0) := x"47868C0";  --//7.5e7
	--//for 15 MHz
	--constant REFRESH_CLK_MATCH_1HZ 		: 	std_logic_vector(27 downto 0) := x"0E4E1C0";  --//7.5e6
	--constant REFRESH_CLK_MATCH_100mHz 	: 	std_logic_vector(27 downto 0) := x"8F0D180";  --//7.5e7
	--//for 25 MHz
	--constant REFRESH_CLK_MATCH_1HZ 		: 	std_logic_vector(27 downto 0) := x"17D7840";  --//25e6
	--constant REFRESH_CLK_MATCH_100mHz 	: 	std_logic_vector(27 downto 0) := x"EE6B280";  --//25e7
	--//for 23.475 MHz
	--constant REFRESH_CLK_MATCH_1HZ 		: 	std_logic_vector(27 downto 0) := x"165A0BC";  --//23.4375e6
	--constant REFRESH_CLK_MATCH_100mHz 	: 	std_logic_vector(27 downto 0) := x"DF84758";  --//23.4375e6	
	--//for 31.25 MHz
	constant REFRESH_CLK_MATCH_100HZ 	: 	std_logic_vector(31 downto 0) := x"0004C4B4";  
	constant REFRESH_CLK_MATCH_1HZ 		: 	std_logic_vector(31 downto 0) := x"01DCD650";  
	constant REFRESH_CLK_MATCH_100mHz 	: 	std_logic_vector(31 downto 0) := x"12A05F20";  
	constant REFRESH_CLK_MATCH_60HZ 	: 	std_logic_vector(31 downto 0)    := x"000007D0";  --120kHz clock reference

	component pll_block
		port( refclk, rst			: in 	std_logic;
				outclk_0, outclk_1, 
				outclk_2,
				locked				: out	std_logic);
	end component;
	
	component pll_block_2
		port( refclk, rst			: in 	std_logic;
				outclk_0, outclk_1,
				locked				: out	std_logic);
	end component;

	component pll_block_3
		port( refclk, rst			: in 	std_logic;
				outclk_0, 
				locked				: out	std_logic);
	end component;
	
	component Slow_Clocks
		generic(clk_divide_by   : integer := 500);
		port( IN_CLK, Reset		: in	std_logic;
				OUT_CLK				: out	std_logic);
	end component;	
	
begin
	CLK_1MHz_o			<=	clk_1MHz_sig;
	refresh_1Hz_o		<= refresh_clk_1Hz;
	refresh_100mHz_o	<=	refresh_clk_100mHz;
	refresh_100Hz_o	<=	refresh_clk_100Hz;

	xPLL_BLOCK : pll_block
		port map(CLK0_i, PLL_reset_i, CLK_31MHz_a_o, CLK_31MHz_b_o, 
					clk_1MHz_sig, fpga_pllLock_o);
					
	xPLL_BLOCK_2 : pll_block_2
		port map(CLK1_i, PLL_reset_i or Reset_i, CLK_125MHz_o, CLK_10MHz_o, 
					fpga_fastpllLock_o);
	
	xPLL_BLOCK_3 : pll_block_3
		port map(CLK1_i, PLL_reset_i or Reset_i, clk_6MHz_sig, open); 

	xCLK_GEN_120kHz : Slow_Clocks
		generic map(clk_divide_by => 25)
		port map(clk_6MHz_sig, Reset_i, CLK_120kHz_o);
		
	xCLK_GEN_100kHz : Slow_Clocks
		generic map(clk_divide_by => 5)
		port map(clk_1MHz_sig, Reset_i, CLK_100kHz_o);
	
	xCLK_GEN_1kHz : Slow_Clocks
		generic map(clk_divide_by => 500)
		port map(clk_1MHz_sig, Reset_i, CLK_1kHz_o);

	xCLK_GEN_10Hz : Slow_Clocks
		generic map(clk_divide_by => 50000)
		port map(clk_1MHz_sig, Reset_i, CLK_10Hz_o);
		
	xCLK_GEN_1Hz : Slow_Clocks
		generic map(clk_divide_by => 500000)
		port map(clk_1MHz_sig, Reset_i, CLK_1Hz_o);
		
	--/////////////////////////////////////////////////////////////////////////////////
	--//make 1 Hz and 100mHz refresh pulses from the main iface clock (7.5 OR 15 MHz)
	proc_make_refresh_pulse : process(CLK_31MHz_b_o)
	begin
		if rising_edge(CLK_31MHz_b_o) then
			
			if refresh_clk_1Hz = '1' then
				refresh_clk_counter_1Hz <= (others=>'0');
			else
				refresh_clk_counter_1Hz <= refresh_clk_counter_1Hz + 1;
			end if;
			--//pulse refresh when refresh_clk_counter = REFRESH_CLK_MATCH
			case refresh_clk_counter_1Hz is
				when REFRESH_CLK_MATCH_1HZ =>
					refresh_clk_1Hz <= '1';
				when others =>
					refresh_clk_1Hz <= '0';
			end case;
			
			--//////////////////////////////////////
			
			if refresh_clk_100mHz = '1' then
				refresh_clk_counter_100mHz <= (others=>'0');
			else
				refresh_clk_counter_100mHz <= refresh_clk_counter_100mHz + 1;
			end if;
			--//pulse refresh when refresh_clk_counter = REFRESH_CLK_MATCH
			case refresh_clk_counter_100mHz is
				when REFRESH_CLK_MATCH_100mHz =>
					refresh_clk_100mHz <= '1';
				when others =>
					refresh_clk_100mHz <= '0';
			end case;
			
			--//////////////////////////////////////
			
			if refresh_clk_100Hz = '1' then
				refresh_clk_counter_100Hz <= (others=>'0');
			else
				refresh_clk_counter_100Hz <= refresh_clk_counter_100Hz + 1;
			end if;
			--//pulse refresh when refresh_clk_counter = REFRESH_CLK_MATCH
			case refresh_clk_counter_100Hz is
				when REFRESH_CLK_MATCH_100Hz =>
					refresh_clk_100Hz <= '1';
				when others =>
					refresh_clk_100Hz <= '0';
			end case;
		end if;
	end process;
	--//////////////////////////////////////
	proc_make_refresh_pulse_60cycle : process(CLK_120kHz_o)
	begin
	if rising_edge(CLK_120kHz_o) then

		if refresh_clk_60Hz = '1' then
				refresh_clk_counter_60Hz <= (others=>'0');
			else
				refresh_clk_counter_60Hz <= refresh_clk_counter_60Hz + 1;
			end if;
			--//pulse refresh when refresh_clk_counter = REFRESH_CLK_MATCH
			case refresh_clk_counter_60Hz is
				when REFRESH_CLK_MATCH_1HZ =>
					refresh_clk_60Hz <= '1';
				when others =>
					refresh_clk_60Hz <= '0';
			end case;
		end if;
	end process;
end rtl;