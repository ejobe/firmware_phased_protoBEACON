---------------------------------------------------------------------------------
-- Univ. of Chicago  
--    --KICP--
--
-- PROJECT:      phased-array trigger board
-- FILE:         sixty_cycle_veto.vhd
-- AUTHOR:       e.oberla
-- EMAIL         ejo@uchicago.edu
-- DATE:         7/2019...
--
-- DESCRIPTION:  
--
--
---------------------------------------------------------------------------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

use work.defs.all;
use work.register_map.all;

entity sixty_cycle_veto is
	port(
		rst_i					:	in		std_logic;
		clk_data_i			:	in		std_logic; --//data clock ~93 MHz
		clk_iface_i			: 	in		std_logic; --//data interface clock
		clk_120kHz_i		:  in		std_logic; --//clock from which the 60Hz reference is derived
		refpulse_60Hz_i	:  in		std_logic; --//60Hz pulse
			
		reg_i					: 	in		register_array_type; --//programmable registers
		
		trigger_in_i		:	in		std_logic; --//trigger in from system
		trigger_out_o		:	out	std_logic);--//trigger output from this module
		
end sixty_cycle_veto;
------------------------------------------------------------------------------------------------------------------------------
architecture rtl of sixty_cycle_veto is
------------------------------------------------------------------------------------------------------------------------------
--local signals:
signal sixty_cycle_flag : std_logic;
signal sixty_cycle_flag_align : std_logic;

signal sixty_cycle_veto : std_logic:
signal internal_trig		: std_logic;

signal clock_120kHz_cycles : std_logic_vector(11 downto 0) := x"7D0"; --2000 clock cycles = 60Hz
--//default settings: veto window 500 us long per 60Hz cycle =(30ms deadtime / second ~ 3%)
signal pre_window_cycles : std_logic_vector(11 downto 0) := x"01E"; -- 250 microsecs before 
signal post_window_cycles : std_logic_vector(11 downto 0) := x"01E"; -- 250 microsecs after

signal number_sixty_cycles : std_logic_Vector(7 downto 0) := x"A"; --//number of cycles after a trigger to test
signal threshold_num_triggers_sixty_cycle : std_logic_Vector(7 downto 0) := x"5"; --//

type adaptive_pulse_veto_state_type is (wait_for_trig_st, wait_st, pulse_st, check_st, done_st);
signal adaptive_pulse_veto_state : adaptive_pulse_veto_state_type ;
signal adaptive_pulse_veto_counter : std_logic_vector(11 downto 0) := x"000";

------------------------------------------------------------------------------------------------------------------------------
component flag_sync is
port(
	clkA			: in	std_logic;
   clkB			: in	std_logic;
   in_clkA		: in	std_logic;
   busy_clkA	: out	std_logic;
   out_clkB		: out	std_logic);
end component;
component signal_sync is
port(
	clkA			: in	std_logic;
   clkB			: in	std_logic;
   SignalIn_clkA	: in	std_logic;
   SignalOut_clkB	: out	std_logic);
end component;
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------
begin
------------------------------------------------------------------------------------------------------------------------------

xTRIGSYNC : flag_sync
	port map(
		clkA 			=> clk_data_i,
		clkB			=> clk_120kHz_i,
		in_clkA		=> trigger_in_i,
		busy_clkA	=> open,
		out_clkB		=> internal_trig);

proc_adaptive_veto : process(clk_120kHz_i, rst_i, internal_trig, sixty_cycle_flag_realign)
begin
	if rst_i = '1' then
		sixty_cycle_flag <= '0';
		adaptive_pulse_veto_counter <= (others=>'0');
		adaptive_pulse_veto_state <= wait_for_trig_st;
	
	elsif rising_edge(clk_120khz_i) then
		
		case adaptive_pulse_veto_state is
		
			when wait_for_trig_st =>
				adaptive_pulse_veto_counter <= (others=>'0');
				sixty_cycle_flag <= '0';
				if internal_trig = '1' and sixty_cycle_flag_realign = '1' then
					adaptive_pulse_veto_state <= wait_st;
				else
					adaptive_pulse_veto_state <= wait_for_trig_st;
				end if;
				
			when wait_st =>
				adaptive_pulse_veto_counter <= adaptive_pulse_veto_counter + 1;



					

				end if
		end case;
	end if;
end process;
------------//----------	
proc_veto_pulse_generator : process(clk_102kHz_i, rst_i)
begin



end rtl;


