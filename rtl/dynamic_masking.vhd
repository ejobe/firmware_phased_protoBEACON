---------------------------------------------------------------------------------
-- Univ. of Chicago  
--    --KICP--
--
-- PROJECT:      phased-array trigger board
-- FILE:         dynamic_masking.vhd
-- AUTHOR:       e.oberla
-- EMAIL         ejo@uchicago.edu
-- DATE:         8/2018, 
--
-- DESCRIPTION:  dynamic beam masking for proto beacon
--               
---------------------------------------------------------------------------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

use work.defs.all;

entity dynamic_masking is
	port(
		rst_i				:	in		std_logic;
		clk_i				: 	in		std_logic;
		clk_iface_i		:	in		std_logic;
		clk_update_i 	:	in		std_logic; --//100Hz or 1kHz scaler update 
		reg_i				: 	in		register_array_type;
		trig_beam_i		:	in		std_logic_vector(define_num_beams-1 downto 0); --//for scalers

		dynamic_beammask_clk_iface_o 	: out	std_logic_vector(define_num_beams-1 downto 0);
		dynamic_beammask_o 				: out	std_logic_vector(define_num_beams-1 downto 0));
		
end dynamic_masking;

architecture rtl of dynamic_masking is

signal internal_max_count : std_logic_vector(7 downto 0) := (others=>'0'); --//threshold at which the dynamic mask engages

constant scaler_width   : integer := 8;

type dynamic_scaler_array_type is array (define_num_beams-1 downto 0) of std_logic_vector(scaler_width-1 downto 0);
signal dynamic_scalers : dynamic_scaler_array_type;
signal dynamic_scalers_latched : dynamic_scaler_array_type;
signal scaler_threshold : std_logic_vector(7 downto 0);

signal internal_dynamic_beammask : std_logic_vector(define_num_beams-1 downto 0) := (others=>'0');

type dynamic_beammask_holdoff_counter_type is array (define_num_beams-1 downto 0) of std_logic_vector(15 downto 0);
signal dynamic_beammask_holdoff_counter : dynamic_beammask_holdoff_counter_type := (others=> (others=>'0'));
signal counter_holdon_time : std_logic_vector(15 downto 0);
constant counter_holdon_time_all_ones : std_logic_vector(counter_holdon_time'length-1 downto 0) := (others=>'1');

type dyn_beammask_fsm_type is (pass_st, mask_st);
type dyn_beammask_fsm_array_type is array (define_num_beams-1 downto 0) of dyn_beammask_fsm_type;
signal dyn_beammask_fsm : dyn_beammask_fsm_array_type := (others=>pass_st);

signal internal_beammask_en : std_logic := '0';

component scaler
generic( 
	WIDTH : integer);
port(
	rst_i 		: in 	std_logic;
	clk_i			: in	std_logic;
	refresh_i	: in	std_logic;
	count_i		: in	std_logic;
	scaler_o		: out std_logic_vector(scaler_width-1 downto 0));
end component;
-------
component signal_sync is
port(
	clkA			: in	std_logic;
   clkB			: in	std_logic;
   SignalIn_clkA	: in	std_logic;
   SignalOut_clkB	: out	std_logic);
end component;
-------
begin
-------
DynMaskSync : for i in 0 to define_num_beams-1 generate
	xDYNMASKSYNC : signal_sync
	port map(
		clkA				=> clk_iface_i,
		clkB				=> clk_i,
		SignalIn_clkA	=> internal_dynamic_beammask(i), 
		SignalOut_clkB	=> dynamic_beammask_o(i));
end generate;
--//
CounterThreshSync : for i in 0 to 15 generate
	xCOUNTERTHRESHSYNC : signal_sync
	port map(
		clkA				=> clk_iface_i,
		clkB				=> clk_iface_i,--clk_update_i,
		SignalIn_clkA	=> reg_i(94)(i), 
		SignalOut_clkB	=> counter_holdon_time(i));
end generate;
--//
ScalerThreshSync : for i in 0 to 7 generate
	xSCALERTHRESHSYNC : signal_sync
	port map(
		clkA				=> clk_iface_i,
		clkB				=> clk_iface_i, --clk_update_i,
		SignalIn_clkA	=> reg_i(93)(i), 
		SignalOut_clkB	=> scaler_threshold(i));
end generate;
--//
xDYNMASKENSYNC : signal_sync
port map(
	clkA				=> clk_iface_i,
	clkB				=> clk_iface_i, --clk_update_i,
	SignalIn_clkA	=> reg_i(93)(8), 
	SignalOut_clkB	=> internal_beammask_en);
--//
dynamic_beammask_clk_iface_o <= internal_dynamic_beammask;
--//
DynamicScalers : for i in 0 to define_num_beams-1 generate
	xDYNAMICSCALERS : scaler
	generic map(
		WIDTH => scaler_width)
	port map(
		rst_i => rst_i,
		clk_i => clk_iface_i,
		refresh_i => clk_update_i,
		count_i => trig_beam_i(i),
		scaler_o => dynamic_scalers(i));
end generate;
--//
--//here, latch dynamic scalers
proc_latch : process(rst_i, clk_iface_i, dynamic_scalers, internal_beammask_en)
begin
	
	for i in 0 to define_num_beams-1 loop
		
		if rst_i = '1' then
		
			dynamic_scalers_latched(i) <= (others=>'0');
		
		--//register 93 bit 8 is the enable bit	
		elsif rising_edge(clk_iface_i) and internal_beammask_en = '0' then
			
			dynamic_scalers_latched(i) <= (others=>'0');

		--//register 93 bit 8 is the enable bit
		elsif rising_edge(clk_iface_i) and internal_beammask_en = '1' then
		
			if clk_update_i = '1' then
				dynamic_scalers_latched(i) <= dynamic_scalers(i);		
			else
				dynamic_scalers_latched(i) <= dynamic_scalers_latched(i);
			end if;

		end if;
		
	end loop;
end process;
--//
proc_dyn_mask : process(rst_i, clk_iface_i, clk_update_i, internal_beammask_en, dynamic_beammask_holdoff_counter, 
								dynamic_scalers_latched,
								dyn_beammask_fsm, counter_holdon_time, scaler_threshold)
begin
	----------
	for i in 0 to define_num_beams-1 loop
	----------
	if rst_i = '1' then
		----------
			internal_dynamic_beammask(i) <= '0'; --//start with beam masked [0=mask, 1=enable]
			dynamic_beammask_holdoff_counter(i) <= (others=>'0');
			dyn_beammask_fsm(i) <= pass_st;
		----------
--	elsif clk_update_i = '1' and internal_beammask_en = '0' then
--		----------
--			internal_dynamic_beammask(i) <= '1'; --//if dynamic masking disabled, keep all beams enabled
--			dynamic_beammask_holdoff_counter(i) <= (others=>'0');
--			dyn_beammask_fsm(i) <= pass_st;
		----------
	--//clk_update_i derived from clk_iface_i, so OK to directly compare with register value
	--elsif rising_edge(clk_iface_i) and clk_update_i = '1' then --and internal_beammask_en = '1' then
	elsif rising_edge(clk_update_i) then
		----------
		if internal_beammask_en = '1' then
		----------
			case dyn_beammask_fsm(i) is
				--------------------------
				when pass_st =>
				--------------------------
					dynamic_beammask_holdoff_counter(i) <= (others=>'0');
					internal_dynamic_beammask(i) <= '1'; --//enabled
					
					--//if above programmable threshold, goto masking
					if (dynamic_scalers_latched(i) >  scaler_threshold(7 downto 0)) then 
						dyn_beammask_fsm(i) <= mask_st;
					else
						dyn_beammask_fsm(i) <= pass_st;
					end if;
				--------------------------
				when mask_st =>
				--------------------------	
					internal_dynamic_beammask(i) <= '0'; --//mask
		
					if (dynamic_beammask_holdoff_counter(i)	>= counter_holdon_time) or 
						(dynamic_beammask_holdoff_counter(i) = counter_holdon_time_all_ones) then			
						
						dynamic_beammask_holdoff_counter(i) <= (others=>'0');
						dyn_beammask_fsm(i) <= pass_st;
					else
						dynamic_beammask_holdoff_counter(i) <= dynamic_beammask_holdoff_counter(i) + 1;
						dyn_beammask_fsm(i) <= mask_st;
					end if;
					
				when others =>
					dyn_beammask_fsm(i) <= pass_st;
			----------
			end case;
			----------
		----------
		else
		----------
			internal_dynamic_beammask(i) <= '1'; --//if dynamic masking disabled, keep all beams enabled
			dynamic_beammask_holdoff_counter(i) <= (others=>'0');
			dyn_beammask_fsm(i) <= pass_st;
		end if;
	end if;
	----------
	end loop;
end process;

end rtl;