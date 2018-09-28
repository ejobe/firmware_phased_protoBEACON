---------------------------------------------------------------------------------------------------------
-- Univ. of Chicago  
--    --KICP--
--
-- PROJECT:      phased-array trigger board
-- FILE:         trigger_veto.vhd
-- AUTHOR:       e.oberla
-- EMAIL         ejo@uchicago.edu
-- DATE:         9/2018
--
-- DESCRIPTION:  
-----////////////////////////////////////////////////////////////////////////////////////////////////////
---------------------------------------------------------------------------------------------------------

library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
--use ieee.float_pkg.all;

use work.defs.all;

entity trigger_veto is
	port(
		rst_i			:	in		std_logic;
		clk_i			: 	in		std_logic;
		clk_iface_i	:	in		std_logic;
			
		reg_i			: 	in		register_array_type;
		data_i		:	in	   halfpol_data_type;

		veto_o		:	out	std_logic);
		
end trigger_veto;

architecture rtl of trigger_veto is

signal buf_data_0 		: 	halfpol_data_type;
signal buf_data_1 		: 	halfpol_data_type;
signal buf_data_2 		: 	halfpol_data_type;
signal buf_data_3 		: 	halfpol_data_type;
signal buf_data_4 		: 	halfpol_data_type;
signal buf_data_5 		: 	halfpol_data_type;

--//buffer the data 5x every clock cycle --> allows beam-forming +/- the central buffer
type internal_buf_data_type is array (3 downto 0) of std_logic_vector(6*pdat_size-1 downto 0); --16 * 5 = 80 samples deep buffer
signal dat : internal_buf_data_type;

constant slice_base : integer := 4*pdat_size; 
-------------------------------------------------------------------------------
--//veto enables
signal internal_sat_veto_en : std_logic; 
signal internal_cw_veto_en  : std_logic;
--//generated veto signal
signal veto: std_logic; --//trigger veto based on extended pulse saturation
-------------------------------------------------------------------------------
--//extended signal saturation veto signal (or more generally, a time-over-theshold veto)
signal sat_veto_cut_value : std_logic_vector(7 downto 0);
signal internal_sat_veto: std_logic_vector(3 downto 0); --//per-trigger-channel veto based on extended pulse saturation
type sat_veto_reg_type is array (3 downto 0) of std_logic_vector(6 downto 0);
signal internal_sat_veto_reg: sat_veto_reg_type;

type sat_veto_type is array (3 downto 0) of std_logic_vector(2*define_serdes_factor-1 downto 0);
signal internal_sat_veto_tmp : sat_veto_type;

constant clock_cycles_saturated : integer := 3; --//3 * 1/31.25MHz = 96 ns 
-------------------------------------------------------------------------------
--//low-band cw veto signals
signal cw_veto_cut_value : std_logic_vector(7 downto 0);

signal internal_cw_veto: std_logic_vector(3 downto 0); --//per-trigger-channel veto based on low band cw

type internal_delay_and_sum_waveform_type is array (3 downto 0) of std_logic_vector(2*pdat_size-1 downto 0);
signal internal_delay_and_sum_waveform : internal_delay_and_sum_waveform_type;
signal internal_normal_waveform : internal_delay_and_sum_waveform_type;

type internal_vpp_type is array (3 downto 0) of integer range -1 to 512;
signal internal_vpp_wfm : internal_vpp_type;
signal internal_vpp_summed_wfm : internal_vpp_type;

signal internal_cw_veto_reg: sat_veto_reg_type;

constant clock_cycles_cw : integer := 2;
constant sample_delay : integer := 35; --//number of samples to delay added signal
-------------------------------------------------------------------------------
--//side-swipe veto 
signal internal_sideswipe_veto_reg: sat_veto_reg_type;
signal internal_sideswipe_veto: std_logic_vector(3 downto 0); --//per-trigger-channel veto based on low band cw

constant sideswipe_cut : integer := 16; --//if 
-------------------------------------------------------------------------------
--//signals for generating veto pulse
type veto_pulse_state_type is (idle_st, pulse_st);
signal veto_pulse_state : veto_pulse_state_type;
signal internal_veto_pulse_width : std_logic_vector(7 downto 0); --//programmable veto pulse width
signal internal_veto_pulse_width_counter : std_logic_vector(7 downto 0); 
--//
component signal_sync is
port
	(clkA			: in	std_logic;
   clkB			: in	std_logic;
   SignalIn_clkA	: in	std_logic;
   SignalOut_clkB	: out	std_logic);
end component;
--//
function vector_or(s : std_logic_vector) return std_logic is
	variable temp : integer := 0;
begin
	for i in s'range loop
		if s(i) = '1' then temp := temp + 1; 
		end if;
	end loop;
  
	if temp > 0 then
		return '1';
	else	
		return '0';
	end if;
end function vector_or;
--//get_vpp function. argument vector 's' is unsigned
function get_vpp(s : std_logic_vector) return integer is
	variable temp_min : integer := 64;
	variable temp_max : integer := 128;
	variable vpp : integer := 1;
begin
	for j in 0 to 4*define_serdes_factor-1 loop
		if s((j+1)*define_word_size-1 downto j*define_word_size) > temp_max then
			temp_max := to_integer(unsigned(s((j+1)*define_word_size-1 downto j*define_word_size)));
		end if;
		if s((j+1)*define_word_size-1 downto j*define_word_size) < temp_min then
			temp_min := to_integer(unsigned(s((j+1)*define_word_size-1 downto j*define_word_size)));
		end if;	
		
	end loop;
  
	if temp_max <= temp_min then
		return 1;
	else
		vpp := temp_max - temp_min; 
		return vpp;
	end if;
end function get_vpp;
--//
--function get_vpp_ratio(s1,s2 : std_logic_vector) return std_logic is
--	variable vpp1 : integer := 1;
--	variable vpp2 : integer := 1;
--	variable ratio : integer:= 1;
--begin
--	vpp1 := get_vpp(s1);
--	vpp2 := get_vpp(s2);
--	
--	if vpp1 = 1 or vpp2 = 1 then
--		return '0';
--	end if;
--	
--	ratio := vpp2-vpp1;
--	
--	if ratio <= delay_sum_ratio then
--		return '0';
--	else
--		return '1';
--	end if;
--end function get_vpp_ratio;
--//
function get_vpp_ratio(vpp1,vpp2,cut : integer) return std_logic is
	variable ratio : integer:= 1;
begin

	if vpp1 = 1 or vpp2 = 1 then
		return '0';
	end if;
	
	ratio := vpp2-vpp1;
	
	if ratio <= cut then
		return '0';
	else
		return '1';
	end if;
end function get_vpp_ratio;
begin
--------------------------------------------
xSAT_VETO_SELECT : signal_sync
port map(
	clkA				=> clk_iface_i,
	clkB				=> clk_i,
	SignalIn_clkA	=> reg_i(95)(0), 
	SignalOut_clkB	=> internal_sat_veto_en);
--//not yet implemented:
xCW_VETO_SELECT : signal_sync
port map(
	clkA				=> clk_iface_i,
	clkB				=> clk_i,
	SignalIn_clkA	=> reg_i(95)(1), 
	SignalOut_clkB	=> internal_cw_veto_en);
--//
VetoWidth	:	 for i in 0 to 7 generate	
	xVETO_WIDTH : signal_sync
	port map(
		clkA				=> clk_iface_i,
		clkB				=> clk_i,
		SignalIn_clkA	=> reg_i(95)(8+i), 
		SignalOut_clkB	=> internal_veto_pulse_width(i));
end generate;
--//
SatCut	:	 for i in 0 to 7 generate	
	xSAT_CUT : signal_sync
	port map(
		clkA				=> clk_iface_i,
		clkB				=> clk_i,
		SignalIn_clkA	=> reg_i(96)(i), 
		SignalOut_clkB	=> sat_veto_cut_value(i));
end generate;
CWCut	:	 for i in 0 to 7 generate	
	xCW_CUT : signal_sync
	port map(
		clkA				=> clk_iface_i,
		clkB				=> clk_i,
		SignalIn_clkA	=> reg_i(96)(i+8), 
		SignalOut_clkB	=> cw_veto_cut_value(i));
end generate;
--------------------------------------------
--------------//
proc_buffer_data : process(rst_i, clk_i, data_i)
begin
	--//loop over trigger channels
	for i in 0 to 3 loop
		
		if rst_i = '1' then
		
			buf_data_0(i)<= (others=>'0');
			buf_data_1(i)<= (others=>'0');
			buf_data_2(i)<= (others=>'0');
			buf_data_3(i)<= (others=>'0');		
			buf_data_4(i)<= (others=>'0');		
			buf_data_5(i)<= (others=>'0');		

			dat(i) <= (others=>'0');
			
		elsif rising_edge(clk_i) then
			--//buffer data
			dat(i) <= buf_data_0(i) & buf_data_1(i) & buf_data_2(i) & buf_data_3(i) & buf_data_4(i) & buf_data_5(i);
		
			buf_data_5(i) <= buf_data_4(i);
			buf_data_4(i) <= buf_data_3(i);
			buf_data_3(i) <= buf_data_2(i);
			buf_data_2(i) <= buf_data_1(i);
			buf_data_1(i) <= buf_data_0(i);
			buf_data_0(i) <= data_i(i);

		end if;
	end loop;
end process;

--------------//
--for pulses exhibiting saturation over duration defined by 'clock_cycles_saturated'. 
--this process generates a per-trigger-channel pulse if veto conditions are passed
proc_pulse_saturation_veto : process(rst_i, clk_i, dat, internal_sat_veto_en, internal_sat_veto_reg, sat_veto_cut_value)
begin
	--//loop over trigger channels
	for ch in 0 to 3 loop
		
		if rst_i = '1' then
			
			internal_sat_veto(ch) 		<= '0';           --//the per-channel veto
			internal_sat_veto_reg(ch)	<= (others=>'0');  --//veto register, to determine if signal is saturated for a number of clock cycles 
			internal_sat_veto_tmp(ch) 	<= (others=>'0'); --//this vector checks each data word in a single clock cycle
			
		elsif rising_edge(clk_i) and internal_sat_veto_en = '0'  then

			internal_sat_veto(ch) 		<= '0';
			internal_sat_veto_reg(ch)	<= (others=>'0');
			internal_sat_veto_tmp(ch) 	<= (others=>'0');
			
		elsif rising_edge(clk_i) and internal_sat_veto_en = '1'  then

			--//check saturation condition. Veto if satisfied.
			if (internal_sat_veto_reg(ch)(clock_cycles_saturated+1) = '1' or internal_sat_veto_reg(ch)(clock_cycles_saturated+2) = '1') and 
				(internal_sat_veto_reg(ch)(1) = '1' or internal_sat_veto_reg(ch)(0) = '1') then
				internal_sat_veto(ch) <= '1';
			else
				internal_sat_veto(ch) <= '0';
			end if;

			--//set 0th bit in reg to '1' if saturated, '0' if not. Increment register
			internal_sat_veto_reg(ch) 	<= internal_sat_veto_reg(ch)(5 downto 0) & vector_or(internal_sat_veto_tmp(ch));
				
			--//check each data word for saturation in the parallel data. Saturation simply defined if value exceeds 126 ADC counts
			for j in 0 to 2*define_serdes_factor-1 loop
				if dat(ch)((j+1)*define_word_size+slice_base-1 downto j*define_word_size+slice_base) >= sat_veto_cut_value then
						internal_sat_veto_tmp(ch)(j) <= '1';
				else
						internal_sat_veto_tmp(ch)(j) <= '0';
				end if;
			end loop;

		end if;
	end loop;
end process;

--------------//
--for pulses with significant low-band CW, a delay-and-sum method is employed as a veto
--A trigger channel pulse is delayed (by ~64 ns) and added to itself. A significant boost in in amplitude suggests pulse should be vetoed
proc_pulse_cw_veto : process(rst_i, clk_i, dat, internal_cw_veto_en)
begin
	--//loop over trigger channels
	for ch in 0 to 3 loop
		
		if rst_i = '1' then
			
			internal_cw_veto(ch) 					<= '0';           --//the per-channel veto
			internal_delay_and_sum_waveform(ch) <= (others=>'0');
			internal_normal_waveform(ch)			<= (others=>'0');
			internal_cw_veto_reg(ch)				<= (others=>'0');
			internal_vpp_wfm(ch)						<= 0;
			internal_vpp_summed_wfm(ch)			<= 0;
			
			--//add 'sideswipe' veto here too:
			internal_sideswipe_veto_reg(ch)		<= (others=>'0');
			internal_sideswipe_veto(ch)			<= '0';
			
		elsif rising_edge(clk_i) and internal_cw_veto_en = '0'  then

			internal_cw_veto(ch) 					<= '0';
			internal_delay_and_sum_waveform(ch) <= (others=>'0');
			internal_normal_waveform(ch)			<= (others=>'0');
			internal_cw_veto_reg(ch)				<= (others=>'0');
			internal_vpp_wfm(ch)						<= 0;
			internal_vpp_summed_wfm(ch)			<= 0;

			--//add 'sideswipe' veto here too:
			internal_sideswipe_veto_reg(ch)		<= (others=>'0');
			internal_sideswipe_veto(ch)			<= '0';
			
		elsif rising_edge(clk_i) and internal_cw_veto_en = '1'  then
			----------------------------------------------------------------------------------------------		
			--//add 'sideswipe' veto here too:
			
			internal_sideswipe_veto(ch) <= internal_sideswipe_veto_reg(ch)(clock_cycles_cw) and internal_sideswipe_veto_reg(ch)(clock_cycles_cw+1);	
			
			case ch is
				when 0 =>
					internal_sideswipe_veto_reg(ch) <= internal_sideswipe_veto_reg(ch)(5 downto 0) & 
																		get_vpp_ratio(internal_vpp_wfm(3), internal_vpp_wfm(0), 
																		sideswipe_cut);
				when 1 =>
					internal_sideswipe_veto_reg(ch) <= internal_sideswipe_veto_reg(ch)(5 downto 0) & 
																		get_vpp_ratio(internal_vpp_wfm(0), internal_vpp_wfm(3), 
																		sideswipe_cut);
				when 2 =>
					internal_sideswipe_veto_reg(ch) <= internal_sideswipe_veto_reg(ch)(5 downto 0) & 
																		get_vpp_ratio(internal_vpp_wfm(0), internal_vpp_wfm(2), 
																		sideswipe_cut);
				when 3 =>
					internal_sideswipe_veto_reg(ch) <= internal_sideswipe_veto_reg(ch)(5 downto 0) & 
																		get_vpp_ratio(internal_vpp_wfm(2), internal_vpp_wfm(0), 
																		sideswipe_cut);
			end case;
			----------------------------------------------------------------------------------------------
			
			----------------------------------------------------------------------------------------------
			--//require 2 adjacent flagged time bins to low-band veto:
			--internal_cw_veto(ch) <= internal_cw_veto_reg(ch)(clock_cycles_cw+1) and internal_sat_veto_reg(ch)(clock_cycles_cw);
			--//require a single clock cycle:
			internal_cw_veto(ch) <= internal_sat_veto_reg(ch)(clock_cycles_cw);	

			--//use get_vpp_ratio function to add veto to register:
			internal_cw_veto_reg(ch) <= internal_cw_veto_reg(ch)(5 downto 0) & get_vpp_ratio(internal_vpp_wfm(ch), internal_vpp_summed_wfm(ch), 
																												to_integer(unsigned(cw_veto_cut_value)));
		
			--//get vpp values:
			internal_vpp_wfm(ch)						<= get_vpp(internal_normal_waveform(ch));
			internal_vpp_summed_wfm(ch)			<= get_vpp(internal_delay_and_sum_waveform(ch));
		
			for j in 0 to 4*define_serdes_factor-1 loop
				internal_normal_waveform(ch)((j+1)*define_word_size-1 downto j*define_word_size) <= 
					std_logic_vector(resize(unsigned(dat(ch)(j * define_word_size+slice_base + 7 - 1 downto j * define_word_size+slice_base )),
					define_word_size));	
					
				--//add delayed copy of trigger channel to itself
				internal_delay_and_sum_waveform(ch)((j+1)*define_word_size-1 downto j*define_word_size) <=
					std_logic_vector(resize(unsigned(dat(ch)(j * define_word_size+slice_base + 7 - 1 downto j * define_word_size+slice_base )),
					define_word_size)) +
					std_logic_vector(resize(unsigned(dat(ch)((j - sample_delay) * define_word_size+slice_base + 7 - 1 downto (j - sample_delay) * define_word_size+slice_base )),
					define_word_size));

			end loop;
				
		end if;
	end loop;
end process;

--------------//
--this next process generates a global veto, and pulse stretches
proc_make_veto_pulse : process(rst_i, clk_i, internal_sat_veto, internal_cw_veto)
begin
	if rst_i = '1' then

		veto <= '0';
		internal_veto_pulse_width_counter <= (others=>'0');
		veto_pulse_state <= idle_st;
	
	elsif rising_edge(clk_i) then
	
		case veto_pulse_state is
		
			when idle_st =>
				veto <= '0';
				internal_veto_pulse_width_counter <= (others=>'0');

				--//saturation veto, for now this is simply an OR of all trigger chans:
				if internal_sat_veto(0) = '1' or internal_sat_veto(1) = '1' or 
					internal_sat_veto(2) = '1' or internal_sat_veto(3) = '1' then
					
					veto_pulse_state <= pulse_st;
				
				--//cw veto, for now this is simply an OR of all trigger chans:
				elsif internal_cw_veto(0) = '1' or internal_cw_veto(1) = '1' or 
						internal_cw_veto(2) = '1' or internal_cw_veto(3) = '1' then
									
					veto_pulse_state <= pulse_st;
				
				--//sideswipe veto, for now this is simply an OR of all tests:
				elsif internal_sideswipe_veto(0) = '1' or internal_sideswipe_veto(1) = '1' or 
						internal_sideswipe_veto(2) = '1' or internal_sideswipe_veto(3) = '1' then
									
					veto_pulse_state <= pulse_st;

				else
					
					veto_pulse_state <= idle_st;
					
				end if;
					
			when pulse_st =>
				veto <= '1';
				internal_veto_pulse_width_counter <= internal_veto_pulse_width_counter + 1;
				
				if internal_veto_pulse_width_counter >= internal_veto_pulse_width then
					veto_pulse_state <= idle_st;
				else
					veto_pulse_state <= pulse_st;
				end if;
		end case;
		
	end if;
end process;


--//assign veto
veto_o <= veto;

end rtl;