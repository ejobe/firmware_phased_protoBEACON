---------------------------------------------------------------------------------------------------------
-- Univ. of Chicago  
--    --KICP--
--
-- PROJECT:      phased-array trigger board
-- FILE:         beacon_beamforming.vhd
-- AUTHOR:       e.oberla
-- EMAIL         ejo@uchicago.edu
-- DATE:         7/2018
--
-- DESCRIPTION:  
-----////////////////////////////////////////////////////////////////////////////////////////////////////
---------------------------------------------------------------------------------------------------------

----------- ------------------
-- nominal prototype detector layout: on a 20deg downward slope
--
-- TOP OF SLOPE
--
--          <---------->
--               5 m
--
--          +          +             ^
--                                   |
--                                   | 5 m
--                                   |
--                                   |  
--   +                         +     ^
--
-- BOTTOM OF SLOPE
--
--  30-80 MHz antennas '+'
--
--  ===> 2D beamforming, in both polarizations. Select polarization on which to trigger
--
--
--  general flow of this module:
--     1) select hpol or vpol inputs
--     2) rail data at 5 bits
--     3) pipeline data
--     4) beamform
--     5) calculate power on beams
-----------------------------

library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

use work.defs.all;

entity beacon_beamforming is
	generic(
		ENABLE_BEAMFORMING : std_logic := '1'); --//compile-time flag
	port(
		rst_i			:	in		std_logic;
		clk_i			: 	in		std_logic;
		clk_iface_i	:	in		std_logic;
			
		reg_i			: 	in		register_array_type;
		data_i		:	in	   full_data_type;
		
		beams_o		:	out	array_of_beams_type;   
		sum_pow_o	:	out	sum_power_type);
		
end beacon_beamforming;

architecture rtl of beacon_beamforming is

signal data_pipe_railed	:  full_data_type;  --//option to rail the 7-bit data at 5 bits in order to fit in beamformer
signal buf_data_0 		: 	full_data_type;
signal buf_data_1 		: 	full_data_type;
signal buf_data_2 		: 	full_data_type;
signal buf_data_3 		: 	full_data_type;
signal buf_data_4 		: 	full_data_type;
signal buf_data_5 		: 	full_data_type;
signal buf_data_6 		: 	full_data_type;
signal buf_data_7 		: 	full_data_type;
signal buf_data_8 		: 	full_data_type;
signal pol_data			:	full_data_type;

signal channel_mask		: full_data_type; --//added 5/8/2018
signal channel_mask_meta: full_data_type; --//added 5/8/2018

--//buffer the data 5x every clock cycle --> allows beam-forming +/- the central buffer
type internal_buf_data_type is array (7 downto 0) of std_logic_vector(9*pdat_size-1 downto 0);
signal dat : internal_buf_data_type;

--//starting points for slicing multi-clock cycle pipelined 'dat' to form coh. sums
constant slice_base : integer := 4*pdat_size; 
constant slice_lo   : integer := define_wave2beam_lo_bit+slice_base;
constant slice_hi   : integer := define_wave2beam_hi_bit+slice_base;

signal internal_beams 		: array_of_beams_type := (others=> (others=>'0'));
signal internal_beams_pipe	: array_of_beams_type := (others=> (others=>'0'));

signal internal_summed_power	:	sum_power_type;

signal internal_beam_enable 			: std_logic := '0';
signal internal_pol_select 			: std_logic := '0';

--//------------------------------------------------------------------
--//define beams here, using 'codes' and 'delays'
--//
--//2d beamforming:
--//horz delay steps = 5
--//vert delay steps = 4
--//total beams = 20

type beam_delays_horz_type is array (4 downto 0) of integer range -8 to 8;
constant beam_delays_horz : beam_delays_horz_type := (-6, -3, 0, 3, 6); 

type beam_delays_vert_type is array (3 downto 0) of integer range -8 to 8;
constant beam_delays_vert : beam_delays_vert_type := (-6, -3, 0, 3);

--//beam codes - one per antenna, depends on array geometry
type beam_codes_type is array (3 downto 0) of integer range -8 to 8;
constant beam_codes_horz : beam_codes_type := (-2,-1,0,1); --//equal horizontal spacing
constant beam_codes_vert : beam_codes_type := (-1,0,0,-1); --//two rows vertically

--//coh. sums 
type coh_sum_type is array (beam_delays_horz'length downto 0,
									beam_delays_vert'length downto 0,
									2*define_serdes_factor*define_word_size-1 downto 0)
									of std_logic_vector(define_beam_bits-1 downto 0);
signal coh_sum : coh_sum_type;

--//------------------------------------------------------------------
--//
component signal_sync is
port
	(clkA			: in	std_logic;
   clkB			: in	std_logic;
   SignalIn_clkA	: in	std_logic;
   SignalOut_clkB	: out	std_logic);
end component;
--//
begin

xBEAMENABLE : signal_sync
port map(
	clkA				=> clk_iface_i,
	clkB				=> clk_i,
	SignalIn_clkA	=> reg_i(82)(8), 
	SignalOut_clkB	=> internal_beam_enable);
--------------------------------------------
xPOLSELECT : signal_sync
port map(
	clkA				=> clk_iface_i,
	clkB				=> clk_i,
	SignalIn_clkA	=> reg_i(79)(0), 
	SignalOut_clkB	=> internal_pol_select);
--------------------------------------------
-----//		
--//assign either hpol or vpol channels to beamformer
proc_assign_data : process(clk_i, data_i)
begin
	if rising_edge(clk_i) then
		case internal_pol_select is 
			--//hpol
			when '0' =>
				pol_data(0) <= data_i(0);
				pol_data(1) <= data_i(2);
				pol_data(2) <= data_i(4);
				pol_data(3) <= data_i(6);
			--//vpol
			when '1' =>
				pol_data(0) <= data_i(1);
				pol_data(1) <= data_i(3);
				pol_data(2) <= data_i(5);
				pol_data(3) <= data_i(7);
		end case;
	end if;
end process;
--------------//
proc_buffer_data : process(rst_i, clk_i, pol_data)
begin
	--//loop over trigger channels
	for i in 0 to 3 loop
		
		if rst_i = '1' or ENABLE_BEAMFORMING = '0' then
			
			buf_data_0(i)<= (others=>'0');
			buf_data_1(i)<= (others=>'0');
			buf_data_2(i)<= (others=>'0');
			buf_data_3(i)<= (others=>'0');		
			buf_data_4(i)<= (others=>'0');
			buf_data_5(i)<= (others=>'0');
			buf_data_6(i)<= (others=>'0');
			buf_data_7(i)<= (others=>'0');
			buf_data_8(i)<= (others=>'0');
			data_pipe_railed(i) <= (others=>'0');	
			 
			channel_mask(i)<= (others=>'1'); --//added 5/8/2018 (moved from adc_controller.vhd)
			channel_mask_meta(i) <= (others=>'1');
						
			dat(i) <= (others=>'0');
			
		elsif rising_edge(clk_i) then
		
			channel_mask(i) <= channel_mask_meta(i);
			channel_mask_meta(i) <= (others=> reg_i(48)(i)); --//added 5/8/2018 (moved from adc_controller.vhd)
															--//note for beacon, this is a pol_data mask
		
		
			dat(i) <= 	buf_data_0(i) & buf_data_1(i) & buf_data_2(i) & buf_data_3(i) & buf_data_4(i) & 
							buf_data_5(i) & buf_data_6(i) & buf_data_7(i) & buf_data_8(i);	
			
			buf_data_8(i) <= buf_data_7(i);
			buf_data_7(i) <= buf_data_6(i);
			buf_data_6(i) <= buf_data_5(i);
			buf_data_5(i) <= buf_data_4(i);
			buf_data_4(i) <= buf_data_3(i);
			buf_data_3(i) <= buf_data_2(i);
			buf_data_2(i) <= buf_data_1(i);
			buf_data_1(i) <= buf_data_0(i);
			buf_data_0(i) <= data_pipe_railed(i) and channel_mask(i);
			
			---rail wavefroms if exceed +15/-16 counts from mid-scale 64
			for j in 0 to 2*define_serdes_factor-1 loop
				if pol_data(i)((j+1)*define_word_size-1 downto j*define_word_size) < 48 then
					data_pipe_railed(i)((j+1)*define_word_size-1 downto j*define_word_size) <= '0' & "0110000";
				elsif pol_data(i)((j+1)*define_word_size-1 downto j*define_word_size) > 79 then
					data_pipe_railed(i)((j+1)*define_word_size-1 downto j*define_word_size) <= '0' & "1001111";
				else
					data_pipe_railed(i)((j+1)*define_word_size-1 downto j*define_word_size) <= pol_data(i)((j+1)*define_word_size-1 downto j*define_word_size);
				end if;
			end loop;			
		end if;
	end loop;
end process;
--//
--//pipeline beams to output
proc_pipe_beams : process(rst_i, clk_i, internal_beam_enable)
begin
	for i in 0 to define_num_beams-1 loop
		if rst_i = '1' or ENABLE_BEAMFORMING = '0' then
			internal_beams_pipe(i) <= (others=>'0');
			beams_o(i) <= (others=>'0');

			--//output beam power
			sum_pow_o(i) <= (others=>'0');
			
		elsif rising_edge(clk_i) then
			beams_o(i) <= internal_beams_pipe(i);
			
			--------------------------------------------------------
			if internal_beam_enable = '1' then
				internal_beams_pipe(i) <= internal_beams(i);
			else
				internal_beams_pipe(i) <= (others=>'0');
			end if; 
			
			--//output beam power. 
			sum_pow_o(i) <= std_logic_vector(unsigned(internal_summed_power(i)));
		end if;
	end loop;
end process;
--/////////////////////////////////////--
--/////////////////////////////////////--
proc_delay_and_sum : process(rst_i, clk_i)
begin
	--//loop over individual 8-bit ADC words
	for i in 0 to 2*define_serdes_factor-1 loop
	
		if rst_i = '1' or ENABLE_BEAMFORMING = '0' then
		
			for k in 0 to define_num_beams-1 loop
				internal_beams(k)((i+1)*define_beam_bits-1 downto i*define_beam_bits) <= (others=>'0');
			end loop;
			
			--//loop over horizontal beam delays
			for hz in 0 to  beam_delays_horz'length-1 loop
				--//loop over vertical beam delays
				for vt in 0 to beam_delays_vert'length-1 loop
					
					coh_sum(hz, vt, i) <= (others=>'0');
			
				end loop;
			end loop;	
				
		elsif rising_edge(clk_i) then
			
			--/////////////////////////////////////
			--// Delay-and-Sum here:
			--///////////////////////////////////
			--// resize data chunks from ADC before adding in order to get proper sign extension
			--///////////////////////////////////////////////////////////////////////////////////
			--
			--//loop over horizontal beam delays
			for hz in 0 to  beam_delays_horz'length-1 loop
				--//loop over vertical beam delays
				for vt in 0 to beam_delays_vert'length-1 loop
					
					--//pipeline + flatten coherent sums to internal beams vector
					internal_beams(hz * beam_delays_vert'length + vt)((i+1)*define_word_size-1 downto i*define_word_size) <= coh_sum(hz, vt, i);		
						
					--//protoBEACON: 8 antennas - 4 of each pol so Coh. sum made from 4 adc channels
					--//
					--// calculate antenna shift as such:
					--//     horz_code (ant) * horz_delay (beam) + vert_code (ant) * vert_delay (beam)
					coh_sum (hz, vt, i) <= 
						std_logic_vector(resize(signed(dat(0)(	(i+(beam_delays_horz(hz)*beam_codes_horz(0) + beam_delays_vert(vt)*beam_codes_vert(0))) * 
																			define_word_size+slice_hi-1 downto 
																			(i+(beam_delays_horz(hz)*beam_codes_horz(0) + beam_delays_vert(vt)*beam_codes_vert(0))) *
																			define_word_size+slice_lo )),define_beam_bits)) +
				
						std_logic_vector(resize(signed(dat(1)(	(i+(beam_delays_horz(hz)*beam_codes_horz(1) + beam_delays_vert(vt)*beam_codes_vert(1))) * 
																			define_word_size+slice_hi-1 downto 
																			(i+(beam_delays_horz(hz)*beam_codes_horz(1) + beam_delays_vert(vt)*beam_codes_vert(1))) *
																			define_word_size+slice_lo )),define_beam_bits)) +
						--
						std_logic_vector(resize(signed(dat(2)(	(i+(beam_delays_horz(hz)*beam_codes_horz(2) + beam_delays_vert(vt)*beam_codes_vert(2))) * 
																			define_word_size+slice_hi-1 downto 
																			(i+(beam_delays_horz(hz)*beam_codes_horz(2) + beam_delays_vert(vt)*beam_codes_vert(2))) *
																			define_word_size+slice_lo )),define_beam_bits)) +
						--
						std_logic_vector(resize(signed(dat(3)( (i+(beam_delays_horz(hz)*beam_codes_horz(3) + beam_delays_vert(vt)*beam_codes_vert(3))) * 
																			define_word_size+slice_hi-1 downto 
																			(i+(beam_delays_horz(hz)*beam_codes_horz(3) + beam_delays_vert(vt)*beam_codes_vert(3))) *
																			define_word_size+slice_lo )),define_beam_bits));
				end loop;
			end loop;
			
		end if;
	end loop;
end process;

--//calculate power	
xPOWER_SUM : entity work.power_detector
	port map(
		rst_i  	=> rst_i or (not ENABLE_BEAMFORMING),
		clk_i	 	=> clk_i,
		reg_i		=> reg_i,
		beams_i	=> internal_beams_pipe,
		sum_pow_o=> internal_summed_power);							
		
end rtl;