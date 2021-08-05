---------------------------------------------------------------------------------------------------------
-- Univ. of Chicago  
--    --KICP--
--
-- PROJECT:      phased-array trigger board
-- FILE:         new_beacon_beamforming.vhd
-- AUTHOR:       e.oberla
-- EMAIL         ejo@uchicago.edu
-- DATE:         11/19/2018
--
-- DESCRIPTION:  
-----////////////////////////////////////////////////////////////////////////////////////////////////////
---------------------------------------------------------------------------------------------------------

----------- ------------------
-----------------------------

library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

use work.defs.all;

entity new_beacon_beamforming is
	generic(
		ENABLE_BEAMFORMING : std_logic := '1'); --//compile-time flag
	port(
		rst_i			:	in		std_logic;
		clk_i			: 	in		std_logic;
		clk_iface_i	:	in		std_logic;
			
		reg_i			: 	in		register_array_type;
		data_i		:	in	   full_data_type;
		
		veto_for_mon_o : 	out	std_logic;
		veto_o			:	out	std_logic;
		beams_o			:	out	array_of_beams_type;   
		sum_pow_o		:	out	sum_power_type);
		
end new_beacon_beamforming;
-----------------------------------------------------------
architecture rtl of new_beacon_beamforming is

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
signal buf_data_9 		: 	full_data_type;
signal buf_data_10 		: 	full_data_type;
signal buf_data_11 		: 	full_data_type;
signal buf_data_12 		: 	full_data_type;

signal pol_data			:	halfpol_data_type;

signal channel_mask		: full_data_type; --//added 5/8/2018
signal channel_mask_meta: full_data_type; --//added 5/8/2018

--//buffer the data 5x every clock cycle --> allows beam-forming +/- the central buffer
type internal_buf_data_type is array (7 downto 0) of std_logic_vector(13*pdat_size-1 downto 0);
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
constant : internal_beam_number     : integer := 51;

--//------------------------------------------------------------------

type beam_delays_type is array (internal_beam_number-1 downto 0) of integer range -70 to 70;

--COPIED in from Dan S. after adjusting delays around a zero-delayed mean. 
-- NOTE that a (-) delay here means the signal should be advanced. A positive
-- value indicates the signal should be delayed
constant ant_0_delays : beam_delays_type :=
(-23,-23,-23,-23,-23,-23,-23,-19,-15,-12,-10,-10,-6,-19,-12,-7,-2,2,4,7,12,14,-8,-1,6,12,
17,21,24,30,37,43,46,48,0,64,8,13,17,22,25,29,32,34,39,46,51,56,60,63,65);

constant ant_1_delays : beam_delays_type :=
(-13,-16,-19,-21,-21,-21,-21,-23,-23,-23,-23,-23,-17,-23,-23,-23,-23,-23,-23,-20,-14,-10,
-23,-23,-23,-23,-23,-23,-23,-18,-12,-5,-1,4,-23,10,-23,-23,-23,-23,-23,-23,-23,-23,-20,-14,
-10,-5,-1,3,7);

constant ant_2_delays : beam_delays_type :=
(-12,-4,-8,-14,-19,-23,8,4,-1,-7,-14,-21,-23,22,20,15,9,3,-5,-12,-15,-19,46,43,40,35,
29,21,13,8,5,3,-2,-7,55,4,54,52,49,46,41,37,31,25,23,22,20,18,15,13,9);

constant ant_3_delays : beam_delays_type :=
(-1,3,-3,-8,-12,-14,5,-1,-6,-12,-17,-22,-21,10,6,-1,-7,-13,-19,-23,-23,-23,18,13,7,1,-6,
-13,-20,-23,-23,-23,-23,-23,17,-23,11,8,3,-1,-7,-11,-16,-22,-23,-23,-23,-23,-23,-23,-23);


--//coh. sums 
type coh_sum_type is array (internal_beam_number,
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
			buf_data_9(i)<= (others=>'0');
			buf_data_10(i)<= (others=>'0');
			buf_data_11(i)<= (others=>'0');
			buf_data_12(i)<= (others=>'0');
			
			data_pipe_railed(i) <= (others=>'0');	
			 
			channel_mask(i)<= (others=>'1'); --//added 5/8/2018 (moved from adc_controller.vhd)
			channel_mask_meta(i) <= (others=>'1');
						
			dat(i) <= (others=>'0');
			
		elsif rising_edge(clk_i) then
		
			channel_mask(i) <= channel_mask_meta(i);
			channel_mask_meta(i) <= (others=> reg_i(48)(i)); --//added 5/8/2018 (moved from adc_controller.vhd)
															--//note for beacon, this is a pol_data mask
		
		
			-----------
			--the data buffer:
			-----------
			--note that MSByte is earliest data, LSByte is the latest data 
			-- (so, to delay a signal, pick bytes from the lower side of the vector, i.e. subtract)
			dat(i) <= 	buf_data_0(i) & buf_data_1(i) & buf_data_2(i) & buf_data_3(i) & buf_data_4(i) & 
							buf_data_5(i) & buf_data_6(i) & buf_data_7(i) & buf_data_8(i) & buf_data_9(i) &	
							buf_data_10(i) & buf_data_11(i) & buf_data_12(i);
			
			buf_data_12(i) <= buf_data_11(i);
			buf_data_11(i) <= buf_data_10(i);
			buf_data_10(i) <= buf_data_9(i);
			buf_data_9(i) <= buf_data_8(i);
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
--//-------------------------
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
			for beam_num in 0 to  internal_beam_number-1 loop
				--//loop over vertical beam delays
			
					coh_sum(beam_num, i) <= (others=>'0');

			end loop;	
				
		elsif rising_edge(clk_i) then
			
			--/////////////////////////////////////
			--// Delay-and-Sum here:
			--///////////////////////////////////
			--// resize data chunks from ADC before adding in order to get proper sign extension
			--///////////////////////////////////////////////////////////////////////////////////
			--
			--//loop over beam delays
			for beam_num in 0 to  ant_0_delays'length-1 loop

					
				--//pipeline + flatten coherent sums to internal beams vector
				internal_beams(hz * beam_delays_vert'length + vt)((i+1)*define_word_size-1 downto i*define_word_size) <= coh_sum(beam_num, i);		
					
				--//protoBEACON: 8 antennas - 4 of each pol so Coh. sum made from 4 adc channels
								--
				coh_sum (beam_num, i) <= 
					std_logic_vector(resize(signed(dat(0)(	(i+(-ant_0_delays(beam_num) )) * 
																		define_word_size+slice_hi-1 downto 
																		(i+(-ant_0_delays(beam_num) )) *
																		define_word_size+slice_lo )),define_beam_bits)) +
			
					std_logic_vector(resize(signed(dat(1)(	(i+(-ant_1_delays(beam_num) )) * 
																		define_word_size+slice_hi-1 downto 
																		(i+(-ant_1_delays(beam_num) )) *
																		define_word_size+slice_lo )),define_beam_bits)) +
					--
					std_logic_vector(resize(signed(dat(2)(	(i+(-ant_2_delays(beam_num) )) * 
																		define_word_size+slice_hi-1 downto 
																		(i+(-ant_2_delays(beam_num) )) *
																		define_word_size+slice_lo )),define_beam_bits)) +
					--
					std_logic_vector(resize(signed(dat(3)( (i+(-ant_3_delays(beam_num) )) * 
																		define_word_size+slice_hi-1 downto 
																		(i+(-ant_3_delays(beam_num) )) *
																		define_word_size+slice_lo )),define_beam_bits));
				--
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

xTRIGGER_VETO : entity work.trigger_veto
	port map(
		rst_i			=> rst_i,
		clk_i			=> clk_i,
		clk_iface_i	=> clk_iface_i,
		reg_i			=> reg_i,
		data_i		=> pol_data,
		veto_mon_o	=> veto_for_mon_o,
		veto_o		=> veto_o);
		
end rtl;
		