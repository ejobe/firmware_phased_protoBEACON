---------------------------------------------------------------------------------
-- Univ. of Chicago  
--    --KICP--
--
-- PROJECT:      phased-array trigger board
-- FILE:         filter.vhd
-- AUTHOR:       e.oberla
-- EMAIL         ejo@uchicago.edu
-- DATE:         8/2018, 
--
-- DESCRIPTION:  low-pass FIR for proto-beacon
--               
---------------------------------------------------------------------------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

use work.defs.all;

entity filter is
	port(
		rst_i			:	in		std_logic;
		clk_i			: 	in		std_logic;
		clk_iface_i	:	in		std_logic;
			
		reg_i			: 	in		register_array_type;
		data_i		:	in	   full_data_type;
		
		filtered_data_o :	out	full_data_type);
		
end filter;

architecture rtl of filter is

constant filter_taps : integer :=  7;
constant filter_coeff_size : integer :=  5;
constant filter_result_length : integer := filter_coeff_size + define_adc_resolution + 3; --extra 3 due to summing up filter_taps

type filter_kernel_type is array (filter_taps-1 downto 0) of std_logic_vector(filter_coeff_size-1 downto 0);
constant kernel : filter_kernel_type := ('0' & x"1", '0' & x"6", '0' & x"F", '1' & x"4", '0' & x"F", '0' & x"6", '0' & x"1");
--constant kernel : filter_kernel_type := (1, 6, 15, 20, 15, 6, 1);

constant integral_bit_shift : integer := 5; --integral of kernal is 64
--signal integral_bit_shift : integer := 5; 

type internal_buf_data_type is array (7 downto 0) of std_logic_vector(2*pdat_size-1 downto 0);
signal dat : internal_buf_data_type;
signal buf_data_0 		: 	full_data_type;
signal buf_data_1 		: 	full_data_type;

type filter_result_type is array (7 downto 0, 2*define_serdes_factor-1 downto 0) of std_logic_vector(filter_result_length-1 downto 0);
signal filter_result : filter_result_type; 
signal buf_filter_result : full_data_type;

--//
component signal_sync is
port
	(clkA			: in	std_logic;
   clkB			: in	std_logic;
   SignalIn_clkA	: in	std_logic;
   SignalOut_clkB	: out	std_logic);
end component;
--//
signal internal_filter_enable : std_logic := '0';
--//
begin
--//
--------------------------------------------
xFILTEN : signal_sync
port map(
	clkA				=> clk_iface_i,
	clkB				=> clk_i,
	SignalIn_clkA	=> reg_i(90)(0), 
	SignalOut_clkB	=> internal_filter_enable);
--------------------------------------------
proc_filter : process(rst_i, clk_i)
begin
	for j in 0 to 7 loop

		if rst_i = '1' then
		
			dat(j) <= (others=>'0');
			buf_data_0(j) <= (others=>'0');
			buf_data_1(j) <= (others=>'0');

			for i in 0 to 2*define_serdes_factor-1 loop
			
				filter_result(j,i) <= (others=>'0');
			
			end loop;
			
			--integral_bit_shift <= 5;
	
		elsif rising_edge(clk_i) then

			dat(j) <= buf_data_0(j) & buf_data_1(j);
			
			buf_data_1(j) <= buf_data_0(j);
			buf_data_0(j) <= data_i(j);

			case internal_filter_enable is
				when '0' => filtered_data_o(j) <= buf_data_1(j);
				when '1' => filtered_data_o(j) <= buf_filter_result(j);
			end case;

			--integral_bit_shift <= to_integer(unsigned(reg_i(91)(7 downto 0)));

	
			for i in 0 to 2*define_serdes_factor-1 loop
			
				buf_filter_result(j)((i+1)*define_word_size-1 downto i*define_word_size) <= '0' & filter_result(j,i)(filter_result_length-1) & --//get sign bit 
													filter_result(j,i)(integral_bit_shift+define_adc_resolution-2 downto integral_bit_shift); --//get value
			
				filter_result(j,i) <= 
				std_logic_vector(resize(signed(
						unsigned(dat(j)(pdat_size-2-define_word_size*0 + define_word_size*i downto pdat_size-define_word_size*1 + define_word_size*i )) * 
						unsigned(kernel(0))), filter_result_length)) +
				std_logic_vector(resize(signed(
						unsigned(dat(j)(pdat_size-2-define_word_size*1 + define_word_size*i downto pdat_size-define_word_size*2 + define_word_size*i )) * 
						unsigned(kernel(1))), filter_result_length)) +											 
				std_logic_vector(resize(signed(
						unsigned(dat(j)(pdat_size-2-define_word_size*2 + define_word_size*i downto pdat_size-define_word_size*3 + define_word_size*i )) * 
						unsigned(kernel(2))), filter_result_length)) +
				std_logic_vector(resize(signed(
						unsigned(dat(j)(pdat_size-2-define_word_size*3 + define_word_size*i downto pdat_size-define_word_size*4 + define_word_size*i )) * 
						unsigned(kernel(3))), filter_result_length)) +
				std_logic_vector(resize(signed(
						unsigned(dat(j)(pdat_size-2-define_word_size*4 + define_word_size*i downto pdat_size-define_word_size*5 + define_word_size*i )) * 
						unsigned(kernel(4))), filter_result_length)) +
				std_logic_vector(resize(signed(
						unsigned(dat(j)(pdat_size-2-define_word_size*5 + define_word_size*i downto pdat_size-define_word_size*6 + define_word_size*i )) * 
						unsigned(kernel(5))), filter_result_length)) +	
				std_logic_vector(resize(signed(
						unsigned(dat(j)(pdat_size-2-define_word_size*6 + define_word_size*i downto pdat_size-define_word_size*7 + define_word_size*i )) * 
						unsigned(kernel(6))), filter_result_length));		
						
			end loop;
			
		end if;
	end loop;
end process;

end rtl;