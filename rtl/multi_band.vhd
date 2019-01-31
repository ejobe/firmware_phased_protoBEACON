---------------------------------------------------------------------------------
-- Univ. of Chicago  
--    --KICP--
--
-- PROJECT:      phased-array trigger board
-- FILE:         multi_band.vhd
-- AUTHOR:       e.oberla
-- EMAIL         ejo@uchicago.edu
-- DATE:         10/2018, 
--
-- DESCRIPTION:  multi-band FIR for proto-beacon
--               
---------------------------------------------------------------------------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

use work.defs.all;

entity multi_band is
	port(
		rst_i			:	in		std_logic;
		clk_i			: 	in		std_logic;
		clk_iface_i	:	in		std_logic;
			
		reg_i			: 	in		register_array_type;
		data_i		:	in	   halfpol_data_type;
		
		veto_o		:	out	std_logic;
		gate_o		:	out  	std_logic);
		
end multi_band;

architecture rtl of multi_band is

constant filter_taps : integer :=  17;
constant filter_coeff_size : integer :=  5; 
constant filter_result_length : integer := filter_coeff_size + define_adc_resolution + 4; --extra 3 due to summing up filter_taps

-----------------------------------------------
----filter via std_logic:
-----------------------------------------------
--type filter_kernel_type is array (filter_taps-1 downto 0) of std_logic_vector(filter_coeff_size-1 downto 0);
--constant kernel : filter_kernel_type := ('0' & x"1", '0' & x"6", '0' & x"F", '1' & x"4", '0' & x"F", '0' & x"6", '0' & x"1");
--constant kernel : filter_kernel_type := (0' & x"1", '0' & x"6", '0' & x"F", '1' & x"4", '0' & x"F", '0' & x"6", '0' & x"1");

---------------------------------------------
--filter via conversion to integer:
---------------------------------------------
type filter_kernel_type is array (filter_taps-1 downto 0) of integer range -31 to 32;
constant kernel_lo : filter_kernel_type := (-10,-12,-11, -8, -3,  4, 10, 14, 16, 14, 10,  4, -3, -8,-11, 12,-10);
constant kernel_hi : filter_kernel_type := (  4,  9,  8, -2,-14,-17, -5, 12, 20, 12, -5,-17,-14, -2,  8,  9,  4);


constant integral_bit_shift : integer := 6; 
--signal integral_bit_shift : integer := 5; 

type internal_buf_data_type is array (3 downto 0) of std_logic_vector(2*pdat_size-1 downto 0);
signal dat : internal_buf_data_type;
signal buf_data_0 		: 	halfpol_data_type;
signal buf_data_1 		: 	halfpol_data_type;

-----------------------------------------------
----filter via std_logic:
-----------------------------------------------
--type filter_result_type is array (3 downto 0, 2*define_serdes_factor-1 downto 0) of std_logic_vector(filter_result_length-1 downto 0);
--signal filter_result : filter_result_type;  -- <---filter via std_logic
--signal buf_filter_result : full_data_type;

---------------------------------------------
--filter via conversion to integer:
---------------------------------------------
type filter_result_int_type is array (7 downto 0, 2*define_serdes_factor-1 downto 0) of integer range 0 to 8192;
signal filter_result : filter_result_int_type;
