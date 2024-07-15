------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: Top Entity
-- Description:
--      Top Entity
--		Input 	-	i_clock_100: Clock (100MHz)
--		Input 	-	i_reset_btn: Reset ('0': NO Reset, '1': Reset)
--		Input 	-	i_filter_mode: Filter Modes Selector
--						'0': Filter 0 (Pass-Through)
--						'1': Filter 1 (Default: Laplacian)
--      Output	-	o_leds: 16 LEDs
--      Output	-	o_hsync: VGA Horizontal Synchronization
--      Output	-	o_vsync: VGA Vertical Synchronization
--      Output	-	o_vga_red: VGA Red Signal
--      Output	-	o_vga_green: VGA Green Signal
--      Output	-	o_vga_blue: VGA Blue Signal
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY Top is
PORT(
	i_clock_100: IN STD_LOGIC;
	i_reset_btn: IN STD_LOGIC;
	-- Image Filter
	i_filter_mode: IN STD_LOGIC;
	-- LEDs
	o_leds: OUT STD_LOGIC_VECTOR(15 downto 0);
	-- VGA
	o_hsync: OUT STD_LOGIC;
	o_vsync: OUT STD_LOGIC;
    o_vga_red: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_vga_green: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_vga_blue: OUT STD_LOGIC_VECTOR(3 downto 0)
);
END Top;

ARCHITECTURE Behavioral of Top is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------
COMPONENT ClockTickGenerator is
	PORT(
		i_clock_100: IN STD_LOGIC;
		o_clock_tick_1: OUT STD_LOGIC;
		o_clock_tick_25M: OUT STD_LOGIC
	);
END COMPONENT;

COMPONENT Debouncer is
	GENERIC(
		DEBOUNCE_COUNTER_SIZE: INTEGER := 20
	);
    PORT(
        i_clock: IN STD_LOGIC;
        i_input: IN STD_LOGIC;
        o_output: OUT STD_LOGIC
    );
END COMPONENT;

COMPONENT Synchronizer is
	PORT(
		i_domain_clock: IN STD_LOGIC;
		i_input: IN STD_LOGIC;
		o_output: OUT STD_LOGIC
	);
END COMPONENT;

COMPONENT ChainedLed is
	PORT(
		i_clock_100: IN STD_LOGIC;
		i_clock_enable: IN STD_LOGIC;
		i_reset: IN STD_LOGIC;
		o_leds: OUT STD_LOGIC_VECTOR(15 downto 0)
	);
END COMPONENT;

COMPONENT vga_pixel_clock is
	PORT(
		clk_out1: OUT STD_LOGIC;
		clk_in1: IN STD_LOGIC
	);
END COMPONENT;

COMPONENT VGAController is
	PORT(
		i_pixel_clock: IN STD_LOGIC;
		i_reset: IN STD_LOGIC;
		i_image_data_empty: IN STD_LOGIC;
		i_image_data: IN STD_LOGIC_VECTOR(11 downto 0);
		o_next_image_data: OUT STD_LOGIC;
		o_hsync: OUT STD_LOGIC;
		o_vsync: OUT STD_LOGIC;
		o_vga_red: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_vga_green: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_vga_blue: OUT STD_LOGIC_VECTOR(3 downto 0)
	);
END COMPONENT;

COMPONENT ImageFilter is
	PORT(
		i_pixel_clock: IN STD_LOGIC;
		i_filter_mode: IN STD_LOGIC;
		i_image_to_filter_data: IN STD_LOGIC_VECTOR(11 downto 0);
		o_image_to_filter_addr: OUT STD_LOGIC_VECTOR(18 downto 0);
		o_filtered_image_write_enable: OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
		o_filtered_image_addr: OUT STD_LOGIC_VECTOR(18 downto 0);
		o_filtered_image_data: OUT STD_LOGIC_VECTOR(11 downto 0)
	);
END COMPONENT;

COMPONENT fifo_camera IS
	PORT (
		clk : IN STD_LOGIC;
		srst : IN STD_LOGIC;
		din : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
		wr_en : IN STD_LOGIC;
		rd_en : IN STD_LOGIC;
		dout : OUT STD_LOGIC_VECTOR(11 DOWNTO 0);
		full : OUT STD_LOGIC;
		empty : OUT STD_LOGIC
	);
END COMPONENT;

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Clock Tick Generator Outputs
signal clock_tick_out_1: STD_LOGIC := '0';
signal clock_tick_out_25M: STD_LOGIC := '0';

-- Reset BTN Debouncer & Synchronizer
signal debounced_reset: STD_LOGIC := '0';
signal synchronized_reset: STD_LOGIC := '0';

-- Filter Mode SW Debouncer
signal debounced_filter_mode: STD_LOGIC := '0';
signal synchronized_filter_mode: STD_LOGIC := '0';

-- Pixel Clock (148.5 MHz)
signal pixel_clock: STD_LOGIC := '0';

-- VGA Controls
signal read_image_data_empty: STD_LOGIC := '0';
signal read_image_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal read_next_image_data: STD_LOGIC := '0';

-- Camera Image
signal write_next_camera_data: STD_LOGIC := '0';
signal write_camera_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal write_camera_data_full: STD_LOGIC := '0';
signal write_camera_data_addr: UNSIGNED(18 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	--------------------------
	-- Clock Tick Generator --
	--------------------------
	inst_clockTickGenerator : ClockTickGenerator port map (
		i_clock_100 => i_clock_100,
		o_clock_tick_1 => clock_tick_out_1,
		o_clock_tick_25M => clock_tick_out_25M);

	-----------------------------
	-- Debouncer - i_reset_btn --
	-----------------------------
	inst_debouncer_reset_btn : Debouncer generic map (DEBOUNCE_COUNTER_SIZE => 20) port map (
		i_clock => i_clock_100,
		i_input => i_reset_btn,
		o_output => debounced_reset);

	------------------------------------
	-- Synchronized - debounced_reset --
	------------------------------------
	inst_synchronizer_reset : Synchronizer port map (
		i_domain_clock => pixel_clock,
		i_input => debounced_reset,
		o_output => synchronized_reset);

	---------------------------------
	-- Debouncer - i_filter_mode --
	---------------------------------
	inst_debouncer_filter_mode : Debouncer generic map (DEBOUNCE_COUNTER_SIZE => 20) port map (
		i_clock => i_clock_100,
		i_input => i_filter_mode,
		o_output => debounced_filter_mode);

	------------------------------------------
	-- Synchronized - debounced_filter_mode --
	------------------------------------------
	inst_synchronizer_filter_mode : Synchronizer port map (
		i_domain_clock => pixel_clock,
		i_input => debounced_filter_mode,
		o_output => synchronized_filter_mode);

	------------------
	-- Chained LEDs --
	------------------
	inst_chainedLEDs : ChainedLed port map (
		i_clock_100 => i_clock_100,
		i_clock_enable => clock_tick_out_1,
		i_reset => debounced_reset,
		o_leds => o_leds);

	---------------------
	-- VGA Pixel Clock --
	---------------------
	inst_vgaPixelClock : vga_pixel_clock port map (clk_out1 => pixel_clock, clk_in1 => i_clock_100);

	--------------------
	-- VGA Controller --
	--------------------
	inst_vgaController : VGAController port map (
		i_pixel_clock => pixel_clock,
		i_reset => synchronized_reset,
		i_image_data_empty => read_image_data_empty,
		i_image_data => read_image_data,
		o_next_image_data => read_next_image_data,
		o_hsync => o_hsync,
		o_vsync => o_vsync,
		o_vga_red => o_vga_red,
		o_vga_green => o_vga_green,
		o_vga_blue => o_vga_blue);

	------------------
	-- Image Filter --
	------------------
	-- inst_imageFilter : ImageFilter port map (
	-- 	i_pixel_clock => pixel_clock,
	-- 	i_filter_mode => debounced_filter_mode,
	-- 	i_image_to_filter_data => write_camera_data,
	-- 	o_image_to_filter_addr => write_camera_data_addr,
	-- 	o_filtered_image_write_enable => ram_write_enable,
	-- 	o_filtered_image_addr => ram_write_addr,
	-- 	o_filtered_image_data => ram_write_data);

	-------------------
	-- FIFO Camera 0 --
	-------------------
    inst_fifo_camera_0 : fifo_camera port map (
        clk => pixel_clock,
        srst => synchronized_reset,
        din => write_camera_data,
        wr_en => write_next_camera_data,
        rd_en => read_next_image_data,
        dout => read_image_data,
        full => write_camera_data_full,
        empty => read_image_data_empty);

	----------------------
	-- Image Simulation --
	----------------------
	-- First Pixel: Red
	-- Second Pixel: Green
	-- Third Pixel: Blue
	-- First Line: White
	-- Edges: Orange
	-- Middle: Purple
	-- Last Line: Green

	-- Camera Next Image Data
	process(pixel_clock)
	begin
		if rising_edge(pixel_clock) then
	       write_next_camera_data <= not(synchronized_reset) and not(write_camera_data_full);
	    end if;
    end process;
	
	-- Camera Image Address
	process(pixel_clock)
	begin
		if rising_edge(pixel_clock) then
			if (synchronized_reset = '1') or (write_camera_data_addr = x"4AFFF") then
				write_camera_data_addr <= (others=>'0');
			elsif (write_camera_data_full = '0') and (write_next_camera_data = '1') then
				write_camera_data_addr <= write_camera_data_addr +1;
			end if;
	   end if;
	end process;
		
	-- Camera Image Data
	process(write_camera_data_addr)
	begin			
		-- First Pixel
		if (write_camera_data_addr = "0000000000000000000")  then
			write_camera_data <= X"F00";

		-- Second Pixel
		elsif (write_camera_data_addr = "0000000000000000001")  then
			write_camera_data <= X"0F0";

		-- Third Pixel
		elsif (write_camera_data_addr = "0000000000000000010")  then
			write_camera_data <= X"00F";

		-- First Line
		elsif (write_camera_data_addr < "0000000001010000000")  then
			write_camera_data <= X"FFF";
		
		-- Last Line
		elsif (write_camera_data_addr >= "1001010110110000000") then
			write_camera_data <= X"0F0";
			
		else
			-- Left or Right Edges
			case write_camera_data_addr is
                when "0000000001010000000" => write_camera_data <= X"F80";
                when "0000000010011111111" => write_camera_data <= X"F80";
                when "0000000010100000000" => write_camera_data <= X"F80";
                when "0000000011101111111" => write_camera_data <= X"F80";
                when "0000000011110000000" => write_camera_data <= X"F80";
                when "0000000100111111111" => write_camera_data <= X"F80";
                when "0000000101000000000" => write_camera_data <= X"F80";
                when "0000000110001111111" => write_camera_data <= X"F80";
                when "0000000110010000000" => write_camera_data <= X"F80";
                when "0000000111011111111" => write_camera_data <= X"F80";
                when "0000000111100000000" => write_camera_data <= X"F80";
                when "0000001000101111111" => write_camera_data <= X"F80";
                when "0000001000110000000" => write_camera_data <= X"F80";
                when "0000001001111111111" => write_camera_data <= X"F80";
                when "0000001010000000000" => write_camera_data <= X"F80";
                when "0000001011001111111" => write_camera_data <= X"F80";
                when "0000001011010000000" => write_camera_data <= X"F80";
                when "0000001100011111111" => write_camera_data <= X"F80";
                when "0000001100100000000" => write_camera_data <= X"F80";
                when "0000001101101111111" => write_camera_data <= X"F80";
                when "0000001101110000000" => write_camera_data <= X"F80";
                when "0000001110111111111" => write_camera_data <= X"F80";
                when "0000001111000000000" => write_camera_data <= X"F80";
                when "0000010000001111111" => write_camera_data <= X"F80";
                when "0000010000010000000" => write_camera_data <= X"F80";
                when "0000010001011111111" => write_camera_data <= X"F80";
                when "0000010001100000000" => write_camera_data <= X"F80";
                when "0000010010101111111" => write_camera_data <= X"F80";
                when "0000010010110000000" => write_camera_data <= X"F80";
                when "0000010011111111111" => write_camera_data <= X"F80";
                when "0000010100000000000" => write_camera_data <= X"F80";
                when "0000010101001111111" => write_camera_data <= X"F80";
                when "0000010101010000000" => write_camera_data <= X"F80";
                when "0000010110011111111" => write_camera_data <= X"F80";
                when "0000010110100000000" => write_camera_data <= X"F80";
                when "0000010111101111111" => write_camera_data <= X"F80";
                when "0000010111110000000" => write_camera_data <= X"F80";
                when "0000011000111111111" => write_camera_data <= X"F80";
                when "0000011001000000000" => write_camera_data <= X"F80";
                when "0000011010001111111" => write_camera_data <= X"F80";
                when "0000011010010000000" => write_camera_data <= X"F80";
                when "0000011011011111111" => write_camera_data <= X"F80";
                when "0000011011100000000" => write_camera_data <= X"F80";
                when "0000011100101111111" => write_camera_data <= X"F80";
                when "0000011100110000000" => write_camera_data <= X"F80";
                when "0000011101111111111" => write_camera_data <= X"F80";
                when "0000011110000000000" => write_camera_data <= X"F80";
                when "0000011111001111111" => write_camera_data <= X"F80";
                when "0000011111010000000" => write_camera_data <= X"F80";
                when "0000100000011111111" => write_camera_data <= X"F80";
                when "0000100000100000000" => write_camera_data <= X"F80";
                when "0000100001101111111" => write_camera_data <= X"F80";
                when "0000100001110000000" => write_camera_data <= X"F80";
                when "0000100010111111111" => write_camera_data <= X"F80";
                when "0000100011000000000" => write_camera_data <= X"F80";
                when "0000100100001111111" => write_camera_data <= X"F80";
                when "0000100100010000000" => write_camera_data <= X"F80";
                when "0000100101011111111" => write_camera_data <= X"F80";
                when "0000100101100000000" => write_camera_data <= X"F80";
                when "0000100110101111111" => write_camera_data <= X"F80";
                when "0000100110110000000" => write_camera_data <= X"F80";
                when "0000100111111111111" => write_camera_data <= X"F80";
                when "0000101000000000000" => write_camera_data <= X"F80";
                when "0000101001001111111" => write_camera_data <= X"F80";
                when "0000101001010000000" => write_camera_data <= X"F80";
                when "0000101010011111111" => write_camera_data <= X"F80";
                when "0000101010100000000" => write_camera_data <= X"F80";
                when "0000101011101111111" => write_camera_data <= X"F80";
                when "0000101011110000000" => write_camera_data <= X"F80";
                when "0000101100111111111" => write_camera_data <= X"F80";
                when "0000101101000000000" => write_camera_data <= X"F80";
                when "0000101110001111111" => write_camera_data <= X"F80";
                when "0000101110010000000" => write_camera_data <= X"F80";
                when "0000101111011111111" => write_camera_data <= X"F80";
                when "0000101111100000000" => write_camera_data <= X"F80";
                when "0000110000101111111" => write_camera_data <= X"F80";
                when "0000110000110000000" => write_camera_data <= X"F80";
                when "0000110001111111111" => write_camera_data <= X"F80";
                when "0000110010000000000" => write_camera_data <= X"F80";
                when "0000110011001111111" => write_camera_data <= X"F80";
                when "0000110011010000000" => write_camera_data <= X"F80";
                when "0000110100011111111" => write_camera_data <= X"F80";
                when "0000110100100000000" => write_camera_data <= X"F80";
                when "0000110101101111111" => write_camera_data <= X"F80";
                when "0000110101110000000" => write_camera_data <= X"F80";
                when "0000110110111111111" => write_camera_data <= X"F80";
                when "0000110111000000000" => write_camera_data <= X"F80";
                when "0000111000001111111" => write_camera_data <= X"F80";
                when "0000111000010000000" => write_camera_data <= X"F80";
                when "0000111001011111111" => write_camera_data <= X"F80";
                when "0000111001100000000" => write_camera_data <= X"F80";
                when "0000111010101111111" => write_camera_data <= X"F80";
                when "0000111010110000000" => write_camera_data <= X"F80";
                when "0000111011111111111" => write_camera_data <= X"F80";
                when "0000111100000000000" => write_camera_data <= X"F80";
                when "0000111101001111111" => write_camera_data <= X"F80";
                when "0000111101010000000" => write_camera_data <= X"F80";
                when "0000111110011111111" => write_camera_data <= X"F80";
                when "0000111110100000000" => write_camera_data <= X"F80";
                when "0000111111101111111" => write_camera_data <= X"F80";
                when "0000111111110000000" => write_camera_data <= X"F80";
                when "0001000000111111111" => write_camera_data <= X"F80";
                when "0001000001000000000" => write_camera_data <= X"F80";
                when "0001000010001111111" => write_camera_data <= X"F80";
                when "0001000010010000000" => write_camera_data <= X"F80";
                when "0001000011011111111" => write_camera_data <= X"F80";
                when "0001000011100000000" => write_camera_data <= X"F80";
                when "0001000100101111111" => write_camera_data <= X"F80";
                when "0001000100110000000" => write_camera_data <= X"F80";
                when "0001000101111111111" => write_camera_data <= X"F80";
                when "0001000110000000000" => write_camera_data <= X"F80";
                when "0001000111001111111" => write_camera_data <= X"F80";
                when "0001000111010000000" => write_camera_data <= X"F80";
                when "0001001000011111111" => write_camera_data <= X"F80";
                when "0001001000100000000" => write_camera_data <= X"F80";
                when "0001001001101111111" => write_camera_data <= X"F80";
                when "0001001001110000000" => write_camera_data <= X"F80";
                when "0001001010111111111" => write_camera_data <= X"F80";
                when "0001001011000000000" => write_camera_data <= X"F80";
                when "0001001100001111111" => write_camera_data <= X"F80";
                when "0001001100010000000" => write_camera_data <= X"F80";
                when "0001001101011111111" => write_camera_data <= X"F80";
                when "0001001101100000000" => write_camera_data <= X"F80";
                when "0001001110101111111" => write_camera_data <= X"F80";
                when "0001001110110000000" => write_camera_data <= X"F80";
                when "0001001111111111111" => write_camera_data <= X"F80";
                when "0001010000000000000" => write_camera_data <= X"F80";
                when "0001010001001111111" => write_camera_data <= X"F80";
                when "0001010001010000000" => write_camera_data <= X"F80";
                when "0001010010011111111" => write_camera_data <= X"F80";
                when "0001010010100000000" => write_camera_data <= X"F80";
                when "0001010011101111111" => write_camera_data <= X"F80";
                when "0001010011110000000" => write_camera_data <= X"F80";
                when "0001010100111111111" => write_camera_data <= X"F80";
                when "0001010101000000000" => write_camera_data <= X"F80";
                when "0001010110001111111" => write_camera_data <= X"F80";
                when "0001010110010000000" => write_camera_data <= X"F80";
                when "0001010111011111111" => write_camera_data <= X"F80";
                when "0001010111100000000" => write_camera_data <= X"F80";
                when "0001011000101111111" => write_camera_data <= X"F80";
                when "0001011000110000000" => write_camera_data <= X"F80";
                when "0001011001111111111" => write_camera_data <= X"F80";
                when "0001011010000000000" => write_camera_data <= X"F80";
                when "0001011011001111111" => write_camera_data <= X"F80";
                when "0001011011010000000" => write_camera_data <= X"F80";
                when "0001011100011111111" => write_camera_data <= X"F80";
                when "0001011100100000000" => write_camera_data <= X"F80";
                when "0001011101101111111" => write_camera_data <= X"F80";
                when "0001011101110000000" => write_camera_data <= X"F80";
                when "0001011110111111111" => write_camera_data <= X"F80";
                when "0001011111000000000" => write_camera_data <= X"F80";
                when "0001100000001111111" => write_camera_data <= X"F80";
                when "0001100000010000000" => write_camera_data <= X"F80";
                when "0001100001011111111" => write_camera_data <= X"F80";
                when "0001100001100000000" => write_camera_data <= X"F80";
                when "0001100010101111111" => write_camera_data <= X"F80";
                when "0001100010110000000" => write_camera_data <= X"F80";
                when "0001100011111111111" => write_camera_data <= X"F80";
                when "0001100100000000000" => write_camera_data <= X"F80";
                when "0001100101001111111" => write_camera_data <= X"F80";
                when "0001100101010000000" => write_camera_data <= X"F80";
                when "0001100110011111111" => write_camera_data <= X"F80";
                when "0001100110100000000" => write_camera_data <= X"F80";
                when "0001100111101111111" => write_camera_data <= X"F80";
                when "0001100111110000000" => write_camera_data <= X"F80";
                when "0001101000111111111" => write_camera_data <= X"F80";
                when "0001101001000000000" => write_camera_data <= X"F80";
                when "0001101010001111111" => write_camera_data <= X"F80";
                when "0001101010010000000" => write_camera_data <= X"F80";
                when "0001101011011111111" => write_camera_data <= X"F80";
                when "0001101011100000000" => write_camera_data <= X"F80";
                when "0001101100101111111" => write_camera_data <= X"F80";
                when "0001101100110000000" => write_camera_data <= X"F80";
                when "0001101101111111111" => write_camera_data <= X"F80";
                when "0001101110000000000" => write_camera_data <= X"F80";
                when "0001101111001111111" => write_camera_data <= X"F80";
                when "0001101111010000000" => write_camera_data <= X"F80";
                when "0001110000011111111" => write_camera_data <= X"F80";
                when "0001110000100000000" => write_camera_data <= X"F80";
                when "0001110001101111111" => write_camera_data <= X"F80";
                when "0001110001110000000" => write_camera_data <= X"F80";
                when "0001110010111111111" => write_camera_data <= X"F80";
                when "0001110011000000000" => write_camera_data <= X"F80";
                when "0001110100001111111" => write_camera_data <= X"F80";
                when "0001110100010000000" => write_camera_data <= X"F80";
                when "0001110101011111111" => write_camera_data <= X"F80";
                when "0001110101100000000" => write_camera_data <= X"F80";
                when "0001110110101111111" => write_camera_data <= X"F80";
                when "0001110110110000000" => write_camera_data <= X"F80";
                when "0001110111111111111" => write_camera_data <= X"F80";
                when "0001111000000000000" => write_camera_data <= X"F80";
                when "0001111001001111111" => write_camera_data <= X"F80";
                when "0001111001010000000" => write_camera_data <= X"F80";
                when "0001111010011111111" => write_camera_data <= X"F80";
                when "0001111010100000000" => write_camera_data <= X"F80";
                when "0001111011101111111" => write_camera_data <= X"F80";
                when "0001111011110000000" => write_camera_data <= X"F80";
                when "0001111100111111111" => write_camera_data <= X"F80";
                when "0001111101000000000" => write_camera_data <= X"F80";
                when "0001111110001111111" => write_camera_data <= X"F80";
                when "0001111110010000000" => write_camera_data <= X"F80";
                when "0001111111011111111" => write_camera_data <= X"F80";
                when "0001111111100000000" => write_camera_data <= X"F80";
                when "0010000000101111111" => write_camera_data <= X"F80";
                when "0010000000110000000" => write_camera_data <= X"F80";
                when "0010000001111111111" => write_camera_data <= X"F80";
                when "0010000010000000000" => write_camera_data <= X"F80";
                when "0010000011001111111" => write_camera_data <= X"F80";
                when "0010000011010000000" => write_camera_data <= X"F80";
                when "0010000100011111111" => write_camera_data <= X"F80";
                when "0010000100100000000" => write_camera_data <= X"F80";
                when "0010000101101111111" => write_camera_data <= X"F80";
                when "0010000101110000000" => write_camera_data <= X"F80";
                when "0010000110111111111" => write_camera_data <= X"F80";
                when "0010000111000000000" => write_camera_data <= X"F80";
                when "0010001000001111111" => write_camera_data <= X"F80";
                when "0010001000010000000" => write_camera_data <= X"F80";
                when "0010001001011111111" => write_camera_data <= X"F80";
                when "0010001001100000000" => write_camera_data <= X"F80";
                when "0010001010101111111" => write_camera_data <= X"F80";
                when "0010001010110000000" => write_camera_data <= X"F80";
                when "0010001011111111111" => write_camera_data <= X"F80";
                when "0010001100000000000" => write_camera_data <= X"F80";
                when "0010001101001111111" => write_camera_data <= X"F80";
                when "0010001101010000000" => write_camera_data <= X"F80";
                when "0010001110011111111" => write_camera_data <= X"F80";
                when "0010001110100000000" => write_camera_data <= X"F80";
                when "0010001111101111111" => write_camera_data <= X"F80";
                when "0010001111110000000" => write_camera_data <= X"F80";
                when "0010010000111111111" => write_camera_data <= X"F80";
                when "0010010001000000000" => write_camera_data <= X"F80";
                when "0010010010001111111" => write_camera_data <= X"F80";
                when "0010010010010000000" => write_camera_data <= X"F80";
                when "0010010011011111111" => write_camera_data <= X"F80";
                when "0010010011100000000" => write_camera_data <= X"F80";
                when "0010010100101111111" => write_camera_data <= X"F80";
                when "0010010100110000000" => write_camera_data <= X"F80";
                when "0010010101111111111" => write_camera_data <= X"F80";
                when "0010010110000000000" => write_camera_data <= X"F80";
                when "0010010111001111111" => write_camera_data <= X"F80";
                when "0010010111010000000" => write_camera_data <= X"F80";
                when "0010011000011111111" => write_camera_data <= X"F80";
                when "0010011000100000000" => write_camera_data <= X"F80";
                when "0010011001101111111" => write_camera_data <= X"F80";
                when "0010011001110000000" => write_camera_data <= X"F80";
                when "0010011010111111111" => write_camera_data <= X"F80";
                when "0010011011000000000" => write_camera_data <= X"F80";
                when "0010011100001111111" => write_camera_data <= X"F80";
                when "0010011100010000000" => write_camera_data <= X"F80";
                when "0010011101011111111" => write_camera_data <= X"F80";
                when "0010011101100000000" => write_camera_data <= X"F80";
                when "0010011110101111111" => write_camera_data <= X"F80";
                when "0010011110110000000" => write_camera_data <= X"F80";
                when "0010011111111111111" => write_camera_data <= X"F80";
                when "0010100000000000000" => write_camera_data <= X"F80";
                when "0010100001001111111" => write_camera_data <= X"F80";
                when "0010100001010000000" => write_camera_data <= X"F80";
                when "0010100010011111111" => write_camera_data <= X"F80";
                when "0010100010100000000" => write_camera_data <= X"F80";
                when "0010100011101111111" => write_camera_data <= X"F80";
                when "0010100011110000000" => write_camera_data <= X"F80";
                when "0010100100111111111" => write_camera_data <= X"F80";
                when "0010100101000000000" => write_camera_data <= X"F80";
                when "0010100110001111111" => write_camera_data <= X"F80";
                when "0010100110010000000" => write_camera_data <= X"F80";
                when "0010100111011111111" => write_camera_data <= X"F80";
                when "0010100111100000000" => write_camera_data <= X"F80";
                when "0010101000101111111" => write_camera_data <= X"F80";
                when "0010101000110000000" => write_camera_data <= X"F80";
                when "0010101001111111111" => write_camera_data <= X"F80";
                when "0010101010000000000" => write_camera_data <= X"F80";
                when "0010101011001111111" => write_camera_data <= X"F80";
                when "0010101011010000000" => write_camera_data <= X"F80";
                when "0010101100011111111" => write_camera_data <= X"F80";
                when "0010101100100000000" => write_camera_data <= X"F80";
                when "0010101101101111111" => write_camera_data <= X"F80";
                when "0010101101110000000" => write_camera_data <= X"F80";
                when "0010101110111111111" => write_camera_data <= X"F80";
                when "0010101111000000000" => write_camera_data <= X"F80";
                when "0010110000001111111" => write_camera_data <= X"F80";
                when "0010110000010000000" => write_camera_data <= X"F80";
                when "0010110001011111111" => write_camera_data <= X"F80";
                when "0010110001100000000" => write_camera_data <= X"F80";
                when "0010110010101111111" => write_camera_data <= X"F80";
                when "0010110010110000000" => write_camera_data <= X"F80";
                when "0010110011111111111" => write_camera_data <= X"F80";
                when "0010110100000000000" => write_camera_data <= X"F80";
                when "0010110101001111111" => write_camera_data <= X"F80";
                when "0010110101010000000" => write_camera_data <= X"F80";
                when "0010110110011111111" => write_camera_data <= X"F80";
                when "0010110110100000000" => write_camera_data <= X"F80";
                when "0010110111101111111" => write_camera_data <= X"F80";
                when "0010110111110000000" => write_camera_data <= X"F80";
                when "0010111000111111111" => write_camera_data <= X"F80";
                when "0010111001000000000" => write_camera_data <= X"F80";
                when "0010111010001111111" => write_camera_data <= X"F80";
                when "0010111010010000000" => write_camera_data <= X"F80";
                when "0010111011011111111" => write_camera_data <= X"F80";
                when "0010111011100000000" => write_camera_data <= X"F80";
                when "0010111100101111111" => write_camera_data <= X"F80";
                when "0010111100110000000" => write_camera_data <= X"F80";
                when "0010111101111111111" => write_camera_data <= X"F80";
                when "0010111110000000000" => write_camera_data <= X"F80";
                when "0010111111001111111" => write_camera_data <= X"F80";
                when "0010111111010000000" => write_camera_data <= X"F80";
                when "0011000000011111111" => write_camera_data <= X"F80";
                when "0011000000100000000" => write_camera_data <= X"F80";
                when "0011000001101111111" => write_camera_data <= X"F80";
                when "0011000001110000000" => write_camera_data <= X"F80";
                when "0011000010111111111" => write_camera_data <= X"F80";
                when "0011000011000000000" => write_camera_data <= X"F80";
                when "0011000100001111111" => write_camera_data <= X"F80";
                when "0011000100010000000" => write_camera_data <= X"F80";
                when "0011000101011111111" => write_camera_data <= X"F80";
                when "0011000101100000000" => write_camera_data <= X"F80";
                when "0011000110101111111" => write_camera_data <= X"F80";
                when "0011000110110000000" => write_camera_data <= X"F80";
                when "0011000111111111111" => write_camera_data <= X"F80";
                when "0011001000000000000" => write_camera_data <= X"F80";
                when "0011001001001111111" => write_camera_data <= X"F80";
                when "0011001001010000000" => write_camera_data <= X"F80";
                when "0011001010011111111" => write_camera_data <= X"F80";
                when "0011001010100000000" => write_camera_data <= X"F80";
                when "0011001011101111111" => write_camera_data <= X"F80";
                when "0011001011110000000" => write_camera_data <= X"F80";
                when "0011001100111111111" => write_camera_data <= X"F80";
                when "0011001101000000000" => write_camera_data <= X"F80";
                when "0011001110001111111" => write_camera_data <= X"F80";
                when "0011001110010000000" => write_camera_data <= X"F80";
                when "0011001111011111111" => write_camera_data <= X"F80";
                when "0011001111100000000" => write_camera_data <= X"F80";
                when "0011010000101111111" => write_camera_data <= X"F80";
                when "0011010000110000000" => write_camera_data <= X"F80";
                when "0011010001111111111" => write_camera_data <= X"F80";
                when "0011010010000000000" => write_camera_data <= X"F80";
                when "0011010011001111111" => write_camera_data <= X"F80";
                when "0011010011010000000" => write_camera_data <= X"F80";
                when "0011010100011111111" => write_camera_data <= X"F80";
                when "0011010100100000000" => write_camera_data <= X"F80";
                when "0011010101101111111" => write_camera_data <= X"F80";
                when "0011010101110000000" => write_camera_data <= X"F80";
                when "0011010110111111111" => write_camera_data <= X"F80";
                when "0011010111000000000" => write_camera_data <= X"F80";
                when "0011011000001111111" => write_camera_data <= X"F80";
                when "0011011000010000000" => write_camera_data <= X"F80";
                when "0011011001011111111" => write_camera_data <= X"F80";
                when "0011011001100000000" => write_camera_data <= X"F80";
                when "0011011010101111111" => write_camera_data <= X"F80";
                when "0011011010110000000" => write_camera_data <= X"F80";
                when "0011011011111111111" => write_camera_data <= X"F80";
                when "0011011100000000000" => write_camera_data <= X"F80";
                when "0011011101001111111" => write_camera_data <= X"F80";
                when "0011011101010000000" => write_camera_data <= X"F80";
                when "0011011110011111111" => write_camera_data <= X"F80";
                when "0011011110100000000" => write_camera_data <= X"F80";
                when "0011011111101111111" => write_camera_data <= X"F80";
                when "0011011111110000000" => write_camera_data <= X"F80";
                when "0011100000111111111" => write_camera_data <= X"F80";
                when "0011100001000000000" => write_camera_data <= X"F80";
                when "0011100010001111111" => write_camera_data <= X"F80";
                when "0011100010010000000" => write_camera_data <= X"F80";
                when "0011100011011111111" => write_camera_data <= X"F80";
                when "0011100011100000000" => write_camera_data <= X"F80";
                when "0011100100101111111" => write_camera_data <= X"F80";
                when "0011100100110000000" => write_camera_data <= X"F80";
                when "0011100101111111111" => write_camera_data <= X"F80";
                when "0011100110000000000" => write_camera_data <= X"F80";
                when "0011100111001111111" => write_camera_data <= X"F80";
                when "0011100111010000000" => write_camera_data <= X"F80";
                when "0011101000011111111" => write_camera_data <= X"F80";
                when "0011101000100000000" => write_camera_data <= X"F80";
                when "0011101001101111111" => write_camera_data <= X"F80";
                when "0011101001110000000" => write_camera_data <= X"F80";
                when "0011101010111111111" => write_camera_data <= X"F80";
                when "0011101011000000000" => write_camera_data <= X"F80";
                when "0011101100001111111" => write_camera_data <= X"F80";
                when "0011101100010000000" => write_camera_data <= X"F80";
                when "0011101101011111111" => write_camera_data <= X"F80";
                when "0011101101100000000" => write_camera_data <= X"F80";
                when "0011101110101111111" => write_camera_data <= X"F80";
                when "0011101110110000000" => write_camera_data <= X"F80";
                when "0011101111111111111" => write_camera_data <= X"F80";
                when "0011110000000000000" => write_camera_data <= X"F80";
                when "0011110001001111111" => write_camera_data <= X"F80";
                when "0011110001010000000" => write_camera_data <= X"F80";
                when "0011110010011111111" => write_camera_data <= X"F80";
                when "0011110010100000000" => write_camera_data <= X"F80";
                when "0011110011101111111" => write_camera_data <= X"F80";
                when "0011110011110000000" => write_camera_data <= X"F80";
                when "0011110100111111111" => write_camera_data <= X"F80";
                when "0011110101000000000" => write_camera_data <= X"F80";
                when "0011110110001111111" => write_camera_data <= X"F80";
                when "0011110110010000000" => write_camera_data <= X"F80";
                when "0011110111011111111" => write_camera_data <= X"F80";
                when "0011110111100000000" => write_camera_data <= X"F80";
                when "0011111000101111111" => write_camera_data <= X"F80";
                when "0011111000110000000" => write_camera_data <= X"F80";
                when "0011111001111111111" => write_camera_data <= X"F80";
                when "0011111010000000000" => write_camera_data <= X"F80";
                when "0011111011001111111" => write_camera_data <= X"F80";
                when "0011111011010000000" => write_camera_data <= X"F80";
                when "0011111100011111111" => write_camera_data <= X"F80";
                when "0011111100100000000" => write_camera_data <= X"F80";
                when "0011111101101111111" => write_camera_data <= X"F80";
                when "0011111101110000000" => write_camera_data <= X"F80";
                when "0011111110111111111" => write_camera_data <= X"F80";
                when "0011111111000000000" => write_camera_data <= X"F80";
                when "0100000000001111111" => write_camera_data <= X"F80";
                when "0100000000010000000" => write_camera_data <= X"F80";
                when "0100000001011111111" => write_camera_data <= X"F80";
                when "0100000001100000000" => write_camera_data <= X"F80";
                when "0100000010101111111" => write_camera_data <= X"F80";
                when "0100000010110000000" => write_camera_data <= X"F80";
                when "0100000011111111111" => write_camera_data <= X"F80";
                when "0100000100000000000" => write_camera_data <= X"F80";
                when "0100000101001111111" => write_camera_data <= X"F80";
                when "0100000101010000000" => write_camera_data <= X"F80";
                when "0100000110011111111" => write_camera_data <= X"F80";
                when "0100000110100000000" => write_camera_data <= X"F80";
                when "0100000111101111111" => write_camera_data <= X"F80";
                when "0100000111110000000" => write_camera_data <= X"F80";
                when "0100001000111111111" => write_camera_data <= X"F80";
                when "0100001001000000000" => write_camera_data <= X"F80";
                when "0100001010001111111" => write_camera_data <= X"F80";
                when "0100001010010000000" => write_camera_data <= X"F80";
                when "0100001011011111111" => write_camera_data <= X"F80";
                when "0100001011100000000" => write_camera_data <= X"F80";
                when "0100001100101111111" => write_camera_data <= X"F80";
                when "0100001100110000000" => write_camera_data <= X"F80";
                when "0100001101111111111" => write_camera_data <= X"F80";
                when "0100001110000000000" => write_camera_data <= X"F80";
                when "0100001111001111111" => write_camera_data <= X"F80";
                when "0100001111010000000" => write_camera_data <= X"F80";
                when "0100010000011111111" => write_camera_data <= X"F80";
                when "0100010000100000000" => write_camera_data <= X"F80";
                when "0100010001101111111" => write_camera_data <= X"F80";
                when "0100010001110000000" => write_camera_data <= X"F80";
                when "0100010010111111111" => write_camera_data <= X"F80";
                when "0100010011000000000" => write_camera_data <= X"F80";
                when "0100010100001111111" => write_camera_data <= X"F80";
                when "0100010100010000000" => write_camera_data <= X"F80";
                when "0100010101011111111" => write_camera_data <= X"F80";
                when "0100010101100000000" => write_camera_data <= X"F80";
                when "0100010110101111111" => write_camera_data <= X"F80";
                when "0100010110110000000" => write_camera_data <= X"F80";
                when "0100010111111111111" => write_camera_data <= X"F80";
                when "0100011000000000000" => write_camera_data <= X"F80";
                when "0100011001001111111" => write_camera_data <= X"F80";
                when "0100011001010000000" => write_camera_data <= X"F80";
                when "0100011010011111111" => write_camera_data <= X"F80";
                when "0100011010100000000" => write_camera_data <= X"F80";
                when "0100011011101111111" => write_camera_data <= X"F80";
                when "0100011011110000000" => write_camera_data <= X"F80";
                when "0100011100111111111" => write_camera_data <= X"F80";
                when "0100011101000000000" => write_camera_data <= X"F80";
                when "0100011110001111111" => write_camera_data <= X"F80";
                when "0100011110010000000" => write_camera_data <= X"F80";
                when "0100011111011111111" => write_camera_data <= X"F80";
                when "0100011111100000000" => write_camera_data <= X"F80";
                when "0100100000101111111" => write_camera_data <= X"F80";
                when "0100100000110000000" => write_camera_data <= X"F80";
                when "0100100001111111111" => write_camera_data <= X"F80";
                when "0100100010000000000" => write_camera_data <= X"F80";
                when "0100100011001111111" => write_camera_data <= X"F80";
                when "0100100011010000000" => write_camera_data <= X"F80";
                when "0100100100011111111" => write_camera_data <= X"F80";
                when "0100100100100000000" => write_camera_data <= X"F80";
                when "0100100101101111111" => write_camera_data <= X"F80";
                when "0100100101110000000" => write_camera_data <= X"F80";
                when "0100100110111111111" => write_camera_data <= X"F80";
                when "0100100111000000000" => write_camera_data <= X"F80";
                when "0100101000001111111" => write_camera_data <= X"F80";
                when "0100101000010000000" => write_camera_data <= X"F80";
                when "0100101001011111111" => write_camera_data <= X"F80";
                when "0100101001100000000" => write_camera_data <= X"F80";
                when "0100101010101111111" => write_camera_data <= X"F80";
                when "0100101010110000000" => write_camera_data <= X"F80";
                when "0100101011111111111" => write_camera_data <= X"F80";
                when "0100101100000000000" => write_camera_data <= X"F80";
                when "0100101101001111111" => write_camera_data <= X"F80";
                when "0100101101010000000" => write_camera_data <= X"F80";
                when "0100101110011111111" => write_camera_data <= X"F80";
                when "0100101110100000000" => write_camera_data <= X"F80";
                when "0100101111101111111" => write_camera_data <= X"F80";
                when "0100101111110000000" => write_camera_data <= X"F80";
                when "0100110000111111111" => write_camera_data <= X"F80";
                when "0100110001000000000" => write_camera_data <= X"F80";
                when "0100110010001111111" => write_camera_data <= X"F80";
                when "0100110010010000000" => write_camera_data <= X"F80";
                when "0100110011011111111" => write_camera_data <= X"F80";
                when "0100110011100000000" => write_camera_data <= X"F80";
                when "0100110100101111111" => write_camera_data <= X"F80";
                when "0100110100110000000" => write_camera_data <= X"F80";
                when "0100110101111111111" => write_camera_data <= X"F80";
                when "0100110110000000000" => write_camera_data <= X"F80";
                when "0100110111001111111" => write_camera_data <= X"F80";
                when "0100110111010000000" => write_camera_data <= X"F80";
                when "0100111000011111111" => write_camera_data <= X"F80";
                when "0100111000100000000" => write_camera_data <= X"F80";
                when "0100111001101111111" => write_camera_data <= X"F80";
                when "0100111001110000000" => write_camera_data <= X"F80";
                when "0100111010111111111" => write_camera_data <= X"F80";
                when "0100111011000000000" => write_camera_data <= X"F80";
                when "0100111100001111111" => write_camera_data <= X"F80";
                when "0100111100010000000" => write_camera_data <= X"F80";
                when "0100111101011111111" => write_camera_data <= X"F80";
                when "0100111101100000000" => write_camera_data <= X"F80";
                when "0100111110101111111" => write_camera_data <= X"F80";
                when "0100111110110000000" => write_camera_data <= X"F80";
                when "0100111111111111111" => write_camera_data <= X"F80";
                when "0101000000000000000" => write_camera_data <= X"F80";
                when "0101000001001111111" => write_camera_data <= X"F80";
                when "0101000001010000000" => write_camera_data <= X"F80";
                when "0101000010011111111" => write_camera_data <= X"F80";
                when "0101000010100000000" => write_camera_data <= X"F80";
                when "0101000011101111111" => write_camera_data <= X"F80";
                when "0101000011110000000" => write_camera_data <= X"F80";
                when "0101000100111111111" => write_camera_data <= X"F80";
                when "0101000101000000000" => write_camera_data <= X"F80";
                when "0101000110001111111" => write_camera_data <= X"F80";
                when "0101000110010000000" => write_camera_data <= X"F80";
                when "0101000111011111111" => write_camera_data <= X"F80";
                when "0101000111100000000" => write_camera_data <= X"F80";
                when "0101001000101111111" => write_camera_data <= X"F80";
                when "0101001000110000000" => write_camera_data <= X"F80";
                when "0101001001111111111" => write_camera_data <= X"F80";
                when "0101001010000000000" => write_camera_data <= X"F80";
                when "0101001011001111111" => write_camera_data <= X"F80";
                when "0101001011010000000" => write_camera_data <= X"F80";
                when "0101001100011111111" => write_camera_data <= X"F80";
                when "0101001100100000000" => write_camera_data <= X"F80";
                when "0101001101101111111" => write_camera_data <= X"F80";
                when "0101001101110000000" => write_camera_data <= X"F80";
                when "0101001110111111111" => write_camera_data <= X"F80";
                when "0101001111000000000" => write_camera_data <= X"F80";
                when "0101010000001111111" => write_camera_data <= X"F80";
                when "0101010000010000000" => write_camera_data <= X"F80";
                when "0101010001011111111" => write_camera_data <= X"F80";
                when "0101010001100000000" => write_camera_data <= X"F80";
                when "0101010010101111111" => write_camera_data <= X"F80";
                when "0101010010110000000" => write_camera_data <= X"F80";
                when "0101010011111111111" => write_camera_data <= X"F80";
                when "0101010100000000000" => write_camera_data <= X"F80";
                when "0101010101001111111" => write_camera_data <= X"F80";
                when "0101010101010000000" => write_camera_data <= X"F80";
                when "0101010110011111111" => write_camera_data <= X"F80";
                when "0101010110100000000" => write_camera_data <= X"F80";
                when "0101010111101111111" => write_camera_data <= X"F80";
                when "0101010111110000000" => write_camera_data <= X"F80";
                when "0101011000111111111" => write_camera_data <= X"F80";
                when "0101011001000000000" => write_camera_data <= X"F80";
                when "0101011010001111111" => write_camera_data <= X"F80";
                when "0101011010010000000" => write_camera_data <= X"F80";
                when "0101011011011111111" => write_camera_data <= X"F80";
                when "0101011011100000000" => write_camera_data <= X"F80";
                when "0101011100101111111" => write_camera_data <= X"F80";
                when "0101011100110000000" => write_camera_data <= X"F80";
                when "0101011101111111111" => write_camera_data <= X"F80";
                when "0101011110000000000" => write_camera_data <= X"F80";
                when "0101011111001111111" => write_camera_data <= X"F80";
                when "0101011111010000000" => write_camera_data <= X"F80";
                when "0101100000011111111" => write_camera_data <= X"F80";
                when "0101100000100000000" => write_camera_data <= X"F80";
                when "0101100001101111111" => write_camera_data <= X"F80";
                when "0101100001110000000" => write_camera_data <= X"F80";
                when "0101100010111111111" => write_camera_data <= X"F80";
                when "0101100011000000000" => write_camera_data <= X"F80";
                when "0101100100001111111" => write_camera_data <= X"F80";
                when "0101100100010000000" => write_camera_data <= X"F80";
                when "0101100101011111111" => write_camera_data <= X"F80";
                when "0101100101100000000" => write_camera_data <= X"F80";
                when "0101100110101111111" => write_camera_data <= X"F80";
                when "0101100110110000000" => write_camera_data <= X"F80";
                when "0101100111111111111" => write_camera_data <= X"F80";
                when "0101101000000000000" => write_camera_data <= X"F80";
                when "0101101001001111111" => write_camera_data <= X"F80";
                when "0101101001010000000" => write_camera_data <= X"F80";
                when "0101101010011111111" => write_camera_data <= X"F80";
                when "0101101010100000000" => write_camera_data <= X"F80";
                when "0101101011101111111" => write_camera_data <= X"F80";
                when "0101101011110000000" => write_camera_data <= X"F80";
                when "0101101100111111111" => write_camera_data <= X"F80";
                when "0101101101000000000" => write_camera_data <= X"F80";
                when "0101101110001111111" => write_camera_data <= X"F80";
                when "0101101110010000000" => write_camera_data <= X"F80";
                when "0101101111011111111" => write_camera_data <= X"F80";
                when "0101101111100000000" => write_camera_data <= X"F80";
                when "0101110000101111111" => write_camera_data <= X"F80";
                when "0101110000110000000" => write_camera_data <= X"F80";
                when "0101110001111111111" => write_camera_data <= X"F80";
                when "0101110010000000000" => write_camera_data <= X"F80";
                when "0101110011001111111" => write_camera_data <= X"F80";
                when "0101110011010000000" => write_camera_data <= X"F80";
                when "0101110100011111111" => write_camera_data <= X"F80";
                when "0101110100100000000" => write_camera_data <= X"F80";
                when "0101110101101111111" => write_camera_data <= X"F80";
                when "0101110101110000000" => write_camera_data <= X"F80";
                when "0101110110111111111" => write_camera_data <= X"F80";
                when "0101110111000000000" => write_camera_data <= X"F80";
                when "0101111000001111111" => write_camera_data <= X"F80";
                when "0101111000010000000" => write_camera_data <= X"F80";
                when "0101111001011111111" => write_camera_data <= X"F80";
                when "0101111001100000000" => write_camera_data <= X"F80";
                when "0101111010101111111" => write_camera_data <= X"F80";
                when "0101111010110000000" => write_camera_data <= X"F80";
                when "0101111011111111111" => write_camera_data <= X"F80";
                when "0101111100000000000" => write_camera_data <= X"F80";
                when "0101111101001111111" => write_camera_data <= X"F80";
                when "0101111101010000000" => write_camera_data <= X"F80";
                when "0101111110011111111" => write_camera_data <= X"F80";
                when "0101111110100000000" => write_camera_data <= X"F80";
                when "0101111111101111111" => write_camera_data <= X"F80";
                when "0101111111110000000" => write_camera_data <= X"F80";
                when "0110000000111111111" => write_camera_data <= X"F80";
                when "0110000001000000000" => write_camera_data <= X"F80";
                when "0110000010001111111" => write_camera_data <= X"F80";
                when "0110000010010000000" => write_camera_data <= X"F80";
                when "0110000011011111111" => write_camera_data <= X"F80";
                when "0110000011100000000" => write_camera_data <= X"F80";
                when "0110000100101111111" => write_camera_data <= X"F80";
                when "0110000100110000000" => write_camera_data <= X"F80";
                when "0110000101111111111" => write_camera_data <= X"F80";
                when "0110000110000000000" => write_camera_data <= X"F80";
                when "0110000111001111111" => write_camera_data <= X"F80";
                when "0110000111010000000" => write_camera_data <= X"F80";
                when "0110001000011111111" => write_camera_data <= X"F80";
                when "0110001000100000000" => write_camera_data <= X"F80";
                when "0110001001101111111" => write_camera_data <= X"F80";
                when "0110001001110000000" => write_camera_data <= X"F80";
                when "0110001010111111111" => write_camera_data <= X"F80";
                when "0110001011000000000" => write_camera_data <= X"F80";
                when "0110001100001111111" => write_camera_data <= X"F80";
                when "0110001100010000000" => write_camera_data <= X"F80";
                when "0110001101011111111" => write_camera_data <= X"F80";
                when "0110001101100000000" => write_camera_data <= X"F80";
                when "0110001110101111111" => write_camera_data <= X"F80";
                when "0110001110110000000" => write_camera_data <= X"F80";
                when "0110001111111111111" => write_camera_data <= X"F80";
                when "0110010000000000000" => write_camera_data <= X"F80";
                when "0110010001001111111" => write_camera_data <= X"F80";
                when "0110010001010000000" => write_camera_data <= X"F80";
                when "0110010010011111111" => write_camera_data <= X"F80";
                when "0110010010100000000" => write_camera_data <= X"F80";
                when "0110010011101111111" => write_camera_data <= X"F80";
                when "0110010011110000000" => write_camera_data <= X"F80";
                when "0110010100111111111" => write_camera_data <= X"F80";
                when "0110010101000000000" => write_camera_data <= X"F80";
                when "0110010110001111111" => write_camera_data <= X"F80";
                when "0110010110010000000" => write_camera_data <= X"F80";
                when "0110010111011111111" => write_camera_data <= X"F80";
                when "0110010111100000000" => write_camera_data <= X"F80";
                when "0110011000101111111" => write_camera_data <= X"F80";
                when "0110011000110000000" => write_camera_data <= X"F80";
                when "0110011001111111111" => write_camera_data <= X"F80";
                when "0110011010000000000" => write_camera_data <= X"F80";
                when "0110011011001111111" => write_camera_data <= X"F80";
                when "0110011011010000000" => write_camera_data <= X"F80";
                when "0110011100011111111" => write_camera_data <= X"F80";
                when "0110011100100000000" => write_camera_data <= X"F80";
                when "0110011101101111111" => write_camera_data <= X"F80";
                when "0110011101110000000" => write_camera_data <= X"F80";
                when "0110011110111111111" => write_camera_data <= X"F80";
                when "0110011111000000000" => write_camera_data <= X"F80";
                when "0110100000001111111" => write_camera_data <= X"F80";
                when "0110100000010000000" => write_camera_data <= X"F80";
                when "0110100001011111111" => write_camera_data <= X"F80";
                when "0110100001100000000" => write_camera_data <= X"F80";
                when "0110100010101111111" => write_camera_data <= X"F80";
                when "0110100010110000000" => write_camera_data <= X"F80";
                when "0110100011111111111" => write_camera_data <= X"F80";
                when "0110100100000000000" => write_camera_data <= X"F80";
                when "0110100101001111111" => write_camera_data <= X"F80";
                when "0110100101010000000" => write_camera_data <= X"F80";
                when "0110100110011111111" => write_camera_data <= X"F80";
                when "0110100110100000000" => write_camera_data <= X"F80";
                when "0110100111101111111" => write_camera_data <= X"F80";
                when "0110100111110000000" => write_camera_data <= X"F80";
                when "0110101000111111111" => write_camera_data <= X"F80";
                when "0110101001000000000" => write_camera_data <= X"F80";
                when "0110101010001111111" => write_camera_data <= X"F80";
                when "0110101010010000000" => write_camera_data <= X"F80";
                when "0110101011011111111" => write_camera_data <= X"F80";
                when "0110101011100000000" => write_camera_data <= X"F80";
                when "0110101100101111111" => write_camera_data <= X"F80";
                when "0110101100110000000" => write_camera_data <= X"F80";
                when "0110101101111111111" => write_camera_data <= X"F80";
                when "0110101110000000000" => write_camera_data <= X"F80";
                when "0110101111001111111" => write_camera_data <= X"F80";
                when "0110101111010000000" => write_camera_data <= X"F80";
                when "0110110000011111111" => write_camera_data <= X"F80";
                when "0110110000100000000" => write_camera_data <= X"F80";
                when "0110110001101111111" => write_camera_data <= X"F80";
                when "0110110001110000000" => write_camera_data <= X"F80";
                when "0110110010111111111" => write_camera_data <= X"F80";
                when "0110110011000000000" => write_camera_data <= X"F80";
                when "0110110100001111111" => write_camera_data <= X"F80";
                when "0110110100010000000" => write_camera_data <= X"F80";
                when "0110110101011111111" => write_camera_data <= X"F80";
                when "0110110101100000000" => write_camera_data <= X"F80";
                when "0110110110101111111" => write_camera_data <= X"F80";
                when "0110110110110000000" => write_camera_data <= X"F80";
                when "0110110111111111111" => write_camera_data <= X"F80";
                when "0110111000000000000" => write_camera_data <= X"F80";
                when "0110111001001111111" => write_camera_data <= X"F80";
                when "0110111001010000000" => write_camera_data <= X"F80";
                when "0110111010011111111" => write_camera_data <= X"F80";
                when "0110111010100000000" => write_camera_data <= X"F80";
                when "0110111011101111111" => write_camera_data <= X"F80";
                when "0110111011110000000" => write_camera_data <= X"F80";
                when "0110111100111111111" => write_camera_data <= X"F80";
                when "0110111101000000000" => write_camera_data <= X"F80";
                when "0110111110001111111" => write_camera_data <= X"F80";
                when "0110111110010000000" => write_camera_data <= X"F80";
                when "0110111111011111111" => write_camera_data <= X"F80";
                when "0110111111100000000" => write_camera_data <= X"F80";
                when "0111000000101111111" => write_camera_data <= X"F80";
                when "0111000000110000000" => write_camera_data <= X"F80";
                when "0111000001111111111" => write_camera_data <= X"F80";
                when "0111000010000000000" => write_camera_data <= X"F80";
                when "0111000011001111111" => write_camera_data <= X"F80";
                when "0111000011010000000" => write_camera_data <= X"F80";
                when "0111000100011111111" => write_camera_data <= X"F80";
                when "0111000100100000000" => write_camera_data <= X"F80";
                when "0111000101101111111" => write_camera_data <= X"F80";
                when "0111000101110000000" => write_camera_data <= X"F80";
                when "0111000110111111111" => write_camera_data <= X"F80";
                when "0111000111000000000" => write_camera_data <= X"F80";
                when "0111001000001111111" => write_camera_data <= X"F80";
                when "0111001000010000000" => write_camera_data <= X"F80";
                when "0111001001011111111" => write_camera_data <= X"F80";
                when "0111001001100000000" => write_camera_data <= X"F80";
                when "0111001010101111111" => write_camera_data <= X"F80";
                when "0111001010110000000" => write_camera_data <= X"F80";
                when "0111001011111111111" => write_camera_data <= X"F80";
                when "0111001100000000000" => write_camera_data <= X"F80";
                when "0111001101001111111" => write_camera_data <= X"F80";
                when "0111001101010000000" => write_camera_data <= X"F80";
                when "0111001110011111111" => write_camera_data <= X"F80";
                when "0111001110100000000" => write_camera_data <= X"F80";
                when "0111001111101111111" => write_camera_data <= X"F80";
                when "0111001111110000000" => write_camera_data <= X"F80";
                when "0111010000111111111" => write_camera_data <= X"F80";
                when "0111010001000000000" => write_camera_data <= X"F80";
                when "0111010010001111111" => write_camera_data <= X"F80";
                when "0111010010010000000" => write_camera_data <= X"F80";
                when "0111010011011111111" => write_camera_data <= X"F80";
                when "0111010011100000000" => write_camera_data <= X"F80";
                when "0111010100101111111" => write_camera_data <= X"F80";
                when "0111010100110000000" => write_camera_data <= X"F80";
                when "0111010101111111111" => write_camera_data <= X"F80";
                when "0111010110000000000" => write_camera_data <= X"F80";
                when "0111010111001111111" => write_camera_data <= X"F80";
                when "0111010111010000000" => write_camera_data <= X"F80";
                when "0111011000011111111" => write_camera_data <= X"F80";
                when "0111011000100000000" => write_camera_data <= X"F80";
                when "0111011001101111111" => write_camera_data <= X"F80";
                when "0111011001110000000" => write_camera_data <= X"F80";
                when "0111011010111111111" => write_camera_data <= X"F80";
                when "0111011011000000000" => write_camera_data <= X"F80";
                when "0111011100001111111" => write_camera_data <= X"F80";
                when "0111011100010000000" => write_camera_data <= X"F80";
                when "0111011101011111111" => write_camera_data <= X"F80";
                when "0111011101100000000" => write_camera_data <= X"F80";
                when "0111011110101111111" => write_camera_data <= X"F80";
                when "0111011110110000000" => write_camera_data <= X"F80";
                when "0111011111111111111" => write_camera_data <= X"F80";
                when "0111100000000000000" => write_camera_data <= X"F80";
                when "0111100001001111111" => write_camera_data <= X"F80";
                when "0111100001010000000" => write_camera_data <= X"F80";
                when "0111100010011111111" => write_camera_data <= X"F80";
                when "0111100010100000000" => write_camera_data <= X"F80";
                when "0111100011101111111" => write_camera_data <= X"F80";
                when "0111100011110000000" => write_camera_data <= X"F80";
                when "0111100100111111111" => write_camera_data <= X"F80";
                when "0111100101000000000" => write_camera_data <= X"F80";
                when "0111100110001111111" => write_camera_data <= X"F80";
                when "0111100110010000000" => write_camera_data <= X"F80";
                when "0111100111011111111" => write_camera_data <= X"F80";
                when "0111100111100000000" => write_camera_data <= X"F80";
                when "0111101000101111111" => write_camera_data <= X"F80";
                when "0111101000110000000" => write_camera_data <= X"F80";
                when "0111101001111111111" => write_camera_data <= X"F80";
                when "0111101010000000000" => write_camera_data <= X"F80";
                when "0111101011001111111" => write_camera_data <= X"F80";
                when "0111101011010000000" => write_camera_data <= X"F80";
                when "0111101100011111111" => write_camera_data <= X"F80";
                when "0111101100100000000" => write_camera_data <= X"F80";
                when "0111101101101111111" => write_camera_data <= X"F80";
                when "0111101101110000000" => write_camera_data <= X"F80";
                when "0111101110111111111" => write_camera_data <= X"F80";
                when "0111101111000000000" => write_camera_data <= X"F80";
                when "0111110000001111111" => write_camera_data <= X"F80";
                when "0111110000010000000" => write_camera_data <= X"F80";
                when "0111110001011111111" => write_camera_data <= X"F80";
                when "0111110001100000000" => write_camera_data <= X"F80";
                when "0111110010101111111" => write_camera_data <= X"F80";
                when "0111110010110000000" => write_camera_data <= X"F80";
                when "0111110011111111111" => write_camera_data <= X"F80";
                when "0111110100000000000" => write_camera_data <= X"F80";
                when "0111110101001111111" => write_camera_data <= X"F80";
                when "0111110101010000000" => write_camera_data <= X"F80";
                when "0111110110011111111" => write_camera_data <= X"F80";
                when "0111110110100000000" => write_camera_data <= X"F80";
                when "0111110111101111111" => write_camera_data <= X"F80";
                when "0111110111110000000" => write_camera_data <= X"F80";
                when "0111111000111111111" => write_camera_data <= X"F80";
                when "0111111001000000000" => write_camera_data <= X"F80";
                when "0111111010001111111" => write_camera_data <= X"F80";
                when "0111111010010000000" => write_camera_data <= X"F80";
                when "0111111011011111111" => write_camera_data <= X"F80";
                when "0111111011100000000" => write_camera_data <= X"F80";
                when "0111111100101111111" => write_camera_data <= X"F80";
                when "0111111100110000000" => write_camera_data <= X"F80";
                when "0111111101111111111" => write_camera_data <= X"F80";
                when "0111111110000000000" => write_camera_data <= X"F80";
                when "0111111111001111111" => write_camera_data <= X"F80";
                when "0111111111010000000" => write_camera_data <= X"F80";
                when "1000000000011111111" => write_camera_data <= X"F80";
                when "1000000000100000000" => write_camera_data <= X"F80";
                when "1000000001101111111" => write_camera_data <= X"F80";
                when "1000000001110000000" => write_camera_data <= X"F80";
                when "1000000010111111111" => write_camera_data <= X"F80";
                when "1000000011000000000" => write_camera_data <= X"F80";
                when "1000000100001111111" => write_camera_data <= X"F80";
                when "1000000100010000000" => write_camera_data <= X"F80";
                when "1000000101011111111" => write_camera_data <= X"F80";
                when "1000000101100000000" => write_camera_data <= X"F80";
                when "1000000110101111111" => write_camera_data <= X"F80";
                when "1000000110110000000" => write_camera_data <= X"F80";
                when "1000000111111111111" => write_camera_data <= X"F80";
                when "1000001000000000000" => write_camera_data <= X"F80";
                when "1000001001001111111" => write_camera_data <= X"F80";
                when "1000001001010000000" => write_camera_data <= X"F80";
                when "1000001010011111111" => write_camera_data <= X"F80";
                when "1000001010100000000" => write_camera_data <= X"F80";
                when "1000001011101111111" => write_camera_data <= X"F80";
                when "1000001011110000000" => write_camera_data <= X"F80";
                when "1000001100111111111" => write_camera_data <= X"F80";
                when "1000001101000000000" => write_camera_data <= X"F80";
                when "1000001110001111111" => write_camera_data <= X"F80";
                when "1000001110010000000" => write_camera_data <= X"F80";
                when "1000001111011111111" => write_camera_data <= X"F80";
                when "1000001111100000000" => write_camera_data <= X"F80";
                when "1000010000101111111" => write_camera_data <= X"F80";
                when "1000010000110000000" => write_camera_data <= X"F80";
                when "1000010001111111111" => write_camera_data <= X"F80";
                when "1000010010000000000" => write_camera_data <= X"F80";
                when "1000010011001111111" => write_camera_data <= X"F80";
                when "1000010011010000000" => write_camera_data <= X"F80";
                when "1000010100011111111" => write_camera_data <= X"F80";
                when "1000010100100000000" => write_camera_data <= X"F80";
                when "1000010101101111111" => write_camera_data <= X"F80";
                when "1000010101110000000" => write_camera_data <= X"F80";
                when "1000010110111111111" => write_camera_data <= X"F80";
                when "1000010111000000000" => write_camera_data <= X"F80";
                when "1000011000001111111" => write_camera_data <= X"F80";
                when "1000011000010000000" => write_camera_data <= X"F80";
                when "1000011001011111111" => write_camera_data <= X"F80";
                when "1000011001100000000" => write_camera_data <= X"F80";
                when "1000011010101111111" => write_camera_data <= X"F80";
                when "1000011010110000000" => write_camera_data <= X"F80";
                when "1000011011111111111" => write_camera_data <= X"F80";
                when "1000011100000000000" => write_camera_data <= X"F80";
                when "1000011101001111111" => write_camera_data <= X"F80";
                when "1000011101010000000" => write_camera_data <= X"F80";
                when "1000011110011111111" => write_camera_data <= X"F80";
                when "1000011110100000000" => write_camera_data <= X"F80";
                when "1000011111101111111" => write_camera_data <= X"F80";
                when "1000011111110000000" => write_camera_data <= X"F80";
                when "1000100000111111111" => write_camera_data <= X"F80";
                when "1000100001000000000" => write_camera_data <= X"F80";
                when "1000100010001111111" => write_camera_data <= X"F80";
                when "1000100010010000000" => write_camera_data <= X"F80";
                when "1000100011011111111" => write_camera_data <= X"F80";
                when "1000100011100000000" => write_camera_data <= X"F80";
                when "1000100100101111111" => write_camera_data <= X"F80";
                when "1000100100110000000" => write_camera_data <= X"F80";
                when "1000100101111111111" => write_camera_data <= X"F80";
                when "1000100110000000000" => write_camera_data <= X"F80";
                when "1000100111001111111" => write_camera_data <= X"F80";
                when "1000100111010000000" => write_camera_data <= X"F80";
                when "1000101000011111111" => write_camera_data <= X"F80";
                when "1000101000100000000" => write_camera_data <= X"F80";
                when "1000101001101111111" => write_camera_data <= X"F80";
                when "1000101001110000000" => write_camera_data <= X"F80";
                when "1000101010111111111" => write_camera_data <= X"F80";
                when "1000101011000000000" => write_camera_data <= X"F80";
                when "1000101100001111111" => write_camera_data <= X"F80";
                when "1000101100010000000" => write_camera_data <= X"F80";
                when "1000101101011111111" => write_camera_data <= X"F80";
                when "1000101101100000000" => write_camera_data <= X"F80";
                when "1000101110101111111" => write_camera_data <= X"F80";
                when "1000101110110000000" => write_camera_data <= X"F80";
                when "1000101111111111111" => write_camera_data <= X"F80";
                when "1000110000000000000" => write_camera_data <= X"F80";
                when "1000110001001111111" => write_camera_data <= X"F80";
                when "1000110001010000000" => write_camera_data <= X"F80";
                when "1000110010011111111" => write_camera_data <= X"F80";
                when "1000110010100000000" => write_camera_data <= X"F80";
                when "1000110011101111111" => write_camera_data <= X"F80";
                when "1000110011110000000" => write_camera_data <= X"F80";
                when "1000110100111111111" => write_camera_data <= X"F80";
                when "1000110101000000000" => write_camera_data <= X"F80";
                when "1000110110001111111" => write_camera_data <= X"F80";
                when "1000110110010000000" => write_camera_data <= X"F80";
                when "1000110111011111111" => write_camera_data <= X"F80";
                when "1000110111100000000" => write_camera_data <= X"F80";
                when "1000111000101111111" => write_camera_data <= X"F80";
                when "1000111000110000000" => write_camera_data <= X"F80";
                when "1000111001111111111" => write_camera_data <= X"F80";
                when "1000111010000000000" => write_camera_data <= X"F80";
                when "1000111011001111111" => write_camera_data <= X"F80";
                when "1000111011010000000" => write_camera_data <= X"F80";
                when "1000111100011111111" => write_camera_data <= X"F80";
                when "1000111100100000000" => write_camera_data <= X"F80";
                when "1000111101101111111" => write_camera_data <= X"F80";
                when "1000111101110000000" => write_camera_data <= X"F80";
                when "1000111110111111111" => write_camera_data <= X"F80";
                when "1000111111000000000" => write_camera_data <= X"F80";
                when "1001000000001111111" => write_camera_data <= X"F80";
                when "1001000000010000000" => write_camera_data <= X"F80";
                when "1001000001011111111" => write_camera_data <= X"F80";
                when "1001000001100000000" => write_camera_data <= X"F80";
                when "1001000010101111111" => write_camera_data <= X"F80";
                when "1001000010110000000" => write_camera_data <= X"F80";
                when "1001000011111111111" => write_camera_data <= X"F80";
                when "1001000100000000000" => write_camera_data <= X"F80";
                when "1001000101001111111" => write_camera_data <= X"F80";
                when "1001000101010000000" => write_camera_data <= X"F80";
                when "1001000110011111111" => write_camera_data <= X"F80";
                when "1001000110100000000" => write_camera_data <= X"F80";
                when "1001000111101111111" => write_camera_data <= X"F80";
                when "1001000111110000000" => write_camera_data <= X"F80";
                when "1001001000111111111" => write_camera_data <= X"F80";
                when "1001001001000000000" => write_camera_data <= X"F80";
                when "1001001010001111111" => write_camera_data <= X"F80";
                when "1001001010010000000" => write_camera_data <= X"F80";
                when "1001001011011111111" => write_camera_data <= X"F80";
                when "1001001011100000000" => write_camera_data <= X"F80";
                when "1001001100101111111" => write_camera_data <= X"F80";
                when "1001001100110000000" => write_camera_data <= X"F80";
                when "1001001101111111111" => write_camera_data <= X"F80";
                when "1001001110000000000" => write_camera_data <= X"F80";
                when "1001001111001111111" => write_camera_data <= X"F80";
                when "1001001111010000000" => write_camera_data <= X"F80";
                when "1001010000011111111" => write_camera_data <= X"F80";
                when "1001010000100000000" => write_camera_data <= X"F80";
                when "1001010001101111111" => write_camera_data <= X"F80";
                when "1001010001110000000" => write_camera_data <= X"F80";
                when "1001010010111111111" => write_camera_data <= X"F80";
                when "1001010011000000000" => write_camera_data <= X"F80";
                when "1001010100001111111" => write_camera_data <= X"F80";
                when "1001010100010000000" => write_camera_data <= X"F80";
                when "1001010101011111111" => write_camera_data <= X"F80";
                when "1001010101100000000" => write_camera_data <= X"F80";
                when "1001010110101111111" => write_camera_data <= X"F80";
				-- Middle
                when others => write_camera_data <= X"808";
			end case;
		end if;
	end process;

end Behavioral;
