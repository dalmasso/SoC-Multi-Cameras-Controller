------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: Top Entity
-- Description:
--      Top Entity
--		Input 	-	i_clock_100: Clock (100MHz)
--		Input 	-	i_reset_btn: Reset ('0': NO Reset, '1': Reset)
--		Input 	-	i_filter_mode: Select Filter Modes
--						"00": Filter 0 (GrayScale)
--						"01": Filter 1 (RGB)
--						"10": Filter 2 (Default: Laplacian)
--						"11": Filter 3 (Default: Laplacian)

-- Chained LEDs
--      Output	-	o_leds: 16 LEDs

-- VGA Controller
--      Output	-	o_vsync: VGA Vertical Synchronization
--      Output	-	o_hsync: VGA Horizontal Synchronization
--      Output	-	o_vga_red: VGA Red Signal
--      Output	-	o_vga_green: VGA Green Signal
--      Output	-	o_vga_blue: VGA Blue Signal

-- OV7670 Camera
--		Input 	-	i_ov7670_vsync: OV7670 Vertical Synchronization ('0': No Image, '1': Active Image)
--		Input 	-	i_ov7670_href: OV7670 Horizontal Synchronization ('0': No Image Data, '1': Image Data available)
--		Output 	-	o_ov7670_scl: OV7670 Configuration - Serial Interface Clock
--		Output 	-	o_ov7670_sda: OV7670 Configuration - Serial Interface Data
--		Output 	-	o_ov7670_reset: OV7670 Configuration - Reset All Registers ('0': Reset, '1': No Reset)
--		Output 	-	o_ov7670_write_image_reset: OV7670 FIFO Image Write Reset
--		Output 	-	o_ov7670_read_clock: OV7670 FIFO Image Read Clock
--		Output 	-	o_ov7670_read_image_reset: OV7670 FIFO Image Read Reset
--		Input 	-	i_ov7670_read_image_data: OV7670 FIFO Image Data (Default Format: YUV 4:2:2, Sequence: U0 Y0 V0 Y1)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY Top is
PORT(
    i_clock_100: IN STD_LOGIC;
	i_reset_btn: IN STD_LOGIC;
	-- Image Filter
	i_filter_mode: IN STD_LOGIC_VECTOR(1 downto 0);
	-- LEDs
	o_leds: OUT STD_LOGIC_VECTOR(15 downto 0);
	-- VGA
	o_vsync: OUT STD_LOGIC;
	o_hsync: OUT STD_LOGIC;
    o_vga_red: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_vga_green: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_vga_blue: OUT STD_LOGIC_VECTOR(3 downto 0);
    -- OV7670 Sensor
    i_ov7670_vsync: IN STD_LOGIC;
	i_ov7670_href: IN STD_LOGIC;
	o_ov7670_scl: OUT STD_LOGIC;
	o_ov7670_sda: OUT STD_LOGIC;
	o_ov7670_reset: OUT STD_LOGIC;
    o_ov7670_write_image_reset: OUT STD_LOGIC;
    o_ov7670_read_clock: OUT STD_LOGIC;
    o_ov7670_read_image_reset: OUT STD_LOGIC;
    i_ov7670_read_image_data: IN STD_LOGIC_VECTOR(7 downto 0)
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
		o_read_next_image_data: OUT STD_LOGIC;
		i_image_data: IN STD_LOGIC_VECTOR(11 downto 0);
		o_hsync: OUT STD_LOGIC;
		o_vsync: OUT STD_LOGIC;
		o_vga_red: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_vga_green: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_vga_blue: OUT STD_LOGIC_VECTOR(3 downto 0)
	);
END COMPONENT;

COMPONENT ImageFilter is
	PORT(
		i_image_data_clock: IN STD_LOGIC;
		i_image_data_enable: IN STD_LOGIC;
		i_image_data: IN STD_LOGIC_VECTOR(7 downto 0);
		i_filter_mode: IN STD_LOGIC_VECTOR(1 downto 0);
		i_pixel_clock: IN STD_LOGIC;
		i_read_reset: IN STD_LOGIC;
		i_read_pixel_data: IN STD_LOGIC;
		o_pixel_data: OUT STD_LOGIC_VECTOR(11 downto 0)
	);
END COMPONENT;

COMPONENT OV7670FifoController is
	PORT(
		i_clock_100: IN STD_LOGIC;
		i_reset: IN STD_LOGIC;
		i_ov7670_vsync: IN STD_LOGIC;
		i_ov7670_href: IN STD_LOGIC;
		o_ov7670_scl: OUT STD_LOGIC;
		o_ov7670_sda: OUT STD_LOGIC;
		o_ov7670_reset: OUT STD_LOGIC;
		o_ov7670_fifo_write_reset: OUT STD_LOGIC;
		o_ov7670_fifo_read_clock: OUT STD_LOGIC;
		o_ov7670_fifo_read_reset: OUT STD_LOGIC;
		i_ov7670_fifo_read_data: IN STD_LOGIC_VECTOR(7 downto 0);
		o_image_output_clock: OUT STD_LOGIC;
		o_image_output_data_enable: OUT STD_LOGIC;
		o_image_output_data: OUT STD_LOGIC_VECTOR(7 downto 0)
	);
END COMPONENT;

COMPONENT ImageSimulation is
    PORT(
		i_clock: IN STD_LOGIC;
		i_reset: IN STD_LOGIC;
		i_enable: IN STD_LOGIC;
		o_image_data: OUT STD_LOGIC_VECTOR(11 downto 0)
    );
END COMPONENT;

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Clock Tick Generator Outputs
signal clock_tick_out_1: STD_LOGIC := '0';
signal clock_tick_out_25M: STD_LOGIC := '0';

-- Reset BTN Debouncer
signal debounced_reset: STD_LOGIC := '0';

-- VGA Controller
signal pixel_clock_148M: STD_LOGIC := '0';
signal synchronized_vga_reset: STD_LOGIC := '0';
signal read_next_image_data: STD_LOGIC := '0';

-- Image Filter
signal debounced_filter_mode: STD_LOGIC_VECTOR(1 downto 0) := (others => '0');
signal synchronized_filter_mode: STD_LOGIC_VECTOR(1 downto 0) := (others => '0');
signal filtered_image_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

-- OV7670 Controller
signal image_data_clock: STD_LOGIC := '0';
signal image_data_enable: STD_LOGIC := '0';
signal image_data: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');

-- Image Simulation
-- signal image_simulation_clock: STD_LOGIC := '0';
-- signal image_simulation_reset: STD_LOGIC := '0';
-- signal image_simulation_enable: STD_LOGIC := '0';
-- signal image_simulation_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

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

	----------------------------
	-- Reset Button Debouncer --
	----------------------------
	inst_debouncerResetBtn : Debouncer generic map (DEBOUNCE_COUNTER_SIZE => 20) port map (
		i_clock => i_clock_100,
		i_input => i_reset_btn,
		o_output => debounced_reset);

	------------------
	-- Chained LEDs --
	------------------
	inst_chainedLEDs : ChainedLed port map (
		i_clock_100 => i_clock_100,
		i_clock_enable => clock_tick_out_1,
		i_reset => debounced_reset,
		o_leds => o_leds);

	---------------------------------
	-- VGA Pixel Clock (148.4 MHz) --
	---------------------------------
	inst_vgaPixelClock : vga_pixel_clock port map (clk_out1 => pixel_clock_148M, clk_in1 => i_clock_100);

	----------------------------
	-- VGA Reset Synchronizer --
	----------------------------
	inst_synchronizerVGAReset : Synchronizer port map (
		i_domain_clock => pixel_clock_148M,
		i_input => debounced_reset,
		o_output => synchronized_vga_reset);

	--------------------
	-- VGA Controller --
	--------------------
	inst_vgaController : VGAController port map (
		i_pixel_clock => pixel_clock_148M,
		i_reset => synchronized_vga_reset,
		o_read_next_image_data => read_next_image_data,
		i_image_data => filtered_image_data,
		o_hsync => o_hsync,
		o_vsync => o_vsync,
		o_vga_red => o_vga_red,
		o_vga_green => o_vga_green,
		o_vga_blue => o_vga_blue);

	----------------------------
	-- Filter Mode Debouncers --
	----------------------------
	generate_debouncerFilterModes: for i in 0 to 1 generate
		inst_debouncerFilterModes : Debouncer generic map (DEBOUNCE_COUNTER_SIZE => 20) port map (
			i_clock => i_clock_100,
			i_input => i_filter_mode(i),
			o_output => debounced_filter_mode(i));
  	end generate generate_debouncerFilterModes;

	-------------------------------
	-- Filter Mode Synchronizers --
	-------------------------------
	generate_synchronizerFilterModes: for i in 0 to 1 generate
		inst_synchronizerFilterModes : Synchronizer port map (
			i_domain_clock => image_data_clock,
			i_input => debounced_filter_mode(i),
			o_output => synchronized_filter_mode(i));
	end generate generate_synchronizerFilterModes;

	------------------
	-- Image Filter --
	------------------
	inst_imageFilter : ImageFilter port map (
		i_image_data_clock => image_data_clock,
		i_image_data_enable => image_data_enable,
		i_image_data => image_data,
		i_filter_mode => synchronized_filter_mode,
		i_pixel_clock => pixel_clock_148M,
		i_read_reset => synchronized_vga_reset,
		i_read_pixel_data => read_next_image_data,
		o_pixel_data => filtered_image_data);

	--------------------------------------------
	-- OV7670 Controller (with embedded FIFO) --
	--------------------------------------------
	inst_OV7670FifoController : OV7670FifoController port map (
        i_clock_100 => i_clock_100,
		i_reset => debounced_reset,
        i_ov7670_vsync => i_ov7670_vsync,
		i_ov7670_href => i_ov7670_href,
		o_ov7670_scl => o_ov7670_scl,
		o_ov7670_sda => o_ov7670_sda,
		o_ov7670_reset => o_ov7670_reset,
        o_ov7670_fifo_write_reset => o_ov7670_write_image_reset,
        o_ov7670_fifo_read_clock => o_ov7670_read_clock,
        o_ov7670_fifo_read_reset => o_ov7670_read_image_reset,
        i_ov7670_fifo_read_data => i_ov7670_read_image_data,
		o_image_output_clock => image_data_clock,
		o_image_output_data_enable => image_data_enable,
		o_image_output_data => image_data);

	----------------------
	-- Image Simulation --
	----------------------
    -- inst_imageSimulation : ImageSimulation port map (
    --     i_clock => image_simulation_clock,
    --     i_reset => image_simulation_reset,
    --     i_enable => image_simulation_enable,
    --     o_image_data => image_simulation_data);

end Behavioral;