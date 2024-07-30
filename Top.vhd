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

-- VGA Controller
--      Output	-	o_hsync: VGA Horizontal Synchronization
--      Output	-	o_vsync: VGA Vertical Synchronization
--      Output	-	o_vga_red: VGA Red Signal
--      Output	-	o_vga_green: VGA Green Signal
--      Output	-	o_vga_blue: VGA Blue Signal

-- OV7670 Camera
--		Input 	-	i_ov7670_vsync: OV7670 Vertical Synchronization ('0': No Image, '1': Active Image)
--		Output 	-	o_ov7670_write_image_reset: OV7670 FIFO Image Write Reset
--		Output 	-	o_ov7670_write_image_enable: OV7670 FIFO Image Write Enable
--		Output 	-	o_ov7670_read_clock: OV7670 FIFO Image Read Clock
--		Output 	-	o_ov7670_read_image_reset: OV7670 FIFO Image Read Reset
--		Output 	-	o_ov7670_read_image_enable: OV7670 FIFO Image Read Enable
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
	i_filter_mode: IN STD_LOGIC;
	-- LEDs
	o_leds: OUT STD_LOGIC_VECTOR(15 downto 0);
	-- VGA
	o_hsync: OUT STD_LOGIC;
	o_vsync: OUT STD_LOGIC;
    o_vga_red: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_vga_green: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_vga_blue: OUT STD_LOGIC_VECTOR(3 downto 0);
    -- OV7670 Sensor
    i_ov7670_vsync: IN STD_LOGIC;
    o_ov7670_write_image_reset: OUT STD_LOGIC;
    o_ov7670_write_image_enable: OUT STD_LOGIC;
    o_ov7670_read_clock: OUT STD_LOGIC;
    o_ov7670_read_image_reset: OUT STD_LOGIC;
    o_ov7670_read_image_enable: OUT STD_LOGIC;
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
		i_sync: IN STD_LOGIC;
		i_image_data_empty: IN STD_LOGIC;
		i_image_data: IN STD_LOGIC_VECTOR(11 downto 0);
		o_next_image_data: OUT STD_LOGIC;
		o_reset_image_data: OUT STD_LOGIC;
		o_hsync: OUT STD_LOGIC;
		o_vsync: OUT STD_LOGIC;
		o_vga_red: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_vga_green: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_vga_blue: OUT STD_LOGIC_VECTOR(3 downto 0)
	);
END COMPONENT;

COMPONENT fifo_camera IS
    PORT (
        wr_clk : IN STD_LOGIC;
        wr_rst : IN STD_LOGIC;
        rd_clk : IN STD_LOGIC;
        rd_rst : IN STD_LOGIC;
        din : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        wr_en : IN STD_LOGIC;
        rd_en : IN STD_LOGIC;
        dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        full : OUT STD_LOGIC;
        empty : OUT STD_LOGIC
	);
END COMPONENT;

COMPONENT ov7670_fifo_clock_gen is
	PORT(
		clk_out1: OUT STD_LOGIC;
		clk_in1: IN STD_LOGIC
	);
END COMPONENT;

COMPONENT ImageFilter is
	PORT(
		i_pixel_clock_148M: IN STD_LOGIC;
		i_filter_mode: IN STD_LOGIC;
		i_image_to_filter_data: IN STD_LOGIC_VECTOR(11 downto 0);
		o_image_to_filter_addr: OUT STD_LOGIC_VECTOR(18 downto 0);
		o_filtered_image_write_enable: OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
		o_filtered_image_addr: OUT STD_LOGIC_VECTOR(18 downto 0);
		o_filtered_image_data: OUT STD_LOGIC_VECTOR(11 downto 0)
	);
END COMPONENT;

COMPONENT OV7670FifoController is
	PORT(
		i_fifo_read_clock: IN STD_LOGIC;
		-- OV7670 Synchronizations
		i_ov7670_vsync: IN STD_LOGIC;
		-- OV7670 Embedded FIFO Write Controller
		o_ov7670_fifo_write_reset: OUT STD_LOGIC;
		o_ov7670_fifo_write_enable: OUT STD_LOGIC;
		-- OV7670 Embedded FIFO Read Controller
		o_ov7670_fifo_read_clock: OUT STD_LOGIC;
		i_ov7670_fifo_read_restart_trigger: IN STD_LOGIC;
		o_ov7670_fifo_read_reset: OUT STD_LOGIC;
		o_ov7670_fifo_read_enable: OUT STD_LOGIC;
		i_ov7670_fifo_read_data: IN STD_LOGIC_VECTOR(7 downto 0);
		-- Image Data Ouput
		o_new_image_trigger: OUT STD_LOGIC;
		o_new_image_data_enable: OUT STD_LOGIC;
		o_new_image_data: OUT STD_LOGIC_VECTOR(7 downto 0)
		-- OV7670 Configuration (TODO)
		--o_ov7670_scl: OUT STD_LOGIC;
		--io_ov7670_sda: INOUT STD_LOGIC
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

-- Filter Mode SW Debouncer
signal debounced_filter_mode: STD_LOGIC := '0';

-- VGA Controller
signal pixel_clock_148M: STD_LOGIC := '0';
signal synchronized_vga_reset: STD_LOGIC := '0';

-- FIFO Camera 0
signal fifo_write_reset: STD_LOGIC := '0';
signal fifo_write_enable: STD_LOGIC := '0';
signal fifo_write_data: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal fifo_write_data_full: STD_LOGIC := '0';
signal fifo_read_reset: STD_LOGIC := '0';
signal fifo_read_enable: STD_LOGIC := '0';
signal fifo_read_data: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal fifo_read_data_empty: STD_LOGIC := '0';

-- OV7670 Controller
signal ov7670_fifo_clock_43M: STD_LOGIC := '0';
signal synchronized_ov7670_fifo_read_reset: STD_LOGIC := '0';

-- Image Simulation
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

	-----------------------------
	-- Debouncer - i_reset_btn --
	-----------------------------
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

	------------------------------------------------
	-- Synchronized (VGA Reset) - debounced_reset --
	------------------------------------------------
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
		i_sync => i_ov7670_vsync,
		i_image_data_empty => fifo_read_data_empty,
		i_image_data => fifo_read_data,
		o_next_image_data => fifo_read_enable,
        o_reset_image_data => fifo_read_reset,
		o_hsync => o_hsync,
		o_vsync => o_vsync,
		o_vga_red => o_vga_red,
		o_vga_green => o_vga_green,
		o_vga_blue => o_vga_blue);

	-------------------------------
	-- Debouncer - i_filter_mode --
	-------------------------------
	inst_debouncerFilterMode : Debouncer generic map (DEBOUNCE_COUNTER_SIZE => 20) port map (
		i_clock => i_clock_100,
		i_input => i_filter_mode,
		o_output => debounced_filter_mode);

	------------------
	-- Image Filter --
	------------------
	-- inst_imageFilter : ImageFilter port map (
	-- 	i_pixel_clock_148M => pixel_clock_148M,
	-- 	i_filter_mode => debounced_filter_mode,
	-- 	i_image_to_filter_data => write_camera_data,
	-- 	o_image_to_filter_addr => write_camera_data_addr,
	-- 	o_filtered_image_write_enable => ram_write_enable,
	-- 	o_filtered_image_addr => ram_write_addr,
	-- 	o_filtered_image_data => ram_write_data);

	-------------------
	-- FIFO Camera 0 --
	-------------------
    inst_fifoCamera0 : fifo_camera port map (
        wr_clk => ov7670_fifo_clock_43M,
        wr_rst => fifo_write_reset,
        rd_clk => pixel_clock_148M,
        rd_rst => fifo_read_reset,
        din => fifo_write_data,
        wr_en => fifo_write_enable,
        rd_en => fifo_read_enable,
        dout => fifo_read_data,
        full => fifo_write_data_full,
        empty => fifo_read_data_empty);

    -----------------------------------------
	-- OV7670 FIFO Image Clock (43.20 MHz) --
	-----------------------------------------
	inst_ov7670FifoClock : ov7670_fifo_clock_gen port map (clk_out1 => ov7670_fifo_clock_43M, clk_in1 => pixel_clock_148M);

	--------------------------------------------------------
	-- Synchronized (OV7670 Reset Read) - fifo_read_reset --
	--------------------------------------------------------
	inst_synchronizerOV7670ReadReset : Synchronizer port map (
		i_domain_clock => ov7670_fifo_clock_43M,
		i_input => fifo_read_reset,
		o_output => synchronized_ov7670_fifo_read_reset);

	--------------------------------------------
	-- OV7670 Controller (with embedded FIFO) --
	--------------------------------------------
	inst_OV7670FifoController : OV7670FifoController port map (
        i_fifo_read_clock => ov7670_fifo_clock_43M,
        i_ov7670_vsync => i_ov7670_vsync,
        o_ov7670_fifo_write_reset => o_ov7670_write_image_reset,
        o_ov7670_fifo_write_enable => o_ov7670_write_image_enable,
        o_ov7670_fifo_read_clock => o_ov7670_read_clock,
        i_ov7670_fifo_read_restart_trigger => synchronized_ov7670_fifo_read_reset,
        o_ov7670_fifo_read_reset => o_ov7670_read_image_reset,
        o_ov7670_fifo_read_enable => o_ov7670_read_image_enable,
        i_ov7670_fifo_read_data => i_ov7670_read_image_data,
        o_new_image_trigger => fifo_write_reset,
        o_new_image_data_enable => fifo_write_enable,
        o_new_image_data => fifo_write_data(11 downto 4));

	----------------------
	-- Image Simulation --
	----------------------
    -- inst_imageSimulation : ImageSimulation port map (
    --     i_clock => ov7670_fifo_clock_43M,
    --     i_reset => image_simulation_reset,
    --     i_enable => image_simulation_enable,
    --     o_image_data => image_simulation_data);

end Behavioral;
