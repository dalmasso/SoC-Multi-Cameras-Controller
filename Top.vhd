------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: Top Entity
-- Description:
--      Top Entity
--		Input 	-	i_clock_100: Clock (100MHz)
--		Input 	-	i_reset_btn: Reset (active high)
--		Input 	-	i_filter_restart: Restart Filter (active high)
--		Input 	-	i_filter_mode: Filter Modes Selector
--						'0': Filter 0 (Default: Pass-Through)
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

ENTITY Top is
PORT(
	i_clock_100: IN STD_LOGIC;
	i_reset_btn: IN STD_LOGIC;
	-- Image Filter
	i_filter_restart: IN STD_LOGIC;
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

COMPONENT ChainedLed is
	PORT(
		i_clock_100: IN STD_LOGIC;
		i_clock_enable: IN STD_LOGIC;
		i_reset: IN STD_LOGIC;
		o_leds: OUT STD_LOGIC_VECTOR(15 downto 0)
	);
END COMPONENT;

COMPONENT VGAController is
	PORT(
		i_clock_100: IN STD_LOGIC;
		i_reset: IN STD_LOGIC;
		i_filter_restart: IN STD_LOGIC;
		i_filter_mode: IN STD_LOGIC;
		o_hsync: OUT STD_LOGIC;
		o_vsync: OUT STD_LOGIC;
		o_vga_red: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_vga_green: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_vga_blue: OUT STD_LOGIC_VECTOR(3 downto 0)
	);
END COMPONENT;

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Clock Tick Generator Outputs
signal clock_tick_out_1: STD_LOGIC := '0';
signal clock_tick_out_25M: STD_LOGIC := '0';

-- Reset BTN Inverter
signal inv_reset_btn: STD_LOGIC := '0';

-- Debouncer Output
signal debouncer_out: STD_LOGIC := '0';

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

	---------------
	-- Debouncer --
	---------------
	inv_reset_btn <= not(i_reset_btn);
	inst_debouncer : Debouncer generic map (DEBOUNCE_COUNTER_SIZE => 20) port map (
		i_clock => i_clock_100,
		i_input => inv_reset_btn,
		o_output => debouncer_out);

	------------------
	-- Chained LEDs --
	------------------
	inst_chainedLEDs : ChainedLed port map (
		i_clock_100 => i_clock_100,
		i_clock_enable => clock_tick_out_1,
		i_reset => debouncer_out,
		o_leds => o_leds);

	--------------------
	-- VGA Controller --
	--------------------
	inst_vgaController : VGAController port map (
		i_clock_100 => i_clock_100,
		i_reset => debouncer_out,
		i_filter_restart => i_filter_restart,
		i_filter_mode => i_filter_mode,
		o_hsync => o_hsync,
		o_vsync => o_vsync,
		o_vga_red => o_vga_red,
		o_vga_green => o_vga_green,
		o_vga_blue => o_vga_blue);

end Behavioral;
