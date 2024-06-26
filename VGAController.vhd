------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 21/06/2024
-- Module Name: VGAController
-- Description:
--      VGA Controller
--		Input 	-	i_clock_100: Clock (100MHz)
--		Input 	-	i_reset: Reset (active low)
--		Input 	-	i_filter_mode_sw: Filter Mode Selector
--      Output	-	o_hsync: VGA Horizontal Synchronization
--      Output	-	o_vsync: VGA Vertical Synchronization
--      Output	-	o_vga_red: VGA Red Signal
--      Output	-	o_vga_green: VGA Green Signal
--      Output	-	o_vga_blue: VGA Blue Signal
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY VGAController is
PORT(
	i_clock_100: IN STD_LOGIC;
    i_reset: IN STD_LOGIC;
	i_filter_mode_sw: IN STD_LOGIC_VECTOR(0 downto 0);
	o_hsync: OUT STD_LOGIC;
	o_vsync: OUT STD_LOGIC;
    o_vga_red: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_vga_green: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_vga_blue: OUT STD_LOGIC_VECTOR(3 downto 0)
);
END VGAController;

ARCHITECTURE Behavioral of VGAController is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------
COMPONENT clk_wiz_0 is
	PORT(
		clk_out1: OUT STD_LOGIC;
		locked: OUT STD_LOGIC;
		clk_in1: IN STD_LOGIC
	);
END COMPONENT;

COMPONENT Synchronizer is
	PORT(
		i_domain_clock: IN STD_LOGIC;
		i_input: IN STD_LOGIC;
		o_output: OUT STD_LOGIC
	);
END COMPONENT;

COMPONENT VGADisplay is
	PORT(
		i_enable: IN STD_LOGIC;
		i_data: IN STD_LOGIC_VECTOR(11 downto 0);
		o_red: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_green: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_blue: OUT STD_LOGIC_VECTOR(3 downto 0)
	);
END COMPONENT;

COMPONENT ImageFilter is
	PORT(
		i_pixel_clock: IN STD_LOGIC;
		i_filter_mode_sw: IN STD_LOGIC_VECTOR(0 downto 0);
		i_image_to_filter: IN STD_LOGIC_VECTOR(11 downto 0);
		o_image_to_filter_addr: OUT STD_LOGIC_VECTOR(15 downto 0);
		o_filtered_image_write_enable: OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
		o_filtered_image_addr: OUT STD_LOGIC_VECTOR(15 downto 0);
		o_filtered_image_data: OUT STD_LOGIC_VECTOR(11 downto 0)
	);
END COMPONENT;

-- ROM
COMPONENT blk_mem_gen_0 IS
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

-- RAM Dual Port
COMPONENT blk_mem_gen_1 IS
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- Horizontal Front Porch: 88 pixels
constant HFP: UNSIGNED(7 downto 0) := X"58";

-- Horizontal Back Porch: 148 pixels
constant HBP: UNSIGNED(7 downto 0) := X"94";

-- Horizontal Sync Width: 44 pixels
constant HSW: UNSIGNED(7 downto 0) := X"2C";

-- Horizontal Pixels (2200 per line)
constant HPIXELS: UNSIGNED(11 downto 0) := X"898";

-- Vertical Front Porch: 4 lines
constant VFP: UNSIGNED(3 downto 0) := X"4";

-- Vertical Back Porch: 36 lines
constant VBP: UNSIGNED(7 downto 0) := X"24";

-- Vertical Sync Width: 5 lines
constant VSW: UNSIGNED(3 downto 0) := X"5";

-- Vertical Lines (1125 per line)
constant VLINES: UNSIGNED(11 downto 0) := X"465";

-- VGA Image Width (240 pixel) & Heigh (160 pixels)
constant IMAGE_WIDTH: UNSIGNED(11 downto 0) := X"0F0";
constant IMAGE_HEIGH: UNSIGNED(11 downto 0) := X"0A0";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Synchronizer
signal synchronized_reset: STD_LOGIC := '0';

-- Horizontal Counter
signal h_counter: UNSIGNED(11 downto 0) := (others => '0');

-- Vertical Counter
signal v_counter: UNSIGNED(11 downto 0) := (others => '0');
signal v_counter_enable: STD_LOGIC := '0';

-- VGA Pixel Clock
signal pixel_clock: STD_LOGIC := '0';
signal pixel_clock_enable: STD_LOGIC := '0';

-- VGA Video ON
signal video_on: STD_LOGIC := '0';

-- VGA Image ON
signal image_on: STD_LOGIC := '0';

-- Image Pixel Coordinates
signal xpix: UNSIGNED(11 downto 0) := (others => '0');
signal ypix: UNSIGNED(11 downto 0) := (others => '0');

-- Dual RAM
signal filtered_image_write_enable: STD_LOGIC_VECTOR(0 downto 0) := (others => '0');
signal filtered_image_addr: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
signal filtered_image_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal read_ram_addr: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
signal read_ram_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

-- ROM
signal rom_addr: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
signal rom_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	---------------------
	-- VGA Pixel Clock --
	---------------------
	inst_vgaPixelClock : clk_wiz_0 port map (clk_out1 => pixel_clock, locked => pixel_clock_enable, clk_in1 => i_clock_100);

	------------------
	-- Synchronizer --
	------------------
	inst_synchronizer : Synchronizer port map (i_domain_clock => pixel_clock, i_input => i_reset, o_output => synchronized_reset);

	------------------------
	-- Horizontal Counter --
	------------------------
	process(pixel_clock)
	begin
		if rising_edge(pixel_clock) then
            if (pixel_clock_enable = '1') then
                if (synchronized_reset = '0') or (h_counter = HPIXELS -1) then
                    h_counter <= (others => '0');
                    v_counter_enable <= '1';
                else
                    h_counter <= h_counter +1;
                    v_counter_enable <= '0';
                end if;
            end if;
		end if;
	end process;

    ----------------------
	-- Vertical Counter --
	----------------------
	process(pixel_clock)
	begin
		if rising_edge(pixel_clock) then
            if (pixel_clock_enable = '1') then
                if (v_counter_enable = '1') then
                    if (v_counter = VLINES -1) then
                        v_counter <= (others => '0');
                    else
                        v_counter <= v_counter +1;
                    end if;
                end if;
            end if;
		end if;
	end process;

    -- HSync & VSync
    o_hsync <= '0' when h_counter < HSW else '1';
    o_vsync <= '0' when v_counter < VSW else '1';

	-- Video ON
	video_on <= '1' when ((HSW + HBP) <= h_counter and h_counter < (HPIXELS - HFP)) and ((VSW + VBP) <= v_counter and v_counter < (VLINES - VFP)) else '0';

	-- Image ON
	image_on <= '1' when video_on = '1' and h_counter < (HSW + HBP + IMAGE_WIDTH) and v_counter < (VSW + VBP + IMAGE_HEIGH) else '0';

	-- Image Pixel Coordinates (240 x 160)
	xpix <= h_counter - (HSW + HBP) when image_on = '1' else (others => '0');
	ypix <= v_counter - (VSW + VBP) when image_on = '1' else (others => '0');

	-------------------------------
	-- VGA Image (from Dual RAM) --
	-------------------------------
	process(pixel_clock)
	begin
		if rising_edge(pixel_clock) then
            if (pixel_clock_enable = '1') and (image_on = '1') then

				-- RAM Addr = ypix * 240 + xpix
                read_ram_addr <= STD_LOGIC_VECTOR(
								( "0" & ypix( 7 downto 0 ) & "0000000" )
								+ ( "00" & ypix( 7 downto 0 ) & "000000" )
								+ ( "000" & ypix( 7 downto 0 ) & "00000" )
								+ ( "0000" & ypix( 7 downto 0 ) & "0000" )
								+ ( "0000000" & xpix( 7 downto 0 )));

            end if;
		end if;
	end process;

	--------------
	-- Dual RAM --
	--------------
	inst_DualRAM : blk_mem_gen_1 port map (clka => pixel_clock,
		wea => filtered_image_write_enable,
		addra => filtered_image_addr, 
		dina => filtered_image_data,
		clkb => pixel_clock,
		addrb => read_ram_addr,
		doutb => read_ram_data);

	------------------
	-- Image Filter --
	------------------
	inst_imageFilter : ImageFilter port map (i_pixel_clock => pixel_clock,
	i_filter_mode_sw => i_filter_mode_sw,
	i_image_to_filter => rom_data,
	o_image_to_filter_addr => rom_addr,
	o_filtered_image_write_enable => filtered_image_write_enable,
	o_filtered_image_addr => filtered_image_addr,
	o_filtered_image_data => filtered_image_data);

	inst_ROM : blk_mem_gen_0 port map (clka => pixel_clock, addra => rom_addr, douta => rom_data);

	-----------------
	-- VGA Display --
	-----------------
	inst_vgaDisplay : VGADisplay port map (i_enable => image_on, i_data => read_ram_data, o_red => o_vga_red, o_green => o_vga_green, o_blue => o_vga_blue);

end Behavioral;
