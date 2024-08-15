------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 21/06/2024
-- Module Name: VGAController
-- Description:
--      VGA Controller (1920x1080 60Hz)
--		Input 	-	i_pixel_clock: Pixel Clock (148.5 MHz)
--		Input 	-	i_reset: Reset ('0': NO Reset, '1': Reset)
--		Output 	-	o_read_next_image_data: Read Next Image Data
--		Input 	-	i_image_data: Image Data
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
END VGAController;

ARCHITECTURE Behavioral of VGAController is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------
COMPONENT VGADisplay is
	PORT(
		i_enable: IN STD_LOGIC;
		i_data: IN STD_LOGIC_VECTOR(11 downto 0);
		o_red: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_green: OUT STD_LOGIC_VECTOR(3 downto 0);
		o_blue: OUT STD_LOGIC_VECTOR(3 downto 0)
	);
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- Image Width (640 pixels)
constant IMAGE_WIDTH: UNSIGNED(11 downto 0) := X"280";

-- Image Heigh pixels (480 pixels)
constant IMAGE_HEIGH: UNSIGNED(11 downto 0) := X"1E0";

-- Horizontal Front Porch: 88 pixels
constant HFP: UNSIGNED(7 downto 0) := X"58";

-- Horizontal Back Porch: 148 pixels
constant HBP: UNSIGNED(7 downto 0) := X"94";

-- Horizontal Sync Width: 44 pixels
constant HSW: UNSIGNED(7 downto 0) := X"2C";

-- Horizontal Pixels (2200 pixels per line)
constant HPIXELS: UNSIGNED(11 downto 0) := X"898";

-- Vertical Front Porch: 4 lines
constant VFP: UNSIGNED(3 downto 0) := X"4";

-- Vertical Back Porch: 36 lines
constant VBP: UNSIGNED(7 downto 0) := X"24";

-- Vertical Sync Width: 5 lines
constant VSW: UNSIGNED(3 downto 0) := X"5";

-- Vertical Lines (1125 lines)
constant VLINES: UNSIGNED(11 downto 0) := X"465";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Horizontal Counter
signal h_counter: UNSIGNED(11 downto 0) := (others => '0');

-- Vertical Counter
signal v_counter: UNSIGNED(11 downto 0) := (others => '0');
signal v_counter_enable: STD_LOGIC := '0';

-- Image Data
signal image_enable: STD_LOGIC := '0';
signal image_enable_reg1: STD_LOGIC := '0';
signal image_enable_reg2: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	------------------------
	-- Horizontal Counter --
	------------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then
            if (i_reset = '1') or (h_counter = HPIXELS -1) then
                h_counter <= (others => '0');
                v_counter_enable <= '1';
            else
                h_counter <= h_counter +1;
                v_counter_enable <= '0';
            end if;
		end if;
	end process;

    ----------------------
	-- Vertical Counter --
	----------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then
		    if (i_reset = '1') then
				v_counter <= (others => '0');
		    elsif (v_counter_enable = '1') then
                if (v_counter = VLINES -1) then
					v_counter <= (others => '0');
                else
                    v_counter <= v_counter +1;
                end if;
            end if;
		end if;
	end process;

    -- HSync & VSync
    o_hsync <= '0' when h_counter < HSW else '1';
	o_vsync <= '0' when v_counter < VSW else '1';

	-- Video ON: '1' when ((HSW + HBP) <= h_counter and h_counter < (HPIXELS - HFP)) and ((VSW + VBP) <= v_counter and v_counter < (VLINES - VFP)) else '0';

	-- Image Enable (active Image Coordinates)
	-- Handle Read Next Image Data Latency: Read Memory and Get Data (2 cycles)
	image_enable <= '1' when (HSW + HBP-2) <= h_counter and h_counter < (HSW + HBP-2 + IMAGE_WIDTH) and h_counter < (HPIXELS - HFP) and 
							(VSW + VBP) <= v_counter and v_counter < (VSW + VBP + IMAGE_HEIGH) and v_counter < (VLINES - VFP)
							else '0';

	------------------------
	-- Image Data Manager --
	------------------------
	-- Read Next Image Data
	o_read_next_image_data <= image_enable;

	-----------------
	-- VGA Display --
	-----------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then
			-- Handle Read Next Image Data Latency (2 cycles)
			image_enable_reg1 <= image_enable;
			image_enable_reg2 <= image_enable_reg1;
        end if;
    end process;

	inst_vgaDisplay : VGADisplay port map (i_enable => image_enable_reg2, i_data => i_image_data, o_red => o_vga_red, o_green => o_vga_green, o_blue => o_vga_blue);

end Behavioral;