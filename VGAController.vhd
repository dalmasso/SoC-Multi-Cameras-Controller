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

-- Image Width (240 pixels) & Heigh (160 pixels)
constant IMAGE_WIDTH: UNSIGNED(11 downto 0) := X"0F0";
constant IMAGE_HEIGH: UNSIGNED(11 downto 0) := X"0A0";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Pixel Clock
signal pixel_clock: STD_LOGIC := '0';

-- Synchronizer
signal synchronized_reset: STD_LOGIC := '0';

-- Horizontal Counter
signal h_counter: UNSIGNED(11 downto 0) := (others => '0');

-- Vertical Counter
signal v_counter: UNSIGNED(11 downto 0) := (others => '0');
signal v_counter_enable: STD_LOGIC := '0';
signal v_sync: STD_LOGIC := '0';

-- Video ON
signal video_on: STD_LOGIC := '0';

-- ROM
signal rom_addr: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
signal rom_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

-- Dual Port RAM
signal ram_write_enable: STD_LOGIC_VECTOR(0 downto 0) := (others => '0');
signal ram_write_addr: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
signal ram_write_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal ram_read_addr: UNSIGNED(15 downto 0) := (others => '0');
signal ram_read_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal ram_read_enable: STD_LOGIC := '0';

-- VGA Display
signal display_data_ready: STD_LOGIC := '0';
signal display_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	---------------------
	-- VGA Pixel Clock --
	---------------------
	inst_vgaPixelClock : clk_wiz_0 port map (clk_out1 => pixel_clock, clk_in1 => i_clock_100);

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
            if (synchronized_reset = '0') or (h_counter = HPIXELS -1) then
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
	process(pixel_clock)
	begin
		if rising_edge(pixel_clock) then
            if (v_counter_enable = '1') then
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
	v_sync <= '0' when v_counter < VSW else '1';
    o_vsync <= v_sync;

	-- Video ON
	video_on <= '1' when ((HSW + HBP) <= h_counter and h_counter < (HPIXELS - HFP)) and ((VSW + VBP) <= v_counter and v_counter < (VLINES - VFP)) else '0';

	-----------------------------------------
	-- Memory Address (from Dual Port RAM) --
	-----------------------------------------
	-- Image Pixel Coordinates (240 x 160)
	-- Memory Read Enable (Need to anticipate pixels to pre-load data from memory)
	ram_read_enable <= '1' when (HSW + HBP-1) <= h_counter and h_counter < (HSW + HBP-1 + IMAGE_WIDTH) and h_counter < (HPIXELS - HFP) and 
								(VSW + VBP) <= v_counter and v_counter < (VSW + VBP + IMAGE_HEIGH) and v_counter < (VLINES - VFP) else '0'; 

	process(pixel_clock)
	begin
		if rising_edge(pixel_clock) then

			-- Reset RAM Read Addr
			if (v_sync = '0') then
				ram_read_addr <= (others => '0');
			
			-- Read RAM (active Image Coordinates AND NOT no Write Operation)
            elsif (ram_read_enable = '1') and (ram_write_enable = "0") then
				-- Need to anticipate pixels to pre-load data from memory
				ram_read_addr <= ram_read_addr +1;
	        end if;
	    end if;
	end process;
	
	------------------
	-- Image Filter --
	------------------
	inst_imageFilter : ImageFilter port map (
		i_pixel_clock => pixel_clock,
		i_filter_mode_sw => i_filter_mode_sw,
		i_image_to_filter => rom_data,
		o_image_to_filter_addr => rom_addr,
		o_filtered_image_write_enable => ram_write_enable,
		o_filtered_image_addr => ram_write_addr,
		o_filtered_image_data => ram_write_data);

	------------------
	-- Memory - ROM --
	------------------
	inst_ROM : blk_mem_gen_0 port map (clka => pixel_clock, addra => rom_addr, douta => rom_data);

	----------------------------
	-- Memory - Dual Port RAM --
	----------------------------
	inst_DualRAM : blk_mem_gen_1 port map (clka => pixel_clock,
		wea => ram_write_enable,
		addra => ram_write_addr, 
		dina => ram_write_data,
		clkb => pixel_clock,
		addrb => STD_LOGIC_VECTOR(ram_read_addr),
		doutb => ram_read_data);

	-----------------
	-- VGA Display --
	-----------------
	-- Display Data Ready
	process(pixel_clock)
	begin
		if rising_edge(pixel_clock) then
            display_data_ready <= ram_read_enable;
	    end if;
	end process;

	-- Data Selector
	display_data <= ram_read_data when display_data_ready = '1' else (others => '0');
	
	inst_vgaDisplay : VGADisplay port map (i_enable => video_on, i_data => display_data, o_red => o_vga_red, o_green => o_vga_green, o_blue => o_vga_blue);

end Behavioral;
