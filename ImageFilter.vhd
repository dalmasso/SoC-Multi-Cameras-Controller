------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: ImageFilter
-- Description:
--      Image Filter
--		Input 	-	i_image_data_clock: Image Data Clock
--		Input 	-	i_image_data_enable: Image Data Enable ('0': Disable, '1': Enable)
--		Input 	-	i_image_data: Image Data (8-bit, Format: YUV 4:2:2, Sequence: U0 Y0 V0 Y1)
--		Input 	-	i_filter_mode: Select Filter Modes
--						"00": Filter 0 (GrayScale)
--						"01": Filter 1 (RGB)
--						"10": Filter 2 (Default: Laplacian)
--						"11": Filter 3 (Default: Laplacian)
--		Input 	-	i_pixel_clock: Pixel Clock (148.5 MHz)
--		Input 	-	i_read_reset: Read Reset ('0': NO Reset, '1': Reset)
--		Input 	-	i_read_pixel_data: Read Pixel Data ('0': No Read, '1': Read Pixel Data)
--		Output 	-	o_pixel_data: 12-bit Pixel Data
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ImageFilter is
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
END ImageFilter;

ARCHITECTURE Behavioral of ImageFilter is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------
COMPONENT ImageFilterGrayScale is
	PORT(
		i_clock: IN STD_LOGIC;
		i_clock_enable: IN STD_LOGIC;
		i_image_data_enable: IN STD_LOGIC;
		i_image_data: IN STD_LOGIC_VECTOR(7 downto 0);
		o_filtered_data_enable: OUT STD_LOGIC;
		o_filtered_data: OUT STD_LOGIC_VECTOR(11 downto 0);
		o_end_of_filter: OUT STD_LOGIC
	);
END COMPONENT;

COMPONENT dual_port_ram_memory IS
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(18 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(18 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- RAM Max Address (640 * 480 = 307 200 => Last RAM Address: 307 199)
constant RAM_MAX_ADDR: UNSIGNED(19 downto 0) := X"4AFFF";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- GrayScale Image Filter
signal grayscale_image_filter_enable: STD_LOGIC := '0';
signal grayscale_output_data_enable: STD_LOGIC := '0';
signal grayscale_output_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal grayscale_end: STD_LOGIC := '0';

-- Dual-Port RAM Memory
signal ram_write_reset: STD_LOGIC := '0';
signal ram_write_addr: UNSIGNED(18 downto 0) := (others => '0');
signal ram_write_enable: STD_LOGIC_VECTOR(0 downto 0) := (others => '0');
signal ram_write_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal ram_read_addr: UNSIGNED(18 downto 0) := (others => '0');
signal ram_read_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	-----------------------------
	-- Image Filter: GrayScale --
	-----------------------------
	grayscale_image_filter_enable <= '1' when i_filter_mode = "00" else '0';

	inst_ImageFilterGrayScale : ImageFilterGrayScale port map(
		i_clock => i_image_data_clock,
		i_clock_enable => grayscale_image_filter_enable,
		i_image_data_enable => i_image_data_enable,
		i_image_data => i_image_data,
		o_filtered_data_enable => grayscale_output_data_enable,
		o_filtered_data => grayscale_output_data,
		o_end_of_filter => grayscale_end);

	----------------------------------------
	-- Dual-Port RAM Memory Write Manager --
	----------------------------------------
	-- RAM Write Enable (TODO x or x or x)
	ram_write_enable(0) <= grayscale_output_data_enable;

	-- RAM Write Data (TODO with filterMode)
	ram_write_data <= grayscale_output_data;

	-- RAM Write Reset (TODO with filterMode)
	ram_write_reset <= grayscale_end;

	-- RAM Write Address
	process(i_image_data_clock)
	begin
		if rising_edge(i_image_data_clock) then

			-- Reset Write Address
			if (ram_write_reset = '1') then
				ram_write_addr <= (others => '0');

			-- Increment Write Address
			elsif (ram_write_enable = "1") and (ram_write_addr < RAM_MAX_ADDR) then
				ram_write_addr <= ram_write_addr +1;
			end if;
        end if;
    end process;

	--------------------------------------------
	-- Dual-Port RAM Memory (READ_FIRST Mode) --
	--------------------------------------------
	inst_dualPortRamMemory : dual_port_ram_memory port map (
		clka => i_image_data_clock,
		wea => ram_write_enable,
		addra => STD_LOGIC_VECTOR(ram_write_addr),
		dina => ram_write_data,
		clkb => i_pixel_clock,
		addrb => STD_LOGIC_VECTOR(ram_read_addr),
		doutb => ram_read_data);

	---------------------------------------
	-- Dual-Port RAM Memory Read Manager --
	---------------------------------------
	-- RAM Read Address
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then
			
			-- Reset Read Address
			if (i_read_reset = '1') then
				ram_read_addr <= (others => '0');

			-- Increment Read Address
			elsif (i_read_pixel_data = '1') and (ram_read_addr < RAM_MAX_ADDR) then
				ram_read_addr <= ram_read_addr +1;
			end if;
        end if;
    end process;

	-- RAM Read Data
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then
			o_pixel_data <= ram_read_data;
		end if;
	end process;

end Behavioral;