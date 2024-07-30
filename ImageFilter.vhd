------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: ImageFilter
-- Description:
--      Image Filter
--		Input 	-	i_pixel_clock: Pixel Clock
--		Input 	-	i_restart: Restart Filtering Image
--		Input 	-	i_filter_mode: Filter Modes Selector
--						'0': Filter 0 (Pass-Through)
--						'1': Filter 1 (Default: Laplacian)
--		Output 	-	o_image_to_filter_addr: Image Address to Filter
--		Input 	-	i_image_to_filter_data: Image Data to Filter
--		Input 	-	o_filtered_image_data_ready: Filtered Image Write Ready
--		Output 	-	o_filtered_image_write_enable: Filtered Image Write Enable
--		Output 	-	o_filtered_image_data: Filtered Image Data
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ImageFilter is
PORT(
	i_pixel_clock: IN STD_LOGIC;
	i_restart: IN STD_LOGIC;
	i_filter_mode: IN STD_LOGIC;
	o_image_to_filter_addr: OUT STD_LOGIC_VECTOR(18 downto 0);
	i_image_to_filter_data: IN STD_LOGIC_VECTOR(11 downto 0);
	o_filtered_image_data_ready: OUT STD_LOGIC;
    o_filtered_image_write_enable: OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
    o_filtered_image_data: OUT STD_LOGIC_VECTOR(11 downto 0)
);
END ImageFilter;

ARCHITECTURE Behavioral of ImageFilter is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------
COMPONENT shift_ram IS
  PORT (
    D : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    CLK : IN STD_LOGIC;
    Q : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- Image Max Address (640 * 480 = 307 200 pixels)
constant IMAGE_ADDR_MAX: UNSIGNED(19 downto 0) := X"4B000";

-- Image Last Line (n°306 560)
constant IMAGE_LAST_LINE: UNSIGNED(19 downto 0) := x"4AD80";

-- Filter Initialization Line (640 pixels)
constant FILTER_INIT_LINE: UNSIGNED(11 downto 0) := X"280";

-- Filter Initialization Pixel (2 pixels)
constant FILTER_INIT_PIXEL: UNSIGNED(1 downto 0) := "10";

-- Filter Initialization Window (Initialization Line + Initialization Pixel)
constant FILTER_INIT_WINDOW: UNSIGNED(11 downto 0) := FILTER_INIT_LINE + FILTER_INIT_PIXEL;

-- Filter Line Edges (Left: pixel 0, Right: pixel 639)
constant FILTER_LEFT_EDGE_POS: UNSIGNED(7 downto 0) := X"00";
constant FILTER_RIGHT_EDGE_POS: UNSIGNED(11 downto 0) := FILTER_INIT_LINE-1;

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Image Address & Data Valid
signal image_addr: UNSIGNED(18 downto 0) := (others => '0');
signal image_data_ready: STD_LOGIC := '0';

-- Filter Controls
signal filter_init: STD_LOGIC := '1';
signal filter_left_edge: STD_LOGIC := '0';
signal filter_right_edge: STD_LOGIC := '0';
signal filter_pixel: UNSIGNED(7 downto 0) := (others => '0');
signal filter_data_valid: STD_LOGIC := '0';
signal filter_image_addr: UNSIGNED(18 downto 0) := (others => '0');

-- Filter Params (Default: Laplacian)
signal filter_h00: INTEGER:= 0;
signal filter_h01: INTEGER:= 1;
signal filter_h02: INTEGER:= 0;
signal filter_h10: INTEGER:= 1;
signal filter_h11: INTEGER:= -4;
signal filter_h12: INTEGER:= 1;
signal filter_h20: INTEGER:= 0;
signal filter_h21: INTEGER:= 1;
signal filter_h22: INTEGER:= 0;

-- Filter Variables
signal previous_data2: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal previous_data1: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal previous_data0: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal current_data2: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal current_data1: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal current_data0: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal next_data2: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal next_data1: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal next_data0: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

-- Image Outputs
signal image_output_write_enable: STD_LOGIC_VECTOR(0 downto 0) := (others => '0');
signal image_output_addr: STD_LOGIC_VECTOR(18 downto 0) := (others => '0');
signal image_output_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	-------------------------
	-- Image Input Manager --
	-------------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then

			-- Restart Image Filter
			if (i_restart = '1') then
				-- Reset Image Address
				image_addr <= (others => '0');
				image_data_ready <= '0';

			-- Next Image Address
			elsif (image_addr < IMAGE_ADDR_MAX) then
				image_addr <= image_addr +1;
				image_data_ready <= '1';
			else
				-- Reset Image Address
				image_addr <= (others => '0');
				image_data_ready <= '0';
			end if;
		end if;
	end process;

    -- Image Input Address
    o_image_to_filter_addr <= STD_LOGIC_VECTOR(image_addr);

	---------------------------
	-- Filter Initialization --
	---------------------------
	filter_init <= '1' when image_addr < FILTER_INIT_WINDOW else '0';

	----------------------------------------
	-- Filter Data: Previous Line Manager --
	----------------------------------------
	inst_shiftRam_prevLigne : shift_ram port map (D => current_data2, CLK => i_pixel_clock, Q => previous_data2);
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then
			previous_data1 <= previous_data2;
			previous_data0 <= previous_data1;
		end if;
	end process;

	---------------------------------------
	-- Filter Data: Current Line Manager --
	---------------------------------------
	inst_shiftRam_currentLigne : shift_ram port map (D => i_image_to_filter_data, CLK => i_pixel_clock, Q => current_data2);
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then
			current_data1 <= current_data2;
			current_data0 <= current_data1;
		end if;
	end process;

	------------------------------------
	-- Filter Data: Next Line Manager --
	------------------------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then

			-- Handle Last Line
			if (image_addr < IMAGE_LAST_LINE) then
				next_data2 <= i_image_to_filter_data;
			else
				next_data2 <= (others => '0');
			end if;

			next_data1 <= next_data2;
			next_data0 <= next_data1;
		end if;
	end process;

	---------------------------
	-- Filtered Pixel Column --
	---------------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then

			-- Restart Filtered Pixel Column Position
			if (filter_init = '1') then
				filter_pixel <= (others => '0');

			-- Start counting ONLY after end of Filter Initialization
			elsif (filter_pixel < FILTER_RIGHT_EDGE_POS) then
				filter_pixel <= filter_pixel +1;

			else
				-- Restart Filtered Pixel Column Position
				filter_pixel <= (others => '0');
			end if;
		end if;
	end process;

	-------------------------------
	-- Filter Left & Right Edges --
	-------------------------------
	filter_left_edge <= '1' when filter_pixel = FILTER_LEFT_EDGE_POS else '0';
	filter_right_edge <= '1' when filter_pixel = FILTER_RIGHT_EDGE_POS else '0';

	--------------------------
	-- Image Output Manager --
	--------------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then

			-- Restart Image Output Address
			if (filter_init = '1') then
				filter_image_addr <= (others => '0');
				filter_data_valid <= '0';

			-- Start counting ONLY after end of Filter Initialization
			elsif (filter_image_addr < IMAGE_ADDR_MAX) then
				filter_image_addr <= filter_image_addr +1;
				filter_data_valid <= '1';

			else
				filter_data_valid <= '0';
			end if;
		end if;
	end process;

	-----------------------
	-- Filter Processing --
	-----------------------
	process(i_pixel_clock)
	variable filter_00: INTEGER;
	variable filter_01: INTEGER;
	variable filter_02: INTEGER;
	variable filter_10: INTEGER;
	variable filter_11: INTEGER;
	variable filter_12: INTEGER;
	variable filter_20: INTEGER;
	variable filter_21: INTEGER;
	variable filter_22: INTEGER;
	variable filtered_value: INTEGER;
	begin
		if rising_edge(i_pixel_clock) then
			
			-- Filter Processing 0 (Default: Pass-Through)
			if (synchronized_filter_mode = '0') then
				-- 1 latency cycle from Image Input to Image Output
				image_output_addr <= STD_LOGIC_VECTOR(image_addr-1);
				image_output_data <= i_image_to_filter_data;
				image_output_write_enable(0) <= image_data_ready;
					
			-- Filter Processing 1 (Default: Laplacian)
			else
				-- Filter Computation - Left Edge
				if (filter_left_edge = '1') then
					filter_00 := 0;
					filter_10 := 0;
					filter_20 := 0;
				else
					filter_00 := TO_INTEGER(SIGNED(previous_data0));
					filter_10 := TO_INTEGER(SIGNED(current_data0));
					filter_20 := TO_INTEGER(SIGNED(next_data0));
				end if;

				-- Filter Computation - Left Edge
				if (filter_right_edge = '1') then
					filter_02 := 0;
					filter_12 := 0;
					filter_22 := 0;
				else
					filter_02 := TO_INTEGER(SIGNED(previous_data2));
					filter_12 := TO_INTEGER(SIGNED(current_data2));
					filter_22 := TO_INTEGER(SIGNED(next_data2));
				end if;


				-- Filter Computation - Right Edge
				filter_01 := TO_INTEGER(SIGNED(previous_data1));
				filter_11 := TO_INTEGER(SIGNED(current_data1));
				filter_21 := TO_INTEGER(SIGNED(next_data1)); 

				-- Filter Computation
				filtered_value := 	filter_h00 * filter_00 + 
									filter_h01 * filter_01 + 
									filter_h02 * filter_02 + 
									filter_h10 * filter_10 + 
									filter_h11 * filter_11 + 
									filter_h12 * filter_12 + 
									filter_h20 * filter_20 +
									filter_h21 * filter_21 +
									filter_h22 * filter_22;
	
				-- 1 latency cycle from ROM Read Address to RAM Write Address
				image_output_addr <= STD_LOGIC_VECTOR(filter_image_addr-1);
				image_output_data <= STD_LOGIC_VECTOR(TO_SIGNED(filtered_value, 12));
				image_output_write_enable(0) <= filter_data_valid;
			end if;
		end if;
	end process;

	-- Filtered Image Address & Data
	o_filtered_image_addr <= image_output_addr;
	o_filtered_image_data <= image_output_data;

	-- Filtered Image Output Enable
	o_filtered_image_write_enable <= image_output_write_enable;

end Behavioral;