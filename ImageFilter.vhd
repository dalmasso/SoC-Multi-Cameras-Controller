------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: ImageFilter
-- Description:
--      Image Filter
--		Input 	-	i_pixel_clock: Pixel Clock
--		Input 	-	i_restart: Restart Filter (active high)
--		Input 	-	i_filter_mode: Filter Modes Selector
--						'0': Filter 0 (Default: Pass-Through)
--						'1': Filter 1 (Default: Laplacian)
--		Input 	-	i_image_to_filter: Image to Filter
--		Output 	-	o_image_to_filter_addr: Address of the Image to Filter
--		Output 	-	o_filtered_image_write_enable: Write Enable of the Filtered Image to store
--		Output 	-	o_filtered_image_addr: Address of the Filtered Image to store
--		Output 	-	o_filtered_image_data: Image after Filtering
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ImageFilter is
PORT(
	i_pixel_clock: IN STD_LOGIC;
	i_restart: IN STD_LOGIC;
	i_filter_mode: IN STD_LOGIC;
	i_image_to_filter: IN STD_LOGIC_VECTOR(11 downto 0);
	o_image_to_filter_addr: OUT STD_LOGIC_VECTOR(15 downto 0);
    o_filtered_image_write_enable: OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
	o_filtered_image_addr: OUT STD_LOGIC_VECTOR(15 downto 0);
    o_filtered_image_data: OUT STD_LOGIC_VECTOR(11 downto 0)
);
END ImageFilter;

ARCHITECTURE Behavioral of ImageFilter is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------

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

COMPONENT c_shift_ram_0 IS
  PORT (
    D : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    CLK : IN STD_LOGIC;
    Q : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
  );
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- ROM Max Address (240 * 160 = 38 400 pixels)
constant ROM_ADDR_MAX: UNSIGNED(15 downto 0) := X"9600";

-- Filter Initialization Line (240 pixels)
constant FILTER_INIT_LINE: UNSIGNED(7 downto 0) := X"F0";

-- Filter Initialization Pixel (2 pixels)
constant FILTER_INIT_PIXEL: UNSIGNED(1 downto 0) := "10";

-- Filter Initialization Window (Initialization Line + Initialization Pixel)
constant FILTER_INIT_WINDOW: UNSIGNED(7 downto 0) := FILTER_INIT_LINE + FILTER_INIT_PIXEL;

-- Filter Line Edges (Left: pixel 0, Right: pixel 239)
constant FILTER_LEFT_EDGE_POS: UNSIGNED(7 downto 0) := X"00";
constant FILTER_RIGHT_EDGE_POS: UNSIGNED(7 downto 0) := FILTER_INIT_LINE-1;

-- Filter Max Lines (160 lines)
constant FILTER_LINE_MAX: UNSIGNED(7 downto 0) := X"A0";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Debouncer Output
signal debounced_restart: STD_LOGIC := '0';
signal debounced_filter_mode: STD_LOGIC := '0';

-- ROM
signal rom_addr: UNSIGNED(15 downto 0) := (others => '0');
signal rom_data_valid: STD_LOGIC := '0';

-- Filter Controls
signal filter_init: STD_LOGIC := '1';
signal filter_left_edge: STD_LOGIC := '0';
signal filter_right_edge: STD_LOGIC := '0';
signal filter_pixel: UNSIGNED(7 downto 0) := (others => '0');
signal filter_data_valid: STD_LOGIC := '0';
signal filter_write_addr: UNSIGNED(15 downto 0) := (others => '0');

-- Filter 1 Params (Default: Laplacian)
signal filter_1_h00: INTEGER:= 0;
signal filter_1_h01: INTEGER:= 1;
signal filter_1_h02: INTEGER:= 0;
signal filter_1_h10: INTEGER:= 1;
signal filter_1_h11: INTEGER:= -4;
signal filter_1_h12: INTEGER:= 1;
signal filter_1_h20: INTEGER:= 0;
signal filter_1_h21: INTEGER:= 1;
signal filter_1_h22: INTEGER:= 0;

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

-- Filtered Image Outputs
signal filtered_image_write_enable: STD_LOGIC_VECTOR(0 downto 0) := (others => '0');
signal filtered_image_addr: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
signal filtered_image_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

    ---------------
	-- Debouncer --
	---------------
	inst_debouncer_0 : Debouncer generic map (DEBOUNCE_COUNTER_SIZE => 20) port map (i_clock => i_pixel_clock, i_input => i_restart, o_output => debounced_restart);
	inst_debouncer_1 : Debouncer generic map (DEBOUNCE_COUNTER_SIZE => 20) port map (i_clock => i_pixel_clock, i_input => i_filter_mode, o_output => debounced_filter_mode);

	-----------------------------------
	-- ROM Manager (Image to Filter) --
	-----------------------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then
			
			-- Restart ROM Read
            if (debounced_restart = '1') then
                rom_addr <= (others => '0');
                rom_data_valid <= '0';
            
            elsif (rom_addr < ROM_ADDR_MAX) then
                -- Read ROM (Next Addr & Data)
                rom_addr <= rom_addr +1;
                rom_data_valid <= '1';
            else
                rom_data_valid <= '0';
            end if;

		end if;
	end process;

    -- ROM Addr
    o_image_to_filter_addr <= STD_LOGIC_VECTOR(rom_addr);

	---------------------------
	-- Filter Initialization --
	---------------------------
	filter_init <= '1' when rom_addr < FILTER_INIT_WINDOW else '0';

	--------------------
	-- Filtered Pixel --
	--------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then

			-- Restart Filtered Pixel Position
			if (filter_init = '1') then
				filter_pixel <= (others => '0');

			-- Start counting ONLY after end of Filter Initialization
			elsif (filter_pixel < FILTER_RIGHT_EDGE_POS) then
				filter_pixel <= filter_pixel +1;

			else
				filter_pixel <= (others => '0');
			end if;
		end if;
	end process;

	-------------------------------
	-- Filter Left & Right Edges --
	-------------------------------
	filter_left_edge <= '1' when filter_pixel = FILTER_LEFT_EDGE_POS else '0';
	filter_right_edge <= '1' when filter_pixel = FILTER_RIGHT_EDGE_POS else '0';

	---------------------------------
	-- Filter Write Memory Manager --
	---------------------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then

			-- Restart Write Address
			if (filter_init = '1') then
				filter_write_addr <= (others => '0');
				filter_data_valid <= '0';

			-- Start counting ONLY after end of Filter Initialization
			elsif (filter_write_addr < ROM_ADDR_MAX) then
				filter_write_addr <= filter_write_addr +1;
				filter_data_valid <= '1';

			else
				filter_data_valid <= '0';
			end if;
		end if;
	end process;

	----------------------------------------
	-- Filter Data: Previous Line Manager --
	----------------------------------------
	inst_shiftRam_prevLigne : c_shift_ram_0 port map (D => current_data2, CLK => i_pixel_clock, Q => previous_data2);
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
	inst_shiftRam_currentLigne : c_shift_ram_0 port map (D => i_image_to_filter, CLK => i_pixel_clock, Q => current_data2);
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
	next_data2 <= i_image_to_filter;
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then
			next_data1 <= next_data2;
			next_data0 <= next_data1;
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
			if (debounced_filter_mode = '0') then
				-- 1 latency cycle from ROM Read Address to RAM Write Address
				filtered_image_addr <= STD_LOGIC_VECTOR(rom_addr-1);
				filtered_image_data <= i_image_to_filter;
				filtered_image_write_enable(0) <= rom_data_valid;
					
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
				filtered_value := 	filter_1_h00 * filter_00 + 
									filter_1_h01 * filter_01 + 
									filter_1_h02 * filter_02 + 
									filter_1_h10 * filter_10 + 
									filter_1_h11 * filter_11 + 
									filter_1_h12 * filter_12 + 
									filter_1_h20 * filter_20 +
									filter_1_h21 * filter_21 +
									filter_1_h22 * filter_22;
	
				-- 1 latency cycle from ROM Read Address to RAM Write Address
				filtered_image_addr <= STD_LOGIC_VECTOR(filter_write_addr-1);
				filtered_image_data <= STD_LOGIC_VECTOR(TO_SIGNED(filtered_value, 12));
				filtered_image_write_enable(0) <= filter_data_valid;
			end if;
	
		end if;
	end process;

	---------------
	-- RAM Write --
	---------------
	-- RAM Write Address & Data
	o_filtered_image_addr <= filtered_image_addr;
	o_filtered_image_data <= filtered_image_data;

	-- RAM Write Enable
	o_filtered_image_write_enable <= filtered_image_write_enable;

end Behavioral;