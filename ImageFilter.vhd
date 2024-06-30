------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: ImageFilter
-- Description:
--      Image Filter
--		Input 	-	i_pixel_clock: Pixel Clock
--		Input 	-	i_filter_mode_sw: Select Filter Mode from Switches (0: No Filter, 1: Pass Through)
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
    i_filter_mode_sw: IN STD_LOGIC_VECTOR(0 downto 0);
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


------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- ROM Max Address (240 * 160 = 38 400)
constant ROM_WIDTH: UNSIGNED(15 downto 0) := X"9600";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Debouncer Output
signal debounced_filter_mode: STD_LOGIC_VECTOR(0 downto 0) := (others => '0');

-- ROM
signal rom_addr: UNSIGNED(15 downto 0) := (others => '0');
signal rom_data: STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal rom_data_valid: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

    ---------------
	-- Debouncer --
	---------------
	inst_debouncer : Debouncer generic map (DEBOUNCE_COUNTER_SIZE => 21) port map (i_clock => i_pixel_clock, i_input => i_filter_mode_sw(0), o_output => debounced_filter_mode(0));

	-----------------------------------
	-- ROM Manager (Image to Filter) --
	-----------------------------------
	process(i_pixel_clock)
	begin
		if rising_edge(i_pixel_clock) then

            -- Reset (No Filter Apply)
            if (debounced_filter_mode = "0") then
                rom_addr <= (others => '0');
                rom_data_valid <= '0';
            
            elsif (rom_addr < ROM_WIDTH) then
                -- Applying Filter: Next ROM Addr & Data
                rom_addr <= rom_addr +1;
                rom_data <= i_image_to_filter;
                rom_data_valid <= '1';
            else
                rom_data_valid <= '0';
            end if;

		end if;
	end process;

    -- ROM Addr
    o_image_to_filter_addr <= STD_LOGIC_VECTOR(rom_addr);

	------------------------------
	-- RAM Manager (Write Mode) --
	------------------------------
    o_filtered_image_write_enable(0) <= rom_data_valid;
	o_filtered_image_addr <= STD_LOGIC_VECTOR(rom_addr-1) when rom_data_valid = '1' else (others => '0');
    o_filtered_image_data <= STD_LOGIC_VECTOR(rom_data);
end Behavioral;