------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 08/08/2024
-- Module Name: ImageFilterGrayScale
-- Description:
--      GrayScale Image Filter
--		Input 	-	i_clock: Master Clock
--		Input 	-	i_clock_enable: Master Clock Enable ('0': Disable, '1': Enable)
--		Input 	-	i_image_data_enable: Image Data Enable ('0': Disable, '1': Enable)
--		Input 	-	i_image_data: Image Data (8-bit, Format: YUV 4:2:2, Sequence: U0 Y0 V0 Y1)
--		Output 	-	o_filtered_data_enable: Filtered Data Enable ('0': Disable, '1': Enable)
--		Output 	-	o_filtered_data: 12-bit Image Data
--		Output 	-	o_end_of_filter: End of Filtering Process ('0': In Progress, '1': End)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ImageFilterGrayScale is
PORT(
	i_clock: IN STD_LOGIC;
	i_clock_enable: IN STD_LOGIC;
	i_image_data_enable: IN STD_LOGIC;
	i_image_data: IN STD_LOGIC_VECTOR(7 downto 0);
	o_filtered_data_enable: OUT STD_LOGIC;
	o_filtered_data: OUT STD_LOGIC_VECTOR(11 downto 0);
	o_end_of_filter: OUT STD_LOGIC
);
END ImageFilterGrayScale;

ARCHITECTURE Behavioral of ImageFilterGrayScale is

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Filter In Progress
signal in_progress_reg1: STD_LOGIC := '0';
signal in_progress_reg2: STD_LOGIC := '0';

-- Image Luma Trigger
signal lumaTrigger: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin
	
    --------------------------------
	-- Filter In Progress Manager --
	--------------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
            in_progress_reg1 <= i_clock_enable and i_image_data_enable;
			in_progress_reg2 <= in_progress_reg1;
        end if;
    end process;

    ---------------------------
	-- End of Filter Manager --
	---------------------------
	o_end_of_filter <= not(in_progress_reg1) and not(in_progress_reg2);

    --------------------------
	-- Luma Trigger Manager --
	--------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Reset Luma Trigger
			if (i_image_data_enable = '0') then
				lumaTrigger <= '0';
			
			elsif (in_progress_reg1 = '1') then
				-- Luma Trigger
				lumaTrigger <= not(lumaTrigger);
			end if;
        end if;
    end process;

	----------------------
	-- Luma Data Enable --
	----------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
            o_filtered_data_enable <= lumaTrigger;
        end if;
    end process;

	---------------
	-- Luma Data --
	---------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			-- Luma Data: Convert 8-bits to 3*4-bits (÷16: >> 4)
			o_filtered_data <= i_image_data(7 downto 4) & i_image_data(7 downto 4) & i_image_data(7 downto 4);
        end if;
    end process;

end Behavioral;