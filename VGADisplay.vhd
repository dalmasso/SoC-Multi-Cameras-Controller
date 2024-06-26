------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 21/06/2024
-- Module Name: VGAStripes
-- Description:
--      VGA Display
--		Input 	-	i_enable: Enable Display
--		Input 	-	i_data: Data to Display
--      Output	-	o_red: Red Pixel Signals
--      Output	-	o_green: Green Pixel Signals
--      Output	-	o_blue: Blue Pixel Signals
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY VGADisplay is
PORT(
    i_enable: IN STD_LOGIC;
    i_data: IN STD_LOGIC_VECTOR(11 downto 0);
    o_red: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_green: OUT STD_LOGIC_VECTOR(3 downto 0);
    o_blue: OUT STD_LOGIC_VECTOR(3 downto 0)
);
END VGADisplay;

ARCHITECTURE Behavioral of VGADisplay is

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

    -- Red Control
    o_red <= i_data(11) & i_data(10) & i_data(9) & i_data(8) when i_enable = '1' else (others => '0');
    
    -- Green Control
    o_green <= i_data(7) & i_data(6) & i_data(5) & i_data(4) when i_enable = '1' else (others => '0');

    -- Blue Control
    o_blue <= i_data(3) & i_data(2) & i_data(1) & i_data(0) when i_enable = '1' else (others => '0');

end Behavioral;
