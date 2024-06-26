------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: Testbench - ChainedLed
-- Description:
--      Shift single LED on left to right then right to left
--		Input 	-	i_clock_100: Clock (100MHz)
--		Input 	-	i_clock_enable: Clock Enable (active high)
--		Input 	-	i_reset: Reset (active low)
--      Output	-	o_leds: 16 LEDs
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY Testbench_ChainedLed is
END Testbench_ChainedLed;

ARCHITECTURE Behavioral of Testbench_ChainedLed is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------

COMPONENT ChainedLed is
PORT(
	i_clock_100: IN STD_LOGIC;
    i_clock_enable: IN STD_LOGIC;
	i_reset: IN STD_LOGIC;
	o_leds: OUT STD_LOGIC_VECTOR(15 downto 0)
);
END COMPONENT;

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Clock 100MHz
signal clock100: STD_LOGIC := '0';

-- Clock Enable
signal clock_enable: STD_LOGIC := '0';

-- Reset
signal reset: STD_LOGIC := '0';

-- LEDs
signal leds: STD_LOGIC_VECTOR(15 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	--------------------
	-- Signal Control --
	--------------------
	clock100 <= not clock100 after 5 ns;
	clock_enable <= not clock_enable after 15 ns;
    reset <= '0', '1' after 203 ns, '0' after 600 ns, '1' after 650 ns;

	---------------------
	-- Unit Under Test --
	---------------------
	uut : ChainedLed port map (i_clock_100 => clock100, i_clock_enable => clock_enable, i_reset => reset, o_leds => leds);

end Behavioral;
