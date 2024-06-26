------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: Testbench - ClockTickGenerator
-- Description:
--      Generate Clock Tick signals from master clock 100MHz
--		Input 	-	i_clock_100: Clock (100MHz)
--      Output	-	o_clock_tick_1: 1Hz Clock Tick
--      Output	-	o_clock_tick_25M: 25MHz Clock Tick
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY Testbench_ClockTickGenerator is
END Testbench_ClockTickGenerator;

ARCHITECTURE Behavioral of Testbench_ClockTickGenerator is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------

COMPONENT ClockTickGenerator is
	PORT(
		i_clock_100: IN STD_LOGIC;
		o_clock_tick_1: OUT STD_LOGIC;
		o_clock_tick_25M: OUT STD_LOGIC
	);
END COMPONENT;

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Clock 100MHz
signal clock100: STD_LOGIC := '0';

-- Clock Tick 1Hz
signal clock_tick_1: STD_LOGIC := '0';

-- Clock Tick 25MHz
signal clock_tick_25M: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	--------------------
	-- Signal Control --
	--------------------
	clock100 <= not clock100 after 5 ns;

	---------------------
	-- Unit Under Test --
	---------------------
	uut : ClockTickGenerator port map (i_clock_100 => clock100, o_clock_tick_1 => clock_tick_1, o_clock_tick_25M => clock_tick_25M);

end Behavioral;
