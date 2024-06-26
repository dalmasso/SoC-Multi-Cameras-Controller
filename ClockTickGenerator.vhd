------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: ClockTickGenerator
-- Description:
--      Generate Clock Tick signals from master clock 100MHz
--		Input 	-	i_clock_100: Clock (100MHz)
--      Output	-	o_clock_tick_1: 1Hz Clock Tick
--      Output	-	o_clock_tick_25M: 25MHz Clock Tick
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY ClockTickGenerator is
PORT(
	i_clock_100: IN STD_LOGIC;
	o_clock_tick_1: OUT STD_LOGIC;
	o_clock_tick_25M: OUT STD_LOGIC
);
END ClockTickGenerator;

ARCHITECTURE Behavioral of ClockTickGenerator is

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- Counter Threshold for Tick Counter 1Hz (Threshold: 100000000-1)
constant COUNTER_1HZ: UNSIGNED(27 downto 0) := x"5F5E0FF";

-- Counter Threshold for Tick Counter 25MHz (Threshold: 4-1)
constant COUNTER_25MHZ: UNSIGNED(3 downto 0) := x"3";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Clock Tick Counter
signal counter: UNSIGNED(26 downto 0) := (others => '0');
signal counter_end: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	----------------------------
	-- Clock Tick Counter 1Hz --
	----------------------------
	counter_end <= '1' when counter = COUNTER_1HZ else '0';
	process(i_clock_100)
	begin
		if rising_edge(i_clock_100) then
			if (counter_end = '1') then
				counter <= (others => '0');
			else
				counter <= counter +1;
			end if;
		end if;
	end process;

    -- 1Hz Clock
	o_clock_tick_1 <= counter_end;

    -- 25Hz Clock
	o_clock_tick_25M <= '1' when counter(1 downto 0) = COUNTER_25MHZ else '0';

end Behavioral;
