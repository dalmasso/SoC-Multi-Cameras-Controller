------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: ChainedLed
-- Description:
--      Shift single LED on left to right then right to left
--		Input 	-	i_clock_100: Clock (100MHz)
--		Input 	-	i_clock_enable: Clock Enable (active high)
--		Input 	-	i_reset: Reset (active low)
--      Output	-	o_leds: 16 LEDs
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY ChainedLed is
PORT(
	i_clock_100: IN STD_LOGIC;
    i_clock_enable: IN STD_LOGIC;
	i_reset: IN STD_LOGIC;
	o_leds: OUT STD_LOGIC_VECTOR(15 downto 0)
);
END ChainedLed;

ARCHITECTURE Behavioral of ChainedLed is

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Shift Order: 0 shift left, 1 shift right
signal shift_order: STD_LOGIC := '0';

-- Chained LEDs Register
signal chained_leds: STD_LOGIC_VECTOR(15 downto 0) := (others=>'0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	--------------------------------------
	-- Chained LEDs: LEDs Shift Control --
	--------------------------------------
	process(i_clock_100)
	begin
		if rising_edge(i_clock_100) then
			if (i_reset = '0') then
				shift_order <= '0';
			else
				if chained_leds(15) = '1' then
					shift_order <= '1';
				elsif chained_leds(0) = '0' then
					shift_order <= '0';
				end if;
			end if;
		end if;
	end process;

	--------------------------------
	-- Chained LEDs: LEDs Control --
	--------------------------------
	process(i_clock_100)
	begin
		if rising_edge(i_clock_100) then
			if (i_reset = '0') then
				chained_leds <= (others => '0');
			elsif (i_clock_enable = '1') then
				if shift_order = '0' then
					chained_leds <= chained_leds(14 downto 0) & '1';
				else
					chained_leds <= '0' & chained_leds(15 downto 1);
				end if;
			end if;
		end if;
	end process;
	o_leds <= chained_leds;

end Behavioral;
