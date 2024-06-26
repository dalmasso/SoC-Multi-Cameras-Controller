------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: Testbench - Debouncer
-- Description:
--      Debounce the input signal
--		Input 	-	i_clock_100: Clock (100MHz)
--		Input 	-	i_input: Input to debounce
--      Output	-	o_output: Debounced input
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY Testbench_Debouncer is
END Testbench_Debouncer;

ARCHITECTURE Behavioral of Testbench_Debouncer is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------

COMPONENT Debouncer is
    PORT(
        i_clock_100: IN STD_LOGIC;
        i_input: IN STD_LOGIC;
        o_output: OUT STD_LOGIC
    );
END COMPONENT;

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Clock 100MHz
signal clock100: STD_LOGIC := '0';

-- Input
signal input_debounce: STD_LOGIC := '0';

-- Ouput
signal debounced_output: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	--------------------
	-- Signal Control --
	--------------------
	clock100 <= not clock100 after 5 ns;

	process
	variable i : natural;
	begin
		wait for 50 ns;
		for i in 0 to 6 loop
			input_debounce <= not(input_debounce);
			wait for 2 ns;
		end loop;
	end process;

	---------------------
	-- Unit Under Test --
	---------------------
	uut : Debouncer port map (i_clock_100 => clock100, i_input => input_debounce, o_output => debounced_output);

end Behavioral;
