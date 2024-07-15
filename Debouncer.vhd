------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: Debouncer
-- Description:
--      Debounce the input signal
--		Generic -	DEBOUNCE_COUNTER_SIZE: Counter Size (Default: 20)
--		Input 	-	i_clock: Clock
--		Input 	-	i_input: Input to debounce
--      Output	-	o_output: Debounced input
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY Debouncer is
GENERIC (
	-- Debounce Threshold for 10ms button with 100MHz Clock: 20 bits
	DEBOUNCE_COUNTER_SIZE: INTEGER := 20
);
PORT(
	i_clock: IN STD_LOGIC;
	i_input: IN STD_LOGIC;
    o_output: OUT STD_LOGIC
);
END Debouncer;

ARCHITECTURE Behavioral of Debouncer is

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Synchronizer
signal synchronizer_reg1: STD_LOGIC := '0';
signal synchronizer_reg2: STD_LOGIC := '0';
signal synchronizer_mismatch: STD_LOGIC := '0';

-- Counter (10ms * Clock 100MHz = 1 000 000-1)
signal counter: UNSIGNED(DEBOUNCE_COUNTER_SIZE downto 0) := (others => '0');
signal valid_debounce: STD_LOGIC := '0';

-- Output
signal output_reg: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	------------------
	-- Synchronizer --
	------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
            synchronizer_reg1 <= i_input;
            synchronizer_reg2 <= synchronizer_reg1;
		end if;
	end process;

	-------------
	-- Counter --
	-------------
	synchronizer_mismatch <= synchronizer_reg1 xor synchronizer_reg2;
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			if (synchronizer_mismatch = '1')then
				counter <= (others => '0');
			else
				counter <= counter +1;
			end if;
		end if;
	end process;
	valid_debounce <= '1' when counter(DEBOUNCE_COUNTER_SIZE) = '1' else '0';

	--------------------
	-- Output Control --
	--------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			if (valid_debounce = '1') then
				output_reg <= synchronizer_reg2;
			end if;
		end if;
	end process;
	o_output <= output_reg;

end Behavioral;