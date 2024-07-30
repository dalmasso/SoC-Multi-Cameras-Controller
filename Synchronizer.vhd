------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 18/06/2024
-- Module Name: Synchronizer
-- Description:
--      Synchronize the input signal
--		Input 	-	i_domain_clock: Clock Domain
--		Input 	-	i_input: Input to synchronize
--      Output	-	o_output: Synchronized input
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY Synchronizer is
PORT(
	i_domain_clock: IN STD_LOGIC;
	i_input: IN STD_LOGIC;
    o_output: OUT STD_LOGIC
);
END Synchronizer;

ARCHITECTURE Behavioral of Synchronizer is

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- Synchronizer
-- Placement Constraints:
-- set_property ASYNC_REG TRUE [get_cells .../synchronizer_reg*]
-- Require Timing Constraints:
-- set_max_delay -datapath_only -from [get_cells .../...] -to [get_cells .../synchronizer_reg*] NewClockPeriode * NbReg
signal synchronizer_reg1: STD_LOGIC := '0';
signal synchronizer_reg2: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	------------------
	-- Synchronizer --
	------------------
	process(i_domain_clock)
	begin
		if rising_edge(i_domain_clock) then
            synchronizer_reg1 <= i_input;
            synchronizer_reg2 <= synchronizer_reg1;
		end if;
	end process;

	--------------------
	-- Output Control --
	--------------------
	o_output <= synchronizer_reg2;

end Behavioral;