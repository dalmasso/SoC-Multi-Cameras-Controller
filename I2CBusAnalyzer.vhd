------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 30/10/2024
-- Module Name: I2CBusAnalyzer
-- Description:
--      I2C Bus Analyzer in charge to detect:
--          - Bus Busy Detection
--          - Bus Arbitration
--          - Clock Stretching
--
-- WARNING: /!\ Require Pull-Up on SCL and SDA pins /!\
--
-- Ports
--		Input 	-	i_clock: Module Input Clock
--		Input 	-	i_scl_master: I2C Serial Clock from Master
--		Input 	-	i_scl_line: I2C Serial Clock bus line
--		Input 	-	i_sda_master: I2C Serial Data from Master
--		Input 	-	i_sda_line: I2C Serial Data bus line
--		Output 	-	o_bus_busy: Bus Busy detection ('0': Not Busy, '1': Busy)
--		Output 	-	o_bus_arbitration: Bus Arbitration detection ('0': Lost Arbitration, '1': Win Arbitration)
--		Output 	-	o_scl_stretching: Serial Clock Stretching detection ('0': Not Stretching, '1': Stretching)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY I2CBusAnalyzer is

PORT(
	i_clock: IN STD_LOGIC;
    i_scl_master: IN STD_LOGIC;
	i_scl_line: IN STD_LOGIC;
	i_sda_master: IN STD_LOGIC;
    i_sda_line: IN STD_LOGIC;
    o_bus_busy: OUT STD_LOGIC;
    o_bus_arbitration: OUT STD_LOGIC;
	o_scl_stretching: OUT STD_LOGIC
);

END I2CBusAnalyzer;

ARCHITECTURE Behavioral of I2CBusAnalyzer is

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- I2C SCL / SDA Line Level
signal scl_line_level: STD_LOGIC := '0';
signal sda_line_level: STD_LOGIC := '0';

-- I2C SCL Stretching
signal scl_stretching: STD_LOGIC := '0';

-- I2C SDA Line
signal sda_line_edge: STD_LOGIC := '0';

-- I2C Bus Busy
signal bus_busy: STD_LOGIC := '0';

-- I2C Bus Arbitration
signal bus_arbitration: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin
	----------------------------------
	-- I2C SCL Line Level Converter --
	----------------------------------
	-- Convert 'Z' into '1' level
	scl_line_level <= '0' when i_scl_line = '0' else '1';

	----------------------------------
	-- I2C SDA Line Level Converter --
	----------------------------------
	-- Convert 'Z' into '1' level
	sda_line_level <= '0' when i_sda_line = '0' else '1';

    ----------------------------
	-- I2C Bus Busy Detection --
	----------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			
			-- SDA Line Edge Detection
			sda_line_edge <= sda_line_level;
			
			-- Start Condition (SCL High while SDA High to Low)
			if (scl_line_level = '1') and (sda_line_edge = '1') and (sda_line_level = '0') then
				bus_busy <= '1';
			
			-- Stop Condition (SCL High while SDA Low to High)
			elsif (scl_line_level = '1') and (sda_line_edge = '0') and (sda_line_level = '1') then
				bus_busy <= '0';	
			end if;
		end if;
	end process;
    o_bus_busy <= bus_busy;

	-------------------------
	-- I2C Bus Arbitration --
	-------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			
			-- SDA from Master = SDA Line
			bus_arbitration <= i_sda_master xnor sda_line_level;

		end if;
	end process;
    o_bus_arbitration <= bus_arbitration;

    ------------------------
	-- I2C SCL Stretching --
	------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			
			-- I2C Master release SCL ('Z') while I2C Slave pull-down SCL
			scl_stretching <= i_scl_master and not(scl_line_level);

		end if;
	end process;
	o_scl_stretching <= scl_stretching;

end Behavioral;