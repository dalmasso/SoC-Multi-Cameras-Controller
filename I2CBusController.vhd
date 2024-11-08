------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 30/10/2024
-- Module Name: I2CBusController
-- Description:
--      I2C Bus Controller in charge of applying SCL & SDA signals on the I2C bus, according to operations.
--
-- WARNING: /!\ Require Pull-Up on SCL and SDA pins /!\
--
-- Usage:
--      The Ready signal indicates no operation is on going and the I2C Bus Controller is waiting Write/Read operation.
--		At any time, if an error occurs, the Error signal is asserted and the I2C Bus Controller returns in IDLE state.
--      Reset input can be trigger at any time to reset the I2C Bus Controller to the IDLE state.
--      Write mode:
--          1. Set i_data input with the data to write on the bus.
--          2. Set i_write input to '1' and i_read to '0'. The module will generate a start-then-write or single-write operation, 
--			   depending on the current state of the I2C Bus Controller. The o_busy signal indicates the I2C Bus Controller is
--			   performing the operation.
--          3. While the o_busy signal is asserted:
--              - If new write operation is required, set i_data input with the next data to write, and set i_write input to '1' (i_read to '0').
--              - If read operation is required, set i_read input to '1' and i_write to '0'. The module will generate Read operation.
--              - If Repeated Start operation is required, set i_write and i_read to '1'.
--				- If stop operation is required, set i_write and i_read inputs to '0'.
--      Read mode (always after Write Mode)
--          1. While the o_busy signal is asserted:
--              - If new read operation is required, set i_read input to '1' (i_write to '0').
--				- If Repeated Start operation is required, set i_write and i_read to '1'.
--              - If stop operation is required, set i_read and i_write inputs to '0'.
--          2. When the o_read_value_valid is asserted, the read data is available on the o_read_value output. The value MUST be processed
--			   BEFORE the next read operation.
--
-- Generics
--		input_clock: Module Input Clock Frequency
--		i2c_clock: I2C Serial Clock Frequency
-- Ports
--		Input 	-	i_clock: Module Input Clock
--		Input 	-	i_reset: Reset ('0': No Reset, '1': Reset)
--		Input 	-	i_write: Write Cycle Trigger ('0': No Write, '1': Write)
--		Input 	-	i_read: Read Cycle Trigger ('0': No Read, '1': Read)
--		Input 	-	i_data: Data to write on the bus (8 bits)
--		Output 	-	o_ready: Ready State of I2C Bus ('0': Not Ready, '1': Ready)
--		Output 	-	o_error: Error State of I2C Bus ('0': No Error, '1': Error)
--		Output 	-	o_busy: Busy State of I2C Bus ('0': Not Busy, '1': Busy)
--		Output 	-	o_read_value_valid: I2C Slave Register Value is valid ('0': Not Valid, '1': Valid)
--		Output 	-	o_read_value: I2C Slave Register Value
--		In/Out 	-	io_scl: I2C Serial Clock ('0'-'Z'(as '1') values, working with Pull-Up)
--		In/Out 	-	io_sda: I2C Serial Data ('0'-'Z'(as '1') values, working with Pull-Up)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY I2CBusController is

GENERIC(
	input_clock: INTEGER := 12_000_000;
	i2c_clock: INTEGER := 100_000
);

PORT(
	i_clock: IN STD_LOGIC;
	i_reset: IN STD_LOGIC;
	i_write: IN STD_LOGIC;
	i_read: IN STD_LOGIC;
	i_data: IN STD_LOGIC_VECTOR(7 downto 0);
	o_ready: OUT STD_LOGIC;
	o_error: OUT STD_LOGIC;
	o_busy: OUT STD_LOGIC;
	o_read_value_valid: OUT STD_LOGIC;
	o_read_value: OUT STD_LOGIC_VECTOR(7 downto 0);
	io_scl: INOUT STD_LOGIC;
	io_sda: INOUT STD_LOGIC
);

END I2CBusController;

ARCHITECTURE Behavioral of I2CBusController is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------
COMPONENT I2CBusAnalyzer is
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
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- I2C Clock Dividers
constant CLOCK_DIV: INTEGER := input_clock / i2c_clock;
constant CLOCK_DIV_X2_1_4: INTEGER := CLOCK_DIV /4;
constant CLOCK_DIV_X2_3_4: INTEGER := CLOCK_DIV - CLOCK_DIV_X2_1_4;

-- I2C IDLE ('Z' with Pull-Up)
constant TRANSMISSION_IDLE: STD_LOGIC := 'Z';

-- I2C Transmission Don't Care Bit
constant TRANSMISSION_DONT_CARE_BIT: STD_LOGIC := '1';

-- I2C Transmission Start Bit
constant TRANSMISSION_START_BIT: STD_LOGIC := '0';

-- I2C Transmission ACK Bit
constant TRANSMISSION_ACK_BIT: STD_LOGIC := '0';

-- I2C Transmission NACK Bit
constant TRANSMISSION_NACK_BIT: STD_LOGIC := '1';

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- I2C Master States
TYPE i2cState is (IDLE, START_TX, RE_START_TX, I2C_WRITE, I2C_WRITE_ACK, I2C_READ, I2C_READ_ACK, STOP_TX);
signal state: i2cState := IDLE;
signal next_state: i2cState;

-- I2C Clock Divider
signal clock_divider: INTEGER range 0 to CLOCK_DIV-1 := 0;
signal clock_enable: STD_LOGIC := '0';
signal clock_enable_x2_1_4: STD_LOGIC := '0';
signal clock_enable_x2_3_4: STD_LOGIC := '0';

-- I2C Bus Analyzer
signal bus_busy: STD_LOGIC := '0';
signal bus_arbitration: STD_LOGIC := '0';
signal scl_stretching: STD_LOGIC := '0';

-- I2C Error
signal error_flag: STD_LOGIC := '0';

-- I2C Transmission Bit Counter ('8 cycles per phase)
signal bit_counter: UNSIGNED(2 downto 0) := (others => '0');
signal bit_counter_end: STD_LOGIC := '0';

-- I2C SCL
signal scl_in: STD_LOGIC := '1';
signal scl_reg_out: STD_LOGIC := '1';

-- I2C SDA
signal sda_in: STD_LOGIC := '1';
signal sda_in_reg_valid: STD_LOGIC := '0';
signal sda_in_reg: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal sda_out: STD_LOGIC := '1';
signal data_write: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	------------------------
	-- I2C SCL/SDA Inputs --
	------------------------
	scl_in <= io_scl;
	sda_in <= io_sda;

	----------------------
	-- I2C Bus Analyzer --
	----------------------
	inst_i2cBusAnalyzer: I2CBusAnalyzer port map(
		i_clock => i_clock,
		i_scl_master => scl_reg_out,
		i_scl_line => scl_in,
		i_sda_master => sda_out,
		i_sda_line => sda_in,
		o_bus_busy => bus_busy,
		o_bus_arbitration => bus_arbitration,
		o_scl_stretching => scl_stretching);

	-----------------------
	-- I2C Clock Divider --
	-----------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Reset Clock Divider
			if (i_reset = '1') or (clock_divider = CLOCK_DIV-1) then
				clock_divider <= 0;

			-- Increment Clock Divider
			else
				clock_divider <= clock_divider +1;
			end if;
		end if;
	end process;

	-----------------------
	-- I2C Clock Enables --
	-----------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- SCL Stretching (Waiting no SCL Stretching)
			if (scl_stretching = '0') then

				-- Clock Enable
				if (clock_divider = CLOCK_DIV-1) then
					clock_enable <= '1';
				else
					clock_enable <= '0';
				end if;

				-- Clock Enable x2 (1/4)
				if (clock_divider = CLOCK_DIV_X2_1_4-1) then
					clock_enable_x2_1_4 <= '1';
				else
					clock_enable_x2_1_4 <= '0';
				end if;

				-- Clock Enable x2 (3/4)
				if (clock_divider = CLOCK_DIV_X2_3_4-1) then
					clock_enable_x2_3_4 <= '1';
				else
					clock_enable_x2_3_4 <= '0';
				end if;
			end if;
		end if;
	end process;

	-----------------------
	-- I2C State Machine --
	-----------------------
    -- I2C State
	process(i_clock)
	begin
        if rising_edge(i_clock) then
            
            -- Reset State
            if (i_reset = '1') then
                state <= IDLE;

            -- Next State (when Clock Enable)
			elsif (clock_enable = '1') then
                state <= next_state;
			end if;
		end if;
	end process;

    -- I2C Next State
	process(state, i_write, i_read, bus_busy, bus_arbitration, bit_counter_end, sda_in)
	begin

		case state is
			when IDLE => 	if (i_write = '1') and (bus_busy = '0') then
								next_state <= START_TX;
							else
								next_state <= IDLE;
							end if;
			
			-- Start Transmission
			when START_TX => next_state <= I2C_WRITE;

			-- Write Cycle
			when I2C_WRITE =>
							-- End of Write Cyle
                            if (bit_counter_end = '1') then
                                next_state <= I2C_WRITE_ACK;
   
							-- Master Loses Arbitration (during Write Cycle)
							elsif (bus_arbitration = '0') then
								next_state <= IDLE;

							else
								next_state <= I2C_WRITE;
							end if;

			-- End of Write Cycle (Slave ACK)
			when I2C_WRITE_ACK =>
                            -- Slave ACK Error or Stop Command
                            if (sda_in /= TRANSMISSION_ACK_BIT) then
                                next_state <= STOP_TX;
                            
							-- Re-Start Transmission (Write and Read)
							elsif (i_write = '1') and (i_read = '1') then
								next_state <= RE_START_TX;

                            -- Write Cycle Trigger (Write Only)
                            elsif (i_write = '1') then
                                next_state <= I2C_WRITE;

                            -- Read Cycle Trigger (Read Only)
                            elsif (i_read = '1') then
                                next_state <= I2C_READ;

                            -- Stop Trigger (No Write, No Read)
                            else
                                next_state <= STOP_TX;
                            end if;

			-- Read Cycle
			when I2C_READ =>
							-- End of Read Cyle
                            if (bit_counter_end = '1') then
                                next_state <= I2C_READ_ACK;
                            else
                                next_state <= I2C_READ;
                            end if;
            
            -- End of Read Cycle (Master ACK)
            when I2C_READ_ACK =>
							-- Re-Start Transmission (Write and Read)
							if (i_write = '1') and (i_read = '1') then
								next_state <= RE_START_TX;

							-- Read Cycle Trigger
                            elsif (i_read = '1') then
                                next_state <= I2C_READ;

                            -- Stop Trigger
                            else
                                next_state <= STOP_TX;
                            end if;

			-- Re-Start Transmission
			when RE_START_TX => next_state <= START_TX;

			-- End of Transmission
			when others => next_state <= IDLE;
		end case;
	end process;

	---------------------
	-- I2C Bit Counter --
	---------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_enable = '1') then

				-- Increment Counter
				if (state = I2C_WRITE) or (state = I2C_READ) then
					bit_counter <= bit_counter +1;
				
				-- Reset Counter
				else
					bit_counter <= (others => '0');
				end if;
			end if;
        end if;
    end process;

	-- Bit Counter End
	bit_counter_end <= bit_counter(2) and bit_counter(1) and bit_counter(0);

	-----------------------------
	-- I2C SCL Output Register --
	-----------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- SCL High ('Z')
			if (clock_enable_x2_1_4 = '1') or (state = IDLE) then
				scl_reg_out <= '1';
			
			-- SCL Low ('0')
			elsif (clock_enable_x2_3_4 = '1') and (state /= RE_START_TX) and (state /= STOP_TX) then
				scl_reg_out <= '0';
			end if;
		end if;
	end process;

	--------------------
	-- I2C SCL Output --
	--------------------
	-- ('0' or 'Z' values)
	io_scl <= '0' when scl_reg_out = '0' else TRANSMISSION_IDLE;

	-----------------------------
	-- I2C Date Write Register --
	-----------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_enable = '1') then

				-- Left-Shift
				if (state = I2C_WRITE) then
					data_write <= data_write(6 downto 0) & data_write(7);
			
				-- Load Data Write
				else
					data_write <= i_data;
				end if;

			end if;
		end if;
	end process;

	--------------------------
	-- I2C SDA Output Value --
	--------------------------
	process(state, i_read, data_write)
	begin
		-- Start & Stop Transmission
		if (state = START_TX) or (state = STOP_TX) then
			sda_out <= TRANSMISSION_START_BIT;
		
		-- End of Read Cycle
		elsif (state = I2C_READ_ACK) then

			-- New Read Cycle planned
			if (i_read = '1') then
				sda_out <= TRANSMISSION_ACK_BIT;
			
			-- Last Read Cycle
			else
				sda_out <= TRANSMISSION_NACK_BIT;
			end if;

		-- Write Cycle
		elsif (state = I2C_WRITE) then
			sda_out <= data_write(7);

		-- IDLE, End of Write Cycle, Read Cycle, Re-Start Cycle
		else
			sda_out <= TRANSMISSION_DONT_CARE_BIT;
		end if;
	end process;

	--------------------
	-- I2C SDA Output --
	--------------------
	-- ('0' or 'Z' values)
	io_sda <= '0' when sda_out = '0' else TRANSMISSION_IDLE;

	----------------------------
	-- I2C SDA Input Register --
	----------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_enable = '1') then
            
                -- SDA Input Enable
                if (state = I2C_READ) then
				    sda_in_reg <= sda_in_reg(6 downto 0) & sda_in;
                end if;

			end if;
		end if;
	end process;
    o_read_value <= sda_in_reg;

	----------------------------------
	-- I2C SDA Input Register Valid --
	----------------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Disable SDA Valid Data (New cycle)
			if (state = START_TX) or (state = I2C_READ) then
				sda_in_reg_valid <= '0';
			
			-- Enable SDA Valid Data (End of Read Cycle)
			elsif (state = I2C_READ_ACK) then
				sda_in_reg_valid <= '1';
			end if;
			
		end if;
	end process;
	o_read_value_valid <= sda_in_reg_valid;

	-------------------
	-- I2C Bus Ready --
	-------------------
	o_ready <= '1' when state = IDLE else '0';

	-------------------
	-- I2C Bus Error --
	-------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_enable = '1') then

				-- Disable Error Flag (New cycle)
				if (state = START_TX) then
					error_flag <= '0';
				
				-- Arbitration Fail (Write Cycle)
				elsif (state = I2C_WRITE) and (bus_arbitration = '0') then
					error_flag <= '1';
					
				-- No ACK at the End of Write Cycle
				elsif (state = I2C_WRITE_ACK) and (sda_in /= TRANSMISSION_ACK_BIT) then
					error_flag <= '1';
				end if;
			end if;
		end if;
	end process;
	o_error <= error_flag;

	------------------
	-- I2C Bus Busy --
	------------------
	o_busy <= '1' when state = START_TX or state = RE_START_TX or state = I2C_WRITE or state = I2C_READ else '0';

end Behavioral;