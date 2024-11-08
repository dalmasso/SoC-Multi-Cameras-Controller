------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 30/10/2024
-- Module Name: I2CMaster
-- Description:
--      I2C Master allowing Write/Read operations on slave devices
--		Features:
--			- Configurable Register Address/Data length
--			- Start Byte
--			- Clock Stretching Detection
--			- Multimaster (arbitration)
--
-- WARNING: /!\ Require Pull-Up on SCL and SDA pins /!\
--
-- Usage:
--      The Ready signal indicates no operation is on going and the I2C Master is waiting Write/Read operation.
--		At any time, if an error occurs, the Error signal is asserted and the I2C Master returns in IDLE state.
--      Reset input can be trigger at any time to reset the I2C Master to the IDLE state.
--		1. Set all necessary inputs
--			* Mode: Write ('0') or Read ('1')
--			* Slave Addresss
--			* Register Address Length in byte & Register Address
--			* Register Value Length in byte & Register Value (Write Mode only)
--			* Read Register Value Length in byte (Read Mode only)
--			* (Optionally) a START BYTE can be sent at the begining of the transmission to wake-up slave(s).
--      2. Asserts Start input. The Ready signal is de-asserted and the Busy signal is asserted.
--		3. I2C Master re-asserts the Ready signal at the end of transmission (Master is ready for a new transmission)
--		4. In Read mode only, the read value is available when its validity signal is asserted
--
-- Generics
--		input_clock: Module Input Clock Frequency
--		i2c_clock: I2C Serial Clock Frequency
--		max_bus_length: Maximum Length of the I2C Bus Address/Data in bits
--
-- Ports
--		Input 	-	i_clock: Module Input Clock
--		Input 	-	i_reset: Reset ('0': No Reset, '1': Reset)
--		Input 	-	i_start: Start I2C Transmission ('0': No Start, '1': Start)
--		Input 	-	i_start_byte_enable: Enable I2C Start Byte Transmission Phase ('0': Disable, '1': Enable)
--		Input 	-	i_mode: Read or Write Mode ('0': Write, '1': Read)
--		Input 	-	i_slave_addr: Slave Address (7 bits)
--		Input 	-	i_reg_addr_byte: Register Address in bytes
--		Input 	-	i_reg_addr: Register Address to Read/Write
--		Input 	-	i_reg_value_byte: Register Value to Write in bytes
--		Input 	-	i_reg_value: Register Value to Write
--		Input 	-	i_read_value_byte: Register Value to Read in bytes
--		Output 	-	o_read_value_valid: Validity of the Read Register Value ('0': Not Valid, '1': Valid)
--		Output 	-	o_read_value: Read Register Value
--		Output 	-	o_ready: Ready State of I2C Master ('0': Not Ready, '1': Ready)
--		Output 	-	o_error: Error State of I2C Master ('0': No Error, '1': Error)
--		Output 	-	o_busy: Busy State of I2C Master ('0': Not Busy, '1': Busy)
--		In/Out 	-	io_scl: I2C Serial Clock ('0'-'Z'(as '1') values, working with Pull-Up)
--		In/Out 	-	io_sda: I2C Serial Data ('0'-'Z'(as '1') values, working with Pull-Up)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY I2CMaster is

GENERIC(
	input_clock: INTEGER := 12_000_000;
	i2c_clock: INTEGER := 100_000;
	max_bus_length: INTEGER := 8
);

PORT(
	i_clock: IN STD_LOGIC;
    i_reset: IN STD_LOGIC;
	i_start: IN STD_LOGIC;
	i_start_byte_enable: IN STD_LOGIC;
	i_mode: IN STD_LOGIC;
	i_slave_addr: IN STD_LOGIC_VECTOR(6 downto 0);
	i_reg_addr_byte: IN INTEGER range 0 to max_bus_length/8;
	i_reg_addr: IN STD_LOGIC_VECTOR(max_bus_length-1 downto 0);
	i_reg_value_byte: IN INTEGER range 0 to max_bus_length/8;
	i_reg_value: IN STD_LOGIC_VECTOR(max_bus_length-1 downto 0);
	i_read_value_byte: IN INTEGER range 0 to max_bus_length/8;
	o_read_value_valid: OUT STD_LOGIC;
	o_read_value: OUT STD_LOGIC_VECTOR(max_bus_length-1 downto 0);
	o_ready: OUT STD_LOGIC;
	o_error: OUT STD_LOGIC;
	o_busy: OUT STD_LOGIC;
	io_scl: INOUT STD_LOGIC;
	io_sda: INOUT STD_LOGIC
);

END I2CMaster;

ARCHITECTURE Behavioral of I2CMaster is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------
COMPONENT I2CBusController is

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
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- I2C Modes ('0': Write, '1': Read)
constant I2C_WRITE_MODE: STD_LOGIC := '0';
constant I2C_READ_MODE: STD_LOGIC := '1';

-- I2C Data Selection
constant MSB_DATA_SELECTION_INIT: INTEGER := 7;
constant LSB_DATA_SELECTION_INIT: INTEGER := 0;
constant DATA_SELECTION_INCREMENT: INTEGER := 8;

-- I2C Left Shift Data Value
constant LEFT_SHIFT_EMPTY_VALUE: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');

-- I2C Start Byte
constant START_BYTE_VALUE: STD_LOGIC_VECTOR(7 downto 0) := X"01";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- I2C Master States
TYPE i2cState is (	IDLE,
					START_BYTE, END_START_BYTE,
					WRITE_SLAVE_ADDR_W, WRITE_REG_ADDR, WRITE_REG_VALUE,
					WRITE_SLAVE_ADDR_R, READ_REG_VALUE);
signal state: i2cState := IDLE;
signal next_state: i2cState;

-- I2C Bus Controller Signals
signal i2c_write: STD_LOGIC := '0';
signal i2c_write_value: STD_LOGIC_VECTOR(max_bus_length-1 downto 0) := (others => '0');
signal i2c_read: STD_LOGIC := '0';
signal i2c_read_value_valid_reg: STD_LOGIC := '0';
signal i2c_read_value_valid: STD_LOGIC := '0';
signal i2c_read_value: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal i2c_ready: STD_LOGIC := '0';
signal i2c_error: STD_LOGIC := '0';
signal i2c_busy: STD_LOGIC := '0';
signal i2c_busy_reg: STD_LOGIC := '0';

-- I2C Byte Counter
signal byte_counter: INTEGER range 0 to max_bus_length/8 := 0;
signal byte_counter_end: STD_LOGIC := '0';

-- I2C Busy Rising/Falling Edge Detection
signal busy_rising_edge: STD_LOGIC := '0';
signal busy_falling_edge: STD_LOGIC := '0';

-- I2C Read Value Valid Rising Edge Detection
signal read_valid_rising_edge: STD_LOGIC := '0';

-- I2C Read Data
signal read_value_valid: STD_LOGIC := '0';
signal read_value_reg: STD_LOGIC_VECTOR(max_bus_length-1 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	-----------------------
	-- I2C State Machine --
	-----------------------
    -- I2C State
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Reset State (on Reset or Error)
			if (i_reset = '1') or (i2c_error = '1') then
				state <= IDLE;
			
			-- Next State
			else
				state <= next_state;
			end if;
		end if;
	end process;

	-- I2C Next State
	process(state, i_start, i2c_ready, i_start_byte_enable, i_mode, byte_counter_end, busy_rising_edge)
	begin

		case state is
			when IDLE => 		if (i_start = '1') and (i2c_ready = '1') then

									-- Start Byte
									if (i_start_byte_enable = '1') then
										next_state <= START_BYTE;
									
									-- Write Slave Address
									else
										next_state <= WRITE_SLAVE_ADDR_W;
									end if;
								else
									next_state <= IDLE;
								end if;

			-- Start Byte
			when START_BYTE =>
								if (byte_counter_end = '1') then
									next_state <= END_START_BYTE;
								else
									next_state <= START_BYTE;
								end if;

			-- End Start Byte
			when END_START_BYTE =>
								if (busy_rising_edge = '1') then
									next_state <= WRITE_SLAVE_ADDR_W;
								else
									next_state <= END_START_BYTE;
								end if;

			-- Write Slave Address (Write Access)
			when WRITE_SLAVE_ADDR_W =>
								if (byte_counter_end = '1') then
									next_state <= WRITE_REG_ADDR;
								else
									next_state <= WRITE_SLAVE_ADDR_W;
								end if;

			-- Write Register Address
			when WRITE_REG_ADDR =>
								if (byte_counter_end = '1') then

									-- Write Mode
									if (i_mode = I2C_WRITE_MODE) then
										next_state <= WRITE_REG_VALUE;
									
									-- Read Mode
									else
										next_state <= WRITE_SLAVE_ADDR_R;
									end if;
								else
									next_state <= WRITE_REG_ADDR;
								end if;

			-- Write Register Value
			when WRITE_REG_VALUE =>
								if (byte_counter_end = '1') then
									next_state <= IDLE;
								else
									next_state <= WRITE_REG_VALUE;
								end if;
			
			-- Write Slave Address (Read Access)
			when WRITE_SLAVE_ADDR_R =>
								if (byte_counter_end = '1') then
									next_state <= READ_REG_VALUE;
								else
									next_state <= WRITE_SLAVE_ADDR_R;
								end if;

			-- Read Register Value
			when READ_REG_VALUE =>
								if (byte_counter_end = '1') then
									next_state <= IDLE;
								else
									next_state <= READ_REG_VALUE;
								end if;

			when others => next_state <= IDLE;
		end case;
	end process;

	------------------------
	-- I2C Bus Controller --
	------------------------
	inst_i2cBusController: I2CBusController
		generic map (
			input_clock => input_clock,
			i2c_clock => i2c_clock)
		
		port map (
			i_clock => i_clock,
			i_reset => i_reset,
			i_write => i2c_write,
			i_read => i2c_read,
			i_data => i2c_write_value(max_bus_length-1 downto max_bus_length-8),
			o_ready => i2c_ready,
			o_error => i2c_error,
			o_busy => i2c_busy,
			o_read_value_valid => i2c_read_value_valid,
			o_read_value => i2c_read_value,
			io_scl => io_scl,
			io_sda => io_sda);
	
	----------------
	-- I2C Status --
	----------------
	o_ready <= i2c_ready;
	o_error <= i2c_error;
	o_busy <= i2c_busy;

	----------------------------------
	-- I2C Busy Rising/Falling Edge --
	----------------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			
			-- I2C Busy Register
			i2c_busy_reg <= i2c_busy;

			-- Rising Edge Detection
			busy_rising_edge <= i2c_busy and not(i2c_busy_reg);

			-- Falling Edge Detection
			busy_falling_edge <= not(i2c_busy) and i2c_busy_reg;
		end if;
	end process;

	----------------------
	-- I2C Byte Counter --
	----------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Reset Byte Counter
			if (state = IDLE) or (byte_counter_end = '1') then
				byte_counter <= 0;

			-- Increment Byte Counter (on I2C Bus Busy Falling Edge)
			elsif (busy_falling_edge = '1') then
				byte_counter <= byte_counter +1;
			end if;
		end if;
	end process;

	-- Byte Counter End
	byte_counter_end <= '1' when ((state = START_BYTE) or (state = WRITE_SLAVE_ADDR_W) or (state = WRITE_SLAVE_ADDR_R)) and (byte_counter = 1) else
						'1' when (state = WRITE_REG_ADDR) and (byte_counter = i_reg_addr_byte) else
						'1' when (state = WRITE_REG_VALUE) and (byte_counter = i_reg_value_byte) else
						'1' when (state = READ_REG_VALUE) and (byte_counter = i_read_value_byte) else
						'0';

	-----------------------
	-- I2C Write Command --
	-----------------------
	i2c_write <= '1' when (state /= IDLE) and (state /= READ_REG_VALUE) and (byte_counter_end = '0') else '0';

	-----------------------
	-- I2C Read Command --
	-----------------------
	i2c_read <= '1' when ((state = END_START_BYTE) or (state = WRITE_SLAVE_ADDR_R) or (state = READ_REG_VALUE)) and (byte_counter_end = '0') else '0';

	--------------------
	-- I2C Data Write --
	--------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Write Start Byte
			if (state = START_BYTE) then
				i2c_write_value(max_bus_length-1 downto max_bus_length-8) <= START_BYTE_VALUE;

			-- Write Slave Address (Write Access)
			elsif (state = WRITE_SLAVE_ADDR_W) then
				i2c_write_value(max_bus_length-1 downto max_bus_length-8) <= i_slave_addr & I2C_WRITE_MODE;
			
			-- Write Slave Address (Read Access)
			elsif (state = WRITE_SLAVE_ADDR_R) then
				i2c_write_value(max_bus_length-1 downto max_bus_length-8) <= i_slave_addr & I2C_READ_MODE;

			-- Left-Shift during Multiple Write Cycles
			elsif (busy_falling_edge = '1') then
				i2c_write_value <= i2c_write_value(max_bus_length-9 downto 0) & LEFT_SHIFT_EMPTY_VALUE;

			-- Write Register Address (Init Write Value with Register Address)
			elsif (state = WRITE_REG_ADDR) and (byte_counter = 0) then
				i2c_write_value <= i_reg_addr;
			
			-- Write Register Value (Init Write Value with Register Address)
			elsif (state = WRITE_REG_VALUE) and (byte_counter = 0) then
				i2c_write_value <= i_reg_value;
			end if;
		end if;
	end process;

	--------------------------------------
	-- I2C Read Value Valid Rising Edge --
	--------------------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			
			-- I2C Busy Register
			i2c_read_value_valid_reg <= i2c_read_value_valid;

			-- Rising Edge Detection
			read_valid_rising_edge <= i2c_read_value_valid and not(i2c_read_value_valid_reg);
		end if;
	end process;

	--------------------
	-- I2C Read Value --
	--------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Read Register Value
			if (state = READ_REG_VALUE) and (read_valid_rising_edge = '1') then

				-- Add New Read Value & Left-Shift
				read_value_reg <= read_value_reg(max_bus_length-9 downto 0) & i2c_read_value;
			end if;
		end if;
	end process;
	o_read_value <= read_value_reg;

	--------------------------
	-- I2C Read Value Valid --
	--------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Enable Read Value Valid (End of Read Cycle)
			if (state = READ_REG_VALUE) and (byte_counter_end = '1') then
				read_value_valid <= '1';

			-- Disable Read Value Valid (New cycle)
			elsif (state /= IDLE) then
				read_value_valid <= '0';
			end if;

		end if;
	end process;
	o_read_value_valid <= read_value_valid;

end Behavioral;