------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 07/10/2024
-- Module Name: SCCBController
-- Description:
--      Serial Camera Control Bus (SCCB) Controller for OV7670 Camera Module.
--		Supports:
--			- Write Mode: 3-Phase Write Transmission
--			- Read Mode: 2-Phase Write Transmission then 2-Phase Read Transmission
-- Generics
--		Input	-	master_clock: Master Clock Frequency
--		Output	-	sccb_clock: SCCB Clock Frequency
-- Ports
--		Input 	-	i_clock: Master Clock
--		Input 	-	i_mode: Read or Write Mode ('0': Read, '1': Write)
--		Input 	-	i_addr: Address of the SCCB Slave
--		Input 	-	i_reg_addr: Address of the Register to Read/Write
--		Input 	-	i_reg_value: Value of the Register to Write
--		Input 	-	i_start: Start SCCB Transmission ('0': No Start, '1': Start)
--		Output 	-	o_ready: Ready State of SCCB Controller ('0': Not Ready, '1': Ready)
--		Output 	-	o_read_value_valid: Validity of value of the SCCB Slave Register ('0': Not Valid, '1': Valid)
--		Output 	-	o_read_value: Value of the SCCB Slave Register
--		Output 	-	o_scl: OV7670 Serial Clock (100kHz)
--		In/Out 	-	io_sda: OV7670 Serial Data
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY SCCBController is

GENERIC(
	master_clock: INTEGER := 12_000_000;
	sccb_clock: INTEGER := 100_000
);

PORT(
	i_clock: IN STD_LOGIC;
	i_mode: IN STD_LOGIC;
	i_addr: IN STD_LOGIC_VECTOR(7 downto 0);
	i_reg_addr: IN STD_LOGIC_VECTOR(7 downto 0);
	i_reg_value: IN STD_LOGIC_VECTOR(7 downto 0);
	i_start: IN STD_LOGIC;
	o_ready: OUT STD_LOGIC;
	o_read_value_valid: OUT STD_LOGIC;
	o_read_value: OUT STD_LOGIC_VECTOR(7 downto 0);
	o_scl: OUT STD_LOGIC;
	io_sda: INOUT STD_LOGIC
);

END SCCBController;

ARCHITECTURE Behavioral of SCCBController is

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- SCCB CLock Dividers
constant SCL_CLOCK_DIVIDER: INTEGER := master_clock / sccb_clock;
constant SCL_CLOCK_DIVIDER_X2: INTEGER := SCL_CLOCK_DIVIDER /2;
constant SCL_CLOCK_DIVIDER_X4_1: INTEGER := SCL_CLOCK_DIVIDER /4;
constant SCL_CLOCK_DIVIDER_X4_2: INTEGER := SCL_CLOCK_DIVIDER - SCL_CLOCK_DIVIDER_X4_1;

-- SCCB Transmission Don't Care Bit (Write Only)
constant TRANSMISSION_DONT_CARE_BIT: STD_LOGIC := 'Z';

-- SCCB Transmission SDA Output Read (Read Only)
constant TRANSMISSION_READ_BITS: STD_LOGIC_VECTOR(8 downto 0) := "ZZZZZZZZ1";

-- SCCB Transmission End Condition Bits
constant TRANSMISSION_END_COND_BITS: STD_LOGIC_VECTOR(8 downto 0) := "01ZZZZZZZ";

-- SCCB Transmission Bit Counter (8 bits Addr/Data + 1 Don't Care bit = 9 bits -1 for process anticipation)
constant TRANSMISSION_BIT_COUNTER: UNSIGNED(2 downto 0) := "111";

-- SCCB Modes ('0': Read, '1': Write)
constant SCCB_READ_MODE: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- SCCB Controller States
TYPE sccbState is (	IDLE,
					START_COND,
					LOAD_SLAVE_ADDR, WRITE_SLAVE_ADDR,
					LOAD_REG_ADDR, WRITE_REG_ADDR, 
					LOAD_WRITE_REG_VALUE, WRITE_REG_VALUE, 
					LOAD_SLAVE_ADDR_READ, WRITE_SLAVE_ADDR_READ,
					READ_REG_VALUE_START, READ_REG_VALUE,
					LOAD_END_COND, END_COND,
					END_TX);
signal state: sccbState := IDLE;
signal next_state: sccbState;

-- SCCB Input Registers
signal mode: STD_LOGIC := '0';
signal addr: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal reg_addr: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal reg_value: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal start: STD_LOGIC := '0';

-- SCCB Transmission Bit Counter
signal bit_counter: UNSIGNED(2 downto 0) := (others => '0');

-- SCCB Clock Divider
signal scl_divider: INTEGER range 0 to SCL_CLOCK_DIVIDER-1 := 0;
signal scl_enable: STD_LOGIC := '0';
signal scl_enable_x2: STD_LOGIC := '0';

-- SCCB SCL
signal scl_reg: STD_LOGIC := '1';

-- SCCB SDA Reg (8 bits Addr/Data + 1 Don't Care bit = 9 bits)
signal sda_reg_sync: STD_LOGIC := '0';
signal sda_reg: STD_LOGIC_VECTOR(8 downto 0) := (others => '0');

-- SCCB SDA (Active at last quarter of SCL)
signal sda_value: STD_LOGIC := 'Z';

-- SCCB Read Value Valid & Value
signal read_value_valid_reg: STD_LOGIC := '0';
signal read_sda_value_sync: STD_LOGIC_VECTOR(1 downto 0) := "00";
signal read_value_reg: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	---------------------------------------
	-- SCCB Clock Divider & Clock Enable --
	---------------------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			
			-- Reset Clock Divider & Enable Clock Enable
			if (scl_divider = SCL_CLOCK_DIVIDER-1) then
				scl_divider <= 0;
				scl_enable <= '1';
				scl_enable_x2 <= '1';
			
			-- Increment Clock Divider & Disable Clock Enable
			else
				scl_divider <= scl_divider +1;
				scl_enable <= '0';
				scl_enable_x2 <= '0';

				-- Enable Clock Enable x2
				if (scl_divider = SCL_CLOCK_DIVIDER_X2-1) then
					scl_enable_x2 <= '1';
				end if;
			end if;
		end if;
	end process;

	-------------------------
	-- SCCB Inputs Handler --
	-------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			
			if (state = IDLE) then
				mode <= i_mode;
				addr <= i_addr;
				reg_addr <= i_reg_addr;
				reg_value <= i_reg_value;

				-- Handle Pulse (clear value when scl_enable at '1')
				start <= (i_start or start) and not (scl_enable);
			end if;
		end if;
	end process;

	------------------------
	-- SCCB State Machine --
	------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (scl_enable = '1') then
				-- Next State
				state <= next_state;
			end if;
		end if;
	end process;

	-- SCCB Next State
	process(state, mode, start, bit_counter)
	begin

		case state is
			when IDLE => 	if (start = '1') then
								next_state <= START_COND;
							else
								next_state <= IDLE;
							end if;
			
			-- Start Condition
			when START_COND => next_state <= LOAD_SLAVE_ADDR;

			-- Slave Address
			when LOAD_SLAVE_ADDR => next_state <= WRITE_SLAVE_ADDR;
            when WRITE_SLAVE_ADDR =>	if (bit_counter = TRANSMISSION_BIT_COUNTER) then
											next_state <= LOAD_REG_ADDR;
										else
											next_state <= WRITE_SLAVE_ADDR;
										end if;

			-- Register Address
			when LOAD_REG_ADDR => next_state <= WRITE_REG_ADDR;
            when WRITE_REG_ADDR =>		if (bit_counter = TRANSMISSION_BIT_COUNTER) then

											-- Read Mode
											if (mode = SCCB_READ_MODE) then
												next_state <= LOAD_SLAVE_ADDR_READ;
										
											-- Write Mode
											else
												next_state <= LOAD_WRITE_REG_VALUE;
											end if;
										
										else
											next_state <= WRITE_REG_ADDR;
										end if;

			-- Read Mode (Slave Address)
			when LOAD_SLAVE_ADDR_READ => next_state <= WRITE_SLAVE_ADDR_READ;
            when WRITE_SLAVE_ADDR_READ =>
										if (bit_counter = TRANSMISSION_BIT_COUNTER) then
											next_state <= READ_REG_VALUE_START;
										else
											next_state <= WRITE_SLAVE_ADDR_READ;
										end if;

			-- Read Mode (Read Register Value)
			when READ_REG_VALUE_START => next_state <= READ_REG_VALUE;
            when READ_REG_VALUE =>		if (bit_counter = TRANSMISSION_BIT_COUNTER) then
											next_state <= LOAD_END_COND;
										else
											next_state <= READ_REG_VALUE;
										end if;

			-- Write Mode (Write Register Value)
			when LOAD_WRITE_REG_VALUE => next_state <= WRITE_REG_VALUE;
            when WRITE_REG_VALUE =>		if (bit_counter = TRANSMISSION_BIT_COUNTER) then
											next_state <= LOAD_END_COND;
										else
											next_state <= WRITE_REG_VALUE;
										end if;

			-- End Condition
			when LOAD_END_COND => next_state <= END_COND;
			when END_COND => next_state <= END_TX;

			when others => next_state <= IDLE;
		end case;
	end process;

	----------------------
	-- SCCB Bit Counter --
	----------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (scl_enable = '1') then
				
				-- Increment Counter
				if (state = WRITE_SLAVE_ADDR) or (state = WRITE_REG_ADDR) or 
					(state = WRITE_SLAVE_ADDR_READ) or (state = READ_REG_VALUE) or (state = WRITE_REG_VALUE) then
					bit_counter <= bit_counter +1;
				
				-- Reset Counter
				else
					bit_counter <= (others => '0');
				end if;
			end if;
        end if;
    end process;

	---------------------------
	-- SCCB Controller Ready --
	---------------------------
	o_ready <= '1' when state = IDLE else '0';

	-----------------------
	-- SCCB SCL Register --
	-----------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Reset SCL (IDLE or END_TX)
			if (state = IDLE) or (state = START_COND) or (state = END_COND) or (state = END_TX) then
				scl_reg <= '1';

			-- SCL Clock
			elsif (scl_enable_x2 = '1') then
				scl_reg <= not(scl_reg);
			end if;
		end if;
	end process;
	o_scl <= scl_reg;

	----------------------------
	-- SCCB SDA Register Sync --
	----------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Enable SDA Register Sync at last quarter of SCL (-3 to match SCL timing)
			if (scl_divider = SCL_CLOCK_DIVIDER_X4_2-3) then
				sda_reg_sync <= '1';
			else
				sda_reg_sync <= '0';
			end if;
		end if;
	end process;

	-----------------------
	-- SCCB SDA Register --
	-----------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (sda_reg_sync = '1') then

				case state is
					-- Load Slave Address
					when LOAD_SLAVE_ADDR | LOAD_SLAVE_ADDR_READ => sda_reg <= addr & TRANSMISSION_DONT_CARE_BIT;

					-- Load Register Address
					when LOAD_REG_ADDR => sda_reg <= reg_addr & TRANSMISSION_DONT_CARE_BIT;

					-- Load Register Value to Write
					when LOAD_WRITE_REG_VALUE => sda_reg <= reg_value & TRANSMISSION_DONT_CARE_BIT;

					-- Load Register during Read Process
					when READ_REG_VALUE_START => sda_reg <= TRANSMISSION_READ_BITS;

					-- Load End Condition
					when LOAD_END_COND => sda_reg <= TRANSMISSION_END_COND_BITS;

					-- Shift States
					when WRITE_SLAVE_ADDR | WRITE_SLAVE_ADDR_READ | WRITE_REG_ADDR | WRITE_REG_VALUE | READ_REG_VALUE | END_COND => sda_reg <= sda_reg(7 downto 0) & '0';

					-- Others States
					when others => sda_reg <= (others => '0');
				end case;
			end if;
		end if;
	end process;

	-----------------------
	-- SCCB SDA (Output) --
	-----------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- IDLE & END States
			if (state = IDLE) or (state = END_TX) then
				sda_value <= 'Z';

			-- Start Condition State
			elsif (state = START_COND) then
				sda_value <= '1';

			-- Value States
			else
				sda_value <= sda_reg(8);
			end if;
		end if;
	end process;
	io_sda <= sda_value;

	---------------------------
	-- SCCB Read Value Valid --
	---------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Read Value NOT valid
			if (state = START_COND) then
				read_value_valid_reg <= '0';
			
			-- Read Value valid
			elsif (mode = SCCB_READ_MODE) and (state = END_TX) then
				read_value_valid_reg <= '1';
			end if;
		end if;
	end process;
	o_read_value_valid <= read_value_valid_reg;

	---------------------
	-- SCCB Read Value --
	---------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Detect Rising Edge of SCL
			read_sda_value_sync(0) <= scl_reg;
			read_sda_value_sync(1) <= not(read_sda_value_sync(0)) and scl_reg;

			-- Clock Enable & Read State
			if (read_sda_value_sync(1) = '1') and (state = READ_REG_VALUE) then
				read_value_reg <= read_value_reg(6 downto 0) & io_sda;
			end if;
		end if;
	end process;
	o_read_value <= read_value_reg;

end Behavioral;