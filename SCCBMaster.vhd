------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 07/10/2024
-- Module Name: SCCBMaster
-- Description:
--      Serial Camera Control Bus (SCCB) Master Controller for OV7670 Camera Module.
--		Supports:
--			- Write Mode: 3-Phase Write Transmission
--			- Read Mode: 2-Phase Write Transmission, then 2-Phase Read Transmission
--
-- Usage:
--		1. Set the inputs (keep unchanged until Ready signal is de-asserted)
--			* Mode (Read/Write)
--			* SCCB Slave Addresss
--			* Register Address
--			* Register Value (Write Mode only)
--		2. Asserts Start input (available only when the SCCB Master is Ready)
--		3. SCCB Master de-assert the Ready signal
--		4. SCCB Master re-asserts ths Ready signal at the end of transmission ans it is ready for a new transmission
--		5. In Read mode only, the read value is available when its validity signal is asserted
--
-- WARNING: /!\ Require Pull-Up on SCL and SDA pins /!\
--
-- Generics
--		Input	-	input_clock: Master Clock Frequency
--		Output	-	sccb_clock: SCCB Clock Frequency
-- Ports
--		Input 	-	i_clock: Input Clock
--		Input 	-	i_mode: Read or Write Mode ('0': Write, '1': Read)
--		Input 	-	i_addr: Address of the SCCB Slave (7 bits)
--		Input 	-	i_reg_addr: Address of the Register to Read/Write
--		Input 	-	i_reg_value: Value of the Register to Write
--		Input 	-	i_start: Start SCCB Transmission ('0': No Start, '1': Start)
--		Output 	-	o_ready: Ready State of SCCB Controller ('0': Not Ready, '1': Ready)
--		Output 	-	o_read_value_valid: Validity of value of the SCCB Slave Register ('0': Not Valid, '1': Valid)
--		Output 	-	o_read_value: Value of the SCCB Slave Register
--		Output 	-	o_scl: OV7670 Serial Clock ('0'-'Z'('1') values, working with Pull-Up)
--		In/Out 	-	io_sda: OV7670 Serial Data ('0'-'Z'('1') values, working with Pull-Up)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY SCCBMaster is

GENERIC(
	input_clock: INTEGER := 12_000_000;
	sccb_clock: INTEGER := 100_000
);

PORT(
	i_clock: IN STD_LOGIC;
	i_mode: IN STD_LOGIC;
	i_addr: IN STD_LOGIC_VECTOR(6 downto 0);
	i_reg_addr: IN STD_LOGIC_VECTOR(7 downto 0);
	i_reg_value: IN STD_LOGIC_VECTOR(7 downto 0);
	i_start: IN STD_LOGIC;
	o_ready: OUT STD_LOGIC;
	o_read_value_valid: OUT STD_LOGIC;
	o_read_value: OUT STD_LOGIC_VECTOR(7 downto 0);
	o_scl: OUT STD_LOGIC;
	io_sda: INOUT STD_LOGIC
);

END SCCBMaster;

ARCHITECTURE Behavioral of SCCBMaster is

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- SCCB Clock Dividers
constant CLOCK_DIV: INTEGER := input_clock / sccb_clock;
constant CLOCK_DIV_X2: INTEGER := CLOCK_DIV /2;

-- SCCB Modes ('0': Write, '1': Read)
constant SCCB_WRITE_MODE: STD_LOGIC := '0';
constant SCCB_READ_MODE: STD_LOGIC := '1';

-- SCCB Transmission Start Bit
constant TRANSMISSION_START_BIT: STD_LOGIC := '0';

-- SCCB Transmission Don'T Care Bit ('Z' with Pull-Up)
constant TRANSMISSION_DONT_CARE_BIT: STD_LOGIC := '1';

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- SCCB Controller States
TYPE sccbState is (IDLE, START_TX, WRITE_SLAVE_ADDR, REGISTER_WRITE, REGISTER_READ, WRITE_REG_VALUE, PREP_READ_PHASE, END_TX);
signal state: sccbState := IDLE;
signal next_state: sccbState;

-- SCCB Input Registers
signal mode: STD_LOGIC_VECTOR(1 downto 0) := "00";
signal addr: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal reg_addr: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal reg_value: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal start: STD_LOGIC := '0';

-- SCCB Transmission Bit Counter (8 cycles per phase)
signal bit_counter: UNSIGNED(3 downto 0) := (others => '0');

-- SCCB Clock Divider
signal clock_divider: INTEGER range 0 to CLOCK_DIV-1 := 0;
signal clock_enable: STD_LOGIC := '0';
signal clock_enable_x2: STD_LOGIC := '0';

-- SCCB SCL Register
signal scl_reg: STD_LOGIC := '0';
signal scl_enable: STD_LOGIC := '0';

-- SCCB SDA Output
signal sda_out: STD_LOGIC := TRANSMISSION_DONT_CARE_BIT;

-- SCCB SDA Input (Valid & Value)
signal sda_in_valid: STD_LOGIC := '0';
signal sda_in_enable: STD_LOGIC := '0';
signal sda_in_reg: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	------------------------
	-- SCCB Clock Divider --
	------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			
			-- Reset Clock Divider
			if (clock_divider = CLOCK_DIV-1) then
				clock_divider <= 0;
			
			-- Increment Clock Divider
			else
				clock_divider <= clock_divider +1;
			end if;
		end if;
	end process;

	------------------------
	-- SCCB Clock Enables --
	------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then
			
			-- Clock Enable
			if (clock_divider = CLOCK_DIV-1) then
				clock_enable <= '1';
				clock_enable_x2 <= '1';
			else
				clock_enable <= '0';
				clock_enable_x2 <= '0';
			end if;

			-- Clock Enable 1/2
			if (clock_divider = CLOCK_DIV_X2-1) then
				clock_enable_x2 <= '1';
			end if;

		end if;
	end process;

	------------------------
	-- SCCB Start Handler --
	------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Handle Pulse (clear value when clock_enable at '1')
			if (state = IDLE) then	
				start <= (i_start or start) and not (clock_enable);
			end if;
		end if;
	end process;

	------------------------
	-- SCCB State Machine --
	------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Next State (when Clock Enable)
			if (clock_enable = '1') then
				state <= next_state;
			end if;
		end if;
	end process;

	-- SCCB Next State
	process(state, start, bit_counter, mode)
	begin

		case state is
			when IDLE => 	if (start = '1') then
								next_state <= START_TX;
							else
								next_state <= IDLE;
							end if;
			
			-- Start of Transmission
			when START_TX => next_state <= WRITE_SLAVE_ADDR;

			-- Write Slave Address
			when WRITE_SLAVE_ADDR =>
							if (bit_counter(3) = '1') then

								-- Read Mode (Current Phase)
								if (mode(0) = SCCB_READ_MODE) then
									next_state <= REGISTER_READ;
								
								-- Write Mode
								else
									next_state <= REGISTER_WRITE;
								end if;
							else
								next_state <= WRITE_SLAVE_ADDR;
							end if;
			
			-- Write Register Value
			when REGISTER_WRITE =>
							if (bit_counter(3) = '1') then

								-- Read Mode (for Next Phase)
								if (mode(1) = SCCB_READ_MODE) then
									next_state <= END_TX;
								
								-- Write Mode
								else
									next_state <= WRITE_REG_VALUE;
								end if;
							else
								next_state <= REGISTER_WRITE;
							end if;

			-- Write Register Value
			when WRITE_REG_VALUE =>
							if (bit_counter(3) = '1') then
								next_state <= END_TX;
							else
								next_state <= WRITE_REG_VALUE;
							end if;

			-- Read Register Value
			when REGISTER_READ =>
							if (bit_counter(3) = '1') then
								next_state <= END_TX;
							else
								next_state <= REGISTER_READ;
							end if;

			-- End of Transmission
			when END_TX => 	-- Read Mode (for Next Phase)
							if (mode(1) = SCCB_READ_MODE) then
								next_state <= PREP_READ_PHASE;

							else
								next_state <= IDLE;
							end if;

			-- Prepare Read Phase
			when PREP_READ_PHASE => next_state <= START_TX;

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
			if (clock_enable = '1') then

				-- Reset Counter
				if (state = START_TX) or (bit_counter(3) = '1') then
					bit_counter <= (others => '0');
				else
					bit_counter <= bit_counter +1;
				end if;
			end if;
        end if;
    end process;

	----------------------------
	-- Slave Address Register --
	----------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_enable = '1') then

				-- Load Slave Address Input (Handle continuous left-shift)
				if (state = IDLE) then
					addr <= SCCB_WRITE_MODE & i_addr;
			
				-- Slave Address Read Mode (Handle continuous left-shift)
				elsif (state = PREP_READ_PHASE) then
					addr <= SCCB_READ_MODE & addr(2 downto 0) & addr(7 downto 4);
				
				-- Left-Shift
				else
					addr <= addr(6 downto 0) & addr(7);
				end if;
			end if;
		end if;
	end process;

	-------------------------------
	-- Register Address Register --
	-------------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_enable = '1') then

				-- Load Register Address Input (Handle continuous left-shift)
				if (state = IDLE) then
					reg_addr <= i_reg_addr(1 downto 0) & i_reg_addr(7 downto 2);
				
				-- Left-Shift
				else
					reg_addr <= reg_addr(6 downto 0) & reg_addr(7);
				end if;
			end if;
		end if;
	end process;

	-----------------------------
	-- Register Value Register --
	-----------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_enable = '1') then

				-- Load Register Value Input (Handle continuous left-shift)
				if (state = IDLE) then
					reg_value <= i_reg_value(2 downto 0) & i_reg_value(7 downto 3);
				
				-- Left-Shift
				else
					reg_value <= reg_value(6 downto 0) & reg_value(7);
				end if;
			end if;
		end if;
	end process;

	-------------------
	-- Mode Register --
	-------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_enable = '1') then
				
				-- Load Mode Input
				if (state = IDLE) then
					mode <= i_mode & SCCB_WRITE_MODE;
			
				-- Right-Shift
				elsif (state = END_TX) then
					mode <= SCCB_WRITE_MODE & mode(1);
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

			-- Toggle SCL Reg
			if (clock_enable_x2 = '1') then
				scl_reg <= not(scl_reg);
			end if;
		end if;
	end process;

	---------------------
	-- SCCB SCL Enable --
	---------------------
	scl_enable <= '0' when state = IDLE or state = START_TX or state = PREP_READ_PHASE else '1';

	--------------
	-- SCCB SCL --
	--------------
	-- ('0' or 'Z' values)
	o_scl <= '0' when scl_reg = '0' and scl_enable = '1' else 'Z';

	-----------------------
	-- SCCB SDA Selector --
	-----------------------
	sda_out <=	TRANSMISSION_START_BIT when state = START_TX or state = END_TX else
				TRANSMISSION_DONT_CARE_BIT when bit_counter(3) = '1' else
				addr(7) when state = WRITE_SLAVE_ADDR else
				reg_addr(7) when state = REGISTER_WRITE else
				reg_value(7) when state = WRITE_REG_VALUE else
				TRANSMISSION_DONT_CARE_BIT;

	---------------------
	-- SCCB SDA Output --
	---------------------
	-- ('0' or 'Z' values)
	io_sda <= '0' when sda_out = '0' else 'Z';

	---------------------------
	-- SCCB SDA Input Enable --
	---------------------------
	sda_in_enable <= '1' when (state = REGISTER_READ) and (bit_counter(3) = '0') else '0';

	--------------------------
	-- SCCB SDA Input Value --
	--------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- SDA Input Enable
			if (clock_enable = '1') and (sda_in_enable = '1') then
				sda_in_reg <= sda_in_reg(6 downto 0) & io_sda;
			end if;
		end if;
	end process;

	---------------------------
	-- SCCB Read Value Valid --
	---------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Disable SDA Valid Data (New cycle)
			if (state = START_TX) then
				sda_in_valid <= '0';
			
			-- Read Value valid
			elsif (mode(0) = SCCB_READ_MODE) and (state = END_TX) then
				sda_in_valid <= '1';
			end if;
		end if;
	end process;
	o_read_value_valid <= sda_in_valid;

	---------------------
	-- SCCB Read Value --
	---------------------
	o_read_value <= sda_in_reg when sda_in_valid = '1' else (others => '0');

end Behavioral;