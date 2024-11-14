------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 11/11/2024
-- Module Name: SPIMaster
-- Description:
--      SPI Master allowing Write/Read operations on slave devices.
--		Features:
--          - CPOL Configuration
--          - CPHA Configuration
--          - Slave Select Polarity (active Low or High)
--          - 2-Byte Delay Interval (0 to 7 SPI Clock Cycles)
--          - Byte Number Configuration
--          - Daisy-Chain (Slaves MUST support this feature)
--
-- Usage:
--      The Ready signal indicates no operation is on going and the SPI Master is waiting operation.
--		The Busy signal indicates operation is on going.
--      Reset input can be trigger at any time to reset the SPI Master to the IDLE state.
--		1. Set all necessary inputs
--			* Byte Number (number of byte required to write/read)
--			* Byte Delay (number of SPI SCLK Clock Cycles between 2 bytes to write/read)
--			* Slave Select (set to '1' the Slave Select Line to enable)
--			* Data to Write
--      2. Asserts Start input. The Ready signal is de-asserted and the Busy signal is asserted.
--		3. SPI Master re-asserts the Ready signal at the end of transmission (Master is ready for a new transmission)
--		4. The read value is available when its validity signal is asserted
--
-- Generics
--      input_clock: Module Input Clock Frequency
--      spi_clock: SPI Serial Clock Frequency
--      cpol: SPI Clock Polarity ('0': SCLK IDLE at Low, '1': SCLK IDLE at High)
--      cpha: SPI Clock Phase ('0': Data valid on Leading/First Edge of SCLK, '1': Data valid on Trailing/Second Edge of SCLK)
--      ss_polarity: SPI Slave Select Polarity ('0': active Low, '1': active High)
--      ss_length: Number of Chip/Slave Select Lines
--      max_data_register_length: Maximum SPI Data Register Length in bits
--
-- Ports
--		Input 	-	i_clock: Module Input Clock
--		Input 	-	i_reset: Reset ('0': No Reset, '1': Reset)
--		Input 	-	i_byte_number: SPI Byte Number during the Transmission
--		Input 	-	i_byte_delay: SPI Delay between 2-Byte Transmission (0 to 7 SPI Clock Cycles)
--		Input 	-	i_slave_select: SPI Slave Selection ('0': Not Selected, '1': Selected)
--		Input 	-	i_start: Start SPI Transmission ('0': No Start, '1': Start)
--		Input 	-	i_write_value: Data to Write
--		Output 	-	o_read_value: Data Read from Slave
--		Output 	-	o_read_value_valid: Validity of the Data Read ('0': Not Valid, '1': Valid)
--		Output 	-	o_ready: Ready State of SPI Master ('0': Not Ready, '1': Ready)
--		Output 	-	o_busy: Busy State of SPI Master ('0': Not Busy, '1': Busy)
--		Output 	-	o_sclk: SPI Serial Clock
--		Output 	-	o_mosi: SPI Master Output Slave Input Data line
--		Input 	-	i_miso: SPI Master Input Slave Output Data line
--		Output 	-	o_ss: SPI Slave Select Line (inverted ss_polarity: Not Selected, ss_polarity: Selected)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.MATH_REAL."ceil";
USE IEEE.MATH_REAL."log2";

ENTITY SPIMaster is

GENERIC(
    input_clock: INTEGER := 12_000_000;
    spi_clock: INTEGER := 100_000;
    cpol: STD_LOGIC := '0';
    cpha: STD_LOGIC := '0';
    ss_polarity: STD_LOGIC := '0';
    ss_length: INTEGER := 1;
    max_data_register_length: INTEGER := 8
);

PORT(
    i_clock: IN STD_LOGIC;
    i_reset: IN STD_LOGIC;
    i_byte_number: IN INTEGER range 0 to 2*(max_data_register_length/8);
    i_byte_delay: IN INTEGER range 0 to 7;
    i_slave_select: IN STD_LOGIC_VECTOR(ss_length-1 downto 0);
    i_start: IN STD_LOGIC;
    i_write_value: IN STD_LOGIC_VECTOR(max_data_register_length-1 downto 0);
	o_read_value: OUT STD_LOGIC_VECTOR(max_data_register_length-1 downto 0);
    o_read_value_valid: OUT STD_LOGIC;
    o_ready: OUT STD_LOGIC;
    o_busy: OUT STD_LOGIC;
    o_sclk: OUT STD_LOGIC;
    o_mosi: OUT STD_LOGIC;
    i_miso: IN STD_LOGIC;
    o_ss: OUT STD_LOGIC_VECTOR(ss_length-1 downto 0)
);

END SPIMaster;

ARCHITECTURE Behavioral of SPIMaster is

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- SPI Clock Dividers
constant CLOCK_DIV: INTEGER := input_clock / spi_clock;
constant CLOCK_DIV_X2: INTEGER := CLOCK_DIV /2;

-- Bit Counter Length: Byte Number(≥1 bit) + SPI TX/RX cycle (3 bits)
constant BIT_COUNTER_LENGTH: INTEGER := INTEGER(ceil(log2(real(max_data_register_length/8)))) +1 +2;

-- SPI MOSI IDLE Bit
constant MOSI_IDLE_BIT: STD_LOGIC := '0';

-- SPI Left Shift Empty Data Value
constant LEFT_SHIFT_EMPTY_VALUE: STD_LOGIC := '0';

-- SPI Disable Slave Select Bit
constant DISABLE_SS_BIT: STD_LOGIC := not(ss_polarity);

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- SPI Master States
TYPE spiState is (IDLE, START_TX, BYTE_TXRX, WAITING, STOP_TX);
signal state: spiState := IDLE;
signal next_state: spiState;

-- SPI Clock Divider
signal clock_divider: INTEGER range 0 to CLOCK_DIV-1 := 0;
signal clock_enable: STD_LOGIC := '0';
signal clock_enable_x2: STD_LOGIC := '0';

-- SPI Transmission Bit Counter (8 cycles per phase, 1 phase = 1 Byte)
signal bit_counter: UNSIGNED(BIT_COUNTER_LENGTH downto 0) := (others => '0');
signal bit_counter_end: STD_LOGIC := '0';
signal delay_counter_end: STD_LOGIC := '0';
signal byte_counter_end: STD_LOGIC := '0';

-- SPI SCLK
signal sclk_out: STD_LOGIC := '0';

-- SPI Write/Read Data
signal write_value_reg: STD_LOGIC_VECTOR(max_data_register_length-1 downto 0) := (others => '0');
signal read_value_reg: STD_LOGIC_VECTOR(max_data_register_length-1 downto 0) := (others => '0');
signal read_value_valid: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	-----------------------
	-- SPI Clock Divider --
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
	-- SPI Clock Enables --
	-----------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_divider = CLOCK_DIV-1) then
				clock_enable <= '1';
			else
				clock_enable <= '0';
			end if;

			-- Clock Enable x2 (1/2)
			if (clock_divider = CLOCK_DIV_X2-1) then
				clock_enable_x2 <= '1';
			else
				clock_enable_x2 <= '0';
			end if;

		end if;
	end process;

	-----------------------
	-- SPI State Machine --
	-----------------------
    -- SPI State
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Reset State
			if (i_reset = '1') then
				state <= IDLE;
			
			-- Next State (When Clock Enable)
			elsif (clock_enable = '1') then
				state <= next_state;
			end if;
		end if;
	end process;

	-- SPI Next State
	process(state, i_start, i_byte_delay, bit_counter_end, byte_counter_end, delay_counter_end)
	begin

		case state is
			when IDLE =>    if (i_start = '1') then
                                next_state <= START_TX;
                            else
                                next_state <= IDLE;
							end if;
            
            -- Start TX
            when START_TX => next_state <= BYTE_TXRX;

			-- TX/RX Cycle
			when BYTE_TXRX =>
							-- End of TX/RX Cycle
							if (bit_counter_end = '1') then

								-- End of Transmission
								if (byte_counter_end = '1') then
									next_state <= STOP_TX;
								
								-- No 2-Byte Delay
								elsif (i_byte_delay = 0) then
									next_state <= BYTE_TXRX;
								
								-- 2-Byte Delay
								else
									next_state <= WAITING;
								end if;
							else
								next_state <= BYTE_TXRX;
							end if;

			-- Waiting Cycle
			when WAITING =>
                            -- Next Write Cycle
                            if (delay_counter_end = '1') then
                                next_state <= BYTE_TXRX;
                            
                            else
                                next_state <= WAITING;
                            end if;

            -- End of Transmission
			when others => next_state <= IDLE;
		end case;
	end process;
	
	----------------
	-- SPI Status --
	----------------
	o_ready <= '1' when state = IDLE else '0';
	o_busy <= '1' when state = BYTE_TXRX else '0';

	----------------------------
	-- SPI Bit & Byte Counter --
	----------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Clock Enable
			if (clock_enable = '1') then

                -- Reset Bit & Byte Counter
                if (state = IDLE) then
                    bit_counter <= (others => '0');

                -- Reset Bit Counter
                elsif (delay_counter_end = '1') or ((state /= BYTE_TXRX) and (state /= WAITING)) then
                    bit_counter(2 downto 0) <= (others => '0');
				
				-- Increment Bit Counter
				else
					bit_counter <= bit_counter +1;
				end if;
			end if;
		end if;
    end process;

	-- Bit Counter End
	bit_counter_end <= bit_counter(2) and bit_counter(1) and bit_counter(0);

    -- Delay Counter End
    delay_counter_end <= '1' when (bit_counter(2 downto 0) = i_byte_delay-1) and (state = WAITING) else '0';

	-- Byte Counter End
	byte_counter_end <= '1' when (i_byte_number = 0) or (i_byte_number = 1) else
						'1' when (bit_counter(BIT_COUNTER_LENGTH downto 3) = i_byte_number-1) else '0';

    ---------------------
	-- SPI SCLK Output --
	---------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- SCLK Edge 1
			if (clock_enable = '1') then

				-- CPHA Mode '0'
				if (cpha = '0') then
					sclk_out <= cpol;
				else
					sclk_out <= not(cpol);
				end if;
			
			-- SCLK Edge 2
			elsif (clock_enable_x2 = '1') then

				-- CPHA Mode '1'
				if (cpha = '0') then
					sclk_out <= not(cpol);
				else
					sclk_out <= cpol;
				end if;				
			end if;
		end if;
	end process;
	o_sclk <= sclk_out when state = BYTE_TXRX else cpol;

	----------------------------
	-- SPI Write Value (MOSI) --
	----------------------------
	process(i_clock)
	begin

		if rising_edge(i_clock) then
			
			-- Load Write Value
			if (state = START_TX) then
				write_value_reg <= i_write_value;

			-- Left-Shift Data Enable
			elsif (state = BYTE_TXRX) and (clock_enable = '1') then
				write_value_reg <= write_value_reg(max_data_register_length-2 downto 0) & LEFT_SHIFT_EMPTY_VALUE;
			end if;

		end if;
	end process;
	o_mosi <= write_value_reg(max_data_register_length-1) when (state = BYTE_TXRX) or (state = WAITING) else MOSI_IDLE_BIT;

	---------------------------
	-- SPI Read Value (MISO) --
	---------------------------
	process(i_clock)
	begin

		if rising_edge(i_clock) then

			-- Sampling Read Data Enable
			if (clock_enable_x2 = '1') then
    			
                -- Add New MISO Value & Left-Shift
				if (state = BYTE_TXRX) then
                	read_value_reg <= read_value_reg(max_data_register_length-2 downto 0) & i_miso;
				end if;

            end if;
        end if;
	end process;
    o_read_value <= read_value_reg;

	--------------------------
	-- SPI Read Value Valid --
	--------------------------
	process(i_clock)
	begin
		if rising_edge(i_clock) then

			-- Enable Read Value Valid (End of TX/RX Cycle)
			if (state = STOP_TX) then
				read_value_valid <= '1';

			-- Disable Read Value Valid (New cycle)
			elsif (state = START_TX) then
				read_value_valid <= '0';
			end if;

		end if;
	end process;
	o_read_value_valid <= read_value_valid;

	---------------------------
	-- SPI Slave Select Line --
	---------------------------
	SPISlaveSelect: for i in 0 to ss_length-1 generate
		o_ss(i) <= DISABLE_SS_BIT when (state = IDLE) or i_slave_select(i) = '0' else not(DISABLE_SS_BIT);
	end generate SPISlaveSelect;

end Behavioral;