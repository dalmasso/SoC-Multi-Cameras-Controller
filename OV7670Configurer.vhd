------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 28/08/2024
-- Module Name: OV7670Configurer
-- Description:
--      Serial Camera Control Bus (SCCB) Controller for OV7670 Camera Module
--		Input 	-	i_clock_12M: Clock (12MHz)
--		Input 	-	i_reset: Reset ('0': NO Reset, '1': Reset)
--		Output 	-	o_end_of_config: End of OV7670 Configuration process ('0': In Progress, '1': End of Configuration)
--		Output 	-	o_ov7670_scl: OV7670 Serial Clock (400KHz)
--		Output 	-	o_ov7670_sda: OV7670 Serial Data
--		Output 	-	o_ov7670_reset: OV7670 Configuration - Reset All Registers ('0': Reset, '1': No Reset)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY OV7670Configurer is
PORT(
	i_clock_12M: IN STD_LOGIC;
	i_reset: IN STD_LOGIC;
	o_end_of_config: OUT STD_LOGIC;
	o_ov7670_scl: OUT STD_LOGIC;
	o_ov7670_sda: OUT STD_LOGIC;
	o_ov7670_reset: OUT STD_LOGIC
);
END OV7670Configurer;

ARCHITECTURE Behavioral of OV7670Configurer is

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- OV7670 SCCB Transmission Next Phase (3x9 = 27-1 bits - 1 for anticipation)
constant TRANSMISSION_NEXT_PHASE: UNSIGNED(4 downto 0) := "11001";

-- OV7670 SCCB Transmission Don't Care bit
constant TRANSMISSION_DONT_CARE_BIT: STD_LOGIC := '0';

-- OV7670 SCCB Write Address (0x42 + Don't Care bit)
constant OV7670_WRITE_ADDR: STD_LOGIC_VECTOR(8 downto 0) := X"21" & TRANSMISSION_DONT_CARE_BIT;

-- OV7670 SCCB Register: HSTART Address & Value (156) (+ Don't Care bit)
constant REGISTER_HSTART_ADDR: STD_LOGIC_VECTOR(8 downto 0) := X"17" & TRANSMISSION_DONT_CARE_BIT;
constant REGISTER_HSTART_VALUE: STD_LOGIC_VECTOR(8 downto 0) := X"13" & TRANSMISSION_DONT_CARE_BIT;

-- OV7670 SCCB Register: HSTOP Address & Value (14) (+ Don't Care bit)
constant REGISTER_HSTOP_ADDR: STD_LOGIC_VECTOR(8 downto 0) := X"18" & TRANSMISSION_DONT_CARE_BIT;
constant REGISTER_HSTOP_VALUE: STD_LOGIC_VECTOR(8 downto 0) := X"01" & TRANSMISSION_DONT_CARE_BIT;

-- OV7670 SCCB Register: HREF Address & Value (HSTOP[2:0], HSTART[2:0]) (+ Don't Care bit)
constant REGISTER_HREF_ADDR: STD_LOGIC_VECTOR(8 downto 0) := X"32" & TRANSMISSION_DONT_CARE_BIT;
constant REGISTER_HREF_VALUE: STD_LOGIC_VECTOR(8 downto 0) := X"B6" & TRANSMISSION_DONT_CARE_BIT;

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- OV7670 SCCB State Machine
TYPE ov7670SCCBState is (IDLE, WRITE_HSTART_LOAD, WRITE_HSTART, WRITE_HSTOP_LOAD, WRITE_HSTOP, WRITE_HREF_LOAD, WRITE_HREF, END_OF_TRANSMISSION, END_OF_CONFIG);
signal state: ov7670SCCBState := IDLE;
signal next_state: ov7670SCCBState;

-- OV7670 SCCB Clock Divider (12MHz -> 187.5KHz Clock: 64 --> 6 bits)
signal ov7670_scl_divider: UNSIGNED(5 downto 0) := (others => '0');
signal ov7670_scl_divider_last_bit: STD_LOGIC := '0';
signal ov7670_scl_enable: STD_LOGIC := '0';
signal ov7670_scl: STD_LOGIC := '1';

-- OV7670 SCCB Data (MUST be set to '1' when unused)
signal ov7670_sda_reg: STD_LOGIC_VECTOR(26 downto 0) := (others => '0');

-- OV7670 SCCB Transmission Bit Counter
signal bit_counter: UNSIGNED(4 downto 0) := (others => '0');

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	-------------------------------
	-- OV7670 SCCB Clock Divider --
	-------------------------------
	process(i_clock_12M)
	begin
		if rising_edge(i_clock_12M) then

			-- Reset Clock Divider
			if (i_reset = '1') then
				ov7670_scl_divider <= (others => '0');

			-- Increment Clock Divier
			else
				ov7670_scl_divider <= ov7670_scl_divider +1;
			end if;
		end if;
	end process;

	------------------------------
	-- OV7670 SCCB Clock Enable --
	------------------------------
	process(i_clock_12M)
	begin
		if rising_edge(i_clock_12M) then

			-- Last SCL Divider bit
			ov7670_scl_divider_last_bit <= ov7670_scl_divider(5);

			-- Reset Clock Enable
			if (i_reset = '1') then
				ov7670_scl_enable <= '0';
			
			-- Set Clock Enable (detect Rising Edge)
			else
				ov7670_scl_enable <= not(ov7670_scl_divider_last_bit) and ov7670_scl_divider(5);
			end if;
		end if;
	end process;

	-------------------------------
	-- OV7670 SCCB State Machine --
	-------------------------------
	-- OV7670 SCCB State
	process(i_clock_12M)
	begin
       if rising_edge(i_clock_12M) then

			-- Reset
			if (i_reset = '1') then
            	state <= IDLE;

			-- Clock Enable
			elsif (ov7670_scl_enable = '1') then
            	state <= next_state;
			end if;

		end if;
	end process;

	-- OV7670 SCCB Next State
	process(bit_counter, state)
	begin

		case state is
			when IDLE => next_state <= WRITE_HSTART_LOAD;

			when WRITE_HSTART_LOAD => next_state <= WRITE_HSTART;
            when WRITE_HSTART =>
									if (bit_counter = TRANSMISSION_NEXT_PHASE) then
										next_state <= WRITE_HSTOP_LOAD;
									else
										next_state <= WRITE_HSTART;
									end if;

			when WRITE_HSTOP_LOAD => next_state <= WRITE_HSTOP;
            when WRITE_HSTOP =>
									if (bit_counter = TRANSMISSION_NEXT_PHASE) then
										next_state <= WRITE_HREF_LOAD;
									else
										next_state <= WRITE_HSTOP;
									end if;

			when WRITE_HREF_LOAD => next_state <= WRITE_HREF;
            when WRITE_HREF =>
									if (bit_counter = TRANSMISSION_NEXT_PHASE) then
										next_state <= END_OF_TRANSMISSION;
									else
										next_state <= WRITE_HREF;
									end if;

			when END_OF_TRANSMISSION => next_state <= END_OF_CONFIG;
            when END_OF_CONFIG => next_state <= END_OF_CONFIG;
			when others => next_state <= IDLE;
		end case;
	end process;

	-------------------------------
	-- OV7670 SCCB State Counter --
	-------------------------------
	process(i_clock_12M)
	begin
		if rising_edge(i_clock_12M) then

			-- Restart Counter
			if (state = WRITE_HSTART_LOAD) or (state = WRITE_HSTOP_LOAD) or (state = WRITE_HREF_LOAD) then
				bit_counter <= (others => '0');
				
			elsif (ov7670_scl_enable = '1') then
				-- Increment Counter when Clock enable
				bit_counter <= bit_counter +1;
			end if;
        end if;
    end process;

	--------------------------------------
	-- OV7670 SCCB End Of Configuration --
	--------------------------------------
	o_end_of_config <= '1' when state = END_OF_CONFIG else '0';

	--------------------------------
	-- OV7670 Reset Configuration --
	--------------------------------
	o_ov7670_reset <= '0' when state = IDLE else '1';

	------------------------------
	-- OV7670 SCCB Output Clock --
	------------------------------
	process(i_clock_12M)
	begin
		if rising_edge(i_clock_12M) then

			-- Reset SCL (Reset or SDA Not Ready)
			if (state = IDLE) or (state = END_OF_CONFIG) then
				ov7670_scl <= '1';
			
			-- Init SCL
			elsif (state = WRITE_HSTART_LOAD) then
				ov7670_scl <= '0';

			-- SCL Clock
			else
				ov7670_scl <= ov7670_scl_divider_last_bit;
			end if;
		end if;
	end process;
	o_ov7670_scl <= ov7670_scl;

	-----------------------------
	-- OV7670 SCCB Output Data --
	-----------------------------
	process(i_clock_12M)
	begin
		if rising_edge(i_clock_12M) then

			-- Clock Enable
			if (ov7670_scl_enable = '1') then

				case state is	
					-- Load States
					when WRITE_HSTART_LOAD => ov7670_sda_reg <= OV7670_WRITE_ADDR & REGISTER_HSTART_ADDR & REGISTER_HSTART_VALUE;
					when WRITE_HSTOP_LOAD => ov7670_sda_reg <= OV7670_WRITE_ADDR & REGISTER_HSTOP_ADDR & REGISTER_HSTOP_VALUE;
					when WRITE_HREF_LOAD => ov7670_sda_reg <= OV7670_WRITE_ADDR & REGISTER_HREF_ADDR & REGISTER_HREF_VALUE;

					-- Shift States
					when WRITE_HSTART | WRITE_HSTOP | WRITE_HREF => ov7670_sda_reg <= ov7670_sda_reg(25 downto 0) & '0';

					-- Reset States
					when others => ov7670_sda_reg <= (others => '0');
				end case;
			end if;
		end if;
	end process;
	o_ov7670_sda <= ov7670_sda_reg(26);

end Behavioral;