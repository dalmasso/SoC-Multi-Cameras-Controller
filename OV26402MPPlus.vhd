------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 23/01/2025
-- Module Name: OV26402MPPlus
-- Description:
--      OV2640 2MP Plus Camera Module controller
--		Input 	-	i_clock_8M: Clock (8MHz)
--		Input 	-	i_reset: Reset ('0': NO Reset, '1': Reset)
--		Input 	-	i_image_capture: Triggers Image Capture ('0': No Capture, '1': Capture)
--		Output 	-	o_image_data_enable: OV2640 Image Output Data Enable ('0': Disable, '1': Enable)
--		Output 	-	o_image_data: OV2640 Image Output Data (8-bit)
--		In/Out 	-	io_scl: OV2640 I2C Serial Clock ('0'-'Z'(as '1') values, working with Pull-Up)
--		In/Out 	-	io_sda: OV2640 I2C Serial Data ('0'-'Z'(as '1') values, working with Pull-Up)
--		Output 	-	o_sclk: OV2640 SPI Serial Clock
--		Output 	-	o_mosi: OV2640 SPI Master Output Slave Input Data line
--		Input 	-	i_miso: OV2640 SPI Master Input Slave Output Data line
--		Output 	-	o_ss: OV2640 SPI Slave Select Line (inverted ss_polarity: Not Selected, ss_polarity: Selected)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY OV26402MPPlus is
PORT(
	i_clock_8M: IN STD_LOGIC;
	i_reset: IN STD_LOGIC;
	-- OV2640 Image Controller
    i_image_capture: IN STD_LOGIC;
	o_image_data_enable: OUT STD_LOGIC;
	o_image_data: OUT STD_LOGIC_VECTOR(7 downto 0);
	-- OV2640 Image Sensor Configuration (I2C)
	io_scl: INOUT STD_LOGIC;
	io_sda: INOUT STD_LOGIC;
    -- OV2640 Image Data (SPI)
    o_sclk: OUT STD_LOGIC;
    o_mosi: OUT STD_LOGIC;
    i_miso: IN STD_LOGIC;
    o_ss: OUT STD_LOGIC_VECTOR(ss_length-1 downto 0)
);
END OV26402MPPlus;

ARCHITECTURE Behavioral of OV26402MPPlus is

------------------------------------------------------------------------
-- Component Declarations
------------------------------------------------------------------------
COMPONENT Synchronizer is
	PORT(
		i_domain_clock: IN STD_LOGIC;
		i_input: IN STD_LOGIC;
		o_output: OUT STD_LOGIC
	);
END COMPONENT;

COMPONENT I2CMaster is
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
END COMPONENT;

COMPONENT SPIMaster is
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
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- OV2640 I2C Address (Write: 0x60, Read: 0x61)
constant OV2640_I2C_ADDR: STD_LOGIC_VECTOR(5 downto 0) := "110000";

-- OV2640 I2C Register: RESET Address & Value (156)
constant REGISTER_RESET_ADDR: STD_LOGIC_VECTOR(7 downto 0) := X"12";
constant REGISTER_RESET_VALUE: STD_LOGIC_VECTOR(7 downto 0) := X"80";





-- OV7670 SCCB Register: HSTART Address & Value (156)
constant REGISTER_HSTART_ADDR: STD_LOGIC_VECTOR(7 downto 0) := X"17";
constant REGISTER_HSTART_VALUE: STD_LOGIC_VECTOR(7 downto 0) := X"13";

-- OV7670 SCCB Register: HSTOP Address & Value (14)
constant REGISTER_HSTOP_ADDR: STD_LOGIC_VECTOR(7 downto 0) := X"18";
constant REGISTER_HSTOP_VALUE: STD_LOGIC_VECTOR(7 downto 0) := X"01";

-- OV7670 SCCB Register: HREF Address & Value (HSTOP[2:0], HSTART[2:0])
constant REGISTER_HREF_ADDR: STD_LOGIC_VECTOR(7 downto 0) := X"32";
constant REGISTER_HREF_VALUE: STD_LOGIC_VECTOR(7 downto 0) := X"B6";

-- OV7670 SCCB Register: ...
constant REGISTER_xxx_ADDR: STD_LOGIC_VECTOR(7 downto 0) := X"32";
constant REGISTER_xxx_VALUE: STD_LOGIC_VECTOR(7 downto 0) := X"B6";






-- Handle OV7670 FIFO Read/Write Collision (Start Read after the 138 239th Clock Cycles)
constant OV7670_READ_WRITE_FIFO_SYNC: UNSIGNED(19 downto 0) := X"21BFF";

-- OV7670 End of Image Write (after the 614 082th Clock Cyles)
constant END_OF_IMAGE_WRITE: UNSIGNED(19 downto 0) := X"95EC2";

-- OV7670 Remaining Pixel Data of Image to Read:
-- Total Pixel Data: 614 400
-- Last Pixel Data: 614 399
-- Remaining Pixel Data to Read: 614 398
constant REMAINING_PIXELS_TO_READ: UNSIGNED(19 downto 0) := X"95FFE";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- OV7670 Controller Clock
signal clock_12M: STD_LOGIC := '0';
signal clock_12M_ready: STD_LOGIC := '0';

-- 0V7670 Synchronized Reset
signal synchronized_reset: STD_LOGIC := '0';

-- OV7670 Controller State
TYPE ov7670ControllerState is (	IDLE,
								OV7670_RESET_REG, CONFIG_RESET_REG, WAITING_RESET_CONFIG,
								OV7670_HSTART_REG, CONFIG_HSTART_REG, WAITING_HSTART_CONFIG,
								OV7670_HSTOP_REG, CONFIG_HSTOP_REG, WAITING_HSTOP_CONFIG,
								OV7670_HREF_REG, CONFIG_HREF_REG, WAITING_HREF_CONFIG,
								SYNC_OV7670, WAITING_IMAGE_START, READ_WRITE_FIFO_SYNC, GET_FIRST_IMAGE_DATA, GET_REMAINING_IMAGE_DATA, GET_LAST_IMAGE_DATA);
signal state: ov7670ControllerState := IDLE;
signal next_state: ov7670ControllerState;
signal pixel_counter: UNSIGNED(19 downto 0) := (others => '0');

-- OV7670 SCCB Register Address & Value
signal ov7670_sccb_reg_addr: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
signal ov7670_sccb_reg_value: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');

-- OV7670 SCCB Read Value
signal ov7670_sccb_read_value_ready: STD_LOGIC := '0';
signal ov7670_sccb_read_value: STD_LOGIC_VECTOR(7 downto 0) := (others => '0');

-- OV7670 SCCB Ready & Start Configuration
signal ov7670_sccb_ready: STD_LOGIC := '0';
signal ov7670_sccb_start: STD_LOGIC := '0';

-- OV7670 Image Output Enable
signal image_output_data_enable: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	---------------------------------
	-- OV7670 FIFO - Write Manager --
	---------------------------------
	-- OV7670 FIFO Write Reset ('0': Reset, '1': No Reset)
	o_ov7670_fifo_write_reset <= '0' when i_ov7670_vsync = '1' or state = GET_LAST_IMAGE_DATA else '1';

	------------------------------------
	-- OV7670 Master Clock (12 MHz) --
	------------------------------------
	inst_ov7670ControllerClock : ov7670_fifo_clock_gen port map (clk_out1 => clock_12M, locked => clock_12M_ready, clk_in1 => i_clock_100);

	-------------------------------
	-- OV7670 Reset Synchronizer --
	-------------------------------
	inst_ov7670SynchronizerReset : Synchronizer port map (
		i_domain_clock => clock_12M,
		i_input => i_reset,
		o_output => synchronized_reset);

	-----------------------------------
	-- OV7670 SCCB Configurer Inputs --
	-----------------------------------
	process(state)
	begin
		case state is
			when OV7670_RESET_REG | WAITING_RESET_CONFIG =>
										ov7670_sccb_reg_addr <= REGISTER_RESET_ADDR;
										ov7670_sccb_reg_value <= REGISTER_RESET_VALUE;
										ov7670_sccb_start <= '0';
			
			when CONFIG_RESET_REG  =>	ov7670_sccb_reg_addr <= REGISTER_RESET_ADDR;
										ov7670_sccb_reg_value <= REGISTER_RESET_VALUE;
										ov7670_sccb_start <= '1';

			when OV7670_HSTART_REG | WAITING_HSTART_CONFIG =>
										ov7670_sccb_reg_addr <= REGISTER_HSTART_ADDR;
										ov7670_sccb_reg_value <= REGISTER_HSTART_VALUE;
										ov7670_sccb_start <= '0';
			
			when CONFIG_HSTART_REG  =>	ov7670_sccb_reg_addr <= REGISTER_HSTART_ADDR;
										ov7670_sccb_reg_value <= REGISTER_HSTART_VALUE;
										ov7670_sccb_start <= '1';
														
			when OV7670_HSTOP_REG | WAITING_HSTOP_CONFIG =>	
										ov7670_sccb_reg_addr <= REGISTER_HSTOP_ADDR;
										ov7670_sccb_reg_value <= REGISTER_HSTOP_VALUE;
										ov7670_sccb_start <= '0';
			
			when CONFIG_HSTOP_REG  =>	ov7670_sccb_reg_addr <= REGISTER_HSTOP_ADDR;
										ov7670_sccb_reg_value <= REGISTER_HSTOP_VALUE;
										ov7670_sccb_start <= '1';

			when OV7670_HREF_REG | WAITING_HREF_CONFIG =>
										ov7670_sccb_reg_addr <= REGISTER_HREF_ADDR;
										ov7670_sccb_reg_value <= REGISTER_HREF_VALUE;
										ov7670_sccb_start <= '0';
			
			when CONFIG_HREF_REG  =>	ov7670_sccb_reg_addr <= REGISTER_HREF_ADDR;
										ov7670_sccb_reg_value <= REGISTER_HREF_VALUE;
										ov7670_sccb_start <= '1';

			when others => 	ov7670_sccb_reg_addr <= (others => '0');
							ov7670_sccb_reg_value <= (others => '0');
							ov7670_sccb_start <= '0';
		end case;
	end process;

	------------------------
	-- OV7670 SCCB Master --
	------------------------
	inst_sccbMaster : SCCBMaster port map (
		i_clock => clock_12M,
		i_mode => '0',
		i_salve_addr => OV7670_WRITE_ADDR,
		i_reg_addr => ov7670_sccb_reg_addr,
		i_reg_value => ov7670_sccb_reg_value,
		i_start => ov7670_sccb_start,
		o_ready => ov7670_sccb_ready,
		o_read_value_valid => ov7670_sccb_read_value_ready,
		o_read_value => ov7670_sccb_read_value,
		o_scl => o_ov7670_scl,
		io_sda => io_ov7670_sda);

	-------------------------------------
	-- OV7670 Controller State Machine --
	-------------------------------------
	-- OV7670 Controller State
	process(clock_12M)
	begin
		if rising_edge(clock_12M) then

			-- Reset State
			if (synchronized_reset = '1') then
				state <= IDLE;

			-- Next State
			elsif (clock_12M_ready = '1') then
				state <= next_state;
			end if;
		end if;
	end process;

	-- OV7670 Controller Next State
	process(state, ov7670_sccb_ready, i_ov7670_vsync, i_ov7670_href, pixel_counter)
	begin
		case state is
			when IDLE => 	if (i_ov7670_vsync = '1') then
								next_state <= OV7670_RESET_REG;
							else
								next_state <= IDLE;
							end if;

			when OV7670_RESET_REG => next_state <= CONFIG_RESET_REG;
			when CONFIG_RESET_REG =>
									if (ov7670_sccb_ready = '0') then
										next_state <= WAITING_RESET_CONFIG;
									else
										next_state <= CONFIG_RESET_REG;
									end if;
			when WAITING_RESET_CONFIG =>
									if (ov7670_sccb_ready = '1') then
										next_state <= OV7670_HSTART_REG;
									else
										next_state <= WAITING_RESET_CONFIG;
									end if;

			when OV7670_HSTART_REG => next_state <= CONFIG_HSTART_REG;
			when CONFIG_HSTART_REG =>
									if (ov7670_sccb_ready = '0') then
										next_state <= WAITING_HSTART_CONFIG;
									else
										next_state <= CONFIG_HSTART_REG;
									end if;
			when WAITING_HSTART_CONFIG =>
									if (ov7670_sccb_ready = '1') then
										next_state <= OV7670_HSTOP_REG;
									else
										next_state <= WAITING_HSTART_CONFIG;
									end if;

			when OV7670_HSTOP_REG => next_state <= CONFIG_HSTOP_REG;
			when CONFIG_HSTOP_REG =>
									if (ov7670_sccb_ready = '0') then
										next_state <= WAITING_HSTOP_CONFIG;
									else
										next_state <= CONFIG_HSTOP_REG;
									end if;
			when WAITING_HSTOP_CONFIG =>
									if (ov7670_sccb_ready = '1') then
										next_state <= OV7670_HREF_REG;
									else
										next_state <= WAITING_HSTOP_CONFIG;
									end if;

			when OV7670_HREF_REG => next_state <= CONFIG_HREF_REG;
			when CONFIG_HREF_REG =>
									if (ov7670_sccb_ready = '0') then
										next_state <= WAITING_HREF_CONFIG;
									else
										next_state <= CONFIG_HREF_REG;
									end if;
			when WAITING_HREF_CONFIG =>
									if (ov7670_sccb_ready = '1') then
										next_state <= SYNC_OV7670;
									else
										next_state <= WAITING_HREF_CONFIG;
									end if;

			when SYNC_OV7670 =>	
									if (i_ov7670_vsync = '1') then
										next_state <= WAITING_IMAGE_START;
									else
										next_state <= SYNC_OV7670;
									end if;

			when WAITING_IMAGE_START =>	
									if (i_ov7670_href = '1') then
										next_state <= READ_WRITE_FIFO_SYNC;
									else
										next_state <= WAITING_IMAGE_START;
									end if;

			when READ_WRITE_FIFO_SYNC =>
									if (pixel_counter = OV7670_READ_WRITE_FIFO_SYNC) then
										next_state <= GET_FIRST_IMAGE_DATA;
									else
										next_state <= READ_WRITE_FIFO_SYNC;
									end if;

			when GET_FIRST_IMAGE_DATA => next_state <= GET_REMAINING_IMAGE_DATA;

			when GET_REMAINING_IMAGE_DATA =>
									if (pixel_counter = END_OF_IMAGE_WRITE) then
										next_state <= GET_LAST_IMAGE_DATA;
									else
										next_state <= GET_REMAINING_IMAGE_DATA;
									end if;

			when GET_LAST_IMAGE_DATA =>
									if (pixel_counter = REMAINING_PIXELS_TO_READ) then
										next_state <= WAITING_IMAGE_START;
									else
										next_state <= GET_LAST_IMAGE_DATA;
									end if;

			when others => next_state <= IDLE;
		end case;
	end process;

	-------------------------------------
	-- OV7670 Controller State Counter --
	-------------------------------------
	process(clock_12M)
	begin
		if rising_edge(clock_12M) then

			-- Increment Pixel Counter
			if (state = READ_WRITE_FIFO_SYNC) or (state = GET_REMAINING_IMAGE_DATA) or (state = GET_LAST_IMAGE_DATA) then
				pixel_counter <= pixel_counter +1;
			
			else
				-- Reset Pixel Counter
				pixel_counter <= (others => '0');
			end if;

		end if;
	end process;

	--------------------------------------
	-- OV7670 FIFO - Read Image Manager --
	--------------------------------------
	-- OV7670 FIFO Read Clock
	inst_ov7670FifoReadClockODDR: ODDR
	generic map(DDR_CLK_EDGE => "OPPOSITE_EDGE", INIT => '0', SRTYPE => "SYNC")
	port map (
		Q => o_ov7670_fifo_read_clock,
		C => clock_12M,
		CE => '1',
		D1 => '1',  -- data on positive edge
		D2 => '0',  -- data on negative edge
		R => '0',
		S => '0');

	-- OV7670 FIFO Read Reset ('0': Reset, '1': No Reset)
	o_ov7670_fifo_read_reset <= '1' when state = GET_FIRST_IMAGE_DATA or state = GET_REMAINING_IMAGE_DATA or state = GET_LAST_IMAGE_DATA else '0';

	--------------------------
	-- Image Output Manager --
	--------------------------
	-- Image Output Clock
	o_image_output_clock <= clock_12M;

	-- Image Output Data Enable
	process(clock_12M)
	begin
		if rising_edge(clock_12M) then

			-- Image Output Data Enable ('0': Disable, '1': Enable)
			if (state = GET_FIRST_IMAGE_DATA) or (state = GET_REMAINING_IMAGE_DATA) or (state = GET_LAST_IMAGE_DATA) then
				image_output_data_enable <= '1';
			else
				image_output_data_enable <= '0';
			end if;
		end if;
	end process;
	o_image_output_data_enable <= image_output_data_enable;

	-- Image Output Data
	process(clock_12M)
	begin
		if rising_edge(clock_12M) then
			o_image_output_data <= i_ov7670_fifo_read_data;
		end if;
	end process;

end Behavioral;