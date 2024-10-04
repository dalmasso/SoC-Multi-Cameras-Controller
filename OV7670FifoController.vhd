------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 15/07/2024
-- Module Name: OV7670FifoController (with embedded FIFO)
-- Description:
--      OV7670 640x480 CMOS Camera Module with embedded FIFO controller
--		Input 	-	i_clock_100: Clock (100MHz)
--		Input 	-	i_reset: Reset ('0': NO Reset, '1': Reset)
--		Input 	-	i_ov7670_vsync: OV7670 Vertical Synchronization ('0': Current Image, '1': New Image)
--		Input 	-	i_ov7670_href: OV7670 Horizontal Synchronization ('0': No Image Data, '1': Image Data available)
--		Output 	-	o_ov7670_scl: OV7670 Configuration - Serial Interface Clock
--		Output 	-	o_ov7670_sda: OV7670 Configuration - Serial Interface Data
--		Output 	-	o_ov7670_reset: OV7670 Configuration - Reset All Registers ('0': Reset, '1': No Reset)
--		Output 	-	o_ov7670_fifo_write_reset: OV7670 FIFO Write Reset ('0': Reset, '1': No Reset)
--		Output 	-	o_ov7670_fifo_read_clock: OV7670 FIFO Read Clock (12 MHz)
--		Output 	-	o_ov7670_fifo_read_reset: OV7670 FIFO Read Reset ('0': Reset, '1': No Reset)
--		Input 	-	i_ov7670_fifo_read_data: OV7670 FIFO Read Data (Format: YUV 4:2:2, Sequence: U0 Y0 V0 Y1)
--		Output 	-	o_image_output_clock: OV7670 Image Output Clock
--		Output 	-	o_image_output_data_enable: OV7670 Image Output Data Enable ('0': Disable, '1': Enable)
--		Output 	-	o_image_output_data: OV7670 Image Output Data (8-bit, Format: YUV 4:2:2, Sequence: U0 Y0 V0 Y1)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

Library UNISIM;
use UNISIM.vcomponents.all;

ENTITY OV7670FifoController is
PORT(
	i_clock_100: IN STD_LOGIC;
	i_reset: IN STD_LOGIC;
	-- OV7670 Synchronization & Control
	i_ov7670_vsync: IN STD_LOGIC;
	i_ov7670_href: IN STD_LOGIC;
	o_ov7670_scl: OUT STD_LOGIC;
	o_ov7670_sda: OUT STD_LOGIC;
	o_ov7670_reset: OUT STD_LOGIC;
	-- OV7670 Embedded FIFO Write Controller
	o_ov7670_fifo_write_reset: OUT STD_LOGIC;
	-- OV7670 Embedded FIFO Read Controller
	o_ov7670_fifo_read_clock: OUT STD_LOGIC;
	o_ov7670_fifo_read_reset: OUT STD_LOGIC;
	i_ov7670_fifo_read_data: IN STD_LOGIC_VECTOR(7 downto 0);
	-- Image Data Output
	o_image_output_clock: OUT STD_LOGIC;
	o_image_output_data_enable: OUT STD_LOGIC;
	o_image_output_data: OUT STD_LOGIC_VECTOR(7 downto 0)
);
END OV7670FifoController;

ARCHITECTURE Behavioral of OV7670FifoController is

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

COMPONENT ov7670_fifo_clock_gen is
	PORT(
		clk_out1: OUT STD_LOGIC;
		clk_in1: IN STD_LOGIC
	);
END COMPONENT;

COMPONENT OV7670Configurer is
	PORT(
		i_clock_12M: IN STD_LOGIC;
		i_reset: IN STD_LOGIC;
		o_end_of_config: OUT STD_LOGIC;
		o_ov7670_scl: OUT STD_LOGIC;
		o_ov7670_sda: OUT STD_LOGIC;
		o_ov7670_reset: OUT STD_LOGIC
	);
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
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

-- 0V7670 Synchronized Reset
signal synchronized_reset: STD_LOGIC := '0';

-- OV7670 Controller State Machine
TYPE ov7670ControllerState is (OV7670_CONFIG, SYNC_OV7670, WAITING_IMAGE_START, READ_WRITE_FIFO_SYNC, GET_FIRST_IMAGE_DATA, GET_REMAINING_IMAGE_DATA);
signal state: ov7670ControllerState := OV7670_CONFIG;
signal next_state: ov7670ControllerState;
signal pixel_counter: UNSIGNED(19 downto 0) := (others => '0');

-- OV7670 Image Output Enable
signal image_output_data_enable: STD_LOGIC := '0';

-- OV7670 End of Configuration
signal ov7670_end_config: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	---------------------------------
	-- OV7670 FIFO - Write Manager --
	---------------------------------
	-- OV7670 FIFO Write Reset ('0': Reset, '1': No Reset)
	o_ov7670_fifo_write_reset <= '0' when i_ov7670_vsync = '1' or pixel_counter >= END_OF_IMAGE_WRITE else '1';

	------------------------------------
	-- OV7670 Master Clock (12 MHz) --
	------------------------------------
	inst_ov7670ControllerClock : ov7670_fifo_clock_gen port map (clk_out1 => clock_12M, clk_in1 => i_clock_100);

	-------------------------------
	-- OV7670 Reset Synchronizer --
	-------------------------------
	inst_ov7670SynchronizerReset : Synchronizer port map (
		i_domain_clock => clock_12M,
		i_input => i_reset,
		o_output => synchronized_reset);

	-----------------------
	-- OV7670 Configurer --
	-----------------------
	inst_ov7670Configurer : OV7670Configurer port map (i_clock_12M => clock_12M, i_reset => synchronized_reset, o_end_of_config => ov7670_end_config, o_ov7670_scl => o_ov7670_scl, o_ov7670_sda => o_ov7670_sda, o_ov7670_reset => o_ov7670_reset);

	-------------------------------------
	-- OV7670 Controller State Machine --
	-------------------------------------
	-- OV7670 Controller State
	process(clock_12M)
	begin
		if rising_edge(clock_12M) then
			if (synchronized_reset = '1') then
				state <= OV7670_CONFIG;
			else
				state <= next_state;
			end if;
		end if;
	end process;

	-- OV7670 Controller Next State
	process(ov7670_end_config, i_ov7670_vsync, i_ov7670_href, pixel_counter, state)
	begin
		case state is

			when OV7670_CONFIG => 	if (ov7670_end_config = '1') then
										next_state <= SYNC_OV7670;
									else
										next_state <= OV7670_CONFIG;
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
									if (pixel_counter = REMAINING_PIXELS_TO_READ) then
										next_state <= WAITING_IMAGE_START;
									else
										next_state <= GET_REMAINING_IMAGE_DATA;
									end if;

			when others => next_state <= OV7670_CONFIG;
		end case;
	end process;

	-------------------------------------
	-- OV7670 Controller State Counter --
	-------------------------------------
	process(clock_12M)
	begin
		if rising_edge(clock_12M) then

			-- Increment Pixel Counter
			if (state = READ_WRITE_FIFO_SYNC) or (state = GET_REMAINING_IMAGE_DATA) then
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
	o_ov7670_fifo_read_reset <= '1' when state = GET_FIRST_IMAGE_DATA or state = GET_REMAINING_IMAGE_DATA else '0';

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
			if (state = GET_FIRST_IMAGE_DATA) or (state = GET_REMAINING_IMAGE_DATA) then
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