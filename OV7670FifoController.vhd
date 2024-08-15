------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 15/07/2024
-- Module Name: OV7670FifoController (with embedded FIFO)
-- Description:
--      OV7670/OV7171 640x480 CMOS Camera Module with embedded FIFO
--		Input 	-	i_pixel_clock: Pixel Clock (148.5 MHz)
--		Input 	-	i_ov7670_vsync: OV7670 Vertical Synchronization ('0': Current Image, '1': New Image)
--		Input 	-	i_ov7670_href: OV7670 Horizontal Synchronization ('0': No Image Data, '1': Image Data available)
--		Output 	-	o_ov7670_fifo_write_reset: OV7670 FIFO Write Reset ('0': Reset, '1': No Reset)
--		Output 	-	o_ov7670_fifo_write_enable: OV7670 FIFO Write Enable ('0': No Write, '1': Write Enable)
--		Output 	-	o_ov7670_fifo_read_clock: OV7670 FIFO Read Clock (25.6 MHz)
--		Output 	-	o_ov7670_fifo_read_reset: OV7670 FIFO Read Reset ('0': Reset, '1': No Reset)
--		Output 	-	o_ov7670_fifo_read_enable: OV7670 FIFO Read Enable ('0': Enable, '1': Disable)
--		Input 	-	i_ov7670_fifo_read_data: OV7670 FIFO Read Data (Format: YUV 4:2:2, Sequence: U0 Y0 V0 Y1)
--		Output 	-	o_image_output_clock: OV7670 Image Output Clock
--		Output 	-	o_image_output_data_enable: OV7670 Image Output Data Enable ('0': Disable, '1': Enable)
--		Output 	-	o_image_output_data: OV7670 Image Output Data (8-bit, Format: YUV 4:2:2, Sequence: U0 Y0 V0 Y1)
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY OV7670FifoController is
PORT(
	i_pixel_clock: IN STD_LOGIC;
	-- OV7670 Synchronizations
	i_ov7670_vsync: IN STD_LOGIC;
	i_ov7670_href: IN STD_LOGIC;
	-- OV7670 Embedded FIFO Write Controller
	o_ov7670_fifo_write_reset: OUT STD_LOGIC;
	o_ov7670_fifo_write_enable: OUT STD_LOGIC;
	-- OV7670 Embedded FIFO Read Controller
	o_ov7670_fifo_read_clock: OUT STD_LOGIC;
	o_ov7670_fifo_read_reset: OUT STD_LOGIC;
	o_ov7670_fifo_read_enable: OUT STD_LOGIC;
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
COMPONENT ov7670_fifo_clock_gen is
	PORT(
		clk_out1: OUT STD_LOGIC;
		clk_in1: IN STD_LOGIC
	);
END COMPONENT;

------------------------------------------------------------------------
-- Constant Declarations
------------------------------------------------------------------------
-- Waiting OV7670 FIFO Almost Full: 393 216 8-bit Pixel Data, Last FIFO Data Index: 393 215)
-- Trigger at: 393 215 @12MHz =x2.133..=> 838 824 -100 = 838 724 @25.6MHz
constant OV7670_FIFO_ALMOST_FULL: UNSIGNED(19 downto 0) := X"CCC44";

-- OV7670 Total Pixel Data of Image (614 400 Pixel Data => Last Pixel: 614 399)
constant TOTAL_PIXELS: UNSIGNED(19 downto 0) := X"95FFF";

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- OV7670 Controller Clock
signal clock_25M: STD_LOGIC := '0';

-- OV7670 Controller State Machine
TYPE ov7670ControllerState is (SYNC_OV7670, WAITING_IMAGE_START, WAITING_FIFO_ALMOST_FULL, RESET_PIXEL_COUNTER, GET_IMAGE);
signal state: ov7670ControllerState := SYNC_OV7670;
signal next_state: ov7670ControllerState;
signal pixel_counter: UNSIGNED(19 downto 0) := (others => '0');

-- OV7670 FIFO Read Controller
signal ov7670_fifo_read_enable: STD_LOGIC := '0';

-- Image Output Controller
signal image_output_data_enable: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	---------------------------------
	-- OV7670 FIFO - Write Manager --
	---------------------------------
	-- OV7670 FIFO Write Reset ('0': Reset, '1': No Reset)
	o_ov7670_fifo_write_reset <= not(i_ov7670_vsync);

	-- OV7670 FIFO Write Enable ('0': No Write, '1': Write Enable)
	o_ov7670_fifo_write_enable <= '1';

	------------------------------------
	-- OV7670 Master Clock (25.6 MHz) --
	------------------------------------
	inst_ov7670ControllerClock : ov7670_fifo_clock_gen port map (clk_out1 => clock_25M, clk_in1 => i_pixel_clock);

	-------------------------------------
	-- OV7670 Controller State Machine --
	-------------------------------------
	-- OV7670 Controller State
	process(clock_25M)
	begin
		if rising_edge(clock_25M) then
			state <= next_state;
		end if;
	end process;

	-- OV7670 Controller Next State
	process(i_ov7670_vsync, i_ov7670_href, pixel_counter, state)
	begin
		case state is
			when SYNC_OV7670 =>
									if (i_ov7670_vsync = '1') then
										next_state <= WAITING_IMAGE_START;
									else
										next_state <= SYNC_OV7670;
									end if;

			when WAITING_IMAGE_START =>
									if (i_ov7670_href = '1') then
										next_state <= WAITING_FIFO_ALMOST_FULL;
									else
										next_state <= WAITING_IMAGE_START;
									end if;

			when WAITING_FIFO_ALMOST_FULL =>
									if (pixel_counter = OV7670_FIFO_ALMOST_FULL) then
										next_state <= RESET_PIXEL_COUNTER;
									else
										next_state <= WAITING_FIFO_ALMOST_FULL;
									end if;

			when RESET_PIXEL_COUNTER => next_state <= GET_IMAGE;

			when GET_IMAGE =>
									if (pixel_counter < TOTAL_PIXELS) then
										next_state <= GET_IMAGE;
									else
										next_state <= WAITING_IMAGE_START;
									end if;

			when others => next_state <= SYNC_OV7670;
		end case;
	end process;

	-------------------------------------
	-- OV7670 Controller State Counter --
	-------------------------------------
	process(i_ov7670_vsync, clock_25M)
	begin
		if rising_edge(clock_25M) then

			-- Reset Pixel Counter
			if (state = WAITING_IMAGE_START) or (state = RESET_PIXEL_COUNTER)then
				pixel_counter <= (others => '0');

			-- Increment Pixel Counter
			elsif ((state = WAITING_FIFO_ALMOST_FULL) and (i_ov7670_href = '1')) or (state = GET_IMAGE) then
				pixel_counter <= pixel_counter +1;
			end if;

		end if;
	end process;

	--------------------------------------
	-- OV7670 FIFO - Read Image Manager --
	--------------------------------------
	-- OV7670 FIFO Read Clock
	o_ov7670_fifo_read_clock <= clock_25M;

	-- OV7670 FIFO Read Enable
	process(clock_25M)
	begin
		if rising_edge(clock_25M) then
			if (state = GET_IMAGE) then
				ov7670_fifo_read_enable <= '1';
			else
				ov7670_fifo_read_enable <= '0';
			end if;
		end if;
	end process;

	-- OV7670 FIFO Read Enable ('0': Enable, '1': Disable)
	o_ov7670_fifo_read_enable <= '0';

	-- OV7670 FIFO Read Reset ('0': Reset, '1': No Reset)
	o_ov7670_fifo_read_reset <= ov7670_fifo_read_enable;

	--------------------------
	-- Image Output Manager --
	--------------------------
	-- Image Output Clock
	o_image_output_clock <= clock_25M;

	-- Image Output Data Enable (Handle 1 Cycle Latency)
	process(clock_25M)
	begin
		if rising_edge(clock_25M) then
			o_image_output_data_enable <= ov7670_fifo_read_enable;
		end if;
	end process;

	-- Image Output Data
	process(clock_25M)
	begin
		if rising_edge(clock_25M) then
			o_image_output_data <= i_ov7670_fifo_read_data;
		end if;
	end process;

end Behavioral;