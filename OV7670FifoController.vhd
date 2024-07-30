------------------------------------------------------------------------
-- Engineer:    Dalmasso Loic
-- Create Date: 15/07/2024
-- Module Name: OV7670FifoController (with embedded FIFO)
-- Description:
--      OV7670/OV7171 640x480 CMOS Camera Module with embedded FIFO
--		Input 	-	i_fifo_read_clock: OV7670 FIFO Read Clock (43.2 MHz)
--		Input 	-	i_ov7670_vsync: OV7670 Vertical Synchronization ('0': Current Image, '1': New Image)
--		Output 	-	o_ov7670_fifo_write_reset: OV7670 FIFO Write Reset ('0': Reset, '1': No Reset)
--		Output 	-	o_ov7670_fifo_write_enable: OV7670 FIFO Write Enable ('0': No Write, '1': Write Enable)
--		Output 	-	o_ov7670_fifo_read_clock: OV7670 FIFO Read Clock
--		Input 	-	i_ov7670_fifo_read_restart_trigger: OV7670 FIFO Read Restart Trigger ('0': No Restart, '1': Restart)
--		Output 	-	o_ov7670_fifo_read_reset: OV7670 FIFO Read Reset ('0': Reset, '1': No Reset)
--		Output 	-	o_ov7670_fifo_read_enable: OV7670 FIFO Read Enable ('0': Enable, '1': Disable)
--		Input 	-	i_ov7670_fifo_read_data: OV7670 FIFO Read Data (Default Format: YUV 4:2:2, Sequence: U0 Y0 V0 Y1)
--		Output 	-	o_new_image_trigger: New OV7670 Image Available Trigger ('0': Current Image, '1': New Image)
--		Output 	-	o_new_image_data_enable: New OV7670 Image Write Enable ('0': Disable, '1': Enable)
--		Output 	-	o_new_image_data: New OV7670 Image Data
--		Output 	-	o_ov7670_scl: OV7670 Configuration - Serial Interface Clock
--		In/Out 	-	io_ov7670_sda: OV7670 Configuration - Serial Interface Data
------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY OV7670FifoController is
PORT(
	i_fifo_read_clock: IN STD_LOGIC;
	-- OV7670 Synchronizations
	i_ov7670_vsync: IN STD_LOGIC;
	-- OV7670 Embedded FIFO Write Controller
	o_ov7670_fifo_write_reset: OUT STD_LOGIC;
	o_ov7670_fifo_write_enable: OUT STD_LOGIC;
	-- OV7670 Embedded FIFO Read Controller
	o_ov7670_fifo_read_clock: OUT STD_LOGIC;
	i_ov7670_fifo_read_restart_trigger: IN STD_LOGIC;
	o_ov7670_fifo_read_reset: OUT STD_LOGIC;
	o_ov7670_fifo_read_enable: OUT STD_LOGIC;
	i_ov7670_fifo_read_data: IN STD_LOGIC_VECTOR(7 downto 0);
	-- Image Data Ouput
	o_new_image_trigger: OUT STD_LOGIC;
	o_new_image_data_enable: OUT STD_LOGIC;
	o_new_image_data: OUT STD_LOGIC_VECTOR(7 downto 0)
    -- OV7670 Configuration (TODO)
	--o_ov7670_scl: OUT STD_LOGIC;
    --io_ov7670_sda: INOUT STD_LOGIC
);
END OV7670FifoController;

ARCHITECTURE Behavioral of OV7670FifoController is

------------------------------------------------------------------------
-- Signal Declarations
------------------------------------------------------------------------
-- OV7670 FIFO Read Control
signal read_fifo_enable: STD_LOGIC := '0';

-- New Image Data Enable
signal new_image_data_enable: STD_LOGIC := '0';

------------------------------------------------------------------------
-- Module Implementation
------------------------------------------------------------------------
begin

	----------------------------------
	-- OV7670 FIFO - Write Manager --
	----------------------------------
	-- OV7670 FIFO Write Reset ('0': Reset, '1': No Reset)
	o_ov7670_fifo_write_reset <= not(i_ov7670_vsync);

	-- OV7670 FIFO Write Enable ('0': No Write, '1': Write Enable)
	o_ov7670_fifo_write_enable <= '1';

	--------------------------------
	-- OV7670 FIFO - Read Manager --
	--------------------------------
	-- OV7670 FIFO Read Clock
	o_ov7670_fifo_read_clock <= i_fifo_read_clock;

	-- OV7670 FIFO Read Reset ('0': Reset, '1': No Reset)
	o_ov7670_fifo_read_reset <= not(i_ov7670_fifo_read_restart_trigger);

	-- OV7670 FIFO Read Enable
	process(i_fifo_read_clock)
	begin
		if rising_edge(i_fifo_read_clock) then

			-- Disable FIFO Read Enable
			if (i_ov7670_fifo_read_restart_trigger = '1') then
				read_fifo_enable <= '0';
			else
				-- Start FIFO Read
				read_fifo_enable <= '1';
			end if;
		end if;
	end process;

	-- OV7670 FIFO Read Enable ('0': Enable, '1': Disable)
	o_ov7670_fifo_read_enable <= not(read_fifo_enable);

	---------------------------
	-- OV7670 - Image Output --
	---------------------------
	-- New OV7670 Image Trigger ('0': Current Image, '1': New Image)
	o_new_image_trigger <= read_fifo_enable and not(new_image_data_enable);

	-- New OV7670 Image Write Enable & Data 
	process(i_fifo_read_clock)
	begin
		if rising_edge(i_fifo_read_clock) then

			-- New OV7670 Image Write Enable ('0': Disable, '1': Enable)
			new_image_data_enable <= read_fifo_enable;

			-- New OV7670 Image Data
			if (read_fifo_enable = '1') then
				-- FIFO Read Data Format: YUV 4:2:2 (Sequence: U0 Y0 V0 Y1)
				o_new_image_data <= i_ov7670_fifo_read_data;
			else
				o_new_image_data <= (others => '0');
			end if;
		end if;
	end process;

	-- New OV7670 Image Write Enable ('0': Disable, '1': Enable)
	o_new_image_data_enable <= new_image_data_enable;

end Behavioral;