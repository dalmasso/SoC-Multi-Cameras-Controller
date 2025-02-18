#!/bin/sh

# Note: Vivado on ARM Mac --> https://github.com/ichi4096/vivado-on-silicon-mac
# modify the Dockerfile to use "ubuntu:focal"
# Not use of Rosetta Emulation in Docker Desktop
# Error with XQuartz: https://gist.github.com/cschiewek/246a244ba23da8b9f0e7b11a68bf3285
# Error with /tmp/.X11-unix, use command: mkfifo /tmp/.X11-unix

echo "Starting FPGA Programmer..."

# Get Bit File to program
echo "Paste the bit file to program"
read bitpath
cp $bitpath ./myBinary.bit

# Create OPENOCD Uploader
echo "# File: 7series.txt
interface ftdi
ftdi_device_desc "Digilent USB Device"
ftdi_vid_pid 0x0403 0x6010
# channel 1 does not have any functionality
ftdi_channel 0
# just TCK TDI TDO TMS, no reset
ftdi_layout_init 0x0088 0x008b
reset_config none
adapter_khz 10000

source [find cpld/xilinx-xc7.cfg]
source [find cpld/jtagspi.cfg]
init

puts [irscan xc7.tap 0x09]
puts [drscan xc7.tap 32 0]  

puts \"Programming FPGA...\"
pld load 0 myBinary.bit
exit
" >> 7series.txt

# Program FPGA
sudo openocd -f 7series.txt

# Cleanup
rm 7series.txt
rm myBinary.bit
