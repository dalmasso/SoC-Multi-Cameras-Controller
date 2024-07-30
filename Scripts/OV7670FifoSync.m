% W Freq: 12 MHz
% Before W: 3+17 Lines * 784 pixels = 15 680 cycles
% W: 480 Lines * 784 pixels = 376 320 cycles
% After W: 10 Lines * 784 pixels = 7 840 cycles
% Total W: 399 840 cycles

% R Freq: 24 MHz -> ((392000 * 2) - 307200) /2 (--> R = R+2)
% R Freq: 43.20 MHz -> ((392000 * 4) - 307200) /4 (--> R = R+4)
% R Freq: 148.5 MHz -> ((392000 * 13) - 307200) /13 (--> R = R+13)

clear;
close();
before_write = 15680;
image_pixel = 376320;
after_write = 7840;
total_pixel = before_write + image_pixel + after_write;

read_image_pixel = 307200;
read_factor = 4;
read_start_trig = (((total_pixel * read_factor) - read_image_pixel)/ read_factor);

writeAddr=1;readAddr=1;
for i=1:(total_pixel)

    disp("====");
    disp(i);
    disp(writeAddr);
    disp(readAddr);

    if (i > before_write) && (i < total_pixel-after_write)
        writeAddr = writeAddr+1;
    end;

    if (i > read_start_trig)
        readAddr = readAddr+read_factor;
    end;

    if (writeAddr ~= 1) && ((readAddr >= writeAddr) || (readAddr > read_image_pixel))
        disp("!!!!!!!!");
        pause();
    end;
end;