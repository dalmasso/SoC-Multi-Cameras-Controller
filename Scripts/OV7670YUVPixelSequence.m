% YUV 4:2:2
% Output Sequence: U Y V Y
% Pixel Sequence: U0 Y0 V0 Y1 | U2 Y2 V2 Y3 | ...
% Pixel 0: Y0 U0 V0
% Pixel 1: Y1 U0 V0
% Pixel 2: Y2 U2 V2
% Pixel 3: Y3 U2 V2

clear;
close();
total_Y_pixel = 640 * 480;
total_U_pixel = (640 * 480) /2;
total_V_pixel = (640 * 480) /2;
total_pixel = total_Y_pixel + total_U_pixel + total_V_pixel;
memory_size = 393216;

% Pixels & Memory Addr Sequence
sequence = strings([2,total_pixel]);

seq_index = 0;
uv_index = 0;
y_index = 0;
for i=1:total_pixel

    % U0
    if (mod(i,4) == 1)
        sequence(1, i) = "U" + uv_index;
    end;

    % Y0
    if (mod(i,4) == 2)
        sequence(1, i) = "Y" + y_index;
        y_index = y_index +1;
    end;    

    % V0
    if (mod(i,4) == 3)
        sequence(1, i) = "V" + uv_index;
        uv_index = uv_index +2;
    end;    

    % Y1
    if (mod(i,4) == 0)
        sequence(1, i) = "Y" + y_index;
        y_index = y_index +1;
    end;

    % Memory Addr
    if (i > memory_size)
        sequence(2, i) = i-memory_size-1;
    else
        sequence(2, i) = i-1;
    end;

end;