clear;

% Image Definition
image_width = 640;
image_height = 480;
image_href_sync = 144; % @ 6MHz

% Write @ 12 MHz
write_line_data = image_width *2;
write_line = (image_width + image_href_sync) *2;
% No HREF Sync for the last Line (@12MHz)
write_image = (write_line * image_height) - (2*image_href_sync);

% Read @ 12 MHz
read_line = image_width *2;
read_image = read_line * image_height;
read_wait = write_image + (2*image_href_sync) - read_image;

% FIFO
fifo_max_addr = 393216;

cycle = 1;
write_sync = 0;
writeAddrRaw=1;readAddrRaw=1;
writeAddr=1;readAddr=1;
for i=1:write_image-1

    disp("==========");
    fprintf('i:%d\n', i);
    fprintf('W:%d\t\traw:%d\n', writeAddr, writeAddrRaw);
    fprintf('R:%d\t\traw:%d\n', readAddr, readAddrRaw);

    % Verify Write & Read Collision
    if (i ~= 1) && (readAddr == writeAddr)
        disp("!!!!!!!!");
        pause();
    end;

    % Increment Read Addr
    if (i > read_wait)

        % Reset FIFO Read Addr
        if (readAddr == fifo_max_addr)
            readAddr = 1;
        else
            readAddr = readAddr+1;
        end;

        % Increment Raw Read Addr
        readAddrRaw = readAddrRaw +1;
    end;

    % Increment Write Addr
    if ((mod(i, write_line) > 0) && (mod(i, write_line) <= write_line_data))
        
        % Reset FIFO Write Addr
        if (writeAddr == fifo_max_addr)
            writeAddr = 1;
        else
            writeAddr = writeAddr+1;
        end;

         % Increment Raw Write Addr
        writeAddrRaw = writeAddrRaw +1;

        % Reset Write Image Sync
        write_sync = 0;

    else
        % Write Image Sync
        write_sync = write_sync +1;
        fprintf('Waiting ............... (%d)\n', write_sync);
    end;

    cycle = i;
end;

disp("=======================");
disp("===== End of Read =====")
disp("=======================");

% End of Read
for i=readAddrRaw:read_image
    disp("==========");
    fprintf('i:%d\n', cycle);
    fprintf('W:%d\t\traw:%d\n', writeAddr, writeAddrRaw);
    fprintf('R:%d\t\traw:%d\n', readAddr, readAddrRaw);

    % Reset FIFO Read Addr
    if (readAddr == fifo_max_addr)
        readAddr = 1;
    else
        readAddr = readAddr+1;
    end;

    % Increment Raw Read Addr
    readAddrRaw = readAddrRaw +1;

    cycle = cycle +1;
end;