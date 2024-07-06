function filteredImage = laplaceFilter(imageToFilter)

%% Filter Coefficients
h00 = 0;
h01 = 1;
h02 = 0;
h10 = 1;
h11 = -4;
h12 = 1;
h20 = 0;
h21 = 1;
h22 = 0;

img = imread(imageToFilter);
height = size(img, 1);
width = size(img, 2);

%% Filter on Red value %%
filteredImage = zeros(height,width,3);
for p=1:3
    myImg = double(img(:,:,p));
    for r=1:height
        for c=1:width

            prevLine = r-1;
            nextLine = r+1;
            prevCol = c-1;
            nextCol = c+1;

            %% Previous Line
            prevLine = r-1;
            if (r == 1)
                prevLine = height;
            end

            %% Previous Line & Previous Column
            if (r == 1) || (c == 1)
                d0 = 0;
            else
                d0 = double(myImg(prevLine, prevCol));
            end
            
            %% Previous Line & Current Column
            if (r == 1)
                d1 = 0;
            else
                d1 = double(myImg(prevLine, c));
            end

            %% Previous Line & Next Column
            if (r == 1) || (c == width)
                d2 = 0;
            else
                d2 = double(myImg(prevLine, nextCol));
            end

            %% Current Line & Previous Column
            if (c == 1)
                d3 = 0;
            else
                d3 = double(myImg(r, prevCol));
            end

            %% Current Line & Current Column
            d4 = double(myImg(r, c));

            %% Current Line & Next Column
            if (c == width)
                d5 = 0;
            else
                d5 = double(myImg(r, nextCol));
            end

            %% Next Line & Previous Column
            if (r == height) || (c == 1)
                d6 = 0;
            else
                d6 = double(myImg(nextLine, prevCol));
            end
       
            %% Next Line & Current Column
            if (r == height)
                d7 = 0;
            else
                d7 = double(myImg(nextLine, c));
            end

            %% Next Line & Next Column
            if (r == height) || (c == width)
                d8 = 0;
            else
                d8 = double(myImg(nextLine, nextCol));
            end

            %% Compute Filter
            filteredImage(r, c, p) = (h00*d0 + h01*d1 + h02*d2 + h10*d3 + h11*d4 + h12*d5 + h20*d6 + h21*d7 + h22*d8);
        end
    end
end

% Save Image
imwrite(filteredImage,'filteredImage.jpeg','JPEG');
imwrite(uint8(filteredImage),'filteredImage_uint8.jpeg','JPEG');