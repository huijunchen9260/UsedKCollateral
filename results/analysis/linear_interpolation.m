function [index, weight] = linear_interpolation(grid, num, point)

    % linear interpolation
    % (input):
	    % grid: grid to lin. interpolate
	    % num: number of grid pts
	    % point: point that is not on the grid
    % (output):
	    % index: index for point
	    % weight: weight to calculate point

    if point <= grid(1)  % if x' < grid(1), i=1&w=1.
        index = 1;
        weight = 1;
    elseif point >= grid(num)  % if x' < grid(ngrid), i=ngrid-1 & w=0.
        index = num-1;
        weight = 0;
    else
        ilow = 1; ihigh = num;

        distance = 2;

        while (distance > 1)

            inow = floor((ilow + ihigh)/2);
            valnow = grid(inow);

            % The strict inequality here ensures that grid(iloc) is less than or equal to point.
            if (valnow > point)
                ihigh = inow;
            else
                ilow = inow;
            end

            distance = ihigh - ilow;
        end

        index = ilow;
        weight = (grid(index+1) - point)/(grid(index+1) - grid(index));

    end

end
