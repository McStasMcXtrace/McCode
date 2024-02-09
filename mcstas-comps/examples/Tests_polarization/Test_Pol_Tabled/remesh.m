function remesh(infile, xsiz, ysiz, zsiz)
%REMESH Resample magnetic vector field data onto a new grid.
%   REMESH(INFILE, XSIZ, YSIZ, ZSIZ) resamples the magnetic vector field
%   data loaded from INFILE onto a new grid defined by the dimensions XSIZ,
%   YSIZ, and ZSIZ. The resampled data is saved to a new file with a name
%   constructed based on the dimensions and the input filename.
%
%   Input arguments:
%   - infile:   Input file containing magnetic vector field data
%   - xsiz:     Number of points along the x-axis in the new grid
%   - ysiz:     Number of points along the y-axis in the new grid
%   - zsiz:     Number of points along the z-axis in the new grid

% Check if all input arguments are provided
if nargin < 4
    error('I need precisely 4 inputs: infile, xsiz, ysiz, zsiz');
end

% Load data from the input file
data = load(infile);

% Extract coordinates and magnetic field components
XYZ = data(:, [1 2 3]); % Coordinates (x, y, z)
BXYZ = data(:, [4 5 6]); % Magnetic field components (Bx, By, Bz)

% Create a scattered interpolant for interpolation
BInterp = scatteredInterpolant(XYZ, BXYZ);

% Determine the range of coordinates
xmin = min(XYZ(:, 1));
xmax = max(XYZ(:, 1));
ymin = min(XYZ(:, 2));
ymax = max(XYZ(:, 2));
zmin = min(XYZ(:, 3));
zmax = max(XYZ(:, 3));

% Generate new grid coordinates
Xnew = linspace(xmin, xmax, xsiz);
Ynew = linspace(ymin, ymax, ysiz);
Znew = linspace(zmin, zmax, zsiz);

% Create meshgrid for new coordinates
[Xmesh, Ymesh, Zmesh] = meshgrid(Xnew, Ynew, Znew);

% Flatten meshgrid arrays
XYZnew = [Xmesh(:), Ymesh(:), Zmesh(:)];

% Interpolate magnetic field at new points
BXYZnew = BInterp(XYZnew);

% Construct output filename
outfile = sprintf('remeshed_%dx%dx%d_%s', xsiz, ysiz, zsiz, infile);

% Combine coordinates and interpolated magnetic field
Table = [XYZnew, BXYZnew];

% Save data to output file in ASCII format
save(outfile, 'Table', '-ascii');

