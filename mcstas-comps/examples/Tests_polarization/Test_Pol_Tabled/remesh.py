import argparse
import numpy as np
from scipy.interpolate import griddata

def remesh(infile, xsiz, ysiz, zsiz):
    data = np.loadtxt(infile)
    
    XYZ = data[:, :3]
    BXYZ = data[:, 3:]
    
    xmin, xmax = np.min(XYZ[:,0]), np.max(XYZ[:,0])
    ymin, ymax = np.min(XYZ[:,1]), np.max(XYZ[:,1])
    zmin, zmax = np.min(XYZ[:,2]), np.max(XYZ[:,2])
    
    Xnew = np.linspace(xmin, xmax, xsiz)
    Ynew = np.linspace(ymin, ymax, ysiz)
    Znew = np.linspace(zmin, zmax, zsiz)
    
    Xmesh, Ymesh, Zmesh = np.meshgrid(Xnew, Ynew, Znew, indexing='ij')
    
    XYZnew = np.column_stack([Xmesh.ravel(), Ymesh.ravel(), Zmesh.ravel()])
    
    # Interpolate the magnetic field at the new points
    BInterp = griddata(XYZ, BXYZ, XYZnew, method='linear')
    
    outfile = f"remeshed_{xsiz}x{ysiz}x{zsiz}_{infile}"
    
    Table = np.column_stack([XYZnew, BInterp])
    
    # Calculate the product of xsiz, ysiz, and zsiz as an integer
    N = int(xsiz * ysiz * zsiz)
    
    # Add header to the output file
    header = f"# {N} ({xsiz} x {ysiz} x {zsiz})\n"
    
    # Format Table as string with scientific notation and 2 decimal places
    Table_str = "\n".join([" ".join([f"{val:.2e}" for val in row]) for row in Table])
    
    with open(outfile, 'w') as f:
        f.write(header)
        f.write(Table_str)

def main():
    parser = argparse.ArgumentParser(description='Remesh magnetic vector field data.')
    parser.add_argument('infile', type=str, help='Input file containing magnetic vector field data')
    parser.add_argument('xsiz', type=int, help='Number of points along the x-axis in the new grid')
    parser.add_argument('ysiz', type=int, help='Number of points along the y-axis in the new grid')
    parser.add_argument('zsiz', type=int, help='Number of points along the z-axis in the new grid')
    
    args = parser.parse_args()
    
    remesh(args.infile, args.xsiz, args.ysiz, args.zsiz)

if __name__ == "__main__":
    main()

