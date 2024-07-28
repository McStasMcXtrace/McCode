% MCDISPLAY-MATLAB(1)
% McStas Neutron Ray Tracing Team
% July 2024

# NAME

**mcdisplay-matlab** - McStas Graphical display of simulations using Matlab/Octave

# SYNOPSIS

**mcdisplay-matlab** [-hom] [-png|-jpg|-fig|-eps|-pdf|-tif] [--inspect=COMP] INSTR name=value...

# DESCRIPTION

The **mcdisplay-matlab** front-end is a graphical debugging tool. It presents a
schematic drawing of the instrument definition, showing the position of the
components and the paths of the simulated neutrons through the instrument. It is
thus very useful for debugging a simulation, for example to spot components in
the wrong position or to find out where neutrons are getting lost. To use the
**mcdisplay-matlab** front-end with a simulation, run it as follows: 
`mcdisplay INSTR args...` where INSTR is the name of either the instrument 
source `INSTR.instr` or the simulation program `INSTR.out` generated with
McStas, and `args` are the normal command line arguments for the simulation,
as for *mcrun*. The `-h` option will list valid options.

This is the Matlab/Octave plotting tool.

# OPTIONS

**INSTR**
:   Displays the given McCode 3D model with its defaults/current parameters.

**name1=value1 ...**
:   Displays the given McCode model with given parameters.

**-n N, --ncount N**
:   Number of particles to simulate

**[-png|-jpg|-fig|-eps|-pdf|-tif]**
:   Same  as above, and specifies an output file format to generate.
    Possible save_as are -png -pdf -fig -tif -jpg -eps

**--inspect=COMP**
:   Same as above, and only plot component names that match `COMP`, given as
    a  single  component  word  for partial match, such as Monitor a
    component interval such as Monok:Sample or 2:10 or 2:end

**-m**
:   Explicitely request to use Matlab

**-o**
:   Explicitely request to use Octave

# FILES

/usr/share/mcstas/tools/matlab

# EXAMPLES

Display the *Test_SX* example (Single crystal diffraction)
:   - `mcdisplay-matlab Test_SX.instr -n 1e2 TTH=13.4`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mc(1), mcrun(1), mcgui(1), mcdisplay(1)

