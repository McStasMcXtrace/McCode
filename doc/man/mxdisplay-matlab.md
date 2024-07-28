% MXDISPLAY-MATLAB(1)
% McXtrace X-ray Ray Tracing Team
% July 2024

# NAME

**mxdisplay-matlab** - McXtrace Graphical display of simulations using Matlab/Octave

# SYNOPSIS

**mxdisplay-matlab** [-hom] [-png|-jpg|-fig|-eps|-pdf|-tif] [--inspect=COMP] INSTR name=value...

# DESCRIPTION

The **mxdisplay-matlab** front-end is a graphical debugging tool. It presents a
schematic drawing of the instrument definition, showing the position of the
components and the paths of the simulated photons through the instrument. It is
thus very useful for debugging a simulation, for example to spot components in
the wrong position or to find out where photons are getting lost. To use the
**mxdisplay-matlab** front-end with a simulation, run it as follows: 
`mxdisplay INSTR args...` where INSTR is the name of either the instrument 
source `INSTR.instr` or the simulation program `INSTR.out` generated with
McXtrace, and `args` are the normal command line arguments for the simulation,
as for *mxrun*. The `-h` option will list valid options.

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

/usr/share/mcxtrace/tools/matlab

# EXAMPLES

Display the *Test_SX* example (Single crystal diffraction)
:   - `mxdisplay-matlab Test_SX.instr -n 1e2 TTH=13.4`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)

