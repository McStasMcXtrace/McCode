% MXDISPLAY-MATPLOTLIB(1)
% McXtrace X-ray Ray Tracing Team
% July 2024

# NAME

**mxdisplay-matplotlib** - McXtrace Graphical display of simulations using Matplotlib

# SYNOPSIS

**mxdisplay-matplotlib** [-h] INSTR name=value...

# DESCRIPTION

The **mxdisplay-matplotlib** front-end is a graphical debugging tool. It presents a
schematic drawing of the instrument definition, showing the position of the
components and the paths of the simulated photons through the instrument. It is
thus very useful for debugging a simulation, for example to spot components in
the wrong position or to find out where photons are getting lost. To use the
**mxdisplay-matplotlib** front-end with a simulation, run it as follows: 
`mxdisplay-matplotlib INSTR args...` where INSTR is the name of either the instrument 
source `INSTR.instr` or the simulation program `INSTR.out` generated with
McXtrace, and `args` are the normal command line arguments for the simulation,
as for *mxrun*. The `-h` option will list valid options.

This is the Matplotlib plotting tool.

# OPTIONS

**INSTR**
:   Displays the given McCode 3D model with its defaults/current parameters.

**name1=value1 ...**
:   Displays the given McCode model with given parameters.

**-n N, --ncount N**
:   Number of particles to simulate

# FILES

/usr/share/mcxtrace/tools/Python

# EXAMPLES

Display the *Test_SX* example (Single crystal diffraction)
:   - `mxdisplay-matplotlib Test_SX.instr -n 1e2 TTH=13.4`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)

