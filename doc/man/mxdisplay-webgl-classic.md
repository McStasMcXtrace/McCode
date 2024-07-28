% MXDISPLAY-WEBGL-CLASSIC(1)
% McXtrace X-ray Ray Tracing Team
% July 2024

# NAME

**mxdisplay-webgl-classic** - McXtrace Graphical display of simulations using WebGL (in a browser)

# SYNOPSIS

**mxdisplay-webgl-classic** [-h] [--default] [--dirname DIRNAME] [--inspect INSPECT] [--nobrowse] [--invcanvas] [--first FIRST] [--last LAST] [-n N] INSTR [options ...]

# DESCRIPTION

The **mxdisplay-webgl-classic** front-end is a graphical debugging tool. It presents a
schematic drawing of the instrument definition, showing the position of the
components and the paths of the simulated photons through the instrument. It is
thus very useful for debugging a simulation, for example to spot components in
the wrong position or to find out where photons are getting lost. To use the
**mxdisplay-webgl-classic** front-end with a simulation, run it as follows: 
`mxdisplay-webgl-classic INSTR args...` where INSTR is the name of either the instrument 
source `INSTR.instr` or the simulation program `INSTR.out` generated with
McXtrace, and `args` are the normal command line arguments for the simulation,
as for *mxrun*. The `-h` option will list valid options.

This is the classic WebGL (browser) plotting tool.

# OPTIONS

**INSTR**
:   Displays the given McCode 3D model with its defaults/current parameters.

**name1=value1 ...**
:   Displays the given McCode model with given parameters.

**-h, --help**
:   show this help message and exit

**--default**
:   automatically use instrument defaults for simulation run

**--dirname DIRNAME**
:   output directory name override

**--inspect INSPECT**
:   display only particle rays reaching this component

**--nobrowse**
:   do not open a webbrowser viewer

**--invcanvas**
:   invert canvas background from black to white

**--first FIRST**
:   zoom range first component

**--last LAST**
:   zoom range last component

**-n N, --ncount N**
:   Number of particles to simulate

# FILES

/usr/share/mcxtrace/tools/Python

# EXAMPLES

Display the *Test_SX* example (Single crystal diffraction)
:   - `mxdisplay-webgl-classic Test_SX.instr -n 1e2 TTH=13.4`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)

