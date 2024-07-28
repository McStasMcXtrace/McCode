% MCDISPLAY-WEBGL-CLASSIC(1)
% McStas Neutron Ray Tracing Team
% July 2024

# NAME

**mcdisplay-webgl-classic** - McStas Graphical display of simulations using WebGL (in a browser)

# SYNOPSIS

**mcdisplay-webgl-classic** [-h] [--default] [--dirname DIRNAME] [--inspect INSPECT] [--nobrowse] [--invcanvas] [--first FIRST] [--last LAST] [-n N] INSTR [options ...]

# DESCRIPTION

The **mcdisplay-webgl-classic** front-end is a graphical debugging tool. It presents a
schematic drawing of the instrument definition, showing the position of the
components and the paths of the simulated neutrons through the instrument. It is
thus very useful for debugging a simulation, for example to spot components in
the wrong position or to find out where neutrons are getting lost. To use the
**mcdisplay-webgl-classic** front-end with a simulation, run it as follows: 
`mcdisplay-webgl-classic INSTR args...` where INSTR is the name of either the instrument 
source `INSTR.instr` or the simulation program `INSTR.out` generated with
McStas, and `args` are the normal command line arguments for the simulation,
as for *mcrun*. The `-h` option will list valid options.

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

/usr/share/mcstas/tools/Python

# EXAMPLES

Display the *Test_SX* example (Single crystal diffraction)
:   - `mcdisplay-webgl-classic Test_SX.instr -n 1e2 TTH=13.4`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)

