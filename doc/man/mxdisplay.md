% MXDISPLAY(1)
% McXtrace Team
% July 2024

# NAME

**mxdisplay** - McXtrace Graphical display of simulations

# SYNOPSIS

**mxdisplay** [-h] [--default] [--dirname DIRNAME] [--inspect INSPECT] [--invcanvas] [-n N] INSTR [options ...]

# DESCRIPTION

The **mxdisplay** front-end is a graphical debugging tool. It presents a
schematic drawing of the instrument definition, showing the position of the
components and the paths of the simulated photons through the instrument. It is
thus very useful for debugging a simulation, for example to spot components in
the wrong position or to find out where photons are getting lost. To use the
**mxdisplay** front-end with a simulation, run it as follows: 
`mxdisplay INSTR args...` where INSTR is the name of either the instrument 
source `INSTR.instr` or the simulation program `INSTR.out` generated with
McXtrace, and `args` are the normal command line arguments for the simulation,
as for *mxrun*. The `-h` option will list valid options.

The default plotting backend is `mxdisplay-pyqtgraph`, but there exists a number of additional plotters such as `mxdisplay-matplotlib`, `mxdisplay-webgl-classic` (using WebGL), `mxdisplay-webgl` (using WebGL and NodeJS), `mxdisplay-matlab` (using Matlab or Octave).

# OPTIONS

**-h, --help**
:   show this help message and exit

**--default**
:   automatically use instrument defaults for simulation run

**--dirname DIRNAME**
:   output directory name override

**--inspect INSPECT**
:   display only particle rays reaching this component

**--invcanvas**
:   invert canvas background from black to white

**-n N, --ncount N**
:   Number of particles to simulate

# FILES

/usr/share/mcxtrace/tools/Python/mccodelib/mccode_config.json
~/.mcxtrace/mccode_config.json

# EXAMPLES

Display the *Test_SX* example (Single crystal diffraction)
:   - `mxdisplay Test_SX.instr -d output_dir -n 1e7 TTH=13.4`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)

