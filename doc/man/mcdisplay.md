% MCDISPLAY(1)
% McStas Neutron Ray-Tracing Team
% July 2024

# NAME

**mcdisplay** - McStas Graphical display of simulations

# SYNOPSIS

**mcdisplay** [-h] [--default] [--dirname DIRNAME] [--inspect INSPECT] [--invcanvas] [-n N] INSTR [options ...]

# DESCRIPTION

The **mcdisplay** front-end is a graphical debugging tool. It presents a
schematic drawing of the instrument definition, showing the position of the
components and the paths of the simulated neutrons through the instrument. It is
thus very useful for debugging a simulation, for example to spot components in
the wrong position or to find out where neutrons are getting lost. To use the
**mcdisplay** front-end with a simulation, run it as follows: 
`mcdisplay INSTR args...` where INSTR is the name of either the instrument 
source `INSTR.instr` or the simulation program `INSTR.out` generated with
McStas, and `args` are the normal command line arguments for the simulation,
as for *mcrun*. The `-h` option will list valid options.

The default plotting backend is `mcdisplay-pyqtgraph`, but there exists a number of additional plotters such as `mcdisplay-matplotlib`, `mcdisplay-webgl-classic` (using WebGL), `mcdisplay-webgl` (using WebGL and NodeJS), `mcdisplay-matlab` (using Matlab or Octave).

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

/usr/share/mcstas/tools/Python/mccodelib/mccode_config.json
~/.mcstas/mccode_config.json

# EXAMPLES

Display the *Test_SX* example (Single crystal diffraction)
:   - `mcdisplay Test_SX.instr -d output_dir -n 1e7 TTH=13.4`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)

