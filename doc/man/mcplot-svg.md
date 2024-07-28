% MCPLOT-SVG(1)
% McStas Neutron Ray Tracing Team
% July 2024

# NAME

**mcplot-svg** - Plotting the results of a McStas simulation in a browser

# SYNOPSIS

**mcplot-svg** [-h] [-n] [-l] [--autosize] [--libpath [LIBPATH ...]] [-o OUTPUT] [simulation ...]

# DESCRIPTION

The front-end **mcplot-svg** is a program that produces plots of all the
monitors in a simulation, and it is thus useful to get a quick overview of the
simulation results. In the simplest case, the front-end is run simply by typing
`mcplot-svg`. This will plot any simulation data stored in the current
directory, which is where simulations store their results by default. If the
`--dir` or `--file` options have been used, the name of the file or directory
should be passed to *mcplot-svg*, e.g. `mcplot-svg dir` or
`mcplot-svg file`. It is also possible to plot one single text (not
binary) data file from a given monitor, passing its name to `mcplot-svg`.
The `-h` option will list valid options.

This is the web/svg plotting tool (in browser).

# OPTIONS

**-h, --help**
:   show this help message and exit

**-n, --nobrowse**
:   do not open a webbrowser viewer

**-l, --log**
:   enable logscale on plot

**--autosize**
:   expand to window size on load

**--libpath [LIBPATH ...]**
:   js lib files path

**-o OUTPUT, --output OUTPUT**
:   specify output file (.html extension)

# EXAMPLES

Run and plot the *Test_SX* example (Single crystal diffraction)
:   - `mcrun Test_SX.instr -d output_dir -n 1e7 TTH=13.4`
:   - `mcplot-svg output_dir`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)
