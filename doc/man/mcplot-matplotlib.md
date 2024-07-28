% MCPLOT-MATPLOTLIB(1)
% McStas Neutron Ray Tracing Team
% July 2024

# NAME

**mcplot-matplotlib** - Plotting the results of a McStas simulation using Matplotlib

# SYNOPSIS

**mcplot-matplotlib** [-h]  [-t]  [--html] [--format FORMAT] [--output OUTPUT] [--log] [--backend BACKEND] [simulation ...]

# DESCRIPTION

The front-end **mcplot-matplotlib** is a program that produces plots of all the
monitors in a simulation, and it is thus useful to get a quick overview of the
simulation results. In the simplest case, the front-end is run simply by typing
`mcplot-matplotlib`. This will plot any simulation data stored in the current
directory, which is where simulations store their results by default. If the
`--dir` or `--file` options have been used, the name of the file or directory
should be passed to *mcplot-matplotlib*, e.g. `mcplot-matplotlib dir` or
`mcplot-matplotlib file`. It is also possible to plot one single text (not
binary) data file from a given monitor, passing its name to `mcplot-matplotlib`.
The `-h` option will list valid options.

This is the Matplotlib plotting tool.

# OPTIONS

**simulation**
:   file or directory to plot

**-h, --help**
:   show this help message and exit

**-t, --test**
:   mccode data loader test run

**--html**
:   save plot to html using mpld3 (linux only)

**--format FORMAT**
:   save plot to pdf/png/eps/svg... without bringing up window

**--output OUTPUT**
:   save plot to given file without bringing up  window.   Extension
    (e.g.  pdf/png/eps/svg)  can  be  specified  in the file name or `--format`

**--log**
:   initiate plot(s) with log of signal

**--backend BACKEND**
:   use non-default backend for matplotlib plot

# EXAMPLES

Run and plot the *Test_SX* example (Single crystal diffraction)
:   - `mcrun Test_SX.instr -d output_dir -n 1e7 TTH=13.4`
:   - `mcplot-matplotlib output_dir`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)

