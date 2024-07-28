% MXPLOT-MATPLOTLIB(1)
% McXtrace X-ray Ray Tracing Team
% July 2024

# NAME

**mxplot-matplotlib** - Plotting the results of a McXtrace simulation using Matplotlib

# SYNOPSIS

**mxplot-matplotlib** [-h]  [-t]  [--html] [--format FORMAT] [--output OUTPUT] [--log] [--backend BACKEND] [simulation ...]

# DESCRIPTION

The front-end **mxplot-matplotlib** is a program that produces plots of all the
monitors in a simulation, and it is thus useful to get a quick overview of the
simulation results. In the simplest case, the front-end is run simply by typing
`mxplot-matplotlib`. This will plot any simulation data stored in the current
directory, which is where simulations store their results by default. If the
`--dir` or `--file` options have been used, the name of the file or directory
should be passed to *mxplot-matplotlib*, e.g. `mxplot-matplotlib dir` or
`mxplot-matplotlib file`. It is also possible to plot one single text (not
binary) data file from a given monitor, passing its name to `mxplot-matplotlib`.
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
:   - `mxrun Test_SX.instr -d output_dir -n 1e7 TTH=13.4`
:   - `mxplot-matplotlib output_dir`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)

