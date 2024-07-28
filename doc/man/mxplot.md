% MXPLOT(1)
% McXtrace X-ray Ray Tracing Team
% July 2024

# NAME

**mxplot** - Plotting the results of a McXtrace simulation

# SYNOPSIS

**mxplot** [-h] [-t] [--invcanvas] [simulation ...]

# DESCRIPTION

The front-end **mxplot** is a program that produces plots of
all the monitors in a simulation, and it is thus useful to get a quick overview
of the simulation results. In the simplest case, the front-end is run simply by
typing `mxplot`. This will plot any simulation data stored in the current
directory, which is where simulations store their results by default. If the
--dir or --file options have been used (see section 5.2), the name of the file
or directory should be passed to *mxplot*, e.g. `mxplot dir` or `mxplot file`. It
is also possible to plot one single text (not binary) data file from a given
monitor, passing its name to `mxplot`. The `-h` option will list valid options.

The default plotting backend is `mxplot-pyqtgraph`, but there exists a number of additional plotters such as `mxplot-matplotlib`, `mxplot-svg` (in a browser), `mxplot-matlab` (using Matlab or Octave).

# OPTIONS

**simulation**
:   file or directory to plot

   options:
**-h, --help**
:   show this help message and exit

**-t, --test**
:   mccode data loader test run

**--invcanvas**
:   invert canvas background from black to white

# EXAMPLES

Run and plot the *Test_SX* example (Single crystal diffraction)
:   - `mxrun Test_SX.instr -d output_dir -n 1e7 TTH=13.4`
:   - `mxplot output_dir`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)
