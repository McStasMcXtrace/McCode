% MCPLOT(1)
% McStas Neutron Ray-Tracing Team
% July 2024

# NAME

**mcplot** - Plotting the results of a McStas simulation

# SYNOPSIS

**mcplot** [-h] [-t] [--invcanvas] [simulation ...]

# DESCRIPTION

The front-end **mcplot** is a program that produces plots of
all the monitors in a simulation, and it is thus useful to get a quick overview
of the simulation results. In the simplest case, the front-end is run simply by
typing `mcplot`. This will plot any simulation data stored in the current
directory, which is where simulations store their results by default. If the
--dir or --file options have been used (see section 5.2), the name of the file
or directory should be passed to *mcplot*, e.g. `mcplot dir` or `mcplot file`. It
is also possible to plot one single text (not binary) data file from a given
monitor, passing its name to `mcplot`. The `-h` option will list valid options.

The default plotting backend is `mcplot-pyqtgraph`, but there exists a number of additional plotters such as `mcplot-matplotlib`, `mcplot-svg` (in a browser), `mcplot-matlab` (using Matlab or Octave).

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
:   - `mcrun Test_SX.instr -d output_dir -n 1e7 TTH=13.4`
:   - `mcplot output_dir`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)
