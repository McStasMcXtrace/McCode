% MXPLOT-MATLAB(1)
% McXtrace X-ray Ray Tracing Team
% July 2024

# NAME

**mxplot-matlab** - Plotting the results of a McXtrace simulation using Matlab/Octave

# SYNOPSIS

**mxplot-matlab** [-hom] [-png|-jpg|-fig|-eps|-pdf] [FILE|DIR]

# DESCRIPTION

The front-end **mxplot-matlab** is a program that produces plots of
all the monitors in a simulation, and it is thus useful to get a quick overview
of the simulation results. In the simplest case, the front-end is run simply by
typing `mxplot-matlab`. This will plot any simulation data stored in the current
directory, which is where simulations store their results by default. If the
`--dir` or `--file` options have been used, the name of the file
or directory should be passed to *mxplot-matlab*, e.g. `mxplot-matlab dir` or `mxplot-matlab file`. It
is also possible to plot one single text (not binary) data file from a given
monitor, passing its name to `mxplot-matlab`. The `-h` option will list valid options.

This is the Matlab/Octave plotting tool.

# OPTIONS

**[-png|-jpg|-fig|-eps|-pdf]**
:   Export the specified monitor file or directory to given format.

**-m FILE|DIR**
:   Explicitely request to use Matlab

**-o**
:   Explicitely request to use Octave

# EXAMPLES

Run and plot the *Test_SX* example (Single crystal diffraction)
:   - `mxrun Test_SX.instr -d output_dir -n 1e7 TTH=13.4`
:   - `mxplot-matlab output_dir`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)

