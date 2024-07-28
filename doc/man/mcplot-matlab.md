% MCPLOT-MATLAB(1)
% McStas Neutron Ray Tracing Team
% July 2024

# NAME

**mcplot-matlab** - Plotting the results of a McStas simulation using Matlab/Octave

# SYNOPSIS

**mcplot-matlab** [-hom] [-png|-jpg|-fig|-eps|-pdf] [FILE|DIR]

# DESCRIPTION

The front-end **mcplot-matlab** is a program that produces plots of
all the monitors in a simulation, and it is thus useful to get a quick overview
of the simulation results. In the simplest case, the front-end is run simply by
typing `mcplot-matlab`. This will plot any simulation data stored in the current
directory, which is where simulations store their results by default. If the
`--dir` or `--file` options have been used, the name of the file
or directory should be passed to *mcplot-matlab*, e.g. `mcplot-matlab dir` or `mcplot-matlab file`. It
is also possible to plot one single text (not binary) data file from a given
monitor, passing its name to `mcplot-matlab`. The `-h` option will list valid options.

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
:   - `mcrun Test_SX.instr -d output_dir -n 1e7 TTH=13.4`
:   - `mcplot-matlab output_dir`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)



/usr/bin/mcplot-matlab: plot a McCode simulation result using the Matlab/Octave backend.
### usage

Name | Description
---- | -----------
`/usr/bin/mcplot-matlab FILE|DIR Display the specified monitor file or directory.
`/usr/bin/mcplot-matlab [-png|-jpg|-fig|-eps|-pdf] [FILE|DIR] Export the specified monitor file or directory to given format.
`/usr/bin/mcplot-matlab -m FILE|DIR Explicitely request to use Matlab
`/usr/bin/mcplot-matlab -o FILE|DIR Explicitely request to use Octave
