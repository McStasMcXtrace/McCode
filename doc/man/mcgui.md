% MCGUI(1)
% McStas Neutron Ray-Tracing Team
% July 2024

# NAME

**mcgui** - The McStas graphical user interface

# SYNOPSIS

**mcgui** [INSTR]

# DESCRIPTION

The front-end **mcgui** provides a graphical user interface that interfaces the
various parts of the McStas package. It is started from a shell/command prompt
using simply the command mcgui The `mcgui` program may optionally be given the
name INSTR of an instrument file. When the front-end is started, a main window
is opened. This window displays the output from compiling and running
simulations, and also contains a few menus and buttons. The main purpose of the
front-end is to edit and compile instrument definitions, run the simulations,
and visualize the results.

# OPTIONS

**INSTR**
:   optional name of the instrument to open.

# EXAMPLES

Open the *Test_SX* example (Single crystal diffraction)
:   - `mcgui Test_SX.instr`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)
