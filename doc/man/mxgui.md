% MXGUI(1)
% McXtrace Team
% July 2024

# NAME

**mxgui** - The McXtrace graphical user interface

# SYNOPSIS

**mxgui** [INSTR]

# DESCRIPTION

The front-end **mxgui** provides a graphical user interface that interfaces the
various parts of the McXtrace package. It is started from a shell/command prompt
using simply the command mxgui The `mxgui` program may optionally be given the
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
:   - `mxgui Test_SX.instr`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)
