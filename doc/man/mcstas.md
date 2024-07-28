% MCSTAS(1)
% McStas Neutron Ray Tracing Team
% July 2024

# NAME

**mcstas** Compiler of the McStas ray-trace simulation package

# SYNOPSIS

**mcstas** [-o file] [-I dir1 ...] [-t] [-p] [-v] [--no-main] [--no-runtime] [--verbose] file

# DESCRIPTION

The compiler for the McStas instrument definition is invoked by typing a
command of the form `mcstas name.instr` in a shell or command prompt.
This will read the beamline definition `name.instr` which is written in the
McStas meta-language. The compiler will translate the instrument definition
into a Monte Carlo simulation program provided in ISO-C. The output is by
default written to a file in the current directory with the same name as the
instrument file, but with extension `.c` rather than `.instr`. This can be
overridden using the `-o` option as follows: `mcstas -o code.c name.instr` which
gives the output in the file `code.c`. A single dash `-` may be used for both
input and output filename to represent standard input and standard output,
respectively. The default component search list is usually defined by the 
environment variable `MCSTAS`  (default is `/usr/share/mcstas/resources`).

Often, it will be more convenient to use the front-end program `mcgui` or
`mcrun`. These front-ends will compile and run the simulations automatically.

# OPTIONS

**-o FILE --output-file=FILE**
:   Place C output in file FILE.

**-I DIR --search-dir=DIR**
:   Append DIR to the component search list.

**-t --trace**
:   Enable 'trace' mode for instrument display.

**-v --version**
:   Prints McStas version.

**--no-main**
:   Do not create main(), for external embedding.

**--no-runtime**
:   Do not embed run-time libraries.

**--verbose**
:   Display compilation process steps.

**--source**
:   Embed the instrument source code in executable.

# FILES

/usr/share/mcstas/resources

# EXAMPLES

Translate the *Test_SX* example (Single crystal diffraction) into C
:   - `mcstas Test_SX.instr`
:   - `gcc Test_SX.instr -o Test_SX.out -lm`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)
