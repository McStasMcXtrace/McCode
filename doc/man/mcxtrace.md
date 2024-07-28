% MCXTRACE(1)
% McXtrace X-ray Ray Tracing Team
% July 2024

# NAME

**mcxtrace** Compiler of the McXtrace ray-trace simulation package

# SYNOPSIS

**mcxtrace** [-o file] [-I dir1 ...] [-t] [-p] [-v] [--no-main] [--no-runtime] [--verbose] file

# DESCRIPTION

The compiler for the McXtrace instrument definition is invoked by typing a
command of the form `mcxtrace name.instr` in a shell or command prompt.
This will read the beamline definition `name.instr` which is written in the
McXtrace meta-language. The compiler will translate the instrument definition
into a Monte Carlo simulation program provided in ISO-C. The output is by
default written to a file in the current directory with the same name as the
instrument file, but with extension `.c` rather than `.instr`. This can be
overridden using the `-o` option as follows: `mcxtrace -o code.c name.instr` which
gives the output in the file `code.c`. A single dash `-` may be used for both
input and output filename to represent standard input and standard output,
respectively. The default component search list is usually defined by the 
environment variable `MCXTRACE`  (default is `/usr/share/mcxtrace/resources`).

Often, it will be more convenient to use the front-end program `mxgui` or
`mxrun`. These front-ends will compile and run the simulations automatically.

# OPTIONS

**-o FILE --output-file=FILE**
:   Place C output in file FILE.

**-I DIR --search-dir=DIR**
:   Append DIR to the component search list.

**-t --trace**
:   Enable 'trace' mode for instrument display.

**-v --version**
:   Prints McXtrace version.

**--no-main**
:   Do not create main(), for external embedding.

**--no-runtime**
:   Do not embed run-time libraries.

**--verbose**
:   Display compilation process steps.

**--source**
:   Embed the instrument source code in executable.

# FILES

/usr/share/mcxtrace/resources

# EXAMPLES

Translate the *Test_SX* example (Single crystal diffraction) into C
:   - `mcxtrace Test_SX.instr`
:   - `gcc Test_SX.instr -o Test_SX.out -lm`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)
