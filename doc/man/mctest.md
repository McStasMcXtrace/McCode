% MCTEST(1)
% McStas Neutron Ray Tracing Team
% July 2024

# NAME

**mctest** - Test the McStas installation

# SYNOPSIS

**mctest** [-h] [--ncount NCOUNT] [--mpi MPI] [--openacc] [--config [CONFIG]] [--instr [INSTR]] [--mccoderoot [MCCODEROOT]] [--testroot [TESTROOT]] [--testdir [TESTDIR]] [--limit LIMIT] [--versions] [--verbose] [--skipnontest] [--suffix SUFFIX] [TESTVERSION]

# DESCRIPTION

The front-end **mctest** is a program to test and benchmarck the current McStas
installation.

# OPTIONS

**TESTVERSION**
:   mccode version to test

**-h, --help**
:   show this help message and exit

**--ncount NCOUNT**
:   ncount sent to mcrun

**--mpi MPI**
:   mpi nodecount sent to mcrun

**--openacc**
:   openacc flag sent to mcrun

**--config [CONFIG]**
:   test this specific config only - label name or absolute path

**--instr [INSTR]**
:   test only intruments matching this filter (py regex)

**--mccoderoot [MCCODEROOT]**
:   manually select root search folder for mccode installations

**--testroot [TESTROOT]**
:   output test results in a datetime folder in this root

**--testdir [TESTDIR]**
:   output test results directly in this dir (overrides testroot)

**--limit LIMIT**
:   test only the first [LIMIT] instrs in every version

**--versions**
:   display local versions info

**--verbose**
:   output a test/notest instrument status header before each test

**--skipnontest**
:   Skip compilation of instruments without a test

**--suffix SUFFIX**
:   Add suffix to test directory name, e.g. 3.x-dev_suffix

# FILES

/usr/share/mcstas/
/usr/share/mcstas/tools/Python/mccodelib/mccode_config.json
~/.mcstas/mccode_config.json

# EXAMPLES

`mctest`

# AUTHORS

McStas Team (mcstas.org)

# SEE ALSO

mcstas(1), mcdoc(1), mcplot(1), mcrun(1), mcgui(1), mcdisplay(1)
