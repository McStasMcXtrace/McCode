% MXTEST(1)
% McXtrace X-ray Ray Tracing Team
% July 2024

# NAME

**mxtest** - Test the McXtrace installation

# SYNOPSIS

**mxtest** [-h] [--ncount NCOUNT] [--mpi MPI] [--openacc] [--config [CONFIG]] [--instr [INSTR]] [--mccoderoot [MCCODEROOT]] [--testroot [TESTROOT]] [--testdir [TESTDIR]] [--limit LIMIT] [--versions] [--verbose] [--skipnontest] [--suffix SUFFIX] [TESTVERSION]

# DESCRIPTION

The front-end **mxtest** is a program to test and benchmarck the current McXtrace
installation.

# OPTIONS

**TESTVERSION**
:   mccode version to test

**-h, --help**
:   show this help message and exit

**--ncount NCOUNT**
:   ncount sent to mxrun

**--mpi MPI**
:   mpi nodecount sent to mxrun

**--openacc**
:   openacc flag sent to mxrun

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

/usr/share/mcxtrace/
/usr/share/mcxtrace/tools/Python/mccodelib/mccode_config.json
~/.mcxtrace/mccode_config.json

# EXAMPLES

`mxtest`

# AUTHORS

McXtrace Team (mcxtrace.org)

# SEE ALSO

mcxtrace(1), mxdoc(1), mxplot(1), mxrun(1), mxgui(1), mxdisplay(1)
