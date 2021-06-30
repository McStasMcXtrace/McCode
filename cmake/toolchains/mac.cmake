# the name of the target operating system
set(CMAKE_SYSTEM_NAME "Darwin")
set(ARCH   "amd64")

# Commands for running various external tools
set(TOOLS_CC "/usr/bin/clang")
set(TERMINAL "open ")
set(BROWSER "open")
set(VRMLVIEW "open")
set(MPICC "mpicc.clang")
set(MPIRUN "mpirun")
set(PGPLOT "pgxwin_server")
set(GNUPLOT "gnuplot")
set(PERL "/Applications/PDL/bin/perl")
set(MCCODE_CFLAGS "-g -O2 -lm -std=c99")

# Plotting options

set(PLOTTER "PGPLOT")
set(PGDEV "/xserv")
set(GNUDEV "x11")

# Suffix for executables
set(EXE "out")

