# the name of the target operating system
set(CMAKE_SYSTEM_NAME "Darwin")
set(ARCH   "arm64")

# Commands for running various external tools
set(TOOLS_CC "/usr/bin/gcc")
set(TERMINAL "open ")
set(BROWSER "open")
set(VRMLVIEW "open")
set(MPICC "mpicc.silicon")
set(MPILIB "mpi")
set(OACCFLAGS "-ta:multicore -DOPENACC")
set(MPIRUN "mpirun")
set(PGPLOT "pgxwin_server")
set(GNUPLOT "gnuplot")
set(PERL "/usr/bin/perl")
set(MCCODE_CFLAGS "-g -O2 -lm -std=c99 -I/opt/homebrew/include -L/opt/homebrew/lib")
set(EDITOR "open")
set(QSCI "1")

# Plotting options

set(PLOTTER "PGPLOT")
set(PGDEV "/xserv")
set(GNUDEV "x11")

# Suffix for executables
set(EXE "out")

