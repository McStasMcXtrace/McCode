# the name of the target operating system
SET(CMAKE_SYSTEM_NAME "Linux")
SET(ARCH   "armhf")
set(TOOLS_CC "gcc")

# Commands for running various external tools
set(BROWSER "xdg-open")
set(VRMLVIEW "whitedune")
set(MPICC "mpicc")
set(MPIRUN "mpirun")
set(MPILIB "mpi")
set(PGPLOT "pgxwin_server")
set(GNUPLOT "gnuplot")
set(MCCODE_CFLAGS "-g -O2 -lm -std=c99 -D_POSIX_SOURCE")

# Plotting options

set(PLOTTER "PGPLOT")
set(PGDEV "/xserv")
set(GNUDEV "wxt")

# Suffix for executables
set(EXE "out")

