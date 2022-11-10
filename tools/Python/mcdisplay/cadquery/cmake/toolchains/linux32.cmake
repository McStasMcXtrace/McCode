# the name of the target operating system
SET(CMAKE_SYSTEM_NAME "Linux")
SET(ARCH   "i386")

# Commands for running various external tools
set(BROWSER "xdg-open")
set(TERMINAL "xterm -e")
set(VRMLVIEW "whitedune")
set(TOOLS_CC "gcc")
set(MPICC "mpicc")
set(MPILIB "mpi")
set(MPIRUN "mpirun")
set(PGPLOT "pgxwin_server")
set(GNUPLOT "gnuplot")
set(MCCODE_CFLAGS "-g -O2 -lm -std=c99 -D_POSIX_SOURCE")
set(EDITOR "gedit")
set(QSCI "1")

# Plotting options

set(PLOTTER "PGPLOT")
set(PGDEV "/xserv")
set(GNUDEV "wxt")

# Suffix for executables
set(EXE "out")

