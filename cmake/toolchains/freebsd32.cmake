# the name of the target operating system
SET(CMAKE_SYSTEM_NAME "FreeBSD")
SET(ARCH   "i386")

# Commands for running various external tools
set(BROWSER "xdg-open")
set(TERMINAL "xterm -e")
set(VRMLVIEW "whitedune")
set(TOOLS_CC "clang")
set(MPICC "mpicc")
set(MPIRUN "mpirun")
set(PGPLOT "pgxwin_server")
set(GNUPLOT "gnuplot")

# Plotting options

set(PLOTTER "PGPLOT")
set(PGDEV "/xserv")
set(GNUDEV "wxt")

# Suffix for executables
set(EXE "out")
