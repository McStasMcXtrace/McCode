# the name of the target operating system
set(CMAKE_SYSTEM_NAME "Darwin")
set(ARCH   "amd64")

# Commands for running various external tools
set(CC "gcc")
set(BROWSER "open")
set(VRMLVIEW "open")
set(MPICC "mpicc.gcc")
set(MPIRUN "mpirun --mca btl self,sm,vader")
set(PGPLOT "pgxwin_server")
set(GNUPLOT "gnuplot")
set(PERL "/usr/bin/perl")
set(CFLAGS "-g -lm -O2 -headerpad_max_install_names")

# Plotting options

set(PLOTTER "PGPLOT")
set(PGDEV "/xserv")
set(GNUDEV "x11")

# Suffix for executables
set(EXE "out")

