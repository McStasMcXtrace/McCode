# the name of the target operating system
SET(CMAKE_SYSTEM_NAME "FreeBSD")
SET(ARCH   "i386")

# Commands for running various external tools
set(BROWSER "xdg-open")
set(TERMINAL "xterm -e")
set(TOOLS_CC "clang")
set(MPICC "mpicc")
set(MPILIB "mpi")
set(MPIRUN "mpirun")
set(MCCODE_CFLAGS "-g -O2 -lm -std=c99 -D_POSIX_SOURCE")
set(EDITOR "gedit")
