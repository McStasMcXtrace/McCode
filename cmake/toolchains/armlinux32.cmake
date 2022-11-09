# the name of the target operating system
SET(CMAKE_SYSTEM_NAME "Linux")
SET(ARCH   "armhf")
set(TOOLS_CC "gcc")

# Commands for running various external tools
set(BROWSER "xdg-open")
set(MPICC "mpicc")
set(MPIRUN "mpirun")
set(MPILIB "mpi")
set(MCCODE_CFLAGS "-g -O2 -lm -std=c99 -D_POSIX_SOURCE")
