# the name of the target operating system
set(CMAKE_SYSTEM_NAME "Darwin")
set(ARCH   "amd64")

# Commands for running various external tools
set(TOOLS_CC "/usr/bin/clang")
set(TERMINAL "open ")
set(BROWSER "open")
set(MPICC "mpicc.clang")
set(OACCFLAGS "-ta:multicore -DOPENACC")
set(MPIRUN "mpirun")
set(MPILIB "mpi")
set(MCCODE_CFLAGS "-g -O2 -lm -std=c99")
set(EDITOR "open")
