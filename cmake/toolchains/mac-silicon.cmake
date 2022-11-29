# the name of the target operating system
set(CMAKE_SYSTEM_NAME "Darwin")
set(ARCH   "arm64")

# Commands for running various external tools
set(TOOLS_CC "/usr/bin/gcc")
set(TERMINAL "open")
set(BROWSER "open")
set(MPICC "mpicc.clang")
set(MPILIB "mpi")
set(OACCFLAGS "-ta:multicore -DOPENACC")
set(MPIRUN "mpirun")
set(MCCODE_CFLAGS "-g -O2 -lm -DNDEBUG -std=c99 -I/opt/homebrew/include -L/opt/homebrew/lib")
set(EDITOR "open")
