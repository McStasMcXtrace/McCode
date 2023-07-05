# the name of the target operating system
set(CMAKE_SYSTEM_NAME "Darwin")
set(ARCH   "arm64")

# Commands for running various external tools
set(TOOLS_CC "/usr/bin/clang")
set(TERMINAL "open")
set(BROWSER "open")
set(MPICC "mpicc.clang")
set(MPILIBDIR "GETPATH(miniconda3/lib)")
set(MPIINCLUDEDIR "GETPATH(miniconda3/include)")
set(OACCFLAGS "-ta:multicore -DOPENACC")
set(MPIRUN "mpirun")
set(MPILIB "mpi")
set(NEXUSLIB "GETPATH(lib)")
set(NEXUSINCLUDE "GETPATH(include/nexus)")
set(MCCODE_CFLAGS "-g -O2 -DNDEBUG -lm -std=c99")
set(EDITOR "open")
