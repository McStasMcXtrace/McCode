# the name of the target operating system
SET(CMAKE_SYSTEM_NAME "Linux")
SET(ARCH   "amd64")

# Commands for running various external tools
set(BROWSER "xdg-open")
set(TERMINAL "x-terminal-emulator -e")
set(TOOLS_CC "gcc")
set(MPICC "mpicc")
set(OACCFLAGS "-fast -Minfo=accel -acc=gpu -gpu=managed -DOPENACC")
set(MPILIB "mpi")
set(MPIRUN "mpirun")
set(NEXUSLIB "/usr/lib")
set(NEXUSINCLUDE "/usr/include/nexus")
set(MCCODE_CFLAGS "-g -O2 -DNDEBUG -lm -std=c99 -D_POSIX_SOURCE")
set(EDITOR "gedit")
