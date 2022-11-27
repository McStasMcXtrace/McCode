# the name of the target operating system
SET(CMAKE_SYSTEM_NAME "Linux")
SET(ARCH   "x86_64")

# Commands for running various external tools
set(BROWSER "xdg-open")
set(TERMINAL "gnome-terminal -e")
set(TOOLS_CC "gcc")
set(MPICC "mpicc")
set(OACCFLAGS "-fast -Minfo=accel -acc=gpu -gpu=managed -DOPENACC")
set(MPIRUN "mpirun")
set(MPILIB "mpi")
set(MCCODE_CFLAGS "-g -O2 -DNDEBUG -lm -std=c99 -D_POSIX_SOURCE")
set(EDITOR "gedit")
