# the name of the target operating system
SET(CMAKE_SYSTEM_NAME "Windows")
SET(ARCH   "x86_64")

# which compilers to use for C and C++
SET(CMAKE_C_COMPILER x86_64-w64-mingw32-gcc)
SET(CMAKE_CXX_COMPILER x86_64-w64-mingw32-g++-posix)
set(CMAKE_CXX_FLAGS "-fpermissive")
SET(CMAKE_RC_COMPILER x86_64-w64-mingw32-windres)
SET(CMAKE_Fortran_COMPILER x86_64-w64-mingw32-gfortran)

# here is the target environment located
SET(CMAKE_FIND_ROOT_PATH  /usr/x86_64-w64-mingw32)

# include general MinGW module
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
include(MinGW)

# Commands for running various external tools
set(BROWSER "start")
set(TERMINAL "start")
set(TOOLS_CC "gcc")
set(OACCFLAGS "-ta:multicore -DOPENACC")
set(MPICC "mpicc.bat")
set(MPIRUN "mpiexec.exe")
set(MPILIB "msmpi")
set(MCCODE_CFLAGS "-g -O2 -DNDEBUG -lm -std=c99 -D_POSIX_SOURCE")
set(EDITOR "start")
