# the name of the target operating system
SET(CMAKE_SYSTEM_NAME "Windows")
SET(ARCH   64)

# which compilers to use for C and C++
SET(CMAKE_C_COMPILER x86_64-w64-mingw32-gcc)
SET(CMAKE_CXX_COMPILER x86_64-w64-mingw32-g++)
SET(CMAKE_RC_COMPILER x86_64-w64-mingw32-windres)

# here is the target environment located
SET(CMAKE_FIND_ROOT_PATH  /usr/x86_64-w64-mingw32)

# include general MinGW module
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
include(MinGW)

# Commands for running various external tools
set(BROWSER "start")
set(VRMLVIEW "start")
set(MPICC "mpicc.bat")
set(MPIRUN "mpiexec.exe")
set(PGPLOT "") # <- Empty indicates not available
set(GNUPLOT "gnuplot.exe")

# Plotting options
set(PLOTTER "PGPLOT")
set(PGDEV "/gw")
set(GNUDEV "windows")
