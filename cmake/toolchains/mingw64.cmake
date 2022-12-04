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

# adjust the default behaviour of the FIND_XXX() commands:
# search headers and libraries in the target environment, search
# programs in the host environment
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

# Fetch ZLIB
set(MCPL_ENABLE_ZLIB FETCH)

# Python and Perl suffixes
set(PYTHON_SUFFIX "py")
set(PERL_SUFFIX "pl")

# Set executable extension
set(EXE_SUFFIX   "exe")
set(CMAKE_EXECUTABLE_SUFFIX ".exe")

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
