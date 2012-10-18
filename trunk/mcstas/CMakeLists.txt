cmake_minimum_required(VERSION 2.6)

## This file will build McStas
project(mcstas)
set(FLAVOR     "mcstas")
set(FLAVOR_LIB "nlib")


## We are building with CMake and need to let port.h know this
add_definitions(
  -DCMake
)


## If this value is not empty, we have the following macros from mkdist:
## @MCCODE_DATE@, @MCCODE_STRING@, @MCCODE_NAME@, @MCCODE_TARNAME@, @MCCODE_VERSION@
set(MKDIST "@MCCODE_DATE@")

if(MKDIST STREQUAL "@MCCODE_DATE@")
  ## Set SVN-specific version
  set(MAJOR "2")
  set(MINOR "9999")
  set(MCCODE_VERSION "${MAJOR}.${MINOR}-svn")
  set(MCCODE_NAME "McStas")
  set(MCCODE_DATE "svn")
  set(MCCODE_STRING "McStas ${MCCODE_VERSION}, ${MCCODE_DATE}")
  set(MCCODE_TARNAME "${FLAVOR}")
else()
  ## Set mkdist-provided version
  set(MAJOR "2")  # TODO: Replace with information from ${VERSION}
  set(MINOR "0")
  set(MCCODE_VERSION "@MCCODE_VERSION@")
  set(MCCODE_NAME "@MCCODE_NAME@")
  set(MCCODE_DATE "@MCCODE_DATE@")
  set(MCCODE_STRING "@MCCODE_STRING@")
  set(MCCODE_TARNAME "@MCCODE_TARNAME")
endif()

set(MCCODE_PARTICLE "neutron")
set(MCCODE_LIBENV   "MCSTAS")
set(MCCODE_PROJECT  1)


## CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}")
set(CPACK_BUNDLE_NAME          "${FLAVOR}")
set(CPACK_BUNDLE_PLIST          "${FLAVOR}")
set(CPACK_BUNDLE_ICON          "${FLAVOR}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_VERSION_MAJOR "${MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${MINOR}")
set(CPACK_PACKAGE_VERSION       "${MCCODE_VERSION}")
set(CPACK_PACKAGE_DEPENDS       "")

set(CPACK_PACKAGE_CONTACT "jsbn@fysik.dtu.dk") #required

include(CPack)


## Add global definitions
add_definitions(
	-DMCCODE_NAME=\"${MCCODE_NAME}\"
	-DMCCODE_TARNAME=\"${MCCODE_TARNAME}\"
	-DMCCODE_VERSION=\"${MCCODE_VERSION}\"
	-DMCCODE_STRING=\"${MCCODE_STRING}\"
	-DMCCODE_BUGREPORT=\"www.mcstas.org\"
	-DMCCODE_URL=\"\"

	# -DCC_HAS_PROTOS=1
	# -DSTDC_HEADERS=1
	# -DHAVE_THREADS=\"-DUSE_THREADS\ \$$OPENMP_CFLAGS\ \"
)



## User-adjustable options
option (USE_NEXUS
  "Support the NEXUS file format" OFF)

option (USE_THREADS
  "Enable threading; OBSOLETE: Use MPI/SSH grid feature instead." OFF)



## Functionality needed to check dependencies
include (CheckFunctionExists)
include (CheckLibraryExists)
include (CheckIncludeFiles)


# A macro for ensuring that all of the values in a variable list are true
macro(check_vars vars errmsg)
  foreach(var ${vars})
    if(NOT ${var})
      # throw fatal error when seeing a false value
      message(FATAL_ERROR ${errmsg})
    endif()
  endforeach()
endmacro()


## Check system configuration and dependencies

# REQUIRED
check_function_exists(malloc      HAVE_MALLOC)
check_function_exists(realloc     HAVE_REALLOC)
check_vars(
  "${HAVE_MALLOC};${HAVE_REALLOC}"
  "Missing either malloc or realloc!")

check_include_files("stdlib.h"    HAVE_STDLIB_H)
check_include_files("memory.h"    HAVE_MEMORY_H)
check_include_files("unistd.h"    HAVE_UNISTD_H)
check_vars(
  "${HAVE_STDLIB_H};${HAVE_MEMORY_H};${HAVE_UNISTD_H}"
  "Missing either stdlib.h, memory.h or unistd.h!"
)

check_include_files("inttypes.h"  HAVE_INT_TYPES_H)
check_include_files("stdint.h"    HAVE_STD_INT_H)
check_vars(
  "${HAVE_INT_TYPES_H};${HAVE_STD_INT_H}"
  "Missing either inttypes.h or stdint.h"
)

check_include_files("sys/types.h" HAVE_SYS_TYPES_H)
check_include_files("sys/stat.h"  HAVE_SYS_STAT_H)
check_vars(
  "${HAVE_SYS_TYPES_H};${HAVE_SYS_STAT_H}"
  "Missing either sys/types.h or sys/stat.h"
)

check_include_files("string.h"    HAVE_STRING_H)
check_include_files("strings.h"   HAVE_STRINGS_H)
check_vars(
  "${HAVE_STRING_H};${HAVE_STRINGS_H}"
  "Missing either string.h or strings.h"
)


# Check for math
check_library_exists(m sqrt "" HAVE_MATH)
if(NOT HAVE_MATH)
  message(FATAL_ERROR "Error: Cannot find sqrt in math library [m]")
endif()


# Check for BISON and FLEX (will fail if they are not found)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
message("-- Looking for bison and flex")
find_package(BISON)
find_package(FLEX)
message("-- Looking for bison and flex - found")


# OPTIONAL
check_function_exists(strcasecmp  HAVE_STRCASECMP)
check_function_exists(strcasestr HAVE_STRCASESTR)
check_function_exists(fdopen      HAVE_FDOPEN)
check_function_exists(qsort       HAVE_QSORT)



# Create work directory, where all rewritten source files go to
# (to support in-place builds)
file(MAKE_DIRECTORY "work")


## Rules for processing files while expanding macros

# Macro for configuring every file in a directory
# *.in files are configured, while other files are copied unless target exists
macro(configure_directory IN_GLOB OUT_DIR)
  file(GLOB MAN_IN_FILES "" "${IN_GLOB}")
  foreach(file_in ${MAN_IN_FILES})
    get_filename_component(filename "${file_in}" NAME)      # /doc/man/example.1.in -> example.1.in
    string(REGEX MATCH "^(.+)\\.in" matches "${filename}")  # example.1.in -> example.1
    if(matches)
      # from IN/doc/man/example.1.in -> OUT/doc/man/example.1
      configure_file (
        "${file_in}"
        "${OUT_DIR}/${CMAKE_MATCH_1}"
        )
    else()
      # do not overwrite files created by configure
      if(NOT (EXISTS "${OUT_DIR}/${filename}"))
        file(
          COPY "${file_in}"
          DESTINATION "${OUT_DIR}")
      endif()
    endif()
  endforeach()
endmacro()

# Generate c and header files
configure_file (
  "../config.h.in"
  "work/src/config.h"
)

configure_directory ("lib/*" "work/lib")
configure_directory ("lib/share/*" "work/lib/share")

configure_directory ("src/*" "work/src")


# Generate man pages
message("-- Preparing man files")
file(MAKE_DIRECTORY "work/doc/man")
configure_directory("doc/man/*" "work/doc/man")


## Include source directories for building
include_directories(
  "${PROJECT_BINARY_DIR}/work/src"             # rewritten files from output dir
  "${PROJECT_BINARY_DIR}/work/lib/share"       # rewritten library files
  "${PROJECT_SOURCE_DIR}/${FLAVOR_LIB}/share"  # lib depending on flavor (nlib/xlib)
)


## Generate lex.yy.c with flex
add_custom_command(
  OUTPUT work/src/lex.yy.c
  COMMAND "${FLEX_EXECUTABLE}" -i "${PROJECT_SOURCE_DIR}/src/instrument.l"
  WORKING_DIRECTORY work/src
)


## Generate instrument.tab.{h,c} with bison
add_custom_command(
  OUTPUT work/src/instrument.tab.h work/src/instrument.tab.c
  COMMAND "${BISON_EXECUTABLE}" -v -d "${PROJECT_SOURCE_DIR}/src/instrument.y"
  WORKING_DIRECTORY work/src
)


## Build executable for flavor
add_executable(
	${FLAVOR}
	work/src/cexp.c
	work/src/cogen.c
	work/src/coords.c
	work/src/debug.c
  work/src/file.c
	work/src/list.c
  work/src/memory.c
	work/src/symtab.c
 	work/src/port.c
	work/src/port.h
  work/src/mccode.h

  # files generated with flex and bison
 	work/src/lex.yy.c
  work/src/instrument.tab.h
  work/src/instrument.tab.c
)


## Build McFormat executable
add_executable(
	mcformat
	work/src/mcformat.c
)
## McFormat needs to be linked against m
target_link_libraries(mcformat m)



## Add install targets

# Macro for installing library files into lib/${FLAVOR},
# while skipping unneeded files
macro(install_lib path)
  install (
    DIRECTORY "${path}"
    DESTINATION "lib/${FLAVOR}"
    PATTERN "Makefile*" EXCLUDE  # skip makefiles
    PATTERN "#*"        EXCLUDE  # skip backup files
    PATTERN ".*"        EXCLUDE  # skip hidden files
    PATTERN "*.out"     EXCLUDE  # skip binary files
    )
endmacro()

# Shared library, lib
install_lib("${PROJECT_BINARY_DIR}/work/lib/")
install_lib("${PROJECT_SOURCE_DIR}/${FLAVOR_LIB}/")

# Man pages
install (
  DIRECTORY "${PROJECT_BINARY_DIR}/work/doc/man/"
  DESTINATION man/man1
  PATTERN "*.1"
)

# Binaries
install (
  PROGRAMS "${PROJECT_BINARY_DIR}/mcstas" "${PROJECT_BINARY_DIR}/mcformat"
  DESTINATION bin
)
