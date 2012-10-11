cmake_minimum_required(VERSION 2.6)

## This file will build McStas
project(mcstas)


## We are building with CMake and need to let port.h know this
add_definitions(
  -DCMake
)


## McStas version
set(MAJOR "1")
set(MINOR "12b")
set(VERSION "${MAJOR}.${MINOR}")


## CPack configuration
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_PACKAGE_VERSION_MAJOR "${MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${MINOR}")
set(CPACK_PACKAGE_NAME          "mcstas")
set(CPACK_PACKAGE_VERSION       "${VERSION}")
set(CPACK_PACKAGE_DEPENDS       "")

set(CPACK_PACKAGE_CONTACT "jsbn@fysik.dtu.dk") #required

include(CPack)


## Add global definitions
add_definitions(
	-DMCCODE_NAME=\"McStas\"
	-DMCCODE_TARNAME=\"mcstas\"
	-DMCCODE_VERSION=\"${VERSION}\"
	-DMCCODE_STRING=\"McStas\ ${MAJOR}.${MINOR}\"
	-DMCCODE_BUGREPORT=\"www.mcstas.org\"
	-DMCCODE_URL=\"\"

	# -DCC_HAS_PROTOS=1
	# -DSTDC_HEADERS=1
	# -DHAVE_THREADS=\"-DUSE_THREADS\ \$$OPENMP_CFLAGS\ \"
)

set(MCCODE_STRING   "McStas Jul-07-2011 - Jul. 07, 2011")
set(MCCODE_DATE     "Jul. 07, 2011")
set(MCCODE_VERSION  "Jul-07-2011")
set(MCCODE_NAME     "McStas")
set(MCCODE_PARTICLE "neutron")
set(MCCODE_LIBENV   "MCSTAS")



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
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
message("-- Looking for bison and flex")
find_package(BISON)
find_package(FLEX)
message("-- Looking for bison and flex - found")


# OPTIONAL
check_function_exists(strcasecmp  HAVE_STRCASECMP)
check_function_exists(fdopen      HAVE_FDOPEN)
check_function_exists(qsort       HAVE_QSORT)




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
  "config.h.in"
  "src/config.h"
)

configure_file (
  "lib/share/mccode-r.h.in"
  "lib/share/mccode-r.h"
)

configure_directory("src/*" "src")


# Generate man pages
message("-- Preparing man files")
file(MAKE_DIRECTORY "doc/man")
configure_directory("doc/man/*" "doc/man")


## Include source directories for building
include_directories(
  "${PROJECT_BINARY_DIR}/src"        # rewritten files from output dir
  "${PROJECT_BINARY_DIR}/lib/share"  # rewritten mccode-r.h
  "${PROJECT_SOURCE_DIR}/lib/share"  # source lib
  "${PROJECT_SOURCE_DIR}/nlib/share" # McStas lib
)


## Generate lex.yy.c with flex
add_custom_command(
  OUTPUT src/lex.yy.c
  COMMAND "${FLEX_EXECUTABLE}" -i "${PROJECT_SOURCE_DIR}/src/instrument.l"
  WORKING_DIRECTORY src
)


## Generate instrument.tab.{h,c} with bison
add_custom_command(
  OUTPUT src/instrument.tab.h src/instrument.tab.c
  COMMAND "${BISON_EXECUTABLE}" -v -d "${PROJECT_SOURCE_DIR}/src/instrument.y"
  WORKING_DIRECTORY src
)


## Build McStas executable
add_executable(
	mcstas
	src/cexp.c
	src/cogen.c
	src/coords.c
	src/debug.c
  src/file.c
	src/list.c
  src/mccode.h
  src/memory.c
 	src/port.c
	src/port.h
	src/symtab.c

  # files generated with flex and bison
 	src/lex.yy.c
  src/instrument.tab.h
  src/instrument.tab.c
)


## Build McFormat executable
#add_executable(
#	mcformat
#	src/mcformat.c
#)
## McFormat needs to be linked against m
#target_link_libraries(mcformat m)



## Add install targets

# Macro for installing library files into lib/mcstas,
# while skipping unneeded files
macro(install_lib path)
  install (
    DIRECTORY "${path}"
    DESTINATION lib/mcstas
    PATTERN "Makefile*" EXCLUDE  # skip makefiles
    PATTERN "#*"        EXCLUDE  # skip backup files
    PATTERN ".*"        EXCLUDE  # skip hidden files
    PATTERN "*.out"     EXCLUDE  # skip binary files
    )
endmacro()

# shared library, lib
install_lib("${PROJECT_SOURCE_DIR}/lib/")
install_lib("${PROJECT_SOURCE_DIR}/nlib/")

# Man pages
install (
  DIRECTORY "${PROJECT_BINARY_DIR}/doc/man/"
  DESTINATION man/man1
  PATTERN "*.1"
)
