cmake_minimum_required(VERSION 2.6)

## This file will build McStas
project(mcstas)
set(NAME            "McStas")
set(FLAVOR          "mcstas")
set(FLAVOR_FMT      "mcformat")
set(FLAVOR_LIB      "nlib")
set(MCCODE_PARTICLE "neutron")
set(MCCODE_LIBENV   "MCSTAS")
set(MCCODE_PROJECT  1)


## We are building with CMake and need to let port.h know this
add_definitions(
  -DCMake
)


## If this value is not empty, we have the following macros from mkdist:
## @MCCODE_DATE@, @MCCODE_STRING@, @MCCODE_NAME@, @MCCODE_TARNAME@, @MCCODE_VERSION@
string(REPLACE "@" "_" "@MCCODE_DATE@" MKDIST)

if(MKDIST STREQUAL "_MCCODE_DATE_")
  ## Set SVN-specific version
  set(MAJOR "2")
  set(MINOR "9999")
  set(MCCODE_VERSION "${MAJOR}.${MINOR}-svn")
  set(MCCODE_NAME "${NAME}")
  set(MCCODE_DATE "svn")
  set(MCCODE_STRING "${NAME} ${MCCODE_VERSION}, ${MCCODE_DATE}")
  set(MCCODE_TARNAME "${FLAVOR}")
else()
  ## Set mkdist-provided version
  set(MAJOR "2")  # TODO: Replace with information from ${MCCODE_VERSION}
  set(MINOR "0")
  set(MCCODE_VERSION "@MCCODE_VERSION@")
  set(MCCODE_NAME "@MCCODE_NAME@")
  set(MCCODE_DATE "@MCCODE_DATE@")
  set(MCCODE_STRING "@MCCODE_STRING@")
  set(MCCODE_TARNAME "@MCCODE_TARNAME@")
endif()


set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
include(InstallMCCODE)
InstallMCCODE()
