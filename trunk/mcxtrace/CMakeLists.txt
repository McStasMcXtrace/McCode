cmake_minimum_required(VERSION 2.6)


## This file will build McXtrace
project(mcstas)
set(NAME            "McXtrace")
set(FLAVOR          "mcxtrace")
set(FLAVOR_FMT      "mxformat")
set(FLAVOR_LIB      "xlib")
set(MCCODE_PARTICLE "xray")
set(MCCODE_LIBENV   "MCXTRACE")
set(MCCODE_PROJECT  2)


# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Setup McCode values (from mkdist or defaults)
include(MCUtil)
setup_mccode_mkdist()

# Setup install target
include(InstallMCCODE)
InstallMCCODE()
