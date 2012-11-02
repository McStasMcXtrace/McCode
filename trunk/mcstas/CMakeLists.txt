cmake_minimum_required(VERSION 2.6)

# This file will build McStas
project(mcstas)
set(NAME            "McStas")
set(FLAVOR          "mcstas")
set(FLAVOR_FMT      "mcformat")
set(FLAVOR_LIB      "nlib")
set(MCCODE_PARTICLE "neutron")
set(MCCODE_LIBENV   "MCSTAS")
set(MCCODE_PROJECT  1)


# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Setup McCode values (from mkdist or defaults)
include(MCUtil)
setup_mccode_mkdist()

# Setup install target
include(InstallMCCODE)
InstallMCCODE()
