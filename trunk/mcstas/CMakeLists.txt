cmake_minimum_required(VERSION 2.6)

# This file will build McStas
project(mcstas)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Setup McCode values (from mkdist or defaults)
include(MCUtil)
setup_mccode_mkdist("mcstas")

# Setup install target
include(InstallMCCODE)
InstallMCCODE()
