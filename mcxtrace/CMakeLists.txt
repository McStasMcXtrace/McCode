cmake_minimum_required(VERSION 2.8.12)

## This file will build McXtrace
project(mcxtrace C)
set(CMAKE_C_STANDARD 99)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Setup McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("mcxtrace")

# Setup install target
include(InstallMCCODE)
installMCCODE()
