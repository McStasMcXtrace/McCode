cmake_minimum_required(VERSION 2.8.12)

## This file will build McXtrace
project(mcxtrace C)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Setup McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("mcxtrace")

# Setup install target
include(InstallMCCODE)
installMCCODE()
