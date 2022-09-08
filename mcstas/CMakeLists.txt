cmake_minimum_required(VERSION 2.8.12)

# This file will build McStas
project(mcstas C)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Setup McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("mcstas")

# Setup install target
include(InstallMCCODE)
installMCCODE()
