cmake_minimum_required(VERSION 3.18.0)
cmake_policy(VERSION 3.18.0)

# This file will build McStas and/or McXtrace
project(mccode C)

option(BUILD_MCSTAS "Build McStas" OFF)
option(BUILD_MCXTRACE "Build McXtrace" OFF)

if (${BUILD_MCSTAS} AND ${BUILD_MCXTRACE})
    message(WARNING "Configuration requests building both McStas and McXtrace which is likely to fail.")
elseif(NOT (${BUILD_MCSTAS} OR ${BUILD_MCXTRACE}))
    message(FATAL_ERROR "Configuration requires building one of McStas or McXtrace via -DBUILD_MCXTRACE=ON or -DBUILD_MCSTAS=ON")
endif()

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Setup McCode values (from mkdist or defaults)
include(MCUtil)

if(${BUILD_MCSTAS})
    setupMCCODE("mcstas")
    message(STATUS "Configuring McStas build ${FLAVOR}")
    add_subdirectory(mcstas)
    add_subdirectory(mcstas-comps)
    add_subdirectory(tools)
endif()

if (${BUILD_MCXTRACE})
    setupMCCODE("mcxtrace")
    message(STATUS "Configuring McXtrace build ${FLAVOR}")
    add_subdirectory(mcxtrace)
    add_subdirectory(mcxtrace-comps)
endif()