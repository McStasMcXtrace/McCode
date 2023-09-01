cmake_minimum_required(VERSION 3.17.0)
cmake_policy(VERSION 3.17.0)

# This file will build McStas and/or McXtrace
project(mccode C)
set(CMAKE_C_STANDARD 99)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

#Ensure various variables are set to sensible default values for the target
#platform (if not already set by a toolchain file):

option( BUILD_MCSTAS "Build McStas" OFF )
option( BUILD_MCXTRACE "Build McXtrace" OFF )
option( BUILD_TOOLS "Include tools" ON )
option( BUILD_MCXRUN "Include mcrun/mxrun even if BUILD_TOOLS is OFF" )#fixme: replace with more fine-grained control
option( ENABLE_COMPONENTS "Whether or not to include comps and instruments" ON )
option( EXCLUDE_CONTRIB "Whether or not to exclude contrib comps" OFF )#fixme: replace with more fine-grained control
option( EXCLUDE_DATA "Whether or not to exclude data files" OFF )#fixme: replace with more fine-grained control

if (${BUILD_MCSTAS} AND ${BUILD_MCXTRACE})
    message(WARNING "Configuration requests building both McStas and McXtrace which is likely to fail.")
elseif(NOT (${BUILD_MCSTAS} OR ${BUILD_MCXTRACE}))
    message(FATAL_ERROR "Configuration requires building one of McStas or McXtrace via -DBUILD_MCXTRACE=ON or -DBUILD_MCSTAS=ON")
endif()

if ("${MCVERSION}" STREQUAL "")
    message(WARNING "Build version is unset. Define using e.g.  -DMCVERSION=3.9a")
endif()


# Setup McCode values (from mkdist or defaults)
include(MCUtil)

if(${BUILD_MCSTAS})
    setupMCCODE("mcstas")
    message(STATUS "Configuring McStas build ${FLAVOR}")
    add_subdirectory(mcstas)
    if ( ENABLE_COMPONENTS )
      add_subdirectory(mcstas-comps)
    endif()
endif()

if (${BUILD_MCXTRACE})
    setupMCCODE("mcxtrace")
    message(STATUS "Configuring McXtrace build ${FLAVOR}")
    add_subdirectory(mcxtrace)
    if ( ENABLE_COMPONENTS )
      add_subdirectory(mcxtrace-comps)
    endif()
endif()

if ( BUILD_TOOLS )
  add_subdirectory( tools )
elseif ( BUILD_MCXRUN )
  add_subdirectory( tools/Python/mcrun )
  add_subdirectory( tools/Python/mccodelib)
endif()

# Find/Fetch dependencies:

option( ENSURE_MCPL "Build Third Party code MCPL if not already available" ON )
if( ENSURE_MCPL )
  include( MCPL )
endif()

if ( BUILD_MCSTAS )
  option( ENSURE_NCRYSTAL "Build Third Party code NCrystal if not already available" ON )
  if ( ENSURE_NCRYSTAL )
    set ( NCRYSTAL_LEGACY_USE 1 )
  else()
    set ( NCRYSTAL_LEGACY_USE 0 )
  endif()
  if( ENSURE_NCRYSTAL )
    include( NCrystal )
  endif()
endif()

