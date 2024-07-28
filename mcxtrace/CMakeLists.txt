cmake_minimum_required( VERSION 3.17.0 )

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

# Configure doc
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/mcxtrace.md.in" "${WORK}/mcxtrace.md" @ONLY)

install(
  PROGRAMS "${WORK}/mcxtrace.md"
  DESTINATION "${DEST_TOOLDIR}/${TOOLS_NAME}"
  )
