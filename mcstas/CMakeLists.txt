cmake_minimum_required( VERSION 3.17.0 )

# This file will build McStas
project(mcstas C)
set(CMAKE_C_STANDARD 99)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Setup McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("mcstas")

# Setup install target
include(InstallMCCODE)
installMCCODE()

# Configure doc
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/mcstas.md.in" "${WORK}/mcstas.md" @ONLY)

install(
  FILES "${WORK}/mcstas.md"
  DESTINATION "${DEST_DATADIR_DOC}"
  )
