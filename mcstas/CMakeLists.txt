cmake_minimum_required( VERSION 3.17.0 )

# This file will build McStas
project(mcstas C)
set(CMAKE_C_STANDARD 99)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Setup McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("${FLAVOR}")

# Configure doc
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/${FLAVOR}.md.in" "${WORK}/${FLAVOR}.md" @ONLY)

install(
  PROGRAMS "${WORK}/${FLAVOR}.md"
  DESTINATION "${DEST_TOOLDIR}/${TOOLS_NAME}"
  )


# Setup install target
include(InstallMCCODE)
installMCCODE()
