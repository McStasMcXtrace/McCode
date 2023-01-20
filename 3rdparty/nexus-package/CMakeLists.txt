cmake_minimum_required(VERSION 3.17)

# This will install NeXus components
project(NeXus_wrapper)

message(STATUS "Configure ${CMAKE_PROJECT_NAME} ${FLAVOR} ${PROJECT_NAME}")

# Choose between McStas or McXtrace
option(BUILD_MCSTAS   "Choose the McStas flavor" off)
option(BUILD_MCXTRACE "Choose the McXtrace flavor (trumps BUILD_MCSTAS)" Off)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
# Set McCode values (from mkdist or defaults)
include(MCUtil)

if (${BUILD_MCXTRACE})
  set(FLAVOR "mcxtrace")
elseif(${BUILD_MCSTAS})
  set(FLAVOR "mcstas")
else()
  message(FATAL_ERROR "No flavor specified!\n"
    "Please use either -DBUILD_MCSTAS=1 or -DBUILD_MCXTRACE=1")
endif()
setupMCCODE("${FLAVOR}")

set(CPACK_PACKAGE_NAME "${FLAVOR}-nexus-${MCCODE_VERSION}")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")
set(CPACK_PACKAGE_VERSION       "0.0.1")
set(CPACK_PACKAGE_VERSION_MAJOR "0.0")
set(CPACK_PACKAGE_VERSION_MINOR "1")
set(CPACK_NSIS_PACKAGE_NAME "NeXus 0.0.1")
set(CPACK_NSIS_DISPLAY_NAME "NeXus 0.0.1")

# NSIS
set(NSIS_NAME "${CPACK_PACKAGE_NAME}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NSIS_NAME}")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")

add_subdirectory(nexus-code)

