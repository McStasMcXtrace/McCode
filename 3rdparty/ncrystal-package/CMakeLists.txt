cmake_minimum_required(VERSION 3.16)

# This will install NCrystal components
project(NCrystal VERSION 3.4.1 LANGUAGES CXX C)

message(STATUS "Configure ${CMAKE_PROJECT_NAME} ${FLAVOR} ${PROJECT_NAME}")
set(BUILD_EXAMPLES OFF)

# To be built with/for McStas
set(BUILD_MCSTAS   "Choose the McStas flavor" on)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
# Set McCode values (from mkdist or defaults)
include(MCUtil)

set(FLAVOR "mcstas")
setupMCCODE("${FLAVOR}")

set(CPACK_PACKAGE_NAME "${FLAVOR}-NCrystal")
set(CPACK_PACKAGE_CONTACT       "ncrystal-developers@cern.ch")
set(CPACK_PACKAGE_VERSION       "3.4.1")
set(CPACK_PACKAGE_VERSION_MAJOR "3.4")
set(CPACK_PACKAGE_VERSION_MINOR "1")
set(CPACK_NSIS_PACKAGE_NAME "NCrystal 3.4.1")
set(CPACK_NSIS_DISPLAY_NAME "NCrystal 3.4.1")

# NSIS
set(NSIS_NAME "${CPACK_PACKAGE_NAME}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NSIS_NAME}")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")

add_subdirectory(ncrystal)

