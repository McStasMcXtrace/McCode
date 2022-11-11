cmake_minimum_required(VERSION 3.16)

# This will install MCPL components
project(MCPL VERSION 1.5.1 LANGUAGES CXX C)
set(CPACK_PACKAGE_NAME NCrystal)
set(CPACK_PACKAGE_CONTACT       "mcpl-developers@cern.ch")
set(CPACK_PACKAGE_VERSION       "1.5.1")
set(CPACK_PACKAGE_VERSION_MAJOR "1.5")
set(CPACK_PACKAGE_VERSION_MINOR "1")
set(CPACK_NSIS_PACKAGE_NAME "MCPL 1.5.1")
set(CPACK_NSIS_DISPLAY_NAME "MCPL 1.5.1")
include(CPack)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)
set(WORK "${PROJECT_BINARY_DIR}/work")

add_subdirectory(mcpl)

