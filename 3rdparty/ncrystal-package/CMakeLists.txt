cmake_minimum_required(VERSION 3.16)

# This will install NCrystal components
project(NCrystal VERSION 3.4.1 LANGUAGES CXX C)
set(CPACK_PACKAGE_NAME NCrystal)
set(CPACK_PACKAGE_CONTACT       "ncrystal-developers@cern.ch")
set(CPACK_PACKAGE_VERSION       "3.4.1")
set(CPACK_PACKAGE_VERSION_MAJOR "3.4")
set(CPACK_PACKAGE_VERSION_MINOR "1")
set(CPACK_NSIS_PACKAGE_NAME "NCrystal 3.4.1")
set(CPACK_NSIS_DISPLAY_NAME "NCrystal 3.4.1")
include(CPack)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)
set(WORK "${PROJECT_BINARY_DIR}/work")

set(BUILD_EXAMPLES OFF)

add_subdirectory(ncrystal)

