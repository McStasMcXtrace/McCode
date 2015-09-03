cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(mcstas-comps C)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("mcstas")


# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-comps-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}")

# NSIS
set(CPACK_NSIS_PACKAGE_NAME "${MCCODE_STRING} Components")
set(CPACK_NSIS_DISPLAY_NAME "${MCCODE_STRING} Components")


include(CPack)

add_library(nxs "STATIC" libs/libnxs/nxs.c libs/libnxs/sgclib.c libs/libnxs/sgfind.c libs/libnxs/sghkl.c libs/libnxs/sgio.c libs/libnxs/sgsi.c)

# Include mcstas-comp files
foreach(name contrib data examples misc monitors obsolete optics samples share sources editors libs)
  installLib("${PROJECT_SOURCE_DIR}/${name}")
endforeach()

if(WINDOWS)
  install(TARGETS nxs DESTINATION "${lib}/libs/libnxs/")
else()
  install(TARGETS nxs DESTINATION "${MCCODE_LIB}/libs/libnxs/")
endif()

