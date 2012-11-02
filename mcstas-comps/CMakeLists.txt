cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(mcstas-comps)
set(FLAVOR "mcstas")


# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")


# Set McCode values (from mkdist or defaults)
include(MCUtil)
setup_mccode_mkdist()


# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-comps-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}")


# Make CPack respect the install prefix
SET(CPACK_SET_DESTDIR "ON")
SET(CPACK_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

include(CPack)


# Include mcstas-comp files
foreach(name contrib data examples misc monitors obsolete optics samples share sources)
  install_lib("${PROJECT_SOURCE_DIR}/${name}")
endforeach()
