cmake_minimum_required(VERSION 2.6)

## This will install McStas components
project(mcstas-comps)
set(FLAVOR "mcstas")

## We are building with CMake and need to let port.h know this
add_definitions(
  -DCMake
)


## If this value is not empty, we have the following macros from mkdist:
## @MCCODE_DATE@, @MCCODE_STRING@, @MCCODE_NAME@, @MCCODE_TARNAME@, @MCCODE_VERSION@
set(MKDIST "@MCCODE_DATE@")

if(MKDIST STREQUAL "@MCCODE_DATE@")
  ## Set SVN-specific version
  set(MAJOR "2")
  set(MINOR "9999")
  set(MCCODE_VERSION "${MAJOR}.${MINOR}-svn")
  set(MCCODE_NAME "${NAME}")
  set(MCCODE_DATE "svn")
  set(MCCODE_STRING "${NAME} ${MCCODE_VERSION}, ${MCCODE_DATE}")
  set(MCCODE_TARNAME "${FLAVOR}")
else()
  ## Set mkdist-provided version
  set(MAJOR "2")  # TODO: Replace with information from ${MCCODE_VERSION}
  set(MINOR "0")
  set(MCCODE_VERSION "@MCCODE_VERSION@")
  set(MCCODE_NAME "@MCCODE_NAME@")
  set(MCCODE_DATE "@MCCODE_DATE@")
  set(MCCODE_STRING "@MCCODE_STRING@")
  set(MCCODE_TARNAME "@MCCODE_TARNAME@")
endif()


## CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-comps-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_VERSION_MAJOR "${MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${MINOR}")
set(CPACK_PACKAGE_VERSION       "${MCCODE_VERSION}")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")


# Make CPack respect the install prefix
SET(CPACK_SET_DESTDIR "ON")
SET(CPACK_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}")

include(CPack)



set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
include(MCUtil)

foreach(name contrib data examples misc monitors obsolete optics samples share sources)
  install_lib("${PROJECT_SOURCE_DIR}/${name}")
endforeach()
