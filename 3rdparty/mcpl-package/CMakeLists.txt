cmake_minimum_required(VERSION 3.17)

# This will install MCPL components
project(MCPL VERSION 1.5.1 LANGUAGES CXX C)

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

set(CPACK_PACKAGE_NAME "${FLAVOR}-mcpl")
set(CPACK_PACKAGE_CONTACT       "mcpl-developers@cern.ch")
set(CPACK_PACKAGE_VERSION       "1.5.1")
set(CPACK_PACKAGE_VERSION_MAJOR "1.5")
set(CPACK_PACKAGE_VERSION_MINOR "1")
set(CPACK_NSIS_PACKAGE_NAME "MCPL 1.5.1")
set(CPACK_NSIS_DISPLAY_NAME "MCPL 1.5.1")

# NSIS
set(NSIS_NAME "${CPACK_PACKAGE_NAME}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NSIS_NAME}")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")

if(WINDOWS)
  # batch snippets for the Python tools
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/batches/mcpl-config.bat.in" "${WORK}/mcpl-config.bat" @ONLY)
  install(
    PROGRAMS "${WORK}/mcpl-config.bat"
    DESTINATION "${bin}"
    )

  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/batches/pymcpltool.bat.in" "${WORK}/pymcpltool.bat" @ONLY)
  install(
    PROGRAMS "${WORK}/pymcpltool.bat"
    DESTINATION "${bin}"
    )
endif()

add_subdirectory(mcpl)

