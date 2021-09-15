cmake_minimum_required(VERSION 2.6)

# This will install McXtrace components
project(mcxtrace-comps C Fortran)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("mcxtrace")
set(WORK "${PROJECT_BINARY_DIR}/work")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-comps-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "erkn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}")

# NSIS
set(CPACK_NSIS_PACKAGE_NAME "${MCCODE_STRING} Components")
set(CPACK_NSIS_DISPLAY_NAME "${MCCODE_STRING} Components")

include(CPack)

## Build mcpltool
add_executable(
  mcpltool
  libs/mcpl/tool.c
  libs/mcpl/mcpl.c
)
target_compile_options(mcpltool PUBLIC "-fPIC" "-std=c99")
target_link_libraries(mcpltool m)




# Install mcxtrace components
foreach(name contrib data examples misc monitors obsolete optics samples share sources editors)
  installLib("${PROJECT_SOURCE_DIR}/${name}")
endforeach()

# Include mcxtrace-comp revision tag file
configure_file("${CMAKE_SOURCE_DIR}/revision.in" "${WORK}/revision" @ONLY)

# copy some descriptory files
file(COPY "${CMAKE_SOURCE_DIR}/NOMENCLATURE" "${CMAKE_SOURCE_DIR}/README.md" DESTINATION "${WORK}")

if(WINDOWS)
  install(
    FILES "${WORK}/revision"
    DESTINATION "${lib}"
  )
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/mcpltool${DOT_EXE_SUFFIX}"
    DESTINATION ${bin}
    )
  add_library(mcpl "STATIC" libs/mcpl/mcpl.c)
  target_compile_options(mcpl PUBLIC "-std=c99")
else()
  install(
    FILES "${WORK}/revision" "${WORK}/NOMENCLATURE" "${WORK}/README.md"
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}"
  )
endif()

if (NOT WINDOWS)
  install(TARGETS mcpl DESTINATION "${FLAVOR}/${MCCODE_VERSION}/libs/mcpl/")
else()
  install(TARGETS mcpl DESTINATION "${lib}/libs/mcpl/")
endif()
