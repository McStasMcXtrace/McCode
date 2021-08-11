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
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION},gsl-bin,libgsl-dev")

# RPM
set(CPACK_RPM_PACKAGE_REQUIRES "${FLAVOR}-${MCCODE_VERSION}")
set(CPACK_RPM_PACKAGE_REQUIRES "gsl")
set(CPACK_RPM_PACKAGE_REQUIRES "gsl-devel")

# NSIS
set(CPACK_NSIS_PACKAGE_NAME "${MCCODE_STRING} Components")
set(CPACK_NSIS_DISPLAY_NAME "${MCCODE_STRING} Components")

include(CPack)

## Build mcpltool
add_executable(
  mcpltool
  libs/mcpl/tool.c
)
target_compile_options(mcpltool PUBLIC "-fPIC" "-std=c99")
target_link_libraries(mcpltool m)

## Build ssw2mcpl
add_executable(
  ssw2mcpl
  libs/mcpl/ssw2mcpl.c
)
target_compile_options(ssw2mcpl PUBLIC "-std=c99")
target_link_libraries(ssw2mcpl m)

## Build mcpl2ssw
add_executable(
  mcpl2ssw
  libs/mcpl/mcpl2ssw.c
)
target_compile_options(mcpl2ssw PUBLIC "-std=c99")
target_link_libraries(mcpl2ssw m)

add_executable(
  cif2hkl
  ${CMAKE_SOURCE_DIR}/libs/cif2hkl/cif2hkl.F90
)

if (NOT WINDOWS)
  add_library(mcpl "STATIC" libs/mcpl/mcpl.c)
  target_compile_options(mcpl PUBLIC "-fPIC" "-std=c99")
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/mcpltool${DOT_EXE_SUFFIX}"
    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
  )
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/ssw2mcpl${DOT_EXE_SUFFIX}"
    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
  )
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/mcpl2ssw${DOT_EXE_SUFFIX}"
    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
  )
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/cif2hkl${DOT_EXE_SUFFIX}"
    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
  )
endif()

# Install mcxtrace components
foreach(name contrib data examples misc monitors obsolete optics samples share sources editors libs)
  installLib("${PROJECT_SOURCE_DIR}/${name}")
endforeach()

# Include mcxtrace-comp revision tag file
configure_file("${CMAKE_SOURCE_DIR}/revision.in" "${WORK}/revision" @ONLY)

# copy some descriptory files
file(COPY "${CMAKE_SOURCE_DIR}/NOMENCLATURE" "${CMAKE_SOURCE_DIR}/README" DESTINATION "${WORK}")

if(WINDOWS)
  install(
    FILES "${WORK}/revision"
    DESTINATION "${lib}"
  )
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/mcpltool${DOT_EXE_SUFFIX}"
    DESTINATION ${bin}
    )
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/ssw2mcpl${DOT_EXE_SUFFIX}"
    DESTINATION ${bin}
    )
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/mcpl2ssw${DOT_EXE_SUFFIX}"
    DESTINATION ${bin}
    )
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/cif2hkl${DOT_EXE_SUFFIX}"
    DESTINATION ${bin}
    )
  add_library(mcpl "STATIC" libs/mcpl/mcpl.c)
  target_compile_options(mcpl PUBLIC "-std=c99") 
else()
  install(
    FILES "${WORK}/revision" "${WORK}/NOMENCLATURE" "${WORK}/README"
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}"
  )
endif()

if (NOT WINDOWS)
    install(TARGETS mcpl DESTINATION "${FLAVOR}/${MCCODE_VERSION}/libs/mcpl/")
else()
    install(TARGETS mcpl DESTINATION "${lib}/libs/mcpl/")
endif()
