cmake_minimum_required(VERSION 2.8.12)

# This will install McStas components
project(mcstas-comps C Fortran)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("mcstas")
set(WORK "${PROJECT_BINARY_DIR}/work")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-comps-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, libnexus-dev")

# NSIS
set(CPACK_NSIS_PACKAGE_NAME "${MCCODE_STRING} Components")
set(CPACK_NSIS_DISPLAY_NAME "${MCCODE_STRING} Components")

include(CPack)

add_executable(
  cif2hkl
  ${CMAKE_CURRENT_SOURCE_DIR}/libs/cif2hkl/cif2hkl.F90
)
target_compile_options(cif2hkl PUBLIC "-ffree-line-length-512")

if (NOT WINDOWS)
  add_library(neutronics "STATIC" libs/neutronics/neutronics-subs.f)
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/cif2hkl${DOT_EXE_SUFFIX}"
    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
  )
endif()

# Include mcstas-comp files
foreach(name contrib data examples misc monitors obsolete optics samples share sources union sasmodels editors libs)
  installLib("${PROJECT_SOURCE_DIR}/${name}")
endforeach()

# Include mcstas-comp revision tag file
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/revision.in" "${WORK}/revision" @ONLY)

# copy some descriptory files
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/NOMENCLATURE" "${CMAKE_CURRENT_SOURCE_DIR}/README.md" DESTINATION "${WORK}")

if(WINDOWS)
  install(
    FILES "${WORK}/revision"
    DESTINATION "${lib}"
  )
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/cif2hkl${DOT_EXE_SUFFIX}"
    DESTINATION ${bin}
    )
  add_library(neutronics "STATIC" libs/neutronics/neutronics-subs.f)
else()
  install(
    FILES "${WORK}/revision" "${WORK}/NOMENCLATURE" "${WORK}/README.md"
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}"
  )
endif()

if (NOT WINDOWS)
  install(TARGETS neutronics DESTINATION "${FLAVOR}/${MCCODE_VERSION}/libs/neutronics/")
else()
  install(TARGETS neutronics DESTINATION "${lib}/libs/neutronics/")
endif()
