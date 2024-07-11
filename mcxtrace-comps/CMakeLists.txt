cmake_minimum_required( VERSION 3.17.0 )

# This will install McXtrace components
project( mcxtrace-comps C )

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("mcxtrace")

set(WORK "${PROJECT_BINARY_DIR}/work")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-comps-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, libnexus-dev, libxrl-dev, libgsl-dev")

# NSIS
set(CPACK_NSIS_PACKAGE_NAME "${MCCODE_STRING} Components")
set(CPACK_NSIS_DISPLAY_NAME "${MCCODE_STRING} Components")

include(CPack)

option( ENABLE_CIF2HKL "Build Third Party code cif2hkl (fortran)" ON )#TODO: Only enable enable Fortran if this is on
if ( ENABLE_CIF2HKL )
  enable_language( Fortran )
  add_executable(
    cif2hkl
    ${CMAKE_CURRENT_SOURCE_DIR}/libs/cif2hkl/cif2hkl.F90
    )
  target_compile_options(cif2hkl PUBLIC "-ffree-line-length-512")
  install (
    PROGRAMS "${PROJECT_BINARY_DIR}/cif2hkl${DOT_EXE_SUFFIX}"
    DESTINATION "${DEST_BINDIR}"
    )
endif()

# System c-code
install( DIRECTORY "share/" DESTINATION "${DEST_DATADIR_CODEFILES}")

# Editor syntax-highlighting etc
if (NOT WINDOWS)
  install( DIRECTORY "editors/" DESTINATION "${DEST_DATADIR_EDITORS}" )
endif()

# System comps
install( DIRECTORY "misc/" DESTINATION "${DEST_DATADIR_COMPS}/misc")
install( DIRECTORY "monitors/" DESTINATION "${DEST_DATADIR_COMPS}/monitors")
install( DIRECTORY "obsolete/" DESTINATION "${DEST_DATADIR_COMPS}/obsolete")
install( DIRECTORY "optics/" DESTINATION "${DEST_DATADIR_COMPS}/optics")
install( DIRECTORY "samples/" DESTINATION "${DEST_DATADIR_COMPS}/samples")
install( DIRECTORY "sources/" DESTINATION "${DEST_DATADIR_COMPS}/sources")
install( DIRECTORY "union/" DESTINATION "${DEST_DATADIR_COMPS}/union")
install( DIRECTORY "sasmodels/" DESTINATION "${DEST_DATADIR_COMPS}/sasmodels")
install( DIRECTORY "astrox/" DESTINATION "${DEST_DATADIR_COMPS}/astrox")

# Contrib comps
if ( NOT EXCLUDE_CONTRIB )
  install( DIRECTORY "contrib/" DESTINATION "${DEST_DATADIR_COMPS}/contrib")
endif()

# Data files
if ( NOT EXCLUDE_DATA )
  install( DIRECTORY "data/" DESTINATION "${DEST_DATADIR_COMPS}/data")
endif()

install( DIRECTORY "examples/" DESTINATION "${DEST_DATADIR_EXAMPLES}")


# Include mcstas-comp revision tag file
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/revision.in" "${WORK}/revision" @ONLY)

# copy some descriptory files
configure_directory ("NOMENCLATURE" "work/nomenclature")
configure_directory ("README.md" "work/readme")
install( FILES "${WORK}/nomenclature/NOMENCLATURE" DESTINATION "${DEST_DATADIR_COMPS}")
install( FILES "${WORK}/readme/README.md" DESTINATION "${DEST_DATADIR_COMPS}")
install( FILES "${WORK}/revision" DESTINATION "${DEST_DATADIR_COMPS}" )
