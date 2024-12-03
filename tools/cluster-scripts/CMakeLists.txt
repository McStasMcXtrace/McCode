cmake_minimum_required( VERSION 3.17.0 )

# This will install McStas or McXtrace mc/mxrun
project(perl-cluster-tools C)

# Choose between McStas or McXtrace
option(BUILD_MCSTAS   "Choose the McStas flavor" off)
option(BUILD_MCXTRACE "Choose the McXtrace flavor (trumps BUILD_MCSTAS)" Off)

# Set module path
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")

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

message("Enabling ${FLAVOR} flavor")
setupMCCODE("${FLAVOR}")

# Select prefix
set(P "${MCCODE_PREFIX}")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-clustertools")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

# Split incoming MCCODE_VERSION to list
string(REPLACE "." ";" MCCODE_VERSION_LIST ${MCCODE_VERSION})

## Package versioning
list(GET MCCODE_VERSION_LIST 0 MAJOR)
list(GET MCCODE_VERSION_LIST 1 MINOR)
list(GET MCCODE_VERSION_LIST 2 PATCH)

set(CPACK_PACKAGE_VERSION       "${MAJOR}.${MINOR}.${PATCH}")
set(CPACK_PACKAGE_VERSION_MAJOR "${MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${PATCH}")

# NSIS
set(NSIS_NAME "Cluster scripting tools (slurm, pbs), ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NSIS_NAME}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}, perl, libfile-which-perl")
set(CPACK_DEBIAN_PACKAGE_REPLACES "${FLAVOR}-clustertools-3.5.1")

# RPM
set(CPACK_RPM_PACKAGE_REQUIRES
  "${FLAVOR}-${MCCODE_VERSION}, perl"
  )


include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")

# Configure scripts
configure_file("${CMAKE_SOURCE_DIR}/mcsub_pbs.in" "${WORK}/${FLAVOR}_mcsub_pbs" @ONLY)
configure_file("${CMAKE_SOURCE_DIR}/mcsub_slurm.in" "${WORK}/${FLAVOR}_mcsub_slurm" @ONLY)

if(WINDOWS)
  foreach(NAME "mcsub_pbs" "mcsub_slurm")
    install(
      FILES "${WORK}/${NAME}"
      DESTINATION "${bin}/${NAME}.pl"
      )
  endforeach()
else()
  foreach(NAME "${FLAVOR}_mcsub_pbs" "${FLAVOR}_mcsub_slurm")
    install(
      FILES "${WORK}/${NAME}"
      DESTINATION "${DEST_BINDIR}"
      )
  endforeach()
endif()
