cmake_minimum_required(VERSION 2.6)

# This will install McStas or McXtrace mc/mxrun
project(perl-cluster-tools C)

# Choose between McStas or McXtrace
option(enable_mcstas   "Choose the McStas flavor" off)
option(enable_mcxtrace "Choose the McXtrace flavor (trumps enable_mcstas)" Off)

# Set module path
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)

if (${enable_mcxtrace})
  set(FLAVOR "mcxtrace")
elseif(${enable_mcstas})
  set(FLAVOR "mcstas")
else()
  message(FATAL_ERROR "No flavor specified!\n"
    "Please use either -Denable_mcstas=1 or -Denable_mcxtrace=1")
endif()

message("Enabling ${FLAVOR} flavor")
setupMCCODE("${FLAVOR}")

# Select prefix
set(P "${MCCODE_PREFIX}")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-clustertools-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# NSIS
set(NSIS_NAME "Cluster scripting tools, ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, perl, libfile-which-perl")

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
      DESTINATION "${FLAVOR}/${MCCODE_VERSION}/bin"
      )
  endforeach()
endif()
