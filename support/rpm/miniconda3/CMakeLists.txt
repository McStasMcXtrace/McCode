cmake_minimum_required(VERSION 3.0)

# This will install McStas components
project(miniconda3 C)

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

# Set McCode values (from mkdist or defaults)
set(WORK "${PROJECT_BINARY_DIR}/work")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-miniconda3-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

set(CPACK_RPM_PACKAGE_AUTOREQPROV " no")
set(CPACK_RPM_SPEC_INSTALL_POST "/bin/true")

include(CPack)

if(NOT EXISTS "${PROJECT_SOURCE_DIR}/miniconda3")
    execute_process(COMMAND ./prepare.sh
      WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
endif()

install(DIRECTORY "${PROJECT_SOURCE_DIR}/miniconda3"
  DESTINATION "${FLAVOR}/${MCCODE_VERSION}"
  USE_SOURCE_PERMISSIONS)

