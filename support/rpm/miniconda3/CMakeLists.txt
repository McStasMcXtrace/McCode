cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(miniconda3 C)

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

# Set McCode values (from mkdist or defaults)
set(WORK "${PROJECT_BINARY_DIR}/work")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-miniconda3-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

include(CPack)

if(NOT EXISTS "${PROJECT_SOURCE_DIR}/miniconda3")
  if(NOT EXISTS "${PROJECT_SOURCE_DIR}/miniconda3.tgz")
    execute_process(COMMAND wget http://support.mccode.org/rpm/miniconda3.tgz
      WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
  endif()
  execute_process(COMMAND tar xzf miniconda3.tgz
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
endif()

install(DIRECTORY "${PROJECT_SOURCE_DIR}/miniconda3"
  DESTINATION "${FLAVOR}/${MCCODE_VERSION}"
  USE_SOURCE_PERMISSIONS)

