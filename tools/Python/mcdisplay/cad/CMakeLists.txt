cmake_minimum_required(VERSION 3.17.0)
cmake_policy(VERSION 3.17.0)

# This will install McStas or McXtrace mc/display-cad
project(python-tools-mcdisplay-cad C)

message(STATUS "Configure ${CMAKE_PROJECT_NAME} ${FLAVOR} ${PROJECT_NAME}")

# Choose between McStas or McXtrace
option(BUILD_MCSTAS   "Choose the McStas flavor" off)
option(BUILD_MCXTRACE "Choose the McXtrace flavor (trumps BUILD_MCSTAS)" Off)

# Set module path
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)

if (${BUILD_MCXTRACE})
  set(FLAVOR "mcxtrace")
  set(CPACK_PACKAGE_HOMEPAGE_URL "http://mcxtrace.org/")
elseif(${BUILD_MCSTAS})
  set(FLAVOR "mcstas")
  set(CPACK_PACKAGE_HOMEPAGE_URL "http://mcstas.org/")
else()
  message(FATAL_ERROR "No flavor specified!\n"
    "Please use either -DBUILD_MCSTAS=1 or -DBUILD_MCXTRACE=1")
endif()

setupMCCODE("${FLAVOR}")

# Select prefix
set(P "${MCCODE_PREFIX}")

set(TOOLS_NAME "Python/${P}display/cad")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-python-${P}display-cad-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../../COPYING")
set(CPACK_PACKAGE_CONTACT       "gregory.tucker@ess.eu")

set(CPACK_PACKAGE_VERSION       "${MCCODE_VERSION}")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# NSIS
set(NSIS_NAME "Python/cad -based ${P}display (${P}display-cad-py), ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NSIS_NAME}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, python3, python3-ply, python3-numpy")

# RPM
if (RPMPROFILE)
   if (RPMPROFILE STREQUAL "fedora")
        set(PYDEPS "python3, python3-ply, python3-numpy")
   else()
	set(PYDEPS ${FLAVOR}-miniconda3-${MCCODE_VERSION})
   endif()
else()
	set(PYDEPS ${FLAVOR}-miniconda3-${MCCODE_VERSION})
endif()

set(CPACK_RPM_PACKAGE_REQUIRES
  "${FLAVOR}-${MCCODE_VERSION}, ${PYDEPS}"
)

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")

# Configure fallback script
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/mcdisplay.in" "${WORK}/${P}display-cad" @ONLY)

# Main script, including rename mc/mxdisplay
install(
  PROGRAMS "${PROJECT_SOURCE_DIR}/mcdisplay.py"
  DESTINATION "${DEST_TOOLDIR}/${TOOLS_NAME}"
  RENAME "${P}display.py"
  )

# Other .py infrastructure
# None

# Wrapper-scripts on windows are handled via
# cmake Modules/MCUtil
if(NOT WINDOWS)
  install(
    PROGRAMS "${WORK}/${P}display-cad"
    DESTINATION ${DEST_BINDIR}
    )
endif()
