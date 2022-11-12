cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(python-tools-mcdisplay C)

# Choose between McStas or McXtrace
option(enable_mcstas   "Choose the McStas flavor" off)
option(enable_mcxtrace "Choose the McXtrace flavor (trumps enable_mcstas)" Off)

# Set module path
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)

if (${enable_mcxtrace})
  set(FLAVOR "mcxtrace")
  set(CPACK_PACKAGE_HOMEPAGE_URL "http://mcxtrace.org/")
elseif(${enable_mcstas})
  set(FLAVOR "mcstas")
  set(CPACK_PACKAGE_HOMEPAGE_URL "http://mcstas.org/")
else()
  message(FATAL_ERROR "No flavor specified!\n"
    "Please use either -Denable_mcstas=1 or -Denable_mcxtrace=1")
endif()

message("Enabling ${FLAVOR} flavor")
setupMCCODE("${FLAVOR}")


# Select prefix
set(P "${MCCODE_PREFIX}")

set(TOOLS_NAME "tools/Python/${P}display/matplotlib")

# Windows needs special handling of this path...
if(WINDOWS)
  set(TOOLS_LIB "${lib}/${TOOLS_NAME}")
else()
  set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}")
endif()


# Set some extra options for mccode_config
set(CC "${CMAKE_C_COMPILER}")
set(CFLAGS "-g -O2 -lm ${CMAKE_C_FLAGS}")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-python-${P}display-matplotlib-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "${MCCODE_VERSION}")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# NSIS
set(NSIS_NAME "Python and matplotlib based mcdisplay (mcdisplay-matplotlib), ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NSIS_NAME}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, python, python-matplotlib")

# RPM
set(CPACK_RPM_PACKAGE_REQUIRES
  "${FLAVOR}-${MCCODE_VERSION}, python, python-matplotlib"
  )


include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")


# Configure fallback script
configure_file("${CMAKE_SOURCE_DIR}/mcdisplay.in" "${WORK}/${P}display" @ONLY)

# Configure internal naming and how we call mcrun/mxrun across systems
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/mcdisplay.py" "${WORK}/${P}display.py" @ONLY)

if(WINDOWS)
  set(BINS "${PROJECT_SOURCE_DIR}/mcdisplay.py")
else()
  set(BINS "${PROJECT_SOURCE_DIR}/${P}display.py" "${WORK}/${P}display")
endif()

# Program files
if(WINDOWS)
  # Python files
  foreach(name util.py)
    install(
      FILES "${PROJECT_SOURCE_DIR}/${name}"
      DESTINATION "${lib}/${TOOLS_NAME}"
      )
  endforeach()

  install(
    FILES ${BINS}
    DESTINATION "${lib}/${TOOLS_NAME}"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )
endif()


if(NOT WINDOWS)
  # Python files
  foreach(name util.py)
    install(
      FILES "${PROJECT_SOURCE_DIR}/${name}"
      DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
      )
  endforeach()

  install(
    FILES ${BINS}
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )

  install(
    PROGRAMS "${WORK}/${P}display"
    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
    RENAME "${P}display-matplotlib${DOT_PYTHON_SUFFIX}"
    )
endif()
