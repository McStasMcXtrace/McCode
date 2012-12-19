cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(python-tools-mcplot C)

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
setup_mccode_mkdist("${FLAVOR}")


set(TOOLS_NAME "${FLAVOR}-tools-python-${MCCODE_PREFIX}plot-chaco-${MCCODE_VERSION}")
if(WINDOWS)
  # Windows needs special handling of this path...
  set(TOOLS_LIB "${lib}/${TOOLS_NAME}")
else()
  set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${lib}/${TOOLS_NAME}")
endif()


# Set some extra options for mccode_config
set(CC "${CMAKE_C_COMPILER}")
set(CFLAGS "-g -O2 -lm ${CMAKE_C_FLAGS}")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${TOOLS_NAME}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../../COPYING")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# NSIS
set(NSIS_NAME "Python-based mcplot (mcplot-py), ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, python-chaco")


include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")


# Select prefix
if(${enable_mcstas})
  set(P "mc")
else()
  set(P "mx")
endif()


# Python files
foreach(name mcdata.py plotting.py)
  install(
    FILES "${PROJECT_SOURCE_DIR}/${name}"
    DESTINATION "${TOOLS_LIB}"
  )
endforeach()

# Program files
install(
  FILES "${PROJECT_SOURCE_DIR}/main.py"
  DESTINATION "${TOOLS_LIB}"
  PERMISSIONS
  OWNER_READ OWNER_WRITE OWNER_EXECUTE
  GROUP_READ GROUP_EXECUTE
  WORLD_READ WORLD_EXECUTE
)

# Symlink to main
add_custom_command(
  OUTPUT "${WORK}/${MCCODE_PREFIX}plot"
  COMMAND "${CMAKE_COMMAND}" -E make_directory "${WORK}"
  COMMAND "${CMAKE_COMMAND}" -E remove "${WORK}/${MCCODE_PREFIX}plot"
  COMMAND "${CMAKE_COMMAND}" -E create_symlink
    "${TOOLS_LIB}/main.py" "${WORK}/${MCCODE_PREFIX}plot"
)

add_custom_target(
  "CREATE_SYMLINK" ALL DEPENDS "${WORK}/${MCCODE_PREFIX}plot"
)

install(
  PROGRAMS "${WORK}/${MCCODE_PREFIX}plot"
  DESTINATION ${bin}
  RENAME "${MCCODE_PREFIX}plot-${MCCODE_VERSION}-py${DOT_PYTHON_SUFFIX}"
)
