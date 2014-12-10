cmake_minimum_required(VERSION 2.6)

# This will install McStas or McXtrace mc/mxrun
project(python-tools-mcrun C)

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


set(TOOLS_NAME "${FLAVOR}-tools-python-${P}run-${MCCODE_VERSION}")

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
set(NSIS_NAME "Python-based mcrun (mcrun-py), ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, python-yaml")

# RPM
set(CPACK_RPM_PACKAGE_REQUIRES
  "${FLAVOR}-${MCCODE_VERSION}, python-yaml"
  )


include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")

# Program files
if(WINDOWS)
  set(BINS "${PROJECT_SOURCE_DIR}/mcrun.py")
  set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${lib}/${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}")
else()
  set(BINS "${PROJECT_SOURCE_DIR}/mcrun.py" "${WORK}/mcrun")
  set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}")
endif()

# Configure fallback script
configure_file("${CMAKE_SOURCE_DIR}/mcrun.in" "${WORK}/mcrun" @ONLY)

# Configure Python files
configure_file(config.py "${WORK}/config.py"  @ONLY)

if(WINDOWS)
  install(
    FILES ${BINS}
    DESTINATION "${lib}/${TOOLS_NAME}"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )
  install(FILES "${WORK}/config.py" DESTINATION "${lib}/${TOOLS_NAME}")

  foreach(NAME "__init__.py" "log.py" "mcstas.py" "optimisation.py")
    install(
      FILES "${PROJECT_SOURCE_DIR}/${NAME}"
      DESTINATION "${lib}/${TOOLS_NAME}"
      )
  endforeach()
endif()

if(NOT WINDOWS)
  install(FILES "${WORK}/config.py" DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}")

  foreach(NAME "__init__.py" "log.py" "mcstas.py" "optimisation.py")
    install(
      FILES "${PROJECT_SOURCE_DIR}/${NAME}"
      DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
      )
  endforeach()

  Install(
    FILES ${BINS}
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )
  # Symlink to main
  add_custom_command(
    OUTPUT "${WORK}/${P}run-py"
    COMMAND "${CMAKE_COMMAND}" -E remove "${WORK}/${P}run-py"
    COMMAND "${CMAKE_COMMAND}" -E create_symlink
    "${TOOLS_LIB}/mcrun" "${WORK}/${P}run-py"
    )

  add_custom_target(
    "CREATE_SYMLINK" ALL DEPENDS "${WORK}/${P}run-py"
    )

  install(
    PROGRAMS "${WORK}/${P}run-py"
    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
    RENAME "${P}run-py${DOT_PYTHON_SUFFIX}"
    )
endif()
