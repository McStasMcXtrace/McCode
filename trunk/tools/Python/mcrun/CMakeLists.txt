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
setup_mccode_mkdist("${FLAVOR}")

set(TOOLS_NAME "${FLAVOR}-tools-python-${MCCODE_PREFIX}run-${MCCODE_VERSION}")
set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${lib}/${TOOLS_NAME}")


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





# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, python-yaml")


include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")


# Select prefix
if(${enable_mcstas})
  set(P "mc")
else()
  set(P "mx")
endif()


# Configure Python files
configure_file(config.py "${WORK}/config.py"  @ONLY)

install(FILES "${WORK}/config.py" DESTINATION "${lib}/${TOOLS_NAME}")

foreach(NAME "__init__.py" "log.py" "mcstas.py" "optimisation.py")
    install(
      FILES "${PROJECT_SOURCE_DIR}/${NAME}"
      DESTINATION "${lib}/${TOOLS_NAME}"
      )
endforeach()

# Program files
install(
  FILES "${PROJECT_SOURCE_DIR}/main.py"
  DESTINATION "${lib}/${TOOLS_NAME}"
  PERMISSIONS
  OWNER_READ OWNER_WRITE OWNER_EXECUTE
  GROUP_READ GROUP_EXECUTE
  WORLD_READ WORLD_EXECUTE
)

# Symlink to main
add_custom_command(
  OUTPUT "${WORK}/${MCCODE_PREFIX}run"
  COMMAND "${CMAKE_COMMAND}" -E remove "${WORK}/${MCCODE_PREFIX}run"
  COMMAND "${CMAKE_COMMAND}" -E create_symlink
    "${TOOLS_LIB}/main.py" "${WORK}/${MCCODE_PREFIX}run"
)

add_custom_target(
  "CREATE_SYMLINK" ALL DEPENDS "${WORK}/${MCCODE_PREFIX}run"
)

install(
  PROGRAMS "${WORK}/${MCCODE_PREFIX}run"
  DESTINATION ${bin}
  RENAME "${MCCODE_PREFIX}run${PROGRAM_SUFFIX}-py${DOT_PYTHON_SUFFIX}"
)
