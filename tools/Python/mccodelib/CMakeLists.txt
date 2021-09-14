cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(python-tools-mccodelib C)

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
if(${enable_mcstas})
  set(P "mc")
else()
  set(P "mx")
endif()

set(TOOLS_NAME "tools/Python/mccodelib")

if(WINDOWS)
  set(TOOLS_LIB "${lib}/${TOOLS_NAME}")
  set(MCCODE_LOCATION "${lib}/")
else()
  set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}")
  set(MCCODE_LOCATION "${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/")
endif()


# Set some extra options for mccode_config
set(CC "${CMAKE_C_COMPILER}")
if (APPLE)
  set(CFLAGS "${MCCODE_CFLAGS} -headerpad_max_install_names ${CMAKE_C_FLAGS}")
else()
  set(CFLAGS "${MCCODE_CFLAGS} ${CMAKE_C_FLAGS}")
endif()

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-python-mccodelib-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/COPYING")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# NSIS
set(NSIS_NAME "Python internal library, ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, python3, python3-ply")

# RPM
if (RPMPROFILE)
   if (RPMPROFILE STREQUAL "fedora")
        set(PYDEPS "python3, python3-ply")
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


# Configure config file
configure_file("${CMAKE_SOURCE_DIR}/mccode_config.py.in" "${WORK}/mccode_config.py" @ONLY)
set(BINS "${WORK}/mccode_config.py")

if(WINDOWS)
  # Python files
  foreach(name __init__.py fcparticleparser.py utils.py flowchart.py instrgeom.py instrparser.py mcdisplayutils.py mcplotloader.py nodetree.py particleparser.py pipetools.py test.py plotgraph.py pqtgfrontend.py)
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
  foreach(name __init__.py fcparticleparser.py utils.py flowchart.py instrgeom.py instrparser.py mcdisplayutils.py mcplotloader.py nodetree.py particleparser.py pipetools.py test.py plotgraph.py pqtgfrontend.py)
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

endif()
