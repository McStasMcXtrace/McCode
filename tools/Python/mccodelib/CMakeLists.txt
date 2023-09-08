cmake_minimum_required(VERSION 3.17.0)
cmake_policy(VERSION 3.17.0)

# This will install McStas components
project(python-tools-mccodelib C)

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
elseif(${BUILD_MCSTAS})
  set(FLAVOR "mcstas")
else()
  message(FATAL_ERROR "No flavor specified!\n"
    "Please use either -DBUILD_MCSTAS=1 or -DBUILD_MCXTRACE=1")
endif()

setupMCCODE("${FLAVOR}")

# Select prefix
if(${BUILD_MCSTAS})
  set(P "mc")
else()
  set(P "mx")
endif()

set(TOOLS_NAME "Python/mccodelib")

set(TOOLS_LIB "${DEST_TOOLDIR}/${TOOLS_NAME}")
set(MCCODE_LOCATION "${DEST_TOOLDIR}")


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

set(CPACK_PACKAGE_VERSION       "${MCCODE_VERSION}")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# NSIS
set(NSIS_NAME "Python internal library, ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NSIS_NAME}")

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

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/mccode_config.py.in" "${WORK}/mccode_config.py" @ONLY)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/mccode_config.json.in" "${WORK}/mccode_config.json" @ONLY)
set(BINS "${WORK}/mccode_config.py" "${WORK}/mccode_config.json")

if(WINDOWS)
  # Python files
  foreach(name __init__.py fcparticleparser.py utils.py flowchart.py instrgeom.py instrparser.py mcdisplayutils.py mcplotloader.py nodetree.py particleparser.py pipetools.py plotgraph.py pqtgfrontend.py cflags.py)
    install(
      FILES "${PROJECT_SOURCE_DIR}/${name}"
      DESTINATION "${DEST_TOOLDIR}/${TOOLS_NAME}"
      )
  endforeach()
  install(
    FILES ${BINS}
    DESTINATION "${DEST_TOOLDIR}/${TOOLS_NAME}"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )
endif()

if(NOT WINDOWS)
  # Python files
  foreach(name __init__.py fcparticleparser.py utils.py flowchart.py instrgeom.py instrparser.py mcdisplayutils.py mcplotloader.py nodetree.py particleparser.py pipetools.py plotgraph.py pqtgfrontend.py cflags.py)
    install(
      FILES "${PROJECT_SOURCE_DIR}/${name}"
      DESTINATION "${DEST_TOOLDIR}/${TOOLS_NAME}"
      )
  endforeach()
  install(
    FILES ${BINS}
    DESTINATION "${DEST_TOOLDIR}/${TOOLS_NAME}"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )

endif()
