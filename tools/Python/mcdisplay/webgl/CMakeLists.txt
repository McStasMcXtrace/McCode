cmake_minimum_required(VERSION 3.17.0)
cmake_policy(VERSION 3.17.0)

# This will install McStas components
project(python-tools-mcdisplay-webgl C)

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

set(TOOLS_NAME "tools/Python/${P}display/webgl")

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
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-python-${P}display-webgl-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "${MCCODE_VERSION}")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# NSIS
set(NSIS_NAME "Python/webgl -based ${P}display (${P}display-webgl-py), ${MCCODE_STRING}")
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
  "${FLAVOR}-${MCCODE_VERSION},  ${PYDEPS}"
)


include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/mcdisplay.in" "${WORK}/${P}display-webgl" @ONLY)

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/mcdisplay.in" "${WORK}/${P}display-webgl-py" @ONLY)

# Program files
if(WINDOWS)
  # Python files
  foreach(name mcdisplay.js Lut.js OrbitControls.js dat.gui.min.js jquery.min.js three.min.js template.html __init__.py)
    install(
      FILES "${PROJECT_SOURCE_DIR}/${name}"
      DESTINATION "${lib}/${TOOLS_NAME}"
      )
  endforeach()

  install(
    FILES "${PROJECT_SOURCE_DIR}/mcdisplay.py"
    DESTINATION "${lib}/${TOOLS_NAME}"
    RENAME "${P}display.py"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )
endif()


if(NOT WINDOWS)
  # Python files
  foreach(name mcdisplay.js Lut.js OrbitControls.js dat.gui.min.js jquery.min.js three.min.js template.html __init__.py)
    install(
      FILES "${PROJECT_SOURCE_DIR}/${name}"
      DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
      )
  endforeach()

  install(
    FILES ${PROJECT_SOURCE_DIR}/mcdisplay.py
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
    RENAME "${P}display.py"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )

  install(
    PROGRAMS "${WORK}/${P}display-webgl"
    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
    RENAME "${P}display-webgl${DOT_PYTHON_SUFFIX}"
    )
endif()
