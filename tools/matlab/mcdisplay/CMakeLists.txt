cmake_minimum_required( VERSION 3.17.0 )
cmake_policy(VERSION 3.17.0)

# This will install McStas components
project(matlab-tools-mcdisplay C)

# Choose between McStas or McXtrace
option(BUILD_MCSTAS   "Choose the McStas flavor" off)
option(BUILD_MCXTRACE "Choose the McXtrace flavor (trumps BUILD_MCSTAS)" Off)

# Set module path
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")

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

set(TOOLS_NAME "matlab/${P}display/")
set(TOOLS_LIB  "${DEST_TOOLDIR}/${TOOLS_NAME}")

# Set some extra options for mccode_config
set(CC "${CMAKE_C_COMPILER}")
set(CFLAGS "-g -O2 -lm ${CMAKE_C_FLAGS}")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-matlab-${P}display")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

# Split incoming MCCODE_VERSION to list
string(REPLACE "." ";" MCCODE_VERSION_LIST ${MCCODE_VERSION})

## Package versioning
list(GET MCCODE_VERSION_LIST 0 MAJOR)
list(GET MCCODE_VERSION_LIST 1 MINOR)
list(GET MCCODE_VERSION_LIST 2 PATCH)

set(CPACK_PACKAGE_VERSION       "${MAJOR}.${MINOR}.${PATCH}")
set(CPACK_PACKAGE_VERSION_MAJOR "${MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${PATCH}")

# NSIS
set(NSIS_NAME "Matlab/octave-based mcdisplay (mcdisplay-matlab), ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NSIS_NAME}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}")
set(CPACK_DEBIAN_PACKAGE_REPLACES "${FLAVOR}-tools-matlab-${P}display-3.5.1")

# RPM
set(CPACK_RPM_PACKAGE_REQUIRES
  "${FLAVOR}-${MCCODE_VERSION}"
  )


include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")


# Configure fallback script
configure_file("${CMAKE_SOURCE_DIR}/mcdisplay.in" "${WORK}/${P}display" @ONLY)

# Configure doc
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/mcdisplay-matlab.md.in" "${WORK}/${P}display-matlab.md" @ONLY)

if(WINDOWS)
  set(BINS "${PROJECT_SOURCE_DIR}/mcdisplay.m")
else()
  set(BINS "${WORK}/${P}display")
  set(BINS "${PROJECT_SOURCE_DIR}/mcdisplay.m")
endif()


if(WINDOWS)
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
  install(
    FILES ${BINS}
    DESTINATION "${DEST_TOOLDIR}/${TOOLS_NAME}"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )

  # Symlink to main
  add_custom_command(
    OUTPUT "${WORK}/${P}display"
    COMMAND "${CMAKE_COMMAND}" -E make_directory "${WORK}"
    COMMAND "${CMAKE_COMMAND}" -E remove "${WORK}/${P}display-matlab"
    COMMAND "${CMAKE_COMMAND}" -E create_symlink
    "${TOOLS_LIB}/mcdisplay" "${WORK}/${P}display"
    )

  add_custom_target(
    "CREATE_SYMLINK" ALL DEPENDS "${WORK}/${P}display"
    )

  install(
    PROGRAMS "${WORK}/${P}display"
    DESTINATION ${DEST_BINDIR}
    RENAME "${P}display-matlab"
    )
    
  install(
    FILES "${WORK}/${P}display-matlab.md"
    DESTINATION "${DEST_DATADIR_DOC}"
  )

endif()
