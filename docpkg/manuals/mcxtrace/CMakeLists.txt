cmake_minimum_required( VERSION 3.17.0 )

# This will install McXtrace manuals
set(FLAVOR mcxtrace)
project(${FLAVOR} C)

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")

message( "CMAKE_SOURCE_DIR=        ${CMAKE_SOURCE_DIR}" )
message( "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}" )
message( "CMAKE_CURRENT_LIST_DIR=  ${CMAKE_CURRENT_LIST_DIR}" )
message( "PROJECT_SOURCE_DIR=      ${PROJECT_SOURCE_DIR}" )
message( "PROJECT_BINARY_DIR=      ${PROJECT_BINARY_DIR}" )

message( "include MCUtil" )
include(MCUtil)
message( "setup MCCODE" )
setupMCCODE("${FLAVOR}")

# CPack configuration
message( "CPack configuration" )
set(CPACK_PACKAGE_NAME          "${FLAVOR}-manuals")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../COPYING")
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
set(NSIS_NAME "Manuals, ${MCCODE_VERSION}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")

set(MANUAL "${FLAVOR}-manual.pdf")
set(CMANUAL "${FLAVOR}-components.pdf")

set(MCCODE_LIB "${DEST_DATADIR_COMPS}")
set(MANDEST "${DEST_DATADIR_DOC}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}")
set(CPACK_DEBIAN_PACKAGE_REPLACES "${FLAVOR}-manuals-3.5.1")

include(CPack)

install(FILES "${PROJECT_SOURCE_DIR}/manual.pdf"
  DESTINATION "${MANDEST}"
  RENAME "${MANUAL}"
)

install(FILES "${PROJECT_SOURCE_DIR}/Component_manual.pdf"
  DESTINATION "${MANDEST}"
  RENAME "${CMANUAL}"
)
