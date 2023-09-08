cmake_minimum_required(VERSION 2.6)

# This will install McStas components
set(FLAVOR mcstas)
project(${FLAVOR}_manuals C)

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
set(CPACK_PACKAGE_NAME          "${FLAVOR}-manuals-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

# NSIS
set(NSIS_NAME "Manuals, ${MCCODE_VERSION}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")

set(MANUAL "${FLAVOR}-manual.pdf")
set(CMANUAL "${FLAVOR}-components.pdf")

set(MCCODE_LIB "${DEST_DATADIR_COMPS}")
set(MANDEST "${DEST_DATADIR_DOC}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}")

include(CPack)

install(FILES "${PROJECT_SOURCE_DIR}/manual.pdf"
  DESTINATION "${MANDEST}"
  RENAME "${MANUAL}"
)

install(FILES "${PROJECT_SOURCE_DIR}/Component_manual.pdf"
  DESTINATION "${MANDEST}"
  RENAME "${CMANUAL}"
)
