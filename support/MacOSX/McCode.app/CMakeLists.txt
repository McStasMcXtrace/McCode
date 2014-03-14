cmake_minimum_required(VERSION 2.6)

# This will install the McStas app bundle
project(mcstas-app NONE)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)
setupMCCODE("mcstas")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-app-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../../../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

set(WORK "${PROJECT_BINARY_DIR}/work")

# Not too nice, hardcoded PREFIX
set(dest "/Applications/${NAME}-${MCCODE_VERSION}.app/")
include(CPack)

configure_file(Contents/Resources/launcher.sh.in ${WORK}/launcher.sh @ONLY)

# Include the Contents subfolder...
install (
  DIRECTORY "Contents"
  DESTINATION "${dest}"
  PATTERN "Makefile*" EXCLUDE  # skip makefiles
  PATTERN "#*"        EXCLUDE  # skip backup files
  PATTERN ".*"        EXCLUDE  # skip hidden files
  PATTERN "*.out"     EXCLUDE  # skip binary files
  )

install(
    FILES "${WORK}/launcher.sh"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ 
    DESTINATION "${dest}/Contents/Resources/"
  )

install(
    FILES "Contents/MacOS/droplet"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ 
    DESTINATION "${dest}/Contents/MacOS/"
  )
