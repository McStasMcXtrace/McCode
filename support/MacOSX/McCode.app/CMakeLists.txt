cmake_minimum_required(VERSION 2.6)

# This will install the McCode app bundle
project(mcstas-app NONE)

# Choose between McStas or McXtrace
option(enable_mcstas   "Choose the McStas flavor" off)
option(enable_mcxtrace "Choose the McXtrace flavor (trumps enable_mcstas)" Off)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)

if (${enable_mcxtrace})
  set(FLAVOR "mcxtrace")
  set(GUIPREFIX "mx")
elseif(${enable_mcstas})
  set(FLAVOR "mcstas")
  set(GUIPREFIX "mc")
else()
  message(FATAL_ERROR "No flavor specified!\n"
    "Please use either -Denable_mcstas=1 or -Denable_mcxtrace=1")
endif()

message("Enabling ${FLAVOR} flavor")
setupMCCODE("${FLAVOR}")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-app-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../../../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")

# These two provide a method to change between running the .pl and .py launchers
configure_file(Contents/Resources/launcher-pl.sh.in ${WORK}/launcher-pl.sh @ONLY)
configure_file(Contents/Resources/launcher-py.sh.in ${WORK}/launcher-py.sh @ONLY)
# This one is the one actually run by the app "droplet"
configure_file(Contents/Resources/launcher-py.sh.in ${WORK}/launcher.sh @ONLY)

exec_program(chmod ARGS 755 work/launcher.sh)
exec_program(chmod ARGS 755 work/launcher-pl.sh)
exec_program(chmod ARGS 755 work/launcher-py.sh)

# Include the Contents subfolder...
install (
  DIRECTORY "Contents/"
  DESTINATION "Contents/"
  PATTERN "Makefile*" EXCLUDE  # skip makefiles
  PATTERN "#*"        EXCLUDE  # skip backup files
  PATTERN ".*"        EXCLUDE  # skip hidden files
  PATTERN "*.out"     EXCLUDE  # skip binary files
  )

install(
    FILES "${WORK}/launcher.sh" "${WORK}/launcher-py.sh" "${WORK}/launcher-pl.sh"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ 
    DESTINATION "Contents/Resources/"
  )

install(
    FILES "Contents/MacOS/droplet"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ 
    DESTINATION "Contents/MacOS/"
  )

install(
    FILES "Contents/Resources/${FLAVOR}.icns"
    DESTINATION "Contents/Resources/"
    RENAME "droplet.icns"
  )