cmake_minimum_required(VERSION 2.6)

# This will install the McCode app bundle
project(mcstas-gcc NONE)

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
set(CPACK_PACKAGE_NAME          "${FLAVOR}-gcc-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../../../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")



# Include the Contents subfolder...
install (
  DIRECTORY "gcc-4.7/"
  DESTINATION "."
)

foreach(name cpp g++ gcc gcc-ar gcc-nm gcc-ranlib gcov gfortran)
  install(
    FILES "gcc-4.7/bin/${name}"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ 
    DESTINATION "bin"
  )
endforeach()

foreach(name cc1 cc1plus collect2 f951 lto-wrapper)
  install(
    FILES "gcc-4.7/libexec/gcc/x86_64-apple-darwin11.4.0/4.7.1/${name}"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ 
    DESTINATION "libexec/gcc/x86_64-apple-darwin11.4.0/4.7.1/"
  )
endforeach()
