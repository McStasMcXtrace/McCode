cmake_minimum_required(VERSION 2.6)

# This will install the McCode app bundle
project(mcstas-openmpi NONE)

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

set(MCINSTALL_PREFIX "/usr/local/")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-openmpi-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../../../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")



# Include the Contents subfolder...
install (
  DIRECTORY "openmpi-1.6.5/"
  DESTINATION "."
)

foreach(name mpic++
mpic++-vt
mpicc
mpicc-vt
mpicxx
mpicxx-vt
mpiexec
mpif77
mpif77-vt
mpif90
mpif90-vt
mpirun
ompi-clean
ompi-iof
ompi-probe
ompi-profiler
ompi-ps
ompi-server
ompi-top
ompi_info
opal_wrapper
orte-bootproxy.sh
orte-clean
orte-iof
orte-ps
orte-top
orte_wrapper_script
ortec++
ortecc
orted
orterun
otfaux
otfcompress
otfconfig
otfdecompress
otfinfo
otfmerge
otfmerge-mpi
otfprint
otfprofile
otfprofile-mpi
otfshrink
vtc++
vtcc
vtcxx
vtf77
vtf90
vtfilter
vtfilter-mpi
vtfiltergen
vtfiltergen-mpi
vtfort
vtrun
vtsetup
vtsetup.jar
vtunify
vtunify-mpi
vtwrapper
)
  install(
    FILES "openmpi-1.6.5/bin/${name}"
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ 
    DESTINATION "bin"
  )
endforeach()

