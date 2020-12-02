cmake_minimum_required(VERSION 3.16)

project(mcstas-ncrystal)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/../../cmake/Modules")
include(MCUtil)
setupMCCODE("mcstas")
set(WORK "${PROJECT_BINARY_DIR}/work")

option(BUILD_EXAMPLES OFF)
option(BUILD_G4HOOKS  OFF)

if (NOT COMMAND FetchContent_Declare)
  include(FetchContent)
endif()

FetchContent_Declare(
  NCrystal
  GIT_REPOSITORY https://github.com/mctools/ncrystal
  GIT_TAG        master
  GIT_SHALLOW    ON
)

FetchContent_MakeAvailable(NCrystal)

set(CPACK_PACKAGE_NAME          "${FLAVOR}-ncrystal-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FILE_LICENSE "${NCrystal_SOURCE_DIR}/LICENSE")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

include(CPack)
