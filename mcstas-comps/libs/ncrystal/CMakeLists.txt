##################################################################################
#                                                                                #
# Example CMake file which can be used to compile and link all files distributed #
# with NCrystal. Obviously, in many cases, users will simply copy the source     #
# files into their existing setup, and build them using whatever system they use #
# to build the rest of their code.                                          .    #
#                                                                                #
# One way to invoke cmake to build and install would be like this:               #
#                                                                                #
#  $> cmake /path/to/sourcedir -DCMAKE_INSTALL_PREFIX=/path/to/installdir        #
#                                                                                #
# Followed by:                                                                   #
#                                                                                #
#  $> make install                                                               #
#                                                                                #
# Written 2016-2017 by T. Kittelmann.                                            #
#                                                                                #
##################################################################################

cmake_minimum_required(VERSION 3.0.0)

project(NCrystal CXX C)

set(BUILD_EXAMPLES OFF CACHE STRING "Whether to build examples.")
set(BUILD_G4HOOKS  OFF CACHE STRING "Whether to build the G4 hooks if Geant4 is available.")
set(BUILD_EXTRA    ON CACHE STRING "Whether to build optional modules for .nxs/.laz/.lau support (nb: different license!).")
set(INSTALL_MCSTAS ON CACHE STRING "Whether to install the NCrystal mcstas component and related scripts.")
set(INSTALL_PY     ON CACHE STRING "Whether to install the NCrystal python module and related scripts.")
set(INSTALL_DATA   ON CACHE STRING "Whether to install the shipped data files (always .ncmat files, .nxs files when BUILD_EXTRA=ON).")

if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

get_filename_component(NCLIBDIR "${CMAKE_INSTALL_PREFIX}/lib" ABSOLUTE)
set(CMAKE_INSTALL_RPATH "${NCLIBDIR}")
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

file(GLOB HDRS_NC "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_core/include/NCrystal/*.*")
file(GLOB SRCS_NC "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_core/src/*.cc")
file(GLOB EXAMPLES_NC "${CMAKE_CURRENT_SOURCE_DIR}/examples/ncrystal_example_c*.c*")
file(GLOB DATAFILES "${CMAKE_CURRENT_SOURCE_DIR}/data/*.ncmat")

file(GLOB SRCS_NCPY "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_python/*.py")
file(GLOB SRCS_PYSCRIPTS "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_python/ncrystal_*")

file(GLOB HDRS_NCG4 "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_geant4/include/G4NCrystal/*.*")
file(GLOB SRCS_NCG4 "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_geant4/src/*.cc")
file(GLOB EXAMPLES_NCG4 "${CMAKE_CURRENT_SOURCE_DIR}/examples/ncrystal_example_g4*.cc")

set(INSTDEST "RUNTIME;DESTINATION;bin;LIBRARY;DESTINATION;lib;ARCHIVE;DESTINATION;lib")

#optional source- and data-files:
if (BUILD_EXTRA)
  file(GLOB SRCS_EXTRA "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_extra/*/*.cc")
  list(APPEND SRCS_NC "${SRCS_EXTRA}")
  file(GLOB DATAFILES_NXS "${CMAKE_CURRENT_SOURCE_DIR}/data/*.nxs")
  list(APPEND DATAFILES "${DATAFILES_NXS}")
endif()

#NCrystal library and header files, including optional build-in modules if enabled:
add_library(NCrystal SHARED ${SRCS_NC})
target_include_directories(NCrystal PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_core/include")
target_include_directories(NCrystal PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_core/src")
target_link_libraries(NCrystal m)
if (INSTALL_DATA)
  target_compile_definitions(NCrystal PRIVATE "-DNCRYSTAL_DATADIR=${CMAKE_INSTALL_PREFIX}/data")
  install(FILES ${DATAFILES} DESTINATION data)
endif()
if (BUILD_EXTRA)
  target_compile_definitions(NCrystal PRIVATE "-DNCRYSTAL_ENABLE_NXSLAZ")
endif()
install(TARGETS NCrystal ${INSTDEST})
install(FILES ${HDRS_NC} DESTINATION include/NCrystal)

#Examples:
if (BUILD_EXAMPLES AND EXAMPLES_NC)
  foreach(ex ${EXAMPLES_NC})
    get_filename_component(exbn "${ex}" NAME_WE)
    add_executable(${exbn} "${ex}")
    target_link_libraries(${exbn} NCrystal)
    install(TARGETS ${exbn} ${INSTDEST})
  endforeach()
endif()

#python interface
if (INSTALL_PY)
  find_package(PythonInterp)
  if (NOT PYTHONINTERP_FOUND)
    message(WARNING "INSTALL_PY set to ON but failed to find python interpreter.")
  endif()
else()
  set(INSTALL_PY NO)
endif()

if (INSTALL_PY)
  install(FILES ${SRCS_NCPY} DESTINATION python/NCrystal)
  install(PROGRAMS ${SRCS_PYSCRIPTS} DESTINATION bin)
  if (BUILD_EXAMPLES)
    install(PROGRAMS "${CMAKE_CURRENT_SOURCE_DIR}/examples/ncrystal_example_py" DESTINATION bin)
  endif()
endif()

if (INSTALL_MCSTAS)
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_mcstas/NCrystal_sample.comp DESTINATION mcstas)
  install(PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_mcstas/ncrystal_preparemcstasdir DESTINATION bin)
endif()

#G4NCrystal
if (BUILD_G4HOOKS)
  find_package(Geant4)
  if(NOT Geant4_FOUND)
    message(WARNING "BUILD_G4HOOKS set to ON but failed to enable Geant4 support.")
  endif()
else()
  set(Geant4_FOUND NO)
endif()

if (Geant4_FOUND)
  add_library(G4NCrystal SHARED ${SRCS_NCG4})
  target_include_directories(G4NCrystal PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_geant4/include")
  target_link_libraries(G4NCrystal NCrystal ${Geant4_LIBRARIES})
  target_include_directories(G4NCrystal SYSTEM PUBLIC ${Geant4_INCLUDE_DIRS})
  #Recent G4 versions all requires C++11 (we enable it like this for simplicity,
  #since there is not yet a standard way to enable C++11 with CMake - until CMake 3.1):
  target_compile_options(G4NCrystal PUBLIC "-std=c++11")
  install(TARGETS G4NCrystal ${INSTDEST})
  install(FILES ${HDRS_NCG4} DESTINATION include/G4NCrystal)
  if (BUILD_EXAMPLES AND EXAMPLES_NCG4)
    foreach(ex ${EXAMPLES_NCG4})
      get_filename_component(exbn "${ex}" NAME_WE)
      add_executable(${exbn} "${ex}")
      target_link_libraries(${exbn} G4NCrystal)
      install(TARGETS ${exbn} ${INSTDEST})
    endforeach()
  endif()
endif()

#setup.sh and unsetup.sh files for installation directory:

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/ncrystal_setup.sh "#!/bin/bash\n\n#################################################\n# Source this file to use NCrystal installation #\n#################################################\n\n#First undo effect of previously sourcing a setup.sh file from this or another\n#NCrystal installation:\n\nfunction ncrystal_prunepath() {\n    P=\$(IFS=:;for p in \${!1}; do [[ \$p != \${2}* ]] && echo -n \":\$p\" || :; done)\n    export \$1=\${P:1:99999}\n}\n\nif [ ! -z \${NCRYSTALDIR:-} ]; then\n    ncrystal_prunepath PATH \"\$NCRYSTALDIR\"\n    ncrystal_prunepath LD_LIBRARY_PATH \"\$NCRYSTALDIR\"\n    ncrystal_prunepath DYLD_LIBRARY_PATH \"\$NCRYSTALDIR\"\n    ncrystal_prunepath PYTHONPATH \"\$NCRYSTALDIR\"\nfi\n\nunset ncrystal_prunepath\n\n#Then add this installation (we leave NCRYSTAL_LIB or NCRYSTAL_DATADIR unset\n#since they are not actually needed for a standard installation to work out of\n#the box - they merely exists as optional features):\nexport NCRYSTALDIR=\"\$( cd -P \"\$( dirname \"\${BASH_SOURCE[0]}\" )\" && pwd )\"\nexport PATH=\"\$NCRYSTALDIR/bin:\$PATH\"\nexport LD_LIBRARY_PATH=\"\$NCRYSTALDIR/lib:\$LD_LIBRARY_PATH\"\nexport DYLD_LIBRARY_PATH=\"\$NCRYSTALDIR/lib:\$DYLD_LIBRARY_PATH\"\nif [ -f \$NCRYSTALDIR/python/NCrystal/__init__.py ]; then\n    export PYTHONPATH=\"\$NCRYSTALDIR/python:\$PYTHONPATH\"\nfi\n")

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/ncrystal_unsetup.sh "#!/bin/bash\n\n##########################################################################\n# Source this file to undo effect of sourcing setup.sh in same directory #\n##########################################################################\n\nfunction ncrystal_prunepath() {\n    P=\$(IFS=:;for p in \${!1}; do [[ \$p != \${2}* ]] && echo -n \":\$p\" || :; done)\n    export \$1=\${P:1:99999}\n}\n\nNCRYSTAL_THISDIR=\"\$( cd -P \"\$( dirname \"\${BASH_SOURCE[0]}\" )\" && pwd )\"\n\nif [ ! -z {NCRYSTALDIR:-} -a \"x\$NCRYSTALDIR\" == \"x\$NCRYSTAL_THISDIR\" ]; then\n    ncrystal_prunepath PATH \"\$NCRYSTALDIR\"\n    ncrystal_prunepath LD_LIBRARY_PATH \"\$NCRYSTALDIR\"\n    ncrystal_prunepath DYLD_LIBRARY_PATH \"\$NCRYSTALDIR\"\n    ncrystal_prunepath PYTHONPATH \"\$NCRYSTALDIR\"\n    unset NCRYSTALDIR\nfi\n\nunset ncrystal_prunepath\nunset NCRYSTAL_THISDIR\n")

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/ncrystal_setup.sh DESTINATION . RENAME setup.sh)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/ncrystal_unsetup.sh DESTINATION . RENAME unsetup.sh)

message("###################################################")
message("## NCrystal configuration summary:               ##")
message("##   NCrystal library and headers       : yes    ##")
if (INSTALL_PY)
  message("##   NCrystal python module and scripts : yes    ##")
else()
  message("##   NCrystal python module and scripts : no     ##")
endif()
if (Geant4_FOUND)
  message("##   G4NCrystal library and headers     : yes    ##")
else()
  message("##   G4NCrystal library and headers     : no     ##")
endif()
if (BUILD_EXAMPLES)
  message("##   Enable examples for C and C++      : yes    ##")
else()
  message("##   Enable examples for C and C++      : no     ##")
endif()
if (INSTALL_DATA)
  message("##   Install shipped data files         : yes    ##")
else()
  message("##   Install shipped data files         : no     ##")
endif()
if (BUILD_EXTRA)
  message("##   Enable .nxs/.laz/.lau support      : yes    ##")
else()
  message("##   Enable .nxs/.laz/.lau support      : no     ##")
endif()
message("###################################################")
