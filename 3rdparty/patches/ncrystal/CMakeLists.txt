##################################################################################
#                                                                                #
# CMake file which can be used to compile and link all files distributed with    #
# NCrystal. As an alternative, users can simply copy the source files into an    #
# existing setup, and build them using whatever system they use to build the     #
# rest of their code.                                          .                 #
#                                                                                #
# One way to invoke cmake to build and install would be like this:               #
#                                                                                #
#  $> cmake /path/to/sourcedir -DCMAKE_INSTALL_PREFIX=/path/to/installdir        #
#                                                                                #
# Followed by:                                                                   #
#                                                                                #
#  $> make install                                                               #
#                                                                                #
# Written 2016-2020 by T. Kittelmann.                                            #
#                                                                                #
##################################################################################

#Need CMake 3.1.0 to enable C++11
cmake_minimum_required(VERSION 3.1.0)

project(NCrystal VERSION 2.1.1 LANGUAGES CXX C)

# Set module path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
include(MCUtil)
setupMCCODE("mcstas")
set(WORK "${PROJECT_BINARY_DIR}/work")

set(CPACK_PACKAGE_NAME          "${FLAVOR}-ncrystal-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

include(CPack)

set(BUILD_EXAMPLES OFF CACHE BOOL "Whether to build examples.")
set(BUILD_G4HOOKS  OFF CACHE BOOL "Whether to build the G4 hooks if Geant4 is available.")
set(BUILD_EXTRA    ON CACHE BOOL "Whether to build optional modules for .nxs/.laz/.lau support (nb: different license!).")
set(INSTALL_MCSTAS ON CACHE BOOL "Whether to install the NCrystal mcstas component and related scripts.")
set(INSTALL_PY     ON CACHE BOOL "Whether to install the NCrystal python module and various python scripts.")
set(INSTALL_DATA   ON CACHE BOOL "Whether to install the shipped data files (always .ncmat files, .nxs files when BUILD_EXTRA=ON).")
set(EMBED_DATA     OFF CACHE BOOL "Whether to embed the shipped .ncmat files directly into the NCrystal library (forces INSTALL_DATA=OFF).")

if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

#Try to respect user/compilers choice of C++ standard, but needs at least C++11:
if (DEFINED CMAKE_CXX_STANDARD AND CMAKE_CXX_STANDARD EQUAL 98)
  message(FATAL_ERROR "CMAKE_CXX_STANDARD set to 98 which is unsupported since NCrystal release 2.0.0.")
endif()
if (NOT DEFINED CMAKE_CXX_EXTENSIONS)
  set(CMAKE_CXX_EXTENSIONS OFF)#no unofficial extensions
endif()
if (NOT DEFINED CMAKE_CXX_STANDARD)
  set(TMP_TESTDIR ${CMAKE_CURRENT_BINARY_DIR}/test_cxx11)
  file(WRITE ${TMP_TESTDIR}/test.cpp "int main() { auto ll=[](){return 0;}; return ll(); }\n")
  try_compile(SETUP_HANDLES_CXX11_BY_DEFAULT "${TMP_TESTDIR}" "${TMP_TESTDIR}/test.cpp")
  if(NOT SETUP_HANDLES_CXX11_BY_DEFAULT)
    set(CMAKE_CXX_STANDARD 11)
    message(STATUS "Could not compile simple C++11 test program => Trying to proceed with CMAKE_CXX_STANDARD=11.")
  endif()
endif()

get_filename_component(NCLIBDIR "${CMAKE_INSTALL_PREFIX}/lib" ABSOLUTE)
set(CMAKE_INSTALL_RPATH "${NCLIBDIR}")
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

file(GLOB HDRS_NC "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_core/include/NCrystal/*.*")
file(GLOB HDRS_INTERNAL_NC "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_core/include/NCrystal/internal/*.*")
file(GLOB SRCS_NC "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_core/src/*.cc")
file(GLOB EXAMPLES_NC "${CMAKE_CURRENT_SOURCE_DIR}/examples/ncrystal_example_c*.c*")
file(GLOB DATAFILES_NCMAT "${CMAKE_CURRENT_SOURCE_DIR}/data/*.ncmat")
set(DATAFILES "${DATAFILES_NCMAT}")

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

#Embed data (needs to invoke python process to generate C++ code from .ncmat files)
if (EMBED_DATA)
  if (INSTALL_DATA)
    message(WARNING "EMBED_DATA and INSTALL_DATA flags were both enabled (will force INSTALL_DATA=OFF).")
    set(INSTALL_DATA OFF)
  endif()
  #NB: In principle we should find the python3 executable as ${Python3_EXECUTABLE} after the following code:
  #    However, that probably won't work well until CMake 3.15.
  #
  #find_package (Python3 COMPONENTS Interpreter)
  #if (NOT Python3_Interpreter_FOUND)
  #  message(FATAL_ERROR "Could not find a Python3 interpreter (required since the EMBED_DATA flag was enabled).")
  #endif()
  #
  #We just call "python3" and hope for the best:
  execute_process(COMMAND python3 "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_python/ncrystal_ncmat2cpp"
    "-n" "NCrystal::AutoGenNCMAT::registerStdNCMAT"
    "--regfctname" "NCrystal::internal::registerEmbeddedNCMAT(const char*,const char*)"
    "-o" "${CMAKE_CURRENT_BINARY_DIR}/autogen_ncmat_data.cc" ${DATAFILES_NCMAT} RESULT_VARIABLE status )
  if(status AND NOT status EQUAL 0)
    message(FATAL_ERROR "Failure while trying to invoke ncrystal_ncmat2cpp (needed since the EMBED_DATA flag was enabled).")
  endif()
 list(APPEND SRCS_NC "${CMAKE_CURRENT_BINARY_DIR}/autogen_ncmat_data.cc")
  message("-- Generated autogen_ncmat_data.cc with embedded NCMAT data (will be compiled into the NCrystal library).")
endif()

#NCrystal library and header files, including optional built-in modules if enabled:
add_library(NCrystal SHARED ${SRCS_NC})
target_include_directories(NCrystal PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_core/include")
target_include_directories(NCrystal PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_core/src")
if (EMBED_DATA)
  target_compile_definitions(NCrystal PRIVATE NCRYSTAL_STDCMAKECFG_EMBED_DATA_ON)
endif()

#Make sure we link in math functions correctly (typically the linker needs libm on unix, but nothing on Windows).
set(TMP_TESTLIBMSRC "#include <math.h>\nint main(int argc,char** argv) { (void)argv;double a=(exp)(argc+1.0); return (int)a; }\n")
set(TMP_TESTDIR ${CMAKE_CURRENT_BINARY_DIR}/test_libm)
file(WRITE ${TMP_TESTDIR}/test.c "${TMP_TESTLIBMSRC}")
try_compile(ALWAYS_HAS_MATH "${TMP_TESTDIR}" "${TMP_TESTDIR}/test.c")
if (NOT ALWAYS_HAS_MATH)
  set(TMP_TESTDIR ${CMAKE_CURRENT_BINARY_DIR}/test_libm2)
  file(WRITE ${TMP_TESTDIR}/test.c "${TMP_TESTLIBMSRC}")
  try_compile(MATH_NEEDS_LIBM "${TMP_TESTDIR}" "${TMP_TESTDIR}/test.c" LINK_LIBRARIES m)
  if (MATH_NEEDS_LIBM)
    target_link_libraries(NCrystal PRIVATE m)
  else()
    message(FATAL_ERROR "Could not figure out link flags needed to enable math functions")
  endif()
endif()

#Test if compiler supports -Wl,--disable-new-dtags. If it does, apply it
#(otherwise RPATH sections in binaries become RUNPATH instead, which can be
#overridden by users LD_LIBRARY_PATH (CMake>=3.14 is needed for LINK_OPTIONS on
#try_compile):
if(${CMAKE_VERSION} VERSION_GREATER "3.13.99")
  set(TMP_TESTDIR ${CMAKE_CURRENT_BINARY_DIR}/test_dtagflags)
  file(WRITE ${TMP_TESTDIR}/test.c "int main() { return 0; }\n")
  try_compile(LINKER_HAS_DTAGS "${TMP_TESTDIR}" "${TMP_TESTDIR}/test.c" LINK_OPTIONS "-Wl,--disable-new-dtags")
  if (LINKER_HAS_DTAGS)
    target_link_options(NCrystal PUBLIC "-Wl,--disable-new-dtags")
  endif()
endif()

if (INSTALL_DATA)
  target_compile_definitions(NCrystal PRIVATE "-DNCRYSTAL_DATADIR=${CMAKE_INSTALL_PREFIX}/data")
  install(FILES ${DATAFILES} DESTINATION data)
endif()
if (BUILD_EXTRA)
  target_compile_definitions(NCrystal PRIVATE "-DNCRYSTAL_ENABLE_NXSLAZ")
endif()
install(TARGETS NCrystal ${INSTDEST})
install(FILES ${HDRS_NC} DESTINATION include/NCrystal)
install(FILES ${HDRS_INTERNAL_NC} DESTINATION include/NCrystal/internal)

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
  if (BUILD_EXAMPLES)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/examples/NCrystal_example_mcstas.instr DESTINATION mcstas)
  endif()
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
  #Build G4NCrystal library:
  add_library(G4NCrystal SHARED ${SRCS_NCG4})
  target_include_directories(G4NCrystal PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/ncrystal_geant4/include")
  install(TARGETS G4NCrystal ${INSTDEST})
  install(FILES ${HDRS_NCG4} DESTINATION include/G4NCrystal)
  #Transfer G4 flags uncovered by find_package(Geant4) call:
  target_compile_definitions(G4NCrystal PUBLIC ${Geant4_DEFINITIONS})
  target_link_libraries(G4NCrystal NCrystal ${Geant4_LIBRARIES})
  target_include_directories(G4NCrystal SYSTEM PUBLIC ${Geant4_INCLUDE_DIRS})
  set(Geant4_CXX_FLAGS_aslist ${Geant4_CXX_FLAGS})
  separate_arguments(Geant4_CXX_FLAGS_aslist)
  target_compile_options(G4NCrystal PUBLIC ${Geant4_CXX_FLAGS_aslist})
  #examples if needed:
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

message("####################################################")
message("## NCrystal configuration summary:                ##")
message("##   NCrystal library and headers        : yes    ##")
if (INSTALL_PY)
  message("##   NCrystal python module and scripts  : yes    ##")
else()
  message("##   NCrystal python module and scripts  : no     ##")
endif()
if (Geant4_FOUND)
  message("##   G4NCrystal library and headers      : yes    ##")
else()
  message("##   G4NCrystal library and headers      : no     ##")
endif()
if (BUILD_EXAMPLES)
  message("##   Enable examples for C and C++       : yes    ##")
else()
  message("##   Enable examples for C and C++       : no     ##")
endif()
if (INSTALL_DATA)
  message("##   Install shipped data files          : yes    ##")
else()
  message("##   Install shipped data files          : no     ##")
endif()
if (EMBED_DATA)
  message("##   Embed shipped data files in library : yes    ##")
else()
  message("##   Embed shipped data files in library : no     ##")
endif()
if (BUILD_EXTRA)
  message("##   Enable .nxs/.laz/.lau support       : yes    ##")
else()
  message("##   Enable .nxs/.laz/.lau support       : no     ##")
endif()
message("####################################################")
