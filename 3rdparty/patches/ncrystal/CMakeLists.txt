################################################################################
##                                                                            ##
##  This file is part of NCrystal (see https://mctools.github.io/ncrystal/)   ##
##                                                                            ##
##  Copyright 2015-2022 NCrystal developers                                   ##
##                                                                            ##
##  Licensed under the Apache License, Version 2.0 (the "License");           ##
##  you may not use this file except in compliance with the License.          ##
##  You may obtain a copy of the License at                                   ##
##                                                                            ##
##      http://www.apache.org/licenses/LICENSE-2.0                            ##
##                                                                            ##
##  Unless required by applicable law or agreed to in writing, software       ##
##  distributed under the License is distributed on an "AS IS" BASIS,         ##
##  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  ##
##  See the License for the specific language governing permissions and       ##
##  limitations under the License.                                            ##
##                                                                            ##
################################################################################

##################################################################################
#                                                                                #
# CMake file which can be used to compile and link all files distributed with    #
# NCrystal, and which provides CMake configuration files and setup.sh/unsetup.sh #
# for subsequent usage.                                                          #
#                                                                                #
# One way to invoke cmake to build and install would be like this (run this from #
# a temporary build dir)                                                         #
#                                                                                #
#  $> cmake /path/to/sourcedir -DCMAKE_INSTALL_PREFIX=/path/to/installdir        #
#                                                                                #
# Followed by (replace the number 8 by the number of processes you want to       #
# use for the compilation):                                                      #
#                                                                                #
#  $> make install -j8                                                           #
#                                                                                #
# Written 2016-2022 by T. Kittelmann.                                            #
#                                                                                #
##################################################################################

# We require cmake 3.10. This is intended to strike a balance between features
# and availability. Of popular platforms, a lower number would only have helped
# NCrystal usage on a few slightly older distributions such as Ubuntu 16.04 (has
# 3.5.1), Debian oldstable (has 3.7.2 as of Nov2020). CentOS6 and CentOS7 have
# CMake 2.8.12 which is clearly below any sensible threshold, so users on these
# platforms can already be expected to be used to install custom versions of
# tools like CMake (for instance CentOS7 provides cmake3 as a separate package,
# providing CMake 3.11). In any case, on platforms lacking CMake 3.10, one
# must install a newer cmake somehow (this is usually rather simple). See also:
# https://cliutils.gitlab.io/modern-cmake/chapters/intro/installing.html
#
# The maximum value is the maximum value with which we have tested. The reason
# for specifying this maximum value is that it affects the default values of
# cmake policies, depending on which version introduced them.

cmake_minimum_required(VERSION 3.10...3.19)

# Respect value of CMAKE_BUILD_TYPE if already defined, otherwise fall back to
# Release. In any case, expose CMAKE_BUILD_TYPE as an explicit cache variable
# (gives drop-down list in gui). This must come before the call to project(..).
if( NOT NCRYSTAL_NOTOUCH_CMAKE_BUILD_TYPE )
  if(DEFINED CMAKE_BUILD_TYPE)
    set(_def_cbt ${CMAKE_BUILD_TYPE})
  else()
    set(_def_cbt Release)
  endif()
  set(CMAKE_BUILD_TYPE ${_def_cbt}  CACHE STRING "Choose the type of build, options are: Debug Release RelWithDebInfo and MinSizeRel." )
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release RelWithDebInfo MinSizeRel None )
endif()

#Setup project:
project(NCrystal VERSION 3.4.1 LANGUAGES CXX C)
set(CPACK_PACKAGE_NAME NCrystal)
set(CPACK_PACKAGE_CONTACT       "ncrystal-developers@cern.ch")
set(CPACK_PACKAGE_VERSION       "3.4.1")
set(CPACK_PACKAGE_VERSION_MAJOR "3.4")
set(CPACK_PACKAGE_VERSION_MINOR "1")
set(CPACK_NSIS_PACKAGE_NAME "NCrystal 3.4.1")
set(CPACK_NSIS_DISPLAY_NAME "NCrystal 3.4.1")
include(CPack)

if( NOT NCRYSTAL_NOTOUCH_CMAKE_BUILD_TYPE )
  if ("${CMAKE_BUILD_TYPE}" STREQUAL "")
    #This can happen if parent project called the project(..) function before
    #doing the song and dance we did above.
    set(CMAKE_BUILD_TYPE Release)
  endif()
endif()

#Options:
option( BUILD_EXAMPLES  "Whether to build examples." ON )
option( BUILD_G4HOOKS   "Whether to build the G4 hooks (requires Geant4 to be available)." OFF )
option( BUILD_EXTRA     "Obsolete option. For .nxs support use -DBUILTIN_PLUGIN_LIST=mctools:nxslib (not needed for .laz/.lau support)." OFF )
set( BUILD_STRICT OFF CACHE STRING "Stricter build (primarily for testing). Can optionally select specific C++ standard.")
set_property(CACHE BUILD_STRICT PROPERTY STRINGS ON OFF 11 14 17 20 )
option( INSTALL_MCSTAS  "Whether to install the NCrystal mcstas component and related scripts." ON )
option( INSTALL_PY      "Whether to install the NCrystal python module and various python scripts (including ncrystal-config)." ON )
option( INSTALL_DATA    "Whether to install the shipped data files." ON )
option( INSTALL_SETUPSH "Whether to install setup.sh/unsetup.sh which users can source in order to use installaton." ON )
option( EMBED_DATA      "Whether to embed the shipped .ncmat files directly into the NCrystal library (forces INSTALL_DATA=OFF)." OFF )
option( MODIFY_RPATH    "Whether to try to set RPATH in installed binaries (if disabled all special RPATH handling is skipped)." ON )
option( DISABLE_DYNLOAD "Disable dynamic library loading capabilities (incl. plugins)." OFF )
option( NO_DIRECT_PYMODINST "Prevents Python module installation and creates setup.py-based skeleton in <blddir>/ncrystal_pypkg (w/o _nclibpath.py file)." OFF )

set(BUILTIN_PLUGIN_LIST "" CACHE STRING
    "Semicolon separated list of external NCrystal plugins to statically build into the NCrystal library (local paths to sources or git <repo_url:tag>)" )

if ( NO_DIRECT_PYMODINST )
  if ( NOT INSTALL_PY )
    message(FATAL_ERROR "The NO_DIRECT_PYMODINST flag requires -DINSTALL_PY=ON.")
  endif()
  if ( INSTALL_SETUPSH )
    message(FATAL_ERROR "The NO_DIRECT_PYMODINST flag requires -DINSTALL_SETUPSH=OFF.")
  endif()
endif()

#Installation directories (try to follow standard conventions):
include(GNUInstallDirs)
set(NCrystal_BINDIR "${CMAKE_INSTALL_BINDIR}")#e.g. <prefix>/bin>
set(NCrystal_LIBDIR "${CMAKE_INSTALL_LIBDIR}")#e.g. <prefix>/lib>
set(NCrystal_INCDIR "${CMAKE_INSTALL_INCLUDEDIR}")#e.g. <prefix>/include>
set(NCrystal_DATAROOT "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}")#e.g. <prefix>/share/NCrystal>
if( NOT NCrystal_DATAFILESDIR )
  set( NCrystal_DATAFILESDIR "${NCrystal_DATAROOT}/data")#e.g. <prefix>/share/NCrystal/data>
endif()
if( NOT NCrystal_PYPATH )
  set(NCrystal_PYPATH "${NCrystal_DATAROOT}/python")#e.g. <prefix>/share/NCrystal/python
endif()
if ( NOT NCrystal_CMAKEDIR )
  set(NCrystal_CMAKEDIR "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")#e.g. <prefix>/lib/cmake/NCrystal>
endif()
if ( NOT NO_DIRECT_PYMODINST )
  set(NCrystal_PYMODDIR "${NCrystal_PYPATH}/NCrystal")#e.g. <prefix>/share/NCrystal/python/NCrystal
endif()

#Get a few relative paths, mostly for expansion in various installed files (we
#use PROJECT_BINARY_DIR as prefix here, but it should not matter which as long
#as it is an absolute path):
if ( NOT NO_DIRECT_PYMODINST )
  file(RELATIVE_PATH NCrystal_relpath_PYMODDIR2LIBDIR "${PROJECT_BINARY_DIR}/${NCrystal_PYMODDIR}" "${PROJECT_BINARY_DIR}/${NCrystal_LIBDIR}")
  file(RELATIVE_PATH NCrystal_relpath_CMAKEDIR2PYPATH   "${PROJECT_BINARY_DIR}/${NCrystal_CMAKEDIR}" "${PROJECT_BINARY_DIR}/${NCrystal_PYPATH}")
  file(RELATIVE_PATH NCrystal_relpath_BINDIR2PYPATH "${PROJECT_BINARY_DIR}/${NCrystal_BINDIR}"   "${PROJECT_BINARY_DIR}/${NCrystal_PYPATH}")
endif()
file(RELATIVE_PATH NCrystal_relpath_BINDIR2LIBDIR   "${PROJECT_BINARY_DIR}/${NCrystal_BINDIR}"   "${PROJECT_BINARY_DIR}/${NCrystal_LIBDIR}")
file(RELATIVE_PATH NCrystal_relpath_BINDIR2DATAROOT "${PROJECT_BINARY_DIR}/${NCrystal_BINDIR}"   "${PROJECT_BINARY_DIR}/${NCrystal_DATAROOT}")
file(RELATIVE_PATH NCrystal_relpath_BINDIR2CMAKEDIR "${PROJECT_BINARY_DIR}/${NCrystal_BINDIR}"   "${PROJECT_BINARY_DIR}/${NCrystal_CMAKEDIR}")
file(RELATIVE_PATH NCrystal_relpath_BINDIR2INCDIR   "${PROJECT_BINARY_DIR}/${NCrystal_BINDIR}"   "${PROJECT_BINARY_DIR}/${NCrystal_INCDIR}")
file(RELATIVE_PATH NCrystal_relpath_BINDIR2ROOT     "${PROJECT_BINARY_DIR}/${NCrystal_BINDIR}"   "${PROJECT_BINARY_DIR}/")
file(RELATIVE_PATH NCrystal_relpath_CMAKEDIR2ROOT   "${PROJECT_BINARY_DIR}/${NCrystal_CMAKEDIR}" "${PROJECT_BINARY_DIR}/")
file(RELATIVE_PATH NCrystal_relpath_CMAKEDIR2BINDIR   "${PROJECT_BINARY_DIR}/${NCrystal_CMAKEDIR}" "${PROJECT_BINARY_DIR}/${NCrystal_BINDIR}")
file(RELATIVE_PATH NCrystal_relpath_CMAKEDIR2LIBDIR   "${PROJECT_BINARY_DIR}/${NCrystal_CMAKEDIR}" "${PROJECT_BINARY_DIR}/${NCrystal_LIBDIR}")
file(RELATIVE_PATH NCrystal_relpath_CMAKEDIR2INCDIR   "${PROJECT_BINARY_DIR}/${NCrystal_CMAKEDIR}" "${PROJECT_BINARY_DIR}/${NCrystal_INCDIR}")
file(RELATIVE_PATH NCrystal_relpath_CMAKEDIR2DATAFILESDIR   "${PROJECT_BINARY_DIR}/${NCrystal_CMAKEDIR}" "${PROJECT_BINARY_DIR}/${NCrystal_DATAFILESDIR}")

if ( BUILD_EXTRA )
  message(FATAL_ERROR "The BUILD_EXTRA flag is obsolete. Laz/Lau support is now builtin to the core NCrystal code and is always available. Support for .nxs files is on the other hand moved to a separate repository for license reasons. If you need support for .nxs files, you should add mctools:nxslib to the BUILTIN_PLUGIN_LIST (e.g. -DBUILTIN_PLUGIN_LIST=mctools:nxslib)")
endif()

#Dummy interface target for common properties. Note that the interface is always
#built with C++11 compatible compiler (which CMake will choose from a wide
#variety depending on platform and other targets,
#i.e. C++11/C++14/C++17/gnu++11/...). The BUILD_STRICT option only affects
#private non-transitive properties.
add_library( common INTERFACE )
target_compile_features( common INTERFACE cxx_std_11 )

#Properties for executables and G4NCrystal library (can't transfer all
#properties via INTERFACE targets, so we need this variable-based workaround):
set( binaryprops "" )#empty list
set( libncg4props "" )#empty list

if ( MODIFY_RPATH )
  #Set RPATH properties. For some annoying reason, this is not possible to do
  #via interface targets, so we have to use a variable-based workaround:
  if ( NOT DEFINED CMAKE_INSTALL_RPATH_USE_LINK_PATH )
    #TODO: Figure out if we really need this (perhaps only for geant4 targets?)
    set( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )
  endif()
  if( NOT APPLE )
    #Relocatable RPATHS: $ORIGIN in RPATH (including the $-char!!)  means the
    #location of the binary requiring the dependency:
    list( APPEND binaryprops INSTALL_RPATH "$ORIGIN/${NCrystal_relpath_BINDIR2LIBDIR}" )
    list( APPEND libncg4props INSTALL_RPATH $ORIGIN )
  else()
    #On OSX, rpaths are absolute paths (todo: revisit if this is still the case)
    get_filename_component( tmp "${CMAKE_INSTALL_PREFIX}/${NCrystal_LIBDIR}" ABSOLUTE)
    list( APPEND binaryprops INSTALL_RPATH  "${tmp}" )
    list( APPEND libncg4brops INSTALL_RPATH "${tmp}" )
  endif()

  #Test if compiler supports -Wl,--disable-new-dtags. If it does, apply it
  #(otherwise RPATH sections in binaries become RUNPATH instead, which can be
  #overridden by users LD_LIBRARY_PATH (CMake>=3.14 is needed for LINK_OPTIONS on
  #try_compile and for the target_link_options function):
  #
  #NB: CMake 3.18 introduces CheckLinkerFlag module which we can eventually use
  #    instead of try_compile!!
  if( ${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.14.0" )
    set(TMP_TESTDIR ${PROJECT_BINARY_DIR}/test_dtagflags)
    file(WRITE ${TMP_TESTDIR}/test.c "int main() { return 0; }\n")
    try_compile(LINKER_HAS_DTAGS "${TMP_TESTDIR}" "${TMP_TESTDIR}/test.c" LINK_OPTIONS -Wl,--disable-new-dtags)
    if (LINKER_HAS_DTAGS)
      #target_link_options(NCrystal PUBLIC "-Wl,--disable-new-dtags")
      target_link_options( common INTERFACE -Wl,--disable-new-dtags )
    endif()
  endif()

endif()

# Look for input files. Apparently "file(GLOB ...)" is frowned upon by some
# people. However, the only provided alternative (hardcode all your filenames)
# is rather unappealing. We glob for files, but apply the CONFIGURE_DEPENDS
# keyword when it is supported.

function(file_globsrc output_var pattern)
  if(${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.12.0")
    file(GLOB tmp LIST_DIRECTORIES false CONFIGURE_DEPENDS "${PROJECT_SOURCE_DIR}/${pattern}" )
  else()
    file(GLOB tmp LIST_DIRECTORIES false "${PROJECT_SOURCE_DIR}/${pattern}" )
  endif()
  set(${output_var} ${tmp} PARENT_SCOPE)
endfunction()

file_globsrc( HDRS_NC "ncrystal_core/include/NCrystal/*.*")
file_globsrc( HDRS_NC "ncrystal_core/include/NCrystal/*.*")
file_globsrc( HDRS_INTERNAL_NC "ncrystal_core/include/NCrystal/internal/*.*")
file_globsrc( SRCS_NC "ncrystal_core/src/*.cc")
file_globsrc( EXAMPLES_NC "examples/ncrystal_example_c*.c*")
file_globsrc( DATAFILES "data/*.ncmat")
file_globsrc( SRCS_NCPY "ncrystal_python/*.py")
file_globsrc( SRCS_PYSCRIPTS "ncrystal_python/ncrystal_*")
file_globsrc( tmp "ncrystal_python/nctool" )
list(APPEND SRCS_PYSCRIPTS ${tmp})
file_globsrc( HDRS_NCG4 "ncrystal_geant4/include/G4NCrystal/*.*")
file_globsrc( SRCS_NCG4 "ncrystal_geant4/src/*.cc")
file_globsrc( EXAMPLES_NCG4 "examples/ncrystal_example_g4*.cc")

if (INSTALL_SETUPSH AND NOT INSTALL_PY)
  message(WARNING "INSTALL_SETUPSH is not possible when INSTALL_PY is OFF (forcing INSTALL_SETUPSH=OFF).")
  set(INSTALL_SETUPSH OFF)
endif()

set( NC_STRICT_COMP_FLAGS -Wall -Wextra -pedantic -Werror )
if ( BUILD_STRICT )
  include(CheckCXXCompilerFlag)
  string( REPLACE ";" " " tmp "${NC_STRICT_COMP_FLAGS}" )# check_cxx_compiler_flag needs single argument
  check_cxx_compiler_flag( "${tmp}" COMPILER_SUPPORTS_STRICT_COMP_FLAGS )
endif()

set(STRICT_CPPSTD OFF)
set(STRICT_CSTD OFF)
if ( BUILD_STRICT )
  #We also want to test the C-example with strict C standard (90, 99, 11). For
  #simplicity we simply map: C++11->C90, C++14->C99, C++17->C11. It is mainly
  #done to ensure unit test coverage.
  if ( BUILD_STRICT STREQUAL 11 )
    set( STRICT_CSTD 90 )
    set( STRICT_CPPSTD 11 )
  elseif ( BUILD_STRICT STREQUAL 14 )
    set( STRICT_CSTD 99 )
    set( STRICT_CPPSTD 14 )
  elseif ( BUILD_STRICT STREQUAL 17 )
    set( STRICT_CSTD 11 )
    set( STRICT_CPPSTD 17 )
  elseif ( BUILD_STRICT STREQUAL 20 )
    set( STRICT_CSTD 11 )#NB: Perhaps we will get C22 at some point?
    set( STRICT_CPPSTD 20 )
  endif()
endif()

function(set_target_common_props targetname)
  #Set private non-transitive properties. If strict builds are enabled, this can
  #enforce no warnings and compilation with a specific standards.
  if ( BUILD_STRICT AND COMPILER_SUPPORTS_STRICT_COMP_FLAGS )
    target_compile_options( ${targetname} PRIVATE ${NC_STRICT_COMP_FLAGS} )
  endif()
  if ( STRICT_CPPSTD )
    set_target_properties( ${targetname} PROPERTIES CXX_STANDARD ${STRICT_CPPSTD} CXX_STANDARD_REQUIRED ON CXX_EXTENSIONS OFF)
  endif()
  if ( STRICT_CSTD )
    set_target_properties( ${targetname} PROPERTIES C_STANDARD ${STRICT_CSTD} C_STANDARD_REQUIRED ON C_EXTENSIONS OFF)
  endif()
  #Always disallow M_PI and friends in our own code (they are not portable):
  target_compile_definitions( ${targetname} PRIVATE NCRYSTAL_NO_CMATH_CONSTANTS )
endfunction()

#NCrystal library and header files, including optional built-in modules if enabled:
add_library( NCrystal SHARED ${SRCS_NC} )
set(NCrystal_LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}NCrystal${CMAKE_SHARED_LIBRARY_SUFFIX}")

#put libname in .txt file for downstream (e.g. conda) post-processing:
file(WRITE ${PROJECT_BINARY_DIR}/cfg_ncrystal_libname.txt "${NCrystal_LIBNAME}")

# Dynamic library loading:
if ( CMAKE_DL_LIBS AND UNIX AND NOT DISABLE_DYNLOAD )
  #Cf. https://gitlab.kitware.com/cmake/cmake/-/merge_requests/1642 for why we
  #only do this on UNIX.
  target_link_libraries( NCrystal PRIVATE ${CMAKE_DL_LIBS} )
endif()
if ( DISABLE_DYNLOAD )
  target_compile_definitions( NCrystal PRIVATE NCRYSTAL_DISABLE_DYNLOADER )
endif()

set_target_common_props( NCrystal )
target_link_libraries( NCrystal PRIVATE common )
target_include_directories(NCrystal PRIVATE "${PROJECT_SOURCE_DIR}/ncrystal_core/src"
 PUBLIC   $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/ncrystal_core/include>
        $<INSTALL_INTERFACE:${NCrystal_INCDIR}> )

#Make sure we link in math functions correctly (typically the linker needs libm on unix, but nothing on Windows).
set(TMP_TESTLIBMSRC "#include <math.h>\nint main(int argc,char** argv) { (void)argv;double a=(exp)(argc+1.0); return (int)(a*0.1); }\n")
set(TMP_TESTDIR ${PROJECT_BINARY_DIR}/test_libm)
file(WRITE ${TMP_TESTDIR}/test.c "${TMP_TESTLIBMSRC}")
try_compile(ALWAYS_HAS_MATH "${TMP_TESTDIR}" "${TMP_TESTDIR}/test.c")
if (NOT ALWAYS_HAS_MATH)
  set(TMP_TESTDIR ${PROJECT_BINARY_DIR}/test_libm2)
  file(WRITE ${TMP_TESTDIR}/test.c "${TMP_TESTLIBMSRC}")
  try_compile(MATH_NEEDS_LIBM "${TMP_TESTDIR}" "${TMP_TESTDIR}/test.c" LINK_LIBRARIES m)
  if (MATH_NEEDS_LIBM)
    target_link_libraries(NCrystal PRIVATE m)
  else()
    message(FATAL_ERROR "Could not figure out link flags needed to enable math functions")
  endif()
endif()

#Embed requested external plugins directly into NCrystal:
function(parse_pluginentry entry)
  #Local entries are simply paths, while remote urls always contain at least one
  #semicolon and might look like:
  #
  # https://github.com/mctools/ncplugin-MyPlugin.git::develop <- repo url + '::' + git ref
  # https://github.com/mctools/ncplugin-MyPlugin.git          <- repo url (defaults to git ref 'main')
  # mctools:MyPlugin[::gitref]                                <- same in condensed form (no '/', a single ':')
  if(NOT entry MATCHES ":")
    set(pluginentry_islocal ON PARENT_SCOPE)
    return()
  endif()
  set(pluginentry_islocal OFF PARENT_SCOPE)
  string(REPLACE "::" ";" parts "${entry}")
  list(LENGTH parts nparts)
  if (nparts EQUAL 1)
    list(APPEND parts main)#default git ref tag
  elseif(NOT nparts EQUAL 2)
    message(FATAL_ERROR "Invalid syntax of entry in BUILTIN_PLUGIN_LIST: ${entry}")
  endif()
  list(GET parts 0 remoteurl)
  list(GET parts 1 remoteref)
  #condensed form has no slashes and a single ':'
  if(NOT remoteurl MATCHES "/")
    #condensed form has no slashes and a single ':'
    string(REPLACE ":" ";" urlparts "${remoteurl}")
    list(LENGTH urlparts nurlparts)
    if (nurlparts EQUAL 2)
      list(GET urlparts 0 remoteaccount)
      list(GET urlparts 1 remotereponameending)
      set(remoteurl "https://github.com/${remoteaccount}/ncplugin-${remotereponameending}.git")
    endif()
  endif()
  set(pluginentry_remoteurl ${remoteurl} PARENT_SCOPE)
  set(pluginentry_remoteref ${remoteref} PARENT_SCOPE)
endfunction()


set(all_plugin_names_lc "stdscat;stdabs;stdncmat")

set(NCrystal_builtin_plugin_names "")
set(ncrystal_iplugin 0)
foreach(pluginentry ${BUILTIN_PLUGIN_LIST})
  message("-- Trying to add plugin: ${pluginentry}")
  parse_pluginentry("${pluginentry}")
  if (NOT pluginentry_islocal)
    #need unique id when using FetchContent:
    math(EXPR ncrystal_iplugin "${ncrystal_iplugin}+1")
    string(MD5 pluginid "${pluginentry}")
    set(pluginid "pl${pluginid}_${ncrystal_iplugin}")
    if (NOT COMMAND FetchContent_Declare)
      include(FetchContent)
    endif()
    message("---- Trying to install plugin via remote git repo: ${pluginentry_remoteurl} (gitref ${pluginentry_remoteref})")
    FetchContent_Declare( ${pluginid}
      GIT_REPOSITORY "${pluginentry_remoteurl}"
      GIT_TAG        "${pluginentry_remoteref}"
      GIT_SHALLOW ON
      )
    FetchContent_GetProperties(${pluginid})
    if(NOT ${pluginid}_POPULATED)
      FetchContent_Populate(${pluginid})
      set(pluginlocalsrcdir "${${pluginid}_SOURCE_DIR}")
      message("---- Fetched sources to ${pluginlocalsrcdir}")
    endif()
  else()
    message("---- Trying to install plugin via local path: ${pluginentry}")
    if(NOT IS_ABSOLUTE ${pluginentry})
      message(FATAL_ERROR "Local path in BUILTIN_PLUGIN_LIST is not an absolute paths: ${pluginentry}")
    endif()
    set(pluginlocalsrcdir "${pluginentry}")
  endif()
  if(NOT EXISTS "${pluginlocalsrcdir}/ncplugin_name.txt" OR NOT EXISTS "${pluginlocalsrcdir}/CMakeLists.txt")
    message(FATAL_ERROR "Entry in BUILTIN_PLUGIN_LIST does not appear to contain proper sources (missing ncplugin_name.txt or CMakeLists.txt file): ${pluginentry}")
  endif()
  #Get plugin name from ncplugin_name.txt:
  file(STRINGS "${pluginlocalsrcdir}/ncplugin_name.txt" NCPlugin_NAME LIMIT_COUNT 1)
  string(STRIP "${NCPlugin_NAME}" NCPlugin_NAME)
  #Check unique-ness of plugin name (at least the builtin ones):
  string(TOLOWER "${NCPlugin_NAME}" NCPlugin_NAME_lowercase)
  if (NCPlugin_NAME_lowercase IN_LIST all_plugin_names_lc)
    message(FATAL_ERROR "Multiple plugins have the same name (must be unique and not clash with standard plugins): ${NCPlugin_NAME}.")
  endif()
  list(APPEND all_plugin_names_lc "${NCPlugin_NAME_lowercase}")
  #Add subdirectory after setting a few variables needed by the plugin's
  #CMakeLists.txt, and clearing a few ones we expect it to provide:
  set(NCPLUGIN_DEVMODE OFF)
  set(NCPLUGIN_ASBUILTIN ON)
  unset(NCPLUGIN_SRCFILES)
  unset(NCPLUGIN_DATAFILES)
  unset(NCPLUGIN_COMPILEDEFS)
  unset(NCPLUGIN_INCDIRS)
  set(plugin_binary_dir "${PROJECT_BINARY_DIR}/ncplugins/${NCPlugin_NAME}")
  add_subdirectory(${pluginlocalsrcdir} "${plugin_binary_dir}")
  if (NOT NCPLUGIN_SRCFILES OR NOT NCPlugin_NAME)
    message(FATAL_ERROR "Problem adding plugin (did not provide both NCPLUGIN_SRCFILES and NCPlugin_NAME variables): ${pluginentry}")
  endif()
  if (NCPLUGIN_COMPILEDEFS)
    set_property(SOURCE ${NCPLUGIN_SRCFILES} APPEND PROPERTY COMPILE_DEFINITIONS ${NCPLUGIN_COMPILEDEFS})
  endif()
  if (NCPLUGIN_INCDIRS)
    set_property(SOURCE ${NCPLUGIN_SRCFILES} APPEND PROPERTY INCLUDE_DIRECTORIES ${NCPLUGIN_INCDIRS})
  endif()
  if (NCPLUGIN_DATAFILES)
    foreach(df ${NCPLUGIN_DATAFILES})
      #Check that data file name follows convention: ncplugin-<pluginname>_*.ncmat
      get_filename_component(dfbn "${df}" NAME)
      if(NOT dfbn MATCHES "ncplugin-${NCPlugin_NAME}_.+\.ncmat")
        message(FATAL_ERROR "Problem adding plugin: name of datafile ${dfbn} does not have required form: ncplugin-${NCPlugin_NAME}_*.ncmat")
      endif()
      list(APPEND DATAFILES "${df}")
    endforeach()
  endif()
  list(APPEND NCrystal_builtin_plugin_names ${NCPlugin_NAME})
  target_sources(NCrystal PRIVATE ${NCPLUGIN_SRCFILES})
  message("---- Configured plugin ${NCPlugin_NAME}")
endforeach()
if (NCrystal_builtin_plugin_names)
  target_compile_definitions(NCrystal PRIVATE NCRYSTAL_HAS_BUILTIN_PLUGINS)
  #Add generated .cc file which can load the plugins.
  set(ncplugcc "${PROJECT_BINARY_DIR}/autogen_ncplugins.cc.in")
  file(WRITE ${ncplugcc} "//Autogenerated file\n#include <iostream>\n#include \"NCrystal/NCPluginMgmt.hh\"\n")
  foreach(NCPlugin_NAME ${NCrystal_builtin_plugin_names})
    file(APPEND ${ncplugcc} "namespace NCrystalPlugin_${NCPlugin_NAME} { void registerPlugin(); }\n")
  endforeach()
  file(APPEND ${ncplugcc} "namespace NCrystal {\n  void provideBuiltinPlugins() {\n")
  foreach(NCPlugin_NAME ${NCrystal_builtin_plugin_names})
    file(APPEND ${ncplugcc} "    Plugins::loadBuiltinPlugin(\"${NCPlugin_NAME}\",NCrystalPlugin_${NCPlugin_NAME}::registerPlugin);\n")
  endforeach()
  file(APPEND ${ncplugcc} "  }\n")
  file(APPEND ${ncplugcc} "}\n")
  configure_file("${ncplugcc}" "${PROJECT_BINARY_DIR}/autogen_ncplugins.cc" COPYONLY)
  target_sources(NCrystal PRIVATE "${PROJECT_BINARY_DIR}/autogen_ncplugins.cc")
  message("-- Generated autogen_ncplugins.cc for enabling embedded plugins (will be compiled into the NCrystal library).")
endif()

if ( EMBED_DATA )
  if (INSTALL_DATA)
    message(WARNING "EMBED_DATA and INSTALL_DATA flags were both enabled (forcing INSTALL_DATA=OFF).")
    set(INSTALL_DATA OFF)
  endif()

  target_compile_definitions(NCrystal PRIVATE NCRYSTAL_STDCMAKECFG_EMBED_DATA_ON)

  #Embed data (needs to invoke python process to generate C++ code from .ncmat files)

  #We must find python3 interpreter. The FindPython3 module is only available
  #from CMake 3.12 and is reported to not work well until CMake 3.15.5.
  if( ${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.15.5" )
    find_package (Python3 COMPONENTS Interpreter)#With cmake 3.15.5 we can put
                                                 #3.6 REQUIRED here and simplify
                                                 #the code below.
  else()
    #Older CMake, look for python3 command in path:
    execute_process(COMMAND python3 -c "import sys;print('%s.%s.%s'%sys.version_info[0:3])"
      RESULT_VARIABLE status OUTPUT_VARIABLE Python3_VERSION ERROR_QUIET)
    if(status AND NOT status EQUAL 0)
      set(Python3_FOUND OFF)
      set(Python3_Interpreter_FOUND OFF)
    else()
      set(Python3_FOUND ON)
      set(Python3_Interpreter_FOUND ON)
      set(Python3_EXECUTABLE python3)
    endif()
  endif()
  if( Python3_FOUND AND NOT Python3_Interpreter_FOUND)
    set(Python3_FOUND OFF)
  endif()
  if ( Python3_FOUND AND "${Python3_VERSION}" VERSION_LESS "3.6.0" )
    set(Python3_FOUND OFF)
  endif()
  if (NOT Python3_FOUND)
    message(FATAL_ERROR "Python3.6+ interpreter not found (required when EMBED_DATA options are enabled).")
  endif()
  #Generate C++ code from the .ncmat files:
  execute_process(COMMAND "${Python3_EXECUTABLE}" "${PROJECT_SOURCE_DIR}/ncrystal_python/ncrystal_ncmat2cpp"
    "-n" "NCrystal::AutoGenNCMAT::registerStdNCMAT"
    "--regfctname" "NCrystal::internal::registerEmbeddedNCMAT(const char*,const char*)"
    "-o" "${PROJECT_BINARY_DIR}/autogen_ncmat_data.cc" ${DATAFILES} RESULT_VARIABLE status )
  if(status AND NOT status EQUAL 0)
    message(FATAL_ERROR "Failure while trying to invoke ncrystal_ncmat2cpp (needed since the EMBED_DATA flag was enabled).")
  endif()
  target_sources(NCrystal PRIVATE "${PROJECT_BINARY_DIR}/autogen_ncmat_data.cc")#too late to just append to SRCS_NC
  message("-- Generated autogen_ncmat_data.cc with embedded NCMAT data (will be compiled into the NCrystal library).")
endif()

if (INSTALL_DATA)
  #Hardwiring NCRYSTAL_DATADIR in the binary, although handled with
  #NCRYSTAL_DATADIR env var in setup.sh. The environment variable makes the
  #installation relocatable (at least for users sourcing the installed
  #setup.sh):
  target_compile_definitions(NCrystal PRIVATE "NCRYSTAL_DATADIR=${CMAKE_INSTALL_PREFIX}/${NCrystal_DATAFILESDIR}")
  install(FILES ${DATAFILES} DESTINATION ${NCrystal_DATAFILESDIR})
endif()
install(TARGETS NCrystal EXPORT NCrystalTargets DESTINATION ${NCrystal_LIBDIR} )
install(FILES ${HDRS_NC} DESTINATION ${NCrystal_INCDIR}/NCrystal)
install(FILES ${HDRS_INTERNAL_NC} DESTINATION ${NCrystal_INCDIR}/NCrystal/internal)

install( EXPORT NCrystalTargets FILE NCrystalTargets.cmake NAMESPACE NCrystal:: DESTINATION ${NCrystal_CMAKEDIR} )
add_library(NCrystal::NCrystal ALIAS NCrystal)#always alias namespaces locally

include(CMakePackageConfigHelpers)
write_basic_package_version_file( "${PROJECT_BINARY_DIR}/NCrystalConfigVersion.cmake"
                                  VERSION ${NCrystal_VERSION} COMPATIBILITY SameMajorVersion )

configure_file( "${PROJECT_SOURCE_DIR}/cmake/NCrystalConfig.cmake.in"
                "${PROJECT_BINARY_DIR}/NCrystalConfig.cmake" @ONLY )

install( FILES "${PROJECT_BINARY_DIR}/NCrystalConfigVersion.cmake" "${PROJECT_BINARY_DIR}/NCrystalConfig.cmake"
         DESTINATION ${NCrystal_CMAKEDIR} )

if (INSTALL_PY)
  configure_file( "${PROJECT_SOURCE_DIR}/cmake/ncrystal-config.in" "${PROJECT_BINARY_DIR}/ncrystal-config" @ONLY )
  install( PROGRAMS "${PROJECT_BINARY_DIR}/ncrystal-config" DESTINATION ${NCrystal_BINDIR} )
endif()

#Examples:
if (BUILD_EXAMPLES AND EXAMPLES_NC)
  foreach(ex ${EXAMPLES_NC})
    get_filename_component(exbn "${ex}" NAME_WE)
    add_executable(${exbn} "${ex}")
    set_target_common_props( ${exbn} )
    target_link_libraries(${exbn} NCrystal common)
    if (binaryprops)
      set_target_properties(${exbn} PROPERTIES ${binaryprops})
    endif()
    install(TARGETS ${exbn} DESTINATION ${NCrystal_BINDIR} )
  endforeach()
endif()

#Python interface:
if (INSTALL_PY)
  #NB: We don't actually require Python3 to be available, since we are just
  #copying over files. In principle a user can install python3 after installing
  #NCrystal.
  # --> Python module files:
  if ( NOT NO_DIRECT_PYMODINST )
    install(FILES ${SRCS_NCPY} DESTINATION ${NCrystal_PYMODDIR})
    #autogenerated _nclibpath.py with relative location to library:
    file(WRITE "${PROJECT_BINARY_DIR}/_nclibpath.py.in"
      "#File autogenerated by NCrystal's CMakeLists.txt:\n"
      "liblocation='${NCrystal_relpath_PYMODDIR2LIBDIR}/${NCrystal_LIBNAME}'\n")
    configure_file("${PROJECT_BINARY_DIR}/_nclibpath.py.in" "${PROJECT_BINARY_DIR}/_nclibpath.py" COPYONLY)
    install( FILES "${PROJECT_BINARY_DIR}/_nclibpath.py" DESTINATION ${NCrystal_PYMODDIR})
  else()
    #Similar but preparing a custom directory on which to run pip install during conda build:
    #scripts:
    file( MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/ncrystal_pypkg/NCrystal" )
    configure_file( "${PROJECT_SOURCE_DIR}/cmake/setup.py.in" "${PROJECT_BINARY_DIR}/ncrystal_pypkg/setup.py" @ONLY )
    file(COPY ${SRCS_NCPY} DESTINATION "${PROJECT_BINARY_DIR}/ncrystal_pypkg/NCrystal")
    message("-- Will not install Python modules directly. Instead leaving pypkg skeleton in: ${PROJECT_BINARY_DIR}/ncrystal_pypkg/")
    message("-- Note that the skeleton is incomplete (missing the ${PROJECT_BINARY_DIR}/ncrystal_pypkg/NCrystal/_nclibpath.py file).")
  endif()

  # --> Python scripts:
  install(PROGRAMS ${SRCS_PYSCRIPTS} DESTINATION ${NCrystal_BINDIR})
  if (BUILD_EXAMPLES)
    install(PROGRAMS "${PROJECT_SOURCE_DIR}/examples/ncrystal_example_py" DESTINATION ${NCrystal_BINDIR})
  endif()

endif()

if (INSTALL_MCSTAS)
  install(FILES ${PROJECT_SOURCE_DIR}/ncrystal_mcstas/NCrystal_sample.comp DESTINATION ${NCrystal_DATAROOT}/mcstas)
  install(PROGRAMS ${PROJECT_SOURCE_DIR}/ncrystal_mcstas/ncrystal_preparemcstasdir DESTINATION ${CMAKE_INSTALL_BINDIR})
  if (BUILD_EXAMPLES)
    install(FILES ${PROJECT_SOURCE_DIR}/examples/NCrystal_example_mcstas.instr DESTINATION ${NCrystal_DATAROOT}/mcstas)
  endif()
endif()

#G4NCrystal
if (BUILD_G4HOOKS)
  #Build G4NCrystal library
  find_package(Geant4)
  if(NOT Geant4_FOUND)
    message(FATAL_ERROR "BUILD_G4HOOKS set to ON but failed to enable Geant4 support.")
  endif()

  add_library(G4NCrystal SHARED ${SRCS_NCG4})
  set(G4NCrystal_LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}G4NCrystal${CMAKE_SHARED_LIBRARY_SUFFIX}")
  set_target_common_props( G4NCrystal )
  target_include_directories(G4NCrystal
    PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/ncrystal_geant4/include>
    $<INSTALL_INTERFACE:${NCrystal_INCDIR}>
    )

  install(TARGETS G4NCrystal EXPORT G4NCrystalTargets DESTINATION ${NCrystal_LIBDIR} )

  install(FILES ${HDRS_NCG4} DESTINATION ${NCrystal_INCDIR}/G4NCrystal)
  #Transfer G4 flags uncovered by find_package(Geant4) call:
  #TODO: This should really use Geant4 targets once all known users move to
  #newer Geant4 release (G4 10.6?):
  target_compile_definitions(G4NCrystal PUBLIC ${Geant4_DEFINITIONS})
  target_link_libraries( G4NCrystal PUBLIC NCrystal ${Geant4_LIBRARIES} PRIVATE common )
  if (libncg4props)
    set_target_properties( G4NCrystal PROPERTIES ${libncg4props} )
  endif()
  target_include_directories(G4NCrystal SYSTEM PUBLIC ${Geant4_INCLUDE_DIRS})
  set(Geant4_CXX_FLAGS_aslist ${Geant4_CXX_FLAGS})
  separate_arguments(Geant4_CXX_FLAGS_aslist)
  target_compile_options(G4NCrystal PUBLIC ${Geant4_CXX_FLAGS_aslist})
  #Check if compiler supports -Wno-overloaded-virtual. If so, we add it as a
  #public flag on the G4NCrystal target. This is because of Geant4 adds
  #-Wno-overloaded-virtual, and Geant4 v10.4 headers gives that warning with
  #gcc8/gcc9.
  include(CheckCXXCompilerFlag)
  check_cxx_compiler_flag( -Wno-overloaded-virtual COMPILER_SUPPORTS_NO_OVERLOADED_VIRTUAL_FLAG )
  if (COMPILER_SUPPORTS_NO_OVERLOADED_VIRTUAL_FLAG)
    target_compile_options( G4NCrystal PUBLIC -Wno-overloaded-virtual )
  endif()

  #Add G4NCrystal target in separate file:
  install( EXPORT G4NCrystalTargets FILE G4NCrystalTargets.cmake NAMESPACE NCrystal:: DESTINATION ${NCrystal_CMAKEDIR} )
  add_library(NCrystal::G4NCrystal ALIAS G4NCrystal)#always alias namespaces locally

  #examples if needed:
  if (BUILD_EXAMPLES AND EXAMPLES_NCG4)
    foreach(ex ${EXAMPLES_NCG4})
      get_filename_component(exbn "${ex}" NAME_WE)
      add_executable(${exbn} "${ex}")
      set_target_common_props( ${exbn} )
      target_link_libraries(${exbn} G4NCrystal common)
      if (binaryprops)
        set_target_properties(${exbn} PROPERTIES ${binaryprops} )
      endif()
      install(TARGETS ${exbn} DESTINATION ${NCrystal_BINDIR} )
    endforeach()
  endif()
endif()

if (INSTALL_SETUPSH)
  configure_file( "${PROJECT_SOURCE_DIR}/cmake/template_setup.sh.in" "${PROJECT_BINARY_DIR}/generated_setup.sh" @ONLY )
  configure_file( "${PROJECT_SOURCE_DIR}/cmake/template_unsetup.sh.in" "${PROJECT_BINARY_DIR}/generated_unsetup.sh" @ONLY )
  install( FILES "${PROJECT_BINARY_DIR}/generated_setup.sh" DESTINATION . RENAME setup.sh )
  install( FILES "${PROJECT_BINARY_DIR}/generated_unsetup.sh" DESTINATION . RENAME unsetup.sh )
endif()

#A few status messages summarising everything:
function(ncmsg compstr flag)
  if(flag)
    message( "##   ${compstr} : ON     ##" )
  else()
    message( "##   ${compstr} : OFF    ##" )
  endif()
endfunction()
message( "####################################################" )
message( "## NCrystal configuration summary:                ##" )
message( "##   CMAKE_BUILD_TYPE : ${CMAKE_BUILD_TYPE}                   ##" )
ncmsg(      "NCrystal library and headers       " true               )
ncmsg(      "NCrystal python module and scripts " ${INSTALL_PY}      )
ncmsg(      "G4NCrystal library and headers     " ${BUILD_G4HOOKS}   )
ncmsg(      "Build and install examples         " ${BUILD_EXAMPLES}  )
ncmsg(      "Install shipped data files         " ${INSTALL_DATA}    )
ncmsg(      "Embed shipped data files in library" ${EMBED_DATA}      )
ncmsg(      "Add setup.sh and unsetup.sh files  " ${INSTALL_SETUPSH} )
ncmsg(      "Binaries have RPATH modifications  " ${MODIFY_RPATH}    )
if (DISABLE_DYNLOAD)
  set(enabled_dynload OFF)
else()
  set(enabled_dynload ON)
endif()
ncmsg(      "Support dynamic plugin loading     " ${enabled_dynload}    )
list(LENGTH NCrystal_builtin_plugin_names npluginsbuiltin)
message( "##   Number of builtin plugins:          : ${npluginsbuiltin}      ##" )
message( "####################################################" )
