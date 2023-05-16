cmake_minimum_required(VERSION 3.17.0)
cmake_policy(VERSION 3.17.0)

# This will install McStas components
project(legacy-tools-cmdline C)

message(STATUS "Configure ${CMAKE_PROJECT_NAME} ${FLAVOR} ${PROJECT_NAME}")

# Choose between McStas or McXtrace
option(BUILD_MCSTAS   "Choose the McStas flavor" off)
option(BUILD_MCXTRACE "Choose the McXtrace flavor (trumps BUILD_MCSTAS)" Off)

# Set module path
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)

if (${BUILD_MCXTRACE})
  set(FLAVOR "mcxtrace")
elseif(${BUILD_MCSTAS})
  set(FLAVOR "mcstas")
else()
  message(FATAL_ERROR "No flavor specified!\n"
    "Please use either -DBUILD_MCSTAS=1 or -DBUILD_MCXTRACE=1")
endif()

setupMCCODE("${FLAVOR}")


find_program(AT at)


# Set some extra options for mccode_config
set(CC "${CMAKE_C_COMPILER}")
if (APPLE)
  set(CFLAGS "${MCCODE_CFLAGS} -headerpad_max_install_names ${CMAKE_C_FLAGS}")
else()
  set(CFLAGS "${MCCODE_CFLAGS} ${CMAKE_C_FLAGS}")
endif()



# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-perl-cmdline-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "${MCCODE_VERSION}")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

set(TOOLS_NAME "tools/Perl")

# NSIS
set(NSIS_NAME "Perl core Tools, ${MCCODE_VERSION}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NSIS_NAME}")


if(WINDOWS)
  set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${MCCODE_NAME}/${lib}/${TOOLS_NAME}")

  # Post install script for downloading extra Perl modules
  # - removed since included in metapackage-build
  # set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS "ExecWait '\\\\${FLAVOR}-${MCCODE_VERSION}\\\\bin\\\\postsetup.bat' ")

else()
  set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}")
endif()


# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS
  "${FLAVOR}-${MCCODE_VERSION}, perl, sed, bc"
  )
set(CPACK_DEBIAN_PACKAGE_SUGGESTS "freewrl")


# RPM
set(CPACK_RPM_PACKAGE_REQUIRES
  "${FLAVOR}-${MCCODE_VERSION}, perl, sed, bc"
  )
set(CPACK_RPM_SPEC_MORE_DEFINE "AutoReq: no")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")


# Select prefix
if(${BUILD_MCSTAS})
  set(P "mc")
  set(SOURCENAMES doc run display-matlab display-mantid stas2vitess)
else()
  set(P "mx")
  set(SOURCENAMES doc run display-matlab)
endif()

# Include mcstas-comp files
foreach(NAME ${SOURCENAMES}
    )
  set(filename mc${NAME})
  
  add_custom_command(
    OUTPUT "${WORK}/${P}${NAME}"
    COMMAND "./perl_fix.sh" "${PROJECT_SOURCE_DIR}/${filename}.pl" "${WORK}/${P}${NAME}"
    WORKING_DIRECTORY "${WORK}"
    )

  add_custom_target(
    "FIX_PERL_${NAME}" ALL DEPENDS "${WORK}/${P}${NAME}"
    )
  
  if(NOT WINDOWS)
    install(
      PROGRAMS "${WORK}/${P}${NAME}"
      DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
      RENAME "${P}${NAME}${DOT_PERL_SUFFIX}"
      )
  else()
    install(
      PROGRAMS "${WORK}/${P}${NAME}"
      DESTINATION ${lib}/perlbin
      RENAME "${P}${NAME}${DOT_PERL_SUFFIX}"
      )
  endif()
endforeach()

# On Unix, also put a doc without pl suffix, as there is no equivalent on the python side
#if(NOT WINDOWS)
#  install(
#    PROGRAMS "${WORK}/${P}doc"
#    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
#    RENAME "${P}doc"
#      )
#endif()

# Generate mccode_config.perl and mccode_reconfigure
configure_file(lib/tools/perl/mccode_config.perl.in work/mccode_config.perl @ONLY)
configure_file(lib/tools/perl/mccode_reconfigure.in work/mccode_reconfigure @ONLY)

# Generate perl header
configure_file(perl_env_header.pl.in work/perl_env_header.pl @ONLY)
configure_file(perl_fix.sh.in work/perl_fix.sh @ONLY)

exec_program(chmod ARGS 755  work/perl_env_header.pl work/perl_fix.sh)

if(NOT WINDOWS)
  # install tools (skip *.in files)
  install(
    DIRECTORY lib/tools/
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
    PATTERN "*.in" EXCLUDE
  )

  install(
    FILES "${WORK}/mccode_config.perl"
          "${WORK}/mccode_reconfigure"
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}/perl/"
  )
  
endif()


if(WINDOWS)
  # install tools (skip *.in files)
  install(
    DIRECTORY lib/tools/
    DESTINATION "${lib}/${TOOLS_NAME}"
    PATTERN "*.in" EXCLUDE
  )

  install(
    FILES "${WORK}/mccode_config.perl"
          "${WORK}/mccode_reconfigure"
    DESTINATION "${lib}/${TOOLS_NAME}/perl/"
  )

  # Safe wrap; used when calling scripts
  install(PROGRAMS support/safewrap.pl DESTINATION ${bin})
endif()
