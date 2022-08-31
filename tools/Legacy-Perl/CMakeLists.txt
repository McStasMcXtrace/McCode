cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(legacy-tools C)

# Choose between McStas or McXtrace
option(enable_mcstas   "Choose the McStas flavor" off)
option(enable_mcxtrace "Choose the McXtrace flavor (trumps enable_mcstas)" Off)

# Set module path
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Set McCode values (from mkdist or defaults)
include(MCUtil)

if (${enable_mcxtrace})
  set(FLAVOR "mcxtrace")
  set(CPACK_PACKAGE_HOMEPAGE_URL "http://mcxtrace.org/")
elseif(${enable_mcstas})
  set(FLAVOR "mcstas")
  set(CPACK_PACKAGE_HOMEPAGE_URL "http://mcstas.org/")
else()
  message(FATAL_ERROR "No flavor specified!\n"
    "Please use either -Denable_mcstas=1 or -Denable_mcxtrace=1")
endif()

message("Enabling ${FLAVOR} flavor")
setupMCCODE("${FLAVOR}")


find_program(AT at)


# Set some extra options for mccode_config
set(CC "${CMAKE_C_COMPILER}")
set(CFLAGS "-g -O2 -lm -std=c99 ${CMAKE_C_FLAGS}")


# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-perl-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "${MCCODE_VERSION}")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

set(TOOLS_NAME "tools/Perl")

# NSIS
set(NSIS_NAME "Perl Tools, ${MCCODE_VERSION}")
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
  "${FLAVOR}-${MCCODE_VERSION}, ${FLAVOR}-tools-perl-cmdline-${MCCODE_VERSION}, perl, perl-tk, pdl, pgplot5, libpgplot-perl, sed, bc, libtk-codetext-perl"
  )
set(CPACK_DEBIAN_PACKAGE_SUGGESTS "freewrl")


# RPM
set(CPACK_RPM_PACKAGE_REQUIRES
  "${FLAVOR}-${MCCODE_VERSION}, ${FLAVOR}-tools-perl-cmdline-${MCCODE_VERSION}, perl, perl-Tk, perl-PDL, pgplot, perl-PGPLOT, sed, bc, libtk-codetext-perl"
  )
set(CPACK_RPM_SPEC_MORE_DEFINE "AutoReq: no")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")


# Select prefix
if(${enable_mcstas})
  set(P "mc")
  set(SOURCENAMES daemon display formatgui
      gui plot resplot )
else()
  set(P "mx")
  set(SOURCENAMES daemon display formatgui
      gui plot )
endif()

# Include mcstas-comp files
foreach(NAME ${SOURCENAMES}
    )

  if((NAME STREQUAL "gui") OR (NAME STREQUAL "plot")  OR (NAME STREQUAL "display") )
    set(filename ${P}${NAME})
  else()
    set(filename mc${NAME})
  endif()

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


# Generate perl header
configure_file(perl_env_header.pl.in work/perl_env_header.pl @ONLY)
configure_file(perl_fix.sh.in work/perl_fix.sh @ONLY)

# Configure launcher
configure_file(lib/tools/launcher/McCode.desktop.in work/${NAME}-${MCCODE_VERSION}-pl.desktop @ONLY)

exec_program(chmod ARGS 755  work/perl_env_header.pl work/perl_fix.sh)

if(NOT WINDOWS)
  # install tools (skip *.in files)
  install(
    DIRECTORY lib/tools/
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
    PATTERN "*.in" EXCLUDE
  )

  install(
    FILES "${WORK}/${NAME}-${MCCODE_VERSION}-pl.desktop" "lib/tools/launcher/${FLAVOR}-pl.png"
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}/launchers/"
  ) 

endif()


if(WINDOWS)
  # install tools (skip *.in files)
  install(
    DIRECTORY lib/tools/
    DESTINATION "${lib}/${TOOLS_NAME}"
    PATTERN "*.in" EXCLUDE
  )

  # Safe wrap; used when calling scripts
  install(PROGRAMS support/safewrap.pl DESTINATION ${bin})
endif()
