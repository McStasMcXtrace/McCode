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
elseif(${enable_mcstas})
  set(FLAVOR "mcstas")
else()
  message(FATAL_ERROR "No flavor specified!\n"
    "Please use either -Denable_mcstas=1 or -Denable_mcxtrace=1")
endif()

message("Enabling ${FLAVOR} flavor")
setup_mccode_mkdist("${FLAVOR}")


find_program(AT at)


# Set some extra options for mccode_config
set(CC "${CMAKE_C_COMPILER}")
set(CFLAGS "-g -O2 -lm ${CMAKE_C_FLAGS}")


# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


set(TOOLS_NAME "${FLAVOR}-tools-${MCCODE_VERSION}")
set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${TOOLS_NAME}")


# Debian
set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "work/support/postinst;")
set(CPACK_DEBIAN_PACKAGE_DEPENDS
  "${FLAVOR}-${MCCODE_VERSION}, perl, perl-tk, pdl, pgplot5, libpgplot-perl, gnuplot-x11, sed, bc"
  )
set(CPACK_DEBIAN_PACKAGE_SUGGESTS "scilab, freewrl")


# RPM
set(CPACK_RPM_PACKAGE_REQUIRES
  "${FLAVOR}-${MCCODE_VERSION}, perl, perl-tk, pdl, pgplot5, libpgplot-perl, gnuplot, sed, bc"
  )
set(CPACK_RPM_SPEC_MORE_DEFINE "AutoReq: no")

# Make CPack respect the install prefix
set(CPACK_SET_DESTDIR "ON")
set(CPACK_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")


# Select prefix
if(${enable_mcstas})
  set(P "mc")
else()
  set(P "mx")
endif()

# Include mcstas-comp files
foreach(NAME
    config convert daemon display doc formatgui
    gui plot resplot run stas2vitess
    )

  add_custom_command(
    OUTPUT "${WORK}/${P}${NAME}"
    COMMAND "./perl_fix.sh" "${PROJECT_SOURCE_DIR}/mc${NAME}.pl" "${WORK}/${P}${NAME}"
    WORKING_DIRECTORY "${WORK}"
    )

  add_custom_target(
    "FIX_PERL_${NAME}" ALL DEPENDS "${WORK}/${P}${NAME}"
    )

  install(
    PROGRAMS "${WORK}/${P}${NAME}"
    DESTINATION ${bin}
    RENAME "${P}${NAME}-${MCCODE_VERSION}${DOT_PERL_SUFFIX}"
  )
endforeach()


# Generate debian postinst script
configure_file(cmake/support/debian/postinst.in     work/support/postinst)

# Generate mccode_config.perl and mccode_reconfigure
configure_file(lib/tools/perl/mccode_config.perl.in work/mccode_config.perl @ONLY)
configure_file(lib/tools/perl/mccode_reconfigure.in work/mccode_reconfigure @ONLY)

# Generate perl header
configure_file(perl_env_header.pl.in work/perl_env_header.pl @ONLY immediate)
configure_file(perl_fix.sh.in work/perl_fix.sh @ONLY immediate)

exec_program(chmod ARGS 755  work/perl_env_header.pl work/perl_fix.sh)

# Generate man pages

set(LIST convert display doc gui plot run)
foreach(NAME ${LIST})

  configure_file(
    "doc/man/${P}${NAME}.1.in"
    "${WORK}/doc/man/${P}${NAME}-${MCCODE_VERSION}.1"
    @ONLY)

endforeach()


# install tools (skip *.in files)
install(
  DIRECTORY lib/tools/
  DESTINATION "${lib}/${TOOLS_NAME}"
  PATTERN "*.in" EXCLUDE
)

install(
  FILES "${WORK}/mccode_config.perl"
        "${WORK}/mccode_reconfigure"
  DESTINATION ${lib}/${TOOLS_NAME}/perl/
)

install(
  DIRECTORY "${WORK}/doc/man/"
  DESTINATION ${man}/man1
  FILES_MATCHING PATTERN "*.1"
)
