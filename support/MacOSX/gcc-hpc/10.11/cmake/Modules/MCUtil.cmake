# Install library files into lib/${FLAVOR}, while skipping unneeded files
macro(installLib path)
  if(WINDOWS)
    set(dest "${lib}")
  else()
    set(dest "${FLAVOR}/${MCCODE_VERSION}")
  endif()

  install (
    DIRECTORY "${path}"
    DESTINATION "${dest}"
    PATTERN "Makefile*" EXCLUDE  # skip makefiles
    PATTERN "#*"        EXCLUDE  # skip backup files
    PATTERN ".*"        EXCLUDE  # skip hidden files
    PATTERN "*.out"     EXCLUDE  # skip binary files
    PATTERN "*.*.in"    EXCLUDE  # skip configure processed template files
  )
endmacro()


# Check whether we are being run through mkdist
macro(isMkDist outvar)
  string(REPLACE "@" "_" TMP "@MCCODE_NAME@")
  string(COMPARE NOTEQUAL "${TMP}" "_MCCODE_NAME_" ${outvar})
endmacro()


# Setup McCode constants using either mkdist or SVN-defaults
macro(setupMCCODE FLAVOR)

  # Check for WINDOWS
  if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set(WINDOWS true)
  endif()

  # Set Nexus linking flag
  if(USE_NEXUS)
    set(CMAKE_C_FLAGS  "${CMAKE_C_FLAGS}  -lNeXus")
    set(CMAKE_C_LFLAGS "${CMAKE_C_LFLAGS} -lNeXus")
  endif()

  # Set 32-bit flags
  if(ARCH EQUAL "i386")
    set(CMAKE_C_FLAGS  "-m32")
    set(CMAKE_C_LFLAGS "${CMAKE_C_LFLAGS} -m32")
  endif()

  if(DEFINED ARCH)
    message(STATUS "Compiling for ${ARCH} ${CMAKE_SYSTEM_NAME}")
  else()
    message(STATUS "Compiling for ${CMAKE_SYSTEM_NAME}")
  endif()


  # Set macros
  if("${FLAVOR}" STREQUAL "mcstas")
    set(NAME             "McStas")

    set(FLAVOR           "mcstas")
    set(FLAVOR_UPPER     "MCSTAS")

    set(FLAVOR_FMT       "mcformat")

    set(FLAVOR_LIB       "nlib")
    set(MCCODE_LIBENV    "${FLAVOR_UPPER}")

    set(MCCODE_PARTICLE  "neutron")
    set(MCCODE_PROJECT    1)

    set(MCCODE_PREFIX     "mc")
  endif()

  if("${FLAVOR}" STREQUAL "mcxtrace")
    set(NAME             "McXtrace")

    set(FLAVOR           "mcxtrace")
    set(FLAVOR_UPPER     "MCXTRACE")

    set(FLAVOR_FMT       "mxformat")

    set(FLAVOR_LIB       "xlib")
    set(MCCODE_LIBENV    "${FLAVOR_UPPER}")

    set(MCCODE_PARTICLE "xray")
    set(MCCODE_PROJECT   2)

    set(MCCODE_PREFIX     "mx")
  endif()


  set_property(DIRECTORY ${CMAKE_SOURCE_DIR} APPEND PROPERTY COMPILE_DEFINITIONS
    NAME="${NAME}" FLAVOR="${FLAVOR}" FLAVOR_UPPER="${FLAVOR_UPPER}"
    FLAVOR_FMT="${FLAVOR_FMT}" FLAVOR_LIB="${FLAVOR_LIB}"
    MCCODE_LIBENV="${MCCODE_LIBENV}" MCCODE_PARTICLE="${MCCODE_PARTICLE}"
    MCCODE_PROJECT=${MCCODE_PROJECT}
    )

  # Check for mkdist values
  isMkDist(MKDIST)

  if(MKDIST)
    ## Set mkdist-provided version
    set(MCCODE_VERSION "@MCCODE_VERSION@")
    set(MCCODE_NAME "@MCCODE_NAME@")
    set(MCCODE_DATE "@MCCODE_DATE@")
    set(MCCODE_STRING "@MCCODE_STRING@")
    set(MCCODE_TARNAME "@MCCODE_TARNAME@")
  else()
    ## Set SVN-specific version
    set(MCCODE_VERSION "2.9999-svn")
    set(MCCODE_NAME "${NAME}")
    set(MCCODE_DATE "svn")
    set(MCCODE_STRING "${NAME} ${MCCODE_VERSION}, ${MCCODE_DATE}")
    set(MCCODE_TARNAME "${FLAVOR}")
  endif()


  # Set default installation paths
  foreach(name bin doc etc include lib man sbin share src)
    if(NOT(DEFINED ${name}))
      set(${name} "${name}")
    endif()
  endforeach()


  # Setup mccode name and paths
  set(MCCODE_NAME "${FLAVOR}-${MCCODE_VERSION}")

  if(WINDOWS)
    # Fix installation root
    set(CMAKE_INSTALL_PREFIX "C://")
    set(CPACK_NSIS_INSTALL_ROOT "C:\\\\${FLAVOR}-${MCCODE_VERSION}")

    # Set BIN and LIB paths
    set(MCCODE_BIN "${CMAKE_INSTALL_PREFIX}${MCCODE_NAME}/${bin}/${FLAVOR}")
    set(MCCODE_LIB "${CMAKE_INSTALL_PREFIX}${MCCODE_NAME}/${lib}")
    # Replace '/' with '\'
    string(REPLACE "/" "\\\\" MCCODE_BIN "${MCCODE_BIN}")
    string(REPLACE "/" "\\\\" MCCODE_LIB "${MCCODE_LIB}")
  else()
    set(MCCODE_BIN "${CMAKE_INSTALL_PREFIX}/${bin}/${MCCODE_NAME}")
    set(MCCODE_LIB "${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}")
  endif()

  # Set instrument suffix (after compilation)
  if(NOT DEFINED OUT_SUFFIX)
    if(DEFINED EXE_SUFFIX)
      set(OUT_SUFFIX "${EXE_SUFFIX}")
    else()
      set(OUT_SUFFIX "out")
    endif()
  endif()

  # Helper for adding leading "."
  macro(addDot name val)
    if(NOT DEFINED ${name} AND NOT ${val} STREQUAL "")
      set(${name} ".${val}")
    endif()
  endmacro()

  # Define suffix-macros that include a leading dot "."
  addDot(DOT_EXE_SUFFIX "${EXE_SUFFIX}")
  addDot(DOT_OUT_SUFFIX "${OUT_SUFFIX}")

  addDot(DOT_PYTHON_SUFFIX "${PYTHON_SUFFIX}")
  addDot(DOT_PERL_SUFFIX   "${PERL_SUFFIX}")


  # Add some special Windows/Unix CPACK configuration
  if(WINDOWS)

    # Fix installation folder (installs to ${ROOT}\${DIRECTORY})
    set(CPACK_PACKAGE_INSTALL_DIRECTORY "")

    # Windows program files do not have any version-suffix
    set(PROGRAM_SUFFIX "")

    # Create desktop links for mcguigo and mcstasgo batch files
    set(CPACK_NSIS_CREATE_ICONS "CreateShortCut '$DESKTOP\\\\${MCCODE_PREFIX}gui-${MCCODE_VERSION}.lnk' '\\\\${FLAVOR}-${MCCODE_VERSION}\\\\bin\\\\mccodeguigo.bat' ")
    set(CPACK_NSIS_CREATE_ICONS_EXTRA "CreateShortCut '$DESKTOP\\\\${FLAVOR}-shell-${MCCODE_VERSION}.lnk' '\\\\${FLAVOR}-${MCCODE_VERSION}\\\\bin\\\\mccodego.bat' ")

  else()

    # Have CMake respect install prefix
    message(${CMAKE_INSTALL_PREFIX})
    set(CPACK_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
    set(CPACK_PACKAGING_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

    set(CPACK_RPM_PACKAGE_RELOCATABLE TRUE)

    # Avoid e.g. /usr/local being "part" of the RPMs
    set(CPACK_RPM_EXCLUDE_FROM_AUTO_FILELIST
      ${CMAKE_INSTALL_PREFIX}
      ${CMAKE_INSTALL_PREFIX}/${FLAVOR}
      ${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}
      ${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/bin
      ${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/tools
      ${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/tools/Python
      ${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/tools/Python/mcplot
      ${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/tools/Python/mcdisplay
      ${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/launchers
      )
    
    # Add "-VERSION" to all program files (executables)
    set(PROGRAM_SUFFIX "-${MCCODE_VERSION}")

    # Run postinst and postrm scripts for various platforms
    set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "work/support/postinst;work/support/postrm")
    set(CPACK_RPM_POST_INSTALL_SCRIPT_FILE "${PROJECT_BINARY_DIR}/work/support/postinst;")
    set(CPACK_RPM_POST_UNINSTALL_SCRIPT_FILE "${PROJECT_BINARY_DIR}/work/support/postrm;")

    # Define dependencies for gcc and the like
    set(CPACK_DEBIAN_PACKAGE_DEPENDS "build-essential")
    set(CPACK_RPM_PACKAGE_REQUIRES "gcc")
    
    # Generate postinst and postrm scripts
    configure_file(
      cmake/support/install-scripts/postinst.in
      work/support/postinst
      @ONLY)
    configure_file(
      cmake/support/install-scripts/postrm.in
      work/support/postrm
      @ONLY)

    # Set architecture
    if(ARCH EQUAL "amd64")
      set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
      set(CPACK_RPM_PACKAGE_ARCHITECTURE    "x86_64")
    elseif(ARCH EQUAL "i386")
      set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "i386")
      set(CPACK_RPM_PACKAGE_ARCHITECTURE    "i686")
    else()
      set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "${ARCH}")
      set(CPACK_RPM_PACKAGE_ARCHITECTURE    "${ARCH}")
    endif()

  endif()
endmacro()
