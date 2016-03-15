# InstallMCCODE
# A module for configuring and installing McStas / McXtrace
# The following macros needs to be defined before calling this:
# NAME, FLAVOR, FLAVOR_FMT, FLAVOR_LIB,
# MCCODE_PARTICLE, MCCODE_LIBENV, MCCODE_PROJECT
# MAJOR, MINOR, MCCODE_VERSION, MCCODE_NAME, MCCODE_DATE,
# MCCODE_STRING MCCODE_TARNAME
#
# After doing so (using set()) this module can be included with

macro(AppendDef def)
    set_property(DIRECTORY ${CMAKE_SOURCE_DIR} APPEND
      PROPERTY COMPILE_DEFINITIONS
      ${def}
      )
endmacro(AppendDef)

macro(AppendDefIf def)
  if(${def})
    AppendDef(${def}=${def})
  endif()
endmacro(AppendDefIf)


macro(installMCCODE)

  # Ignore CMake warning when setting "-Denable_mcstas=1"
  option(enable_${FLAVOR} "This option is here for compatibility only." On)
  if (NOT enable_${FLAVOR})
    message(FATAL_ERROR "Cannot deselect ${FLAVOR} flavor.")
  endif()

  ## CPack configuration
  set(CPACK_PACKAGE_NAME          "${FLAVOR}-${MCCODE_VERSION}")
  set(CPACK_RESOURCE_FilE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../COPYING")
  set(CPACK_PACKAGE_CONTACT       "pkwi@fysik.dtu.dk")

  ## Package versioning
  set(MAJOR "1")
  set(MINOR "0")

  set(CPACK_PACKAGE_VERSION       "${MAJOR}.${MINOR}")
  set(CPACK_PACKAGE_VERSION_MAJOR "${MAJOR}")
  set(CPACK_PACKAGE_VERSION_MINOR "${MINOR}")

  ## Debian
  set(CPACK_DEBIAN_PACKAGE_DEPENDS       "build-essential, bash")
  set(CPACK_DEBIAN_PACKAGE_RECOMMENDS    "${FLAVOR}-comps-${MCCODE_VERSION}")
  set(CPACK_DEBIAN_PACKAGE_CONFLICTS    "${FLAVOR}-2.1rc1")
  set(CPACK_DEBIAN_PACKAGE_SUGGESTS      "openmpi-bin, openmpi-dev")

  ## NSIS
  set(CPACK_NSIS_PACKAGE_NAME "${MCCODE_STRING}")
  set(CPACK_NSIS_DISPLAY_NAME "${MCCODE_STRING}")

  include(CPack)


  ## Add global definitions
  # set_property(DIRECTORY ${CMAKE_SOURCE_DIR} APPEND PROPERTY COMPILE_DEFINITIONS
  AppendDef(MCCODE_NAME="${MCCODE_NAME}")
	AppendDef(MCCODE_TARNAME="${MCCODE_TARNAME}")
	AppendDef(MCCODE_VERSION="${MCCODE_VERSION}")
	AppendDef(MCCODE_STRING="${MCCODE_STRING}")
        AppendDef(MCCODE_BUGREPORT="www.${MCCODE_TARNAME}.org")
	AppendDef(MCCODE_URL="")

	# -DCC_HAS_PROTOS=1
	# -DSTDC_HEADERS=1
	# -DHAVE_THREADS=\"-DUSE_THREADS\ \$$OPENMP_CFLAGS\ \"



  ## User-adjustable options
  option (USE_NEXUS
    "Support the NEXUS file format" OFF)

  option (USE_THREADS
    "Enable threading; OBSOLETE: Use MPI/SSH grid feature instead." OFF)

  # update definitions to match choices
  if (USE_NEXUS)
    AppendDef(HAVE_NEXUS="-DUSE_NEXUS -lNeXus")
    AppendDef(USE_NEXUS)
  endif()

  AppendDefIf(USE_THREADS)


  ## Functionality needed to check dependencies
  include (CheckFunctionExists)
  include (CheckLibraryExists)
  include (CheckIncludeFiles)


  # A macro for ensuring that all of the values in a variable list are true
  macro(check_vars vars errmsg)
    foreach(var ${vars})
      if(NOT ${var})
        # throw fatal error when seeing a false value
        message(FATAL_ERROR ${errmsg})
      endif()
    endforeach()
  endmacro()


  ## Check system configuration and dependencies

  # REQUIRED
  check_function_exists(malloc      HAVE_MALLOC)
  check_function_exists(realloc     HAVE_REALLOC)
  check_vars(
    "${HAVE_MALLOC};${HAVE_REALLOC}"
    "Missing either malloc or realloc!")

  check_include_files("stdlib.h"    HAVE_STDLIB_H)
  check_include_files("memory.h"    HAVE_MEMORY_H)
  check_include_files("unistd.h"    HAVE_UNISTD_H)
  check_vars(
    "${HAVE_STDLIB_H};${HAVE_MEMORY_H};${HAVE_UNISTD_H}"
    "Missing either stdlib.h, memory.h or unistd.h!"
    )

  check_include_files("inttypes.h"  HAVE_INT_TYPES_H)
  check_include_files("stdint.h"    HAVE_STD_INT_H)
  check_vars(
    "${HAVE_INT_TYPES_H};${HAVE_STD_INT_H}"
    "Missing either inttypes.h or stdint.h"
    )

  check_include_files("sys/types.h" HAVE_SYS_TYPES_H)
  check_include_files("sys/stat.h"  HAVE_SYS_STAT_H)
  check_vars(
    "${HAVE_SYS_TYPES_H};${HAVE_SYS_STAT_H}"
    "Missing either sys/types.h or sys/stat.h"
    )

  check_include_files("string.h"    HAVE_STRING_H)
  check_include_files("strings.h"   HAVE_STRINGS_H)
  check_vars(
    "${HAVE_STRING_H};${HAVE_STRINGS_H}"
    "Missing either string.h or strings.h"
    )


  # Check for math
  check_include_files("math.h" HAVE_MATH_H)
  if(NOT HAVE_MATH_H)
    message(FATAL_ERROR "Error: Cannot find math.h [m]")
  endif()


  # Check for BISON and FLEX (will fail if they are not found)
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
  message("-- Looking for bison and flex")
  find_package(BISON)
  find_package(FLEX)
  message("-- Looking for bison and flex - found")


  # OPTIONAL
  check_function_exists(strcasecmp  HAVE_STRCASECMP)
  check_function_exists(strcasestr  HAVE_STRCASESTR)
  check_function_exists(fdopen      HAVE_FDOPEN)
  check_function_exists(qsort       HAVE_QSORT)

  # Update definitions
  AppendDefIf(HAVE_STRCASECMP)
  AppendDefIf(HAVE_STRCASESTR)
  AppendDefIf(HAVE_FDOPEN)
  AppendDefIf(HAVE_QSORT)



  # Create work directory, where all rewritten source files go to
  # (to support in-place builds)
  file(MAKE_DIRECTORY "work")
  file(MAKE_DIRECTORY "work/support")


  ## Rules for processing files while expanding macros

  # Macro for configuring every file in a directory
  # *.in files are configured, while other files are copied unless target exists
  macro(configure_directory IN_GLOB OUT_DIR)
    file(GLOB MAN_IN_FILES "" "${IN_GLOB}")
    foreach(file_in ${MAN_IN_FILES})
      get_filename_component(filename "${file_in}" NAME)      # /doc/man/example.1.in -> example.1.in
      string(REGEX MATCH "^(.+)\\.in" matches "${filename}")  # example.1.in -> example.1
      if(matches)
        # from IN/doc/man/example.1.in -> OUT/doc/man/example.1
        configure_file (
          "${file_in}"
          "${OUT_DIR}/${CMAKE_MATCH_1}"
          )
      else()
        # do not overwrite files created by configure
        if(NOT (EXISTS "${OUT_DIR}/${filename}"))
          file(
            COPY "${file_in}"
            DESTINATION "${OUT_DIR}")
        endif()
      endif()
    endforeach()
  endmacro()


  configure_directory ("lib/*" "work/lib")
  configure_directory ("lib/share/*" "work/lib/share")

  configure_directory ("src/*" "work/src")

  # Generate man pages
  message("-- Preparing man files")
  file(MAKE_DIRECTORY "work/doc/man")
  configure_directory("doc/man/*" "work/doc/man")


  ## Include source directories for building
  include_directories(
    "${PROJECT_BINARY_DIR}/work/src"             # rewritten files from output dir
    "${PROJECT_BINARY_DIR}/work/lib/share"       # rewritten library files
    "${PROJECT_SOURCE_DIR}/${FLAVOR_LIB}/share"  # lib depending on flavor (nlib/xlib)
  )


  ## Generate lex.yy.c with flex
  add_custom_command(
    OUTPUT work/src/lex.yy.c
    COMMAND "${FLEX_EXECUTABLE}" -i "${PROJECT_SOURCE_DIR}/src/instrument.l"
    WORKING_DIRECTORY work/src
  )


  ## Generate instrument.tab.{h,c} with bison
  add_custom_command(
    OUTPUT work/src/instrument.tab.h work/src/instrument.tab.c
    COMMAND "${BISON_EXECUTABLE}" -v -d "${PROJECT_SOURCE_DIR}/src/instrument.y"
    WORKING_DIRECTORY work/src
  )

  # Handling of system-provided random functions on windows - 
  # needed only in the link step for mccode and -format
  if(WINDOWS)
    AppendDef(random=rand)
    AppendDef(srandom=srand)
  endif()

  ## Build executable for flavor
  add_executable(
	  ${FLAVOR}
	  work/src/cexp.c
	  work/src/cogen.c
	  work/src/coords.c
	  work/src/debug.c
    work/src/file.c
	  work/src/list.c
    work/src/mccode.h
    work/src/memory.c
 	  work/src/port.c
	  work/src/port.h
	  work/src/symtab.c

    # files generated with flex and bison
 	  work/src/lex.yy.c
    work/src/instrument.tab.h
    work/src/instrument.tab.c
  )


  ## Build McFormat executable
  add_executable(
	  "${FLAVOR_FMT}"
	  work/src/mcformat.c
  )
  ## McFormat needs to be linked against m
  target_link_libraries(${FLAVOR_FMT} m)


  ## Add install targets
  include(MCUtil)
  set(WORK "${PROJECT_BINARY_DIR}/work")

  # Flavor-specific library
  installLib("${PROJECT_SOURCE_DIR}/${FLAVOR_LIB}/")

  # Shared library, lib
  installLib("${WORK}/lib/")

  if(NOT WINDOWS)
    # Man pages
    install (
      FILES "${WORK}/doc/man/${FLAVOR}.1"
      DESTINATION ${FLAVOR}/${MCCODE_VERSION}/doc/man/man1
      RENAME "${FLAVOR}${PROGRAM_SUFFIX}.1"
      )
    install (
      FILES "${WORK}/doc/man/${FLAVOR_FMT}.1"
      DESTINATION "${FLAVOR}/${MCCODE_VERSION}/doc/man/man1"
      RENAME "${FLAVOR_FMT}${PROGRAM_SUFFIX}.1"
      )

    # Binaries
    install (
      PROGRAMS "${PROJECT_BINARY_DIR}/${FLAVOR}${DOT_EXE_SUFFIX}"
      DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
    )
    install (
      PROGRAMS "${PROJECT_BINARY_DIR}/${FLAVOR_FMT}${DOT_EXE_SUFFIX}"
      DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
    )

    foreach (name environment module)
      configure_file(
	      cmake/support/run-scripts/${name}.in
	      work/support/${name}
	      @ONLY)
      install(PROGRAMS ${WORK}/support/${name} DESTINATION ${FLAVOR}/${MCCODE_VERSION}/)
    endforeach()
    install(PROGRAMS ${WORK}/support/postinst DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin/)
  endif()

  if(WINDOWS)
    # Generate and install Windows setup scripts
    foreach (name mccodeenv.bat mccodeguigo.bat mccodego.bat mccodetest.bat)
      configure_file(
	      cmake/support/run-scripts/${name}.in
	      work/support/${name}
	      )
      install(PROGRAMS ${WORK}/support/${name} DESTINATION ${bin})
    endforeach()

    # Python related batches special handling
    foreach (name run-py.bat gui-py.bat plot-gnuplot-py.bat plot-chaco-py.bat plot-matplotlib-py.bat plot-matlab.bat display-x3d-py.bat display-matplotlib-py.bat display-R-py.bat display-vtk-py.bat)
      configure_file(
	      cmake/support/run-scripts/${name}.in
	      work/support/${MCCODE_PREFIX}${name}
	      )
      install(PROGRAMS ${WORK}/support/${MCCODE_PREFIX}${name} DESTINATION ${bin})
    endforeach()

    # Binaries
    install (
      PROGRAMS "${PROJECT_BINARY_DIR}/${FLAVOR}${DOT_EXE_SUFFIX}"
      DESTINATION ${bin}
    )
    install (
      PROGRAMS "${PROJECT_BINARY_DIR}/${FLAVOR_FMT}${DOT_EXE_SUFFIX}"
      DESTINATION ${bin}
    )

    install(PROGRAMS
      cmake/support/install-scripts/postsetup.bat
      DESTINATION ${bin}
      )

    install(PROGRAMS
      cmake/support/run-scripts/mpicc.bat
      DESTINATION ${bin}
      )

  endif()


endmacro(installMCCODE)
