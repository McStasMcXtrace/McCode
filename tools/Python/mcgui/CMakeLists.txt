cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(python-tools-mcgui C)

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
setupMCCODE("${FLAVOR}")

# Select prefix
if(${enable_mcstas})
  set(P "mc")
else()
  set(P "mx")
endif()

set(TOOLS_NAME "tools/Python/${P}gui")

if(WINDOWS)
  set(TOOLS_LIB "${lib}/${TOOLS_NAME}")
else()
  set(TOOLS_LIB "${CMAKE_INSTALL_PREFIX}/${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}")
endif()


# Set some extra options for mccode_config
set(CC "${CMAKE_C_COMPILER}")
set(CFLAGS "-g -O2 -lm ${CMAKE_C_FLAGS}")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-python-${P}gui-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/COPYING")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


# NSIS
set(NSIS_NAME "Python-based mcgui (mcgui-py), ${MCCODE_STRING}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")

# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}, python, python-qscintilla2")

# RPM
set(CPACK_RPM_PACKAGE_REQUIRES
  "${FLAVOR}-${MCCODE_VERSION}, python, qscintilla, PyQt4, qscintilla-python"
  )


include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")


# Configure fallback script
configure_file("${CMAKE_SOURCE_DIR}/mcgui.in" "${WORK}/mcgui" @ONLY)

# Configure config file
configure_file("${CMAKE_SOURCE_DIR}/mccode_config.py.in" "${WORK}/mccode_config.py" @ONLY)

# Configure launcher
configure_file("${CMAKE_SOURCE_DIR}/McCode-py.desktop.in" work/@NAME@-py.desktop @ONLY)

if(WINDOWS)
  set(BINS "${PROJECT_SOURCE_DIR}/mcgui.py" "${WORK}/mccode_config.py")
else()
  set(BINS "${PROJECT_SOURCE_DIR}/mcgui.py" "${WORK}/mcgui" "${WORK}/mccode_config.py")
endif()


if(WINDOWS)
  # Python files
  foreach(name mw.ui confd.ui ew.ui icd.ui mw.ui ssd.ui viewclasses.py run-icon.png compile-icon.png ui-pywrap.py mcfileutils.py mcguiutils.py widgets.py)
    install(
      FILES "${PROJECT_SOURCE_DIR}/${name}"
      DESTINATION "${lib}/${TOOLS_NAME}"
      )
  endforeach()
  install(
    FILES ${BINS}
    DESTINATION "${lib}/${TOOLS_NAME}"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )
endif()


if(NOT WINDOWS)
  # Python files
  foreach(name mw.ui viewclasses.py run-icon.png compile-icon.png ui-pywrap.py mcfileutils.py mcguiutils.py widgets.py)
    install(
      FILES "${PROJECT_SOURCE_DIR}/${name}"
      DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
      )
  endforeach()
  install(
    FILES ${BINS}
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}/${TOOLS_NAME}"
    PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
  )

  # Symlink to main
  add_custom_command(
    OUTPUT "${WORK}/${P}gui-py"
    COMMAND "${CMAKE_COMMAND}" -E make_directory "${WORK}"
    COMMAND "${CMAKE_COMMAND}" -E remove "${WORK}/${P}gui-py"
    COMMAND "${CMAKE_COMMAND}" -E create_symlink
    "${TOOLS_LIB}/mcgui" "${WORK}/${P}gui-py"
    )

  add_custom_target(
    "CREATE_SYMLINK" ALL DEPENDS "${WORK}/${P}gui-py"
    )

  install(
    PROGRAMS "${WORK}/${P}gui-py"
    DESTINATION ${FLAVOR}/${MCCODE_VERSION}/bin
    RENAME "${P}gui-py${DOT_PYTHON_SUFFIX}"
    )

  install(
    FILES "${WORK}/@NAME@-py.desktop" "@FLAVOR@-py.png"
    DESTINATION "${FLAVOR}/${MCCODE_VERSION}/launchers/"
  ) 

endif()
