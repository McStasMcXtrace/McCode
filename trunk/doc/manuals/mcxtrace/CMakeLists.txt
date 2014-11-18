cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(mcxtracetex)

set(FLAVOR mcxtrace)

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")
include(MCUtil)
setupMCCODE("mcxtrace")

# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-manuals-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "erkn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

# NSIS
set(NSIS_NAME "Manuals, ${MCCODE_VERSION}")
set(CPACK_NSIS_PACKAGE_NAME "${NSIS_NAME}")
set(CPACK_NSIS_DISPLAY_NAME "${NSIS_NAME}")

set(MANUAL "mcxtrace-manual.pdf")
set(CMANUAL "mcxtrace-components.pdf")

if(WINDOWS)
  set(MCCODE_LIB "${CMAKE_INSTALL_PREFIX}/")
  set(MANDEST "${lib}/doc/manuals")
else()
  set(MCCODE_LIB "${CMAKE_INSTALL_PREFIX}/${lib}/${MCCODE_NAME}")
  set(MANDEST "${lib}/${MCCODE_NAME}/doc/manuals")
endif()


# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}")

include(CPack)

set(WORK "${PROJECT_BINARY_DIR}/work")

add_custom_command(
  OUTPUT "${WORK}/McCode"
  COMMAND "./setup_links.sh" "${WORK}"
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
  )


file(GLOB LIST "*.tex" "*.sty" "*.bib" "*.bst" )
foreach(NAME ${LIST})
  file(COPY ${NAME} DESTINATION "${WORK}")
endforeach()


file(COPY figures DESTINATION "${WORK}")

file(COPY sources DESTINATION "${WORK}")
file(COPY samples DESTINATION "${WORK}")
file(COPY optics DESTINATION "${WORK}")
file(COPY monitors DESTINATION "${WORK}")
file(COPY misc DESTINATION "${WORK}")
file(COPY COMPprefix DESTINATION "${WORK}")
file(COPY COMPheader DESTINATION "${WORK}")
file(COPY COMPfooter DESTINATION "${WORK}")

foreach(NAME sources samples optics monitors misc)
  add_custom_command(
    OUTPUT "${WORK}/${NAME}.done"
    COMMAND "${PROJECT_SOURCE_DIR}/get_mxdoc_snippets.sh" "${NAME} ${PROJECT_SOURCE_DIR}"
    WORKING_DIRECTORY "${WORK}"
    )
endforeach()


foreach(NAME manual Component_manual)
  add_custom_command(
    OUTPUT "${WORK}/${NAME}.pdf"
    DEPENDS "${WORK}/sources.done"
    DEPENDS "${WORK}/samples.done"
    DEPENDS "${WORK}/optics.done"
    DEPENDS "${WORK}/monitors.done"
    DEPENDS "${WORK}/misc.done"
    COMMAND "${PROJECT_SOURCE_DIR}/compile_pdf.sh" "${NAME}"
    WORKING_DIRECTORY "${WORK}"
    )
  add_custom_target(
    "COMPILE_MANUAL_${NAME}" ALL DEPENDS "${WORK}/McCode" "${WORK}/${NAME}.pdf"
    )
endforeach()


install(FILES "${WORK}/manual.pdf"
  DESTINATION "${MANDEST}"
  RENAME "${MANUAL}"
)

install(FILES "${WORK}/Component_manual.pdf"
  DESTINATION "${MANDEST}"
  RENAME "${CMANUAL}"
)
