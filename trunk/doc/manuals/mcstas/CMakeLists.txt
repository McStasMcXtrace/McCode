cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(mcstastex)

set(FLAVOR mcstas)

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")
include(MCUtil)

setup_mccode_mkdist("${FLAVOR}")


# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-manuals-${MCCODE_VERSION}")
set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")

set(MCCODE_LIB "${CMAKE_INSTALL_PREFIX}/lib/${FLAVOR}-${MCCODE_VERSION}")


# Debian
set(CPACK_DEBIAN_PACKAGE_DEPENDS "${FLAVOR}-${MCCODE_VERSION}")


# Make CPack respect the install prefix
set(CPACK_SET_DESTDIR "ON")
set(CPACK_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

include(CPack)


set(WORK "${PROJECT_BINARY_DIR}/work")


add_custom_command(
  OUTPUT "${WORK}/McCode"
  COMMAND "./setup_links.sh" "${WORK}"
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
  )


file(GLOB LIST "*.tex" "*.sty")
foreach(NAME ${LIST})
  file(COPY ${NAME} DESTINATION "${WORK}")
endforeach()


file(COPY figures DESTINATION "${WORK}")


foreach(NAME manual Component_manual)
  add_custom_command(
    OUTPUT "${WORK}/${NAME}.pdf"
    COMMAND "${PROJECT_SOURCE_DIR}/build_pdf.sh" "${NAME}";
    WORKING_DIRECTORY "${WORK}"
    )
  add_custom_target(
    "COMPILE_MANUAL_${NAME}" ALL DEPENDS "${WORK}/McCode" "${WORK}/${NAME}.pdf"
    )
endforeach()


install(FILES "${WORK}/manual.pdf"
  DESTINATION "${MCCODE_LIB}/doc/manuals"
  RENAME "mcstas-manual.pdf"
)

install(FILES "${WORK}/Component_manual.pdf"
  DESTINATION "${MCCODE_LIB}/doc/manuals"
  RENAME "mcstas-components.pdf"
)
