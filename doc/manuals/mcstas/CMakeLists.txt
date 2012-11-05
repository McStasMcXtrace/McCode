cmake_minimum_required(VERSION 2.6)

# This will install McStas components
project(mcstastex)

set(FLAVOR mcstas)
set(MCCODE_VERSION 2.9999)


# CPack configuration
set(CPACK_PACKAGE_NAME          "${FLAVOR}-tools-${MCCODE_VERSION}-svn")
# set(CPACK_RESOURCE_FilE_LICENSE "${PROJECT_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_CONTACT       "jsbn@fysik.dtu.dk")

set(CPACK_PACKAGE_VERSION       "1.0")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")


set(MCCODE_LIB "${CMAKE_INSTALL_PREFIX}/lib/${FLAVOR}-${MCCODE_VERSION}-svn")


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


# Include mcstas-comp files
# foreach(NAME
#     config convert daemon display doc formatgui
#     gui plot resplot run stas2vitess
#     )

#   add_custom_command(
#     OUTPUT "${WORK}/${P}${NAME}"
#     COMMAND "./perl_fix.sh" "${MCCODE_LIB}" "${TOOLS_LIB}"
#     "mc${NAME}.pl" "${WORK}/${P}${NAME}"
#     WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
#     )

#   add_custom_target(
#     "FIX_PERL_${NAME}" ALL DEPENDS "${WORK}/${P}${NAME}"
#     )

#   install(
#     PROGRAMS "${WORK}/${P}${NAME}"
#     DESTINATION bin
#     RENAME "${P}${NAME}-${MCCODE_VERSION}"
#   )
# endforeach()




# # install tools (skip *.in files)
# install(
#   DIRECTORY lib/tools/
#   DESTINATION "lib/${TOOLS_NAME}"
#   PATTERN "*.in" EXCLUDE
# )

# install(
#   FILES "${WORK}/mccode_config.pl"
#         "${WORK}/mccode_reconfigure"
#   DESTINATION lib/${TOOLS_NAME}/perl/
# )

# install(
#   DIRECTORY "${WORK}/doc/man/"
#   DESTINATION share/man/man1
#   FILES_MATCHING PATTERN "*.1"
# )