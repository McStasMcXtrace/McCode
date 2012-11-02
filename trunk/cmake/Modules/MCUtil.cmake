# Install library files into lib/${FLAVOR}, while skipping unneeded files
macro(install_lib path)
  install (
    DIRECTORY "${path}"
    DESTINATION "lib/${FLAVOR}-${MCCODE_VERSION}"
    PATTERN "Makefile*" EXCLUDE  # skip makefiles
    PATTERN "#*"        EXCLUDE  # skip backup files
    PATTERN ".*"        EXCLUDE  # skip hidden files
    PATTERN "*.out"     EXCLUDE  # skip binary files
  )
endmacro()


# Check whether we are being run through mkdist
macro(is_mkdist outvar)
  string(REPLACE "@" "_" TMP "@MCCODE_NAME@")
  string(COMPARE NOTEQUAL "${TMP}" "_MCCODE_NAME_" ${outvar})
endmacro()


# Setup McCode constants using either mkdist or SVN-defaults
macro(setup_mccode_mkdist)

  # Check for mkdist values
  is_mkdist(MKDIST)

  if(MKDIST)
    ## Set mkdist-provided version
    set(MCCODE_VERSION "@MCCODE_VERSION@")
    set(MCCODE_NAME "${MKDIST} ${MKDIST_S} @__MCCODE_NAME@")
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
endmacro()