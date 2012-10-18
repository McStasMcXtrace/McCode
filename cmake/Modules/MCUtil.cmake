# Macro for installing library files into lib/${FLAVOR},
# while skipping unneeded files
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

