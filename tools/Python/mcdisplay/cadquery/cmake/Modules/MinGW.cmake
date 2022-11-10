
# adjust the default behaviour of the FIND_XXX() commands:
# search headers and libraries in the target environment, search
# programs in the host environment
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)


# Python and Perl suffixes
set(PYTHON_SUFFIX "py")
set(PERL_SUFFIX "pl")

# Set executable extension
set(EXE_SUFFIX   "exe")

# OUT_SUFFIX defaults to EXE_SUFFIX