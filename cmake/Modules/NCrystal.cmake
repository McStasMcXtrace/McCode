include(fetcher)

set( NCRYSTAL_REPO "https://github.com/mctools/ncrystal.git" CACHE STRING "Fallback location (URL or local path) of NCrystal sources." )
set( NCRYSTAL_MINIMUM_VERSION 3.4.1 CACHE STRING "Minimum version of NCrystal (can use git ref-spec)." )
option( NCRYSTAL_REQUIRE_PREINSTALL "Require pre-installed NCrystal (>= NCRYSTAL_MINIMUM_VERSION)" OFF )

#Set variables for NCrystal config like this, to keep them from poluting the current
#scope:
set(ncrystal_fetch_params "")
list( APPEND ncrystal_fetch_params NCRYSTAL_NOTOUCH_CMAKE_BUILD_TYPE ON )
list( APPEND ncrystal_fetch_params MODIFY_RPATH ON )#Might not need this depending on environment
list( APPEND ncrystal_fetch_params INSTALL_SETUPSH OFF )
list( APPEND ncrystal_fetch_params BUILD_EXAMPLES OFF )
list( APPEND ncrystal_fetch_params INSTALL_MCSTAS OFF )#.comp/.instr are now part of McStas itself
list( APPEND ncrystal_fetch_params EMBED_DATA ON )#Files won't appear on disk (To be discussed!)
list( APPEND ncrystal_fetch_params INSTALL_DATA OFF )#OFF since EMBED_DATA is ON
if ( WINDOWS )
  list( APPEND ncrystal_fetch_params DISABLE_DYNLOAD ON )#probably does not work on windows?
endif()

#NB: is there an installprefix/python we can use to ensure modules are in python path?
list( APPEND ncrystal_fetch_params NO_DIRECT_PYMODINST OFF )
list( APPEND ncrystal_fetch_params INSTALL_PY ON )
list( APPEND ncrystal_fetch_params BUILD_G4HOOKS OFF )

if (WINDOWS)
  set( tmp_instprefix "" )
else()
  set( tmp_instprefix "${FLAVOR}/${MCCODE_VERSION}/" )
endif()

list(APPEND ncrystal_fetch_params CMAKE_INSTALL_BINDIR "${tmp_instprefix}bin" )
list(APPEND ncrystal_fetch_params CMAKE_INSTALL_DATADIR "${tmp_instprefix}data" )#probably won't be used if INSTALL_DATA+INSTALL_MCSTAS are off
list(APPEND ncrystal_fetch_params NCrystal_PYPATH "${tmp_instprefix}python" )#fixme: add to McStas environment PYTHONPATH?

if (WINDOWS)
  list(APPEND ncrystal_fetch_params CMAKE_INSTALL_LIBDIR lib )
  list(APPEND ncrystal_fetch_params CMAKE_INSTALL_INCLUDEDIR lib)#??
else()
  list(APPEND ncrystal_fetch_params CMAKE_INSTALL_LIBDIR "${tmp_instprefix}libs/ncrystal" )
  list(APPEND ncrystal_fetch_params CMAKE_INSTALL_INCLUDEDIR "${tmp_instprefix}/libs/ncrystal" )#??
endif()

unset( tmp_instprefix )

git_fetch(ncrystal "${NCRYSTAL_MINIMUM_VERSION}" "${NCRYSTAL_REPO}" ${NCRYSTAL_REQUIRE_PREINSTALL} "${ncrystal_fetch_params}")
