#Detect (guess) values for most platform. Keep most detection logic in
#functions, to avoid poluting surrounding scope accidentally.

function( detect_platform_variables resultvarname )
  set(provided_vars "")
  macro( provide_var varname )
    list(APPEND provided_vars "${varname}" "${${varname}}")
  endmacro()

  #Check 3 major platforms:
  set( WINDOWS OFF )
  set( DARWIN OFF )
  set( LINUX OFF )
  if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set( WINDOWS ON )
  elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set( DARWIN ON )
  else()
    set( LINUX ON )
  endif()

  #Exectuable suffixes:
  set( MCCODE_EXE_SUFFIX "${CMAKE_EXECUTABLE_SUFFIX}" )
  if ( MCCODE_EXE_SUFFIX  MATCHES "^\\." )
    string(SUBSTRING "${MCCODE_EXE_SUFFIX}" 1 -1 MCCODE_EXE_SUFFIX)
  endif()
  provide_var( MCCODE_EXE_SUFFIX )

  #BROWSER (a.k.a. generic "open" command):
  if ( WINDOWS )
    set( BROWSER start )
  elseif( DARWIN )
    set( BROWSER open )
  else()
    set( BROWSER "xdg-open" )
  endif()
  provide_var( BROWSER )

  #Terminal emulater:
  if ( WINDOWS )
    set( TERMINAL start )
  elseif( DARWIN )
    set( TERMINAL open )
  else()
    set( TERMINAL "")
    set( applist x-terminal-emulator gnome-terminal xterm)
    foreach( tmp ${applist} )
      find_program( progpath "${tmp}" )
      if ( progpath )
        set( TERMINAL "${tmp} -e")
        break()
      endif()
    endforeach()
  endif()
  provide_var( TERMINAL )

  #editor:
  if ( WINDOWS )
    set( EDITOR start )
  elseif( DARWIN )
    set( EDITOR open )
  else()
    set( EDITOR "")
    set( applist gedit kate leafpad subl emacs )
    foreach( tmp ${applist} )
      find_program( progpath "${tmp}" )
      if ( progpath )
        set( EDITOR "${tmp}")
        break()
      endif()
    endforeach()
  endif()
  provide_var( EDITOR )

  #C compiler:
  set( TOOLS_CC "${CMAKE_C_COMPILER}" )
  provide_var( TOOLS_CC )

  #MPI / OACC variables (don't attempt to detect, just expose here so it is
  #clear how to change them at the CMake cfg level):

  set(OACCFLAGS "-fast -Minfo=accel -acc=gpu -gpu=managed -DOPENACC")
  provide_var( OACCFLAGS )
  set(OACC "nvc")
  provide_var( OACC )
  set(MPICC "mpicc")
  provide_var( MPICC )
  if ( MPILIB )
    set(MPIFLAGS "-DUSE_MPI -l${MPILIB}")
  else()
    set(MPIFLAGS "-DUSE_MPI -lmpi")
  endif()
  provide_var( MPIFLAGS )
  set(MPILIB "mpi")
  provide_var( MPILIB )
  set(MPIRUN "mpirun")
  provide_var( MPIRUN )
  if ( NOT WINDOWS )
    set(NEXUSFLAGS "-DUSE_NEXUS -lNeXus")#?? should probably have another value
  else()
    set(NEXUSFLAGS "-DUSE_NEXUS -lNeXus")
  endif()
  provide_var( NEXUSFLAGS )

  #C flags:
  include (CheckCCompilerFlag)
  set(MCCODE_CFLAGS "${CMAKE_C_FLAGS_RELWITHDEBINFO}")
  foreach( flag "-std=c99" "-lm" "-D_POSIX_SOURCE" )
    #NB: plethora of "unset(tmp_test_c_flag_result ...)" statements below is
    #added for safety, to prevent CMake's CACHE system to give unpredictable
    #results.
    unset(tmp_test_c_flag_result CACHE)
    unset(tmp_test_c_flag_result )
    check_c_compiler_flag("${flag}" tmp_test_c_flag_result)
    if ( tmp_test_c_flag_result )
      set(MCCODE_CFLAGS "${MCCODE_CFLAGS} ${flag}")
    endif()
    unset(tmp_test_c_flag_result CACHE)
    unset(tmp_test_c_flag_result )
  endforeach()
  provide_var(MCCODE_CFLAGS)

  set(${resultvarname} "${provided_vars}" PARENT_SCOPE)
endfunction()

function( apply_platform_variables detected_var_list )
  while( detected_var_list )
    list( POP_FRONT detected_var_list detected_varname detected_varval)
    if ( NOT DEFINED "${detected_varname}" )
      message("McCode variable: ${detected_varname}=\"${detected_varval}\" (detected)")
      set(${detected_varname} "${detected_varval}" PARENT_SCOPE)
    elseif( "${${detected_varname}}" STREQUAL "${detected_varval}" )
      message("McCode variable: ${detected_varname}=\"${${detected_varname}}\" (provided, but same as detected)")
    else()
      message("McCode variable: ${detected_varname}=\"${${detected_varname}}\"  (provided, ignoring detected \"${detected_varval}\")")
    endif()
  endwhile()
endfunction()

###############################################################

detect_platform_variables( tmp_detected_var_list )
apply_platform_variables( "${tmp_detected_var_list}" )
unset( tmp_detected_var_list )

#A bit of derived variable logic:
set( MCCODE_EXE_SUFFIX_NEVEREMPTY "${MCCODE_EXECUTABLE_SUFFIX}" )
if ( NOT MCCODE_EXE_SUFFIX_NEVEREMPTY )
  set( MCCODE_EXE_SUFFIX_NEVEREMPTY "out" )
endif()
if ( NOT TERMINAL )
  set( TERMINAL "x-terminal-emulator -e" )
  message(WARNING "Could not detect suitable terminal emulator. Will continue assuming TERMINAL=\"${TERMINAL}\" (set TERMINAL to suitable value to avoid this warning.)")
endif()
if ( NOT EDITOR )
  set( EDITOR "gedit" )
  message(WARNING "Could not detect suitable editor. Will continue assuming EDITOR=\"${editor}\" (set EDITOR to suitable value to avoid this warning.)")
endif()
