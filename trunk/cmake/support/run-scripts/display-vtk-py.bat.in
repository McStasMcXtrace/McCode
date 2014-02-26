@SET PATH=@CPACK_NSIS_INSTALL_ROOT@\\lib\\@FLAVOR@-tools-python-@MCCODE_PREFIX@display-@MCCODE_VERSION@;%PATH%
@REM Isn't windows a lovely place???
@set TMPFILE=%RANDOM%.trace
@echo Running @MCCODE_PREFIX@display for Python-VTK output 
@echo - please give a few [return] if nothing happens!!
@mcrun %* --trace --no-output-files -n1e2 > %TMPFILE%
@mcdisplay.py %TMPFILE%
@del %TMPFILE%
