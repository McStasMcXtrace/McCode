@REM mpicc.bat batch file for running gcc and MPICH2 under windows

gcc %* -lmpi -I"c:\Program Files\MPICH2\include" -L"c:\Program Files\MPICH2\lib"
