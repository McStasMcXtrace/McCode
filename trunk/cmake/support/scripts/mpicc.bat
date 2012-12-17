@REM mpicc.bat batch file for running gcc and MPICH2 under windows

gcc %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 -g -lm -O2 -lmpi -I"c:\Program Files\MPICH2\include" -L"c:\Program Files\MPICH2\lib"