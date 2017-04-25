@REM mpicc.bat batch file for running gcc and MPICH2 under windows

gcc %* -lmsmpi -I"C:\Program Files (x86)\Microsoft SDKs\MPI\Include" -L"C:\Program Files (x86)\Microsoft SDKs\MPI\Lib\x86"

