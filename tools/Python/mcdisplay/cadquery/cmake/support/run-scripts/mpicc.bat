@REM mpicc.bat batch file for running gcc and 64-bit MS-MPI under windows

gcc.exe %* -lmsmpi -I"C:\Program Files (x86)\Microsoft SDKs\MPI\Include" -L"C:\Program Files (x86)\Microsoft SDKs\MPI\Lib\x64"

