@echo off
@REM Simple batch script for installation of McStas on Win32 systems,
@REM after build using Bloodshed Dev-Cpp. 
@REM 
@REM Please modify the path below for installing mcstas in non-standard 
@REM location
@if "%MCSTAS_SITE%"=="" set MCSTAS_SITE=c:\mcstas
@SET /P MCSTAS_SITE=Set McStas base directory (default is %MCSTAS_SITE%): 
@echo McStas install.bat for Win32...
@echo Installing in MCSTAS_SITE=%MCSTAS_SITE%
@if exist %MCSTAS_SITE% goto bin
@echo Creating directory %MCSTAS_SITE%
@mkdir %MCSTAS_SITE%
:bin
@if exist %MCSTAS_SITE%\bin goto lib
@echo Creating directory %MCSTAS_SITE%\bin
@mkdir %MCSTAS_SITE%\bin
:lib
@if exist %MCSTAS_SITE%\lib goto inst
@echo Creating directory %MCSTAS_SITE%\lib
@mkdir %MCSTAS_SITE%\lib
:inst
@echo Copying in the files...
@copy mcstas.exe %MCSTAS_SITE%\bin
@copy *.pl %MCSTAS_SITE%\bin
@xcopy /e /y /q /i lib %MCSTAS_SITE%\lib
@echo Done
@echo Please remember to add %MCSTAS_SITE%\bin to your path!
@echo Also, set the MCSTAS environment variable to %MCSTAS_SITE%\lib
