@echo off
@REM Simple batch script for installation of McStas on Win32 systems,
@REM after build using Bloodshed Dev-Cpp. 
@REM 
@REM Please modify the path below for installing mcstas in non-standard 
@REM location
@SET MCSTAS=c:\mcstas
@echo McStas install.bat for Win32...
@echo Installing in MCSTAS=%MCSTAS%
@if exist %MCSTAS% goto bin
@mkdir %MCSTAS%
:bin
@if exist %MCSTAS%\bin goto lib
@mkdir %MCSTAS%\bin
:lib
@if exist %MCSTAS%\lib goto inst
@mkdir %MCSTAS%\lib
:inst
@copy mcstas.exe %MCSTAS%\bin
@copy *.pl %MCSTAS%\bin
@xcopy /e /y /q /i lib %MCSTAS%\lib
@echo Done
@echo Please remember to add %MCSTAS%\bin to your path!
@echo Also, set the MCSTAS environment variable to %MCSTAS%\lib
