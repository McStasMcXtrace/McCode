@rem
@rem   This file is part of the McStas neutron ray-trace simulation package
@rem   Copyright (C) 1997-2004, All rights reserved
@rem   Risoe National Laborartory, Roskilde, Denmark
@rem   Institut Laue Langevin, Grenoble, France
@rem
@rem   This program is free software; you can redistribute it and/or modify
@rem   it under the terms of the GNU General Public License as published by
@rem   the Free Software Foundation; version 2 of the License.
@rem
@rem   This program is distributed in the hope that it will be useful,
@rem   but WITHOUT ANY WARRANTY; without even the implied warranty of
@rem   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@rem   GNU General Public License for more details.
@rem
@rem   You should have received a copy of the GNU General Public License
@rem   along with this program; if not, write to the Free Software
@rem   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
@echo off
@rem Simple batch script for installation of McStas on Win32 systems,
@rem after build using Bloodshed Dev-Cpp.
@rem
@rem Please modify the path below for installing mcstas in non-standard
@rem location

@if "%NSIS%"=="AUTO" cd mcstas-%MCVERSION%
@if "%NSIS%"=="AUTO" goto nsis

@echo ** McStas install.bat for Win32...
@echo ...
@echo To have a fully functional McStas installation, you should have
@echo   * Dec-Cpp from http://www.bloodshed.net/dev/devcpp.html
@echo       Have it installed e.g. in C:\Dev-Cpp
@echo       When installed, add the C:\Dev-Cpp\bin directory to your PATH
@echo       Select from the Windows menu:
@echo       Start/Settings/Control Panel/System/Advanced/Environment Variables
@echo   * Perl and Tcl/Tk from:
@echo       http://www.activestate.com/Products/Download
@echo   IMPORTANT:
@echo   You must install the ppm option with the Perl package!
@echo   * Matlab or Scilab from http://www.scilab.org in C:\Scilab
@echo       With Scilab, add the C:\Program Files\Scilab\bin (or equivalent)
@echo       directory to your PATH
@echo       Select from the Windows menu:
@echo       Start/Settings/Control Panel/System/Advanced/Environment Variables
@echo ...
@echo WARNING: McStas 1.12 for Windows requires Scilab == 4.0 and Perl >= 5.6
@echo ...
@echo Use Ctrl-C if you want to break this script to install these packages or
@pause
@if "%DEVBIN%"=="" set DEVBIN=c:\Dev-CPP\bin
@SET /P DEVBIN=Where is your Dev-CPP installation? (default is %DEVBIN%):
@if "%DEVLIB%"=="" set DEVLIB=c:\Dev-CPP\libexec\gcc\mingw32\3.4.2
@SET /P DEVLIB=What is the location of cc1.exe? (default is %DEVLIB%):
@if "%PERLBIN%"=="" set PERLBIN=c:\perl\bin
@SET /P PERLBIN=Where is your perl.exe? (default is %PERLBIN%):
@if "%SCIBIN%"=="" set SCIBIN=c:\progra~1\scilab-4.0\bin
@SET /P PERLBIN=Where is your Scilab? (default is %SCIBIN%):
@if "%MCSTAS_SITE%"=="" set MCSTAS_SITE=c:\mcstas
@SET /P MCSTAS_SITE=Set McStas base directory (default is %MCSTAS_SITE%):
:nsis
@set ORIGPATH=%PATH%
@set PATH=%PERLBIN%;%SCIBIN%;%DEVBIN%;%DEVLIB%;%PATH%
@echo Trying to guess your plotter and configuration...
@mcconfig.pl
@echo Installing in MCSTAS_SITE=%MCSTAS_SITE%
@if exist %MCSTAS_SITE% move %MCSTAS_SITE% "%MCSTAS_SITE%.%DATE%"
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
@copy mcformat.exe %MCSTAS_SITE%\bin
@copy mpicc.bat %MCSTAS_SITE%\bin
@copy support\Win32\which.exe %MCSTAS_SITE%\bin
@copy *.pl %MCSTAS_SITE%\bin
@copy support\pgplot_win32\*.* %MCSTAS_SITE%\bin
@xcopy /e /y /q /i support\Win32\gnuplot\*.* %MCSTAS_SITE%\bin
@xcopy /e /y /q /i lib %MCSTAS_SITE%\lib
@echo Done
@echo Doing doc update using mcdoc...
@set PATH=%PATH%;%MCSTAS_SITE%\bin
@set MCSTAS=%MCSTAS_SITE%\lib
@mcdoc.pl --text
@echo Placing Mcstas.pm in perl tree....
@support\win32\perlinst.pl Tk\CodeText\Bash.pm support\Tk-CodeText-0.3.4\CodeText\McStas.pm
@echo ...............................................................
@echo Modifying Windows registry for MCSTAS and PATH system variables
@echo ...............................................................
@reg add HKCU\Environment /v MCSTAS /d "%MCSTAS_SITE%\lib" /f
@reg add HKCU\Environment /v PGPLOT_FONT /d "%MCSTAS_SITE%\bin\grfont.dat" /f
@reg add HKCU\Environment /v PGPLOT_DIR /d "%MCSTAS_SITE%\bin" /f
@reg add "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v PATH /d "%MCSTAS_SITE%\bin;%PERLBIN%;%DEVBIN%;%DEVLIB%;%SCIBIN%;%ORIGPATH%" /f /t REG_EXPAND_SZ
@reg add "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v PATHEXT /d "%PATHEXT%;.pl%" /f /t REG_EXPAND_SZ
@echo ..............................................................
@echo .
@echo Please log off and on again to finish the McStas setup!
@echo .
@echo After that, start McStas with the command mcgui.pl. You may create a
@echo shortcut to this program situated in %MCSTAS_SITE%\bin. Put it on your
@echo Desktop.
@echo .
@echo Thanks for using McStas. End of the installation.
@if "%NSIS%"=="AUTO" goto end
@pause
:end
