@rem
@rem   This file is part of the McXtrace neutron ray-trace simulation package
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
@rem Simple batch script for installation of McXtrace on Win32 systems,
@rem after build using Bloodshed Dev-Cpp.
@rem
@rem Please modify the path below for installing mcxtrace in non-standard
@rem location

@if "%NSIS%"=="AUTO" cd mcxtrace-%MCVERSION%
@if "%NSIS%"=="AUTO" goto nsis

@echo ** McXtrace install.bat for Win32...
@echo ...
@echo To have a fully functional McXtrace installation, you should have
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
@echo WARNING: McXtrace 1.12 for Windows requires Scilab == 4.0 and Perl >= 5.6
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
@if "%MCXTRACE_SITE%"=="" set MCXTRACE_SITE=c:\mcxtrace
@SET /P MCXTRACE_SITE=Set McXtrace base directory (default is %MCXTRACE_SITE%):
:nsis
@set ORIGPATH=%PATH%
@set PATH=%PERLBIN%;%SCIBIN%;%DEVBIN%;%DEVLIB%;%PATH%
@echo Trying to guess your plotter and configuration...
@src\mcconfig.pl
@echo Installing in MCXTRACE_SITE=%MCXTRACE_SITE%
@if exist %MCXTRACE_SITE% move %MCXTRACE_SITE% "%MCXTRACE_SITE%.%DATE%"
@echo Creating directory %MCXTRACE_SITE%
@mkdir %MCXTRACE_SITE%
:bin
@if exist %MCXTRACE_SITE%\bin goto lib
@echo Creating directory %MCXTRACE_SITE%\bin
@mkdir %MCXTRACE_SITE%\bin
:lib
@if exist %MCXTRACE_SITE%\lib goto inst
@echo Creating directory %MCXTRACE_SITE%\lib
@mkdir %MCXTRACE_SITE%\lib
:inst
@echo Copying in the files...
@copy src\mcxtrace.exe %MCXTRACE_SITE%\bin
@copy src\mxformat.exe %MCXTRACE_SITE%\bin
@copy support\Win32\install\mpicc.bat %MCXTRACE_SITE%\bin
@copy support\Win32\install\which.exe %MCXTRACE_SITE%\bin
@copy src\mcconvert.pl %MCXTRACE_SITE%\bin\mxconvert.pl
@copy src\mcdaemon.pl %MCXTRACE_SITE%\bin\mxdaemon.pl
@copy src\mcdisplay.pl %MCXTRACE_SITE%\bin\mxdisplay.pl
@copy src\mcdoc.pl %MCXTRACE_SITE%\bin\mxdoc.pl
@copy src\mcformatgui.pl %MCXTRACE_SITE%\bin\mxformatgui.pl
@copy src\mcgui.pl %MCXTRACE_SITE%\bin\mxgui.pl
@copy src\mcplot.pl %MCXTRACE_SITE%\bin\mxplot.pl
@copy src\mcresplot.pl %MCXTRACE_SITE%\bin\mxresplot.pl
@copy src\mcrun.pl %MCXTRACE_SITE%\bin\mxrun.pl
@copy support\Win32\Perl\safewrap.pl %MCXTRACE_SITE%\bin
@copy support\Win32\pgplot\*.* %MCXTRACE_SITE%\bin
@xcopy /e /y /q /i lib %MCXTRACE_SITE%\lib
@xcopy /e /y /q /i xlib %MCXTRACE_SITE%\lib
@echo Done
@echo Doing doc update using mcdoc...
@set PATH=%PATH%;%MCXTRACE_SITE%\bin
@set MCSTAS=%MCXTRACE_SITE%\lib
@mxdoc.pl --text
@echo Placing Mcstas.pm in perl tree....
@support\Win32\install\perlinst.pl support\common\Tk-CodeText-0.3.4\CodeText\McStas.pm
@echo ...............................................................
@echo Modifying Windows registry for MCSTAS and PATH system variables
@echo ...............................................................
@reg add HKCU\Environment /v MCSTAS /d "%MCXTRACE_SITE%\lib" /f
@reg add HKCU\Environment /v PGPLOT_FONT /d "%MCXTRACE_SITE%\bin\grfont.dat" /f
@reg add HKCU\Environment /v PGPLOT_DIR /d "%MCXTRACE_SITE%\bin" /f
@reg add "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v PATH /d "%MCXTRACE_SITE%\bin;%PERLBIN%;%DEVBIN%;%DEVLIB%;%SCIBIN%;%ORIGPATH%" /f /t REG_EXPAND_SZ
@reg add "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v PATHEXT /d "%PATHEXT%;.pl%" /f /t REG_EXPAND_SZ
@echo ..............................................................
@echo .
@echo Please log off and on again to finish the McXtrace setup!
@echo .
@echo After that, start McXtrace with the command mxgui.pl. You may create a
@echo shortcut to this program situated in %MCXTRACE_SITE%\bin. Put it on your
@echo Desktop.
@echo .
@echo Thanks for using McXtrace. End of the installation.
@if "%NSIS%"=="AUTO" goto end
@pause
:end
