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

:menu
@echo off
@echo Installation/setup of McStas support software on Win32 systems.
@echo Please select an option:
@echo .
@echo 1) Install Dev-CPP 4.990 (including gcc) (*)
@echo 2) Install perl 5.10 (*)
@echo 3) Install Scilab 4.0 (*)
@echo 4) Run McStas build script (Unneeded if you dowloaded 'binary' version)
@echo 5) Run McStas install script 
@echo .
@echo s) Set up proxy for internet access
@echo q) Quit
@echo .
@echo Warning: You need internet to complete tasks marked by (*)
@echo .
@if "%MYCHOICE%"=="" set MYCHOICE=1
@SET /P MYCHOICE=Proceed to which task? (default is %MYCHOICE%):
@if "%MYCHOICE%"=="1" goto dev-cpp
@if "%MYCHOICE%"=="2" goto perl
@if "%MYCHOICE%"=="3" goto scilab
@if "%MYCHOICE%"=="4" goto mcbuild
@if "%MYCHOICE%"=="5" goto mcinstall
@if "%MYCHOICE%"=="s" goto proxies
@if "%MYCHOICE%"=="q" goto exit
@if "%MYCHOICE%"=="Q" goto exit
@set MYCHOICE=1
@goto menu

:dev-cpp
@echo .
@echo Proceeding to Dev-CPP install...
@.\support\Win32\wget http://www.mcstas.org/download/Win32/devcpp4990setup.exe
@start devcpp4990setup.exe
@set MYCHOICE=2
@goto menu

:perl
@echo .
@echo Proceeding to perl install...
@.\support\Win32\wget http://www.mcstas.org/download/Win32/ActivePerl-5.10.0.1002-MSWin32-x86-283697.msi
@start ActivePerl-5.10.0.1002-MSWin32-x86-283697.msi
@set MYCHOICE=3
@goto menu

:scilab
@echo .
@echo Proceeding to Scilab install...
@.\support\Win32\wget http://www.mcstas.org/download/Win32/scilab-4.0.exe
@start scilab-4.0.exe
@set MYCHOICE=5
@goto menu

:mcbuild
@echo .
@echo Proceeding to McStas build script...
@start build.bat
@set MYCHOICE=5
@goto menu

:mcinstall
@echo .
@echo Proceeding to McStas install script...
@start install.bat
@set MYCHOICE=q
@goto menu
@pause

:proxies
@echo .
@if "%HTTP_PROXY%"=="" set HTTP_PROXY=none
@echo Define proxy server for http access, e.g. http://proxy.ill.fr:8080
@SET /P HTTP_PROXY=Define proxy server: (default is: %HTTP_PROXY%) 
@if "%HTTP_PROXY%"=="nono" set HTTP_PROXY=""
@set MYCHOICE=1
@goto menu

:exit
@echo .
@echo Thanks for using McStas. End of the installation.
@pause
