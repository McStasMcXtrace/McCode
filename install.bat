@rem  
@rem   This file is part of the McStas neutron ray-trace simulation package
@rem   Copyright (C) 1997-2003, All rights reserved
@rem   Risoe National Laborartory, Roskilde, Denmark
@rem   Institut Laue Langevin, Grenoble, France
@rem
@rem   This program is free software; you can redistribute it and/or modify
@rem   it under the terms of the GNU General Public License as published by
@rem   the Free Software Foundation; either version 2 of the License, or
@rem   (at your option) any later version.
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
@if "%MCSTAS_SITE%"=="" set MCSTAS_SITE=c:\mcstas
@SET /P MCSTAS_SITE=Set McStas base directory (default is %MCSTAS_SITE%): 
@echo McStas install.bat for Win32...
@echo Trying to guess your plotter...
@start mcconfig.pl
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
@copy *.pl %MCSTAS_SITE%\bin
@xcopy /e /y /q /i lib %MCSTAS_SITE%\lib
@echo Done
@echo Doing doc update using mcdoc...
@set PATH=%PATH%;%MCSTAS_SITE%\bin
@set MCSTAS=%MCSTAS_SITE%\lib
@mcdoc.pl 
@echo ..............................................................
@echo Please remember to add %MCSTAS_SITE%\bin to your path!
@echo Also, set the MCSTAS environment variable to %MCSTAS_SITE%\lib
@echo ..............................................................
@pause