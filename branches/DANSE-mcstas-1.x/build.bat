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
@rem
@echo off
@echo Simple batch script for building McStas on Win32 systems,
@echo after build using Bloodshed Dev-Cpp.
@echo Requires
@echo   c compiler (defaults to gcc)
@echo   includes   (defaults to Dev-Cpp standard location)
@echo ...
@echo *** Start of User configuration ***
@set CC=c:\Dev-Cpp\bin\gcc.exe
@set /P CC=Set CC compiler variable (default is %CC%):
@set INCLUDE="C:\Dev-Cpp\include"
@echo ...
@echo NOTE: Set the C include and lib paths using / or \\ - NOT single \
@echo ...
@set /P INCLUDE=Set C INCLUDE variable (default is %INCLUDE%):
@set LIB="C:\Dev-Cpp\lib"
@set /P LIB=Set C LIB variable (default is %LIB%):
@echo ...
@echo ...
@set DOZIP="0"
@set DONSIS="0"
@rem *** End of User configuration ***
@rem
@set VERSION=MCSTAS_VERSION
@echo Doing Win32 build using %CC% -I%INCLUDE% -L%LIB%
@del *.o
@del mcstas.exe mcformat.exe


%CC% -c symtab.c -o symtab.o -I%INCLUDE%

%CC% -c cogen.c -o cogen.o -I%INCLUDE%

%CC% -c coords.c -o coords.o -I%INCLUDE%

%CC% -c debug.c -o debug.o -I%INCLUDE%

%CC% -c file.c -o file.o -I%INCLUDE%

%CC% -c instrument.tab.c -o instrument.tab.o -I%INCLUDE%

%CC% -c lex.yy.c -o lex.yy.o -I%INCLUDE%

%CC% -c list.c -o list.o -I%INCLUDE%

%CC% -c memory.c -o memory.o -I%INCLUDE%

%CC% -c port.c -o port.o -I%INCLUDE%

%CC% -c cexp.c -o cexp.o -I%INCLUDE%

%CC% symtab.o cogen.o coords.o debug.o file.o instrument.tab.o lex.yy.o list.o memory.o port.o cexp.o  -o "mcstas.exe" -L%LIB%  -I%INCLUDE%

%CC% mcformat.c -o "mcformat.exe"  -L%LIB%  -I%INCLUDE%

@if %DOZIP%=="0" goto end
@echo creating zipfile for Win Distribution (requires zip)...
cd ..
zip -r mcstas-%VERSION%-i686-Intel-Win32 .\mcstas-%VERSION%
cd mcstas-%VERSION%
@if %DONSIS%=="0" goto end
@echo creating Nullsoft installer .exe (requires nullsoft builder)
copy mcstas.ini ..
copy mcstas.nsi ..
copy mcstas.bmp ..
copy LICENSE.rtf ..
cd ..
@rem In principle, we could have a check for the other support apps here...
c:\progra~1\nsis\makensis mcstas.nsi
:end

@echo Build for Windows done. Press a key to exit.
@pause
