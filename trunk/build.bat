@echo off
@rem Simple batch script for building McStas on Win32 systems,
@rem after build using Bloodshed Dev-Cpp. 
@rem Requires 
@rem   c compiler (defaults to gcc)
@rem   includes (defaults to Dev-Cpp standard location
@rem   runscilab on path (for building plotlib)
@rem
@rem *** Start of User configuration ***
@set CC=c:\Dev-Cpp\bin\gcc.exe
@set /P CC=Set CC compiler variable (default is %CC%): 
@set INCLUDE="C:/Dev-Cpp/include"
@echo ...
@echo NOTE: Set the include and lib paths using / or \\ - NOT single \
@echo ...
@set /P INCLUDE=Set INCLUDE variable (default is %INCLUDE%): 
@set LIB="C:/Dev-Cpp/lib"
@set /P LIB=Set LIB variable (default is %LIB%): 
@echo ...
@echo ...
@set DOPLOTLIB="no"
@echo NOTE: Building plotlib requires installed scilab on windows path!
@set /P DOPLOTLIB=Build plotlib binaries? (yes/no - default is %DOPLOTLIB%) 
@set DOZIP="0"
@rem *** End of User configuration ***
@rem
@set VERSION=MCSTAS_VERSION
@echo Doing Win32 build using %CC% -I%INCLUDE% -L%LIB%
@del *.o
@del mcstas.exe


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

@if %DOPLOTLIB%=="no" goto theend
@echo .
@echo running scilab for creating the plotlib library...
@echo .
cd lib\tools\scilab\plotlib
runscilab -nw -f buildme.sci
@echo .
@echo PLEASE WAIT FOR Scilab EXECUTION TO FINISH - NEXT, PRESS ENTER
@echo .
@pause
cd ..\..\..\..
:theend

@if %DOZIP%=="0" goto end
@echo creating zipfile...
cd ..
zip -r mcstas-%VERSION%-i686-Intel-Win32 .\mcstas-%VERSION%
cd mcstas-%VERSION%
:end

@pause