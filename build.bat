@echo off
@REM Simple batch script for building McStas on Win32 systems,
@REM after build using Bloodshed Dev-Cpp. 
@REM Requires 
@REM   c compiler (defaults to gcc)
@REM   includes (defaults to Dev-Cpp standard location
@REM
@REM *** Start of User configuration ***
@SET CC=c:\Dev-Cpp\bin\gcc.exe
@SET INCLUDE="C:/Dev-Cpp/include"
@SET LIB="C:/Dev-Cpp/lib"
@SET DOZIP="0"
@REM *** End of User configuration ***
@REM
@SET VERSION=MCSTAS_VERSION
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


@if %DOZIP%=="0" goto end
@echo creating zipfile...
cd ..
zip -r mcstas-%VERSION%-i686-unknown-Win32 .\mcstas-%VERSION%
cd mcstas-%VERSION%
:end