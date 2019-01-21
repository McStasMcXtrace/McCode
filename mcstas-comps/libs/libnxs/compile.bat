@REM Script for post-install compilation of libnxs.a on Windows
@REM
cd /d %~dp0%
gcc -c nxs.c sgclib.c sgfind.c sghkl.c sgio.c sgsi.c
ar rcs libnxs.a nxs.o sgclib.o sgfind.o sghkl.o sgio.o sgsi.o
