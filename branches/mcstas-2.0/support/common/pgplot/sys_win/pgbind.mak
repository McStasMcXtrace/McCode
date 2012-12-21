# Makefile for Microsoft Visual C++ V4.0
# platform: Windows-95 and Windows-NT (not tested)
# purpose:  creates CPGPLOT (C bindings) for PGPLOT graphics package
# usage:    copy this file (pgbind.mak), pgbind_prototypes and pgbind.c
#           to the directory where cpgplot.lib and cpgplot.h are 
#           to be created
#           then type        NMAKE /F PGBIND.MAK

all: pgbind.exe cpgplot.lib clean

pgbind.exe : pgbind.c

cpgplot.lib::
	.\pgbind ms -h -w pgbind_prototypes
	for %f in ( cpg*.c )  do cl /c %f
	lib /out:cpgplot.lib cpg*.obj

clean::
	del cpg*.obj
	del cpg*.c
	del pgbind.exe
