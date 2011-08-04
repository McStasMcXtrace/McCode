$! DCL command procedure to compile C binding for PGPLOT for
$! OpenVMS VAX and AXP
$!----------------------------------------------------------------------
$ DELETE = "DELETE/NOLOG/NOCONFIRM"
$ PURGE  = "PURGE/NOLOG/NOCONFIRM"
$!
$ ON WARNING THEN EXIT
$ PROC     = P1
$ IF PROC.EQS."" THEN PROC = "[]"
$ PGPLOT   = F$PARSE(PROC,,,"DEVICE","SYNTAX_ONLY") + -
             F$PARSE(PROC,,,"DIRECTORY","SYNTAX_ONLY")
$ CPG      = PGPLOT - "]" + ".CPG]"
$ SRC      = PGPLOT - "]" + ".SRC]"
$ ECHO     = "WRITE SYS$OUTPUT"
$!
$! Check for VMS or AXP.
$!
$ ON WARNING THEN GOTO VAX
$ MACHINE=F$GETSYI("ARCH_NAME")
$ GOTO START
$ VAX:  MACHINE="VAX"
$ START:  ON WARNING THEN GOTO EXIT
$!
$! Compile the PGBIND program.
$!
$ ECHO "Compiling PGBIND for OpenVMS ", MACHINE
$ CC 'CPG'PGBIND.C
$ IF MACHINE .EQS. "VAX"
$ THEN
$    LINK PGBIND, SYS$INPUT:/OPT
     SYS$SHARE:VAXCRTL/SHARE
$ ELSE
$    LINK PGBIND
$ ENDIF
$ PGBIND == "$"+F$SEARCH("PGBIND.EXE", 3)
$!
$! Run PGBIND to generate the header file and wrapper routines.
$!
$ ECHO "Creating C header file and C wrapper routines"
$ SEARCH 'SRC'PG*.F "C%" /NOHEADING/OUTPUT=PROTO.TXT
$ PGBIND vms -h -w PROTO.TXT
$!
$! Compile them.
$!
$ ECHO "Compiling C wrapper routines"
$ FILES = "CPG*.C"
$LOOP:
$   FILE = F$SEARCH(FILES,1)
$   IF FILE .EQS. "" THEN GOTO ENDLOOP
$   NAME = F$PARSE(FILE,,,"NAME","SYNTAX_ONLY")
$   ECHO NAME
$   CC/NOWARNINGS 'FILE'
$   GOTO LOOP
$ ENDLOOP:
$!
$! Create the library.
$!
$ ECHO "Creating PGPLOT C interface library"
$ LIBRARY/CREATE/NOLOG CPGPLOT.OLB CPG*.OBJ
$ PURG CPGPLOT.OLB
$!
$! Remove intermediate files.
$!
$ DELETE pgbind.obj;*,pgbind.exe;*,cpg*.obj;*,cpg*.c;*,proto.txt;*
$!
$! Compile the demo program.
$!
$ ECHO "Compiling demo program CPGDEMO"
$ CC 'CPG'CPGDEMO /INCLUDE_DIRECTORY=[]
$ LINK/NOUSER CPGDEMO,CPGPLOT.OLB/LIB,GRPSHR.OLB/LIB
$ DELETE CPGDEMO.OBJ;*
$ PURGE  CPGDEMO.EXE
$ SET PROT=(S:RWED,O:RWED,G:RE,W:RE) cpgplot.h, cpgplot.olb, cpgdemo.exe
$!
$ EXIT:  EXIT
