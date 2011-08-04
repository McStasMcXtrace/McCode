$! [VAX/VMS DCL] To compile PGPLOT demo programs.
$! Usage: argument 1 is the PGPOT distribution directory: the source
$! code for the demo programs should be in the [.EXAMPLES] subdirectory
$! of this directory, with names PGDEM*.F.  The executable programs are
$! created in the current directory.
$!----------------------------------------------------------------------
$ ON WARNING THEN EXIT
$ PROC     = P1
$ IF PROC.EQS."" THEN PROC = "[]"
$ PGPLOT   = F$PARSE(PROC,,,"DEVICE","SYNTAX_ONLY") + -
             F$PARSE(PROC,,,"DIRECTORY","SYNTAX_ONLY")
$ SRC      = PGPLOT - "]" + ".EXAMPLES]"
$ ECHO     = "WRITE SYS$OUTPUT"
$
$ ECHO "Compiling demonstration programs"
$ DEMOS = SRC + "PGDEM*.F"
$ LOOP:
$     FILE = F$SEARCH(DEMOS,1)
$     IF FILE .EQS. "" THEN GOTO ENDLOOP
$     FILE = F$PARSE(FILE,,,"NAME","SYNTAX_ONLY")
$     CALL COMPILE 'FILE'
$     GOTO LOOP
$ ENDLOOP:
$ EXIT
$!
$ COMPILE: SUBROUTINE
$   ECHO P1
$   FFILE = SRC + P1 + ".F"
$   FORTRAN/NOWARN/STANDARD=ALL 'FFILE'
$   LINK/NOUSER 'P1'.obj,PGPLOT_DIR:GRPSHR.OLB/LIB
$   DELETE/NOCONFIRM/NOLOG 'P1'.obj;*
$   SET PROTECTION=(S:RWED,O:RWED,G:RE,W:RE) 'P1'.exe
$   PURGE/NOLOG/NOCONFIRM 'P1'.exe
$ ENDSUBROUTINE
