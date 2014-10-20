$! Make the PGPLOT binary font file.
$! Input files: [.FONTS]grfont.txt (ASCII font file)
$!                      pgpack.f
$! Creates:      GRFONT.DAT (binary font file)
$!----------------------------------------------------------------------
$ ON WARNING THEN EXIT
$ PROC     = P1
$ IF PROC.EQS."" THEN PROC = "[]"
$ PGPLOT   = F$PARSE(PROC,,,"DEVICE","SYNTAX_ONLY") + -
             F$PARSE(PROC,,,"DIRECTORY","SYNTAX_ONLY")
$ SRC      = PGPLOT - "]" + ".FONTS]"
$ ECHO     = "WRITE SYS$OUTPUT"
$!
$ ECHO "Compiling font utility program"
$   FORTRAN 'SRC'PGPACK.F/NODEBUG/WARN=ALL
$   LINK/NOUSER PGPACK,GRPSHR.OLB/LIB
$!
$ ECHO "Creating binary font file GRFONT.DAT"
$   DEFINE/USER SYS$INPUT 'SRC'GRFONT.TXT
$   RUN PGPACK
$   DELETE/NOCONF/NOLOG PGPACK.OBJ;*,PGPACK.EXE;*
$   set file/prot=(O:RWED) grfont.dat;*
$   PURGE/NOCONF/NOLOG GRFONT.DAT
$   set file/prot=(S:RWD,O:RWD,G:R,W:R) grfont.dat
$!
$ EXIT
