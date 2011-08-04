$! DCL command procedure to compile Motif demo program for PGPLOT for
$! OpenVMS VAX and AXP
$!----------------------------------------------------------------------
$ DELETE = "DELETE/NOLOG/NOCONFIRM"
$ PURGE  = "PURGE/NOLOG/NOCONFIRM"
$ ON WARNING THEN GOTO VAX
$ MACHINE=F$GETSYI("ARCH_NAME")
$ IF MACHINE .EQS. "AXP" THEN GOTO AXP
$ IF MACHINE .EQS. "Alpha" THEN GOTO AXP
$ GOTO VAX
$VAX:
$  WRITE SYS$OUTPUT "OpenVMS VAX"
$  FCOMPILE = "FORTRAN/NOWARN"
$  CCOMPILE = "CC"
$  GOTO START
$AXP:
$  WRITE SYS$OUTPUT "OpenVMS AXP"
$  FCOMPILE = "FORTRAN/NOWARN/SEPARATE_COMPILATION"
$  CCOMPILE = "CC/STANDARD=VAXC"
$  GOTO START
$START:
$!
$ ON WARNING THEN EXIT
$ PROC     = P1
$ IF PROC.EQS."" THEN PROC = "[]"
$ PGPLOT   = F$PARSE(PROC,,,"DEVICE","SYNTAX_ONLY") + -
             F$PARSE(PROC,,,"DIRECTORY","SYNTAX_ONLY")
$ DRV      = PGPLOT - "]" + ".DRIVERS]"
$ XMOTIF   = PGPLOT - "]" + ".DRIVERS.XMOTIF]"
$ SRC      = PGPLOT - "]" + ".SRC]"
$ WSO      = "WRITE SYS$OUTPUT"
$!
$ IF F$SEARCH("cpgplot.h") .EQS. ""
$ THEN
$    WSO "Install CPG first!"
$    EXIT
$ ENDIF
$!
$ XMOTIF12 = F$SEARCH("SYS$SHARE:DECW$XMLIBSHR12.EXE")
$ XMOTIF11 = F$SEARCH("SYS$SHARE:DECW$XMLIBSHR.EXE")
$ IF XMOTIF12.NES.""
$ THEN
$     WSO "Using DECwindows MOTIF 1.2 libraries"
$     CREATE PGMOTIF.OPT
pgplot_dir:PGXWIN.OBJ
pgplot_dir:XMPGPLOT.OBJ
pgplot_dir:CPGPLOT.OLB/lib
pgplot_dir:GRPCKG.OLB/lib
SYS$SHARE:DECW$XMLIBSHR12.EXE/share
SYS$SHARE:DECW$XTLIBSHRR5.EXE/share
SYS$SHARE:DECW$XLIBSHR.EXE/share
SYS$SHARE:DECC$SHR.EXE/share
$ ELSE IF XMOTIF11.NES.""
$ THEN
$     WSO "Using DECwindows MOTIF 1.1 libraries"
$     CREATE PGMOTIF.OPT
pgplot_dir:PGXWIN.OBJ
pgplot_dir:XMPGPLOT.OBJ
pgplot_dir:CPGPLOT.OLB/lib
pgplot_dir:GRPCKG.OLB/lib
SYS$SHARE:DECW$XMLIBSHR.EXE/share
SYS$SHARE:DECW$XTSHR.EXE/share
SYS$SHARE:DECW$XLIBSHR.EXE/share
SYS$SHARE:DECC$SHR.EXE/share
$ ELSE
$     WSO "MOTIF is not installed on this system"
$     EXIT
$ ENDIF
$ ENDIF
$!
$ WSO "Compiling /XMOTIF Device Handler"
$   FILE = F$SEARCH("XM:XM.H")
$   IF FILE .EQS. ""
$   THEN
$       WSO "MOTIF header files are not installed on this system"
$   ELSE
$       'CCOMPILE' 'DRV'pgxwin.c
$       'CCOMPILE' 'XMOTIF'xmpgplot.c /INCLUDE_DIRECT='DRV'
$   ENDIF
$!
$ COPY 'XMOTIF'xmpgplot.h []
$ 'CCOMPILE' 'XMOTIF'PGMDEMO.C /INCLUDE=[]
$ LINK PGMDEMO, PGMOTIF.OPT/OPT
$ DELETE PGMDEMO.OBJ;*
$ PURGE PGMDEMO.EXE
$!
$ EXIT:  EXIT
