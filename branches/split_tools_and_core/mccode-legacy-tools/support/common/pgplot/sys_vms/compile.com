$! Recompile PGPLOT. 
$! Input files: [.SRC]*.F, PGPLOT.INC, GRPCKG1.INC
$!              [.SYS_VMS]*.F
$!              [.DRIVERS]%%DRIV.F
$! Creates:      GRPCKG.OLB  (object-module library).
$! Updates:
$!  19-Oct-1998 D.Maden		In DRIVERS directory, ensure that VTDRIV-VMS.F
$!				gets compiled rather than VTDRIV.F.
$!----------------------------------------------------------------------
$ DELETE = "DELETE/NOLOG/NOCONFIRM"
$ PURGE  = "PURGE/NOLOG/NOCONFIRM"
$! Different setup required for VAX and AXP.
$!
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
$!----------------------------------------------------------------------
$ ON ERROR THEN EXIT
$ PGPLOT   = P1
$ SRC      = PGPLOT - "]" + ".SRC]"
$ VMS      = PGPLOT - "]" + ".SYS_VMS]"
$ DRV      = PGPLOT - "]" + ".DRIVERS]"
$ MOTIF    = PGPLOT - "]" + ".DRIVERS.XMOTIF]"
$ WSO      = "WRITE SYS$OUTPUT"
$!
$! Create the object-module library.
$!
$ WSO "Creating object-module library"
$ LIBRARY/CREATE=(BLOCKS:200)/LOG TEMP.OLB 
$!
$! Compile GRPCKG and PGPLOT.
$!
$ WSO "Compiling PGPLOT library routines from ", SRC
$! (Include files must be in current directory)
$ COPY 'SRC'PGPLOT.INC,GRPCKG1.INC []
$!WSO "(Ignore messages about variables that were declared but not used)"
$   COPY/CONCAT 'SRC'*.F TEMP.FOR
$   FCOMPILE TEMP/NODEBUG
$   LIBRARY/REPLACE TEMP TEMP
$   DELETE TEMP.OBJ;*,TEMP.FOR;*
$ WSO "Compiling VMS-specific routines"
$   COPY/CONCAT 'VMS'*.F TEMP.FOR
$   FCOMPILE TEMP/NODEBUG
$   LIBRARY/REPLACE TEMP TEMP
$   DELETE TEMP.OBJ;*,TEMP.FOR;*
$CLOOP:
$   FILE = F$SEARCH(VMS+"*.C")
$   IF FILE .EQS. "" THEN GOTO ENDCLOOP
$   FILEX = F$PARSE(FILE,,,"NAME","SYNTAX_ONLY")
$   WSO FILEX
$     CCOMPILE/NODEBUG 'FILE'
$     LIBRARY/REPLACE TEMP 'FILEX'.OBJ
$     DELETE 'FILEX'.OBJ;*
$   GOTO CLOOP
$ENDCLOOP:
$ WSO "Compiling Fortran Device Handlers"
$LOOP:
$ FILE = F$SEARCH(DRV+"%%DRIV.F")
$ IF FILE .EQS. "" THEN GOTO ENDLOOP
$ FILEX = F$PARSE(FILE,,,"NAME","SYNTAX_ONLY")
$ FILEXVMS = F$SEARCH(drv+filex+"-VMS.F", 2)
$ if FILEXVMS .nes. "" 
$ THEN
$     file = FILEXVMS
$     filex = f$parse(file,,,"name","syntax_only")
$ ENDIF
$ WSO FILEX
$   FCOMPILE/NODEBUG 'FILE'
$   LIBRARY/REPLACE TEMP 'FILEX'.OBJ
$   DELETE 'FILEX'.OBJ;*
$ GOTO LOOP
$ENDLOOP:
$!------ Delete the following lines if you do not have a C compiler ----
$ WSO "Compiling /XWINDOW Device Handler"
$   DEFINE/NOLOG X11 DECW$INCLUDE
$   CCOMPILE 'DRV'xwdriv.c
$   LIBRARY/REPLACE TEMP xwdriv.obj
$   DELETE xwdriv.obj;*
$ WSO "Compiling /XDISP Device Handler"
$   DEFINE/NOLOG X11 DECW$INCLUDE
$   CCOMPILE 'DRV'x2driv.c
$   CCOMPILE 'DRV'figdisp_comm.c
$   LIBRARY/REPLACE TEMP x2driv.obj,figdisp_comm.obj
$   DELETE x2driv.obj;*,figdisp_comm.obj;
$ WSO "Compiling /XMOTIF Device Handler stub"
$   CCOMPILE 'DRV'xmdriv.c
$   LIBRARY/REPLACE TEMP xmdriv.obj
$   DELETE xmdriv.obj;*
$!------ End delete ----------------------------------------------------
$ SET FILE/PROT=(O:RWED) PGPLOT.INC,GRPCKG1.INC
$ DELETE PGPLOT.INC;*,GRPCKG1.INC;*
$!
$ RENAME TEMP.OLB GRPCKG.OLB
$ SET FILE/PROTECTION=(S:RWED,O:RWED,G:RE,W:RE) GRPCKG.OLB;*
$ PURGE GRPCKG.OLB
$!
$! Compile the pgxwin_server
$!
$!------ Delete the following lines if you do not have a C compiler ----
$ WSO "Compiling PGXWIN_SERVER program for use with /XWINDOW"
$ CCOMPILE 'DRV'pgxwin_server.c
$ LINK/NOUSER pgxwin_server,sys$input:/opt
sys$share:DECW$XLIBSHR.EXE/share
$ DELETE pgxwin_server.obj;*
$ SET FILE/PROTECTION=(S:RWED,O:RWED,G:RE,W:RE) pgxwin_server.exe;*
$ PURGE pgxwin_server.exe
$!------ End delete ----------------------------------------------------
$ EXIT
