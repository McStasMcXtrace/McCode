$! DCL command procedure to compile PGDISP for OpenVMS VAX and AXP
$!----------------------------------------------------------------------
$! PGDISP is a display server program for Xwindows. Start up PGDISP
$! to put its window on your X server. Then send PGPLOT output to it
$! by specifying device "/XDISP".
$!
$! PGDISP is best run as a subprocess. To specify command-line options, 
$! define PGDISP as a "foreign command", and then spawn a process to run
$! it, e.g.,
$!	$ PGDISP == "$ECC1:[TJP.PGPLOT]PGDISP"
$!	(substitute the correct disk and directory, but keep the $ sign)
$!	$ SPAWN/NOWAIT/INPUT=NL: PGDISP -line 64
$! To set the PGPLOT default device to be PGDISP:
$!	$ DEFINE PGPLOT_DEV "/XDISP"
$!
$! The object and executable files are placed in the current default
$! directory.
$!
$! Ignore the following messages from the linker:
$! %LINK-W-NUDFSYMS, 7 undefined symbols:
$! %LINK-I-UDFSYM,         COMPOSITEOBJECTCLASS
$! %LINK-I-UDFSYM,         COMPOSITEWIDGETCLASS
$! %LINK-I-UDFSYM,         CONSTRAINTWIDGETCLASS
$! %LINK-I-UDFSYM,         OBJECTCLASS
$! %LINK-I-UDFSYM,         RECTOBJCLASS
$! %LINK-I-UDFSYM,         WIDGETCLASS
$! %LINK-I-UDFSYM,         WINDOWOBJCLASS
$!----------------------------------------------------------------------
$ DELETE = "DELETE/NOLOG/NOCONFIRM"
$ PURGE  = "PURGE/NOLOG/NOCONFIRM"
$ ECHO   = "WRITE SYS$OUTPUT"
$!
$! Check for VMS or AXP
$!
$ ON WARNING THEN GOTO VAX
$ MACHINE=F$GETSYI("ARCH_NAME")
$ IF MACHINE .EQS. "AXP" THEN GOTO AXP
$ IF MACHINE .EQS. "Alpha" THEN GOTO AXP
$ GOTO VAX
$VAX:
$!
$! Hardwire in MACHINE as VAX in case we got here from the warning line
$! above since old versions of VMS won't support the ARCH_NAME field to 
$! F$GETSYI and this will mess up the link statement below.
$!
$  MACHINE="VAX"
$  ECHO "Compiling PGDISP server program for OpenVMS VAX"
$!
$! Check that necessary libraries exist
$!
$ XLIB = F$SEARCH("SYS$SHARE:DECW$XLIBSHR.EXE")
$ CRTL = F$SEARCH("SYS$SHARE:VAXCRTL.EXE")
$ IF XLIB .EQS. ""
$ THEN
$     ECHO "DECW$XLIBSHR not found: PGDISP cannot be compiled"
$     EXIT
$ ENDIF
$ IF CRTL .EQS. ""
$ THEN
$     ECHO "VAXCRTL not found: PGDISP cannot be compiled"
$     EXIT
$ ENDIF
$  CCOMPILE = "CC"
$  GOTO START
$AXP:
$  ECHO "Compiling PGDISP server program for OpenVMS AXP"
$!
$! Check that necessary libraries exist
$!
$ XLIB = F$SEARCH("SYS$SHARE:DECW$XLIBSHR.EXE")
$ CRTL = F$SEARCH("SYS$SHARE:DECC$SHR.EXE")
$ IF XLIB .EQS. ""
$ THEN
$     ECHO "DECW$XLIBSHR not found: PGDISP cannot be compiled"
$     EXIT
$ ENDIF
$ IF CRTL .EQS. ""
$ THEN
$     ECHO "VAXCRTL not found: PGDISP cannot be compiled"
$     EXIT
$ ENDIF
$  CCOMPILE = "CC/STANDARD=VAXC"
$  GOTO START
$START:
$ SET NOON
$!
$! The source code is found in directory with logical name SRC, defined
$! as follows (change this line for your installation):
$!
$ SRC = "[.PGDISPD]"
$ IF P1 .NES. "" THEN SRC = P1 - "]" + ".PGDISPD]"
$!
$! Xwindow include files are in the following directory:
$!
$ DEFINE/NOLOG X11 DECW$INCLUDE
$ DEFINE/NOLOG SYS SYS$LIBRARY
$ DEFINE/NOLOG NETINET SYS$LIBRARY
$!
$! Compile:
$!
$ CCOMPILE /OBJECT=pg_cleanup.obj      /define=PGDISP 'SRC'cleanup.c
$ CCOMPILE 'SRC'pgdisp.c
$ CCOMPILE /OBJECT=pg_figcurs.obj      /define=PGDISP 'SRC'figcurs.c
$ CCOMPILE /OBJECT=pg_getdata.obj      /define=PGDISP 'SRC'getdata.c
$ CCOMPILE /OBJECT=pg_getvisuals.obj   /define=PGDISP 'SRC'getvisuals.c
$ CCOMPILE /OBJECT=pg_handlexevent.obj /define=PGDISP 'SRC'handlexevent.c
$ CCOMPILE /OBJECT=pg_proccom.obj      /define=PGDISP 'SRC'proccom.c
$ CCOMPILE /OBJECT=pg_resdb.obj        /define=PGDISP 'SRC'resdb.c
$ CCOMPILE 'SRC'exposelgwin.c
$ CCOMPILE 'SRC'getcolors.c
$ CCOMPILE 'SRC'initlgluts.c
$ CCOMPILE 'SRC'initlgwin.c
$ CCOMPILE 'SRC'initlock.c
$ CCOMPILE 'SRC'initwmattr.c
$ CCOMPILE 'SRC'mainloop.c
$ CCOMPILE 'SRC'ntoh.c
$ CCOMPILE 'SRC'resizelgwin.c
$ CCOMPILE 'SRC'returnbuf.c
$ CCOMPILE 'SRC'waitevent.c
$ CCOMPILE 'SRC'updatelgtitle.c
$!
$! Link:
$!
$ IF MACHINE .EQS. "VAX"
$ THEN
$    LINK/NOUSER/EXEC=pgdisp.exe pgdisp, pg_cleanup, pg_figcurs, pg_getdata, -
pg_getvisuals, pg_handlexevent, pg_proccom, pg_resdb, exposelgwin, -
getcolors, initlgluts, initlgwin, initlock, initwmattr, mainloop, ntoh,-
resizelgwin, returnbuf, waitevent, updatelgtitle, SYS$INPUT:/opt
SYS$SHARE:VAXCRTL.EXE/SHARE
SYS$SHARE:DECW$XLIBSHR.EXE/SHARE
$ ELSE
$    LINK/NOUSER/EXEC=pgdisp.exe pgdisp, pg_cleanup, pg_figcurs, pg_getdata, -
pg_getvisuals, pg_handlexevent, pg_proccom, pg_resdb, exposelgwin, -
getcolors, initlgluts, initlgwin, initlock, initwmattr, mainloop, ntoh,-
resizelgwin, returnbuf, waitevent, updatelgtitle, SYS$INPUT:/opt
SYS$SHARE:DECW$XLIBSHR.EXE/SHARE
$ ENDIF
$!
$! Remove intermediate files:
$!
$ DELETE pgdisp.obj;*, pg_cleanup.obj;*, pg_figcurs.obj;*,-
pg_getdata.obj;*, pg_getvisuals.obj;*, pg_handlexevent.obj;*,-
pg_proccom.obj;*, pg_resdb.obj;*, exposelgwin.obj;*, getcolors.obj;*,-
initlgluts.obj;*, initlgwin.obj;*, initlock.obj;*, initwmattr.obj;*,-
mainloop.obj;*, ntoh.obj;*, resizelgwin.obj;*, returnbuf.obj;*,-
waitevent.obj;*, updatelgtitle.obj;*
$ PURGE pgdisp.exe
$ SET FILE/PROT=(S:RWED,O:RWED,G:RE,W:RE) pgdisp.exe
$!
$ EXIT
