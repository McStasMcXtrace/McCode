$ ver='F$VERIFY(0)
$ SET NOON
$ ECHO = "WRITE SYS$OUTPUT"
$ ECHO "Creating device-dispatch routine GREXEC.F from DRIVERS.LIST"
$!
$! This command file deletes the existing GREXEC.F routine and
$! then creates a new version that includes calls to all device
$! handlers sepecified in file DRIVERS.LIST.
$!
$! modified 8/11/93 bht to check for existence of SYS$LIBRARY:UISSHR.EXE
$! and SYS$LIBRARY:DECW$XLIBSHR.EXE before including windowing drivers
$!
$	ON control_y THEN GOTO Ex
$! Delete any existing GREXEC.F routines (so as to not confuse things).
$	IF F$SEARCH("GREXEC.F;*") .NES. "" THEN -
$		DELETE/NOLOG/NOCONFIRM GREXEC.F;*
$!
$! Now scan the [.DRIVERS] directory for %%DRIVER.F routines and count
$! them.
$!
$	NDEV=0
$	NAMES = ""
$	UIS = F$SEARCH("SYS$LIBRARY:UISSHR.EXE") 
$	XLIB = F$SEARCH("SYS$LIBRARY:DECW$XLIBSHR.EXE")
$	open list DRIVERS.LIST /read
$
$ 40:	read list line /end=50
$       line = f$edit(line,"COMPRESS,TRIM,UNCOMMENT,UPCASE")
$	if line .eqs. "" then goto 40
$	name = f$element(0," ",line)
$! for WSDRIVER check that SYS$LIBRARY:UISSHR.EXE is online
$	IF F$EXTRACT(0,2,NAME) .eqs. "WS" .and. UIS .eqs. "" 
$	    THEN
$		ECHO "Rejecting WSDRIVER -- UIS is not installed"
$		GOTO 40
$           ENDIF
$! for XEDRIVER check that SYS$LIBRARY:DECW$XLIBSHR.EXE is online
$	IF F$EXTRACT(0,2,NAME) .eqs. "XE" .and. XLIB .eqs. ""
$	    THEN
$		ECHO "Rejecting XEDRIVER -- DECWindows is not installed"
$		GOTO 40
$           ENDIF 
$	IF F$EXTRACT(0,2,NAME) .eqs. "X2" .and. XLIB .eqs. ""
$	    THEN
$		ECHO "Rejecting X2DRIVER -- DECWindows is not installed"
$		GOTO 40
$           ENDIF 
$	NDEV = NDEV+1
$	DRIVER'NDEV' = F$EXTRACT(0,6,NAME)
$	CODE'NDEV' = F$ELEMENT(1," ",line)
$	NAMES = NAMES + "," + F$EXTRACT(0,2,DRIVER'ndev')
$	goto 40
$ 50:	close list
$!
$! Create the dispatch routine.  Open file and write header info.
$!
$ Make:
$	ECHO NDEV," device handlers found:"
$	NAMES = NAMES - "," + "."
$	ECHO NAMES
$	OPEN/WRITE TMP TMP.F
$	WRT = "WRITE TMP"
$ WRT "C*GREXEC -- PGPLOT device handler dispatch routine"
$ WRT "C+
$ WRT "      SUBROUTINE GREXEC(IDEV,IFUNC,RBUF,NBUF,CHR,LCHR)"
$ WRT "      INTEGER IDEV, IFUNC, NBUF, LCHR"
$ WRT "      REAL    RBUF(*)"
$ WRT "      CHARACTER*(*) CHR"
$ WRT "C"
$ WRT "C DO NOT MODIFY THIS ROUTINE."
$ WRT "C You should always create a new version by re-executing"
$ WRT "C the command file NEWEXEC.COM."
$ WRT "C---"
$ WRT "      INTEGER NDEV
$ WRT "      PARAMETER (NDEV=",NDEV,")"
$ WRT "      CHARACTER*10 MSG"
$ WRT "C---"
$!
$! Now construct the computed GOTO statement.
$!
$	ICNT=0
$	CBUF="      GOTO("
$ Cgoto:
$	ICNT=ICNT+1
$       IF ICNT .GT. NDEV THEN GOTO Done
$	CBUF=CBUF+"''ICNT'"
$	IF ICNT.NE.NDEV THEN CBUF=CBUF+","
$	IF F$LENGTH(CBUF).LT.60 THEN GOTO Cgoto
$	WRT CBUF
$	CBUF="     :   "
$	GOTO Cgoto
$!
$ Done:
$ WRT CBUF+") IDEV"
$ WRT "      IF (IDEV.EQ.0) THEN"
$ WRT "          RBUF(1) = NDEV"
$ WRT "          NBUF = 1"
$ WRT "      ELSE"
$ WRT "          WRITE (MSG,'(I10)') IDEV"
$ WRT "          CALL GRQUIT('Unknown device code in GREXEC: '//MSG)"
$ WRT "      END IF"
$ WRT "      RETURN"
$ WRT "C---"
$!
$! Now add lines that actually call the device handlers.
$!
$	ICNT=0
$ Mloop:
$	ICNT=ICNT+1
$       IF ICNT.GT.NDEV THEN GOTO Last
$       IF (CODE'ICNT' .GT. 0)
$       THEN
$ WRT f$fao("!5UL CALL !AS(IFUNC,RBUF,NBUF,CHR,LCHR,!AS)", ICNT, -
		DRIVER'ICNT', CODE'ICNT')
$       ELSE
$ WRT f$fao("!5UL CALL !AS(IFUNC,RBUF,NBUF,CHR,LCHR)", ICNT, -
                DRIVER'ICNT')
$       ENDIF
$ WRT "      RETURN"
$	GOTO Mloop
$!
$ Last:
$ WRT "C"
$ WRT "      END"
$	CLOSE TMP
$	RENAME TMP.F GREXEC.F
$	FILE = F$SEARCH("GREXEC.F")
$	ECHO FILE," created"
$	FORTRAN/WARN=ALL/STAND=ALL 'FILE'
$	LIBRARY/REPLACE GRPCKG.OLB GREXEC.OBJ
$	SET FILE/PROT=(O:RWED,S:RWED) GREXEC.OBJ;*
$	DELETE/NOCONFIRM GREXEC.OBJ;*
$ Ex:
$	IF ver THEN SET VERIFY
