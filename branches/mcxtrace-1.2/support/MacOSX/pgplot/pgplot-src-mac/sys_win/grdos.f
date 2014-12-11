C*GRDATE -- get date and time as character string (Fortran90)
C+
      SUBROUTINE GRDATE(CDATE, LDATE)
      CHARACTER CDATE*(17)
      INTEGER   LDATE
C
C Return the current date and time, in format 'dd-Mmm-yyyy hh:mm'.
C To receive the whole string, the CDATE should be declared
C CHARACTER*17.
C
C Arguments:
C  CDATE : receives date and time, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks. This will always be 17, unless the length
C           of the string supplied is shorter.
C--
C 1989-Mar-17 - [AFT]
C 12/1993 C. T. Dum MS Power Station F32 Version
C 1996-Apr-16 - Fortran 90 version [P.A.Seeger]
C-----------------------------------------------------------------------
      CHARACTER CMON(12)*3
      INTEGER   II(8)
      DATA      CMON/'Jan','Feb','Mar','Apr','May','Jun',               &
     &               'Jul','Aug','Sep','Oct','Nov','Dec'/
C---
      CALL DATE_AND_TIME(VALUES=II)
      WRITE(CDATE,111) II(3),CMON(II(2)),II(1),II(5),II(6)
  111 FORMAT(I2,'-',A3,'-',I4,' ',I2,':',I2)
      LDATE = 17
      RETURN
      END

C*GRFLUN -- free a Fortran logical unit number (MS-DOS)
C+
      SUBROUTINE GRFLUN(LUN)
      INTEGER LUN
C
C Free a Fortran logical unit number allocated by GRGLUN. 
C
C Arguments:
C  LUN    : the logical unit number to free.
C--
C 22-Apr-1996 [PAS]
C-----------------------------------------------------------------------
      CLOSE (LUN)
      RETURN
      END

C*GRGCOM -- read with prompt from user's terminal (Fortran 90)
C+
      INTEGER FUNCTION GRGCOM(CREAD, CPROM, LREAD)
      CHARACTER CREAD*(*), CPROM*(*)
      INTEGER   LREAD
C
C Issue prompt and read a line from the user's terminal; in VMS,
C this is equivalent to LIB$GET_COMMAND.
C
C Arguments:
C  CREAD : (output) receives the string read from the terminal.
C  CPROM : (input) prompt string.
C  LREAD : (output) length of CREAD.
C
C Returns:
C  GRGCOM : 1 if successful, 0 if an error occurs (e.g., end of file).
C--
C 1989-Mar-29
ctd 3/95:len_trim (MS Fortran/Fortran 90)
C-----------------------------------------------------------------------
      INTEGER IER
C---
   11 FORMAT(A)
C---
      GRGCOM = 0
      LREAD = 0
      WRITE (*, 101, IOSTAT=IER) CPROM
  101 FORMAT(1X,A,\)
      IF (IER.EQ.0) READ (*, 11, IOSTAT=IER) CREAD
      IF (IER.EQ.0) GRGCOM = 1
      LREAD = LEN_TRIM(CREAD)
      RETURN
      END
 
C*GRGENV -- get value of PGPLOT environment parameter (Win95)
C+
      SUBROUTINE GRGENV(CNAME, CVALUE, LVALUE)
      USE       MSFLIB
      CHARACTER CNAME*(*), CVALUE*(*)
      INTEGER   LVALUE
C
C Return the value of a PGPLOT environment parameter.
C
C Arguments:
C CNAME   : (input) the name of the parameter to evaluate.
C CVALUE  : receives the value of the parameter, truncated or extended
C           with blanks as necessary. If the parameter is undefined,
C           a blank string is returned.
C LVALUE  : receives the number of characters in CVALUE, excluding
C           trailing blanks. If the parameter is undefined, zero is
C           returned.
C--
C 1990-Mar-19 - [AFT]
C 12/93;3/95 CTD F32
C 16-Apr-1996 - Win95, F90 (MSFLIB, LEN_TRIM) [P.A.Seeger]
C-----------------------------------------------------------------------
C
      CHARACTER*80 CTMP,CTEMP
      CTMP   = 'PGPLOT_'//CNAME
      LVALUE = GETENVQQ(CTMP(:LEN_TRIM(CTMP)),CTEMP)
      IF(LVALUE .NE. 0) THEN
         CVALUE = CTEMP(:LVALUE)
      ELSE
         CVALUE = ' '
      END IF
      RETURN
      END

C*GRGLUN -- get a Fortran logical unit number (MS-DOS)
C+
      SUBROUTINE GRGLUN(LUN)
      INTEGER LUN
C
C Get an unused Fortran logical unit number.
C Returns a Logical Unit Number that is not currently opened.
C After GRGLUN is called, the unit should be opened to reserve
C the unit number for future calls.  Once a unit is closed, it
C becomes free and another call to GRGLUN could return the same
C number.  Also, GRGLUN will not return a number in the range 1-9
C as older software will often use these units without warning.
C
C Arguments:
C  LUN    : receives the logical unit number
C--
C 12-Feb-1989 [AFT/TJP].
C 22-Apr-1996: count upward from 11 [PAS]
C-----------------------------------------------------------------------
      INTEGER I
      LOGICAL QOPEN
C---
      I = 10
      QOPEN = .TRUE.
      DO WHILE (QOPEN)
         I = I+1
         INQUIRE (UNIT=I, OPENED=QOPEN)
      END DO
      LUN = I
      RETURN
      END

C*GRLGTR -- translate logical name (MS-DOS)
C+
      SUBROUTINE GRLGTR (CNAME)
      CHARACTER CNAME*(*)
C
C Recursive translation of a logical name.
C Up to 20 levels of equivalencing can be handled.
C This is used in the parsing of device specifications in the
C VMS implementation of PGPLOT. In other implementations, it may
C be replaced by a null routine.
C
C Argument:
C  CNAME (input/output): initially contains the name to be
C       inspected.  If an equivalence is found it will be replaced
C       with the new name. If not, the old name will be left there. The
C       escape sequence at the beginning of process-permanent file
C       names is deleted and the '_' character at the beginning of
C       device names is left in place.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER CH*1
      CH = CNAME(1:1)
      RETURN
      END

C*GROPTX -- open output text file [MS-DOS]
C+
      INTEGER FUNCTION GROPTX (UNIT, NAME, DEFNAM, MODE)
      INTEGER UNIT,MODE
      CHARACTER*(*) NAME,DEFNAM
C
C Input:
C  UNIT : Fortran unit number to use
C  NAME : name of file to create
C  DEFNAM : default file name (used to fill in missing fields for VMS)
C
C Returns:
C  0 => success; any other value => error.
C-----------------------------------------------------------------------
      INTEGER IER
      CHARACTER CH*1
      CH = DEFNAM(1:1)
      IER = MODE
      OPEN (UNIT=UNIT, FILE=NAME, STATUS='UNKNOWN', IOSTAT=IER)
      GROPTX = IER
      RETURN 
C-----------------------------------------------------------------------
      END

C*GRTRML -- get name of user's terminal (MS-DOS)
C+
      SUBROUTINE GRTRML(CTERM, LTERM)
      CHARACTER CTERM*(*)
      INTEGER   LTERM
C
C Return the device name of the user's terminal, if any.
C
C Arguments:
C  CTERM : receives the terminal name, truncated or extended with
C           blanks as necessary.
C  LTERM : receives the number of characters in CTERM, excluding
C           trailing blanks. If there is not attached terminal,
C           zero is returned.
C--
C 1989-Nov-08
C-----------------------------------------------------------------------
      CTERM = 'CON'
      LTERM = 3
      RETURN
      END

C*GRTTER -- test whether device is user's terminal (MS-DOS)
C+
      SUBROUTINE GRTTER(CDEV, QSAME)
      CHARACTER CDEV*(*)
      LOGICAL   QSAME
C
C Return a logical flag indicating whether the supplied device
C name is a name for the user's controlling terminal or not.
C (Some PGPLOT programs wish to take special action if they are
C plotting on the user's terminal.)
C
C Arguments:
C  CDEV : (input) the device name to be tested.
C  QSAME   : (output) .TRUE. is CDEV contains a valid name for the
C           user's terminal; .FALSE. otherwise.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER CTERM*64
      INTEGER   LTERM
C
      CALL GRTRML(CTERM, LTERM)
      QSAME = (CDEV.EQ.CTERM(:LTERM))
      RETURN
      END

C*GRUSER -- get user name (MS-DOS)
C+
      SUBROUTINE GRUSER(CUSER, LUSER)
      CHARACTER CUSER*(*)
      INTEGER   LUSER
C
C Return the name of the user running the program.
C
C Arguments:
C  CUSER  : receives user name, truncated or extended with
C           blanks as necessary.
C  LUSER  : receives the number of characters in VALUE, excluding
C           trailing blanks.
C--
C 1989-Mar-19 - [AFT]
C-----------------------------------------------------------------------
C
      CALL GRGENV('USER', CUSER, LUSER)
      RETURN
      END

