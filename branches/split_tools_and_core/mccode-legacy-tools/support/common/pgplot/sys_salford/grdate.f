C*GRDATE -- get date and time as character string (MS-DOS)
C+
      SUBROUTINE GRDATE(CDATE, LDATE)
      CHARACTER CDATE*(17)
      INTEGER   LDATE
C
C Return the current date and time, in format 'dd-Mmm-yyyy hh:mm'.
C To receive the whole string, the CDATE should be declared
C CHARACTER*17.
C
C ArguMents:
C  CDATE : receives date and time, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks. This will always be 17, unless the length
C           of the string supplied is shorter.
C--
C 1989-Mar-17 - [AFT]
C-----------------------------------------------------------------------
C
C 1995-Mar-21 [mlm]  Modified for Salford FTN77
C
      EXTERNAL EDATE@,TIME@
      CHARACTER*8 EDATE@,TIME@
      CDATE = EDATE@()//' '//TIME@()
      LDATE=17
      RETURN
      END
