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
C Arguments:
C  CDATE : receives date and time, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks. This will always be 17, unless the length
C           of the string supplied is shorter.
C--
C 1989-Mar-17 - [AFT]
C-----------------------------------------------------------------------
      CHARACTER CMON(12)*3
      INTEGER*2 IHR, IMIN, ISEC, I100TH
      INTEGER*2 IYR, IMON, IDAY
      DATA CMON/'Jan','Feb','Mar','Apr','May','Jun',
     :          'Jul','Aug','Sep','Oct','Nov','Dec'/
C---
      CALL GETTIM(IHR, IMIN, ISEC, I100TH)
      CALL GETDAT(IYR, IMON, IDAY)
      WRITE(CDATE,111) IDAY,CMON(IMON),IYR,IHR,IMIN
  111 FORMAT(I2,'-',A3,'-',I4,' ',I2,':',I2)
      LDATE=17
      RETURN
      END
