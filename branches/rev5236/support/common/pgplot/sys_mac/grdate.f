
C*GRDATE -- get date and time as character string (Mac)
C+
      SUBROUTINE GRDATE(STRING, L)
      CHARACTER*(*) STRING
      INTEGER L
C
C Return the current date and time, in format 'dd-Mmm-yyyy hh:mm'.
C To receive the whole string, the STRING should be declared
C CHARACTER*17.
C
C Arguments:
C  STRING : receives date and time, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks. This will always be 17, unless the length
C           of the string supplied is shorter.
C--
C 19-Jan-1988
C 21-Jan-1995 Modified to work on mac with MPW Fortran 2.1
C-----------------------------------------------------------------------
      Character CDate*9, CTime*8
C
C     The Date subroutine  returns the current date in this form: DD-MMM-yy.
C     So we if yy is between 00 and 50, we assume that the first two digits are 20.
C     if yy is between 51 to 99, we assume that the first two digits are 19.
C     The Time subroutine returns the current time in this form: HH:MM:SS

      Call Date(CDate)
      Call Time(CTime)
      If ((CDate(8:9) .ge. '00') .and. (CDate(8:9) .le. '50')) Then
        String = CDate(1:7)//'20'//CDate(8:9)//' '//CTime(1:5)
      Else
        String = CDate(1:7)//'19'//CDate(8:9)//' '//CTime(1:5)
      End If
      L = MIN(17,LEN(STRING))
	  Return
      END
