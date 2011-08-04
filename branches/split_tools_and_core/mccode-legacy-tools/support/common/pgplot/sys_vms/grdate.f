
C*GRDATE -- get date and time as character string (VMS)
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
C-----------------------------------------------------------------------
      INTEGER LIB$DATE_TIME
      INTEGER IER
C
      STRING = ' '
      L = MIN(17,LEN(STRING))
      IER = LIB$DATE_TIME(STRING(:L))
      END
