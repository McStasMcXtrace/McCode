C*GRDATE -- get date and time as character string (Alliant-UNIX)
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
C 28-Jul-1988
C 23-Oct-1989 ALF. Conforms to Alliant use of FDATE.
C-----------------------------------------------------------------------
      CHARACTER*24 UTIME, FDATE
      CHARACTER*17 VTIME
C
      UTIME = FDATE()
      VTIME(1:2) = UTIME(9:10)
      VTIME(3:3) = '-'
      VTIME(4:6) = UTIME(5:7)
      VTIME(7:7) = '-'
      VTIME(8:11) = UTIME(21:24)
      VTIME(12:12) = ' '
      VTIME(13:17) = UTIME(12:16)
      STRING = VTIME
      L = MIN(17, LEN(STRING))
      END
