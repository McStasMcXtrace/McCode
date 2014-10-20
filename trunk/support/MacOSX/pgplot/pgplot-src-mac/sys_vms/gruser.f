
C*GRUSER -- get user name (VMS)
C+
      SUBROUTINE GRUSER(STRING, L)
      CHARACTER*(*) STRING
      INTEGER L
C
C Return the name of the user running the program.
C
C Arguments:
C  STRING : receives user name, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks.
C--
C 19-Jan-1988
C-----------------------------------------------------------------------
      INTEGER LIB$GETJPI
      INTEGER I, IER, LENGTH
      EXTERNAL JPI$_USERNAME
C
      STRING = ' '
      IER = LIB$GETJPI(%LOC(JPI$_USERNAME),,,,STRING,LENGTH)
      L = MIN(LENGTH, LEN(STRING))
      IF (L.LT.1 .OR. STRING.EQ.' ') THEN
          L = 0
          STRING = ' '
      ELSE
          DO 10 I=L,1,-1
              L = I
              IF (STRING(I:I).NE.' ') GOTO 20
   10     CONTINUE
          L = 0
   20     CONTINUE
      END IF
      END
