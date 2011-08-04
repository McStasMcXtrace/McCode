
C*GRTRML -- get name of user's terminal (VMS)
C+
      SUBROUTINE GRTRML(STRING, L)
      CHARACTER*(*) STRING
      INTEGER L
C
C Return the device name of the user's terminal, if any. In VMS, the
C name of the terminal is found by translating and expanding the
C logical name TT.
C
C Arguments:
C  STRING : receives the terminal name, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks. If there is not attached terminal, 
C           zero is returned.
C--
C 19-Jan-1988
C-----------------------------------------------------------------------
      INTEGER LIB$GETDVI
      INTEGER I, IER
      EXTERNAL DVI$_FULLDEVNAM
C
      STRING = ' '
      IER = LIB$GETDVI(%LOC(DVI$_FULLDEVNAM), , 'TT:', ,
     1                 STRING, L)
      IF (IER.NE.1 .OR. L.LT.1 .OR. STRING.EQ.' ') THEN
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
