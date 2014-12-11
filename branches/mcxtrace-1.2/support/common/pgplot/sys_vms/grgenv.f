
C*GRGENV -- get value of PGPLOT environment parameter (VMS)
C+
      SUBROUTINE GRGENV(NAME, VALUE, L)
      CHARACTER*(*) NAME, VALUE
      INTEGER L
C
C Return the value of a PGPLOT environment parameter. In VMS,
C environment parameters are VMS logical names; e.g. parameter
C ENVOPT is logical name PGPLOT_ENVOPT. Translation is not
C recursive and is case-sensitive.
C [For historical compatibility, if name PGPLOT_XX is not found,
C this routine will also look for PLT$XX.]
C
C Arguments:
C  NAME   : (input) the name of the parameter to evaluate.
C  VALUE  : receives the value of the parameter, truncated or extended
C           with blanks as necessary. If the parameter is undefined,
C           a blank string is returned.
C  L      : receives the number of characters in VALUE, excluding
C           trailing blanks. If the parameter is undefined, zero is
C           returned.
C--
C 19-Jan-1988
C-----------------------------------------------------------------------
      INTEGER I, LIN, IER, LIB$SYS_TRNLOG
      CHARACTER*32 TEST
C
      TEST = 'PGPLOT_'//NAME
      LIN = INDEX(TEST, ' ')-1
      IER = LIB$SYS_TRNLOG(TEST(:LIN),L,VALUE)
      IF (IER.NE.1) THEN
          TEST = 'PLT$'//NAME
          LIN = INDEX(TEST, ' ')-1
          IER = LIB$SYS_TRNLOG(TEST(:LIN),L,VALUE)
      END IF
      IF (IER.NE.1 .OR. L.LT.1 .OR. VALUE.EQ.' ') THEN
          L = 0
          VALUE = ' '
      ELSE
          DO 10 I=L,1,-1
              L = I
              IF (VALUE(I:I).NE.' ') GOTO 20
   10     CONTINUE
          L = 0
   20     CONTINUE
      END IF
      END
