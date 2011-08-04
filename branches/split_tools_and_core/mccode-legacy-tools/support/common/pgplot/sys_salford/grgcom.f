C*GRGCOM -- read with prompt from user's terminal (MS-DOS)
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
C
C 1995-June-1 - correction to avoid reading a zero-length string included [mlm]
C-----------------------------------------------------------------------
      INTEGER IER
C---
   11 FORMAT(A)
C---
      GRGCOM = 0
      LREAD = 0
      WRITE (*, 101, IOSTAT=IER) CPROM
  101 FORMAT(1X,A,$)
      IF (IER.EQ.0) READ (*, 11, IOSTAT=IER) CREAD
      IF (IER.EQ.0) GRGCOM = 1
      LREAD = LEN(CREAD)
   10 IF (CREAD(LREAD:LREAD).NE.' ') GOTO 20
      LREAD = LREAD-1
      IF (LREAD .GT. 0) GOTO 10
   20 CONTINUE
      END
