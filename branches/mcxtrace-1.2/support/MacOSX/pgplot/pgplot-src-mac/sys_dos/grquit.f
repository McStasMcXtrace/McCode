
C*GRQUIT -- report a fatal error and abort execution (MS-DOS)
C+
      SUBROUTINE GRQUIT (CTEXT)
      CHARACTER CTEXT*(*)
C
C Report a fatal error (via GRWARN) and exit with fatal status; a
C traceback is generated unless the program is linked /NOTRACE.
C
C Argument:
C  CTEXT (input): text of message to be sent to GRWARN.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CALL GRWARN(CTEXT)
      STOP 'Fatal error in PGPLOT library'
      END
