
C*GRGLUN -- get a Fortran logical unit number (VMS)
C+
      SUBROUTINE GRGLUN(LUN)
      INTEGER LUN
C
C Return an unused Fortran logical unit number. 
C
C Arguments:
C  LUN    : receives the logical unit number, or -1 on error.
C--
C 25-Nov-1988
C-----------------------------------------------------------------------
      INTEGER IER, LIB$GET_LUN
C
      IER = LIB$GET_LUN(LUN)
      IF (IER.NE.1) LUN = -1
      RETURN
      END
