
C*GRFLUN -- free a Fortran logical unit number (VMS)
C+
      SUBROUTINE GRFLUN(LUN)
      INTEGER LUN
C
C Free a Fortran logical unit number allocated by GRGLUN. 
C
C Arguments:
C  LUN    : the logical unit number to free.
C--
C 25-Nov-1988
C-----------------------------------------------------------------------
      INTEGER LIB$FREE_LUN, JUNK
C
      JUNK = LIB$FREE_LUN(LUN)
      RETURN
      END
