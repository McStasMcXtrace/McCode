
C*GRFLUN -- free a Fortran logical unit number (MS-DOS)
C+
      SUBROUTINE GRFLUN(LUN)
      INTEGER LUN
C
C Free a Fortran logical unit number allocated by GRGLUN. [This version
C is pretty stupid; GRGLUN allocates units starting at 81, and GRFLUN
C does not free units.]
C
C Arguments:
C  LUN    : the logical unit number to free.
C--
C 25-Nov-1988
C-----------------------------------------------------------------------
      RETURN
      END
