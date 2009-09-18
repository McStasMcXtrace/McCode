C*GROPTX -- open text file for input or output (VMS)
C+
      INTEGER FUNCTION GROPTX (UNIT, NAME, DEFNAM, MODE)
      INTEGER UNIT, MODE
      CHARACTER*(*) NAME, DEFNAM
C
C Input:
C  UNIT : Fortran unit number to use
C  NAME : name of file to create
C  DEFNAM : default file name (used to fill in missing fields for VMS)
C  MODE : 0 for input, 1 for output
C
C Returns:
C  0 => success; any other value => error.
C-----------------------------------------------------------------------
      INTEGER IER
      IF (MODE.EQ.1) THEN
          OPEN (UNIT=UNIT,
     1      FILE=NAME,
     2      STATUS='NEW',
     3      CARRIAGECONTROL='LIST',
     4      DEFAULTFILE=DEFNAM,
     5      RECL=2048,
     6      IOSTAT=IER)
      ELSE
          OPEN (UNIT=UNIT,
     1      FILE=NAME,
     2      STATUS='OLD',
     3      READONLY,
     6      IOSTAT=IER)
      END IF
      GROPTX = IER
C-----------------------------------------------------------------------
      END
