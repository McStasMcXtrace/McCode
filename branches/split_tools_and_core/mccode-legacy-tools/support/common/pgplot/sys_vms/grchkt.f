C*GRCHKT -- determine whether a device is a terminal (VMS)
C+
      INTEGER FUNCTION GRCHKT (TERM)
      CHARACTER*(*) TERM
C
C Argument:
C  TERM (input): the name of the device according to VMS
C       conventions, ie a physical device name (_TTA4:) or a logical
C       name.
C
C Returns:
C  GRCHKT (integer): a TRUE value (odd) if TERM is a terminal device,
C       a FALSE value otherwise.
C--
C  1-Feb-1983
C 24-Jan-1986 - use LIB$GETDVI [TJP].
C-----------------------------------------------------------------------
      INTEGER   DC$_TERM, DVI$_DEVCLASS
      PARAMETER (DC$_TERM=66)
      PARAMETER (DVI$_DEVCLASS=4)
C
      INTEGER  DEVCLASS, IER, LIB$GETDVI
C
      IER = LIB$GETDVI(DVI$_DEVCLASS, , TERM, DEVCLASS)
      IF (IER.NE.1) THEN
          GRCHKT = IER
      ELSE IF (DEVCLASS.EQ.DC$_TERM) THEN
          GRCHKT = 1
      ELSE
          GRCHKT = 4828 ! "input device is not a terminal"
      END IF
C
      END
