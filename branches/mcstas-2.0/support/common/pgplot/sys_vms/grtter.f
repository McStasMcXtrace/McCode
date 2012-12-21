
C*GRTTER -- test whether device is user's terminal (VMS)
C+
      SUBROUTINE GRTTER(STRING, SAME)
      CHARACTER*(*) STRING
      LOGICAL SAME
C
C Return a logical flag indicating whether the supplied device
C name is a name for the user's controlling terminal or not.
C (Some PGPLOT programs wish to take special action if they are
C plotting on the user's terminal.)
C
C Arguments:
C  STRING : (input) the device name to be tested.
C  SAME   : (output) .TRUE. is STRING contains a valid name for the
C           user's terminal; .FALSE. otherwise.
C--
C 9-Feb-1988
C-----------------------------------------------------------------------
      INTEGER LIB$GETDVI
      INTEGER IER1, IER2, L1, L2
      CHARACTER*255 DEV1, DEV2
      EXTERNAL DVI$_FULLDEVNAM
C
      IER1 = LIB$GETDVI(%LOC(DVI$_FULLDEVNAM), , STRING, ,
     1                  DEV1, L1)
      IER2 = LIB$GETDVI(%LOC(DVI$_FULLDEVNAM), , 'TT:', ,
     1                  DEV2, L2)
      SAME = (IER1.EQ.1) .AND. (IER2.EQ.1) .AND.
     1       (L1.EQ.L2) .AND. (DEV1(:L1).EQ.DEV2(:L2))
      END
