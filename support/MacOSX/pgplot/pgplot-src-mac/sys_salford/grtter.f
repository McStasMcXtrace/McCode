C*GRTTER -- test whether device is user's terminal (MS-DOS)
C+
      SUBROUTINE GRTTER(CDEV, QSAME)
      CHARACTER CDEV*(*)
      LOGICAL   QSAME
C
C Return a logical flag indicating whether the supplied device
C name is a name for the user's controlling terminal or not.
C (Some PGPLOT programs wish to take special action if they are
C plotting on the user's terminal.)
C
C Arguments:
C  CDEV : (input) the device name to be tested.
C  QSAME   : (output) .TRUE. is CDEV contains a valid name for the
C           user's terminal; .FALSE. otherwise.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER CTERM*64
      INTEGER   LTERM
C
      CALL GRTRML(CTERM, LTERM)
      QSAME = (CDEV.EQ.CTERM(:LTERM))
      END
