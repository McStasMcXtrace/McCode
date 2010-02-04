
C*GRUSER -- get user name (MS-DOS)
C+
      SUBROUTINE GRUSER(CUSER, LUSER)
      CHARACTER CUSER*(*)
      INTEGER   LUSER
C
C Return the name of the user running the program.
C
C Arguments:
C  CUSER  : receives user name, truncated or extended with
C           blanks as necessary.
C  LUSER  : receives the number of characters in VALUE, excluding
C           trailing blanks.
C--
C 1989-Mar-19 - [AFT]
C-----------------------------------------------------------------------
C
      CALL GRGENV('USER', CUSER, LUSER)
      RETURN
      END
