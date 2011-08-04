
C*GRTRML -- get name of user's terminal (MS-DOS)
C+
      SUBROUTINE GRTRML(CTERM, LTERM)
      CHARACTER CTERM*(*)
      INTEGER   LTERM
C
C Return the device name of the user's terminal, if any.
C
C Arguments:
C  CTERM : receives the terminal name, truncated or extended with
C           blanks as necessary.
C  LTERM : receives the number of characters in CTERM, excluding
C           trailing blanks. If there is not attached terminal,
C           zero is returned.
C--
C 1989-Nov-08
C-----------------------------------------------------------------------
      CTERM = 'CON'
      LTERM = 3
      RETURN
      END
