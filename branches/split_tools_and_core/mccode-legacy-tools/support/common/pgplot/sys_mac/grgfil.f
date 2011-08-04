C*GRGFIL -- find data file
C+
      SUBROUTINE GRGFIL(TYPE, NAME)
      CHARACTER*(*) TYPE, NAME
C
C This routine encsapsulates the algorithm for finding the PGPLOT
C run-time data files.
C
C 1. The binary font file: try the following in order:
C     file specified by PGPLOT_FONT
C     file in DEFFNT in directory specified by PGPLOT_DIR
C          with or without a : appended to the end.
C     file in DEFFNT in directory specified by DEFDIR
C     file in DEFFNT in current directory.
C
C 2. The color-name database: try the following in order:
C     file specified by PGPLOT_RGB
C     file in DEFRGB in directory specified by PGPLOT_DIR
C          with or without a : appended to the end.
C     file in DEFRGB in directory specified by DEFDIR
C     file in DEFRGB" in current directory.
C
C Arguments:
C  TYPE (input)  : either 'FONT' or 'RGB' to request the corresponding
C                  file.
C  NAME (output) : receives the file name.
C--
C  2-Dec-1994 - new routine [TJP].
C 21-Jan-1995 - Modified to work on Macintosh with MPW Fortran 2.1
C-----------------------------------------------------------------------
      CHARACTER*(*) DEFDIR, DEFFNT, DEFRGB
      PARAMETER  (DEFDIR=':fonts:')
      PARAMETER  (DEFFNT='grfont.dat')
      PARAMETER  (DEFRGB='rgb.txt')
      CHARACTER*255 FF
      CHARACTER*16 DEFLT
      INTEGER I, L, LD
      LOGICAL TEST, DEBUG
C
C Is debug output requested?
C
      CALL GRGENV('DEBUG', FF, L)
      DEBUG = L.GT.0
C
C Which file?
C
      IF (TYPE.EQ.'FONT') THEN
         DEFLT = DEFFNT
         LD = LEN(DEFFNT)
      ELSE IF (TYPE.EQ.'RGB') THEN
         DEFLT = DEFRGB
         LD = LEN(DEFRGB)
      ELSE
         CALL GRWARN('Internal error in routine GRGFIL')
      END IF
C
C Try each possibility in turn.
C
      DO 10 I=1,5
         IF (I.EQ.1) THEN
            CALL GRGENV(TYPE, FF, L)
         ELSE IF (I.EQ.2) THEN
            CALL GRGENV('DIR', FF, L)
            IF (L.GT.0) THEN
               FF(L+1:) = DEFLT
               L = L+LD
            END IF
         ELSE IF (I.EQ.3) THEN
            CALL GRGENV('DIR', FF, L)
            IF (L.GT.0) THEN
               FF(L+1:L+1) = ':'
               FF(L+2:) = DEFLT
               L = L+1+LD
            END IF
         ELSE IF (I.EQ.4) THEN
            FF = DEFDIR//DEFLT
            L = LEN(DEFDIR)+LD         
         ELSE IF (I.EQ.5) THEN
            FF = DEFLT
            L = LD
         END IF
         IF (L.GT.0) THEN
            IF (DEBUG) CALL GRWARN('Looking for '//FF(:L))
            INQUIRE (FILE=FF(:L), EXIST=TEST)
            IF (TEST) THEN
               NAME = FF(:L)
               RETURN
            ELSE IF (DEBUG) THEN
               CALL GRWARN('WARNING: file not found')
            END IF
         END IF
 10   CONTINUE
C
C Failed to find the file.
C
      NAME = DEFLT
C-----------------------------------------------------------------------
      END
