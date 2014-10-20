C*GRSY00 -- initialize font definition
C+
      SUBROUTINE GRSY00
C
C This routine must be called once in order to initialize the tables
C defining the symbol numbers to be used for ASCII characters in each
C font, and to read the character digitization from a file.
C
C Arguments: none.
C
C Implicit input:
C  The file with name specified in environment variable PGPLOT_FONT
C  is read, if it is available.
C  This is a binary file containing two arrays INDFON and BUFFER.
C  The digitization of each symbol occupies a number of words in
C  the INTEGER*2 array BUFFER; the start of the digitization
C  for symbol number N is in BUFFER(INDFON(N)), where INDFON is an
C  integer array of 3000 elements. Not all symbols 1...3000 have
C  a representation; if INDFON(N) = 0, the symbol is undefined.
C
*  PGPLOT uses the Hershey symbols for two `primitive' operations:
*  graph markers and text.  The Hershey symbol set includes several
*  hundred different symbols in a digitized form that allows them to
*  be drawn with a series of vectors (polylines).
*
*  The digital representation of all the symbols is stored in common
*  block /GRSYMB/.  This is read from a disk file at run time. The
*  name of the disk file is specified in environment variable
*  PGPLOT_FONT.
*
* Modules:
*
* GRSY00 -- initialize font definition
* GRSYDS -- decode character string into list of symbol numbers
* GRSYMK -- convert marker number into symbol number
* GRSYXD -- obtain the polyline representation of a given symbol
*
* PGPLOT calls these routines as follows:
*
* Routine          Called by
*
* GRSY00          GROPEN
* GRSYDS          GRTEXT, GRLEN
* GRSYMK          GRMKER,
* GRSYXD          GRTEXT, GRLEN, GRMKER
***********************************************************************
C--
C (2-Jan-1984)
C 22-Jul-1984 - revise to use DATA statements [TJP].
C  5-Jan-1985 - make missing font file non-fatal [TJP].
C  9-Feb-1988 - change default file name to Unix name; overridden
C               by environment variable PGPLOT_FONT [TJP].
C 29-Nov-1990 - move font assignment to GRSYMK.
C-----------------------------------------------------------------------
      CHARACTER*(*) UNIX
      PARAMETER  (UNIX='/usr/local/vlb/pgplot/grfont.dat')
      INTEGER MAXCHR
      PARAMETER (MAXCHR=3000)
C
      CHARACTER*128 FF
      INTEGER    FNTFIL, I, IER, IREC, IS, K, NC3
      INTEGER    L
C
      INTEGER*2  BUFFER(27000)
      INTEGER    INDFON(3000), NC1, NC2
      COMMON     /GRSYMB/ NC1, NC2, INDFON, BUFFER
C
C Read the font file. If an I/O error occurs, it is ignored; the
C effect will be that all symbols will be undefined (treated as
C blank spaces).
C
      CALL GRGENV('FONT', FF, L)
      IF (L.EQ.0) THEN
          FF = UNIX
          L = LEN(UNIX)
      END IF
      CALL GRGLUN(FNTFIL)
      IF(INDEX(FF(:L),'dat').GT.0) THEN
         OPEN (UNIT=FNTFIL, FILE=FF(:L), FORM='UNFORMATTED',
     2      STATUS='OLD', IOSTAT=IER)
         IF (IER.EQ.0) READ (UNIT=FNTFIL, IOSTAT=IER)
     1            NC1,NC2,NC3,INDFON,BUFFER
         IF (IER.EQ.0) CLOSE (UNIT=FNTFIL, IOSTAT=IER)
      ELSE
         OPEN (UNIT=2, STATUS='OLD', FILE=FF(:L),
     :      ACCESS='DIRECT', RECL=MAXCHR/4)
         IREC=0
         IS=1
         READ(2, REC=IREC+1) (INDFON(K),K=IS,IS+MAXCHR/4-1)
         IREC=IREC+1
         IF(INDFON(1).NE.123) THEN
            CALL GRWARN('Bad magic number in font file.')
            IER=1
            GOTO 190
         END IF
         NC1=INDFON(2)
         NC2=INDFON(3)
         IS=1
         DO 140 I=1,4
            READ(2, REC=IREC+1) (INDFON(K),K=IS,IS+MAXCHR/4-1)
            IREC=IREC+1
            IS=IS+MAXCHR/4
  140    CONTINUE
         IS=1
         DO 160 I=1,18
            READ(2, REC=IREC+1) (BUFFER(K),K=IS,IS+MAXCHR/2-1)
            IREC=IREC+1
            IS=IS+MAXCHR/2
  160    CONTINUE
      END IF
C
  190 CONTINUE
      CALL GRFLUN(FNTFIL)
      IF (IER.NE.0) CALL GRWARN('Unable to read font file: '//FF(:L))
      RETURN
      END
