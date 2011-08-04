      PROGRAM EXPGMF
C-----------------------------------------------------------------------
C This is a simple program to examine the pictures contained in a
C PGPLOT metafile, displaying them one at a time on a selected PGPLOT
C device. Options are provided for converting color pictures to
C monochrome or grey scale.
C-----------------------------------------------------------------------
      CHARACTER*80 FILE
      CHARACTER*8 OPT
      INTEGER I, ISTAT, NPICT, PGOPEN
C
      FILE = 'pgplot.pgmf'
C
C Scan the metafile to find number of pictures
C
      CALL PGEXMF(FILE, NPICT, ISTAT)
      WRITE (*,*) 'Number of pictures in metafile:', NPICT
C
C Open output graphics device
C
      ISTAT = PGOPEN('?')
      IF (ISTAT.LT.1) STOP
      CALL PGASK(.FALSE.)
C
C Display requested picture(s)
C
      WRITE (*,*) 'Options (M for monochrome, G for grey scale):'
      READ (*, '(A)') OPT
 10   WRITE (*,*) 'Enter number of picture to display: '
      READ (*,*, END=20) I
      IF (I.LT.1) GOTO 20
      IF (I.GT.NPICT) GOTO 10
      CALL PGPAGE
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGRDMF(FILE, OPT, I, ISTAT)
      GOTO 10
C
C Close output graphics device
C
 20   CALL PGCLOS
C-----------------------------------------------------------------------
      END

C*PGRDMF -- read and display a picture from a PGPLOT metafile
C%void cpgrdmf(char *file, char *opt, int npict, int *istat);
C+
      SUBROUTINE PGRDMF (FILE, OPT, NPICT, ISTAT)
      CHARACTER*(*) FILE, OPT
      INTEGER NPICT, ISTAT
C
C This routine reads a PGPLOT metafile from a disk file and displays
C it in the current viewport.
C
C Arguments:
C  FILE   (input)  : name of metafile to read
C  OPT    (input)  : string of single-character options (see below)
C  NPICT  (input)  : sequence number of picture to display
C  ISTAT  (output) : receives 0 if file is read successfully; >0 if
C                    an error occurs (e.g., file not found, wrong
C                    format)
C
C Options:
C  M : display in monochrome, using color indices 0 and 1;
C      all color information in the metafile will be ignored.
C  G : display in grey scale: colors in the metafile will be
C      converted to shades of grey.
C  D : debug: report unrecognized entries in the metafile.
C--
C 3-Jun-1997 - new routine (TJP).
C-----------------------------------------------------------------------
      INTEGER MAXPOL
      PARAMETER (MAXPOL=1000)
      INTEGER UNIT, IER, I, N1, N2, N3, N4, NPTS, NFND, LW, N, CI
      INTEGER GROPTX, GRCTOI
      LOGICAL MONO, GREY, DEBUG
      REAL    X, Y, X1, Y1, X2, Y2, SCALE, POLX(MAXPOL), POLY(MAXPOL)
      REAL    SHADE
      CHARACTER REC*80, OP
C
      MONO = INDEX(OPT,'M').NE.0 .OR. INDEX(OPT,'m').NE.0
      GREY = INDEX(OPT,'G').NE.0 .OR. INDEX(OPT,'g').NE.0
      DEBUG = INDEX(OPT,'D').NE.0 .OR. INDEX(OPT,'d').NE.0
C
C Open file and check that it is a PGPLOT metafile
C
      CALL GRGLUN(UNIT)
      IER = GROPTX(UNIT, FILE, 'pgplot.pgmf', 0)
      IF (IER.NE.0) THEN
         ISTAT = 1
         CALL GRWARN('Cannot open PGPLOT metafile:')
         CALL GRWARN(FILE(1:LEN(FILE)))
         CALL GRFLUN(UNIT)
         RETURN
      END IF
      READ (UNIT, '(A)', IOSTAT=IER) REC
      IF (IER.NE.0 .OR. REC(1:5).NE.'%PGMF') THEN
         ISTAT = 2
         CALL GRWARN('File is not a PGPLOT metafile:')
         CALL GRWARN(FILE(1:LEN(FILE)))
         CLOSE (UNIT=UNIT)
         CALL GRFLUN(UNIT)
         RETURN
      END IF
C
C Skip to start of requested picture
C
      N = 0
 50   READ (UNIT, '(A)', IOSTAT=IER) REC
      IF (IER.NE.0) THEN
         ISTAT = 3
         CALL GRWARN('Requested picture not found in PGPLOT metafile:')
         CALL GRWARN(FILE(1:LEN(FILE)))
         CLOSE (UNIT=UNIT)
         CALL GRFLUN(UNIT)
         RETURN
      END IF
      IF (REC(1:1).NE.'B') GOTO 50
      N = N+1
      IF (N.LT.NPICT) GOTO 50
C
C Display this picture
C
      CALL PGBBUF
      CALL PGSAVE
 100  CONTINUE
         OP = REC(1:1)
         I = 2
         N1 = GRCTOI(REC, I)
         I = I+1
         N2 = GRCTOI(REC, I)
         I = I+1
         N3 = GRCTOI(REC, I)
         I = I+1
         N4 = GRCTOI(REC, I)
         IF (OP.EQ.'L') THEN
C           -- line segment
            X = X + REAL(N1)
            Y = Y + REAL(N2)
            CALL PGDRAW(X, Y)
         ELSE IF (OP.EQ.'M') THEN
C           -- move pen
            X = REAL(N1)
            Y = REAL(N2)
            CALL PGMOVE(X, Y)
         ELSE IF (OP.EQ.'D') THEN
C           -- dot
            X = REAL(N1)
            Y = REAL(N2)
            CALL PGPT1(X, Y, -1)
         ELSE IF (OP.EQ.'S') THEN
C           -- marker
            X = REAL(N2)
            Y = REAL(N3)
            CALL PGPT1(X, Y, N1)
         ELSE IF (OP.EQ.'I') THEN
C           -- set color index
            IF (MONO) THEN
               CI = 1
               IF (N1.EQ.0) CI = 0
            ELSE
               CI = N1
            END IF
            CALL PGSCI(CI)
         ELSE IF (OP.EQ.'W') THEN
C           -- set line width
C              (N1 is width in PGMF units; convert to unit 1/200 inch)
            LW = NINT(200.0*N1/SCALE)
            IF (LW.LT.1) LW = 1
            CALL PGSLW(LW)
         ELSE IF (OP.EQ.'Y') THEN
C           -- begin polygon
            NPTS = N1
            NFND = 0
         ELSE IF (OP.EQ.'X') THEN
C           -- polygon vertex
            NFND = NFND +1
            IF (NFND.LE.MAXPOL) THEN
               POLX(NFND) = REAL(N1)
               POLY(NFND) = REAL(N2)
            END IF
            IF (NFND.EQ.NPTS) THEN
               CALL PGPOLY(MIN(NPTS,MAXPOL), POLX, POLY)
               NPTS = 0
            END IF
         ELSE IF (OP.EQ.'R') THEN
C           -- rectangle
            CALL PGRECT(REAL(N1), REAL(N3), REAL(N2), REAL(N4))
         ELSE IF (OP.EQ.'C') THEN
C           -- set color representation
            IF (MONO) THEN
               CONTINUE
            ELSE IF (GREY) THEN
               SHADE = (0.30*N2 + 0.59*N3 + 0.11*N4)/255.0
               CALL PGSCR(N1, SHADE, SHADE, SHADE)
            ELSE
               CALL PGSCR(N1, 
     :           REAL(N2)/255.0, REAL(N3)/255.0, REAL(N4)/255.0)
            END IF
         ELSE IF (OP.EQ.'B') THEN
C           -- begin picture
            CALL PGWNAD(0.0, REAL(N2), 0.0, REAL(N3))
            X = 0.0
            Y = 0.0
C           -- find device scale (PGMF units per inch)
            CALL PGQVP(1, X1, X2, Y1, Y2)
            SCALE = REAL(N2)/(X2-X1)
         ELSE IF (OP.EQ.'E') THEN
C           -- end picture
            GOTO 200
         ELSE IF (REC(1:1).EQ.'%') THEN
C           -- comment
            CONTINUE
         ELSE
            IF (DEBUG) THEN
               CALL GRWARN('Bad entry in metafile: '//REC(1:16))
            END IF
         END IF
         READ (UNIT, '(A)', IOSTAT=IER) REC
         IF (IER.NE.0) GOTO 200
      GOTO 100
 200  CONTINUE
      CALL PGUNSA
      CALL PGEBUF
      CLOSE (UNIT=UNIT)
      CALL GRFLUN(UNIT)
      RETURN
      END

C*PGEXMF -- determine properties of PGPLOT metafile
C%void cpgexmf(char *file, int *npict, int *istat);
C+
      SUBROUTINE PGEXMF (FILE, NPICT, ISTAT)
      CHARACTER*(*) FILE
      INTEGER NPICT, ISTAT
C
C Arguments:
C  FILE   (input)  : name of metafile to read
C  NPICT  (output) : number of pictures in metafile
C  ISTAT  (output) : receives 0 if file is read successfully; 1 if
C                    an error occurs (e.g., file not found, wrong
C                    format)
C--
C 3-Jun-1997 - new routine (TJP).
C-----------------------------------------------------------------------
      INTEGER UNIT, IER
      INTEGER GROPTX
      CHARACTER REC*80
C
      CALL GRGLUN(UNIT)
      IER = GROPTX(UNIT, FILE, 'pgplot.pgmf', 0)
      IF (IER.NE.0) THEN
         ISTAT = 1
         CALL GRWARN('Cannot open PGPLOT metafile:')
         CALL GRWARN(FILE(1:LEN(FILE)))
         CALL GRFLUN(UNIT)
         RETURN
      END IF
      READ (UNIT, '(A)', IOSTAT=IER) REC
      IF (IER.NE.0 .OR. REC(1:5).NE.'%PGMF') THEN
         ISTAT = 2
         CALL GRWARN('File is not a PGPLOT metafile:')
         CALL GRWARN(FILE(1:LEN(FILE)))
         CLOSE (UNIT=UNIT)
         CALL GRFLUN(UNIT)
         RETURN
      END IF
      NPICT = 0
 100  CONTINUE
         READ (UNIT, '(A)', IOSTAT=IER) REC
         IF (IER.NE.0) GOTO 200
         IF (REC(1:1).EQ.'B') THEN
            NPICT = NPICT+1
         END IF
      GOTO 100
 200  CONTINUE
      CLOSE (UNIT=UNIT)
      CALL GRFLUN(UNIT)
      RETURN
      END

