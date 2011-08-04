C*DOCUMENT
C+
      PROGRAM DOCUME
C
C DOCUMENT extracts documentation from Fortran source code files. 
C Documentation information is flagged by special characters in
C columns 1--3:
C
C C+    Start of documentation block (this line
C       is not part of the block)
C
C C--   End of documentation block (this line
C       is not part of the block)
C
C C*    Start of a new module; the rest of line 
C       (up to first blank) is the module name. If no such delimiter
C       is encountered, the module name is the file name (excluding
C       disk, directory, type, version, etc.)
C
C Usage: 
C  DOCUMENT uses three control parameters:
C   INPUT:    the name of the input disk file(s), which may include
C             VMS wild-cards; must be specified.
C   LISTFILE: the name for the output listing; default |SYS$OUTPUT|.
C   TYPE:     specify the format for the output file as
C             `LIST' (default)
C             `INDEX' for index of modules only
C             `HELP' for a VMS help file
C             `TEX' for TeX code.
C
C History:
C  1.0:  1985 May 21 -- (T. J. Pearson, VAX-11 Fortran).
C  1.1:  1985 Oct  8 -- change delimiters to VLBA standard.
C  1.2:  1985 Oct  8 -- add TYPE parameter.
C  1.4:  1987 May 19 -- add TeX option.
C  1.5:  1987 Nov 12 -- add INDEX option.
C  2.0:  1992 Jun 16 -- remove use of Keyin.
C 
C Subroutines required:
C  |LIB$FIND_FILE|    (VMS) wild-card expansion.
C  |LIB$SYS_GETMSG|   (VMS) system error message.
C----------------------------------------------------------------------
C
C Constants.
C
      CHARACTER*(*) VERSN
      INTEGER    INC,OUTC,INDAT,PR
      INTEGER    RMS$_NMF
      PARAMETER  (VERSN='2.0 - 1992 Jun 16')
      PARAMETER  (INC=5,OUTC=6,INDAT=1,PR=2) 
C            ! I/O unit numbers
      PARAMETER  (RMS$_NMF = '000182CA'X)
C            ! VMS code <no more files found> 
C
C Variables.
C
      CHARACTER  INPFIL*128, INDSN*128, LISFIL*128
      CHARACTER  TEXT*128, TYPE*16, DEFNAM*32
      CHARACTER  LFMT*16
      INTEGER    L1, LINP, LLIS, NFIL
      INTEGER    CONTXT, I, IER, J, L, LDSN, MODE, NREC
      LOGICAL    PROPEN, COPY, HELP, TEX, INDX
C
C External functions.
C
      INTEGER    LIB$FIND_FILE, LEN1
      LOGICAL    ISTERM
C
C Formats.
C
  600 FORMAT(' DOCUMENT extracts documentation from source code'/
     1       ' (Version ',A,')'/)
  605 FORMAT(' Control parameters (INPUT, LISTFILE, TYPE;',
     1       ' end with /):')
  610 FORMAT(1X,A,T20,A)
  620 FORMAT(72('-'))
  650 FORMAT(' ++WARNING++ No documentation found in input file(s).')
C-----------------------------------------------------------------------
C
C Introduction.
C
      WRITE (OUTC,600) VERSN
C
C Control parameters.
C
      WRITE (OUTC, '('' Input file:  '',$)')
      READ (INC, '(A)', ERR=80, END=80) INPFIL
      LINP = LEN1(INPFIL)
      WRITE (OUTC, '('' Output file: '',$)')
      READ (INC, '(A)', ERR=80, END=80) LISFIL
      LLIS = LEN1(LISFIL)
      WRITE (OUTC, '('' Type (LIST, INDEX, HELP, TEX): '',$)')
      READ (INC, '(A)', ERR=80, END=80) TYPE
      HELP = TYPE(1:1).EQ.'H' .OR. TYPE(1:1).EQ.'h'
      TEX  = TYPE(1:1).EQ.'T' .OR. TYPE(1:1).EQ.'t'
      INDX = TYPE(1:1).EQ.'I' .OR. TYPE(1:1).EQ.'i'
      IF (HELP) THEN
          DEFNAM = 'DOCUMENT.HLP'
          LFMT = '(2X,A)'
      ELSE IF (TEX) THEN
          DEFNAM = 'DOCUMENT.TEX'
          LFMT = '(A)'
      ELSE
          DEFNAM = 'DOCUMENT.DOC'
          LFMT = '(A)'
      END IF
C
C Expand wild cards and open input file.
C
      PROPEN = .FALSE.
      CONTXT = 0
      NFIL = 0
   20 NFIL = NFIL+1
      IER = LIB$FIND_FILE(INPFIL(1:LINP),INDSN,CONTXT)
      IF (IER.EQ.RMS$_NMF) GOTO 80
      IF (MOD(IER,2).NE.1) THEN
          CALL LIB$SYS_GETMSG(IER,L,TEXT)
          WRITE (OUTC,'(1X,A)') TEXT(1:L)
          GOTO 80
      END IF
      OPEN (UNIT=INDAT, FILE=INDSN, STATUS='OLD', READONLY, 
     1      FORM='FORMATTED', ERR=80)
      INQUIRE (UNIT=INDAT, NAME=INDSN)
      LDSN = MIN(132,LEN1(INDSN))
C
C Open output file (first time only).
C
      IF (.NOT.PROPEN) THEN
          OPEN (UNIT=PR, FILE=LISFIL(1:LLIS), STATUS='NEW',
     1          CARRIAGECONTROL='LIST', IOSTAT=IER,
     2          DEFAULTFILE=DEFNAM, FORM='FORMATTED')
          IF (IER.NE.0) THEN
            WRITE (OUTC,'(1X,2A)') 'Cannot open LISTFILE, ',
     1                  LISFIL(1:LLIS)
            CLOSE (UNIT=INDAT)
            GOTO 80
          END IF
          PROPEN = .TRUE.
      END IF
C
C Write out file name.
C
      IF (INDX) THEN
          WRITE (PR, '(/A/)') INDSN(1:LDSN)
      END IF
C
C Read input file and look for flags.
C
      NREC = 0
      COPY = .FALSE.
   30 READ (INDAT,END=70,ERR=70, FMT='(Q,A)') L,TEXT
      IF (L.GE.3 .AND. TEXT(1:3).EQ.'C--') THEN
          IF (COPY.AND.TEX) WRITE (UNIT=PR, FMT='(A)') '\endtt}'
          COPY = .FALSE.
      END IF
      IF (COPY) THEN
          L1 = 1
          IF (TEXT(1:2).EQ.'C ') L1 = 3
          IF (L1.GT.L) L = L1
          WRITE (UNIT=PR, FMT=LFMT) TEXT(L1:L)
      END IF
      IF (L.GE.2 .AND. TEXT(1:2).EQ.'C+') THEN
          IF ((.NOT.COPY).AND.TEX) 
     1        WRITE (UNIT=PR, FMT='(A)') '{\eightpoint\begintt'
          COPY = .TRUE. .AND. (.NOT.INDX)
      END IF
      IF (L.GT.2 .AND. TEXT(1:2).EQ.'C*') THEN
           IF (HELP) THEN
               WRITE (UNIT=PR, FMT='(I1,1X,A)') 2,TEXT(3:L)
           ELSE IF (TEX) THEN
               WRITE (UNIT=PR, FMT='(/3A)') '\module{',TEXT(3:L),'}'
           ELSE IF (INDX) THEN
               WRITE (UNIT=PR, FMT='(A)') TEXT(3:L)
           ELSE
               WRITE (UNIT=PR, FMT='(/)')
               WRITE (UNIT=PR, FMT=620)
               WRITE (UNIT=PR, FMT='(2A)') 'Module: ',TEXT(3:L)
*              WRITE (UNIT=PR, FMT='(2A)') 'File:   ',INDSN(1:LDSN)
               WRITE (UNIT=PR, FMT=620)
               WRITE (UNIT=PR, FMT='(:)')
           END IF
      END IF
      NREC = NREC+1
      GOTO 30
C
C End of file: go and find another one.
C
   70 CLOSE (UNIT=INDAT)
      GOTO 20
C
C End of job: close output file if open, else give message.
C
   80 IF (PROPEN) THEN
          CLOSE (UNIT=PR)
      ELSE
          WRITE (UNIT=OUTC,FMT=650)
      END IF
C
      END

C*LEN1 -- length of string excluding trailing blanks
C+
      INTEGER FUNCTION LEN1(S)
      CHARACTER*(*) S
C
C Find the length of a character string excluding trailing blanks.
C A blank string returns a value of 0.
C
C Argument:
C  S      (input)  : character string.
C
C Returns:
C  LEN1            : number of characters in S, excluding trailing
C                    blanks, in range 0...LEN(S). A blank string
C                    returns a value of 0.
C
C Subroutines required:
C  None
C
C Fortran 77 extensions:
C  None
C
C History:
C  1987 Nov 12 - TJP.
C-----------------------------------------------------------------------
      INTEGER  I
C
      IF (S.EQ.' ') THEN
          LEN1 = 0
      ELSE
          DO 10 I=LEN(S),1,-1
              LEN1 = I
              IF (S(I:I).NE.' ') GOTO 20
   10     CONTINUE
          LEN1 = 0
   20     CONTINUE
      END IF
      END
