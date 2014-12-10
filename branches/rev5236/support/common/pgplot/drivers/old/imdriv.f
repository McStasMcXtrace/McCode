      SUBROUTINE IMDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for Impress (Imagen) device. 
C-----------------------------------------------------------------------
C Version 0.9  - 1987 Aug 19 - T. J. Pearson.
C Modifications:
C REW -- 23 MAY 1988 -- Orientation from x axis, not h axis
C REW -- 25 MAY 1988 -- Change physical min/max
C			 from 3074/2324 to 3150/2400  10.5 x 8)
C REW -- 31 MAY 1988 -- Include x and y offsets to improve centering
C
C Note: this is a preliminary release. The driver has the following
C problems: (a) does not use hardware thick lines; (b) white lines do
C not to erase background as they should; (c) lines are handled as
C separate segments, instead of combining connected segments into paths,
C which should be more efficient.
C-----------------------------------------------------------------------
C
C Supported device: any Imagen printer that accepts the Impress page 
C description language.
C
C Device type code: /IMPRESS (landscape mode).
C
C Default file name: PGPLOT.IMPLOT.
C
C Default view surface dimensions:
C 10.5 inches horizontal x  8 inches vertical (landscape mode).
C Note that the Imagen laser printer prints from the bottom edge
C  of the sheet and cannot print on the top half inch of the sheet.
C
C Resolution: the driver uses coordinate increments of 1/300 inch.
C The true resolution is device-dependent.
C
C Color capability: color indices 0 (erase), and 1 (black)
C are supported. Requests for other color indices are
C converted to 1. It is not possible to change color representation. 
C
C Input capability: none.
C
C File format: binary, variable length records (max 1024 bytes); no
C carriage control.
C
C Obtaining hardcopy:  $ IMPRINT/IMPRESS file.type
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (DEFNAM='PGPLOT.IMPLOT')
      PARAMETER (TYPE='IMPRESS')
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER BUFFER
      INTEGER BUFLEV
      INTEGER UNIT, IER
      INTEGER*2 I0, I1, J0, J1, NPTS
      INTEGER GRGMEM, GRFMEM
      CHARACTER*10 MSG
      INTEGER IC
      BYTE    BUF(100), COLOR
      INTEGER NW
	INTEGER SIZEX, SIZEY			! REW -- 26MAY88
	PARAMETER (SIZEX=3150 ,SIZEY=2400)	! REW -- 26MAY88
	INTEGER OFFSETX, OFFSETY		! REW -- 31MAY88
	PARAMETER (OFFSETX=75, OFFSETY=15)      ! REW -- 31MAY88
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in IMPRESS device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = TYPE
      LCHR = LEN(TYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = SIZEX			! rew -- 25 may 1988
      RBUF(3) = 0
      RBUF(4) = SIZEY			! rew -- 25 may 1988
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C    (Nominal values)
C
   30 RBUF(1) = 300.0
      RBUF(2) = 300.0
C      (multiple strokes are spaced by 1 pixels, or 1/300 inch)
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (Hardcopy, No cursor, No dashed lines, Area fill,
C    no thick lines)
C
   40 CHR = 'HNNANNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0
      RBUF(2) = SIZEX				! rew -- 25 May 1988
      RBUF(3) = 0
      RBUF(4) = SIZEY				! rew -- 25 May 1988
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 8.0
      NBUF=1
      RETURN
C
C--- IFUNC = 8, Select plot --------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation ---------------------------------------
C
   90 CONTINUE
C     -- allocate buffer
      IER = GRGMEM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          RETURN
      END IF
C     -- open device
      CALL GRGLUN(UNIT)
      NBUF = 2
      RBUF(1) = UNIT
      OPEN (UNIT=UNIT, FILE=CHR(:LCHR), CARRIAGECONTROL='NONE',
     1      DEFAULTFILE=DEFNAM, DISPOSE='DELETE', STATUS='NEW',
     2      FORM='UNFORMATTED', RECORDTYPE='VARIABLE', IOSTAT=IER,
     3      RECL=256)
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open output file for '//TYPE//' plot: '//
     1                CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
          IER = GRFMEM(BUFSIZ, BUFFER)
          RETURN
      ELSE
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = LEN(CHR)
   91     IF (CHR(LCHR:LCHR).EQ.' ') THEN
              LCHR = LCHR-1
              GOTO 91
          END IF
          RBUF(2) = 1
      END IF
      IC = 1
C     -- initialization
      NPTS = 0
      COLOR = 15
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CLOSE (UNIT, DISPOSE='KEEP')
      CALL GRFLUN(UNIT)
      IER = GRFMEM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRWARN('Error deallocating plot buffer.')
          CALL GRGMSG(IER)
      END IF
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
C     -- set coordinate system
      BUF(1) = 205 ! SET_HV_SYSTEM
      BUF(2) = 29  ! 0 0 3 5			! REW -- 23 MAY 1988
      BUF(3) = 135 ! SET_ABS_H
      BUF(4) = 0
      BUF(5) = 0
      BUF(6) = 137 ! SET_ABS_V
      BUF(7) = 0
      BUF(8) = 0
      NW = 8
      GOTO 1000
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      IF (IC.EQ.0) RETURN
      I0 = OFFSETX + NINT(RBUF(1))
      J0 = OFFSETY + NINT(RBUF(2))
      I1 = OFFSETX + NINT(RBUF(3))
      J1 = OFFSETY + NINT(RBUF(4))
  125 CONTINUE
      BUF(1) = 230 ! CREATE_PATH
      CALL GRIM00(BUF(2), 2) ! 2 vertices
      CALL GRIM00(BUF(4), I0) ! coordinates of vertices
      CALL GRIM00(BUF(6), J0)
      CALL GRIM00(BUF(8), I1)
      CALL GRIM00(BUF(10), J1)
      BUF(12) = 234 ! DRAW_PATH
      BUF(13) = COLOR	! black or white
      NW = 13
      GOTO 1000
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      IF (IC.EQ.0) RETURN
      I0 = OFFSETX + NINT(RBUF(1))
      J0 = OFFSETY + NINT(RBUF(2))
      I1 = I0
      J1 = J0
      GOTO 125
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      BUF(1) = 219 ! ENDPAGE
      NW = 1
      GOTO 1000
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = RBUF(1)
      IF (IC.LT.0 .OR. IC.GT.1) THEN
          IC = 1
          RBUF(1) = IC
      END IF
      COLOR = 15
      IF (IC.EQ.0) COLOR = 0
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRIM03(%val(BUFFER), UNIT, BUFLEV)
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           Not implemented.
C
  170 CONTINUE
      GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Not implemented: no alpha screen)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called)
C
  190 CONTINUE
      GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      IF (NPTS.EQ.0) THEN
          NPTS = RBUF(1)
          BUF(1) = 230 ! CREATE_PATH
          CALL GRIM00(BUF(2), NPTS) ! # vertices
          NW = 3
      ELSE
          NPTS = NPTS-1
	  I0 = OFFSETX + NINT(RBUF(1))
	  J0 = OFFSETY + NINT(RBUF(2))
          CALL GRIM00(BUF(1), I0) ! coordinates of vertex
          CALL GRIM00(BUF(3), J0)
          NW = 4
	  IF (NPTS.EQ.0) THEN
              BUF(5) = 233 ! FILL_PATH
              BUF(6) = COLOR	! black or white
              NW = 6
	  END IF
      END IF
      GOTO 1000
C
C--- IFUNC=21, Set color representation. -------------------------------
C    (Not implemented: ignored)
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called)
C
  220 CONTINUE
      GOTO 900
C
C--- IFUNC=23, Escape --------------------------------------------------
C    (Not implemented: ignored)
C
  230 CONTINUE
      RETURN
C
C--- Send the command. -------------------------------------------------
C
 1000 CALL GRIM02(BUF,NW,%val(BUFFER),BUFLEV,UNIT)
C-----------------------------------------------------------------------
      END

C*GRIM00 -- PGPLOT Impress driver, write word
C+
      SUBROUTINE GRIM00(BUF,WORD)
      BYTE BUF(2), WORD(2)
C--
      BUF(1) = WORD(2)
      BUF(2) = WORD(1)
      END

C*GRIM02 -- PGPLOT Impress driver, transfer data to buffer
C+
      SUBROUTINE GRIM02 (INSTR, N, BUFFER, HWM, UNIT)
      INTEGER   N, HWM, UNIT
      BYTE      INSTR(*), BUFFER(*)
C
C Arguments:
C  INSTR  (input)  : text of instruction (bytes).
C  N      (input)  : number of bytes to transfer.
C  BUFFER (input)  : output buffer.
C  HWM    (in/out) : number of bytes used in BUFFER.
C  UNIT   (input)  : channel number for output (when buffer is full).
C
C Subroutines called:
C   GRIM03
C-----------------------------------------------------------------------
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER  I
C-----------------------------------------------------------------------
      IF (HWM+N.GE.BUFSIZ) CALL GRIM03(BUFFER, UNIT, HWM)
      DO 10 I=1,N
          HWM = HWM + 1
          BUFFER(HWM) = INSTR(I)
   10 CONTINUE
C-----------------------------------------------------------------------
      END

C*GRIM03 -- PGPLOT Impress driver, copy buffer to file
C+
      SUBROUTINE GRIM03 (BUFFER, UNIT, N)
      BYTE BUFFER(*)
      INTEGER UNIT, N
C
C Arguments:
C   BUFFER (input) address of buffer to be output
C   UNIT   (input) unit number for output
C   N      (input) number of bytes to transfer
C          (output) set to zero
C-----------------------------------------------------------------------
      INTEGER J
C-----------------------------------------------------------------------
      IF (N.GT.0) WRITE (UNIT) (BUFFER(J),J=1,N)
      N = 0
C-----------------------------------------------------------------------
      END
