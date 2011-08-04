C*VEDRIV -- PGPLOT Versatec driver
C+
      SUBROUTINE VEDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for Versatec device.
C-----------------------------------------------------------------------
C Version 1.0 - 1987 Jun 11 - T. J. Pearson.
C Version 1.1 - 1987 Jul 6  - minor changes (TJP).
C Version 1.2 - 1987 Aug 19 - add RECL= (TJP).
C Version 1.3 - 1987 Sep 1  - correct 'misc defaults' (TJP).
C Version 2.0 - 1995 Feb 9  - merge landscape and portrait modes (TJP).
C-----------------------------------------------------------------------
C
C Supported device: Versatec V80 dot-matrix printer.
C
C Device type code: /VERSATEC  (landscape orientation)
C                   /VVERSATEC (portrait orientation)
C
C Default device name: pgplot.veplot
C
C Default view surface dimensions: 10.5in (horizontal) by 8.0in
C (vertical) in landscape mode, 8.0 by 10.5 in portrait mode.
C
C Resolution: 200 (x) x 200 (y) pixels/inch.
C
C Color capability: Color indices 0 (erase, white) and 1 (black) are
C supported. It is not possible to change color representation.
C
C Input capability: None.
C
C File format: Variable-length records, maximum 264 bytes, with
C embedded carriage-control characters. A full-page plot occupies
C 832 512-byte blocks.
C
C Obtaining hardcopy: Use the command VMS PRINT/PASSALL.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPEL, TYPEP, DEFNAM
      PARAMETER (TYPEL='VERSATEC')
      PARAMETER (TYPEP='VVERSATEC')
      PARAMETER (DEFNAM='pgplot.veplot')
      CHARACTER FF
      PARAMETER (FF=CHAR(12))
C
      INTEGER UNIT, IER, IC, BX, BY, NPICT
      INTEGER GRGMEM, GRFMEM
      CHARACTER*10 MSG
      INTEGER BITMAP
      REAL    XBUF(4)
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in Versatec device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 IF (MODE.EQ.1) THEN
         CHR = TYPEL
         LCHR = LEN(TYPEL)
      ELSE IF (MODE.EQ.2) THEN
         CHR = TYPEP
         LCHR = LEN(TYPEP)
      ELSE
         CHR = 'Unknown'
         LCHR = 7
      END IF
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(3) = 0
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      IF (MODE.EQ.1) THEN
         RBUF(2) = 2099
         RBUF(4) = -1
      ELSE
         RBUF(2) = -1
         RBUF(4) = 2099
      END IF
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 200.0
      RBUF(2) = 200.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CHR = 'HNNNNNNNNN'
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
      RBUF(2) = 2099
      RBUF(3) = 0
      RBUF(4) = 1599
      IF (MODE.EQ.1) THEN
         RBUF(2) = 2099
         RBUF(4) = 1599
      ELSE
         RBUF(2) = 1599
         RBUF(4) = 2099
      END IF
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 3
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
C     -- dimensions of plot buffer (263=2100/8)
      BX = 263
      BY = 1600
      CALL GRGLUN(UNIT)
      RBUF(1) = UNIT
      NPICT = 0
      OPEN (UNIT=UNIT, FILE=CHR(:LCHR), CARRIAGECONTROL='NONE',
     :      DEFAULTFILE=DEFNAM, STATUS='NEW', RECL=128,
     :      FORM='UNFORMATTED', RECORDTYPE='VARIABLE', IOSTAT=IER)
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open output file for Versatec plot: '//
     1                CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
      ELSE
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = LEN(CHR)
   91     IF (CHR(LCHR:LCHR).EQ.' ') THEN
              LCHR = LCHR-1
              GOTO 91
          END IF
          RBUF(2) = 1
      END IF
      IER = GRGMEM(BX*BY, BITMAP)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          CLOSE (UNIT=UNIT, STATUS='DELETE')
          CALL GRFLUN(UNIT)
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CLOSE (UNIT=UNIT, STATUS='KEEP')
      CALL GRFLUN(UNIT)
      IER = GRFMEM(BX*BY, BITMAP)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to deallocate plot buffer.')
      END IF
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      NPICT = NPICT+1
C%    type *,'Begin picture',NPICT
      IF (NPICT.GT.1) WRITE (UNIT=UNIT) FF
      CALL GRVE03(BX*BY, %VAL(BITMAP))
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      IF (MODE.EQ.1) THEN
         CALL GRVE01(1, RBUF, IC, BX, BY, %VAL(BITMAP))
      ELSE
         XBUF(1) = RBUF(2)
         XBUF(2) = 1599 - RBUF(1)
         XBUF(3) = RBUF(4)
         XBUF(4) = 1599 - RBUF(3)
         CALL GRVE01(1, XBUF, IC, BX, BY, %VAL(BITMAP))
      END IF
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      IF (MODE.EQ.1) THEN
         CALL GRVE01(0, RBUF, IC, BX, BY, %VAL(BITMAP))
      ELSE
         XBUF(1) = RBUF(2)
         XBUF(2) = 1599 - RBUF(1)
         CALL GRVE01(0, XBUF, IC, BX, BY, %VAL(BITMAP))
      END IF
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
C%    type *,'End picture  ',NPICT
      CALL GRVE02(UNIT, BX, BY, %VAL(BITMAP))
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = RBUF(1)
      IF (IC.LT.0 .OR. IC.GT.1) THEN
          IC = 1
          RBUF(1) = IC
      END IF
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    (Not used.)
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called)
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
C    (Not implemented: should not be called)
C
  200 CONTINUE
      GOTO 900
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
C-----------------------------------------------------------------------
      END

C*GRVE01 -- PGPLOT Versatec driver, draw line
C+
      SUBROUTINE GRVE01 (LINE,RBUF,ICOL, BX, BY, BITMAP)
      INTEGER LINE
      REAL RBUF(4)
      INTEGER ICOL, BX, BY
      BYTE BITMAP(BX,BY)
C
C Draw a straight-line segment from absolute pixel coordinates
C (RBUF(1),RBUF(2)) to (RBUF(3),RBUF(4)).  The line either overwrites
C (sets to black) or erases (sets to white) the previous contents
C of the bitmap, depending on the current color index. Setting bits
C is accomplished with a VMS BISB2 instruction, expressed in
C Fortran as .OR.; clearing bits is accomplished with a VMS BICB2
C instruction, expressed in Fortran as .AND..NOT.. The line is
C generated with a Simple Digital Differential Analyser (ref:
C Newman & Sproull). 
C
C Arguments:
C
C LINE            I I      =0 for dot, =1 for line.
C RBUF(1),RBUF(2) I R      Starting point of line.
C RBUF(3),RBUF(4) I R      End point of line.
C ICOL            I I      =0 for erase, =1 for write.
C BITMAP        I/O B      (address of) the frame buffer.
C
C-----------------------------------------------------------------------
      BYTE    QMASK(0:7)
      INTEGER LENGTH, KX, KY, K
      REAL    D, XINC, YINC, XP, YP
      DATA    QMASK /128, 64, 32, 16, 8, 4, 2, 1/
C
      IF (LINE.GT.0) THEN
          D = MAX(ABS(RBUF(3)-RBUF(1)), ABS(RBUF(4)-RBUF(2)))
          LENGTH = D
          IF (LENGTH.EQ.0) THEN
              XINC = 0.
              YINC = 0.
          ELSE
              XINC = (RBUF(3)-RBUF(1))/D
              YINC = (RBUF(4)-RBUF(2))/D
          END IF
      ELSE
          LENGTH = 0
          XINC = 0.
          YINC = 0.
      END IF
      XP = RBUF(1)+0.5
      YP = RBUF(2)+0.5
      IF (ICOL.NE.0) THEN
          DO 10 K=0,LENGTH
              KX = XP
              KY = (BY-1)-INT(YP)
              BITMAP(KX/8+1,KY+1) = BITMAP(KX/8+1,KY+1) .OR. 
     1                              QMASK(MOD(KX,8))
              XP = XP + XINC
              YP = YP + YINC
 10        CONTINUE
      ELSE
          DO 20 K=0,LENGTH
              KX = XP
              KY = (BY-1)-INT(YP)
              BITMAP(KX/8+1,KY+1) = BITMAP(KX/8+1,KY+1) .AND.
     1                              (.NOT.QMASK(MOD(KX,8)))
              XP = XP + XINC
              YP = YP + YINC
 20        CONTINUE
      END IF
      END

C*GRVE02 -- PGPLOT Versatec driver, copy bitmap to output file
C+
      SUBROUTINE GRVE02 (UNIT, BX, BY, BITMAP)
      INTEGER UNIT, BX, BY
      BYTE BITMAP(BX,BY)
C
C Arguments:
C  UNIT   (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP
C  BITMAP (input)  the bitmap array
C-----------------------------------------------------------------------
      BYTE PREFIX
      DATA PREFIX/ 4/
      INTEGER I, J, K
C
C Write bitmap.
C
      DO 15 J=1,BY
         DO 5 K=BX,2,-1
            IF (BITMAP(K,J).NE.0) GOTO 10
 5       CONTINUE
 10      WRITE (UNIT=UNIT) PREFIX,(BITMAP(I,J),I=1,K)
 15   CONTINUE
C
      END

C*GRVE03 -- PGPLOT Versatec driver, zero bitmap
C+
      SUBROUTINE GRVE03(BUFSIZ, BUFFER)
C
C Arguments:
C
C BUFFER (byte array, input): (address of) the buffer.
C BUFSIZ (integer, input): number of bytes in BUFFER.
C-----------------------------------------------------------------------
      INTEGER  BUFSIZ, I
      BYTE     BUFFER(BUFSIZ), FILL
      DATA     FILL/0/
C
      DO 10 I=1,BUFSIZ
          BUFFER(I) = FILL
   10 CONTINUE
      END

