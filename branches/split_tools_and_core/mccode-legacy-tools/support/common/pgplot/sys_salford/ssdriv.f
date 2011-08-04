C*SSDRIV -- PGPLOT device driver for MS-DOS /Salford Software
C+
      SUBROUTINE SSDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      IMPLICIT NONE
      INTRINSIC NINT
      INTEGER NINT
      INTEGER IFUNC, NBUF, LCHR,ICOL
      INTEGER*2 IRESH,IRESV,IH1,IV1,IH2,IV2,IER
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C This driver will put the display into graphics mode, and calls to 'close'
C the workstation will set it back into the previous mode (generally erasing
C the display, so don't do it until you are really finished).
C
C This routine must be compiled and linked with the Lahey graphics
C library GRAPH3 supplied with Lahey Fortran v4.0 or greater.
C
C Microsoft FORTRAN versions:
C 1989-Nov-03 - Started work [AFT]
C 1989-Apr-06 - Improved version [AFT]
C 1991-Mar-13 - Added cursor routine [JHT]
C Lahey FORTRAN versions:
C 1991-Dec-28 - derived from Microsoft version [PAH]
C 1995-Mar-21 - derived from LH-version [mlm]
C 1995-June-1 - variable ICOL  SAVED  [mlm]
C-----------------------------------------------------------------------
C
C     ATTEMPTED PORT TO SALFORD SOFTWARE/MLM
C
C
C
C Supported device: IBM PC's and compatables
C
C Device type code: /LH
C
C Default device name: None (the device name, if specified, is
C ignored).
C
C Default view surface dimensions: Depends on monitor, typical 7x10 inches
C
C Resolution: Depends on graphics card.  Tested with a 640x480 VGA card.
C    Driver should work with other graphics cards, however, expect to
C    tweak it a bit.
C
C Color capability: Color indices 0-15 are accepted.  This version maps
C    the PGPLOT color indices into the IBM color indices for with the
C    default color most closely corresponds to the PGPLOT default color.
C    Thus, PGPLOT index 2 (red) maps to IBM index 12 (light red).
C
C Input capability: Graphics cursor implemented using Microsoft Mouse
C    or compatible, accessed through DOS calls.
C
C File format: None.
C
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
      CHARACTER CMSG*10
      REAL A,B
      INTEGER ICOL
      INTEGER    PENCOLOUR, PEND, COLOURMAP(0:15), IX, IY
      LOGICAL    GOT_MOUSE
      CHARACTER*30 CVALUE
      INTEGER LVALUE
      SAVE       PENCOLOUR, PEND,IRESH,IRESV,ICOL

      DATA colourmap/ 0,15,12,10, 9,11,13,14, 6, 2, 3, 1, 5, 4, 8, 7/

C---
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260) IFUNC
  900 WRITE (CMSG, '(I10)') IFUNC
      CALL GRWARN('Unimplemented function in FTN77 device driver: '/
     :      /CMSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CHR = 'SS  (SALFORD SOFTWARE FTN77 SCREEN DRIVER)'
      LCHR =42
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 CONTINUE
      RBUF(1) = 0
      RBUF(2) = IRESH-1
      RBUF(3) = 0
      RBUF(4) = IRESV-1
      RBUF(5) = 0
      RBUF(6) = ICOL-1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C Divide the number of pixels on screen by a typical screen size in
C inches.
C
   30 continue
      A = IRESH/9.5
      RBUF(1) = A
      B = IRESV/7.5
      RBUF(2) = B
      RBUF(3) = 1.0
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Interactive, No cursor, No dashed lines, No area fill,
C    No thick lines, No rectangle fill, No pixel primitives,)
C
   40 continue
      if (got_mouse()) then
        CHR = 'ICNNNRNNNN'
      else
        CHR = 'ICNNNRNNNN'
      endif
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = ' '
      LCHR = 1
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 CONTINUE
      RBUF(1) = 0
      RBUF(2) = IRESH-1
      RBUF(3) = 0
      RBUF(4) = IRESV-1
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 1
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
      RBUF(1) = 0
      RBUF(2) = 1
      NBUF = 2
      IF(RBUF(3) .NE. 0.0) THEN
C        PEND=1
      ELSE
         CALL GRGENV('RESO', CVALUE, LVALUE)
C
C     READ HORIZONTAL, VERTICAL RESOLUTION AND COLOURS FROM INTERNAL FILE
C
         READ (CVALUE,*,ERR=91) IRESH,IRESV,ICOL
         IER = 0
         CALL GRAPHICS_MODE_SET@(IRESH,IRESV,ICOL,IER)
         IF (IER .NE. 0) GOTO 91
      END IF
      RETURN
C
C     PATCH FOR FAILURE TO RUN IN SPECIFIED MODE; TRY STANDARD VGA
C
  91  CONTINUE
C     CALL TEXT_MODE@
C     WRITE(*,*) ' FAILURE TO INITIALIZE  '
C     PAUSE
      IRESH = 640
      IRESV = 480
      ICOL = 16
      CALL GRAPHICS_MODE_SET@(IRESH,IRESV,ICOL,IER)
C     CALL LOAD_STANDARD_COLOURS@
C     CALL VGA@
      IF (IER .NE. 0) THEN
C
C        STILL INOPERATIVE
C
         CALL TEXT_MODE@
         WRITE(*,*) 'Failure to initialize VGA '
         STOP  ' Exiting program  '
      ENDIF
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
C
C     WAIT FOR KEYSTROKE
C
      CALL GET_KEY@(IX)
      CALL TEXT_MODE@
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      CALL CLEAR_SCREEN@
C     IF(PEND.EQ.0) THEN
C        CALL PLOT(0, 0, -999)
C     ENDIF
C     PEND=0
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
C
C     SALFORD SOFTWARE DRAW-LINE ROUTINE
C
      IH1 = NINT(RBUF(1))
      IV1 = NINT(IRESV-1-RBUF(2))
      IH2 = NINT(RBUF(3))
      IV2 = NINT(IRESV-1-RBUF(4))
      CALL DRAW_LINE@(IH1,IV1,IH2,IV2,PENCOLOUR)
C
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
C
C     SALFORD SOFTWARE PIXEL
C
      IH1 = NINT(RBUF(1))
      IV1 = NINT(IRESV-1-RBUF(2))
      CALL SET_PIXEL@(IH1,IV1,PENCOLOUR)
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      IF (RBUF(1) .NE. 0.0) THEN
c       call clear_screen@
      ENDIF
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
  150 CONTINUE
      PENCOLOUR=MIN( MAX(0,NINT(RBUF(1))) ,15)
      PENCOLOUR=COLOURMAP(PENCOLOUR)
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C
C
  170 CONTINUE
      IX = NINT(RBUF(1))
      IY = IRESV-1-NINT(RBUF(2))
      CALL SHOW_MOUSE
      CALL PUT_MOUSE(IX,IY)
      CALL CURSOR_KEY(IX,IY,CHR)
      RBUF(1) = IX
      RBUF(2) = IRESV-1-IY
      CALL HIDE_MOUSE
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C
  190 CONTINUE
      RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      RETURN
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C
  240 CONTINUE
      IH1 = NINT(RBUF(1))
      IV1 = NINT(IRESV-1-RBUF(2))
      IH2 = NINT(RBUF(3))
      IV2 = NINT(IRESV-1-RBUF(4))
      CALL FILL_RECTANGLE@(IH1,IV1,IH2,IV2,PENCOLOUR)
      RETURN
C
C--- IFUNC=25, Set fill pattern. ---------------------------------------
C
  250 CONTINUE
      RETURN
C
C--- IFUNC=26, Line of pixels. -----------------------------------------
C
  260 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END
      LOGICAL FUNCTION GOT_MOUSE()
      IMPLICIT NONE
      INTEGER*2          NTRUP
      INTEGER*2          INTARY(10), EAX
      EQUIVALENCE        (EAX,INTARY(1))
      EAX = 0
      NTRUP = 51
      CALL REAL_MODE_INTERRUPT@(INTARY,NTRUP)
      GOT_MOUSE = (EAX .EQ. -1)
      RETURN
      END

      SUBROUTINE SHOW_MOUSE
      IMPLICIT NONE
      CALL DISPLAY_MOUSE_CURSOR@
      RETURN
      END

      SUBROUTINE HIDE_MOUSE
      IMPLICIT NONE
      CALL HIDE_MOUSE_CURSOR@
      RETURN
      END

      SUBROUTINE GET_MOUSE(IX, IY, BUTTON)
      IMPLICIT NONE
      INTEGER*2 IX, IY, BUTTON
      CALL GET_MOUSE_POSITION@(IX,IY,BUTTON)
      RETURN
      END

      SUBROUTINE PUT_MOUSE(IX, IY)
      IMPLICIT NONE
      INTEGER*2 IX, IY
      CALL SET_MOUSE_POSITION@(IX,IY)
      RETURN
      END

      SUBROUTINE CURSOR_KEY(IX, IY, KEY)
      IMPLICIT NONE
      CHARACTER*(*) KEY
      INTEGER IX, IY, IKEY, IB
      INTEGER*2 IXKEY
      CALL GET_KEY@(IKEY)
      KEY = CHAR(IKEY)
      IX = 0
      IY = 0
      CALL GET_MOUSE(IX, IY, IB)
      RETURN
      END
