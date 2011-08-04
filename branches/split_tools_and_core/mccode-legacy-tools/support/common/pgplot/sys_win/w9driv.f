C* W9DRIV -- PGPLOT device driver for Windows95 (or WindowsNT)
C+
      SUBROUTINE W9DRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      USE       DFLIB
      IMPLICIT  NONE
      INTEGER   IFUNC, NBUF, LCHR, MODE
      REAL      RBUF(*)
      CHARACTER CHR*(*)
C
C PGPLOT driver for IBM PC's and clones running DIGITAL Visual Fortran
C (5.0 or higher).  This driver will create a graphics window.
C PGEND will return control to the default (text) window, but the graphics
C window is not erased until <RETURN> is pressed.
C
C This routine must be compiled and linked with the Digital DFLIB graphics
C library.  Application must be compiled as a "QuickWin Graphics" project type,
C compiler command line option /MW.
C 
C 1989-Nov-03 - (MSDRIV) Started work [AFT]
C 1989-Apr-06 - Improved version [AFT]
C 1991-Mar-13 - Added cursor routine [JHT]
C 12/1993 C. T. Dum: Version for MS Fortran Power Station
C 1996-Apr-26 - W9DRIV (Windows95/PowerStation 4.0 version):
C               resolution modes; interrupt driven mouse (cursor band modes);  
C               rectangle fill; pixel lines [Phil Seeger, PASeeger@aol.com]
C 1996-Apr-30 - multiple devices; return color representation [PAS]
C 1996-May-03 - each window has its own resolution and palette [PAS]
C 1997-Dec-15 - change USE statement from Microsoft to Digital [PAS]
C 1998-May-04 - had only 7 windows instead of 8 [PAS]
C 1998-Jun-22 - moved window initialization to be after open [ACLarson]
C-----------------------------------------------------------------------
C
C Supported device: IBM PC's and compatibles with Windows95/NT;
C                   requires VGA or higher graphics adapter
C
C Device type code: /W95  (also /WV, /WS, /WX, or /WZ)
C
C Modes:   1, VGA,   640 x 480
C          2, SVGA,  800 x 600
C          3, XGA,  1024 x 768
C          4, ZGA,  1280 x 1024
C          0, from PGPLOT_VIDEO environment parameter, or SVGA
C    The Default mode is 800 x 640 pixels (SVGA). Other resolution
C    modes are accessed by entering SET PGPLOT_VIDEO=VGA (640x480), 
C    XGA (1024x768), or ZGA (1280x1024) in the AUTOEXEC.BAT file, or
C    by using alternate device type designations.  The maximum allowed 
C    mode is determined by the graphics card and the Windows95 driver.
C
C Color capability: Color indices 0-15 are set to the default values
C    of PGPLOT, and indices 16-235 default to a gray scale.  Palettes
C    of up to 235 colors are saved for each of 8 possible device 
C    windows. (20 colors are reserved for the system.)
C    NOTE: There are some peculiar graphics adapters out there, and
C          even more peculiar drivers.  The default colors have been 
C          tweeked to appear unique in either the upper 6 or the lower
C          6 bits of each byte.  If you don't like what you see, you
C          may modify the DATA statement for RGB16.  It may also be
C          necessary to change PARAMETER CNORM from 255 to 63.
C
C Default device name: None (the device name, if specified, is ignored).
C
C View surface dimensions: Depends on monitor, typical 7.5x10 inches
C
C Resolution: Depends on graphics card and Windows95 driver.
C
C Input capability: Mouse position, must be followed by a keyboard key.
C
C File format: None.
C
C Obtaining hardcopy: via Windows95, "File/Print" menu choice.
C-----------------------------------------------------------------------
C Notes:
C  Up to MAXDEV "devices" may be open at once. ACTIVE is the number
C  of the currently selected device (1 if no devices are open).
C  STATE(i) is 0 if device i is not open, 1 if it is open but with
C  no current picture, or 2 if it is open with a current picture.
C-----------------------------------------------------------------------
      EXTERNAL  GRW900, GRW901
      INTEGER   MAXDEV
      REAL*4    CNORM
      PARAMETER (MAXDEV=8, CNORM=255.)
      TYPE (xycoord) XY
C
      CHARACTER CMSG*10, WINTITLE*80
      INTEGER   MX(0:4), MY(0:4), MXX(MAXDEV), MXY(MAXDEV), MXC(MAXDEV),&
     &          I, ACTIVE,IUNIT(MAXDEV), STATE(0:MAXDEV), NPIC(MAXDEV)
      INTEGER*4 IRGB, RGB(0:235,MAXDEV), RGB16(0:15), RGB236(0:235)
      INTEGER*2 I2STAT, I2X0, I2Y0, I2X1, I2Y1, DASHLINE(5), ICOLOR,    &
     &          CBITS(MAXDEV), IX(1), IY(1), IC(1)
      INTEGER*4 I4STAT, I4X, I4Y, IXREF, IYREF, BAND, EVENT, IBUF
      LOGICAL   FIRST, QFIRST(MAXDEV), LPOS
      SAVE FIRST, QFIRST, ACTIVE, STATE, XY, MXX, MXY, MXC, IUNIT, RGB, &
     &     ICOLOR, NPIC, MX, MY, DASHLINE, CBITS, RGB16
      DATA FIRST,  QFIRST,        ACTIVE, STATE(0:MAXDEV)/              &
     &     .TRUE., MAXDEV*.TRUE., 1,      -1, MAXDEV*0/
      DATA MX, MY/ 0, 640, 800, 1024, 1280,                             &
     &             0, 480, 640,  768, 1024/
      DATA DASHLINE/#FFFF, #FF80, #FC30, #CCCC, #FCCC/
C
C     Following data statement provides unique colors on all tested adapters
      DATA RGB16(0:15)/0, #FFFFFF, #0000FF, #00FF00, #FF0000, #FFFF00,  &
     &  #FF00FF, #00FFFF, #005FFF, #00FFAA, #AAFF00, #FF9300, #FF0093,  &
     &  #5F00FF, #555555, #AAAAAA/
C
C---
C     Initialize first 16 RGB values and gray scale for all windows
      IF (FIRST) THEN
         FIRST = .FALSE.
         DO ICOLOR=0,15
            DO I=1,MAXDEV
               RGB(ICOLOR,I) = RGB16(ICOLOR)
            END DO
         END DO
         IRGB = #020202
         DO ICOLOR=16,235
            IRGB = IRGB + #010101
            DO I=1,MAXDEV
               RGB(ICOLOR,I) = IRGB
            END DO
         END DO
         ICOLOR = 1
         DO I=1,MAXDEV
            CBITS(I) = #0F
         END DO
      END IF
C
      SELECT CASE (IFUNC)
C
      CASE (1)
C--- IFUNC = 1, Return device name.-------------------------------------
         SELECT CASE (MODE)
         CASE (1)
            CHR = 'WV    (Windows95/NT, 640x480)'
         CASE (2)
            CHR = 'WS    (Windows95/NT, 800x600)'
         CASE (3)
            CHR = 'WX    (Windows95/NT, 1024x768)'
         CASE (4)
            CHR = 'WZ    (Windows95/NT, 1280x1024)'
         CASE DEFAULT
            CHR = 'W9    (Windows95/NT, mode from environment)' 
         END SELECT
         LCHR = LEN_TRIM(CHR)
C
      CASE (2)
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
         IF (QFIRST(ACTIVE)) THEN
            MXX(ACTIVE) = MX(MODE)
            MXY(ACTIVE) = MY(MODE)
            MXC(ACTIVE) = 236
            CALL GRW900(ACTIVE, MXX, MXY, MXC, QFIRST)
         END IF
         RBUF(1) = 0.
         RBUF(2) = MXX(ACTIVE)
         RBUF(3) = 0.
         RBUF(4) = MXY(ACTIVE)
         RBUF(5) = 0.
         RBUF(6) = MXC(ACTIVE)
         NBUF = 6
C
      CASE (3)
C--- IFUNC = 3, Return device resolution. ------------------------------
C Divide the number of pixels on screen by a typical screen size in
C inches.
         IF (QFIRST(ACTIVE)) THEN
            MXX(ACTIVE) = MX(MODE)
            MXY(ACTIVE) = MY(MODE)
            MXC(ACTIVE) = 236
            CALL GRW900(ACTIVE, MXX, MXY, MXC, QFIRST)
         END IF
         RBUF(1) = FLOAT(MXX(ACTIVE)+1)/10.
         RBUF(2) = FLOAT(MXY(ACTIVE)+1)/7.5
         RBUF(3) = 1.0
         NBUF = 3
C
      CASE (4)
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No area fill,
C    No thick lines, Rectangle fill, Pixel primative, No ?, querY color
C    representation, No markers)
         CHR  = 'ICNNNRPNYN'
         LCHR = 10
C
      CASE (5)
C--- IFUNC = 5, Return default file name. ------------------------------
         CHR = 'PGPlot Graphics'
         LCHR = LEN_TRIM(CHR)
C
      CASE (6)
C--- IFUNC = 6, Return default physical size of plot. ------------------
         IF (QFIRST(ACTIVE)) THEN
            MXX(ACTIVE) = MX(MODE)
            MXY(ACTIVE) = MY(MODE)
            MXC(ACTIVE) = 236
            CALL GRW900(ACTIVE, MXX, MXY, MXC, QFIRST)
         END IF
         RBUF(1) = 0.
         RBUF(2) = MXX(ACTIVE)
         RBUF(3) = 0.
         RBUF(4) = MXY(ACTIVE)
         NBUF    = 4
C
      CASE (7)
C--- IFUNC = 7, Return misc defaults. ----------------------------------
         RBUF(1) = 1.
         NBUF    = 1
C
      CASE (8)
C--- IFUNC = 8, Select plot. -------------------------------------------
         I = NINT(RBUF(2))
         IF (I.GE.1 .AND. I.LE.MAXDEV .AND. STATE(I).GT.0) THEN 
            IF (I .NE. ACTIVE) THEN
               ACTIVE = I
               I4STAT = SETACTIVEQQ(IUNIT(ACTIVE))
               I4STAT = FOCUSQQ(IUNIT(ACTIVE))
               DO I=0,235
                  RGB236(I) = RGB(I,ACTIVE)
               END DO
               I4STAT = REMAPALLPALETTE(RGB236)
               I2STAT = SETCOLOR(ICOLOR)
            END IF
         ELSE
            CALL GRWARN('invalid or unopened graphics window in W9DRIV')
         END IF
C
      CASE (9)
C--- IFUNC = 9, Open workstation. --------------------------------------
         I = 0
         DO WHILE (I.LE.MAXDEV .AND. STATE(I).NE.0)
            I = I + 1
         END DO
         IF (I .GT. MAXDEV) THEN
            CALL GRWARN('maximum number of graphics windows exceeded')
            RBUF(1) = 0.
            RBUF(2) = 0.
         ELSE
            ACTIVE = I
            RBUF(1) = ACTIVE
            RBUF(2) = 1.
C           Initialize this window in requested mode, and open it
            MXX(ACTIVE) = MX(MODE)
            MXY(ACTIVE) = MY(MODE)
            MXC(ACTIVE) = 236
            CALL GRGLUN(IUNIT(ACTIVE))
            WRITE (WINTITLE, '(A,I2)') CHR(:LCHR)//', #', ACTIVE
            OPEN (IUNIT(ACTIVE), FILE='USER', TITLE=WINTITLE(:LCHR+5))
            CALL GRW900(ACTIVE, MXX, MXY, MXC, QFIRST)
            DO I=0,235
               RGB236(I) = RGB(I,ACTIVE)
            END DO
            I4STAT = REMAPALLPALETTE(RGB236)
            I2STAT = SETCOLOR(ICOLOR)
            STATE(ACTIVE) = 1
            NPIC(ACTIVE)  = 0
         END IF
         NBUF = 2
C
      CASE (10)
C--- IFUNC=10, Close workstation. --------------------------------------
         IF (STATE(ACTIVE) .GT. 0) THEN
            print ('(A,I2)'), ' Type <RETURN> to close graphics '//     &
     &                         'window #',active
            read * 
            CLOSE (IUNIT(ACTIVE))
            STATE(ACTIVE)  = 0
            QFIRST(ACTIVE) = .TRUE.
         END IF
C
      CASE (11)
C--- IFUNC=11, Begin picture. ------------------------------------------
         IF(NPIC(ACTIVE) .EQ. 0) THEN
            CALL CLEARSCREEN($GCLEARSCREEN)
         END IF
         STATE(ACTIVE) = 2
         NPIC(ACTIVE) = NPIC(ACTIVE) + 1
         I4STAT = SETACTIVEQQ(IUNIT(ACTIVE))
         I4STAT = FOCUSQQ(IUNIT(ACTIVE))
C
      CASE (12)
C--- IFUNC=12, Draw line. ----------------------------------------------
         I2X0   = NINT(RBUF(1))
         I2Y0   = MXY(ACTIVE) - NINT(RBUF(2))
         CALL     MOVETO(I2X0, I2Y0, XY)
         I2X1   = NINT(RBUF(3))
         I2Y1   = MXY(ACTIVE) - NINT(RBUF(4))
         I2STAT = LINETO(I2X1, I2Y1)
C
      CASE (13)
C--- IFUNC=13, Draw dot. -----------------------------------------------
         I2X0   = NINT(RBUF(1))
         I2Y0   = MXY(ACTIVE) - NINT(RBUF(2))
         I4STAT = SETPIXEL(I2X0, I2Y0)
C
      CASE (14)
C--- IFUNC=14, End picture. --------------------------------------------
         IF (STATE(ACTIVE) .GT. 0) STATE(ACTIVE) = 1
         NPIC(ACTIVE)  = 0
C
      CASE (15)
C--- IFUNC=15, Select color index. -------------------------------------
         ICOLOR = MIN(MXC(ACTIVE), MAX(0, NINT(RBUF(1))))
         I2STAT = SETCOLOR(ICOLOR)
C
      CASE (16)
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
      CASE (17)
C--- IFUNC=17, Read cursor (mouse) AND keystroke -----------------------
         I4X = NINT(RBUF(1))
         I4Y = MXY(ACTIVE) - NINT(RBUF(2))
         IF (NBUF .GE. 6) THEN
C           Support for multiple forms of cursor
            IXREF = NINT(RBUF(3))
            IYREF = MXY(ACTIVE) - NINT(RBUF(4))
            BAND  = NINT(RBUF(5))
            LPOS  = RBUF(6) .GT. 0.
         ELSE
C           Simple crosshair cursor
            IXREF = I4X
            IYREF = I4Y
            BAND  = 0
            LPOS  = .TRUE.
         END IF
C
C        Set color index, for exclusive-ORing
         ICOLOR   = SETCOLOR(CBITS(ACTIVE))
         I4STAT   = SETWRITEMODE($GXOR)
C        Initialize mouse routine by calling with fake arguments
         CALL GRW901(-BAND, MXX(ACTIVE), MXY(ACTIVE), IXREF, IYREF)
C        Initialize position of cursor by simulating mouse button click
         IF (LPOS) CALL GRW901(IUNIT(ACTIVE), MOUSE$MOVE, 0, I4X, I4Y)
C        Activate mouse callback routine
         EVENT    = MOUSE$MOVE
         I4STAT   = REGISTERMOUSEEVENT(IUNIT(ACTIVE), EVENT, GRW901)
C        Wait for a keystroke
         CHR(1:1) = GETCHARQQ()
C
C        A key has been struck; turn off mouse, get position, restore color
         I4STAT   = UNREGISTERMOUSEEVENT(IUNIT(ACTIVE), EVENT)
         CALL GRW901(0, 0, 0, I4X, I4Y)
         I4STAT   = SETWRITEMODE($GPSET)
         I2STAT   = SETCOLOR(ICOLOR)
C        Return results
         LCHR     = 1
         RBUF(1)  = I4X
         RBUF(2)  = MXY(ACTIVE) - I4Y
         NBUF     = 2
C
      CASE (18)
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
      CASE (19)
C--- IFUNC=19, Set line style. -----------------------------------------
C Note: not likely to be called because IFUNC=4 returns "No dashed lines"
         I = MIN(5,  MAX(0, NINT(RBUF(1))))
         CALL SETLINESTYLE(DASHLINE(I))
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
      CASE (21)
C--- IFUNC=21, Set color representation. -------------------------------
         I = NINT(RBUF(1))
         IF (I.GE.0 .AND. I.LE.MXC(ACTIVE)) THEN
            ICOLOR = I
            IRGB =    NINT(RBUF(2)*CNORM)     .OR.                      &
     &          ISHFT(NINT(RBUF(3)*CNORM), 8) .OR.                      &
     &          ISHFT(NINT(RBUF(4)*CNORM),16)
            RGB(I,ACTIVE) = IRGB
            CBITS(ACTIVE) = CBITS(ACTIVE) .OR. ICOLOR
            I4STAT = REMAPPALETTERGB(I, IRGB)
         END IF
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
      CASE (24)
C--- IFUNC=24, Rectangle fill. -----------------------------------------
         I2X0   = NINT(RBUF(1))
         I2Y0   = MXY(ACTIVE) - NINT(RBUF(2))
         I2X1   = NINT(RBUF(3))
         I2Y1   = MXY(ACTIVE) - NINT(RBUF(4))
         I2STAT = RECTANGLE($GFILLINTERIOR, I2X0, I2Y0, I2X1, I2Y1)
C
C--- IFUNC=25, Set fill pattern. ---------------------------------------
C
      CASE (26)
C--- IFUNC=26, Line of pixels. -----------------------------------------
         IY(1) = MXY(ACTIVE) - NINT(RBUF(2))
         IF (IY(1).GE.0 .AND. IY(1).LE.MXY(ACTIVE)) THEN
            IX(1) = NINT(RBUF(1))
            IBUF  = 3
            IF (IX(1) .LT. 0) THEN
               IBUF  = 3 - IX(1)
               IX(1) = 0
            END IF
            DO WHILE (IBUF.LE.NBUF .AND. I2X0.LE.MXX(ACTIVE))
               IC(1) = MIN(MXC(ACTIVE), MAX(0, NINT(RBUF(IBUF))))
               CALL SETPIXELS(1, IX, IY, IC)
               IBUF  = IBUF + 1
               IX(1) = IX(1) + 1
            END DO
         END IF
C
C--- IFUNC=27, Scaling info --------------------------------------------
C
C--- IFUNC=28, Draw marker ---------------------------------------------
C
      CASE (29)
C--- IFUNC=29, Query color representation.------------------------------
         I       = NINT(RBUF(1))
         IRGB    = RGB(I,ACTIVE)
         RBUF(2) = FLOAT(IAND(IRGB, #FF))/CNORM
         RBUF(3) = FLOAT(IAND(ISHFT(IRGB,-8), #FF))/CNORM
         RBUF(4) = FLOAT(IAND(ISHFT(IRGB,-16), #FF))/CNORM
         NBUF    = 4
C
      CASE DEFAULT
C--- Unimplemented Function
         WRITE (CMSG, '(I10)') IFUNC
         CALL GRWARN('Unimplemented function in Win95 device driver:'// &
     &               CMSG)
         NBUF = -1
      END SELECT
      RETURN
      END
C*********
      SUBROUTINE GRW900(I, MXX, MXY, MXC, QFIRST)
      USE       DFLIB
C  1998-May-04 - change from MSFLIB to DFLIB 
C  1998-Jun-22 - don't set WC.title to 'PGPLOT'
      IMPLICIT  NONE
      INTEGER   I, MXX(*), MXY(*), MXC(*)
      LOGICAL   QFIRST(*)
C---          
      TYPE (WINDOWCONFIG) WC
      INTEGER   TR$L
      LOGICAL   STATUS
      CHARACTER TR$VID*128
C---
C     Set default window configuration
C     Try to set to input values of MXX, MXY, default 800x600
      IF (MXX(I).LE.1 .OR. MXY(I).LE.1) THEN
         MXX(I) = 800
         MXY(I) = 600
         MXC(I) = 256
         CALL GRGENV('VIDEO', TR$VID, TR$L)
         IF (TR$L .GT. 0) THEN
C           There is a "PGPLOT_VIDEO" parameter in the Environment
            IF( TR$VID(1:1) .EQ. 'V') THEN
C              Set to VGA resolution
               MXX(I) = 640
               MXY(I) = 480
            ELSE IF (TR$VID(1:1) .EQ. 'X') THEN
               MXX(I) = 1024
               MXY(I) = 768
            ELSE IF (TR$VID(1:1) .EQ. 'Z') THEN
               MXX(I) = 1280
               MXY(I) = 1024
            END IF
         END IF
      END IF
C
      WC.numxpixels  = MXX(I)
      WC.numypixels  = MXY(I)
      WC.numcolors   = MXC(I)
      WC.numtextcols = -1
      WC.numtextrows = -1
      WC.fontsize    = -1
      STATUS = SETWINDOWCONFIG(WC)
      IF(.NOT.STATUS) STATUS = SETWINDOWCONFIG(WC)
C
      MXX(I) = WC.numxpixels - 1
      MXY(I) = WC.numypixels - 1
      MXC(I) = WC.numcolors - 1
      QFIRST(I) = .FALSE.
      RETURN
      END
C*********
      RECURSIVE SUBROUTINE GRW901(IUNIT,EVENT,KEYSTATE,XMOUSE,YMOUSE)
      USE      DFLIB
      IMPLICIT NONE
      INTEGER  IUNIT, EVENT, KEYSTATE, XMOUSE, YMOUSE
C
C  Callback routine for mouse events, specific to Windows95
C       Note: cursor band modes implemented in software
C
C  1996-Apr-26 - P.A.Seeger
C  1998-May-04 - change from MSFLIB to DFLIB 
C--
      RECORD /xycoord/ XY
      INTEGER*2 BAND, LENGTH, IX0, IY0, IX1, IY1, IX2, IY2, IXR, IYR
      INTEGER*4 DUMMY
      LOGICAL   INITIUS, FINIS
      DATA      INITIUS /.TRUE./
      SAVE      INITIUS,BAND,LENGTH,IX0,IY0,IX1,IY1,IX2,IY2,IXR,IYR
C
C     Disable mouse movement interrupts while in callback routine
      IF (IUNIT .GT. 0) DUMMY = UNREGISTERMOUSEEVENT(IUNIT, MOUSE$MOVE)
C
      FINIS = IUNIT.EQ.0 .AND. EVENT.EQ.0 .AND. KEYSTATE.EQ.0
      IF (IUNIT.LE.0 .AND. .NOT.FINIS) THEN
         INITIUS = .TRUE.
C        Get initialization parameters from callback calling sequence
         BAND = -IUNIT
         IXR  = XMOUSE
         IYR  = YMOUSE
         IX0  = IXR
         IY0  = IYR
C        Extract parameters for length of cursor lines
         IF (BAND .EQ. 0) THEN
C           Simple crosshair cursor
            LENGTH = MAX(EVENT, KEYSTATE)/80 + 3
         ELSE
            LENGTH = 0
            IX1 = 0
            IX2 = 0
            IY1 = 0
            IY2 = 0
C           Modes with full width horizontal line(s)
            IF (BAND.EQ.3 .OR. BAND.EQ.5 .OR. BAND.EQ.7) IX2 = EVENT
C           Modes with full height vertical line(s)
            IF (BAND.EQ.4 .OR. BAND.EQ.6 .OR. BAND.EQ.7) IY2 = KEYSTATE
            IF (BAND .EQ. 3) THEN
C              Draw fixed horizontal line at anchor
               CALL    MOVETO(IX1, IYR, XY)
               DUMMY = LINETO(IX2, IYR    )
            ELSE IF (BAND .EQ. 4) THEN
C              Draw fixed vertical line at anchor
               CALL    MOVETO(IXR, IY1, XY)
               DUMMY = LINETO(IXR, IY2    )
            END IF
         END IF
         GO TO 700
      END IF
C
      IF (.NOT. INITIUS) THEN
C        This is NOT an initial call, so need to erase previous cursor
C          by rewriting it in complementary color mode
         IF (BAND .EQ. 1) THEN
            CALL    MOVETO(IXR, IYR, XY)
            DUMMY = LINETO(IX0, IY0    )
         ELSE IF (BAND .EQ. 2) THEN
            DUMMY = RECTANGLE($GBORDER, IXR, IYR, IX0, IY0)
         ELSE
            IF (IY2 .NE. IY1) THEN
               CALL    MOVETO(IX0, IY1, XY)
               DUMMY = LINETO(IX0, IY2    )
            END IF
            IF (IX2 .NE. IX1) THEN
               CALL    MOVETO(IX1, IY0, XY)
               DUMMY = LINETO(IX2, IY0    )
            END IF
         END IF
      END IF
C
      IF (FINIS) THEN
C        Termination call, return latest mouse location
         XMOUSE = IX0
         YMOUSE = IY0
         IF (BAND .EQ. 3) THEN
C           Erase fixed horizontal line at anchor
            CALL    MOVETO(IX1, IYR, XY)
            DUMMY = LINETO(IX2, IYR    )
         ELSE IF (BAND .EQ. 4) THEN
C           Erase fixed vertical line at anchor
            CALL    MOVETO(IXR, IY1, XY)
            DUMMY = LINETO(IXR, IY2    )
         END IF
         INITIUS = .TRUE.
         GO TO 700
      END IF
C
C     Save new cursor location
      IX0 = XMOUSE
      IY0 = YMOUSE
      IF (BAND .EQ. 0) THEN
C        Find ends of cursor line segments
         IX1 = IX0 - LENGTH
         IX2 = IX0 + LENGTH
         IY1 = IY0 - LENGTH
         IY2 = IY0 + LENGTH      
      END IF
C 
C     Now draw line, box, or cursor in complementary color
      INITIUS = .FALSE.
      IF (BAND .EQ. 1) THEN
C        Line from anchor to cursor location
         CALL    MOVETO(IXR, IYR, XY)
         DUMMY = LINETO(IX0, IY0    )
      ELSE IF (BAND .EQ. 2) THEN
C        Box with vertices at anchor and cursor location
         DUMMY = RECTANGLE($GBORDER, IXR, IYR, IX0, IY0)
         ELSE
            IF (IY2 .NE. IY1) THEN
C           Draw a horizontal line (or segment)        
            CALL    MOVETO(IX0, IY1, XY)
            DUMMY = LINETO(IX0, IY2)
         END IF
            IF (IX2 .NE. IX1) THEN
C           Draw a vertical line (or segment)
            CALL    MOVETO(IX1, IY0, XY)
            DUMMY = LINETO(IX2, IY0)
         END IF
      END IF
C
700   CONTINUE
      IF (IUNIT.GT.0 .AND. .NOT.INITIUS)                                &
     &       DUMMY = REGISTERMOUSEEVENT(IUNIT, MOUSE$MOVE, GRW901)
      RETURN
      END
