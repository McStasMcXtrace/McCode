      INCLUDE 'FGRAPH.FI'
      INCLUDE 'FLIB.FI'
      INCLUDE 'MOUSE.FI'
C*MSDRIV -- PGPLOT device driver for MS-DOS machines
C+
      SUBROUTINE MSDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
C     IMPLICIT NONE
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
      INCLUDE 'FGRAPH.FD'
C
C PGPLOT driver for IBM PC's and clones running Microsoft Fortran 32
C This driver will put the display into graphics mode. PGEND will
C return the monitor to the default (text) mode after pressing enter.
C
C This routine must be compiled and linked with the Microsoft graphics
C library supplied with Microsoft Fortran 32 or greater.
C The mouse routines grms1m, grms2m also require linking with mouse.obj
C 1989-Nov-03 - Started work [AFT]
C 1989-Apr-06 - Improved version [AFT]
C 1991-Mar-13 - Added cursor routine [JHT]
C 12/1993 C. T. Dum: Version for MS Fortran Power Station
C 3/95 C. T. Dum: mouse routine added
C-----------------------------------------------------------------------
C
C Supported device: IBM PC's and compatibles
C
C Device type code: /MSOFT
C
C Default device name: None (the device name, if specified, is
C ignored).
C
C Default view surface dimensions: Depends on monitor, typical 7x10 inches
C
C Resolution: Depends on graphics card.  Tested with a SVGA(TSENG
C ET4000, V7-Mercury P-64V)Card. The high resolution modes SRES
C (800*600), XRES(1024*768) and ZRES(1280*1024) require a VESA compliant
C SVGA card or an additional VESA TSR.

C Warning! Use the SRES, XRES, and ZRES modes only if they are
C supported by graphics card and monitor, else damage may result!

C Color capability: Color indices 0-15 are accepted.  This version maps
C    the PGPLOT color indices into the IBM color indices for with the
C    default color most closely corresponds to the PGPLOT default color.
C    Thus, PGPLOT index 2 (red) maps to IBM index 12 (light red).
C    The Default mode is VGA16 with 640*480 resolution. Higher resolution
C    modes are accessed by entering SET PGPLOT_VIDEO=SGA16, or XGA16, ZGA16
C    on the DOS command line, before starting programs.
C    No mapping is performed in the 256 color modes. These modes are accessed
C    by  SET PGPLOT_VIDEO=VGA25 or SGA25, XGA25, ZGA25.

C Input capability: None.
C File format: None.
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
      RECORD /VIDEOCONFIG/ VID
      RECORD /XYCOORD/ XY
C
      CHARACTER CMSG*10
      INTEGER   LEVEL, MXX, MXY
      REAL A,B
      INTEGER*2 I2TAB(0:15)
      INTEGER*2 I2BLU, I2GRN, I2IND, I2PEND, I2RED
      INTEGER*2 I2STAT, I2TMP, I2X0, I2Y0, I2X1, I2Y1
      INTEGER*4 I4STAT
      LOGICAL   QFIRST
      SAVE      QFIRST, MXX, MXY
      DATA I2TAB/ 0,15,12,10, 9,11,13,14, 6, 2, 3, 1, 5, 4, 8, 7/
      DATA QFIRST/.TRUE./
C
C---
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260) IFUNC
  900 WRITE (CMSG, '(I10)') IFUNC
      CALL GRWARN('Unimplemented function in MSOFT device driver: '/
     :      /CMSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CHR = 'MSOFT'
      LCHR = 5
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 CONTINUE
      IF(QFIRST) CALL GRMS00(VID, MXX, MXY, QFIRST)
      RBUF(1) = 0.
      RBUF(2) = FLOAT(MXX)
      RBUF(3) = 0.
      RBUF(4) = FLOAT(MXY)
      RBUF(5) = 0.
      RBUF(6) = VID.NUMCOLORS-1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C Divide the number of pixels on screen by a typical screen size in
C inches.
C
   30 continue
      IF(QFIRST) CALL GRMS00(VID, MXX, MXY, QFIRST)
      A = FLOAT(MXX)/10.0
      RBUF(1) = A
      B = FLOAT(MXY)/7.0
      RBUF(2) = B
      RBUF(3) = 1.0
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Interactive, cursor, No dashed lines, No area fill,
C    No thick lines, No rectangle fill)
C
   40 CHR = 'ICNNNNNNNN'
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
      IF(QFIRST) CALL GRMS00(VID, MXX, MXY, QFIRST)
      RBUF(1) = 0.
      RBUF(2) = FLOAT(MXX)
      RBUF(3) = 0.
      RBUF(4) = FLOAT(MXY)
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 1.
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
      IF(QFIRST) CALL GRMS00(VID, MXX, MXY, QFIRST)
      RBUF(1) = 0.
      RBUF(2) = 1.
      NBUF = 2
      IF(RBUF(3).NE.0.) THEN
         I2PEND=1
      ELSE
         I2PEND=0
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      read(*,*) ! press enter  to end graphics mode
      I2STAT = SETVIDEOMODE( $DEFAULTMODE )
      QFIRST=.TRUE.
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      IF(QFIRST) CALL GRMS00(VID, MXX, MXY, QFIRST)
      IF(I2PEND.EQ.0) THEN
         CALL CLEARSCREEN($GCLEARSCREEN)
      END IF
      I2PEND=0
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      I2X0=NINT(RBUF(1))
      I2Y0=MXY-NINT(RBUF(2))
      CALL MOVETO(I2X0, I2Y0, XY)
      I2X1=NINT(RBUF(3))
      I2Y1=MXY-NINT(RBUF(4))
      I2STAT=LINETO(I2X1, I2Y1)
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I2X0=NINT(RBUF(1))
      I2Y0=MXY-NINT(RBUF(2))
      I2STAT=SETPIXEL(I2X0, I2Y0)
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
  150 CONTINUE
      I2TMP=MAX(0,NINT(RBUF(1)))
      IF(I2TMP.LT.16)then
       I2IND=I2TAB(I2TMP)
      ELSE
       I2IND=I2TMP
      ENDIF
      I2STAT=SETCOLOR(I2IND)
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor or mouse --------------------------------------------
C
C
  170 CONTINUE
      I2X0 = NINT(RBUF(1))
      I2Y0 = MXY-NINT(RBUF(2))
      IF(VID.MODE .EQ. $VRES16COLOR)THEN
ctd    CALL GRMS1M( I2X0, I2Y0, CHR)   ! mouse routine for VGA16, chars
ctd       assigned to buttons
       CALL GRMS2M( I2X0, I2Y0, CHR)   ! mouse routine for VGA16, waits
ctd       for keyboard input
      ELSE
       CALL GRMS1C( I2X0, I2Y0, CHR, VID)   ! cursor routine
      ENDIF
      RBUF(1) = FLOAT(I2X0)
      RBUF(2) = FLOAT(MXY-I2Y0)
      NBUF = 2
      LCHR = 1
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
      I2TMP=MAX(0,NINT(RBUF(1)))
      IF(I2TMP.LT.16)then
       I2IND=I2TAB(I2TMP)
      ELSE
       I2IND=I2TMP
      ENDIF
      IF(VID.NUMCOLORS.EQ.16) THEN
C EGA 16 color mode
         I2RED=INT(RBUF(2)*3.999)
         I2GRN=INT(RBUF(3)*3.999)
         I2BLU=INT(RBUF(4)*3.999)
         LEVEL=(#303030.AND.(ISHFT(I2BLU,20).OR.ISHFT(I2GRN,12).OR.
     :      ISHFT(I2RED,4)))
         I4STAT=REMAPPALETTE(I2IND,LEVEL)
      ELSE IF(VID.NUMCOLORS.EQ.256) THEN
C VGA 256 color mode
         I2RED=INT(RBUF(2)*63.999)
         I2GRN=INT(RBUF(3)*63.999)
         I2BLU=INT(RBUF(4)*63.999)
         LEVEL=(#3F3F3F.AND.(ISHFT(I2BLU,16).OR.ISHFT(I2GRN,8).OR.
     :      I2RED))
         I4STAT=REMAPPALETTE(I2IND,LEVEL)
      END IF
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
C*********
      SUBROUTINE GRMS00(VID, MXX, MXY, QFIRST)
      INCLUDE 'FGRAPH.FD'
      RECORD /VIDEOCONFIG/ VID
      INTEGER   MXX, MXY ,TR$L
      LOGICAL   QFIRST
C---
      INTEGER*2 I2STAT
C added parameter override of default configuration... JHT 23-Feb-1991
      character*128 TR$VID
C---
      QFIRST=.FALSE.
      CALL GRGENV('VIDEO', TR$VID, TR$L)
      IF (TR$L .EQ. 0) THEN
          CALL GETVIDEOCONFIG(VID)
ctd       IF(VID.ADAPTER.EQ.$CGA) THEN
ctd          I2STAT=SETVIDEOMODE($HRESBW)
ctd       ELSE IF(VID.ADAPTER.EQ.$OCGA) THEN
ctd          I2STAT=SETVIDEOMODE($ORESCOLOR)
          IF(VID.ADAPTER.EQ.$EGA .OR. VID.ADAPTER.EQ.$OEGA) THEN
             IF(VID.MONITOR .EQ. $MONO) THEN
                I2STAT=SETVIDEOMODE($ERESNOCOLOR)
             ELSE
                I2STAT=SETVIDEOMODE($ERESCOLOR)
             END IF
          ELSE IF(VID.ADAPTER.EQ.$VGA .OR. VID.ADAPTER.EQ.$OVGA) THEN
ctd    :         .OR. VID.ADAPTER.EQ.$MCGA) THEN
ctd no longer supported
             IF(VID.MONITOR .EQ. $MONO) THEN
                I2STAT=SETVIDEOMODE($VRES2COLOR)
             ELSE
                I2STAT=SETVIDEOMODE($VRES16COLOR)
             END IF
ctd       ELSE IF(VID.ADAPTER.EQ.$HGC) THEN
ctd          I2STAT=SETVIDEOMODE($HERCMONO)
          ELSE IF(VID.ADAPTER.EQ.$SVGA) THEN
             IF(VID.MONITOR .EQ. $MONO) THEN
                I2STAT=SETVIDEOMODE($VRES2COLOR)
             ELSE
                I2STAT=SETVIDEOMODE($VRES16COLOR)
             END IF
          ELSE
             WRITE(*,*) 'Unknown graphics adapter.'
             STOP
          END IF
      ELSE
          IF(TR$VID(:TR$L) .EQ. 'EGA16') THEN
                I2STAT=SETVIDEOMODE($ERESCOLOR)
          ELSE IF(TR$VID(:TR$L) .EQ. 'VGA16') THEN
                I2STAT=SETVIDEOMODE($VRES16COLOR)
ctd warning (MS): careful possibly monitor damage with S,X,Z modes
          ELSE IF(TR$VID(:TR$L) .EQ. 'VGA25') THEN
                I2STAT=SETVIDEOMODE($VRES256COLOR)
          ELSE IF(TR$VID(:TR$L) .EQ. 'SGA16') THEN
                I2STAT=SETVIDEOMODE($SRES16COLOR)
          ELSE IF(TR$VID(:TR$L) .EQ. 'SGA25') THEN
                I2STAT=SETVIDEOMODE($SRES256COLOR)
          ELSE IF(TR$VID(:TR$L) .EQ. 'XGA16') THEN
                I2STAT=SETVIDEOMODE($XRES16COLOR)
          ELSE IF(TR$VID(:TR$L) .EQ. 'XGA25') THEN
                I2STAT=SETVIDEOMODE($XRES256COLOR)
          ELSE IF(TR$VID(:TR$L) .EQ. 'ZGA16') THEN
                I2STAT=SETVIDEOMODE($ZRES16COLOR)
          ELSE IF(TR$VID(:TR$L) .EQ. 'ZGA25') THEN
                I2STAT=SETVIDEOMODE($ZRES256COLOR)
          else
             I2STAT=SETVIDEOMODE($MAXRESMODE)
          END IF
      END IF
      CALL GETVIDEOCONFIG(VID)
      MXX=VID.NUMXPIXELS-1
      MXY=VID.NUMYPIXELS-1
      RETURN
      END
C------
      SUBROUTINE GRMS1M( IX, IY, CHR)
      INCLUDE 'FLIB.FD'
      INCLUDE 'FGRAPH.FD'
      INCLUDE 'MOUSE.FD'
      INTEGER*2 IX, IY
      CHARACTER*(*) CHR
C* pos. and return mouse cursor
C  IX, IY coordinates (I/O)
C  CHR (O) A,X,D or CTRL-D (EOT)
C C. T. Dum, March 23,1995
C link with mouse.obj (FL32 distr.), works for vga16 only
C left button returns A for single click, X for 2 clicks within 1s
C right button returns D for single click CTRL D for 2 clicks within 1s
C link with mouse.obj (FL32 distr.), works for vga16 only
      RECORD /BTN_STS/ btns
      INTEGER*4 ip,ICHR
      ICHR = 0
      ip=0
c move mouse cursor, exit if button is pressed
      DO WHILE(ICHR .EQ. 0)
         call setptrpos(int4(ix),int4(iy))
         call setptrvis(1)
         do while(ip.eq.0)
          btns.Btn=1 ! right button
          ip=getbuttonpress(btns)
c could add middle button (btns.Btn=2) if exists, or left & right
          if(ip.gt.0)then
           ix=btns.x
           iy=btns.y
           chr='D'
           if(ip.eq.2) chr=char(4)
           ichr=ichar(chr)
          else
           btns.Btn=0 ! left button
           ip=getbuttonpress(btns)
           if(ip.gt.0)then
            ix=btns.x
            iy=btns.y
            chr='A'
            if(ip.eq.2) chr='X'
            ichr=ichar(chr)
           else
c wait 1sec
            call sleepqq(1000)
           endif
          endif
        enddo
      ENDDO
      call setptrvis(2)
      RETURN
      END
