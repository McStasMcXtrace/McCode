      INCLUDE 'FGRAPH.FI'
C*MSDRIV -- PGPLOT device driver for MS-DOS machines
C+
      SUBROUTINE MSDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      IMPLICIT NONE
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
      INCLUDE 'FGRAPH.FD'
C
C PGPLOT driver for IBM PC's and clones running Microsoft Fortran 5.0.
C This driver will put the display into graphics mode.  To avoid 'erasing'
C the screen when the program exits, the display will be left in graphics
C mode.  Therefore, you will need some other program to restore to the
C display to a (faster) text mode.
C
C This routine must be compiled and linked with the Microsoft graphics
C library supplied with Microsoft Fortran 5.0 or greater.
C
C LINK QDP,,NUL,\XANADU\PLOT\PGPLOT\PGPLOT+\FOR\LIB\GRAPHICS;
C
C where \XANADU\PLOT\PGPLOT\PGPLOT.LIB is the name of the PGPLOT library
C containing the PGPLOT code and \FOR\LIB\GRAPHICS.LIB is the graphics
C library supplied with Microsoft Fortran 5.0.
***********
C---  The call to the adapter type gives the HIGHEST resoltion, and
C---  within those the higest number of colors.
C---  The list is based on Fortran 5.1 manual advanced topics, table 13.5
C---  The selection for the Olivetti has not been tested.
C---
C---  It was noted that the calls to MICROSOFT FORTRAN V5.1 RGB colors were 
C---  were made as INTEGER*2. This gave too few colors for redefined palettes.
C---  The correct decleration is INTEGER*4.
***********
C
C 1989-Nov-03 - Started work [AFT]
C 1989-Apr-06 - Improved version [AFT]
C 1991-Mar-13 - Added cursor routine [JHT]
C 1994-Mar-30 - Improved the search for the optimum graphics card. [HJL]
C 1994-Jun-08 - Fixed bug in definition of RGB color variables. [HJL]
C 1994-Dec-03 - Added PC9801 NEC mode [YK]
C 1995-Jul-31 - concatnated YK and HJL versions [HJL]
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
C Resolution: Depends on graphics card.  Tested with a 640x300 EGA card.
C    Driver should work with other graphics cards, however, expect to
C    tweak it a bit.
C
C Color capability: Color indices 0-15 are accepted.  This version maps
C    the PGPLOT color indices into the IBM color indices for with the
C    default color most closely corresponds to the PGPLOT default color.
C    Thus, PGPLOT index 2 (red) maps to IBM index 12 (light red).
C
C Input capability: None.
C
C File format: None.
C
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
      RECORD /VIDEOCONFIG/ VID
      RECORD /XYCOORD/ XY
C
      CHARACTER CMSG*10
      INTEGER   LEVEL, MXX, MXY
      REAL A,B
      INTEGER*4 I2TAB(0:15)
      INTEGER*4 I2BLU, I2GRN, I2IND, I2PEND, I2RED
      INTEGER*4 I2STAT, I2TMP
      INTEGER*2 I2X0, I2Y0, I2X1, I2Y1
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
      RBUF(1) = 0
      RBUF(2) = FLOAT(MXX)
      RBUF(3) = 0
      RBUF(4) = FLOAT(MXY)
      RBUF(5) = 0
      RBUF(6) = 15
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
      RBUF(1) = 0
      RBUF(2) = FLOAT(MXX)
      RBUF(3) = 0
      RBUF(4) = FLOAT(MXY)
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
      IF(QFIRST) CALL GRMS00(VID, MXX, MXY, QFIRST)
      RBUF(1) = 0
      RBUF(2) = 1
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
C      I2STAT = SETVIDEOMODE( $DEFAULTMODE )
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
      I2TMP=MIN( MAX(0,NINT(RBUF(1))) ,15)
      I2IND=I2TAB(I2TMP)
      I2STAT=SETCOLOR(I2IND)
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
      I2X0 = NINT(RBUF(1))
      I2Y0 = MXY-NINT(RBUF(2))
      CALL GRMS01( I2X0, I2Y0, CHR, VID)
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
      I2TMP=MIN( MAX(0,NINT(RBUF(1))), 15)
      I2IND=I2TAB(I2TMP)
      IF(VID.NUMCOLORS.EQ.16) THEN
C EGA 16 color mode
         I2RED=INT(RBUF(2)*3.999)
         I2GRN=INT(RBUF(3)*3.999)
         I2BLU=INT(RBUF(4)*3.999)
         LEVEL=(#303030.AND.(ISHFT(I2BLU,20).OR.ISHFT(I2GRN,12).OR.
     :      ISHFT(I2RED,4)))
         I2STAT=REMAPPALETTE(I2IND,LEVEL)
      ELSE IF(VID.NUMCOLORS.EQ.256) THEN
C VGA 256 color mode
         I2RED=INT(RBUF(2)*63.999)
         I2GRN=INT(RBUF(3)*63.999)
         I2BLU=INT(RBUF(4)*63.999)
         LEVEL=(#3F3F3F.AND.(ISHFT(I2BLU,16).OR.ISHFT(I2GRN,8).OR.
     :      I2RED))
         I2STAT=REMAPPALETTE(I2IND,LEVEL)
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
C
C changed defaults to select the more sensible option maximum resolution.
C left in the over ride for compatibility with previous version.
C Option can be overridden by setting an enviroment variable PG_VIDEO
C to MAXCOL. Other options are MAXRES,VGA16,VGA2, EGA15, EGA4 etc.
C                                                      HJL 30-Mar-1994
      character*128 TR$VID
C---
      QFIRST=.FALSE.
      CALL GRGENV('VIDEO', TR$VID, TR$L)
      IF (TR$L.EQ.0. OR. 
     +   (TR$L.NE.0. AND. TR$VID(:TR$L).EQ.'MAXRES')) THEN
         CALL GETVIDEOCONFIG(VID)
         IF(VID.ADAPTER.EQ.$VGA .OR. VID.ADAPTER.EQ.$OVGA)THEN
            I2STAT=SETVIDEOMODE($VRES16COLOR)
         ELSE IF(VID.ADAPTER.EQ.$MCGA) THEN
            I2STAT=SETVIDEOMODE($VRES2COLOR)
         ELSE IF(VID.ADAPTER.EQ.$EGA) THEN
            IF(VID.MONITOR .EQ. $MONO) THEN
               I2STAT=SETVIDEOMODE($ERESNOCOLOR)
            ELSE
               I2STAT=SETVIDEOMODE($ERESCOLOR)
            END IF
         ELSE IF(VID.ADAPTER.EQ.$OEGA) THEN
            I2STAT=SETVIDEOMODE($ORESCOLOR)
         ELSE IF(VID.ADAPTER.EQ.$CGA) THEN
            I2STAT=SETVIDEOMODE($HRESBW)
         ELSE IF(VID.ADAPTER.EQ.$OCGA) THEN
            I2STAT=SETVIDEOMODE($ORESCOLOR)
         ELSE IF(VID.ADAPTER.EQ.$HGC) THEN
            I2STAT=SETVIDEOMODE($HERCMONO)
         ELSE IF(VID.ADAPTER.EQ.$98CGA .or. VID.ADAPTER.EQ.$98EGA) THEN
            I2STAT=SETVIDEOMODE($MAXRESMODE)
            two lines above are added for PC9801 NEC compatibles
         ELSE
            WRITE(*,*) 'Unknown graphics adapter.'
            STOP
         END IF
      ELSE
         IF(TR$VID(:TR$L) .EQ. 'EGA16') THEN
            I2STAT=SETVIDEOMODE($ERESCOLOR)
         ELSE IF(TR$VID(:TR$L) .EQ. 'VGA16') THEN
            I2STAT=SETVIDEOMODE($VRES16COLOR)
         ELSE 
         CALL GETVIDEOCONFIG(VID)
           IF(VID.ADAPTER.EQ.$VGA .OR. VID.ADAPTER.EQ.$OVGA. OR.
     +        VID.ADAPTER.EQ.$MCGA) THEN
              I2STAT=SETVIDEOMODE($MRES256COLOR)
           ELSE IF(VID.ADAPTER.EQ.$EGA) THEN
              IF(VID.MONITOR .EQ. $MONO) THEN
                 I2STAT=SETVIDEOMODE($ERESNOCOLOR)
              ELSE
                 IF(VID.NUMCOLORS.EQ.256) THEN
                     I2STAT=SETVIDEOMODE($ERESCOLOR)
                 ELSE
                     I2STAT=SETVIDEOMODE($HRES16COLOR)
                 END IF
              END IF
           ELSE IF(VID.ADAPTER.EQ.$OEGA) THEN
              I2STAT=SETVIDEOMODE($ERESCOLOR)
           ELSE IF(VID.ADAPTER.EQ.$CGA) THEN
              IF(VID.MONITOR.EQ.$MONO) THEN
                 I2STAT=SETVIDEOMODE($MRESNOCOLOR)
              ELSE
                 I2STAT=SETVIDEOMODE($MRES4COLOR)
              END IF
           ELSE IF(VID.ADAPTER.EQ.$OCGA) THEN
              I2STAT=SETVIDEOMODE($MRES4COLOR)
           ELSE IF(VID.ADAPTER.EQ.$HGC) THEN
              I2STAT=SETVIDEOMODE($HERCMONO)
           ELSE IF(VID.ADAPTER.EQ.$98CGA .or. VID.ADAPTER.EQ.$98EGA) THEN
              I2STAT=SETVIDEOMODE($MAXRESMODE)
c            two lines above are added for PC9801 NEC compatibles
           ELSE
              WRITE(*,*) 'Unknown graphics adapter.'
           STOP
           END IF
         END IF
      END IF
      CALL GETVIDEOCONFIG(VID)
      MXX=VID.NUMXPIXELS-1
      MXY=VID.NUMYPIXELS-1
      RETURN
      END
C*********
      SUBROUTINE GRMS01( IX, IY, CHR, VID)
      INCLUDE 'FGRAPH.FD'
      RECORD /xycoord/ XY
      RECORD /VIDEOCONFIG/ VID
      INTEGER*4 IMSIZE,INC,CNT(2),IHR,IMIN,ISEC,ITICK
      INTEGER*2 IX, IY, X0, Y0, X1, Y1, IERR, DUMMY
      INTEGER*2 ACTION
      INTEGER*1 SCAN
      INTEGER*1 BUFFER[ALLOCATABLE] (:)
      CHARACTER*(*) CHR
      INTEGER*1 ICHR
      DATA ACTION/ $GPSET /
C OVERKILL ON IMAGESIZE IN CASE THERE ARE BYTE ALLIGNMENT ISSUES
      IMSIZE = IMAGESIZE( 0,0,25,25 )
C
C COUNTER AND INCREMENT TO ADD CURSOR ACCELLERATION
      CNT(1) = 0
      INC = 1
      ALLOCATE( BUFFER( IMSIZE ), STAT = IERR )
      IF( IERR .NE. 0 ) THEN
           DUMMY = SETVIDEOMODE( $DEFAULTMODE )
           STOP 'Error: insufficient memory'
      ENDIF
      ICHR = 0
      DO WHILE(ICHR .EQ. 0)
         IX = MAX0( IX, 0)
         IY = MAX0( IY, 0)
         IX = MIN0( IX, (VID.NUMXPIXELS - 1))
C
         IY = MIN0( IY, (VID.NUMYPIXELS - 1))
C
         X0 = MAX0( (IX - 10), 0 )
         Y0 = MAX0( (IY - 10), 0 )
         X1 = MIN0( (IX + 10), (VID.NUMXPIXELS - 1))
         Y1 = MIN0( (IY + 10), (VID.NUMYPIXELS - 1))
C SAVE IMAGE BELOW WHERE CURSOR WILL BE
         CALL GETIMAGE( X0, Y0, X1, Y1, BUFFER )
C NOW DRAW CURSOR
         CALL MOVETO( X0, IY, XY)
         DUMMY = LINETO( X1, IY)
         CALL MOVETO( IX, Y0, XY)
         DUMMY = LINETO( IX, Y1)
         CALL GETCH(ICHR,SCAN)
C RESTORE IMAGE
         CALL PUTIMAGE( X0, Y0, BUFFER, ACTION )
C CALCULATE TIME PAST AND ACCELERATE IF NECESSARY
         CALL GETTIM(IHR,IMIN,ISEC,ITICK)
         CNT(2) = ITICK + 100*ISEC + 6000*IMIN
         IF ((CNT(2)-CNT(1)) .LT. 25) THEN
         INC = MIN0((INC + 1),30)
         CNT(1) = CNT(2)
         ELSE
         INC = 1
         CNT(1) = CNT(2)
         ENDIF
         IF(SCAN .EQ. #48) THEN
         IY = IY - INC
         ELSE IF (SCAN .EQ. #50) THEN
         IY = IY + INC
         ELSE IF(SCAN .EQ. #4D) THEN
         IX = IX + INC
         ELSE IF(SCAN .EQ. #4B) THEN
         IX = IX - INC
         ENDIF
      ENDDO
      DEALLOCATE( BUFFER )
      CHR = ICHR
      RETURN
      END
C---
C   These functions are discussed in Chapter 3 of the Advanced Topics
C       manual. The program DEMOEXEC.FOR illustrates how to use the include
C       file and the functions.
C   Function:   INTDOS
C
C   Purpose:   calls dos interrupt
C
C   Argument:   STRUCTURE - REGISTERS
C         STRUCTURE - RETURNED REGISTERS
C
C   Return:    INTEGER*2 - ax REGISTER
C
C   Example:   dummy2 = INTDOS(INREG,OUTREG)
C
C
      INTERFACE TO FUNCTION INTDOS [C] (INREG,OUTREG)
      INTEGER*2 INTDOS
      STRUCTURE/REGS/
      INTEGER*2 AX
      INTEGER*2 BX
      INTEGER*2 CX
      INTEGER*2 DX
      INTEGER*2 SI
      INTEGER*2 DI
      INTEGER*2 CFLAG
      END STRUCTURE
      RECORD/REGS/INREG [REFERENCE]
      RECORD/REGS/OUTREG [REFERENCE]
      END
CC       DEMOEXEC.FOR - Demonstration program for calling C system and
CC       spawnp library functions. These functions are included in the
CC       FORTRAN library. They are discussed in Chapter 3 of the Advanced
CC       Topics manual.
CC
CC       To compile and link DEMOEXEC.FOR type the command:
CC
CC      FL DEMOEXEC.FOR
      SUBROUTINE GETCH(CHR,SCAN)
      INTEGER*1 CHR,SCAN
C
C
C       Declare return types
C
      INTEGER*2 INTDOS
      STRUCTURE/REGS/
         INTEGER*1 AL, AH
         INTEGER*1 BL, BH
         INTEGER*1 CL, CH
         INTEGER*1 DL, DH
         INTEGER*1 SIL, SIH
         INTEGER*1 DIL, DIH
         INTEGER*1 CFL,CFH
      END STRUCTURE
      RECORD/REGS/INREGS
      RECORD/REGS/OUTREGS
C
      CHR= 0
      SCAN = 0
      INREGS.AH=#08
      INREGS.AL=0
      I = INTDOS( INREGS , OUTREGS )
      CHR = OUTREGS.AL
      IF(CHR .EQ. 0) THEN
         I = INTDOS( INREGS , OUTREGS )
         SCAN = OUTREGS.AL
      ENDIF
C   write(*,'(1x,z2)') OUTREGS.AL
      END
