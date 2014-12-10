C*PKDRIV -- PGPLOT Peritek VCK-Q 1024 driver
C+
      SUBROUTINE PKDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Peritek Corp. VCH-Q display
C
C Version .96  - 1989 Mar 30 - Christopher K. Lee / T. J. Pearson.
C Version .97  - 1989 Jul 14 - color/monochrome monitors.
C
C
C Supported device: Peritek Corp. VCK-Q frame-buffer video 
C interface with HD63484 ACRTC, using a custom VMS device driver.
C (Peritek Corp., 5550 Redwood Rd., Oakland, CA 94619, 415-531-6500)
C
C Device type code: /PK.
C
C Default device name: PKA0:
C
C Default view surface dimensions: Depends on monitor.
C
C Resolution: The full view surface is 1024 x 1024 pixels. 
C
C Color capability: Color indices 0-255 are supported. The
C representation of all color indices can be changed. Define
C PGPLOT_MONITOR = MONOCHROME to use a monochrome monitor: colors
C are then converted to shades of gray.
C
C Input capability: The graphics cursor is a white cross-hair. The 
C user positions the cursor using the arrow keys and PF1-PF4 keys on
C his terminal keyboard (SYS$COMMAND). The arrow keys move the cursor in
C the appropriate direction; the size of the step for each keystroke is
C controlled by the PF1-PF4 keys: PF1 -> 1 pixel, PF2 -> 4 pixels,
C PF3 -> 16 pixels, PF4 -> 64 pixels. The user indicates that the
C cursor has been positioned by typing any character other than an
C arrow or PF1-PF4 key [control characters, eg, ctrl-C, and other
C special characters should be avoided, as they may be intercepted by
C the operating system]. 
C
C File format: It is not possible to send Peritek VCK-Q plots to a 
C disk file.
C
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE
      PARAMETER (TYPE='PK    (Peritek Image Display)')
      INTEGER UNIT, I, IER, I0, I1, J0, J1, IX, IY, ICH, L
      LOGICAL APPEND, MONO
      CHARACTER*10 MSG
      INTEGER*2 IC, IR, IG, IB, LOC(2), KWORD
      INTEGER GRPK00, SYS$DASSGN
      BYTE PXLINE(1024), KBYTE
      EQUIVALENCE (KBYTE, KWORD)
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in PK device driver:'
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
      RBUF(2) = 1023
      RBUF(3) = 0
      RBUF(4) = 1023
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 100.
      RBUF(2) = 100.
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No area fill,
C    no thick lines, no rectangle fill, pixel)
C
   40 CHR = 'ICNNNNPNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = 'PKA0:'
      LCHR = 5
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 1023
      RBUF(3) = 0
      RBUF(4) = 1023
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 1
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
C     -- Monochrome monitor?
      CALL GRGENV('MONITOR', MSG, L)
      MONO = (MSG(1:1).EQ.'M' .OR. MSG(1:1).EQ.'m') 
C     -- Append flag?
      APPEND = RBUF(3).NE.0.0
C     -- Open device
      IER = GRPK00(CHR, LCHR, UNIT)
      RBUF(1) = UNIT
      RBUF(2) = IER
      NBUF = 2
      IF (IER.NE.1) RETURN
      IC = 1
C     -- skip initialization if "APPEND" requested
      IF (APPEND) RETURN
      CALL GRPK03(UNIT,MONO)
C     CALL GRPK04(2,UNIT)
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      IER = SYS$DASSGN(%val(UNIT))
      IF (IER.NE.1) THEN
          CALL GRWARN('Error closing graphics device.')
          CALL GRGMSG(IER)
      END IF
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      IF (.NOT.APPEND) CALL GRPK04(2, UNIT)
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      CALL GRPK01(I0, J0, I1, J1, IC, UNIT)
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      I0 = NINT(RBUF(1))                  
      J0 = NINT(RBUF(2))
      CALL GRPK07(I0, J0, IC, UNIT)
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
C     IF (RBUF(1).NE.0.) CALL GRPK04(2,UNIT)
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = RBUF(1)
      IF (IC.LT.0 .OR. IC.GT.255) THEN
          IC = 1
          RBUF(1) = IC
      END IF
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      RETURN
C                               
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
      IX = NINT(RBUF(1))
      IY = NINT(RBUF(2))
      CALL GRPK05(IX, IY, ICH, IER, UNIT)
      IF (IER.EQ.1) THEN
          RBUF(1) = IX
          RBUF(2) = IY
          CHR = CHAR(ICH)       
      ELSE
          CHR = CHAR(0)
      END IF
      NBUF = 2
      LCHR = 1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      CALL GRPK04(1,UNIT)
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called)
C
  190 CONTINUE
      RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called)
C
  200 CONTINUE 
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      ICH = RBUF(1)
      IR = MIN(255.,MAX(RBUF(2)*255.,0.))
      IG = MIN(255.,MAX(RBUF(3)*255.,0.))
      IB = MIN(255.,MAX(RBUF(4)*255.,0.))
      CALL GRPK08 (ICH,IR,IG,IB,UNIT,MONO)
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called)
C
  220 CONTINUE
      RETURN
C
C--- IFUNC=23, Escape --------------------------------------------------
C    (Not implemented: ignored)
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle fill ------------------------------------------
C    (Not implemented: ignored)
C
  240 CONTINUE
      RETURN
C
C--- IFUNC=25, ---------------------------------------------------------
C    (Not implemented: ignored)
C
  250 CONTINUE
      RETURN
C
C--- IFUNC=26, Line of pixels ------------------------------------------
C
  260 CONTINUE
      LOC(2) = NINT(RBUF(1))
      LOC(1) = NINT(RBUF(2))
      DO 261 I=1,NBUF-2
          KWORD = RBUF(I+2)
          PXLINE(I) = KBYTE
  261 CONTINUE
      CALL GRPK02(4, NBUF-2, PXLINE, LOC, UNIT)
      RETURN
C-----------------------------------------------------------------------
      END

C*GRPK00 -- PGPLOT Peritek VCK-Q driver, open device
C+    
      INTEGER FUNCTION GRPK00 (GRFILE, GRFNLN, GRUNIT)
      CHARACTER*(*) GRFILE
      INTEGER GRFNLN, GRUNIT
C
C Returns:
C   GRPK00         : 1 if the device was opened successfully. Any 
C                    other value indicates that an error occurred.
C                    Usually the value is the VMS error code.
C 
C Arguments:
C   GRFILE (in/out) : requested device name (input); actual
C                     device assigned (output).
C   GRFNLN (in/out) : length of device name.
C   GRUNIT (output) : channel assigned by VMS.
C
C Subroutines called:
C   GRWARN
C   GRGMSG
C   VMS system services
C-----------------------------------------------------------------------
      INTEGER  DVI$_DEVCLASS, DVI$_DEVNAM
      PARAMETER (DVI$_DEVCLASS=4)
      PARAMETER (DVI$_DEVNAM=32)     
      INTEGER  DEVCLASS, ITMLIST(7), IOSB(2)
      INTEGER  IER, L, SYS$ASSIGN
      INTEGER  SYS$GETDVI, SYS$DASSGN, SYS$WAITFR
C    
C Assign an i/o channel.
C
      L = GRFNLN
      IER = SYS$ASSIGN(GRFILE(1:L), GRUNIT,,)
      IF (IER.NE.1) GOTO 100
C
C Check that device has correct characteristics, and obtain true
C device name.
C
      ITMLIST(1) = DVI$_DEVCLASS*2**16 + 4
      ITMLIST(2) = %LOC(DEVCLASS)
      ITMLIST(3) = 0
      ITMLIST(4) = DVI$_DEVNAM*2**16 + LEN(GRFILE)
      ITMLIST(5) = %LOC(GRFILE)
      ITMLIST(6) = %LOC(GRFNLN)
      ITMLIST(7) = 0
      IER = SYS$GETDVI(%VAL(0),,GRFILE(1:GRFNLN), ITMLIST,IOSB,,,)
      IF (.NOT.IER) GOTO 100
      IER = SYS$WAITFR(%VAL(0))
      IF (.NOT.IER) GOTO 100
      IF (.NOT.IOSB(1)) THEN
          IER = IOSB(1)
          GOTO 100
      END IF
      IF (DEVCLASS.NE.32 .AND. DEVCLASS.NE.96) THEN
          CALL GRWARN( GRFILE(1:GRFNLN)//
     2            ' is the wrong sort of device for plot type PK')
          GRPK00 = 32767      ! indicate error
          IER = SYS$DASSGN(%VAL(GRUNIT))
          RETURN
      END IF
C
C Successful completion.
C
      GRPK00 = 1
      RETURN
C
C Error exit.
C
  100 CALL GRWARN('Cannot open graphics device '//GRFILE(1:L))
      CALL GRGMSG(IER)
      GRPK00 = IER
C-----------------------------------------------------------------------
      END

C*GRPK01 -- PGPLOT Peritek VCK-Q driver, draw line segment
C+
      SUBROUTINE GRPK01 (ELEM1,LINE1,ELEM2,LINE2,CI,UNIT)
      INTEGER*2      ELEM1, ELEM2, LINE1, LINE2, CI
      INTEGER*4      UNIT
C
C ELEM1   I*4      start element 
C LINE1   I*4      start line
C ELEM2   I*4      stop element 
C LINE2   I*4      stop line
C CI      I*2      color index to be used
C UNIT    I*4      unit number for output
C
C Note: the arguments are specified as integer*2, but (on the VAX at
C least) integer*4 values may be used. The values should be in the
C range defined by the hardware: (0...1023).
C
C (20-Oct-1987)
C-----------------------------------------------------------------------
      INTEGER*2     DATA
      INTEGER*2     I1, J1, I2, J2
      INTEGER*2     BUF(11),LOC(2)
C
      DATA = CI
      I1 = ELEM1
      J1 = LINE1
      I2 = ELEM2
      J2 = LINE2
      BUF(1)= '4000'O            ! Write Parameter register 0 or COL0
      BUF(2)= DATA + ISHFT(DATA,8)
      BUF(3)= '4001'O            ! Write Parameter register 1 or COL1
      BUF(4)= BUF(2)
      BUF(5)= '100000'O          ! AMOVE
      BUF(6)= I1
      BUF(7)= J1
      BUF(8)= '104000'O          ! ALINE
      BUF(9)= I2    
      BUF(10)= J2
      BUF(11)= '146000'O          ! DOT
      CALL GRPK02(1,11,BUF,LOC,UNIT)
C
      END

C*GRPK02 -- PGPLOT Peritek VCK-Q driver, input/output data
C+
      SUBROUTINE GRPK02 (INOUT,N,BUFFER,LOC,UNIT)
      INTEGER INOUT, N, UNIT, LOC
      BYTE BUFFER(*)
C
C Arguments:
C
C INOUT (input) : 0 => read from frame buffer,
C                 1 => write to graphics chip,
C                 2 => erase alpha screen,
C                 3 => erase primary screen
C                 4 => write to frame buffer
C N (integer, input): the number of bytes or words to transfer.
C BUFFER (byte array, dimension at least N, input): the data
C      bytes to be put in the output buffer.
C LOC (integer*2 array): Line # and element # to read from/write to
C UNIT (input): channel number for output
C
C (20-Oct-1987)
C-----------------------------------------------------------------------
      INCLUDE '($IODEF)'
C
      INTEGER SYS$QIOW, RESULT, IOSB(2)
      INTEGER*2 STBC(2), STATUS, COUNT
      EQUIVALENCE (STBC, IOSB(1)), (STATUS, STBC(1)), (COUNT, STBC(2))
C
      IF (N.LT.1) RETURN
      IF (INOUT.EQ.0) THEN
C       Read from Frame Buffer
          RESULT = SYS$QIOW(,%VAL(UNIT),
     1                 %VAL(IO$_READVBLK),IOSB,,,
     2                 %VAL(2),BUFFER,%VAL(N),%VAL(LOC),,)
      ELSE IF (INOUT.EQ.1) THEN
C       Write to ACRTC FIFO register list
          RESULT = SYS$QIOW(,%VAL(UNIT),
     1                 %VAL(IO$_WRITEVBLK),IOSB,,,
     2                 %VAL(2),BUFFER,%VAL(N),,,)
      ELSE IF (INOUT.EQ.2) THEN
C       Clear Alpha 0 Screen
          RESULT = SYS$QIOW(,%VAL(UNIT),
     1                 %VAL(IO$_MODIFY),IOSB,,,
     2                 %VAL(1),,,,,)
      ELSE IF (INOUT.EQ.3) THEN
C       Clear Primary Screen
          RESULT = SYS$QIOW(,%VAL(UNIT),
     1                 %VAL(IO$_MODIFY),IOSB,,,
     2                 %VAL(2),,,,,)
      ELSE IF (INOUT.EQ.4) THEN
C       Write to Frame Buffer
          RESULT = SYS$QIOW(,%VAL(UNIT),
     1                 %VAL(IO$_WRITEVBLK),IOSB,,,
     2                 %VAL(4),BUFFER,%VAL(N),%VAL(LOC),,)
      END IF
      IF (RESULT.NE.1) THEN
          CALL GRGMSG(RESULT)
          CALL GRQUIT('PK02A: SYS$QIO failure writing to TV')
      END IF
      IF (STATUS.NE.1) THEN
          RESULT = STATUS
          CALL GRGMSG(RESULT)
          CALL GRQUIT('PK02B: SYS$QIO failure writing to TV')
      END IF
      END

C*GRPK03 -- PGPLOT Peritek VCK-Q driver, initialize color display
C+
      SUBROUTINE GRPK03 (UNIT,MONO)
      INTEGER UNIT
      LOGICAL MONO
C
C Initialize the color tables to standard values.
C
C (14-Jul-1989)
C-----------------------------------------------------------------------
      INTEGER*2      I, GCTABL(3,16)
      DATA GCTABL /000,000,000, 255,255,255, 255,000,000, 000,255,000,
     1             000,000,255, 000,255,255, 255,000,255, 255,255,000,
     2             255,128,000, 128,255,000, 000,255,128, 000,128,255,
     3             128,000,255, 255,000,128, 085,085,085, 170,170,170/
C
      DO 20 I=1,16
          CALL GRPK08(I-1, GCTABL(1,I), GCTABL(2,I), GCTABL(3,I), UNIT,
     1                MONO)
   20 CONTINUE
      END

C*GRPK04 -- PGPLOT Peritek VCK-Q driver, clear screens
C+
      SUBROUTINE GRPK04 (ISCR,UNIT)
      INTEGER ISCR,UNIT
C
C Arguments:
C   ISCR     (input) : 1 = clear alpha screen, 2 = clear primary screen
C   UNIT     (input) : channel number for output
C-----------------------------------------------------------------------
      INTEGER*2      BUF(4),LOC(2)
C-----------------------------------------------------------------------
      IF (ISCR.EQ.1) THEN
        CALL GRPK02(2,4,BUF,LOC,UNIT)
      ELSE IF (ISCR.EQ.2) THEN
        CALL GRPK02(3,4,BUF,LOC,UNIT)
      END IF
      END

C*GRPK05 -- PGPLOT Peritek VCK-Q driver, cursor routine
C+
      SUBROUTINE GRPK05 (IX, IY, IC, IER, UNIT)
      INTEGER IX, IY, IC, IER, UNIT
C
C Arguments:
C   IX, IY (in/out) : initial/final coordinates of cursor (device 
C                     coordinates).
C   IC     (output) : character code.
C   IER    (output) : error status (1 => OK).
C   UNIT   (input)  : channel for output to device.
C
C The Peritek device has no hardware cursor. The cursor has to be
C emulated by writing into the image array, and to delete it the 
C former contents must be restored. The cursor is a 5x5 pixel array;
C to avoid edge effects, it cannot be moved within 2 pixels of the 
C edge of the screen.
C
C The cursor is moved by using the arrow keys on the
C terminal; the cursor "speed" (step size) is controlled by the
C PF1 (smallest step) to PF4 (largest step) keys.
C
C The user indicates that the cursor has been positioned by
C typing any character on his keyboard (SYS$COMMAND), with the
C following exceptions: control characters (^C, ^O, ^Q, ^R, ^S,
C ^T, ^U, ^X, ^Y, DEL) are intercepted by the operating system
C and cannot be used; NUL, ESC (^[) and escape sequences (e.g., 
C arrow keys) are ignored.
C 
C (20-Oct-1987)
C-----------------------------------------------------------------------
C CURSIZ is the size of the cursor array: must be odd
      INTEGER        CURSIZ, CURCEN
      PARAMETER      (CURSIZ=15, CURCEN=(CURSIZ+1)/2)
      INTEGER*2      IXG,IYG, LOC2(2)
      INTEGER        GRGETC, SYS$ASSIGN, SYS$DASSGN
      INTEGER        STEP, ICHAN, I, J
      DATA           STEP/4/      ! initial step size
      BYTE           CU1(CURSIZ,CURSIZ), CU2(CURSIZ,CURSIZ)
C-----------------------------------------------------------------------
      IER = SYS$ASSIGN('SYS$COMMAND',ICHAN,,)
      IF (IER.NE.1) RETURN
      IX = MAX(CURCEN-1,MIN(IX,1023-CURCEN+1))
      IY = MAX(CURCEN-1,MIN(IY,1023-CURCEN+1))
   10 IXG = IX
      IYG = IY
C     -- Make cursor visible
      LOC2(2) = IXG-CURCEN+1
      DO J=1,CURSIZ
          LOC2(1) = IYG-CURCEN+J
          CALL GRPK02(0,CURSIZ,CU1(1,J),LOC2,UNIT)
      END DO
      DO I=1,CURSIZ
         DO J=1,CURSIZ
           CU2(I,J) = CU1(I,J)
         END DO      
      END DO
      DO I=1,CURSIZ
          CU2(I,CURCEN) = 1
          CU2(CURCEN,I) = 1
          CU2(I,I) = 0
          CU2(I,CURSIZ+1-I) = 0
      END DO
      DO J=1,CURSIZ
          LOC2(1) = IYG-CURCEN+J
          CALL GRPK02(4,CURSIZ,CU2(1,J),LOC2,UNIT)
      END DO
      IC = GRGETC(ICHAN)
      IF (IC.EQ.-1) THEN
          IY = MIN(1023-CURCEN+1,IY+STEP)
      ELSE IF (IC.EQ.-2) THEN
          IY = MAX(CURCEN-1,IY-STEP)
      ELSE IF (IC.EQ.-3) THEN
          IX = MIN(1023-CURCEN+1,IX+STEP)
      ELSE IF (IC.EQ.-4) THEN
          IX = MAX(CURCEN-1,IX-STEP)
      ELSE IF (IC.EQ.-11) THEN
          STEP = 1
      ELSE IF (IC.EQ.-12) THEN
          STEP = 4
      ELSE IF (IC.EQ.-13) THEN
          STEP = 16
      ELSE IF (IC.EQ.-14) THEN
          STEP = 64
      END IF
C     --- Erase cursor by restoring former image contents
      DO J=1,CURSIZ
          LOC2(1) = IYG-CURCEN+J
          CALL GRPK02(4,CURSIZ,CU1(1,J),LOC2,UNIT)
      END DO
      IF (IC.LE.0 .OR. IC.GT.255) GOTO 10
      IER = SYS$DASSGN(%VAL(ICHAN))
C-----------------------------------------------------------------------
      END                        

C*GRPK07 -- PGPLOT Peritek VCK-Q driver, draw a dot
C+
      SUBROUTINE GRPK07 (ELEM1,LINE1,CI,UNIT)
      INTEGER*2      ELEM1, LINE1, CI
      INTEGER*4      UNIT
C
C ELEM1   I*4      dot element 
C LINE1   I*4      dot line
C CI      I*2      color index to be used
C UNIT    I*4      unit number for output
C
C Note: the arguments are specified as integer*2, but (on the VAX at
C least) integer*4 values may be used. The values should be in the
C range defined by the hardware: (0...1023).
C
C (20-Oct-1987)
C-----------------------------------------------------------------------
      INTEGER*2           DATA
      INTEGER*2     I0, J0, BUF(8), LOC(2)
C
      DATA = CI
      I0 = ELEM1
      J0 = LINE1
      BUF(1)= '4000'O            ! Write Parameter Register 0 or COL0
      BUF(2)= DATA + ISHFT(DATA,8)
      BUF(3)= '4001'O            ! Write Parameter Register 1 or COL1
      BUF(4)= BUF(2)
      BUF(5)= '100000'O          ! AMOVE
      BUF(6)= I0
      BUF(7)= J0
      BUF(8)= '146000'O          ! DOT
      CALL GRPK02(1,8,BUF,LOC,UNIT)
C
      END

C*GRPK08 -- PGPLOT Peritek VCK-Q driver, set color representation
C+
      SUBROUTINE GRPK08 (CIN,R,G,B,UNIT,MONO)
      INTEGER*2 CIN, R, G, B
      INTEGER UNIT
      LOGICAL MONO
C
C Arguments:
C   CIN      I*2     LUT index 0 to 255
C   R,G,B    I*2     Color component 0 to 255
C   UNIT     I*4     Channel number for output
C   MONO     L*4     If .TRUE., convert color to gray shade.
C
C (14-Jul-1989)
C-----------------------------------------------------------------------
      INTEGER*2      MCLR(2), DATA
C
      MCLR(1)=0
      IF (MONO) THEN
          DATA = NINT(0.30*R+0.59*G+0.11*B)
          MCLR(2)=7
          CALL GRPK09(1,DATA,1,CIN,MCLR,UNIT)
      ELSE
          DATA = R
          MCLR(2)=1
          CALL GRPK09(1,DATA,1,CIN,MCLR,UNIT)
          DATA = G
          MCLR(2)=2
          CALL GRPK09(1,DATA,1,CIN,MCLR,UNIT)
          DATA = B
          MCLR(2)=4
          CALL GRPK09(1,DATA,1,CIN,MCLR,UNIT)
      END IF
C
      END

C*GRPK09 -- PGPLOT Peritek VCK-Q driver, input/output CMM data
C+
      SUBROUTINE GRPK09 (INOUT,BUFFER,N,ELEM,MCLR,UNIT)
      INTEGER INOUT, N, ELEM, UNIT, MCLR
      BYTE BUFFER(*)
C
C Arguments:
C
C INOUT (input) : 1 => write, 0 => read
C N (input) : # bytes to transfer
C ELEM (input) : element to write to.
C BUFFER (byte array, dimension at least N, input): the data
C      bytes to be put in the output buffer.
C MCLR (integer*2 array): which map # and which color to modify
C      bit set 0=red, 1=green, 2=blue.
C UNIT (input): channel number for output
C
C (20-Oct-1987)
C-----------------------------------------------------------------------
      INCLUDE '($IODEF)'
C
      INTEGER SYS$QIOW, RESULT, IOSB(2), LOC
      INTEGER*2 STBC(2), STATUS, COUNT, WORD(2)
      EQUIVALENCE (STBC, IOSB(1)), (STATUS, STBC(1)), (COUNT, STBC(2))
      EQUIVALENCE (LOC,WORD(1))
C
      IF (N.LT.1) RETURN
      IF (INOUT.EQ.1) THEN
          WORD(1)=N
          WORD(2)=ELEM
          RESULT = SYS$QIOW(,%VAL(UNIT),
     1                 %VAL(IO$_WRITEVBLK),IOSB,,,
     2                 %VAL(3),BUFFER,%VAL(LOC),%VAL(MCLR),,)
      ELSE                 
          STATUS = 1
          RESULT = 1
      END IF
      IF (RESULT.NE.1) THEN
          CALL GRGMSG(RESULT)
          CALL GRQUIT('PK09A: SYS$QIO failure writing to TV')
      END IF
      IF (STATUS.NE.1) THEN
          RESULT = STATUS
          CALL GRGMSG(RESULT)
          CALL GRQUIT('PK09B: SYS$QIO failure writing to TV')
      END IF
      END
