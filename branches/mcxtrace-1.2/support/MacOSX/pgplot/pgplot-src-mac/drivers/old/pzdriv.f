C*PZDRIV -- PGPLOT Peritek driver
C+
      SUBROUTINE PZDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Peritek Corp. VCH-Q display
C
C Version 1.0  - 1987 May 26 - T. J. Pearson.
C Version 1.1  - 1987 Jun  4 - add cursor support (TJP).
C Version 1.2  - 1987 Jul  1 - larger cursor (TJP).
C
C
C Supported device: Peritek Corp. VCH-Q frame-buffer video 
C interface with HD6845 CRTC, using a custom VMS device driver.
C (Peritek Corp., 5550 Redwood Rd., Oakland, CA 94619, 415-531-6500)
C
C Device type code: /PERITEK.
C
C Default device name: TV_DEVICE (a logical name, usually
C defined by the system manager).
C
C Default view surface dimensions: Depends on monitor.
C
C Resolution: The full view surface is 512 x 512 pixels. 
C
C Color capability: Color indices 0-255 are supported. The
C representation of all color indices can be changed. As the
C device is normally connected to a monochrome monitor, all
C colors are converted to grayscale by averaging the R, G, B
C components.
C
C Input capability: Not implemented.
C
C File format: It is not possible to send plots to a disk file.
C
C Obtaining hardcopy: Not possible.
C
C-----------------------------------------------------------------------
C The driver makes use of the following PGPLOT support routines:
C    GRGETC
C    GRGMSG
C    GRQUIT
C    GRWARN
C-----------------------------------------------------------------------
* The following notes describe the VMS device-driver interface used
* with this device. The QIO functions supported are: 
* 
*       IO$_WRITEVBLK  -  write virtual block,
*       IO$_READVBLK   -  read virtual block,
*       IO$_MODIFY     -  set display mode,
*       IO$_ACCESS     -  write block of x,y,z values.
* 
* IO$_WRITEVBLK: Consecutive bytes (8 bits) of data are transferred
* from a user specified buffer to consecutively addressed elements in
* either the graphics display memory or the color map memory of the
* VCH-Q, starting at a specified element.  The required device
* specific QIO parameters are: 
* 
*       P1 - the address of the data buffer;
*       P2 - the number of bytes of data to be transferred;
*       P3 - a longword containing the initial line number in
*            its least significant word and the initial element
*            number in its most significant word.
* 
* A write operation to the graphics display memory is signalled by an
* initial line number that is non-negative.  In this case the effective
* initial line number and the effective initial element number are 
* calculated from the values, line0 and elem0, in P3 as follows:
* 
*       initial line = (line0 + (elem0 / 512)) modulo 512, and
*       initial element = elem0 modulo 512.
* 
* Line number 0 refers to the bottom line of the display and line
* number 511 refers to the top line. Element number 0 corresponds to
* the leftmost element of the line and element number 511, the
* rightmost.  The data are written to memory elements consecutively
* addressed by incrementing the element number modulo 512 with
* wrap-around to element 0 of the next higher line number modulo 512. 
* 
* A write operation to the color map memory is signalled by an initial
* line number that is negative.  The value of the initial line number
* selects which of the color maps are to be written: 
* 
*       - 1:  bottom red,
*       - 2:  bottom green,
*       - 4:  bottom blue,
*       - 8:  top red,
*       -16:  top green, and
*       -32:  top blue.
* 
* Several of the color maps may be selected in a write operation by
* giving an initial line number which is the sum of the values, above,
* for the individual desired maps.  The elements of a color map are
* numbered from 0 to 255 and the given initial element number and the
* value of the P2 parameter must not be such as to write past the end
* of the map (element 255). 
* 
* IO$_READVBLK: Except that the direction of data transfer is
* reversed, the same description applies as is given above for the
* write function but with one other difference:  only one color map
* may be selected at a time in a read. 
* 
* IO$_MODIFY: The required device specific QIO parameter is:
* 
*       P1 - a longword parameter consisting of two packed word-
*            length booleans:  bit 0, use the top mapbit 16,
*            interpixellate the display.  The bit values are
*            0: false, 1: true.
* 
* IO$_ACCESS: used to modify individual bits in individual pixels in
* the image memory. The required device specific QIO parameters are: 
* 
*       P1 - a longword that is the start of a data buffer
*       P2 - the length of the data buffer in bytes
* 
* The data buffer is organized in groups of 8 byte records. Each 8
* byte record specifies one location in the VCH-Q image memory that is
* to be modified. It also specifies which of the 8 bits to modify in
* that location. The format of a record is 4 16-bit integers organized
* in the following way: 
* 
*       _________
*       |       |  element within a line, modulo 512 ( x )
*       ---------
*       |       |  line number, modulo 512 ( y )
*       ---------
*       |       |  bit pattern to use, modulo 256 ( z )
*       ---------
*       |       |  bit pattern modification mask, modulo 256
*       ---------
* 
* The bit modification mask determines which bits are to be changed in
* the specified location of the image memory. A 1 in a bit in the mask
* indicates that the final value of a bit in memory should be taken
* from the value in bit pattern word of the record. A 0 in a bit in
* the mask indicates that the final value of a bit in memory should be
* the value already in memory. For example, a modification mask of all
* 1's tells the driver to replace the value in memory with the bit
* pattern in the record, whereas a mask of all 0's tells the driver to
* leave the memory alone. 
C-----------------------------------------------------------------------
      INTEGER   UNIT, IER, I0, I1, J0, J1, IX, IY, ICH
      LOGICAL   APPEND
      CHARACTER*10 MSG
      INTEGER*2 IC, IR, IG, IB
      INTEGER   GRPZ00, SYS$DASSGN
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in PERITEK device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = 'PERITEK'
      LCHR = 7
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 511
      RBUF(3) = 0
      RBUF(4) = 511
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 50.0
      RBUF(2) = 50.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No area 
C    fill, No thick lines)
C
   40 CHR = 'ICNNNNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = 'TV_DEVICE'
      LCHR = 9
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 511
      RBUF(3) = 0
      RBUF(4) = 511
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
      APPEND = RBUF(3).NE.0.0
      IER = GRPZ00(CHR, LCHR, UNIT)
      RBUF(1) = UNIT
      RBUF(2) = IER
      NBUF = 2
      IC = 1
C     -- skip initialization if "APPEND" requested
      IF (APPEND) RETURN
      CALL GRPZ05(UNIT)
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      IER = SYS$DASSGN(%val(UNIT))
      IF (IER.NE.1) THEN
          CALL GRWARN('Error closing graphics device')
          CALL GRGMSG(IER)
      END IF
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      IF (.NOT.APPEND) CALL GRPZ04(UNIT)
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      CALL GRPZ01 (I0, J0, I1, J1, IC, UNIT)
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      CALL GRPZ01 (I0, J0, I0, J0, IC, UNIT)
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
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
      CALL GRPZ06(IX, IY, ICH, IER, UNIT)
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
C
  210 CONTINUE
      ICH = RBUF(1)
      IR = MIN(255.,MAX(RBUF(2)*255.,0.))
      IG = MIN(255.,MAX(RBUF(3)*255.,0.))
      IB = MIN(255.,MAX(RBUF(4)*255.,0.))
      CALL GRPZ03 (ICH,IR,IG,IB,UNIT)
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

C*GRPZ00 -- PGPLOT Peritek driver, open device
C+
      INTEGER FUNCTION GRPZ00 (GRFILE, GRFNLN, GRUNIT)
      CHARACTER*(*) GRFILE
      INTEGER GRFNLN, GRUNIT
C
C Returns:
C   GRPZ00         : 1 if the device was opened successfully. Any 
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
      IF (.NOT.IER) GOTO 100
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
     2            ' is the wrong sort of device for plot type PERITEK')
          GRPZ00 = 32767      ! indicate error
          IER = SYS$DASSGN(%VAL(GRUNIT))
          RETURN
      END IF
C
C Successful completion.
C
      GRPZ00 = 1
      RETURN
C
C Error exit.
C
  100 CALL GRWARN('Cannot open graphics device '//GRFILE(1:L))
      CALL GRGMSG(IER)
      GRPZ00 = IER
C-----------------------------------------------------------------------
      END

C*GRPZ01 -- PGPLOT Peritek driver, draw line segment
C+
      SUBROUTINE GRPZ01 (ELEM1,LINE1,ELEM2,LINE2,CI,UNIT)
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
C range defined by the hardware: (0...511).
C
C (28-May-1987)
C-----------------------------------------------------------------------
      BYTE           DATA
      REAL*4         SLOPE, DELEM, DLINE
      INTEGER*2      LOC(2)
      INTEGER*2     I1, J1, I2, J2, STEP
      INTEGER       I, J, N
      BYTE          BUF(512)
C
      DATA = CI
      I1 = ELEM1
      J1 = LINE1
      I2 = ELEM2
      J2 = LINE2
      DLINE = J2-J1
      DELEM = I2-I1
      SLOPE = 1.0E10
C
      IF (DELEM .NE. 0) SLOPE = DLINE/DELEM
      IF (DLINE.EQ.0) THEN
          LOC(2) = MIN(I1,I2)
          LOC(1) = J1
          N = ABS(I1-I2)+1
          DO 5 I=1,N
              BUF(I) = DATA
    5     CONTINUE
          CALL GRPZ02(1,N,BUF,LOC,UNIT)
      ELSE IF (ABS(SLOPE) .LT. 1.0) THEN
          STEP = IISIGN(1,(I2-I1))
          DO 10 I = I1, I2, STEP
              LOC(2) = I
              LOC(1) = (LOC(2)-I1)*SLOPE + J1      
              CALL GRPZ02(1,1,DATA,LOC,UNIT)
   10     CONTINUE
C
      ELSE IF (DLINE .NE. 0) THEN
          SLOPE = DELEM/DLINE
          STEP = IISIGN(1,(J2-J1))
          DO 20 J = J1, J2, STEP
              LOC(1) = J
              LOC(2) = (LOC(1) - J1)*SLOPE + I1
              CALL GRPZ02(1,1,DATA,LOC,UNIT)
   20     CONTINUE
C
      ELSE
          LOC(1) = J1
          LOC(2) = I1
          CALL GRPZ02(1,1,DATA,LOC,UNIT)
      END IF
      END

C*GRPZ02 -- PGPLOT Peritek driver, input/output data
C+
      SUBROUTINE GRPZ02 (INOUT,N,BUFFER,LOC,UNIT)
      INTEGER INOUT, N, UNIT
      BYTE BUFFER(*)
      INTEGER*4 LOC
C
C Arguments:
C
C INOUT (input) : 1 => write, 0 => read
C N (integer, input): the number of bytes to transfer.
C BUFFER (byte array, dimension at least N, input): the data
C      bytes to be put in the output buffer.
C LOC (integer*2 array, dimension 2): location of the beginning 
C      of the string of N data points.  (I,J)
C UNIT (input): channel number for output
C
C (28-May-1987)
C-----------------------------------------------------------------------
      INCLUDE '($IODEF)'
C
      INTEGER SYS$QIOW, RESULT, IOSB(2)
      INTEGER*2 STBC(2), STATUS, COUNT
      EQUIVALENCE (STBC, IOSB(1)), (STATUS, STBC(1)), (COUNT, STBC(2))
C
      IF (N.LT.1) RETURN
      IF (INOUT.EQ.1) THEN
          RESULT = SYS$QIOW(,%VAL(UNIT),
     1                 %VAL(IO$_WRITEVBLK),IOSB,,,
     2                 BUFFER,%VAL(N),%VAL(LOC),,,)
      ELSE
          RESULT = SYS$QIOW(,%VAL(UNIT),
     1                 %VAL(IO$_READVBLK),IOSB,,,
     2                 BUFFER,%VAL(N),%VAL(LOC),,,)
      END IF
      IF (RESULT.NE.1) THEN
          CALL GRGMSG(RESULT)
          CALL GRQUIT('SYS$QIO failure writing to TV')
      END IF
      IF (STATUS.NE.1) THEN
          RESULT = STATUS
          CALL GRGMSG(RESULT)
          CALL GRQUIT('SYS$QIO failure writing to TV')
      END IF
      END

C*GRPZ03 -- PGPLOT Peritek driver, set color representation
C+
      SUBROUTINE GRPZ03 (CIN,R,G,B,UNIT)
      INTEGER*2 CIN, R, G, B
      INTEGER UNIT
C
C Arguments:
C   CIN      I*2     LUT index 0 to 255
C   R,G,B    I*2     Color component 0 to 255
C   UNIT     I*4     Channel number for output
C
C *** NB *** This converts requested color index to monochrome shade
C as all known installations of this device have monochrome monitors
C
C (28-May-1987)
C-----------------------------------------------------------------------
      INTEGER*2      LOC2(2), MONO
C
      MONO = NINT((R+G+B)/3.0)
C
      LOC2(2) = CIN
      LOC2(1) = -1
      CALL GRPZ02(1,1,MONO,LOC2,UNIT)
C
      LOC2(1) = -2
      CALL GRPZ02(1,1,MONO,LOC2,UNIT)
C
      LOC2(1) = -4
      CALL GRPZ02(1,1,MONO,LOC2,UNIT)
C
      END

C*GRPZ04 -- PGPLOT Peritek driver, clear screen
C+
      SUBROUTINE GRPZ04(UNIT)
      INTEGER UNIT
C
C Clears the tv by setting all pixel values to 0.
C
C (7-Jun-1987)
C-----------------------------------------------------------------------
      INTEGER*4      BUF(128), J
      INTEGER*2      LOC(2)
      DATA BUF/128*0/
C
      LOC(2) = 0
C
C Zero array.
C
      DO 20 J=0,511
          LOC(1) = J
          CALL GRPZ02(1,512,BUF,LOC,UNIT)
   20 CONTINUE
      END

C*GRPZ05 -- PGPLOT Peritek driver, initialize display
C+
      SUBROUTINE GRPZ05 (UNIT)
      INTEGER UNIT
C
C Initialize the display. Note that the screen is
C NOT erased. The color tables are initialized as follows:
C
C Index      Name                     (H,L,S)               (R,G,B)
C
C  0      Black                    0, 0.0, 0.0            0.0, 0.0, 0.0
C  1      White                    0, 1.0, 0.0            1.0, 1.0, 1.0
C  2      Red                    120, 0.5, 1.0            1.0, 0.0, 0.0
C  3      Green                  240, 0.5, 1.0            0.0, 1.0, 0.0
C  4      Blue                     0, 0.5, 1.0            0.0, 0.0, 1.0
C  5      Cyan    (Green+Blue)   300, 0.5, 1.0            0.0, 1.0, 1.0
C  6      Magenta (Red+Blue)      60, 0.5, 1.0            1.0, 0.0, 1.0
C  7      Yellow  (Red+Green)    180, 0.5, 1.0            1.0, 1.0, 0.0
C  8      Red+Yellow (Orange)    150, 0.5, 1.0            1.0, 0.5, 0.0
C  9      Green+Yellow           210, 0.5, 1.0            0.5, 1.0, 0.0
C 10      Green+Cyan             270, 0.5, 1.0            0.0, 1.0, 0.5
C 11      Blue+Cyan              330, 0.5, 1.0            0.0, 0.5, 1.0
C 12      Blue+Magenta            30, 0.5, 1.0            0.5, 0.0, 1.0
C 13      Red+Magenta             90, 0.5, 1.0            1.0, 0.0, 0.5
C 14      Dark Gray                0, .33, 0.0            .33, .33, .33
C 15      Light Gray               0, .66, 0.0            .66, .66, .66
C
C (28-May-1987)
C-----------------------------------------------------------------------
      BYTE           LUT(256), LUT1
      INTEGER*2      LUT2, LOC2(2), GCTABL(3,16)
      INTEGER*4      I
C      
      EQUIVALENCE      (LUT1,LUT2)
      DATA GCTABL /000,000,000, 255,255,255, 255,000,000, 000,255,000,
     1             000,000,255, 000,255,255, 255,000,255, 255,255,000,
     2             255,128,000, 128,255,000, 000,255,128, 000,128,255,
     3             128,000,255, 255,000,128, 085,085,085, 170,170,170/
C
C                              INITIALIZE RED TABLE TO ABOVE
      LOC2(1) = -9
      LOC2(2) = 0
      DO 20 I=1,16
          LUT2 = (GCTABL(1,I)+GCTABL(2,I)+GCTABL(3,I))/3
          LUT(I) = LUT1
20    CONTINUE
      CALL GRPZ02(1,16,LUT,LOC2,UNIT)
C                              INITIALIZE GREEN TABLE TO ABOVE
      LOC2(1) = -18
      LOC2(2) = 0
      DO 30 I=1,16
          LUT2 = (GCTABL(1,I)+GCTABL(2,I)+GCTABL(3,I))/3
          LUT(I) = LUT1
30    CONTINUE
      CALL GRPZ02(1,16,LUT,LOC2,UNIT)
C                              INITIALIZE BLUE TABLE TO ABOVE
      LOC2(1) = -36
      LOC2(2) = 0
      DO 40 I=1,16
          LUT2 = (GCTABL(1,I)+GCTABL(2,I)+GCTABL(3,I))/3
          LUT(I) = LUT1
40    CONTINUE
      CALL GRPZ02(1,16,LUT,LOC2,UNIT)
C
      END

C*GRPZ06 -- PGPLOT Peritek driver, cursor routine
C+
      SUBROUTINE GRPZ06 (IX, IY, IC, IER, UNIT)
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
C (1-Jul-1987)
C-----------------------------------------------------------------------
C CURSIZ is the size of the cursor array: must be odd
      INTEGER        CURSIZ, CURCEN
      PARAMETER      (CURSIZ=9, CURCEN=(CURSIZ+1)/2)
      INTEGER*2      IXG,IYG, LOC2(2)
      INTEGER        GRGETC, SYS$ASSIGN, SYS$DASSGN
      INTEGER        STEP, ICHAN, I, J
      DATA           STEP/4/      ! initial step size
      BYTE           CU1(CURSIZ,CURSIZ), CU2(CURSIZ,CURSIZ)
C-----------------------------------------------------------------------
      IER = SYS$ASSIGN('SYS$COMMAND',ICHAN,,)
      IF (IER.NE.1) RETURN
      IX = MAX(CURCEN-1,MIN(IX,511-CURCEN+1))
      IY = MAX(CURCEN-1,MIN(IY,511-CURCEN+1))
   10 IXG = IX
      IYG = IY
C     -- Make cursor visible
      LOC2(2) = IXG-CURCEN+1
      DO J=1,CURSIZ
          LOC2(1) = IYG-CURCEN+J
          CALL GRPZ02(0,CURSIZ,CU1(1,J),LOC2,UNIT)
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
          CALL GRPZ02(1,CURSIZ,CU2(1,J),LOC2,UNIT)
      END DO
      IC = GRGETC(ICHAN)
      IF (IC.EQ.-1) THEN
          IY = MIN(511-CURCEN+1,IY+STEP)
      ELSE IF (IC.EQ.-2) THEN
          IY = MAX(CURCEN-1,IY-STEP)
      ELSE IF (IC.EQ.-3) THEN
          IX = MIN(511-CURCEN+1,IX+STEP)
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
          CALL GRPZ02(1,CURSIZ,CU1(1,J),LOC2,UNIT)
      END DO
      IF (IC.LE.0 .OR. IC.GT.255) GOTO 10
      IER = SYS$DASSGN(%VAL(ICHAN))
C-----------------------------------------------------------------------
      END
