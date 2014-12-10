C*GRDRIV -- PGPLOT Grinnell driver
C+
      SUBROUTINE GRDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Grinnell device.
C
C Version 1.0 - 1987 May 26 - T. J. Pearson.
C Version 1.1 - 1987 Jun 6  - dynamically allocate buffer (TJP).
C Version 1.2 - 1988 Jan 11 - use SMG$ routines for keyboard control
C                             of cursor (TJP).
C Version 1.3 - 1988 Mar 16 - test GR_TYPE environment variable (TJP).
C Version 1.4 - 1988 Mar 28 - correct bug in /APPEND (TJP).
C Version 1.5 - 1989 Feb 11 - add MONOCHROME option (TJP).
C
C
C Supported device: Grinnell GMR-270 Image Display System. 
C
C Device type code: /GRINNELL.
C
C Default device name: TV_DEVICE (a logical name, usually
C defined by the system manager).
C
C Default view surface dimensions: Depends on monitor.
C
C Resolution: The full view surface is 512 x 512 pixels. 
C
C Color capability: Color indices 0-255 are supported. The
C representation of all color indices can be changed. 
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
C File format: It is not possible to send Grinnell plots to a 
C disk file.
C
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=8192)
      INTEGER BUFFER
      INTEGER BUFLEV
      INTEGER UNIT, IER, I0, I1, J0, J1, IX, IY, ICH, L
      INTEGER GRGR00, GRGMEM, GRFMEM, SYS$DASSGN
      INTEGER  SMG$CREATE_VIRTUAL_KEYBOARD
      INTEGER  SMG$SET_KEYPAD_MODE
      INTEGER  KBID
      LOGICAL APPEND, MONO
      CHARACTER*10 MSG
      INTEGER*2 GRNLBF(256), IC, IR, IG, IB, GCTABL(3,0:15), MONCOL
      INTEGER   NW, IW
      DATA GCTABL /000,000,000, 255,255,255, 255,000,000, 000,255,000,
     1             000,000,255, 000,255,255, 255,000,255, 255,255,000,
     2             255,128,000, 128,255,000, 000,255,128, 000,128,255,
     3             128,000,255, 255,000,128, 085,085,085, 170,170,170/
      LOGICAL NEW, INIT
      SAVE NEW, INIT, MONO
      DATA INIT /.TRUE./
C-----------------------------------------------------------------------
C
C First call: determine type of Grinnell.
C
      IF (INIT) THEN
          INIT = .FALSE.
          CALL GRGENV('GR_TYPE', MSG, L)
          NEW = (MSG(1:1).EQ.'P') 
C             -- Phobos-type Grinnell (GR273)
C             -- otherwise Deimos-type Grinnell (GR270)
          CALL GRGENV('MONITOR', MSG, L)
          MONO = (MSG(1:1).EQ.'M' .OR. MSG(1:1).EQ.'m') 
C             -- Monochrome monitor
C             -- otherwise color monitor
          IF (MONO) THEN
              DO IW=0,15
                  MONCOL = NINT(0.30*GCTABL(1,IW) + 0.59*GCTABL(2,IW) + 
     1                          0.11*GCTABL(3,IW))
                  GCTABL(1,IW) = MONCOL
                  GCTABL(2,IW) = MONCOL
                  GCTABL(3,IW) = MONCOL
              END DO
          END IF
      END IF
C
C Branch on opcode.
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in GRINNELL device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = 'GRINNELL'
      LCHR = 8
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
C    (This device is Interactive, Cursor, No dashed lines, No area fill,
C    no thick lines)
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
C     -- allocate buffer
      IER = GRGMEM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          RETURN
      END IF
C     -- open device
      IER = GRGR00(CHR, LCHR, UNIT)
      RBUF(1) = UNIT
      RBUF(2) = IER
      NBUF = 2
      IF (IER.NE.1) THEN
          CALL GRFMEM(BUFSIZ, BUFFER)
          RETURN
      END IF
      IC = 1
C     -- create a virtual keyboard for cursor control
      IER = SMG$CREATE_VIRTUAL_KEYBOARD(KBID, 'SYS$COMMAND')
      IER = SMG$SET_KEYPAD_MODE(KBID, 1)
C     -- skip initialization if "APPEND" requested
      IF (APPEND) RETURN
C     -- initialize device
      DO IW=1,256
          GRNLBF(IW) = 'F000'X        ! NOP (cancel byte mode)
      END DO
      CALL GRGR02(GRNLBF,256,%val(BUFFER),BUFLEV,UNIT)
C
      IF (NEW) THEN
          GRNLBF(1) = 'A001'X            ! SPD   (image video driver)
          GRNLBF(2) = 'C000'X            ! LPR0  (split/togg/crop/intr)
          GRNLBF(3) = 'C20F'X            ! LPR1  (red overlays, cursors)
          GRNLBF(4) = 'C40F'X            ! LPR2  (grn overlays, cursors)
          GRNLBF(5) = 'C60F'X            ! LPR3  (blu overlays, cursors)
          GRNLBF(6) = 'C824'X            ! LPR4  (video routing)
          GRNLBF(7) = 'CA00'X            ! LPR5  (clamp, invert)
C
          GRNLBF(8) = 'A002'X            ! SPD  (image function memory)
          GRNLBF(9) = 'C000'X            ! LPR0 (split/toggle/retrace)
          GRNLBF(10) = 'C200'X           ! LPR1 (input bits 8,9)
          GRNLBF(11) = 'C8E4'X           ! LPR4 (lookup table routing)
          GRNLBF(12) = 'CA00'X           ! LPR5 (bypass)
          NW = 12
      ELSE
          GRNLBF(1) = 'A001'X            ! SPD 1 (image function video)
          GRNLBF(2) = 'C800'X            ! LPR4  (select ABC channels)
          GRNLBF(3) = 'CA00'X            ! LPR5  (no bypass or invert)
          NW = 3
      END IF
      CALL GRGR02(GRNLBF,NW,%val(BUFFER),BUFLEV,UNIT)
C
      GRNLBF(1) = 'B000'X            ! LPA (table A) (red)
      DO IW=0,15
            GRNLBF(IW+2)  = 'D000'X.OR.GCTABL(1,IW)
      END DO
      GRNLBF(18) = 'B400'X           ! LPA (table B) (green)
      DO IW=0,15
            GRNLBF(IW+19)  = 'D000'X.OR.GCTABL(2,IW)
      END DO
      GRNLBF(35) = 'B800'X           ! LPA (table C) (blue)
      DO IW=0,15
            GRNLBF(IW+36)  = 'D000'X.OR.GCTABL(3,IW)
      END DO
      NW = 51
      CALL GRGR02(GRNLBF,NW,%val(BUFFER),BUFLEV,UNIT)
C
      IF (NEW) THEN
          GRNLBF(1) = 'A100'X            ! SPD (load PCR)
          GRNLBF(2) = 'C005'X            ! LPR (PCR bits 0,2)
          GRNLBF(3) = 'A008'X            ! SPD (zoom/pan)
          GRNLBF(4) = 'C000'X            ! LPR
          GRNLBF(5) = 'B000'X            ! LPA
          GRNLBF(6) = 'D0FF'X            ! LPD
          GRNLBF(7) = 'D0FF'X            ! LPD
C
          GRNLBF(8) = 'A080'X            ! SPD (quad cursor)
          GRNLBF(9) = 'C000'X            ! LPR (all cursors off)
C
          GRNLBF(10) = 'A000'X           ! SPD (none)
          GRNLBF(11) = '800D'X           ! LDC (channels 0,2,3)
          GRNLBF(12) = '2830'X           ! LWM (R0,A0,Z1,V1,H0,W0)
          GRNLBF(13) = '1001'X           ! LSM (color=1)
          NW =13
      ELSE
          GRNLBF(1) = 'A008'X            ! SPD 8 (zoom/pan)
          GRNLBF(2) = 'B000'X
          GRNLBF(3) = 'B000'X
          GRNLBF(4) = 'D000'X
          GRNLBF(5) = 'C004'X
C
          GRNLBF(6) = 'A080'X            ! SPD (quad cursor)
          GRNLBF(7) = 'C000'X            ! LPR (all cursors off)
C
          GRNLBF(8) = 'A000'X            ! SPD (none)
          GRNLBF(9) = '8FFF'X            ! LDC FFF
          GRNLBF(10) = '2830'X           ! LWM (R0,A0,Z1,V1,H0,W0)
          GRNLBF(11) = '1001'X           ! LSM (color=1)
          NW = 11
      END IF
      GOTO 1000
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      IER = SYS$DASSGN(%val(UNIT))
      IF (IER.NE.1) THEN
          CALL GRWARN('Error closing graphics device.')
          CALL GRGMSG(IER)
      END IF
      IER = GRFMEM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRWARN('Error deallocating plot buffer.')
          CALL GRGMSG(IER)
      END IF
C     -- delete virtual keyboard
      IER = SMG$CREATE_VIRTUAL_KEYBOARD(KBID)
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      GRNLBF(1) = 'A000'X            ! SPD 0
      GRNLBF(2) = '1FFF'X            ! LSM FFF
      GRNLBF(3) = '8FFF'X            ! LDC FFF
      GRNLBF(4) = '2830'X            ! LWM (R0,A0,Z1,V1,H0,W0)
      IF (APPEND) THEN
          GRNLBF(5) = '800D'X        ! dummy
      ELSE
          GRNLBF(5) = '3000'X        ! ERS
      END IF
      GRNLBF(6) = '800D'X            ! LDC (channels 0,2,3)
      GRNLBF(7) = '1000'X .OR. IC    ! LSM <color>
      NW = 7
      GOTO 1000
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      CALL GRGR01(I0, J0, I1, J1, GRNLBF)
      NW = 4
      GOTO 1000
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      CALL GRGR01(I0, J0, I0, J0, GRNLBF)
      NW = 4
      GOTO 1000
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
      GRNLBF(1) = '1000'X .OR. IC
      NW = 1
      GOTO 1000
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRGR03(%val(BUFFER), UNIT, BUFLEV)
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
C     -- flush buffer
      CALL GRGR03(%val(BUFFER), UNIT, BUFLEV)
C     -- 
      IX = NINT(RBUF(1))
      IY = NINT(RBUF(2))
      CALL GRGR04(IX, IY, ICH, IER, UNIT, KBID)
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
      IC = RBUF(1)
      IR = MIN(255.,MAX(RBUF(2)*255.,0.))
      IG = MIN(255.,MAX(RBUF(3)*255.,0.))
      IB = MIN(255.,MAX(RBUF(4)*255.,0.))
      IF (MONO) THEN
          MONCOL = NINT(0.30*IR + 0.59*IG + 0.11*IB)
          IR = MONCOL
          IG = MONCOL
          IB = MONCOL
      END IF
      IF (NEW) THEN
          GRNLBF(1) = 'A002'X          ! SPD 2 (image function video)
      ELSE
          GRNLBF(1) = 'A001'X          ! SPD 1 (image function video)
      END IF
      GRNLBF(2) = 'B000'X .OR. IC      ! LPA (table A) (red)
      GRNLBF(3) = 'D000'X .OR. IR
      GRNLBF(4) = 'B400'X .OR. IC      ! LPA (table B) (green)
      GRNLBF(5) = 'D000'X .OR. IG
      GRNLBF(6) = 'B800'X .OR. IC      ! LPA (table C) (blue)
      GRNLBF(7) = 'D000'X. OR. IB
      NW = 7
      GOTO 1000
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
 1000 CALL GRGR02(GRNLBF,NW,%val(BUFFER),BUFLEV,UNIT)
C-----------------------------------------------------------------------
      END

C*GRGR00 -- PGPLOT Grinnell driver, open device
C+
      INTEGER FUNCTION GRGR00 (GRFILE, GRFNLN, GRUNIT)
      CHARACTER*(*) GRFILE
      INTEGER GRFNLN, GRUNIT
C
C Returns:
C   GRGR00         : 1 if the device was opened successfully. Any 
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
     2            ' is the wrong sort of device for plot type GRINNELL')
          GRGR00 = 32767      ! indicate error
          IER = SYS$DASSGN(%VAL(GRUNIT))
          RETURN
      END IF
C
C Successful completion.
C
      GRGR00 = 1
      RETURN
C
C Error exit.
C
  100 CALL GRWARN('Cannot open graphics device '//GRFILE(1:L))
      CALL GRGMSG(IER)
      GRGR00 = IER
C-----------------------------------------------------------------------
      END

C*GRGR01 -- PGPLOT Grinnell driver, line segment
C+
      SUBROUTINE GRGR01 (I0, J0, I1, J1, GRNLBF)
      INTEGER*2 I0, J0, I1, J1
      INTEGER*2 GRNLBF(4)
C
C Arguments:
C   I0, J0 (input) : device coordinates of the starting point.
C   I1, J1 (input) : device coordinates of the end point.
C   GRNLBF (output) : buffer for instruction.
C 
C Subroutines called:
C   GRGR02
C-----------------------------------------------------------------------
      INTEGER*2 IC0, JC0, IC1, JC1, DX, DY, T
      INTEGER*2 X, CLIP511
C
      CLIP511(X) = X .AND. '1FF'X
C
      IC0 = CLIP511(I0)
      JC0 = CLIP511(J0)
      IC1 = CLIP511(I1)
      JC1 = CLIP511(J1)
      T   = IC1-IC0
      DX  = CLIP511(T)
      T   = JC1-JC0
      DY  = CLIP511(T)
C
C Assume Grinnell has already been initialized for vector drawing
C (LWM) and SPD 0 has been selected, and appropriate channels have
C been selected.
C
      GRNLBF(1) = '4800'X .OR. IC0      ! LEA
      GRNLBF(2) = '5000'X .OR. DX       ! LEB
      GRNLBF(3) = '6800'X .OR. JC0      ! LLA
      GRNLBF(4) = '7400'X .OR. DY       ! LLB (W=1)
C-----------------------------------------------------------------------
      END

C*GRGR02 -- PGPLOT Grinnell driver, transfer data to buffer
C+
      SUBROUTINE GRGR02 (INSTR, N, BUFFER, HWM, UNIT)
      INTEGER   N, HWM, UNIT
      INTEGER*2 INSTR(*), BUFFER(*)
C
C Arguments:
C  INSTR  (input)  : text of instruction (16-bit words).
C  N      (input)  : number of 16-bit words to transfer.
C  BUFFER (input)  : output buffer.
C  HWM    (in/out) : number of bytes used in BUFFER.
C  UNIT   (input)  : channel number for output (when buffer is full).
C
C Subroutines called:
C   GRGR03
C-----------------------------------------------------------------------
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=8192)
      INTEGER  I
C-----------------------------------------------------------------------
      DO 10 I=1,N
          IF (HWM.GE.BUFSIZ) CALL GRGR03(BUFFER, UNIT, HWM)
          HWM = HWM + 2
          BUFFER(HWM/2) = INSTR(I)
   10 CONTINUE
C-----------------------------------------------------------------------
      END

C*GRGR03 -- PGPLOT Grinnell driver, copy buffer to device
C+
      SUBROUTINE GRGR03 (BUFFER, UNIT, N)
      BYTE BUFFER(*)
      INTEGER UNIT, N
C
C Arguments:
C   BUFFER (input) address of buffer to be output
C   UNIT   (input) channel number for output
C   N      (input) number of bytes to transfer
C          (output) set to zero
C-----------------------------------------------------------------------
      INCLUDE '($IODEF)'
C
      INTEGER SYS$QIOW
C
      INTEGER RESULT
      INTEGER IOSB(2)
      INTEGER*2 STBC(2), STATUS, COUNT
      EQUIVALENCE (STBC, IOSB(1)), (STATUS, STBC(1)), (COUNT, STBC(2))
C-----------------------------------------------------------------------
      IF (N.LT.1) RETURN
      RESULT = SYS$QIOW(,%VAL(UNIT),
     1            %VAL(IO$_WRITEVBLK.OR.IO$M_SETFNCT),IOSB,,
     2            ,BUFFER,%VAL(N),,%VAL(0),,)
      IF (RESULT.NE.1) THEN
          CALL GRGMSG(RESULT)
          CALL GRQUIT('SYS$QIOW failure writing to Grinnell')
      END IF
      IF (STATUS.NE.1) THEN
          RESULT = STATUS
          CALL GRGMSG(RESULT)
          CALL GRQUIT('SYS$QIOW failure writing to Grinnell')
      END IF
      N = 0
C-----------------------------------------------------------------------
      END

C*GRGR04 -- PGPLOT Grinnell driver, cursor routine
C+
      SUBROUTINE GRGR04 (IX, IY, IC, IER, UNIT, KBID)
      INTEGER IX, IY, IC, IER, UNIT, KBID
C
C Arguments:
C   IX, IY (in/out) : initial/final coordinates of cursor (device 
C                     coordinates).
C   IC     (output) : character code.
C   IER    (output) : error status (1 => OK).
C   UNIT   (input)  : channel for output to device.
C   KBID   (input)  : SMG keyboard identifier for control.
C
C This version uses the cursor on the zoom-pan card (peripheral
C device bit 3). The cursor is moved by using the arrow keys on the
C terminal; the cursor "speed" (step size) is controlled by the
C PF1 (smallest step) to PF4 (largest step) keys. The numeric keys
C on the keypad can be used in place of the arrow keys, with the
C addition of diagonal motion:
C        ^
C      7 8 9
C    < 4   6 >
C      1 2 3
C        v
C
C The user indicates that the cursor has been positioned by
C typing any character on his keyboard (SYS$COMMAND), with the
C following exceptions: control characters (^C, ^O, ^Q, ^R, ^S,
C ^T, ^U, ^X, ^Y, DEL) are intercepted by the operating system
C and cannot be used; NUL, ESC (^[) and escape sequences (e.g., 
C arrow keys) are ignored by GRCURS.
C-----------------------------------------------------------------------
      INTEGER*2      GRNLBF(5),IXG,IYG
      INTEGER        SMG$READ_KEYSTROKE
      INTEGER        STEP, NBYTES
      DATA           STEP/4/      ! initial step size
C-----------------------------------------------------------------------
   10 IXG = IX
      IYG = IY
      GRNLBF(1) = 'A008'X            ! SPD ZOOM/PAN
      GRNLBF(2) = 'C020'X            ! enable cursor
      GRNLBF(3) = 'B000'X            ! LPA 0
      GRNLBF(4) = 'D000'X .OR. IXG      ! load X coord
      GRNLBF(5) = 'D000'X .OR. IYG      ! load Y coord
      NBYTES = 10
      CALL GRGR03(GRNLBF,UNIT,NBYTES)
      IER = SMG$READ_KEYSTROKE(KBID, IC)
      IF (IER.NE.1) RETURN
      IF (IC.EQ.274 .OR. IC.EQ.268) THEN
C         key UP or KP8
          IY = MIN(511,IY+STEP)
      ELSE IF (IC.EQ.275 .OR. IC.EQ.262) THEN
C         key DOWN or KP2
          IY = MAX(0,IY-STEP)
      ELSE IF (IC.EQ.276 .OR. IC.EQ.264) THEN
C         key LEFT or KP4
          IX = MAX(0,IX-STEP)
      ELSE IF (IC.EQ.277 .OR. IC.EQ.266) THEN
C         key RIGHT or KP6
          IX = MIN(511,IX+STEP)
      ELSE IF (IC.EQ.267) THEN
C         key KP7
          IX = MAX(0,IX-STEP)
          IY = MIN(511,IY+STEP)
      ELSE IF (IC.EQ.269) THEN
C         key KP9
          IX = MIN(511,IX+STEP)
          IY = MIN(511,IY+STEP)
      ELSE IF (IC.EQ.263) THEN
C         key KP3
          IX = MIN(511,IX+STEP)
          IY = MAX(0,IY-STEP)
      ELSE IF (IC.EQ.261) THEN
C         key KP1
          IX = MAX(0,IX-STEP)
          IY = MAX(0,IY-STEP)
      ELSE IF (IC.EQ.265) THEN
C         key KP5 ("home")
          IX = 255
          IY = 255
      ELSE IF (IC.EQ.256) THEN
C         key PF1 or KP4
          STEP = 1
      ELSE IF (IC.EQ.257) THEN
C         key PF2
          STEP = 4
      ELSE IF (IC.EQ.258) THEN
C         key PF3
          STEP = 16
      ELSE IF (IC.EQ.259) THEN
C         key PF4
          STEP = 64
      END IF
      IF (IC.LE.0 .OR. IC.GT.255) GOTO 10
      GRNLBF(1) = 'A008'X
      GRNLBF(2) = 'C000'X            ! disable all cursors
      GRNLBF(3) = 'A000'X
      NBYTES = 6
      CALL GRGR03(GRNLBF,UNIT,NBYTES)
C-----------------------------------------------------------------------
      END
