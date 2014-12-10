C Date:     3-FEB-1988 15:08:50 GMT
C From:     AFT@AST-STAR.CAM.AC.UK
C To:       TJP@CITPHOBO
C Subject:  ARDRIVER.FOR

        SUBROUTINE ARDRIV(IFUNC, RBUF, NBUF, CHR, LCHR)
        INTEGER   IFUNC, NBUF, LCHR
        REAL      RBUF(*)
        CHARACTER CHR*(*)
C
C PGPLOT driver for Args image device.
C---
C Supported device:  Sigma Args, 7000 series.
C
C Device type code:  /ARgs.
C
C Default device name:  ARGS_DEVICE (a logical name).
C
C Default view surface dimensions:  Depends on monitor.
C
C Resolution:  The full view surface is 512 by 512 pixels.
C
C Color capability: Color indices 0-255 are supported.  The default
C   representation is listed in Chapter 5 of the PGPLOT manual.  The
C   representation of all color indices can be changed.
C
C Input capability:  A cursor routine is provided that works on
C   Starlink Args devices.  It is possible to move the cursor with
C   either the tracker ball or by using the arrow keys on the terminal
C   (SYS$COMMAND); the PF1 to PF4 keys can be used to control the rate
C   at which the arrow keys move the cursor.  Terminate the cursor (in
C   all cases) by typing any printable character on the keyboard.
C
C File format:  It is not possible to send ARGS plots to a disk file.
C
C Obtaining hardcopy:  Not possible.
C---
C 17-Jan-1988 - No longer requires GESUPPORT routines [AFT].
C 21-Sep-1986 - [AFT].
C 15-Oct-1986 - Modified to work on a Q-bus [AFT].
C 16-Jan-1988 - [AFT]
C-----------------------------------------------------------------------
        INCLUDE '($IODEF)'
        INCLUDE '($PRDEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYIDEF)'
C
        CHARACTER MSG*10
        INTEGER   GRAR00, SYS$DASSGN, GRGMEM, IER
        INTEGER   IOCODE, I, I0, J0, I1, J1, ICPU
        INTEGER*2 ITMP(10), ILEV, IOSB(4)
        INTEGER   IREM, IWRT, IRD, ICHAN, MXCNT, ICNT, IBADR, ICOL
        SAVE      IREM, IWRT, IRD, ICHAN, MXCNT, ICNT, IBADR, ICOL
        INTEGER   MICRO, ITYPE
        SAVE      MICRO, ITYPE
        LOGICAL   APPEND
        SAVE      APPEND
C---
        GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     &       110,120,130,140,150,160,170,180,900,900,
     &       210,900,230) IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in AR device driver: '//MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC= 1, Return device name. -------------------------------------
10      CHR='ARGS'
        LCHR=LEN(CHR)
        RETURN
C
C--- IFUNC= 2, Return Physical min and max for plot device. ------------
20      RBUF(1)=0
        RBUF(2)=511
        RBUF(3)=0
        RBUF(4)=511
        RBUF(5)=0
        RBUF(6)=255
        NBUF=6
        RETURN
C
C--- IFUNC= 3, Return device resolution. -------------------------------
30      RBUF(1)=50.0
        RBUF(2)=50.0
        RBUF(3)=1
        NBUF=3
        RETURN
C
C--- IFUNC= 4, Return misc device info. --------------------------------
C I= Interactive device
C C= Cursor
C N= No hard dash
C A= No area fill (not implemented)
C N= No hard thick lines
40      CHR='ICNNNNNNNN'
        LCHR=10
        RETURN
C
C--- IFUNC= 5, Return default file name. -------------------------------
50      CHR='ARGS_DEVICE'
        LCHR=LEN(CHR)
        NBUF=1
        RETURN
C
C--- IFUNC= 6, Return default physical size of plot. -------------------
60      RBUF(1)=0
        RBUF(2)=511
        RBUF(3)=0
        RBUF(4)=511
        RETURN
C
C--- IFUNC= 7, Return misc defaults. -----------------------------------
70      RBUF(1)=1
        NBUF=1
        RETURN
C
C--- IFUNC= 8, Select plot. --------------------------------------------
80      CALL INAR03(ICHAN,IWRT)
        RETURN
C
C--- IFUNC= 9, Open workstation. ---------------------------------------
90      APPEND=RBUF(3).NE.0.0
        RBUF(2)=GRAR00(ICHAN,CHR,LCHR)
        RBUF(1)=ICHAN
C---
C- Allocate a buffer.  Note, there could be a hardware interface
C- bug for the ID driver which will cause the QIO to fail if the
C- buffer starts on the last two words of a page.  Since the address
C- returned by GRGMEM begins on a quadword boundary, this
C- condition can never arise.  However, some care is required
C- to make sure that the QIO to the Args only uses the buffer
C- allocated with GRGMEM.
C---
        MXCNT=8192
        IER=GRGMEM(MXCNT,IBADR)
        IF(IER .NE. SS$_NORMAL) THEN
            CALL GRGMSG(IER)
            CALL GRWARN('Unable to allocate virtual memory.')
            RBUF(2)=0
            CALL SYS$DASSGN(%val(ICHAN))
            RETURN
        END IF
C---
C- If device opened remotely, set remote flag.  Note, current
C- driver does not support remote access.
C---
        IF(NINT(RBUF(2)).EQ.1) THEN
            IREM=0
        ELSE IF(NINT(RBUF(2)).EQ.3) THEN
            IREM=1
            RBUF(2)=1
        ELSE
C- Error condition.
            RETURN
        END IF
C---
C- The QIO function is different for Unibus and Q-bus machines so
C- detect CPU type.
C---
        CALL LIB$GETSYI(SYI$_CPU,ICPU)
        IF(ICPU.EQ.PR$_SID_TYPUV1 .OR. ICPU.EQ.PR$_SID_TYPUV2) THEN
            MICRO=1
            ITYPE=1
            IWRT=IO$_WRITEVBLK
            IRD =IO$_READVBLK
        ELSE
            MICRO=0
            ITYPE=2
            IWRT=IO$_WRITEVBLK
            IRD =IO$_READVBLK
        END IF
C---
C- Init the buffer routine.
C---
        CALL INAR03(ICHAN,IWRT)
        CALL INAR01
C---
C- Cancel any outstanding I/O requests
C---
        CALL SYS$CANCEL(%val(ICHAN))
        CALL SYS$WAITFR()
C---
C- Reset interface.
C---
        IF(MICRO.EQ.0) THEN
            I=1
            IOCODE=IOR(IWRT,I*'0040'x)
            CALL SYS$QIOW(,%val(ICHAN),%val(IOCODE),IOSB,,,
     &              I,%val(I),,,,)
        ELSE
            I=2
            IOCODE=IOR(IWRT,'2800'x)
            CALL SYS$QIOW(,%val(ICHAN),%val(IOCODE),IOSB,,,
     &              %val(I),,,,,)
        END IF
C---
C- Load default lookup table (if not appending).
C---
        IF(.NOT.APPEND) CALL GRAR10(%val(IBADR),ICNT,MXCNT)
        RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
100     CALL SYS$DASSGN(%val(ICHAN))
        CALL GRFMEM(MXCNT,IBADR)
        RETURN
C
C--- IFUNC=11, Begin Picture. ------------------------------------------
110     CALL INAR01
        IF(.NOT.APPEND) THEN
          ITMP(1)='3501'x
          ITMP(2)='FFFF'x
          ITMP(3)='0003'x
          ITMP(4)='0003'x
          ITMP(5)='0003'x
          ITMP(6)='3501'x
          ITMP(7)='FFFF'x
          CALL GRAR02(ITMP,14,%val(IBADR),ICNT,MXCNT)
          CALL GRAR03(%val(IBADR),ICNT)
        END IF
        APPEND=.FALSE.
        RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
120     I0=NINT(RBUF(1))
        J0=NINT(RBUF(2))
        I1=NINT(RBUF(3))
        J1=NINT(RBUF(4))
        CALL GRAR01(I0,J0,I1,J1,%val(IBADR),ICNT,MXCNT)
        RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
130     I0=NINT(RBUF(1))
        J0=NINT(RBUF(2))
        CALL GRAR01(I0,J0,I0,J0,%val(IBADR),ICNT,MXCNT)
        RETURN
C
C--- IFUNC=14, End Picture. --------------------------------------------
140     RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
150     ICOL=MAX(0,MIN(NINT(RBUF(1)),255))
        ITMP(1)='2901'x
        ITMP(2)=ICOL
        CALL GRAR02(ITMP,4,%val(IBADR),ICNT,MXCNT)
        RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
160     CALL GRAR03(%val(IBADR),ICNT)
        RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
170     I0=RBUF(1)
        J0=RBUF(2)
        CALL GRAR11(ICHAN,I0,J0,CHR,%val(IBADR),ICNT,MXCNT)
        RBUF(1)=I0
        RBUF(2)=J0
        NBUF=2
        LCHR=1
        RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
180     RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
210     ITMP(1)=IOR('1800'x,0)
        ITMP(2)=NINT(RBUF(1))
        ILEV=IAND(255,INT(RBUF(2)*255.999))
        ITMP(3)=ILEV
        ILEV=IAND(255,INT(RBUF(3)*255.999))
        ITMP(3)=ISHFT(ILEV,8)+ITMP(3)
        ILEV=IAND(255,INT(RBUF(4)*255.999))
        ITMP(4)='FF00'x+ILEV
        CALL GRAR02(ITMP, 8, %val(IBADR), ICNT, MXCNT)
        RETURN
C
C--- IFUNC=23, Escape. -------------------------------------------------
C- Send CHR array directly to Args (user better know what he is doing!)
230     CALL GRAR02(%ref(CHR),LCHR,%val(IBADR),ICNT,MXCNT)
        RETURN
C-----------------------------------------------------------------------
        END
     
        INTEGER FUNCTION GRAR00(LUN,CHR,LCHR)
C-----------------------------------------------------------------------
C GRPCKG (Args internal routine): Opens a QIO channel to the Args.
C
C GRAR00 (returns integer): 0 if the channel could not be opened
C   1 if the channel was opened successfully on a local device,
C   3 for a successful open of a channel over a network,
C   (the remote status of the
C   device must be flagged since, the QIO functions codes are
C   different when writting to a physical device or to the
C   network).
C
C 16-Jan-1988 - Modified from GESUPPORT routine [AFT].
C-----------------------------------------------------------------------
        INCLUDE  '($IODEF)'
        INCLUDE  '($SSDEF)'
        INTEGER   DVI$_DEVCLASS, DVI$_DEVNAM
        PARAMETER (DVI$_DEVCLASS=4)
        PARAMETER (DVI$_DEVNAM=32)
        INTEGER   LUN,LCHR
        CHARACTER CHR*(*)
        INTEGER   IER, ITEMP
        INTEGER   DEVCLASS, ITMLIST(7), MOSB(2), ISTAT, LENGTH
        INTEGER   SYS$ASSIGN, SYS$QIOW
        INTEGER   SYS$GETDVI, SYS$DASSGN, SYS$WAITFR
        INTEGER*2 IOSB(4)
        LOGICAL   WRONG
C---
C- Assign an i/o channel
C---
        IER=SYS$ASSIGN(CHR(:LCHR), LUN,,)
        IF(IER.NE.SS$_NORMAL .AND. IER.NE.SS$_REMOTE) GOTO 900
        IF(IER.EQ.SS$_REMOTE) THEN
C---
C- Cannot check device characteristics easily if network device being used
C- so just check whether we opened the device successfully and return
C- Read back the status from assign to plotting device over network
C---
            IER=SYS$QIOW(,%VAL(LUN),%VAL(IO$_READVBLK),
     :                   IOSB,,,ISTAT,LENGTH,,,,)
            IF(IOSB(1) .NE. SS$_NORMAL) THEN
                CALL GRWARN ('Unable to read status from ASSIGN to' //
     :                      ' graphics device on remote node')
                WRITE(6,*) IOSB(2), ' bytes read'
                ITEMP=IOSB(1)
                CALL GRGMSG(ITEMP)
                GRAR00=0
                RETURN
            END IF
            IF(ISTAT .NE. SS$_NORMAL) THEN
                IER=ISTAT
                GOTO 900
            ELSE
                GRAR00=3
                RETURN
            END IF
        END IF
C---
C- Check that device has correct characteristics,
C- and obtain true device name.
C---
        ITMLIST(1)=DVI$_DEVCLASS*2**16+4
        ITMLIST(2)=%LOC(DEVCLASS)
        ITMLIST(3)=0
        ITMLIST(4)=DVI$_DEVNAM*2**16+LEN(CHR)
        ITMLIST(5)=%LOC(CHR)
        ITMLIST(6)=%LOC(LCHR)
        ITMLIST(7)=0
        IER=SYS$GETDVI(%VAL(0),,CHR(:LCHR),
     1                 ITMLIST,MOSB,,,)
        IF(.NOT.IER) GOTO 900
        IER=SYS$WAITFR(%VAL(0))
        IF(.NOT.IER) GOTO 900
        IF(.NOT.MOSB(1)) THEN
            IER=MOSB(1)
            GOTO 900
        END IF
        WRONG=DEVCLASS.NE.96
        IF(WRONG) THEN
            CALL GRWARN( CHR(:LCHR)//
     2            ' is the wrong sort of device for plot type.')
            IER=SYS$DASSGN(%VAL(LUN))
            GOTO 990
        END IF
C---
C- Successful completion
C---
        GRAR00=1
        RETURN
C---
C- Error exit
C---
900     CALL GRWARN('Cannot open graphics device '//CHR(:LCHR))
        CALL GRGMSG(IER)
990     GRAR00=0
        END
C*********
        SUBROUTINE GRAR01(I0,J0,I1,J1,IBUF,ICNT,MXCNT)
C-----------------------------------------------------------------------
C GRPCKG(internal routine, ARGS): draw a line segment.
C
C Arguments:
C
C I0,J0(integer*2, input): the column and row numbers of the starting
C       point.
C I1,J1(integer*2, input): the column and row numbers of the end point.
C
C Note: the arguments are specified as integer*2, but(on the VAX at
C least) integer*4 values may be used. The values should be in the
C range defined by the hardware:(0...511).
C
C 11-Apr-1983
C  7-Nov-1984 - KS/AAO) Some optimisation based on last position added.
C-----------------------------------------------------------------------
        INTEGER   I0, J0, I1, J1, ICNT, MXCNT
        INTEGER*2 IBUF
        INTEGER*2 CLIP511
        INTEGER   IPTR,X
        INTEGER*2 IC0, JC0, IC1, JC1, ITMP(4)
        INTEGER*2 LASTI, LASTJ
        SAVE      LASTI, LASTJ
C
        CLIP511(X)=X .AND. '1FF'x
C
        IC0=CLIP511(I0)
        JC0=CLIP511(J0)
        IC1=CLIP511(I1)
        JC1=CLIP511(J1)
C
        IF(IC0.NE.LASTI .OR. JC0.NE.LASTJ) THEN
            ITMP(1)=IOR(IC0,'C000'x)
            ITMP(2)=IOR(JC0,'A000'x)
            IPTR=3
        ELSE
            IPTR=1
        END IF
        ITMP(IPTR)  =IOR(IC1,'C000'x)
        ITMP(IPTR+1)=IOR(JC1,'E000'x)
        LASTI=IC1
        LASTJ=JC1
        CALL GRAR02(ITMP,2*(IPTR+1),IBUF,ICNT,MXCNT)
        RETURN
C
        ENTRY INAR01
        LASTI=-1
        LASTJ=-1
        RETURN
        END
C*********
        SUBROUTINE GRAR02(STR, N, QBUF, ICNT, MXCNT)
C-----------------------------------------------------------------------
C GRPCKG (Args internal routine): Transfer N bytes to
C the output buffer, flushing the buffer as necessary with the
C GRAR03 routine.  If the N bytes will not fit into the current
C buffer, then the buffer is first dumped.  This is to to cause
C STR to be transferred as a complete unit.
C Based on early versions of GRxx02 routines, this version does not
C use any common blocks.
C ***NOTE*** INAR03 must be called before any calls to GRAR02 to
C set the LUN/Channel to which the buffer should be dumped.
C
C Arguments:
C
C STR(N)        I Byte  Characters to be written.
C N             I I     The number of bytes to transfer.
C QBUF          I/O Byte The output buffer.
C ICNT          I/O I   Current number of bytes used in QBUF.
C MXCNT         I/O I   Maximum number of bytes that can be stored
C                       -in QBUF.
C
C 16-Jan-1988 - Based on GESUPPORT routine [AFT].
C-----------------------------------------------------------------------
        INTEGER  ICNT,MXCNT,I,N
        BYTE QBUF(N), STR(N)
C---
        IF(ICNT+N.GE.MXCNT) CALL GRAR03(QBUF,ICNT)
        DO 100 I=1,N
            ICNT=ICNT+1
            QBUF(ICNT)=STR(I)
100     CONTINUE
        RETURN
        END
C*********
        SUBROUTINE GRAR03(QBUF,ICNT)
C-----------------------------------------------------------------------
C GRPCKG (Args internal routine): Write ICNT bytes in QBUF to the device
C Reset ICNT to zero.
C ***NOTE*** INAR03 must be called before any calls to GRAR02 to
C set the correct Channel number.
C This subroutine contains the entry point INAR03 that defines
C the variables ICHAN and IFUNC.
C
C Arguments:
C
C QBUF          I/O Byte The output buffer.
C ICNT          I/O I   Current number of bytes used in QBUF.
C
C 16-Jan-1988 - Modified from GESUPPORT routines [AFT].
C-----------------------------------------------------------------------
        INCLUDE '($SSDEF)'
        INTEGER   SYS$QIOW
        INTEGER   ICNT
        BYTE      QBUF(*)
        INTEGER   RESULT, N
        INTEGER*2 IOSB(4)
        INTEGER   INCHAN,INFUNC
        INTEGER   ICHAN,IFUNC
        SAVE      ICHAN,IFUNC
C
        N=ICNT
        ICNT=0
        IF(N.LT.1) RETURN
C---
        RESULT=SYS$QIOW(,%val(ICHAN),%val(IFUNC),IOSB,,,
     1          QBUF(N+1),%val(4),
     2          QBUF,%val(N/2),%val(8),)
C---
        IF(RESULT.NE.SS$_NORMAL) THEN
            CALL GRGMSG(RESULT)
            CALL GRWARN('SYS$QIOW error writing to device.')
        END IF
        IF(IOSB(1).NE.SS$_NORMAL) THEN
            CALL GRGMSG(IOSB(1))
            CALL GRWARN('SYS$QIOW (IOSB) status writing to device.')
        END IF
        RETURN
C---
        ENTRY INAR03(INCHAN,INFUNC)
C- Save info needed to dump buffer.
        ICHAN=INCHAN
        IFUNC=INFUNC
        RETURN
        END
     
        SUBROUTINE GRAR10(IBUF,ICNT,MXCNT)
C-----------------------------------------------------------------------
C GRPCKG(internal routine, ARGS): advance the plotting area to a
C new page. This routine is called for ARGS devices by GRPAGE to
C reinitialize the display.
C
C 11-Apr-1983
C  7-Nov-1984 - Colour table code modified [KS/AAO].
C 21-Sep-1986 - Modified for use with GEDRIVER [AFT].
C-----------------------------------------------------------------------
        INTEGER   MXCOL
        PARAMETER (MXCOL=16)
C
        INTEGER   ICNT, MXCNT
        INTEGER*2 IBUF
C
        INTEGER   IPTR, ICLR
        INTEGER*2 ITMP(257)
C
        INTEGER*2 IRED(MXCOL), IBLUE(MXCOL), IGREEN(MXCOL)
        DATA IRED  /  0,255,255,  0,  0,  0,255,255,
     &              255,127,  0,  0,127,255, 85,170/
        DATA IGREEN/  0,255,  0,255,  0,255,  0,255,
     &              127,255,255,127,  0,  0, 85,170/
        DATA IBLUE /  0,255,  0,  0,255,255,255,  0,
     &                0,  0,127,255,255,127, 85,170/
C
C   Reset GSW to drawing mode,size and direction all zero, autoincrement on,
C   background overlay off, backwash off, cursor scroll disabled
C
        ITMP(1)='0E00'x         ! Load status word
        ITMP(2)='2000'x
C
C   Reset Z status register to 32 bits and 0 offset.
C
        ITMP(3)='0700'x
        ITMP(4)='1F00'x
        CALL GRAR02(ITMP,8,IBUF,ICNT,MXCNT)
C
C   Reset VSR to display graphics and overlay
C
        ITMP(1)='2401'x
        ITMP(2)='01C0'x
        ITMP(3)=0
        ITMP(4)='0007'x
        CALL GRAR02(ITMP,8,IBUF,ICNT,MXCNT)
C
C   Set origin to 0,0
C
        ITMP(1)='1000'x         ! X Origin to 0
        ITMP(2)=0
        ITMP(3)='1100'x         ! Y Origin to 0
        ITMP(4)=0
C
C   Put pen at origin
C
        ITMP(5)='C000'x
        ITMP(6)='A000'x
C
C   Unzoom
C
        ITMP(7)='5000'x
        ITMP(8)=0
        CALL GRAR02(ITMP,16,IBUF,ICNT,MXCNT)
C
C   No scroll
C
        ITMP(1)='3901'x
        ITMP(2)='FFFF'x
        ITMP(3)=0
        ITMP(4)='3B01'x
        ITMP(5)='FFFF'x
        ITMP(6)=0
        CALL GRAR02(ITMP,12,IBUF,ICNT,MXCNT)
C
C   Set write enable for lower 8 bit planes only
C
        ITMP(1)='2D01'x
        ITMP(2)='00FF'x
C
C   Turn on all planes
C
        ITMP(3)='2B01'x
        ITMP(4)='FFFF'x
        CALL GRAR02(ITMP,8,IBUF,ICNT,MXCNT)
C
C   Turn off lamps
C
        ITMP(1)='3C40'x
        ITMP(2)='3C45'x
        ITMP(3)=0
        CALL GRAR02(ITMP,6,IBUF,ICNT,MXCNT)
C
C   Set colour table
C
        IPTR=2
        DO ICLR=1,MXCOL
            IPTR=IPTR+2
            ITMP(IPTR-1)=IOR(ISHFT(IGREEN(ICLR),8),IRED(ICLR))
            ITMP(IPTR)  =IOR('FF00'x,IBLUE(ICLR))
        END DO
        ITMP(1)=IOR('1800'x,MXCOL-1)
        ITMP(2)=0
        CALL GRAR02(ITMP,2*IPTR,IBUF,ICNT,MXCNT)
C
C  Flush ARGS Buffer
C
        CALL GRAR03(IBUF,ICNT)
C
        RETURN
        END
     
      SUBROUTINE GRAR11(ICHAN, IX, IY, CHR, IBUF, ICNT, MXCNT)
      INTEGER   ICHAN, IX, IY, IBUF, ICNT, MXCNT
      CHARACTER CHR
C
C Arguments
C ICHAN  (input)  QIO channel assigned to Args
C IX,IY  (in/out) The cursor position
C CHR    (output) The keyboard character pressed
C IBUF   (input)  Address of a buffer area
C ICNT   (in/out) Number of bytes in use in buffer
C MXCNT  (input)  Maximum size of buffer in bytes
C---
C Read the cursor position on the Args.  The cursor can be moved
C by either rolling the tracker ball.
C The cursor can also be moved by using the cursor keys on the
C terminal associated with SYS$COMMAND in which case the cursor
C "speed" (step size) is controlled by the PF1 (smallest step) to
C PF4 (largest step) keys. The numeric keys on the keypad can be
C used in place of the arrow keys, with the addition of diagonal
C motion:
C         UP
C      7  8  9
C LEFT 4     6 RIGHT
C      1  2  3
C        DOWN
C---
C- 19-Jan-1988 - Modified to track VTDRIVER [AFT].
C---
      INTEGER   SMG$CREATE_VIRTUAL_KEYBOARD, SMG$READ_KEYSTROKE
      INTEGER   IXWAS, IYWAS, ISTEP, IER, IVAL, IXINC, IYINC
      INTEGER   SWITCH, IDSMG
      INTEGER*2 ITMP(10)
      LOGICAL   QKEY
C---
      IER=SMG$CREATE_VIRTUAL_KEYBOARD(IDSMG,'SYS$COMMAND')
      IF(IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRQUIT('Fatal error.')
      END IF
      ITMP(1)='3C40'x
      CALL GRAR02(ITMP,2,IBUF,ICNT,MXCNT)
      CALL GRAR03(IBUF,ICNT)
      CALL GRAR21(IX,IY,.TRUE.,.FALSE.,IBUF,ICNT,MXCNT)
      IXWAS=IX
      IYWAS=IY
      QKEY=.FALSE.
      ISTEP=2
      DO WHILE (.NOT.QKEY)
C- See if user has typed something at keyboard.
          IER=SMG$READ_KEYSTROKE(IDSMG,IVAL,,0)
          IF(IER.NE.1) IVAL=0
          IF(IVAL.EQ.259) THEN
C- PF4=large step
              ISTEP=64
          ELSE IF(IVAL.EQ.258) THEN
              ISTEP=8
          ELSE IF(IVAL.EQ.257) THEN
              ISTEP=4
          ELSE IF(IVAL.EQ.256) THEN
C- PF1=small step
              ISTEP=1
          ELSE IF(IVAL.EQ.49 .OR. IVAL.EQ.261) THEN
C- key 1 or KP1
              IX=IX-ISTEP
              IY=IY-ISTEP
          ELSE IF(IVAL.EQ.50 .OR. IVAL.EQ.262 .OR. IVAL.EQ.275) THEN
C- key 2, KP2 or DOWN
              IY=IY-ISTEP
          ELSE IF(IVAL.EQ.51 .OR. IVAL.EQ.263) THEN
C- key 3 or KP3
              IX=IX+ISTEP
              IY=IY-ISTEP
          ELSE IF(IVAL.EQ.52 .OR. IVAL.EQ.264 .OR. IVAL.EQ.276) THEN
C- key 4, KP4 or LEFT
              IX=IX-ISTEP
          ELSE IF(IVAL.EQ.54 .OR. IVAL.EQ.266 .OR. IVAL.EQ.277) THEN
C- key 6, KP6 or RIGHT
              IX=IX+ISTEP
          ELSE IF(IVAL.EQ.55 .OR. IVAL.EQ.267) THEN
C- key 7 or KP7
              IX=IX-ISTEP
              IY=IY+ISTEP
          ELSE IF(IVAL.EQ.56 .OR. IVAL.EQ.268 .OR. IVAL.EQ.274) THEN
C- key 8, KP8 or UP
              IY=IY+ISTEP
          ELSE IF(IVAL.EQ.57 .OR. IVAL.EQ.269) THEN
C- key 9 or KP9
              IX=IX+ISTEP
              IY=IY+ISTEP
          ELSE IF((IVAL.GT.0 .AND. IVAL.LT.48) .OR.
     &          (IVAL.GT.57 .AND. IVAL.LT.255)) THEN
              QKEY=.TRUE.
          END IF
          CALL GRAR22(ICHAN,IXINC,IYINC,SWITCH,IBUF,ICNT)
          IX=IX+IXINC
          IY=IY+IYINC
          IY=MAX(IY,0)
          IY=MIN(IY,511)
          IX=MAX(IX,0)
          IX=MIN(IX,511)
          CALL GRAR21(IX,IY,.FALSE.,.FALSE.,IBUF,ICNT,MXCNT)
          IF(IX.NE.IXWAS.OR.IY.NE.IYWAS) THEN
              IXWAS=IX
              IYWAS=IY
          ELSE
              CALL LIB$WAIT(0.05)
          END IF
      END DO
      CHR=CHAR(IVAL)
      CALL GRAR21(IX,IY,.FALSE.,.TRUE.,IBUF,ICNT,MXCNT)
C---
C- Free resources.
      CALL SMG$DELETE_VIRTUAL_KEYBOARD(IDSMG)
      RETURN
      END
     
        SUBROUTINE GRAR21(IX,IY,ON,OFF,IBUF,ICNT,MXCNT)
C-----------------------------------------------------------------------
C GRPCKG (Internal routine, ARGS) Control position of ARGS cursor
C
C Arguments:
C
C IX,IY  (Integer, input) Position to which cursor is to be moved.
C ON     (Logical, input) True if cursor is to be switched on.
C OFF    (Logical, input) True if cursor is to be switched off.
C
C If ON and OFF are both false, the cursor is positioned, but
C is switched neither on nor off.  If both are true, it is switched
C on and then off.
C
C (08-Nov-1984 KS / AAO)
C-----------------------------------------------------------------------
        LOGICAL ON,OFF
        INTEGER IX,IY,IBUF,ICNT,MXCNT
C
        INTEGER*2 CURON(4),CUROFF(4),CURPOS(4)
        INTEGER NWORDS
        DATA CURON /'2401'X,'01F8'X,'0008'X,'0000'X/
        DATA CUROFF/'2401'X,'01C0'X,'0000'X,'0000'X/
        DATA CURPOS/'1700'X,'0000'X,'2700'X,'0000'X/
C
        CURPOS(2)=IX
        CURPOS(4)=IY
        CALL GRAR02(CURPOS,8,IBUF,ICNT,MXCNT)
        NWORDS=4
        IF(ON) THEN
            CALL GRAR02(CURON,8,IBUF,ICNT,MXCNT)
            NWORDS=NWORDS+4
        END IF
        IF(OFF) THEN
            CALL GRAR02(CUROFF,8,IBUF,ICNT,MXCNT)
            NWORDS=NWORDS+4
        END IF
        CALL GRAR03(IBUF,ICNT)
C
        END
     
        SUBROUTINE GRAR22(ICHAN,IX,IY,SWITCH,IBUF,ICNT)
C--------------------------------------------------------------------
C GRPCKG  (Internal routine, ARGS) Read trackerball values
C
C Arguments -
C
C IX,IY  (Integer, output) Change in trackerball position in X and Y
C                          since this routine was last called.
C SWITCH (Integer, output) Switch status word.  Bits 3 through 6
C                          indicate switch operations since last
C                          read.
C
C Note: the trackerball must have been reset at some point before
C the first call to this routine.
C
C 13-Dec-1984 - [KS/AAO]
C 21-Mar-1987 - Should now work on micro-VAX [PWH/AFT]
C----------------------------------------------------------------------
        INCLUDE '($PRDEF)'
        INCLUDE '($SYIDEF)'
C
        INTEGER   ICHAN, IX, IY, SWITCH, IBUF, ICNT
C
        INCLUDE '($IODEF)'
C
        INTEGER   ICPU, IOSTAT, IER, SYS$QIOW
        INTEGER*2 IOSB(4), SBUFF(7)
        INTEGER*2 READC(5)
        DATA READC/0,0,0,0,'3C44'X/
C
C- Read trackerball.
C
        CALL GRAR03(IBUF,ICNT)
        CALL LIB$GETSYI(SYI$_CPU,ICPU)
        IF(ICPU.EQ.PR$_SID_TYPUV1 .OR. ICPU.EQ.PR$_SID_TYPUV2) THEN
C---
C- On Micro-VAX (Q-bus).
            IER=SYS$QIOW(,%val(ICHAN),%val(IO$_WRITEVBLK),IOSB,,,
     &                  READC(5), %val(2), %val(5),,,)
            IER=SYS$QIOW(,%val(ICHAN),%val(IO$_READVBLK),IOSB,,,
     &                  SBUFF(5), %val(6), %val(5),,,)
        ELSE
C---
C- On Unibus.
            IER=SYS$QIOW(,%val(ICHAN),%val(IO$_READVBLK),IOSB,,,
     &                  READC, %val(5),SBUFF,%val(7),,)
        END IF
        IOSTAT=IOSB(1)
        IF(.NOT.IER) IOSTAT=IER
        IF(.NOT.IOSTAT) THEN
            CALL GRGMSG(IOSTAT)
            CALL GRQUIT('SYS$QIOW failure reading from ARGS')
        END IF
C
        IX=SBUFF(5)
        IY=SBUFF(6)
        SWITCH=SBUFF(7)
C
        END
