	SUBROUTINE IKDRIV(IFUNC, RBUF, NBUF, CHR, LCHR)
	INTEGER   IFUNC, NBUF, LCHR
	REAL      RBUF(*)
	CHARACTER CHR*(*)
C
C PGPLOT driver for Ikon devices.
C---
C Supported device:  Digisolve Ikon Pixel Engine
C
C Device type code:  /IKon.
C
C Default device name:  IKON_DEFAULT (a logical name).
C
C Default view surface dimensions:  Depends on monitor.
C
C Resolution:  The full view surface is 1024 by 780 pixels.
C
C Color capability: Color indices 0-255 are supported.  The default
C   representation is listed in Chapter 5 of the PGPLOT manual.  The
C   representation of all color indices can be changed.
C
C Input capability:  
C
C File format:  It is not possible to send IKON plots to a disk file.
C
C Obtaining hardcopy:  Not possible.
C---
C 30-Jan-1988 - [AFT].
C-----------------------------------------------------------------------
	INCLUDE '($IODEF)'
C
	CHARACTER MSG*10
	INTEGER   GRIK00, SYS$DASSGN, GRGMEM, SYS$QIOW
	INTEGER   I0, J0, ISTAT
	INTEGER*2 ITMP(9), INIT(51), IOSB(4)
	INTEGER   IREM, ICHAN, MXCNT, ICNT, IBADR, ICOL, NPTS, INEWP
	SAVE      IREM, ICHAN, MXCNT, ICNT, IBADR, ICOL, NPTS, INEWP
	LOGICAL   APPEND
	SAVE      APPEND
	DATA INIT/82,15,0,     0,  0,  0, 255,255,255, 255,  0,  0,
     :		  0,255,  0,   0,  0,255,   0,255,255, 255,  0,255,
     :		255,255,  0, 255,127,  0, 127,255, 0,    0,255,127,
     :		  0,127,255, 127,  0,255, 255,  0,127,  85, 85, 85,
     :		170,170,170/
C---
	GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     &       110,120,130,140,150,160,170,180,900,200,
     &       210,900,230) IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
	CALL GRWARN('Unimplemented function in IK device driver: '//MSG)
	NBUF = -1
	RETURN
C
C--- IFUNC= 1, Return device name. -------------------------------------
10	CHR='IKON'
	LCHR=LEN(CHR)
	RETURN
C
C--- IFUNC= 2, Return Physical min and max for plot device. ------------
20	RBUF(1)=0
	RBUF(2)=1023
	RBUF(3)=0
	RBUF(4)=779
	RBUF(5)=0
	RBUF(6)=255
	NBUF=6
	RETURN
C
C--- IFUNC= 3, Return device resolution. -------------------------------
30	RBUF(1)=50.0
	RBUF(2)=50.0
	RBUF(3)=1
	NBUF=3
	RETURN
C
C--- IFUNC= 4, Return misc device info. --------------------------------
C I= Interactive device
C C= Cursor
C N= No hard dash
C A= Area fill
C N= No hard thick lines
40	CHR='ICNANNNNNN'
	LCHR=10
	RETURN
C
C--- IFUNC= 5, Return default file name. -------------------------------
50	CHR='IKON_DEFAULT'
	LCHR=LEN(CHR)
	NBUF=1
	RETURN
C
C--- IFUNC= 6, Return default physical size of plot. -------------------
60	RBUF(1)=0
	RBUF(2)=1023
	RBUF(3)=0
	RBUF(4)=779
	RETURN
C
C--- IFUNC= 7, Return misc defaults. -----------------------------------
70	RBUF(1)=1
	NBUF=1
	RETURN
C
C--- IFUNC= 8, Select plot. --------------------------------------------
80	CALL INIK03(NINT(RBUF(2)))
	RETURN
C
C--- IFUNC= 9, Open workstation. ---------------------------------------
90	APPEND=RBUF(3).NE.0.0
	RBUF(2)=GRIK00(ICHAN,CHR,LCHR)
	RBUF(1)=ICHAN
C---
C- Allocate a buffer.
	MXCNT=8192
	ISTAT=GRGMEM(MXCNT,IBADR)
	IF(ISTAT.NE.1) THEN
	    CALL GRWARN('Unable to allocate virtual memory.')
C- Error return
92	    CALL GRGMSG(ISTAT)
	    RBUF(2)=0
	    CALL SYS$DASSGN(%val(ICHAN))
	    RETURN
	END IF
C- MXCNT is the number of INTEGER*2
	MXCNT=MXCNT/2
	ICNT=0
C- Define channel for use by GRIK03.
	CALL INIK03(ICHAN)
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
C- Set last (x,y) to be invalid
	CALL INIK01
C---
C- Reset.
	IF(.NOT.APPEND) THEN
C- Reset interface.
	    ISTAT=SYS$QIOW(,%val(ICHAN),
     :                    %val(IO$_WRITEVBLK.OR.IO$M_RESET),
     :                    ,,,%val(0),%val(0),,,,)
C- Wait for status line A to go low (about 2.2 sec)
94	    ISTAT=SYS$QIOW(,%VAL(ICHAN),
     :                    %val(IO$_WRITEVBLK),IOSB,
     :                    ,,%val(0),%val(0),,,,)
	    IF(ISTAT.NE.1) GOTO 92
	    IF((IOSB(3).AND.'800'x) .NE. 0) THEN
		CALL LIB$WAIT(0.25)
		GOTO 94
	    END IF
	END IF
C- Set 8-bit register $3F (set mode) to 32 (non-buffered mode)
	ITMP(1)=96*256+63
	ITMP(2)=32
	CALL GRIK02(ITMP,2,%val(IBADR),ICNT,MXCNT)
	CALL GRIK03(%val(IBADR),ICNT)
C- Set 8-bit register $00 (Background color) to 0.
	ITMP(1)=96*256+0
	ITMP(2)=0
	CALL GRIK02(ITMP,2,%val(IBADR),ICNT,MXCNT)
C- Select frame buffer 0 to write
	ITMP(1)=125*256+0
C- Select frame buffer 0 to read
	ITMP(2)=124*256+0
C- Load reg. 74=xA4, AUX port setup (0 trans, rel. mouse, 19200 baud).
	ITMP(3)=96*256+74
	ITMP(4)=227
C- Disable clipping (useful if APPENDing to a GKS plot).
	ITMP(5)=203
	CALL GRIK02(ITMP,5,%val(IBADR),ICNT,MXCNT)
	IF(.NOT.APPEND) THEN
C- Load default lookup table (if not appending).
	    CALL GRIK02(INIT,51,%val(IBADR),ICNT,MXCNT)
	END IF
	RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
100	CALL SYS$DASSGN(%val(ICHAN))
	CALL GRFMEM(MXCNT,IBADR)
	RETURN
C
C--- IFUNC=11, Begin Picture. ------------------------------------------
110	IF(.NOT.APPEND) THEN
C- Set frame buffer to background color.
	    ITMP(1)=161
	    CALL GRIK02(ITMP,1,%val(IBADR),ICNT,MXCNT)
	END IF
	APPEND=.FALSE.
	RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
120	CALL GRIK01(RBUF,%val(IBADR),ICNT,MXCNT)
	RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
130	CALL GRIK05(RBUF,%val(IBADR),ICNT,MXCNT)
	RETURN
C
C--- IFUNC=14, End Picture. --------------------------------------------
140	RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
150	ICOL=MAX(0,MIN(NINT(RBUF(1)),255))
	RBUF(1)=ICOL
	ITMP(1)=65*256+ICOL
	CALL GRIK02(ITMP,1,%val(IBADR),ICNT,MXCNT)
	RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
160	CALL GRIK03(%val(IBADR),ICNT)
	RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
170	I0=RBUF(1)
	J0=RBUF(2)
	CALL GRIK04(ICHAN,I0,J0,CHR,%val(IBADR),ICNT,MXCNT)
	RBUF(1)=I0
	RBUF(2)=J0
	NBUF=2
	LCHR=1
	RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
180	RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C- Requires Ikon firmware revision V1.2 (or greater)
200	IF(NPTS.EQ.0) THEN
	    NPTS=RBUF(1)
C- Set fill drawing color register (p. 59)
	    ITMP(1)=69*256+ICOL
C- Set fill area style to solid (p. 186)
	    ITMP(2)=97*256+52
	    ITMP(3)=0
	    CALL GRIK02(ITMP,3,%val(IBADR),ICNT,MXCNT)
	    INEWP=1
	ELSE
	    NPTS=NPTS-1
	    IF(INEWP.NE.0) THEN
		INEWP=0
C- Draw filled polygon
		ITMP(1)=188
		ITMP(2)=0
		ITMP(3)=NPTS
		ITMP(4)=RBUF(1)
		ITMP(5)=RBUF(2)
		CALL GRIK02(ITMP,5,%val(IBADR),ICNT,MXCNT)
	    ELSE
		ITMP(1)=RBUF(1)
		ITMP(2)=RBUF(2)
		CALL GRIK02(ITMP,2,%val(IBADR),ICNT,MXCNT)
	    END IF
	END IF
	RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
210	ITMP(1)=81
	ITMP(2)=NINT(RBUF(1))
	ITMP(3)=IAND(255,INT(RBUF(2)*255.999))
	ITMP(4)=IAND(255,INT(RBUF(3)*255.999))
	ITMP(5)=IAND(255,INT(RBUF(4)*255.999))
	CALL GRIK02(ITMP,5,%val(IBADR),ICNT,MXCNT)
	RETURN
C
C--- IFUNC=23, Escape. -------------------------------------------------
C- Send CHR array directly to Ikon (user better know what he is doing!)
230	CALL GRIK02(%ref(CHR),LCHR/2,%val(IBADR),ICNT,MXCNT)
	RETURN
C-----------------------------------------------------------------------
	END

	INTEGER FUNCTION GRIK00(LUN,CHR,LCHR)
C-----------------------------------------------------------------------
C Open a channel to the IKON device.
C
C GRIK00 (returns integer): Opens a channel to the IKON device.
C
C  9-Dec-1987 - [AFT].
C-----------------------------------------------------------------------
        INCLUDE  '($IODEF)'
        INCLUDE  '($SSDEF)'
	INTEGER   LUN, LCHR
	CHARACTER CHR*(*)
	INTEGER   IER, ITEMP, ISTAT, LENGTH
	INTEGER   SYS$ASSIGN, SYS$QIOW
	INTEGER*2 IOSB(4)
C---
C- Assign an i/o channel
C---
	IER = SYS$ASSIGN(CHR(:LCHR), LUN,,)
	IF(IER.NE.SS$_NORMAL .AND. IER.NE.SS$_REMOTE) GOTO 800
C---
C- Poll the interface waiting for status line A to go low.
C---
100	CALL LIB$WAIT(0.5)
	ISTAT = SYS$QIOW(,%val(LUN),
     :                    %val(IO$_WRITEVBLK),IOSB,
     :                    ,,%val(0),%val(0),,,,)
 	IF( (IOSB(3).AND.'800'X) .NE. 0) GOTO 100
C---
	IF(IER .EQ. SS$_REMOTE) THEN
C---
C Cannot check device characteristics easily if network device being used
C so just check whether we opened the device successfully and return
C Read back the status from assign to plotting device over network
C---
	    IER=SYS$QIOW(,%VAL(LUN),%VAL(IO$_READVBLK),
     :                   IOSB,,,ISTAT,LENGTH,,,,)
	    IF(IOSB(1) .NE. SS$_NORMAL) THEN
	        CALL GRWARN ('Unable to read status from ASSIGN to' //
     :                      ' graphics device on remote node')
	        WRITE(6,*) IOSB(2), ' bytes read'
	        ITEMP=IOSB(1)
	        CALL GRGMSG(ITEMP)
	        GRIK00=0
	        RETURN
	    END IF
	    IF(ISTAT .NE. SS$_NORMAL) THEN
	        IER=ISTAT
	        GOTO 800
	    ELSE
	        GRIK00=3
	        RETURN
	    END IF
	END IF
C---
C- Successful completion
C---
	GRIK00 = 1
	RETURN
C---
C- Error exit
C---
  800	CALL GRWARN('Cannot open graphics device '//CHR(:LCHR))
	CALL GRGMSG(IER)
	GRIK00 = 0
	END

	SUBROUTINE GRIK01(RBUF,IBUF,ICNT,MXCNT)
	REAL      RBUF(4)
	INTEGER   ICNT, MXCNT
	INTEGER*2 IBUF
C-----------------------------------------------------------------------
C Part of PGPLOT device driver for IKON
C Draw a line segment.
C
C Arguments:
C RBUF(*) (input)  Draw line from (RBUF(1),RBUF(2)) to (RBUF(3),RBUF(4))
C IBUF    (input)  Address of a buffer area
C ICNT    (in/out) Number of bytes in use in buffer
C MXCNT   (input)  Maximum size of buffer in bytes
C
C 30-Jan-1988 - [AFT]
C-----------------------------------------------------------------------
	INTEGER   IPTR
	INTEGER*2 ITMP(4)
	INTEGER*2 I0, J0, I1, J1
	INTEGER*2 LASTI, LASTJ
	SAVE      LASTI, LASTJ
C
	I0=NINT(RBUF(1))
	J0=NINT(RBUF(2))
	I1=NINT(RBUF(3))
	J1=NINT(RBUF(4))
	IF(I0.NE.LASTI .OR. J0.NE.LASTJ) THEN
	    ITMP(1)=164
	    ITMP(2)=I0
	    ITMP(3)=J0
            IPTR=3
	ELSE
	    IPTR=0
	END IF
	ITMP(IPTR+1)=178*256
	ITMP(IPTR+2)=I1
	ITMP(IPTR+3)=J1
	IPTR=IPTR+3
	LASTI=I1
	LASTJ=J1
	CALL GRIK02(ITMP,IPTR,IBUF,ICNT,MXCNT)
	RETURN
C
	ENTRY INIK01
	LASTI=-1
	LASTJ=-1
	RETURN
	END

	SUBROUTINE GRIK02(ITMP, N, IBUF, ICNT, MXCNT)
C-----------------------------------------------------------------------
C GRPCKG (internal routine for IKON driver): Transfer N words to
C the output buffer, flushing the buffer as necessary with the
C GRIK03 routine.  If the N bytes will not fit into the current
C buffer, then the buffer is first dumped.  This is to to cause
C STR to be transferred as a complete unit.
C Based on early versions of GRxx02 routines, this version does not
C use any common blocks.
C ***NOTE*** INIK03 must be called before any calls to GRIK02 to
C set the LUN/Channel to which the buffer should be dumped.
C
C Arguments:
C
C ITMP(N)	I   I*2	Data to be written.
C N		I   I	The number of words to transfer.
C IBUF		I/O I*2 The output buffer.
C ICNT		I/O I	Current number of words used in QBUF.
C MXCNT		I/O I	Maximum number of words that can be stored
C			-in IBUF.
C
C  9-Dec-1987 - [AFT].
C-----------------------------------------------------------------------
	INTEGER   N, ICNT, MXCNT, I
	INTEGER*2 ITMP(N), IBUF(MXCNT)
C---
	IF(ICNT+N.GE.MXCNT) CALL GRIK03(IBUF,ICNT)
	DO I=1,N
	    IF(ICNT.GE.MXCNT) CALL GRIK03(IBUF,ICNT)
	    ICNT=ICNT+1
	    IBUF(ICNT)=ITMP(I)
	END DO
	RETURN
	END

	SUBROUTINE GRIK03(IBUF,ICNT)
	INTEGER   ICNT
	INTEGER*2 IBUF(*)
C-----------------------------------------------------------------------
C GRPCKG(internal routine, IKON):
C set the channal to which the buffer should be dumped.
C This subroutine contains the entry point INIK03 that defines
C the variables ICHAN.
C
C Arguments:
C
C IBUF		I/O I*2 The output buffer.
C ICNT		I/O I	Current number of words used in QBUF.
C
C  9-Dec-1987 - [AFT].
C-----------------------------------------------------------------------
	INCLUDE '($IODEF)'
	INTEGER   SYS$QIOW
	INTEGER   ISTAT
	INTEGER*2 IOSB(4)
	INTEGER   INCHAN
	INTEGER   ICHAN
	SAVE      ICHAN
C
	IF(ICNT.GT.0) THEN
            ISTAT = sys$qiow(,%val(ICHAN),
     :          %val(IO$_WRITEVBLK.OR.IO$M_SETFNCT.OR.IO$M_TIMED),
     :          IOSB,,,IBUF,%val(2*ICNT),%val(15),%val(0),,)
	END IF
	ICNT=0
	RETURN
C---
	ENTRY INIK03(INCHAN)
C- Save info needed to dump buffer.
	ICHAN=INCHAN
	RETURN
	END

      SUBROUTINE GRIK04(ICHAN,IX,IY,CHR,IBUF,ICNT,MXCNT)
C
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
C Read the cursor position on the Ikon.  The cursor can be moved
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
C- 21-Jan-1988 - Based on ARDRIVER [AFT].
C---
      INCLUDE '($IODEF)'
C-
      INTEGER   SYS$QIOW
      INTEGER   SMG$CREATE_VIRTUAL_KEYBOARD, SMG$READ_KEYSTROKE
      INTEGER   ISTAT, IDSMG
      INTEGER   ISTEP, IXWAS, IYWAS, IVAL
      INTEGER*2 ITMP(9), IOSB(4), ICURS(9)
      LOGICAL   QKEY
C---
      ISTAT=SMG$CREATE_VIRTUAL_KEYBOARD(IDSMG,'SYS$COMMAND')
      IF(ISTAT.NE.1) THEN
          CALL GRGMSG(ISTAT)
          CALL GRQUIT('Fatal error.')
      END IF
C---
C- Load 32-bit reg. 26=x1A GID max position
      ITMP(1)=99*256+26
      ITMP(2)= 779
      ITMP(3)=1023
C- Load 32-bit reg. 28=x1C GID size.
      ITMP(4)=99*256+28
      ITMP(5)= 779
      ITMP(6)=1023
      CALL GRIK02(ITMP,6,IBUF,ICNT,MXCNT)
C- Load reg. 74=xA4, AUX port setup (0 trans, rel. mouse, 19200 baud).
      ITMP(1)=96*256+74
      ITMP(2)=227
C- Set up zone to constrain cursor
      ITMP(3)=99*256+44
      ITMP(4)= 779
      ITMP(5)=1023
      CALL GRIK02(ITMP,5,IBUF,ICNT,MXCNT)
C---
C- Cursor on.
      ITMP(1)=193
C- Load 8-bit reg. 24=x18 with Enable GID
      ITMP(2)=96*256+24
      ITMP(3)=128
      CALL GRIK02(ITMP,3,IBUF,ICNT,MXCNT)
C- Defaults.
      ISTEP=2
      QKEY=.FALSE.
C---
C- Position cursor.
200       ITMP(1)=164
          ITMP(2)=IX
          ITMP(3)=IY
C- Anchor GID to current position (i.e., keep cursor on screen).
          ITMP(4)=86
          CALL GRIK02(ITMP,4,IBUF,ICNT,MXCNT)
          CALL GRIK03(IBUF,ICNT)
          IXWAS=IX
          IYWAS=IY
C- See if user has typed something at keyboard.
          ISTAT=SMG$READ_KEYSTROKE(IDSMG,IVAL,,0)
          IF(ISTAT.NE.1) IVAL=0
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
     &		(IVAL.GT.57 .AND. IVAL.LT.255)) THEN
              QKEY=.TRUE.
          END IF
C---
C- Read current cursor position
C**** Due to possible hardware fault the following code will
C**** sometimes reset the IKON.
          ITMP(1)=165
          CALL GRIK02(ITMP,1,IBUF,ICNT,MXCNT)
          CALL GRIK03(IBUF,ICNT)
C- Read 4 bytes, timing out in 2 sec.
          ISTAT = sys$qiow(,%val(ICHAN),
     :          %val(IO$_READVBLK.OR.IO$M_SETFNCT.OR.IO$M_TIMED),
     :          IOSB,,,ICURS,%val(4),%val(2),%val(1),,)
          IF(ISTAT.EQ.1 .AND. IOSB(1).EQ.1) THEN
              IX=IX+ICURS(1)-IXWAS
              IY=IY+ICURS(2)-IYWAS
          END IF
          IX=MAX(IX,   0)
          IX=MIN(IX,1023)
          IY=MAX(IY,   0)
          IY=MIN(IY, 779)
          IF(IX.EQ.IXWAS .AND. IY.EQ.IYWAS) THEN
              CALL LIB$WAIT(0.05)
          END IF
      IF(.NOT.QKEY) GOTO 200
      CHR=CHAR(IVAL)
C---
C- Turn cursor off
      ITMP(1)=192
      CALL GRIK02(ITMP,1,IBUF,ICNT,MXCNT)
      CALL GRIK03(IBUF,ICNT)
C---
C- Free resources.
      CALL SMG$DELETE_VIRTUAL_KEYBOARD(IDSMG)
      RETURN
      END

	SUBROUTINE GRIK05(RBUF,IBUF,ICNT,MXCNT)
	REAL      RBUF(2)
	INTEGER   ICNT, MXCNT
	INTEGER*2 IBUF
C-----------------------------------------------------------------------
C Part of PGPLOT device driver for IKON
C Draw a dot.
C
C Arguments:
C RBUF(*) (input)  (RBUF(1),RBUF(2)) is the (x,y) position of the dot.
C IBUF    (input)  Address of a buffer area
C ICNT    (in/out) Number of bytes in use in buffer
C MXCNT   (input)  Maximum size of buffer in bytes
C
C 30-Jan-1988 - [AFT]
C-----------------------------------------------------------------------
	INTEGER*2 ITMP(3)
C
C- Move and draw pixel.
	ITMP(1)=166
	ITMP(2)=RBUF(1)
	ITMP(3)=RBUF(2)
	CALL GRIK02(ITMP,3,IBUF,ICNT,MXCNT)
	CALL INIK01
	RETURN
	END
