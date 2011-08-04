C Date:     3-FEB-1988 15:08:32 GMT
C From:     AFT@AST-STAR.CAM.AC.UK
C To:       TJP@CITPHOBO
C Subject:  LIDRIVER.FOR

C*LIDRIV -- PGPLOT driver form Liacom device

	SUBROUTINE LIDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
C- GRPCKG driver for Liacom image device.
C---
C Supported device:  Liacom Graphic Video Display (GVD-02).
C
C Device type code:  /LIacom
C
C Default device name:  LIACOM_DEVICE (a logical name).
C
C Default view surface dimensions:  Depends on monitor.
C
C Resolution:  The full view surface is 512 by 512 pixels.
C
C Color capability:  Color indices 0-15 are supported.  The default
C   color representation is as listed in Chapter 5 of the PGPLOT
C   Manual.  The representation of all color indices can be changed.
C   Color indices 128-255 map into the second row of bit planes and
C   support 15 levels of grey scale.
C
C Input capability:  Cursor is a fat white cross.
C
C File format:  It is not possible to send Liacom plots to a disk file.
C
C Obtaining hardcopy:  Not possible.
C
C  5-Aug-1986 - [AFT].
C 16-Jan-1988 - Track PGPLOT [AFT].
C-----------------------------------------------------------------------
	INCLUDE '($IODEF)'
	INCLUDE '($SSDEF)'
	INTEGER   IFUNC,NBUF,LCHR,I0,J0,I1,J1
	REAL      RBUF(6)
	CHARACTER CHR*(*)
C
	INTEGER   GRGE00, SYS$DASSGN, GRGMEM, IER
	INTEGER*2 ITMP(10), ICHAN
	INTEGER   IREM, IWRT, IRD, LUN, MXCNT, ICNT, IBADR, ICOL
	SAVE      IREM, IWRT, IRD, LUN, MXCNT, ICNT, IBADR, ICOL
	LOGICAL   APPEND
	SAVE      APPEND
C---
	GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     &       110,120,130,140,150,160,170,999,999,999,
     &       210,999,230) IFUNC
	GOTO 999
C---
C- IFUNC= 1, Return device name.
10	CHR='LIACOM'
	LCHR=LEN(CHR)
	RETURN
C---
C- IFUNC= 2, Return Physical min and max for plot device.
20	RBUF(1)=0
	RBUF(2)=511
	RBUF(3)=0
	RBUF(4)=511
	RBUF(5)=0
	RBUF(6)=255
	NBUF=6
	RETURN
C---
C- IFUNC= 3, Return device (X and Y) resolution in pixels per inch as
C- formatted numbers in CHR.
30	RBUF(1)=50.0
	RBUF(2)=50.0
	RBUF(3)=1
	NBUF=3
	RETURN
C---
C- IFUNC= 4, Return misc device info.
40	CHR='ICNNNNNNNN'
	LCHR=10
	RETURN
C---
C- IFUNC= 5, Return default file name.
50	CHR='LIACOM_DEVICE'
	LCHR=LEN(CHR)
	NBUF=1
	RETURN
C---
C- IFUNC= 6, Return default physical size of plot.
60	RBUF(1)=0
	RBUF(2)=511
	RBUF(3)=0
	RBUF(4)=511
	RETURN
C---
C- IFUNC= 7, Return misc defaults.
70	RBUF(1)=1
	NBUF=1
	RETURN
C---
C- IFUNC= 8, Set active plot.
80	CALL INIT03(1,LUN,IWRT)
	RETURN
C---
C- IFUNC= 9, Open workstation.
90	APPEND = RBUF(3).NE.0.0
	RBUF(2)=GRGE00('Q2 ',LUN,CHR,LCHR)
	IF(RBUF(2).EQ.1) THEN
	    IREM=0
	    IWRT='000010A0'X
	    IRD ='000010A0'X
	ELSE IF(RBUF(2).EQ.3) THEN
	    IREM=1
	    IWRT=IO$_WRITEVBLK
	    IRD =IO$_READVBLK
	    RBUF(2)=1
	ELSE
	    IWRT=0
	END IF
	RBUF(1)=LUN
	IF(NINT(RBUF(2)).EQ.1) THEN
	    MXCNT=8192
	    IER=GRGMEM(MXCNT,IBADR)
	    IF(IER .NE. SS$_NORMAL) THEN
		CALL GRGMSG(IER)
		CALL GRWARN('Unable to allocate virtual memory.')
		RBUF(2)=0
		CALL SYS$DASSGN(%val(LUN))
		RETURN
	    END IF
	    CALL INIT03(1,LUN,IWRT)
	    IF(RBUF(3).EQ.0) CALL GRLI10(%val(IBADR),ICNT,MXCNT)
	END IF
	RETURN
C---
C- IFUNC=10, Close workstation.
100	IF(IREM.NE.0) CALL GRLI12(LUN)
	CALL SYS$DASSGN(%val(LUN))
	CALL GRFMEM(MXCNT,IBADR)
	RETURN
C---
C- IFUNC=11, Begin Picture.
110	ITMP(1)='1088'x		! Select cursor
	ITMP(2)='7000'x		! Cursor off
	ITMP(3)='103F'x		! Select all bit planes
	ITMP(4)='700F'x		! Set video blank to open (display on)
	ITMP(5)='1010'x+ICOL	! Select planes corresponding to color
	CALL GRGE02(ITMP,10,%val(IBADR),ICNT,MXCNT)
	CALL GRGE03(%val(IBADR),ICNT)
	IF(.NOT.APPEND) THEN
	  ITMP(1)='103F'x	! Select all bit planes
	  ITMP(2)='3000'x	! Erase
	  CALL GRGE02(ITMP, 4,%val(IBADR),ICNT,MXCNT)
	END IF
	APPEND=.FALSE.
	RETURN
C---
C- IFUNC=12, Draw line.
120	I0=NINT(RBUF(1))
	J0=NINT(RBUF(2))
	I1=NINT(RBUF(3))
	J1=NINT(RBUF(4))
	CALL GRLI01(I0,J0,I1,J1,%val(IBADR),ICNT,MXCNT,ICOL)
	RETURN
C---
C- IFUNC=13, Draw dot.
130	I0=NINT(RBUF(1))
	J0=NINT(RBUF(2))
	CALL GRLI01(I0,J0,I0,J0,%val(IBADR),ICNT,MXCNT,ICOL)
	RETURN
C---
C- IFUNC=14, End Picture.
140	RETURN
C---
C- IFUNC=15, Select color index.
150	ICOL=MAX(0,MIN(NINT(RBUF(1)),255))
	RBUF(1)=ICOL
155	IF(ICOL.GT.0) THEN
	    IF(ICOL.LT.128) THEN
C- Select bit planes in row 1.
	        ITMP(1)='1010'x+IAND(ICOL,15)
	    ELSE
C- Select bit planes in row 2 (must be in range 1-15).
	        ITMP(1)='1020'x+1+INT(15.*IAND((ICOL-128),127)/128.)
	    END IF
	ELSE
C- Select all planes in both rows.
	    ITMP(1)='103F'x
	END IF
	CALL GRGE02(ITMP,2,%val(IBADR),ICNT,MXCNT)
	RETURN
C---
C- IFUNC=16, Flush buffer.
160	CALL GRGE03(%val(IBADR),ICNT)
	RETURN
C---
C- IFUNC=17, Make cursor visible and read position.
170	I0=RBUF(1)
	J0=RBUF(2)
	CALL GRLI11(I0,J0,CHR,LUN,%val(IBADR),ICNT,MXCNT,ICOL,IRD)
	RBUF(1)=I0
	RBUF(2)=J0
	NBUF=2
	LCHR=1
	RETURN
C---
C- IFUNC=21, Set color representation.
210	ITMP( 1)='1084'x		! Select LUT.
	ICHAN=NINT(RBUF(1))
	IF(ICHAN.LT.128) THEN
C- Select bit planes in row 1.
	    ICHAN=IAND(ICHAN,15)
	ELSE
C- Select bit planes in row 2.
	    ICHAN=1+INT(15.*IAND((ICHAN-128),127)/128.)
	    ICHAN=16*ICHAN
	END IF
C
	ITMP( 2)='4300'x+ICHAN		! Load address
	ITMP( 3)='7001'x		! Enable channel 1 (red).
	I0=IAND(255,INT(RBUF(2)*255.999))
	ITMP( 4)='5000'x+I0		! Load data (color level).
C
	ITMP( 5)='4300'x+ICHAN		! Load address
	ITMP( 6)='7002'x		! Enable channel 2 (green).
	I0=IAND(255,INT(RBUF(3)*255.999))
	ITMP( 7)='5000'x+I0		! Load data (color level).
C
	ITMP( 8)='4300'x+ICHAN		! Load address
	ITMP( 9)='7004'x		! Enable channel 4 (blue).
	I0=IAND(255,INT(RBUF(4)*255.999))
	ITMP(10)='5000'x+I0		! Load data (color level).
C
	CALL GRGE02(ITMP,20,%val(IBADR),ICNT,MXCNT)
	GOTO 155
C---
C- IFUNC=23, Escape.
230	CALL GRGE02(%ref(CHR),LCHR,%val(IBADR),ICNT,MXCNT)
	RETURN
C---
C- Flag function not implemented.
999	NBUF=-1
	RETURN
	END
     
	SUBROUTINE GRLI01 (I0,J0,I1,J1,IBUF,ICNT,MXCNT,ICOL)
C-----------------------------------------------------------------------
C GRPCKG (internal routine, Liacom): draw a line segment.  The VAX can
C send data to the Liacom faster than the Liacom can plot it, if lines
C are more than 255 pixels in length.  To avoid this problem, long
C lines must be broken into shorter segments.
C
C Arguments:
C
C I0,J0 (integer, input): the column and row numbers of the starting
C	point.
C I1,J1 (integer, input): the column and row numbers of the end point.
C
C 11-Apr-1983 - Original.
C 18-Jan-1988 - Break long lines into segments [AFT].
C-----------------------------------------------------------------------
	INTEGER   I0,J0,I1,J1,IBUF,ICNT,MXCNT,ICOL
C
	INTEGER   CLIP511,X
	INTEGER   IC0, JC0, IC1, JC1, IDXA, IDYA
	INTEGER   IDX, IDY, IDIST, ISX, ISY
	INTEGER*2 ITMP(11)
C---
	CLIP511(X) = X .AND. '1FF'X
C
	IC0 = CLIP511(I0)
	JC0 = CLIP511(J0)
	IC1 = CLIP511(I1)
	JC1 = CLIP511(J1)
	IDX= IC1-IC0
	IDY= JC1-JC0
	JC0= 511-JC0
	IDXA= ABS(IDX)
	IDYA= ABS(IDY)
	IF(IDX.LT.0) THEN
	    ISX='0200'x
	ELSE
	    ISX=0
	END IF
	IF(IDY.LT.0) THEN
	    ISY='0200'x
	ELSE
	    ISY=0
	END IF
	IDIST=IDXA*IDXA+IDYA*IDYA
C---
	ITMP(1) = '4000'x .OR. IC0	! LXA
	ITMP(2) = '5000'x .OR. JC0	! LYA
	IF(IDIST.EQ.0) THEN
	    IF(ICOL.GT.0) THEN
		ITMP(3)='8010'x
	    ELSE
		ITMP(3)='8000'x
	    END IF
	    CALL GRGE02(ITMP, 6,IBUF,ICNT,MXCNT)
	ELSE IF(IDIST.LE.255*255) THEN
C---
C- Short lines can be dones as a single vector.
C
	    IF(ICOL .GT. 0) THEN
C- FSV-positive.
		ITMP(3) = '2001'x
	    ELSE
C- FSV-negative
		ITMP(3) = '2000'x
	    ENDIF
C- Note reverse order.
	    ITMP(4) = IDYA .OR. ISY
	    ITMP(5) = IDXA .OR. ISX
	    CALL GRGE02(ITMP,10,IBUF,ICNT,MXCNT)
	ELSE IF(IDIST.LE.2*255*255) THEN
C---
C- Break vector into two segments.
	    IF(ICOL .GT. 0) THEN
C- FSV-positive.
		ITMP(3) = '2001'x
		ITMP(6) = '2001'x
	    ELSE
C- FSV-negative
		ITMP(3) = '2000'x
		ITMP(6) = '2000'x
	    ENDIF
C- Note reverse order.
	    IDY=IDYA/2
	    ITMP(4)=IDY .OR. ISY
	    IDX=IDXA/2
	    ITMP(5)=IDX .OR. ISX
	    ITMP(7)=(IDYA-IDY) .OR. ISY
	    ITMP(8)=(IDXA-IDX) .OR. ISX
	    CALL GRGE02(ITMP,16,IBUF,ICNT,MXCNT)
	ELSE
C---
C- Break vector into three segments. (The line must be diagonal.)
	    IF(ICOL .GT. 0) THEN
C- FSV-positive.
		ITMP(3) = '2001'x
		ITMP(6) = '2001'x
		ITMP(9) = '2001'x
	    ELSE
C- FSV-negative
		ITMP(3) = '2000'x
		ITMP(6) = '2000'x
		ITMP(9) = '2000'x
	    ENDIF
C- Note reverse order.
	    IDY=IDYA/3
	    ITMP(4)=IDY .OR. ISY
	    ITMP(7)=ITMP(4)
	    IDX=IDXA/3
	    ITMP(5)=IDX .OR. ISX
	    ITMP(8)=ITMP(5)
	    ITMP(10)=(IDYA-2*IDY) .OR. ISY
	    ITMP(11)=(IDXA-2*IDX) .OR. ISX
	    CALL GRGE02(ITMP,22,IBUF,ICNT,MXCNT)
	END IF
C
	RETURN
	END

	SUBROUTINE GRLI10(IBUF,ICNT,MXCNT)
C--------------------------------------------------------------------
C GRPCKG (Internal routine, Liacom)
C Loads standard PGPLOT look-up table.
C--------------------------------------------------------------------
	INTEGER IBUF(1),ICNT,MXCNT
	INTEGER*2 LIABUF(58)/'1084'X, 'C001'X, '0000'X,
     &	  '7001'x, '4300'x, '5000'x, '50FF'x, '50FF'x, '5000'x,
     &			    '5000'x, '5000'x, '50FF'x, '50FF'x,
     &			    '50FF'x, '508F'x, '5000'x, '5000'x,
     &			    '508F'x, '50FF'x, '5055'x, '50AA'x,
     &	  '7002'x, '4300'x, '5000'x, '50FF'x, '5000'x, '50FF'x,
     &			    '5000'x, '50FF'x, '5000'x, '50FF'x,
     &			    '508F'x, '50FF'x, '50FF'x, '508F'x,
     &			    '5000'x, '5000'x, '5055'x, '50AA'x,
     &	  '7004'x, '4300'x, '5000'x, '50FF'x, '5000'x, '5000'x,
     &			    '50FF'x, '50FF'x, '50FF'x, '5000'x,
     &			    '5000'x, '5000'x, '508F'x, '50FF'x,
     &			    '50FF'x, '508F'x, '5055'x, '50AA'x,
     &    '1010'x/
C---
C- Actions taken by LIABUF are:
C- Select lookup table, send block of zeros,
C- for each channel define bottom 8 colors,
C- Select bottom plane only (color 1).
C---
	CALL GRGE02(LIABUF,116, IBUF,ICNT,MXCNT)
	CALL GRGE03(IBUF,ICNT)
	RETURN
	END

	SUBROUTINE GRLI11(IX,IY,LETTER,LUN,IBUF,ICNT,MXCNT,ICOL,IRD)
C------------------------------------------------------------
C  GRLI11:  (Grpckg internal routine, Liacom)
C  Make cursor visible, then after reading a character
C  from the terminal, read the cursor position.
C
C  Arguments:
C
C  IX,IY (integer, input/output):  position of cursor.
C  LETTER (character, output):  character typed by user.
C
C  19-OCT-1983
C------------------------------------------------------------
	INCLUDE '($SSDEF)'
	INTEGER   IX, IY, LUN, IBUF(1), ICNT, MXCNT, ICOL, IRD
	CHARACTER LETTER
	INTEGER*2 X, Y, LIABUF(4), IOSB(5)
	INTEGER   SYS$QIOW, SYS$ASSIGN, SYS$DASSGN, GRGETC
	INTEGER   I
	INTEGER   ICHAN, IER
C---
	IF(IX .LT. 0) THEN
		X= 0
	ELSE IF(IX .GT. 511) THEN
		X= 511
	ELSE
		X= IX
	END IF
C
	IF(IY .LT. 0) THEN
		Y= 0
	ELSE IF(IY .GT. 511) THEN
		Y= 511
	ELSE
		Y= IY
	END IF
C
	IER = SYS$ASSIGN('TT', ICHAN, ,)
	IF(IER .NE. 1) THEN
		CALL GRGMSG(IER)
		CALL GRQUIT('Fatal error.')
	END IF
C
	LIABUF(1)= '1088'X		!  Select cursor.
	LIABUF(2)= '7003'X		!  Make visible, non-blinking.
	LIABUF(3)= '4000'X .OR. X	!  Load X address.
	LIABUF(4)= '5000'X .OR. (511-Y)	!  Load Y address.
	CALL GRGE02(LIABUF, 8, IBUF,ICNT,MXCNT)
	CALL GRGE03(IBUF,ICNT)
C
	LETTER= CHAR(GRGETC(ICHAN))
C
	LIABUF(1)= 'F002'X		! Initiate readback.
	CALL GRGE02(LIABUF, 2, IBUF,ICNT,MXCNT)
	CALL GRGE03(IBUF,ICNT)
C
	IER= SYS$QIOW(,%val(LUN), %val(IRD),IOSB,,,
     &		    LIABUF, %val(6), %val(5),,,)
	IF(IER .NE. SS$_NORMAL) THEN
		CALL GRGMSG(IER)
		CALL GRQUIT('Error reading from Liacom.')
	END IF
C
	DO I=1, 3
	END DO
C
	IX= LIABUF(2) .AND. '01FF'X
	IY= LIABUF(3) .AND. '01FF'X
	IY= 511-IY
C
	IER= SYS$DASSGN(%val(ICHAN))
C
	LIABUF(1)= '7000'X		!  Make cursor invisible.
	LIABUF(2)= '1010'X + ICOL	!  Back to vector graphics.
	CALL GRGE02(LIABUF, 4, IBUF,ICNT,MXCNT)
	CALL GRGE03(IBUF,ICNT)
C
	RETURN
	END

	SUBROUTINE GRLI12(LUN)
C-----------------------------------------------------------------------
	INCLUDE  '($IODEF)'
	INCLUDE  '($SSDEF)'
	INTEGER   SYS$QIOW
	INTEGER  LUN
	INTEGER  IER, LENGTH, ITEMP
	INTEGER*2 LIABUF, IREC, IOSB(4)
C---
	IREC='FFFF'X
C
C  For the Liacom across the network send an END request and wait to be told
C  that the Liacom has been deassigned - this avoids problems of trying
C  to reallocate it before it has been deassigned
C
	LIABUF='2002'X
	LENGTH=2
	IER=SYS$QIOW(,%val(LUN),%val(IO$_WRITEVBLK),IOSB,,,
     :			LIABUF,%val(LENGTH),,,,)
	IF(IOSB(1) .NE. SS$_NORMAL) THEN
	    CALL GRWARN ('Error sending disconnect request' //
     :                      ' to task on remote node')
	    ITEMP=IOSB(1)
	    CALL GRGMSG(ITEMP)
	END IF
	IER=SYS$QIOW(,%val(LUN),%val(IO$_READVBLK),IOSB,,,
     :			LIABUF,LENGTH,,,,)
	IF(IOSB(1) .NE. SS$_NORMAL) THEN
	    CALL GRWARN ('Unable to read Liacom free message' //
     :                      ' from task on remote node')
	    WRITE(6,*) IOSB(2), ' bytes read'
	    ITEMP=IOSB(1)
	    CALL GRGMSG(ITEMP)
	END IF
	IF(LIABUF .NE. IREC) THEN
	    WRITE(*,500) 'Wrong data read from remote task ',
     :		 IREC, ' expected, ',LIABUF,' read'
 500	    FORMAT(1X,A,Z6,A)
	END IF
	RETURN
	END
