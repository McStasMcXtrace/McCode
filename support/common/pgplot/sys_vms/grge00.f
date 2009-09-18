
C*GRGE00 -- open output device; device handler routine (VMS)
C+
      INTEGER FUNCTION GRGE00(CTYP,LUN,CHR,LCHR)
C
C General routine to open plot device.  This version can open
C 1) A file with of any FORM and CARRIAGECONTROL,
C 2) Any local device for QIO operations,
C 3) A remote device over a network using a network task on the
C    remote node.
C If an error occurs, a message is sent to SYS$OUTPUT via GRWARN.
C
C Arguments:
C- CTYP(1:1)='F' I/O will use standard Fortran I/O and
C-       CTYP(2:2)      ='F' for formatted, ='U' for unformatted.
C-      CTYP(3:3)      ='L' for LIST, ='N' for 'NONE carriagecontrol
C-      LUN            return, logical unit number of file.
C-       CHR(:LCHR)      input, name of file to open.
C- CTYP(1:1)='Q' for DEC QIO and
C-      CTYP(2:2)      ='1' for device type 66 expected.
C-      CTYP(2:2)      ='2' for device type 96 expected.
C-      LUN            return, channel number of opened channel,
C-       CHR(:LCHR)      input, name of device to open.
C
C GRGE00 (returns integer): 0 if the device/channel could
C      not be opened, 1 if the file/channel was opened
C      successfully on a local device, 3 for a successful open
C      of a channel over a network (the remote status of the
C      device must be flagged since, the QIO functions codes are
C      different when writting to a physical device or to the
C      network).
C--
C  5-Aug-1986 - [AFT].
C-----------------------------------------------------------------------
      INCLUDE  '($IODEF)'
      INCLUDE  '($SSDEF)'
      INTEGER  DVI$_DEVCLASS, DVI$_DEVNAM
      PARAMETER (DVI$_DEVCLASS=4)
      PARAMETER (DVI$_DEVNAM=32)
      INTEGER LUN,LCHR
      CHARACTER*(*) CTYP,CHR
      CHARACTER*16 CFORM,CONTRL
      INTEGER  GRCHKT, I, IER, IK1, IK2, IK3, IK4, IK5, ITEMP
      INTEGER  DEVCLASS, ITMLIST(7), MOSB(2), ISTAT, LENGTH
      INTEGER  SYS$ASSIGN, SYS$QIOW
      INTEGER  SYS$GETDVI, SYS$DASSGN, SYS$WAITFR
      INTEGER*2 IOSB(4)
      LOGICAL  WRONG
C
      IF(CTYP(1:1).EQ.'F') THEN
          CALL GRGLUN(LUN)
          CFORM=' '
          IF(CTYP(2:2).EQ.'F') CFORM='FORMATTED'
          IF(CTYP(2:2).EQ.'U') CFORM='UNFORMATTED'
          CONTRL=' '
          IF(CTYP(3:3).EQ.'N') CONTRL='NONE'
          IF(CTYP(3:3).EQ.'L') CONTRL='LIST'
          OPEN (UNIT=LUN,FILE=CHR(:LCHR),STATUS='NEW',
     &            FORM=CFORM, CARRIAGECONTROL=CONTRL,
     &            RECL=512,IOSTAT=IER)
          IF (IER.NE.0) THEN
            CALL ERRSNS(IK1,IK2,IK3,IK4,IK5)
            CALL GRWARN('Cannot open graphics device '
     1                        //CHR(1:LCHR))
            IF (IK2.NE.0 .AND. IK2.NE.1) CALL GRGMSG(IK2)
            IF (IK5.NE.0 .AND. IK5.NE.1) CALL GRGMSG(IK5)
            GRGE00 = 0
          ELSE
            INQUIRE (UNIT=LUN, NAME=CHR)
            I = LEN(CHR)
            DO WHILE (CHR(I:I).EQ.' ')
                I = I-1
            END DO
            LCHR= I
            IF (GRCHKT(CHR(1:I))) THEN
                CALL GRWARN('Cannot send printer plot to terminal.')
                GRGE00 = 0
            ELSE
                GRGE00 = 1
            END IF
          END IF
      ELSE IF(CTYP(1:1).EQ.'Q') THEN
C
C Assign an i/o channel.
C
          IER = SYS$ASSIGN(CHR(:LCHR), LUN,,)
          IF(IER.NE.SS$_NORMAL .AND. IER.NE.SS$_REMOTE) GOTO 100
          IF (IER .EQ. SS$_REMOTE) THEN
C
C Cannot check device characteristics easily if network device being used
C so just check whether we opened the device successfully and return
C Read back the status from assign to plotting device over network
C
            IER=SYS$QIOW(,%VAL(LUN),%VAL(IO$_READVBLK),
     :                   IOSB,,,ISTAT,LENGTH,,,,)
            IF (IOSB(1) .NE. SS$_NORMAL) THEN
                CALL GRWARN ('Unable to read status from ASSIGN to' //
     :                      ' graphics device on remote node')
                WRITE(6,*) IOSB(2), ' bytes read'
                ITEMP=IOSB(1)
                CALL GRGMSG(ITEMP)
                GRGE00=0
                RETURN
            END IF
            IF (ISTAT .NE. SS$_NORMAL) THEN
                IER=ISTAT
                GOTO 100
            ELSE
                GRGE00=3
                RETURN
            END IF
          END IF
C---
C            Check that device has correct characteristics,
C            and obtain true device name.
C
          ITMLIST(1) = DVI$_DEVCLASS*2**16 + 4
          ITMLIST(2) = %LOC(DEVCLASS)
          ITMLIST(3) = 0
          ITMLIST(4) = DVI$_DEVNAM*2**16 + LEN(CHR)
          ITMLIST(5) = %LOC(CHR)
          ITMLIST(6) = %LOC(LCHR)
          ITMLIST(7) = 0
          IER = SYS$GETDVI(%VAL(0),,CHR(:LCHR),
     1                 ITMLIST,MOSB,,,)
          IF (.NOT.IER) GOTO 100
          IER = SYS$WAITFR(%VAL(0))
          IF (.NOT.IER) GOTO 100
          IF (.NOT.MOSB(1)) THEN
            IER = MOSB(1)
            GOTO 100
          END IF
          IF (CTYP(2:2).EQ.'1') THEN
            WRONG = DEVCLASS.NE.66
          ELSE IF(CTYP(2:2).EQ.'2') THEN
            WRONG = DEVCLASS.NE.96
          ELSE
            TYPE *,'DEVCLASS=',DEVCLASS
          END IF
          IF (WRONG) THEN
            CALL GRWARN( CHR(:LCHR)//
     2            ' is the wrong sort of device for plot type.')
            GRGE00 = 0            ! indicate error
            IER = SYS$DASSGN(%VAL(LUN))
            RETURN
          END IF
C
C Successful completion.
C
          GRGE00 = 1
      END IF
      RETURN
C
C Error exit.
C
  100 CALL GRWARN('Cannot open graphics device '//CHR(:LCHR))
      CALL GRGMSG(IER)
      GRGE00 = 0
      END
