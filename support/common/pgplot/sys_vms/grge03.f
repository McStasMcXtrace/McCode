C*GRGE03
C+
      SUBROUTINE GRGE03(QBUF,ICNT)
C
C GRPCKG (Internal routine): Write ICNT bytes in QBUF onto logical
C unit LUN.  Reset ICNT to zero.
C Based on GRxx03 routines, this version does not contain a common
C block.
C ***NOTE*** INIT03 must be called before any calls to GRGE02 to
C set the LUN/Channel to which the buffer should be dumped.
C This subroutine contains the entry point INIT03 that defines
C the variables ITYPE, LUN and IFUNC.  If ITYPE=0 then LUN is
C the Fortran logical unit number to which the data should be
C written.  If ITYPE>0 then LUN is the Channel number for a QIO
C operation and IFUNC is the QIO write function.
C
C Arguments:
C
C QBUF            I/O Byte The output buffer.
C ICNT            I/O I    Current number of bytes used in QBUF.
C
C  5-Aug-1986 - [AFT].
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE '($SSDEF)'
      INTEGER SYS$QIOW
      INTEGER ICNT
      BYTE QBUF(*)
      INTEGER RESULT, N, I
      INTEGER*2 IOSB(4)
      INTEGER INTYPE,INLUN,INFUNC
      INTEGER ITYPE,LUN,IFUNC
      SAVE    ITYPE,LUN,IFUNC
C
      N = ICNT
      ICNT = 0
      IF (N.LT.1) RETURN
C
      IF(ITYPE.EQ.0) THEN
          WRITE(LUN,101, ERR=900) (QBUF(I),I=1,N)
  101     FORMAT(130A1)
      ELSE
          RESULT = SYS$QIOW(,%VAL(LUN),
     1            %VAL(IFUNC),IOSB,,,
     2            QBUF,%VAL(N),%VAL(5),,,)
          IF (RESULT.NE.SS$_NORMAL) THEN
            CALL GRGMSG(RESULT)
            CALL GRGMSG('SYS$QIOW error writing to device.  '//
     &            'Program continues.')
          END IF
          IF (IOSB(1).NE.SS$_NORMAL) THEN
            CALL GRGMSG(IOSB(1))
            CALL GRGMSG('SYS$QIOW (IOSB) status writing to device.  '//
     &            'Program continues.')
          END IF
      END IF
      RETURN
C---
  900 CONTINUE
      RETURN
C---
      ENTRY INIT03(INTYPE,INLUN,INFUNC)
C--- Save info needed to dump buffer.
      ITYPE=INTYPE
      LUN=INLUN
      IFUNC=INFUNC
      RETURN
      END
