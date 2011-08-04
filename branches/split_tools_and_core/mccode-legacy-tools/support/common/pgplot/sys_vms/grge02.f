C*GRGE02
C+
      SUBROUTINE GRGE02(STR, N, QBUF,ICNT,MXCNT)
C
C GRPCKG (internal routine for general driver): transfer N bytes to
C the output buffer, flushing the buffer as necessary with the
C GRGE03 routine.  Based on early versions of GRxx02 routines.
C This version does not used any common blocks.
C ***NOTE*** INIT03 must be called before any calls to GRGE02 to
C set the LUN/Channel to which the buffer should be dumped.
C
C Arguments:
C
C STR(N)      I   Byte  Characters to be written.
C N           I   I     The number of bytes to transfer.
C QBUF        I/O Byte  The output buffer.
C ICNT        I/O I     Current number of bytes used in QBUF.
C MXCNT       I/O I     Maximum number of bytes that can be stored
C                       in QBUF.
C
C  5-Aug-1986 - [AFT].
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ICNT,MXCNT,I,N
      BYTE QBUF(N), STR(N)
C---
      DO I=1,N
          IF (ICNT.GE.MXCNT) CALL GRGE03(QBUF,ICNT)
          ICNT = ICNT+1
          QBUF(ICNT) = STR(I)
      END DO
      RETURN
      END
