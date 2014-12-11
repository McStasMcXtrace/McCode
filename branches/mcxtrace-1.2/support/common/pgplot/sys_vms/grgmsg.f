
C*GRGMSG -- print system message (VMS)
C+
      SUBROUTINE GRGMSG (STATUS)
      INTEGER STATUS
C
C This routine obtains the text of the VMS system message corresponding
C to code STATUS, and displays it using routine GRWARN.
C
C Argument:
C  STATUS (input): 32-bit system message code.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER*80 BUFFER
      INTEGER  LENGTH, MSGLEN
C
      CALL SYS$GETMSG(%VAL(STATUS),MSGLEN,BUFFER,%VAL(1),)
      LENGTH = INDEX(BUFFER(1:MSGLEN),'!')
      IF (LENGTH.GT.1) MSGLEN = LENGTH-1
      CALL GRWARN(BUFFER(1:MSGLEN))
      END
