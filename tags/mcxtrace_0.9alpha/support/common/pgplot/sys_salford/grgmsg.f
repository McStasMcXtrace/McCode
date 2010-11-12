C*GRGMSG -- print system message (MS-DOS)
C+
      SUBROUTINE GRGMSG (ISTAT)
      INTEGER   ISTAT
C
C This routine obtains the text of the VMS system message corresponding
C to code ISTAT, and displays it using routine GRWARN. On non-VMS
C systems, it just displays the error number.
C
C Argument:
C  ISTAT (input): 32-bit system message code.
C--
C 1989-Mar-29
C-----------------------------------------------------------------------
      CHARACTER CBUF*10
C
      WRITE (CBUF, 101) ISTAT
  101 FORMAT(I10)
      CALL GRWARN('system message number: '//CBUF)
      END
