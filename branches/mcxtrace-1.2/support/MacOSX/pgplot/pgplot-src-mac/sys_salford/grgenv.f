C*********
c     INTERFACE TO CHARACTER FUNCTION GETENV [C]
c    +   (CBUF[REFERENCE])
C---
C Allow MS-Fortran to call the GETENV function built into the
C Fortran 5.0 library.
C---
c     CHARACTER*1 CBUF
c     END
C*********
C*GRGENV -- get value of PGPLOT environment parameter (MS-DOS)
C+
      SUBROUTINE GRGENV(CNAME, CVALUE, LVALUE)
      CHARACTER CNAME*(*), CVALUE*(*)
      INTEGER   LVALUE
      INTEGER*2 NONBLK
C
C Return the value of a PGPLOT environment parameter.
C
C Arguments:
C CNAME   : (input) the name of the parameter to evaluate.
C CVALUE  : receives the value of the parameter, truncated or extended
C           with blanks as necessary. If the parameter is undefined,
C           a blank string is returned.
C LVALUE  : receives the number of characters in CVALUE, excluding
C           trailing blanks. If the parameter is undefined, zero is
C           returned.
C--
C 1990-Mar-19 - [AFT]
C 1995-Mar-21   [mlm]
C     MODIFIED MARCH 1995 TO INTERFACE WITH SALFORD SOFTWARE FTN77
C
C
C-----------------------------------------------------------------------
      CHARACTER GETENV*64
C
      CHARACTER CTMP*64
      INTEGER   I, LTMP
C
C     ENVIRONMENT PREFIX CHANGED TO 'PG'  (mlm)
C
      CTMP = 'PG'//CNAME
      LTMP = INDEX(CTMP,' ')
      IF(LTMP.NE.0) THEN
          LTMP=LTMP-1
          ELSE
          LTMP=LEN(CTMP)
      ENDIF
C
C     GET ENVIRONMENT USING SALFORD ROUTINE
C
      CALL DOSPARAM@(CTMP(1:LTMP),CVALUE)
C
C     CHECK IF STRING IS EMPTY; IN CASE, RETURN L = 0
C     SALFORD ROUTINE NONBLK RETURNS POSITION OF FIRST NONBLANK CHARACTER
C     IN THE STRING, 0 IF ALL BLANK
C
      IF (NONBLK(CVALUE) .EQ. 0) THEN
         LVALUE = 0
         ELSE
      LVALUE = LEN(CVALUE)
      ENDIF
      RETURN
      END
