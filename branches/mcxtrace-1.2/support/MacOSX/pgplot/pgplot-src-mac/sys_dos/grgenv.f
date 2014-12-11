C*********
      INTERFACE TO CHARACTER FUNCTION GETENV [C]
     +   (CBUF[REFERENCE])
C---
C Allow MS-Fortran to call the GETENV function built into the
C Fortran 5.0 library.
C---
      CHARACTER*1 CBUF
      END
C*********

C*GRGENV -- get value of PGPLOT environment parameter (MS-DOS)
C+
      SUBROUTINE GRGENV(CNAME, CVALUE, LVALUE)
      CHARACTER CNAME*(*), CVALUE*(*)
      INTEGER   LVALUE
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
C-----------------------------------------------------------------------
      CHARACTER GETENV*64
C
      CHARACTER CTMP*64
      INTEGER   I, LTMP
C
      CTMP = 'PGPLOT_'//CNAME
      LTMP = INDEX(CTMP,' ')
      IF(LTMP.EQ.0) LTMP=LEN(CTMP)-1
      CTMP(LTMP:LTMP)=CHAR(0)
      CTMP=GETENV(CTMP(:LTMP))
      CVALUE = ' '
      LVALUE = 0
C---
C MS-Fortran Kludge, if the environment variable is undefined, then
C GETENV points to NULL (memory location zero).  I see no easy way to
C detect this condition in Fortran, therefore, I compare with an
C environment variable that noone would ever define and hence should
C always point at NULL.
      IF(GETENV('#$%^'//CHAR(0)).EQ.CTMP) GOTO 140
      DO 130 I=1,LEN(CTMP)
         IF(CTMP(I:I).EQ.CHAR(0)) GOTO 140
         LVALUE=LVALUE+1
         CVALUE(LVALUE:LVALUE)=CTMP(I:I)
  130 CONTINUE
  140 CONTINUE
      RETURN
      END
