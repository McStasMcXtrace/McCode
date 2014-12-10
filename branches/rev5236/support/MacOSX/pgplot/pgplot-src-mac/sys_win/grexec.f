C*GREXEC -- PGPLOT device handler dispatch routine
C 12/93 C. T. Dum: version for MS F32 Power Station
C  20-Apr-1996: "W9DRIV", use CASE structure [PAS]
C  10-Jul-1996: add GIF driver (already had PostScript, Null, LaTex) [PAS]
C+
      SUBROUTINE GREXEC(IDEV, IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IDEV, IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C---
      INTEGER NDEV
      PARAMETER (NDEV=13)
      CHARACTER*10 MSG
C---
      SELECT CASE (IDEV)
      CASE (0)
         RBUF(1) = NDEV
         NBUF = 1
         CHR = ' '
         LCHR = 0
      CASE (1)
         CALL W9DRIV(IFUNC,RBUF,NBUF,CHR,LCHR,0)
      CASE (2)
         CALL W9DRIV(IFUNC,RBUF,NBUF,CHR,LCHR,1)
      CASE (3)
         CALL W9DRIV(IFUNC,RBUF,NBUF,CHR,LCHR,2)
      CASE (4)
         CALL W9DRIV(IFUNC,RBUF,NBUF,CHR,LCHR,3)
      CASE (5)
         CALL W9DRIV(IFUNC,RBUF,NBUF,CHR,LCHR,4)
      CASE (6)
         CALL NUDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      CASE (7)
         CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,1)
      CASE (8)
         CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,2)
      CASE (9)
         CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,3)
      CASE (10)
         CALL PSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,4)
      CASE (11)
         CALL GIDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,1)
      CASE (12)
         CALL GIDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,2)
      CASE (13)
         CALL LXDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      CASE DEFAULT
         WRITE (MSG,'(I10)') IDEV
         CALL GRQUIT('Unknown device code in GREXEC: '//MSG)
      END SELECT
      RETURN
      END

