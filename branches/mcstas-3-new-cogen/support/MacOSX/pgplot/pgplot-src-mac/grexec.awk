#!/bin/awk -f

BEGIN {
  printf("C*GREXEC -- PGPLOT device handler dispatch routine\n");
  printf("C+\n");
  printf("      SUBROUTINE GREXEC(IDEV,IFUNC,RBUF,NBUF,CHR,LCHR)\n");
  printf("      INTEGER IDEV, IFUNC, NBUF, LCHR\n");
  printf("      REAL    RBUF(*)\n");
  printf("      CHARACTER*(*) CHR\n");
  printf("C---\n");
  printf("      INTEGER NDEV\n");
}

/^[^!]/ {
  drivpos = index($1,"DRIV");
  if(drivpos > 0) {
    ndev++;
    driver[ndev] = substr($1,1,drivpos-1);
    mode[ndev] = $2;
  }
}

END {
  printf("      PARAMETER (NDEV=%d)\n", ndev);
  printf("      CHARACTER*10 MSG\n");
  printf("C---\n");
  printf("      GOTO(");
  for(i=1; i<=ndev; i++) {
    if(i%15 == 0)
      printf("\n     +     ");
    printf("%d", i);
    if(i<ndev)
      printf(",");
  };
  printf(") IDEV\n");
  printf("      IF (IDEV.EQ.0) THEN\n");
  printf("          RBUF(1) = NDEV\n");
  printf("          NBUF = 1\n");
  printf("      ELSE\n");
  printf("          WRITE (MSG,'(I10)') IDEV\n");
  printf("          CALL GRWARN('Unknown device code in GREXEC: '//MSG)\n");
  printf("      END IF\n");
  printf("      RETURN\n");
  printf("C---\n");
  for(i=1; i<=ndev; i++) {
    printf("%-5d CALL %sDRIV(IFUNC,RBUF,NBUF,CHR,LCHR", i, driver[i]);
    if(mode[i] != 0)
      printf(",%d)\n", mode[i]);
    else
      printf(")\n");
    printf("      RETURN\n");
  };
  printf("C\n");
  printf("      END\n");
}
