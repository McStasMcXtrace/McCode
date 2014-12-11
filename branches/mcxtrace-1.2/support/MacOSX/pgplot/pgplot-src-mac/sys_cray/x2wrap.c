#include <fortran.h>

/*
 * This is a wrapper function used to call the /xdisp driver from
 * CRAY FORTRAN.
 *--
 * 09-Nov-1994 - [mcs]
 */
void X2DRIV(int *ifunc, float *rbuf, int *nbuf, _fcd chr, int *lchr)
{
  x2driv_(ifunc, rbuf, nbuf, _fcdtocp(chr), lchr, mode, _fcdlen(chr));
}
