#include <fortran.h>

/*
 * This is a wrapper function used to call the /xmotif driver from
 * CRAY FORTRAN.
 *--
 * 24-Mar-1997 - [mcs]
 */
void XMDRIV(int *ifunc, float *rbuf, int *nbuf, _fcd chr, int *lchr, int *mode)
{
  xmdriv_(ifunc, rbuf, nbuf, _fcdtocp(chr), lchr, mode, _fcdlen(chr));
}
