#include <fortran.h>

/*
 * This is a wrapper function used to call the /xwindow driver from
 * CRAY FORTRAN.
 *--
 * 09-Nov-1994 - [mcs]
 */
void XWDRIV(int *ifunc, float *rbuf, int *nbuf, _fcd chr, int *lchr, int *mode)
{
  xwdriv_(ifunc, rbuf, nbuf, _fcdtocp(chr), lchr, mode, _fcdlen(chr));
}
