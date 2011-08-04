#include <fortran.h>

/*
 * This is a wrapper function used to call the /xtk driver from
 * CRAY FORTRAN.
 *--
 * 24-Mar-1997 - [mcs]
 */
void TKDRIV(int *ifunc, float *rbuf, int *nbuf, _fcd chr, int *lchr, int *mode)
{
  tkdriv_(ifunc, rbuf, nbuf, _fcdtocp(chr), lchr, mode, _fcdlen(chr));
}
