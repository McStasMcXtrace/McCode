void xwdriv_();

/*
 * This is a wrapper function used to call the /xrv driver from
 * Absoft FORTRAN (NeXT).
 *--
 * 24-Mar-1997 - [mcs]
 */
void RVDRIV(ifunc,   rbuf,   nbuf,   chr,   lchr,   mode,
	  w_ifunc, w_rbuf, w_nbuf, w_chr, w_lchr, w_mode)
 int *ifunc;
 float *rbuf;
 int *nbuf;
 char *chr;
 int *lchr;
 int w_ifunc, w_rbuf, w_nbuf, w_chr, w_lchr, w_mode;  /* Argument widths */
{
  rvdriv_(ifunc, rbuf, nbuf, chr, lchr, mode, w_chr);
  return;
}
