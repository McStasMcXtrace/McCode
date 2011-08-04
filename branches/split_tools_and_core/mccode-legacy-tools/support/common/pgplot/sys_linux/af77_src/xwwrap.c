void xwdriv_();

/*
 * This is a wrapper function used to call the /xwindow driver from
 * Absoft FORTRAN (NeXT).
 *--
 * 13-Nov-1994 - [mcs]
 */
void XWDRIV(ifunc,   rbuf,   nbuf,   chr,   lchr,   mode,
	  w_ifunc, w_rbuf, w_nbuf, w_chr, w_lchr, w_mode)
 int *ifunc;
 float *rbuf;
 int *nbuf;
 char *chr;
 int *lchr;
 int w_ifunc, w_rbuf, w_nbuf, w_chr, w_lchr, w_mode;  /* Argument widths */
{
  xwdriv_(ifunc, rbuf, nbuf, chr, lchr, mode, w_chr);
  return;
}
