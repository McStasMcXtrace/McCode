void x2driv_();

/*
 * This is a wrapper function used to call the /xdisp driver from
 * Absoft FORTRAN (NeXT).
 *--
 * 13-Nov-1994 - [mcs]
 */
void X2DRIV(ifunc,   rbuf,   nbuf,   chr,   lchr,   mode,
	  w_ifunc, w_rbuf, w_nbuf, w_chr, w_lchr, w_mode)
 int *ifunc;
 float *rbuf;
 int *nbuf;
 char *chr;
 int *lchr;
 int w_ifunc, w_rbuf, w_nbuf, w_chr, w_lchr, w_mode;  /* Argument widths */
{
  x2driv_(ifunc, rbuf, nbuf, chr, lchr, mode, w_chr);
  return;
}
