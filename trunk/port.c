/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: port.c
*
* %Identification
* Written by: K.N.
* Date: Nov 11, 1998
* Origin: Risoe
* Release: McStas 1.6
* Version: 1.1
*
* Code to handle portability issues (mainly Win32 stuff).
*
* $Id: port.c,v 1.9 2003-01-21 08:55:32 pkwi Exp $
*
*******************************************************************************/

#include "port.h"

#ifndef HAVE_STRCASECMP
#include <ctype.h>
int strcasecmp(char *s1, char *s2)
{
  int c, c1, c2;
  do {
    c1 = toupper(*s1++);
    c2 = toupper(*s2++);
    c = c1 - c2;
  } while(c1 != 0 && c2 != 0 && c == 0);
  return c;
}
#endif /* HAVE_STRCASECMP */

#ifndef HAVE_FDOPEN
FILE *fdopen(int descr, const char *mode)
{
  fatal_error("The '-' argument for standard input and output is not supported"
	      " on this system.");
}
#endif /* HAVE_FDOPEN */
