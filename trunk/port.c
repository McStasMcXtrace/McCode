/*******************************************************************************
* Code to handle portability issues (mainly Win32 stuff).
*
*	Project: Monte Carlo Simulation of Triple Axis Spectrometers
*	File name: port.c
*
*	Author: K.N.			Nov 11, 1998
*
* Copyright (C) Risoe National Laboratory, 1998, All rights reserved
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
