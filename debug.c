/*******************************************************************************
* Support for conditional output of debugging information.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: debug.c
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: debug.c,v 1.1 1997-07-01 08:17:57 kn Exp $
*
*	$Log: not supported by cvs2svn $
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/

#include <stdarg.h>
#include <stdio.h>

#ifdef DEBUG

int debug_current_level = DEBUG;

/*******************************************************************************
* Output debug information, printf-style. Only included when DEBUG is
* defined.
*******************************************************************************/
void
debug_printf(char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  vfprintf(stderr, format, ap);
  va_end(ap);
}

/*******************************************************************************
* Like 'debug_printf', but only produce output if the current debugging level
* is greater than or equal to the first argument.
*******************************************************************************/
void
debugn_printf(int level, char *format, ...)
{
  va_list ap;

  if(level <= debug_current_level)
  {
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);
  }
}

#endif /* DEBUG */
