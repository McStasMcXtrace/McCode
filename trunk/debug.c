/*******************************************************************************
* Support for conditional output of debugging information.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: debug.c
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: debug.c,v 1.3 1998-09-24 12:15:30 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.2  1997/07/02 07:22:53  kn
*	Error reporting functions.
*
*	Revision 1.1  1997/07/01 08:17:57  kn
*	Initial revision
*
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/

#include <stdarg.h>
#include <stdio.h>

#include "mcstas.h"

/*******************************************************************************
* Error messages.
*******************************************************************************/
void
print_error(char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  vfprintf(stderr, format, ap);
  va_end(ap);
}


/* Print a warning message. May optionally take a pointer to a flag of type
 * int; this should be NULL or point to a variable that is initialized to
 * zero. It the flag is given, the warning will only be displayed the first
 * time this function is called.
 */
void
print_warn(int *flag, char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  if(flag == NULL || *flag == 0)
  {
    fprintf(stderr, "Warning: ");
    vfprintf(stderr, format, ap);
    *flag = 1;
  }
  va_end(ap);
}


/*******************************************************************************
* Fatal errors. These cause the program to terminate immediately. This is not
* very user friendly, so it should be avoided if possible. However, it is
* useful for such things as failed memory allocations of small sizes that are
* a pain to handle correctly and extremely unlikely to occur in modern
* virtual memory-capable systems. 
*
* Outputs a message passed in printf-style to stderr and exits.
*******************************************************************************/
void
fatal_error(char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  fprintf(stderr, "\n\nFatal error: ");
  vfprintf(stderr, format, ap);
  fprintf(stderr, "\n\nProgram aborted.\n");
  va_end(ap);

  exit(1);
}


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
