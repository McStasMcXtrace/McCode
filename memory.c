/*******************************************************************************
* Memory management functions.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: memory.c
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: memory.c,v 1.5 1998-10-01 11:47:38 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.4  1997/09/07 17:58:29  kn
*	Snapshot with (untested) code generation complete.
*
*	Revision 1.3  1997/08/13 09:15:48  kn
*	First version to properly parse instrument definition files.
*
*	Revision 1.2  1997/07/02 07:28:56  kn
*	String functions.
*
*	Revision 1.1  1997/07/01 08:24:20  kn
*	Initial revision
*
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "mcstas.h"


/*******************************************************************************
* Allocate memory. This function never returns NULL; instead, the
* program is aborted if insufficient memory is available.
*******************************************************************************/
void *
mem(size_t size)
{
  void *p = calloc(1, size);	/* Allocate and clear memory. */
  if(p == NULL)
    fatal_error("memory exhausted during allocation of size %d.", size);
  return p;
}

/*******************************************************************************
* Free memory allocated with mem().
*******************************************************************************/
void memfree(void *p)
{
  if(p == NULL)
    debug(("memfree(): freeing NULL memory.\n"));
  else
    free(p);
}

/*******************************************************************************
* Allocate a new copy of a string.
*******************************************************************************/
char *
str_dup(char *string)
{
  char *s;

  s = mem(strlen(string) + 1);
  strcpy(s, string);
  return s;
}


/*******************************************************************************
* Allocate a new copy of initial N chars in a string.
*******************************************************************************/
char *
str_dup_n(char *string, int n)
{
  char *s;

  s = mem(n + 1);
  strncpy(s, string, n);
  s[n] = '\0';
  return s;
}


/*******************************************************************************
* Allocate a new string to hold the concatenation of given strings. Arguments
* are the strings to concatenate, terminated by NULL.
*******************************************************************************/
char *
str_cat(char *first, ...)
{
  char *s;
  va_list ap;
  int size;
  char *arg;
  
  size = 1;			/* Count final '\0'. */
  va_start(ap, first);
  for(arg = first; arg != NULL; arg = va_arg(ap, char *))
    size += strlen(arg);	/* Calculate string size. */
  va_end(ap);
  s = mem(size);
  size = 0;
  va_start(ap, first);
  for(arg = first; arg != NULL; arg = va_arg(ap, char *))
  {
    strcpy(&(s[size]), arg);
    size += strlen(arg);
  }
  va_end(ap);
  return s;
}

/*******************************************************************************
* Free memory for a string.
*******************************************************************************/
void
str_free(char *string)
{
  memfree(string);
}
