/*******************************************************************************
* Memory management functions.
*
*	Project: Monte Carlo Simulation of Triple Axis Spectrometers
*	File name: memory.c
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: memory.c,v 1.7 1998-11-13 07:33:09 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.6  1998/10/02 08:39:02  kn
*	Fixed header comment.
*
*	Revision 1.5  1998/10/01 11:47:38  kn
*	Added str_dup_n().
*
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
* Copyright (C) Risoe National Laboratory, 1997-1998, All rights reserved
*******************************************************************************/

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <stdio.h>

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
* Allocate a new string holding the result of quoting the input string. The
* result is suitable for inclusion in C source code.
*******************************************************************************/
char *
str_quote(char *string)
{
  unsigned char *badchars = "\\\"\r\n\t";
  unsigned char *quotechars = "\\\"rnt";
  unsigned char *q, *res, *ptr;
  int len, pass;
  int c;
  unsigned char new[5];

  /* Loop over the string twice, first counting chars and afterwards copying
     them into an allocated buffer. */
  for(pass = 0; pass < 2; pass++)
  {
    unsigned char *p = (unsigned char *)string;

    if(pass == 0)
      len = 0;			/* Prepare to compute length */
    else
      q = res = mem(len + 1);	/* Allocate buffer */
    while((c = *p++))
    {
      ptr = strchr(badchars, c);
      if(ptr != NULL)
	sprintf(new, "\\%c", quotechars[ptr - badchars]);
      else if(isprint(c))
	sprintf(new, "%c", c);
      else
	sprintf(new, "\\%03o", c);
      if(pass == 0)
	len += strlen(new);	/* Count in length */
      else
	for(ptr = new; (*q = *ptr) != 0; ptr++)
	  q++;			/* Copy over chars */
    }
  }
  return res;
}


/*******************************************************************************
* Free memory for a string.
*******************************************************************************/
void
str_free(char *string)
{
  memfree(string);
}
