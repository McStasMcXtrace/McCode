/*******************************************************************************
* Memory management functions.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: memory.c
*
*	Author: K.N.			Jul  1, 1997
*
*	$Id: memory.c,v 1.1 1997-07-01 08:24:20 kn Exp $
*
*	$Log: not supported by cvs2svn $
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/

#include <stdlib.h>
#include <stdio.h>

#include "mcstas.h"


/*******************************************************************************
* Allocate memory. This function never returns NULL; instead, the
* program is aborted if insufficient memory is available.
*******************************************************************************/
void *
mem(size_t size)
{
  void *p = malloc(size);
  if(p == NULL)
  {
    fprintf(stderr, "\nFatal error: memory exhausted, or invalid allocation.\n");
    exit(1);
  }
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
