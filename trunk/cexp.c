/*******************************************************************************
* Handle expressions used as arguments to components etc.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: cexp.h
*
*	Author: K.N.			Aug  7, 1997
*
*	$Id: cexp.c,v 1.1 1997-08-13 09:11:24 kn Exp $
*
*	$Log: not supported by cvs2svn $
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/
#include <stdio.h>

#include "mcstas.h"

/*******************************************************************************
* Handle identifiers used as arguments to components.
* There are two types of identifiers: normal and extern. Normal identifiers
* refer to formal arguments of the instrument, and are always of type
* double. These can be varied eg. to perform a scan. Extern identifiers refer
* to user-supplied C identifiers that is typically put in the declaration and
* initialization sections of the instrument definition.
*
* The final version will distinguish (using a union) between these two types,
* and will maintain a mapping from formal parameters of the instrument to
* generated names in the generated simulation (the extern names must be
* copied unchanged). But in this first version a simpler scheme is used: all
* identifier expressions are simply strings, and all normal identifiers have
* the name supplied by the user with "mc" prepended.
*******************************************************************************/

CExp
exp_id(char *id)
{
  return str_cat("mc", id, NULL);
}

CExp
exp_extern_id(char *id)
{
  return str_dup(id);
}

CExp
exp_number(double n)
{
  char buf[100];
  sprintf(buf, "%g", n);
  return str_dup(buf);
}

char *
exp_tostring(CExp e)
{
  return str_dup(e);
}

void
exp_fprint(FILE *f, CExp e)
{
  fputs(e, f);
}
