/*******************************************************************************
* Handle expressions used as arguments to components etc.
*
*	Project: Monte Carlo Simulation of Triple Axis Spectrometers
*	File name: cexp.h
*
*	Author: K.N.			Aug  7, 1997
*
*	$Id: cexp.c,v 1.4 1998-10-02 08:35:04 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.3  1998/10/01 11:44:07  kn
*	Added support for string expressions.
*
*	Revision 1.2  1997/09/07 17:54:28  kn
*	Snapshot with (untested) code generation complete.
*
*	Revision 1.1  1997/08/13 09:11:24  kn
*	Initial revision
*
*
* Copyright (C) Risoe National Laboratory, 1997-1998, All rights reserved
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
* identifier expressions are simply strings, and all normal identifiers refer
* to instrument parameters (which have ID_PRE and "ip" prepended).
*******************************************************************************/

CExp
exp_id(char *id)
{
  return str_cat(ID_PRE, "ip", id, NULL);
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

CExp
exp_string(char *s)
{
  return str_cat("\"", s, "\"", NULL);
}

char *
exp_tostring(CExp e)
{
  if(e == NULL)
  {
    e = "";
    debugn((DEBUG_HIGH, "exp_tostring(): NULL cexp received.\n"));
  }
  return str_dup(e);
}

void
exp_fprint(FILE *f, CExp e)
{
  fputs(e, f);
}
