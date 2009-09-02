/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: cexp.c
*
* %Identification
* Written by: K.N.
* Date: Aug  7, 1997
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision: 1.19 $
*
* Handle expressions used as arguments to components etc.
*
*	$Id: cexp.c,v 1.19 2006-04-19 13:06:25 farhi Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.18  2003/02/11 12:28:45  farhi
*	Variouxs bug fixes after tests in the lib directory
*	mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
*	read_table:corrected MC_SYS_DIR -> MCSTAS define
*	monitor_nd-lib: fix Log(signal) log(coord)
*	HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
*	Progress_bar: precent -> percent parameter
*	CS: ----------------------------------------------------------------------
*
*	Revision 1.6  2000/07/27 09:04:59  kn
*	Support full C expressions. Now stores source line numbers within
*	expression representation, and distinguishes between values and compound
*	expressions.
*
*	Revision 1.5  2000/07/05 13:32:10  kn
*	Properly quote constant string expressions.
*
*	Revision 1.4  1998/10/02 08:35:04  kn
*	Fixed header comment.
*
*	Revision 1.3  1998/10/01 11:44:07  kn
*	Added support for string expressions.
*
*	Revision 1.2  1997/09/07 17:54:28  kn
*	Snapshot with (untested) code generation complete.
*
*	Revision 1.1  1997/08/13 09:11:24  kn
*	Initial revision
*
*******************************************************************************/
#include <stdarg.h>
#include <stdio.h>

#include "mcstas.h"

/* The internal structure implementing a C expression. */
struct cexp
  {
    char *s;		    /* String representation */
    int isvalue;	    /* True if identifier or string/number constant */
    int lineno;		    /* Starting line number, or zero */
  };

/* Create an expression from a string representing a value (either identifier,
   constant number, or constant string). */
static CExp
mkvalueexp(char *s)
{
  CExp e;
  palloc(e);
  e->s = s;
  e->isvalue = 1;
  e->lineno = 0;		/* Initially no line number set */
  return e;
}

/* Create an expression from a string not representing a value. */
static CExp
mknonvalueexp(char *s)
{
  CExp e;
  palloc(e);
  e->s = s;
  e->isvalue = 0;
  e->lineno = 0;		/* Initially no line number set */
  return e;
}

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
  return mkvalueexp(str_cat(ID_PRE, "ip", id, NULL));
}

CExp
exp_extern_id(char *id)
{
  return mkvalueexp(str_dup(id));
}

CExp
exp_number(char *n)
{
  return mkvalueexp(str_dup(n));
}

CExp
exp_string(char *s)
{
  char *quoted, *result;
  quoted = str_quote(s);
  result =  str_cat("\"", s, "\"", NULL);
  str_free(quoted);
  return mkvalueexp(result);
}

CExp
exp_ctoken(char *s)
{
  return mknonvalueexp(str_dup(s));
}

CExp
exp_compound(int n, ...)
{
  char *result, *new;
  CExp e;
  va_list ap;
  char *separator = "";		/* Token separator, initially empty */

  va_start(ap, n);
  result = str_dup("");
  while(n-- > 0)
  {
    e = va_arg(ap, CExp);
    new = str_cat(result, separator, e->s, NULL);
    str_free(result);
    result = new;
    separator = " ";		/* Now use space separator for rest. */
  }
  return mknonvalueexp(result);
}

void
exp_free(CExp e)
{
  str_free(e->s);
  memfree(e);
}

char *
exp_tostring(CExp e)
{
  char *s = e->s;
  if(s == NULL)
  {
    s = "";
    debugn((DEBUG_HIGH, "exp_tostring(): NULL cexp received.\n"));
  }
  return str_dup(s);
}

void
exp_fprint(FILE *f, CExp e)
{
  fputs(e->s, f);
}

int
exp_isvalue(CExp e)
{
  return e->isvalue;
}

void
exp_setlineno(CExp e, int n)
{
  e->lineno = n;
}

int
exp_getlineno(CExp e)
{
  return e->lineno;
}
