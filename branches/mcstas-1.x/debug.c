/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: debug.c
*
* %Identification
* Written by: K.N.
* Date: Jul  1, 1997
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision: 1.19 $
*
* Support for conditional output of debugging information.
*
*	$Id: debug.c,v 1.19 2006-04-19 13:06:25 farhi Exp $
*
*	$Log: debug.c,v $
*	Revision 1.19  2006-04-19 13:06:25  farhi
*	* Updated Release, Version and Origin fields in headers
*	* Improved setversion to update all McStasx.y occurencies into current release
*	* Added 'string' type for DEFINITION parameters to be handled as this type so that auto-quoting occurs in mcgui
*	* Added possibility to save log of the session to a file (appended) in mcgui
*	* Made Scilab use either TCL_EvalStr or TK_EvalStr
*
*	Revision 1.18  2003/02/11 12:28:45  farhi
*	Variouxs bug fixes after tests in the lib directory
*	mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
*	read_table:corrected MC_SYS_DIR -> MCSTAS define
*	monitor_nd-lib: fix Log(signal) log(coord)
*	HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
*	Progress_bar: precent -> percent parameter
*	CS: ----------------------------------------------------------------------
*
*	Revision 1.6  1999/03/18 07:29:10  kn
*	Fix bug in print_warn().
*
*	Revision 1.5  1998/10/02 08:36:12  kn
*	Fixed header comment.
*
*	Revision 1.4  1998/10/01 11:44:33  kn
*	Set a flag when print_error is called.
*
*	Revision 1.3  1998/09/24 12:15:30  kn
*	Added print_warn() function.
*
*	Revision 1.2  1997/07/02 07:22:53  kn
*	Error reporting functions.
*
*	Revision 1.1  1997/07/01 08:17:57  kn
*	Initial revision
*
*******************************************************************************/

#include <stdarg.h>
#include <stdio.h>

#include "mcstas.h"

/*******************************************************************************
* Error messages.
*******************************************************************************/
int error_encountered = 0;

void
print_error(char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  vfprintf(stderr, format, ap);
  va_end(ap);
  error_encountered = 1;
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
    if(flag)
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
