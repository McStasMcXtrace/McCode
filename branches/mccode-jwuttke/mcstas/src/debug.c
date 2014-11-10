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
* Version: $Revision$
*
* Support for conditional output of debugging information.
*
*******************************************************************************/

#include <stdarg.h>
#include <stdio.h>

#include "mccode.h"

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
  error_encountered++;
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
