/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: file.c
*
* %Identification
* Written by: K.N.
* Date: Sep 25, 1998
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision: 1.19 $
*
* Code to handle files and command line arguments.
*
*	$Id: file.c,v 1.19 2007-03-12 14:57:18 farhi Exp $
*
*	$Log: file.c,v $
*	Revision 1.19  2007-03-12 14:57:18  farhi
*	Cosmetics
*
*	Revision 1.18  2006/04/19 13:06:25  farhi
*	* Updated Release, Version and Origin fields in headers
*	* Improved setversion to update all McStasx.y occurencies into current release
*	* Added 'string' type for DEFINITION parameters to be handled as this type so that auto-quoting occurs in mcgui
*	* Added possibility to save log of the session to a file (appended) in mcgui
*	* Made Scilab use either TCL_EvalStr or TK_EvalStr
*
*	Revision 1.17  2005/03/02 10:07:23  farhi
*	Now displays info/warning when using contributed components
*
*	Revision 1.16  2004/09/10 15:09:56  farhi
*	Use same macro symbols for mcstas kernel and run-time for code uniformity
*
*	Revision 1.15  2003/02/11 12:28:45  farhi
*	Variouxs bug fixes after tests in the lib directory
*	mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
*	read_table:corrected MC_SYS_DIR -> MCSTAS define
*	monitor_nd-lib: fix Log(signal) log(coord)
*	HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
*	Progress_bar: precent -> percent parameter
*	CS: ----------------------------------------------------------------------
*
*	Revision 1.3  2000/02/15 07:40:59  kn
*	Also search for components in a fixed list of system dir subdirectories.
*
*	Revision 1.2  1998/10/02 08:36:21  kn
*	Fixed header comment.
*
*	Revision 1.1  1998/10/01 08:13:41  kn
*	Initial revision
*
*******************************************************************************/

#include <stdlib.h>
#include <stdio.h>

#include "mcstas.h"

static List search_list = NULL;

/* MOD: E. Farhi, Oct 2nd, 2001: add obsolete dir. Aug 27th, 2002: added share+contrib */
static char *sys_subdir_table[] =
  { "samples", "monitors", "sources", "optics", "misc" , "obsolete", "contrib", "share", "examples" };

/* Attempt to open FILE in directory DIR (or current directory if DIR is
   NULL). */
static FILE *
try_open_file(char *dir, char *name)
{
  char *path =
    dir != NULL ? str_cat (dir, MC_PATHSEP_S, name, NULL) : str_dup(name);
  FILE *f = fopen(path, "r");
  str_free(path);
  return f;
}

/* This variable stores the full path for the definition of a component
   as found by the function try_open_component() (called from
   open_component_search()). */
char *component_pathname;

static FILE *
try_open_component(char *dir, char *name)
{
  static char *suffixes[] = {".comp", ".cmp", ".com"};
  int i;

  for(i = 0; i < sizeof(suffixes)/sizeof(*suffixes); i++)
  {
    char *path =
      dir != NULL ?
      str_cat (dir, MC_PATHSEP_S, name, suffixes[i], NULL) :
      str_cat(name, suffixes[i], NULL);
    FILE *f = fopen(path, "r");
    if(f != NULL)
    {
      component_pathname = path;
      return f;
    }
    else
      str_free(path);
  }
  return NULL;
}

char *
get_sys_dir(void)
{
  static char *sys_dir = NULL;

  if(sys_dir == NULL)
  {
    sys_dir = getenv("MCSTAS");
    if(sys_dir == NULL)
      sys_dir = MCSTAS;
    sys_dir = str_dup(sys_dir);
  }
  return sys_dir;
}

/* Generic file search function. */
static FILE *
generic_open_file_search(char *name, FILE *(*try_open)(char *, char *))
{
  List_handle liter;
  char *dir;
  FILE *f;
  int i;

  f = (*try_open)(NULL, name);
  if(f != NULL)
    return f;
  if(search_list != NULL)
  {
    liter = list_iterate(search_list);
    while(dir = list_next(liter))
    {
      f = (*try_open)(dir, name);
      if(f != NULL)
	return f;
    }
  }
  /* Now look in the system directory. */
  f = (*try_open)(get_sys_dir(), name);
  if(f != NULL)
    return f;
  /* Finally look in the fixed list of system subdirectories. */
  for(i = 0; i < sizeof(sys_subdir_table)/sizeof(char *); i++) {
    dir = str_cat(get_sys_dir(), MC_PATHSEP_S, sys_subdir_table[i], NULL);
    f = (*try_open)(dir, name);
    str_free(dir);
    if(f != NULL)
      {
        if (!strcmp(sys_subdir_table[i], "obsolete")) fprintf(stderr, "Warning: '%s' is an obsolete component (not maintained).\n", name);
        if (!strcmp(sys_subdir_table[i], "contrib"))  fprintf(stderr, "Info:    '%s' is a contributed component.\n", name);
        return f;
      }
  }
  /* Not found. */
  return NULL;
}

/* Open file, searching the full search path. */
FILE *
open_file_search(char *name)
{
  return generic_open_file_search(name, try_open_file);
}

/* Open component definition, searching the full search path. */
FILE *
open_component_search(char *name)
{
  return generic_open_file_search(name, try_open_component);
}

/* Open file, searching only the system directory. */
FILE *
open_file_search_sys(char *name)
{
  return try_open_file(get_sys_dir(), name);
}


/* Add a directory to the search path. */
void
add_search_dir(char *name)
{
  if(search_list == NULL)
    search_list = list_create();
  list_add(search_list, name);
}

