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
* Version: $Revision$
*
* Code to handle files and command line arguments.
*
*******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <dirent.h>
#include <string.h>

#include "mccode.h"
static List search_list = NULL;
/* MOD: E. Farhi, Oct 2nd, 2001: add obsolete dir. Aug 27th, 2002: added share+contrib */
/* MOD: P. Willendrup, Oct 11 2016: Add contrib subdir "union" */
/* MOD: P. Willendrup, May 22 2023: Add contrib subdir "sasmodels" */
static char *sys_subdir_table[] =
  { "samples", "monitors", "sources", "optics", "misc" , "obsolete", "contrib", "union", "sasmodels", "astrox", "share", "examples" };

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
  /* component not found: we make a new case insensitive search */
  struct dirent *dp;
  DIR *dfd;
  if (!dir || (dfd = opendir(dir)) == NULL) {
    return NULL;
  }
  while ((dp = readdir(dfd)) != NULL) {
    if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0)
      continue;    /* skip self and parent */
    for(i = 0; i < sizeof(suffixes)/sizeof(*suffixes); i++) {
      char *lname = str_cat(name, suffixes[i], NULL);
      if (!strcasecmp(dp->d_name, lname))
        fprintf(stderr, "Info:    '%s' component used in instrument matches %s\n"
                        "         from library but may be misspelled. Check instrument.\n", name, dp->d_name);
      str_free(lname);
    }
  }
  return NULL;
}

char *
get_sys_dir(void)
{
  static char *sys_dir = NULL;

  if(sys_dir == NULL)
  {
    sys_dir = getenv(FLAVOR_UPPER);
    if(sys_dir == NULL)
      sys_dir = MCXTRACE;
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
