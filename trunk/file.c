/*******************************************************************************
* Code to handle files and command line arguments.
*
*	Project: Monte Carlo Simulation of Triple Axis Spectrometers
*	File name: file.c
*
*	Author: K.N.			Sep 25, 1998
*
*	$Id: file.c,v 1.3 2000-02-15 07:40:59 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.2  1998/10/02 08:36:21  kn
*	Fixed header comment.
*
*	Revision 1.1  1998/10/01 08:13:41  kn
*	Initial revision
*
*
* Copyright (C) Risoe National Laboratory, 1997-1998, All rights reserved
*******************************************************************************/

#include <stdlib.h>
#include <stdio.h>

#include "mcstas.h"

static List search_list = NULL;

static char *sys_subdir_table[] =
  { "samples", "monitors", "sources", "optics", "misc" };

/* Attempt to open FILE in directory DIR (or current directory if DIR is
   NULL). */
static FILE *
try_open_file(char *dir, char *name)
{
  char *path =
    dir != NULL ? str_cat (dir, PATHSEP_S, name, NULL) : str_dup(name);
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
      str_cat (dir, PATHSEP_S, name, suffixes[i], NULL) :
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

static char *
get_sys_dir(void)
{
  static char *sys_dir = NULL;

  if(sys_dir == NULL)
  {
    sys_dir = getenv("MCSTAS");
    if(sys_dir == NULL)
      sys_dir = MC_SYS_DIR;
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
    dir = str_cat(get_sys_dir(), PATHSEP_S, sys_subdir_table[i], NULL);
    f = (*try_open)(dir, name);
    str_free(dir);
    if(f != NULL)
      return f;
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

