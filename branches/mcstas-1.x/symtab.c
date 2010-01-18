/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: symtab.c
*
* %Identification
* Written by: K.N.
* Date: Jul  1, 1997
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision: 1.18 $
*
* Symbol tables.
*
*	$Id: symtab.c,v 1.18 2006-11-06 14:30:00 farhi Exp $
*
*	$Log: symtab.c,v $
*	Revision 1.18  2006-11-06 14:30:00  farhi
*	Improved COPY grammar, enabling to either redefine sections, or extend them (with e.g. INITIALIZE COPY parent EXTEND %{ %})
*	over-definition of parameters for comp instances in the .instr works OK.
*
*	Revision 1.17  2006/04/19 13:06:26  farhi
*	* Updated Release, Version and Origin fields in headers
*	* Improved setversion to update all McStasx.y occurencies into current release
*	* Added 'string' type for DEFINITION parameters to be handled as this type so that auto-quoting occurs in mcgui
*	* Added possibility to save log of the session to a file (appended) in mcgui
*	* Made Scilab use either TCL_EvalStr or TK_EvalStr
*	
*	Revision 1.16  2003/10/06 15:00:12  farhi
*	Added symtab_previous function for PREVIOUS keyword
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
*	Revision 1.4  1999/04/16 07:41:31  kn
*	Make the value_free argument for the symtab_free function optional.
*
*	Revision 1.3  1998/10/02 08:39:25  kn
*	Fixed header comment.
*
*	Revision 1.2  1997/09/07 17:58:45  kn
*	Snapshot with (untested) code generation complete.
*
*	Revision 1.1  1997/08/13 09:16:14  kn
*	Initial revision
*
*******************************************************************************/

#include <string.h>

#include "mcstas.h"


/*******************************************************************************
* A symbol table is an abstract data type that maps any name to a
* corresponding symbol table entry (which can be anything).
*
* In this first version, we use a simple, but inefficient approach. Symbol
* tables are implemented as fixed-size arrays of string and void pointer
* pairs. Table lookups are performed with a linear search. But the interface
* is designed to make a transition to a more efficient implementation easily
* possible if and when it becomes necessary.
*******************************************************************************/

struct Symbol_table
  {
    int size;			/* Number of entries currently in table. */
    int maxsize;		/* Total size of table. */
    struct Symtab_entry *entries; /* Array of pairs of names and values. */
  };

#define MAXSIZE 1000		/* Max. table size. */

/* Position in a symbol table for doing traversals. */
struct Symtab_position
  {
    struct Symbol_table *symtab; /* The symbol table we are traversing. */
    int index;			 /* Next entry to return. */
  };


/*******************************************************************************
* Allocate and initialize a new symbol table.
*******************************************************************************/
Symtab
symtab_create(void)
{
  Symtab st;

  palloc(st);			/* Allocate new symbol table. */
  st->maxsize = MAXSIZE;
  nalloc(st->entries, st->maxsize); /* Allocate array for entries. */
  st->size = 0;			/* Empty table. */
  return st;
}


/*******************************************************************************
* Look up a name in a symbol table. Returns a void pointer to the table
* entry, or NULL if not found.
*******************************************************************************/
struct Symtab_entry *
symtab_lookup(Symtab st, char *name)
{
  int i;

  for(i = 0; i < st->size; i++)
  {
    if(!strcmp(name, st->entries[i].name)) /* Found? */
    {
      return &(st->entries[i]);
    }
  }

  /* Not found. */
  return NULL;
}


/*******************************************************************************
* Add a new name to an existing symbol table along with a table entry. The
* table entry is a void pointer which could eg. point to a structure
* containing information relevant to the name.
*
* The table entry must point to memory that remains valid for the duration of
* the symbol table. The name, however, may be re-used freely as a new copy is
* allocated in the symbol table.
*******************************************************************************/
struct Symtab_entry *
symtab_add(Symtab st, char *name, void *value)
{
  int i;

  /* First see if an entry for this name already exists (it shouldn't, but ...) */
  for(i = 0; i < st->size; i++)
  {
    if(!strcmp(name, st->entries[i].name))
    {
      /* Hmm ... adding an already present name. */
      debugn((DEBUG_MEDIUM, "add_to_symtab: name already exists: %s.\n", name));
      return &(st->entries[i]);
    }
  }

  /* Make sure the table is large enough. */
  if(st->size >= st->maxsize)
  {
    /* No room in table. This causes the program to abort. */
    fatal_error("symtab_add: symbol table too small.");
  }

  /* Add the name at the end of the table. */
  i = st->size;
  st->size++;
  st->entries[i].name = str_dup(name);
  st->entries[i].val = value;
  return value;
}


/*******************************************************************************
* Free up memory allocated to a symbol table. The caller can supply a
* function value_free that will free the memory for each table entry.
* Pass NULL for value_free if no freeing is necessary.
*******************************************************************************/
void
symtab_free(Symtab st, void (*value_free)(void *))
{
  int i;

  for(i = 0; i < st->size; i++)
  {
    str_free(st->entries[i].name);
    if(value_free)
      (*value_free)(st->entries[i].val);
  }
  memfree(st->entries);
  memfree(st);
}


/*******************************************************************************
* Prepare to start traversing a symbol table. Note that an improved
* implementation is free to change the order in which a traversal returns the
* elements (for example if using a hash table).
*******************************************************************************/
Symtab_handle
symtab_iterate(Symtab s)
{
  Symtab_handle sh;

  palloc(sh);
  sh->symtab = s;
  sh->index = 0;
  return sh;
}


/*******************************************************************************
* Get the next element during a traversal of a symbol table. Returns NULL
* when no more elements exist.
*******************************************************************************/
struct Symtab_entry *
symtab_next(Symtab_handle sh)
{
  int i = sh->index;

  /* Check if there are any more entries. */
  if(i >= sh->symtab->size)
  {
    return NULL;
  }
  else
  {
    sh->index++;
    return &(sh->symtab->entries[i]);
  }
}

/*******************************************************************************
* Get the index-th previous element stored in the symbol table. Returns NULL
* when error occurs.
*******************************************************************************/
struct Symtab_entry *
symtab_previous(Symtab st, int index)
{
  if (index <= 0 || index > st->size) {
    return NULL;
  } else {
    return &(st->entries[st->size - index]);
  }
}


/*******************************************************************************
* End a symbol table traversal, freeing the memory allocated to the handle.
*******************************************************************************/
void
symtab_iterate_end(Symtab_handle sh)
{
  memfree(sh);
}

/*******************************************************************************
* Catenate symtab2 to symtab1
*******************************************************************************/
Symtab symtab_cat(Symtab st1, Symtab st2)
{
  Symtab_handle siter;
  struct Symtab_entry *sitem;
  siter = symtab_iterate(st2);
  while(sitem = symtab_next(siter))
    symtab_add(st1, sitem->name, sitem->val);
  symtab_iterate_end(siter);
  return(st1);
}

