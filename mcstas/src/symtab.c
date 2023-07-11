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
* Version: $Revision$
*
* Symbol tables.
*
*******************************************************************************/

#include <string.h>

#include "mccode.h"


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

#define MAXSIZE 10000		/* Max. table size. */

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
  return &(st->entries[i]);
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
  while((sitem = symtab_next(siter)))
    symtab_add(st1, sitem->name, sitem->val);
  symtab_iterate_end(siter);
  return(st1);
}

