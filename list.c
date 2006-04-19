/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: list.c
*
* %Identification
* Written by: K.N.
* Date: Jul  3, 1997
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision: 1.16 $
*
* Implementation of lists.
*
*	$Id: list.c,v 1.16 2006-04-19 13:06:25 farhi Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.15  2003/02/11 12:28:45  farhi
*	Variouxs bug fixes after tests in the lib directory
*	mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
*	read_table:corrected MC_SYS_DIR -> MCSTAS define
*	monitor_nd-lib: fix Log(signal) log(coord)
*	HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
*	Progress_bar: precent -> percent parameter
*	CS: ----------------------------------------------------------------------
*
*	Revision 1.4  2001/03/15 15:11:13  peo
*	Changed MAXELEMENTS to 5000
*
*	Revision 1.3  1998/10/02 08:37:44  kn
*	Fixed header comment.
*
*	Revision 1.2  1997/09/07 17:58:11  kn
*	Snapshot with (untested) code generation complete.
*
*	Revision 1.1  1997/08/13 09:15:16  kn
*	Initial revision
*
*******************************************************************************/

#include "mcstas.h"


/*******************************************************************************
* Implement lists of void pointers as an abstract data type, so that we can
* change to a better/different implementation at a later time.
*******************************************************************************/

/* The implementation of lists. Simple version: fixed-size array. */
struct List_header
  {
    int size;
    int maxsize;
    void **elements;
  };

#define MAX_ELEMENTS 5000

/* Position in a list for doing list traversals. */
struct List_position
  {
    struct List_header *list;		/* The list we are traversing. */
    int index;			/* Next element to return. */
  };


/*******************************************************************************
* Create a new list.
*******************************************************************************/
List
list_create(void)
{
  List l;

  palloc(l);
  l->maxsize = MAX_ELEMENTS;
  nalloc(l->elements, l->maxsize);
  l->size = 0;
  return l;
}


/*******************************************************************************
* Add an element to a list, adding at the end.
*******************************************************************************/
void
list_add(List l, void *e)
{
  int i;

  /* Check if there is room for the new element. */
  if(l->size >= l->maxsize)
    fatal_error("list_add: List too small.");

  i = l->size;
  l->size++;
  l->elements[i] = e;
}


/*******************************************************************************
* Delete a list and deallocate memory. Caller must supply a function that
* frees the list elements.
*******************************************************************************/

void
list_free(List l, void (*freer)(void *))
{
  int i;

  for(i = 0; i < l->size; i++)
  {
    (*freer)(l->elements[i]);
  }
  memfree(l->elements);
  memfree(l);
}


/*******************************************************************************
* Get the length of (number of elements in) a list.
*******************************************************************************/
int
list_len(List l)
{
  return l->size;
}


/*******************************************************************************
* Prepare to start traversing a list.
*******************************************************************************/
List_handle
list_iterate(List l)
{
  List_handle lh;

  palloc(lh);
  lh->list = l;
  lh->index = 0;
  return lh;
}


/*******************************************************************************
* Get the next element during a traversal of a list. Returns NULL when no
* more elements exist in the list.
*******************************************************************************/
void *
list_next(List_handle lh)
{
  /* Check if there are any more elements */
  if(lh->index >= lh->list->size)
  {
    return NULL;
  }
  else
  {
    int i = lh->index;
    lh->index++;
    return lh->list->elements[i];
  }
}


/*******************************************************************************
* End a list traversal, freeing the memory allocated to the handle.
*******************************************************************************/
void
list_iterate_end(List_handle lh)
{
  memfree(lh);
}
