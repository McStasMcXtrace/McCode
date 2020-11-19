/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2007, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: list.c
*
* %Identification
* Written by: K.N.
* Date: Jul  3, 1997
* Origin: Risoe
* Release: McStas X.Y.Z
* Version: $Revision$
*
* Implementation of lists.
*
*******************************************************************************/

#include "mccode.h"


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

#define MAX_ELEMENTS 500000

/* Position in a list for doing list traversals. */
struct List_position
  {
    struct List_header *list;           /* The list we are traversing. */
    int index;                  /* Next element to return. */
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
    fatal_error("list_add: List too small (%d).", MAX_ELEMENTS);

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
* Prepare to start traversing a list in reverse order
*******************************************************************************/
List_handle
list_iterate_back(List l)
{
  List_handle lh;

  palloc(lh);
  lh->list = l;
  lh->index = l->size-1;
  return lh;
}


/*******************************************************************************
* Get the next element during a traversal of a list. Returns NULL when no
* more elements exist in the list.
*******************************************************************************/
void *
list_next(List_handle lh)
{
  if (!lh) return(NULL);
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
* Get the previous element during a traversal of a list. Returns NULL when no
* more elements exist in the list.
*******************************************************************************/
void *
list_previous(List_handle lh)
{
  if (!lh) return(NULL);
  /* Check if there are any more elements */
  if(lh->index < 0)
  {
    return NULL;
  }
  else
  {
    int i = lh->index;
    lh->index--;
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

/*******************************************************************************
* Catenate list2 to list1
*******************************************************************************/
List list_cat(List l1, List l2)
{
  List_handle liter;
  void*       litem;
  liter = list_iterate(l2);
  while((litem = list_next(liter)))
    list_add(l1, litem);
  list_iterate_end(liter);
  return(l1);
}
