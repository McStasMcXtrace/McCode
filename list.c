/*******************************************************************************
* Implementation of lists.
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: list.c
*
*	Author: K.N.			Jul  3, 1997
*
*	$Id: list.c,v 1.1 1997-08-13 09:15:16 kn Exp $
*
*	$Log: not supported by cvs2svn $
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
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

#define MAX_ELEMENTS 1000

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
