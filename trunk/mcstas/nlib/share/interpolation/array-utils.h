/*******************************************************************************
* array-utils.h
*
* Written by Ross Hemsley for McStas. (September 2009)
*
* These are general purpose routines to be used anywhere they are needed.
* For specifics on use, and more implementation details, see utils.c
*******************************************************************************/
#ifndef array_utils_h
#define array_utils_h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/******************************************************************************/

/* This structure is how we represent a stack. (Very similar to array list). */
typedef struct
{
  int top;
  int slots;
  void** arr;
} arrayStack;


/* This is how we store an array list. */
typedef struct
{
   int   num_slots;
   int   num_elements;
   void** arr;
} arrayList;

/* These structs are needed to store a (doubly) linked list. */
typedef struct _listNode
{
  void *data;
  struct _listNode *next;
  struct _listNode *prev;
} listNode;

typedef struct
{
  listNode *head;
  listNode *last;
  int nelem;
  arrayStack   *deadNodes;
} linkedList;

/*******************************************************************************
* Array list functions.
*******************************************************************************/
int         vsetArrayListAt(arrayList *l, void * element, int index);
//------------------------------------------------------------------------------
int         addToArrayList(arrayList *l, void* element);
//------------------------------------------------------------------------------
void*       getFromArrayList (arrayList *l, int index);
//------------------------------------------------------------------------------
void**      getArrayFromArrayList(arrayList *l);
//------------------------------------------------------------------------------
int         arrayListGetIndex(arrayList *l, void *e);
//------------------------------------------------------------------------------
int         arrayListSize(arrayList *l);
//------------------------------------------------------------------------------
arrayList*  newArrayList();
//------------------------------------------------------------------------------
void        freeArrayList(arrayList *l, void (*destructor)(void *e));
//------------------------------------------------------------------------------
int         arrayListContains(arrayList * l , void * element);
//------------------------------------------------------------------------------
void        freeElements(arrayList *l);
//------------------------------------------------------------------------------
void        emptyArrayList(arrayList *l);
/*******************************************************************************
* Doubly linked list functions.
*******************************************************************************/
linkedList* newLinkedList();
//------------------------------------------------------------------------------
listNode*   addToLinkedList(linkedList *l, void *e);
//------------------------------------------------------------------------------
void*       getFromLinkedList(linkedList *l, int i);
//------------------------------------------------------------------------------
int         linkedListSize(linkedList *l);
//------------------------------------------------------------------------------
void*       nextElement(linkedList *l, listNode **last);
//------------------------------------------------------------------------------
listNode*   topOfLinkedList(linkedList *l);
//------------------------------------------------------------------------------
void        testLinkedList();
//------------------------------------------------------------------------------
void        removeFromLinkedList(linkedList *l, listNode *ln);
//------------------------------------------------------------------------------
void        freeLinkedList(linkedList *l, void (*destructor)(void *e));
//------------------------------------------------------------------------------
int         linkedListContains(linkedList *l, void *e);
/*******************************************************************************
* Stack functions.
*******************************************************************************/
arrayStack* newStack();
//------------------------------------------------------------------------------
void        push(arrayStack *s, void*e);
//------------------------------------------------------------------------------
void*       pop(arrayStack *s);
//------------------------------------------------------------------------------
int         stackSize(arrayStack *s);
//------------------------------------------------------------------------------
void        testStack();
//------------------------------------------------------------------------------
void        freeStack(arrayStack *s, void (*destructor)(void *e));
//------------------------------------------------------------------------------
int         isEmpty(arrayStack *s);
//------------------------------------------------------------------------------
void        emptyStack(arrayStack *s);
/******************************************************************************/

#endif
