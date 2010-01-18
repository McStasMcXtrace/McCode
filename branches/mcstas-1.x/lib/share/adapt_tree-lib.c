/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/adapt_tree-lib.c
*
* %Identification
* Written by: KN, EF
* Date:   Sep 02, 2002
* Origin: Risoe/ILL
* Release: McStas 1.6
* Version: $Revision: 1.8 $
*
* This file is to be imported by components handling adaptive trees, like
* Source_adapt and Adapt_check (in lib/sources)
* Embedded within instrument in runtime mode.
*
* Usage: within SHARE
* %include "adapt_tree-lib"
*
* $Id: adapt_tree-lib.c,v 1.8 2007-03-12 14:57:21 farhi Exp $
*
* $Log: adapt_tree-lib.c,v $
* Revision 1.8  2007-03-12 14:57:21  farhi
* Cosmetics
*
* Revision 1.7  2005/07/25 14:55:08  farhi
* DOC update:
* checked all parameter [unit] + text to be OK
* set all versions to CVS Revision
*
* Revision 1.6  2003/02/11 12:28:46  farhi
* Variouxs bug fixes after tests in the lib directory
* mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
* read_table:corrected MC_SYS_DIR -> MCSTAS define
* monitor_nd-lib: fix Log(signal) log(coord)
* HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
* Progress_bar: precent -> percent parameter
* CS: ----------------------------------------------------------------------
*
* Revision 1.1 2002/09/02 18:59:05 ef
* Initial revision extracted from mcstas-r.c/h
*******************************************************************************/

#ifndef ADAPT_TREE_LIB_H
#error McStas : please import this library with %include "adapt_tree-lib"
#endif

/*******************************************************************************
* Find i in adaptive search tree t s.t. v(i) <= v < v(i+1).
*******************************************************************************/
int
adapt_tree_search(struct adapt_tree *t, adapt_t v)
{
  adapt_t F = 0;    /* Current value. */
  int i = 0;      /* Current candidate. */
  int step = t->initstep;
  adapt_t *s = t->s;
  int j;
  for(j = t->root; step > 0; step >>= 1)
  {
    F += s[j];      /* Cumulative value in current node */
    if(v < F)
      j -= step;    /* Value is to the left or above. */
    else
      i = j, j += step;   /* Value is current or to the right. */
  }
  /* Now j is at the bottom of a tree (a leaf node). */
  if(v < F + s[j])
    return i;
  else
    return j;
}

/*******************************************************************************
* Add v to v[i], updating the cumulative sums appropriately.
*******************************************************************************/
void
adapt_tree_add(struct adapt_tree *t, int i, adapt_t v)
{
  int j = t->root;
  int step = t->initstep;
  adapt_t *s = t->s;
  t->total += v;
  t->v[i++] += v;
  for(;;)
  {
    while(j < i)
      j += step, step >>= 1;
    s[j] += v;
    while(j > i)
      j -= step, step >>= 1;
    if(j == i)
      break;
    s[j] -= v;
  }
  if(step)
    s[j - step] -= v;
}

/*******************************************************************************
* Initialise an adaptive search tree. The tree has N nodes, and all nodes are
* initialized to zero. Any N > 0 is allowed, but is rounded up to the nearest
* value of the form N = 2**k - 2.
*******************************************************************************/
struct adapt_tree *
adapt_tree_init(int N)
{
  struct adapt_tree *t;
  int i;
  int depth;

  /* Round up to nearest 2**k - 2 */
  for(depth = 0; ((1 << (depth + 1)) - 2) < N; depth++);
  N = (1 << (depth + 1)) - 2;

  t = malloc(sizeof(*t));
  if(t)
  {
    t->s = malloc((N + 1) * sizeof(*(t->s)));
    t->v = malloc(N * sizeof(*(t->v)));
  }
  if(!(t && t->s && t->v))
  {
    fprintf(stderr, "Error: Out of memory (adapt_tree_init).\n");
    exit(1);
  }
  t->N = N;
  t->depth = depth;
  t->root = (1 << t->depth) - 1;
  t->initstep = (1 << (t->depth - 1));
  for(i = 0; i < t->N; i++)
  {
    t->s[i] = 0.0;
    t->v[i] = 0.0;
  }
  t->s[i] = 0.0;
  t->total = 0.0;
  return t;
}

/*******************************************************************************
* Free memory allocated to an adaptive search tree.
*******************************************************************************/
void
adapt_tree_free(struct adapt_tree *t)
{
  free(t->v);
  free(t->s);
  free(t);
}

/* end of adapt_tree-lib.c */
