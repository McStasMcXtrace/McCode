/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/adapt_tree-lib.h
*
* %Identification
* Written by: KN, EF
* Date:   Sep 02, 2002
* Origin: Risoe/ILL
* Release: McStas 1.6
* Version: $Revision$
*
* This file is to be imported by components handling adaptive trees, like
* Source_adapt and Adapt_check (in lib/sources)
* It handles some shared functions.
*
* Usage: within SHARE
* %include "adapt_tree-lib"
*
*******************************************************************************/

#ifndef ADAPT_TREE_LIB_H
#define ADAPT_TREE_LIB_H "1.1.0"

/* Adaptive search tree definitions. */
typedef double adapt_t;

/*******************************************************************************
* Structure of an adaptive search tree. The v array runs from 0 to N-1 (a
* total of N elements) and holds the values of each node. The sum of all v
* values is in total.
* The s array runs from 0 to N and is used to represents the cumulative sum
* of v[0] through v[i-1]. The number represented is the sum of s[i] and all
* its parents up to the root node.
*******************************************************************************/

struct adapt_tree
  {
    adapt_t *s, *v, total;
    int N;      /* < 1 << (depth+1) */
    int depth;
    int root;     /* = (1 << depth) - 1 */
    int initstep;   /* = 1 << (depth-1) */
  };

/* adapt_tree-lib function prototypes */
/* ========================================================================= */
#pragma acc routine
int adapt_tree_search(struct adapt_tree *t, adapt_t v);
#pragma acc routine
void adapt_tree_add(struct adapt_tree *t, int i, adapt_t v);
struct adapt_tree * adapt_tree_init(int N);
void adapt_tree_free(struct adapt_tree *t);

#endif

/* end of adapt_tree-lib.h */
