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
* Version: $Revision: 1.8 $
*
* This file is to be imported by components handling adaptive trees, like
* Source_adapt and Adapt_check (in lib/sources)
* It handles some shared functions.
*
* Usage: within SHARE
* %include "adapt_tree-lib"
*
* $Id: adapt_tree-lib.h,v 1.8 2007-03-12 14:57:21 farhi Exp $
*
* $Log: not supported by cvs2svn $
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

int adapt_tree_search(struct adapt_tree *t, adapt_t v);
void adapt_tree_add(struct adapt_tree *t, int i, adapt_t v);
struct adapt_tree * adapt_tree_init(int N);
void adapt_tree_free(struct adapt_tree *t);

#endif

/* end of adapt_tree-lib.h */
