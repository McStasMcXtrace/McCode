/*******************************************************************************
* kdtree.h
*
* Written by Ross Hemsley for McStas. (September 2009)
*
* A simple kd-tree to implement fast nearest-neighbour lookup.
*******************************************************************************/
#ifndef kdtree_h
#define kdtree_h
/******************************************************************************/

%include "interpolation/array-utils.h"
%include "interpolation/nearest/vertex.h"

/******************************************************************************/

/* This is how we store a point to insert into our tree */
/* #ifndef vertex_t */
/* #define vertex_t */

/* typedef struct { */
/*   double v[DIMENSION]; */
/*   // this can be changed arbitrarily to store any data. */
/*   double data[3]; */
/* } vertex; */

/* #endif */

/* This struct will store each node of our kdtree. */
typedef struct _treeNode {
  vertex    *point;
  int       depth;
  struct _treeNode *rChild;
  struct _treeNode *lChild;
} treeNode;

/******************************************************************************/
treeNode* kdtree_addToTree(vertex **points, int left, int right, int depth);
//------------------------------------------------------------------------------
vertex**  kdtree_loadPoints(char *filename, int *n);
//------------------------------------------------------------------------------
void      kdtree_freePoints(vertex **ps, int n);
//------------------------------------------------------------------------------
vertex**  kdtree_initPoints(double *x, double *y, double *z,
                     double *u, double *v, double *w, int n);
//------------------------------------------------------------------------------
int       kdtree_splitAboutMedian(vertex **points, int d, int left, int right);
//------------------------------------------------------------------------------
int       kdtree_partition(vertex **points, int d, int left, int right, int pivot);
//------------------------------------------------------------------------------
double   kdtree_squaredDistance(vertex* a, vertex* b);
//------------------------------------------------------------------------------
vertex*  kdtree_nearestNeighbour(vertex *v, treeNode *tree);
//------------------------------------------------------------------------------
void     kdtree_writePointsToFile(vertex **ps, int n);
//------------------------------------------------------------------------------
vertex*  kdtree_nearestNeighbourApprox(vertex* v, treeNode *tree);
//------------------------------------------------------------------------------
void kdtree_borderCheck(vertex *v, treeNode *thisNode,
                 vertex **currentBest, double *sDist);
/******************************************************************************/
#endif
