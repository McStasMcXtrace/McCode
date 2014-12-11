/******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <string.h>

/******************************************************************************/

/* Unit testing */
/* #define _TEST_ */

/******************************************************************************/

treeNode* kdtree_addToTree(vertex **points, int left, int right, int depth)
{
  // We can modify the number of dimensions in use. This is defined in the
  // header file.

  if (right < left) return NULL;

  int d = depth % DIMENSION;

  treeNode *node = malloc(sizeof(treeNode));
  node->depth    = depth;

  int med      = kdtree_splitAboutMedian(points, d, left, right);
  node->point  = points[med];

  node->lChild = kdtree_addToTree(points, left,  med-1, depth + 1);
  node->rChild = kdtree_addToTree(points, med+1, right, depth + 1);

  return node;
}

/******************************************************************************/

void kdtree_borderCheck(vertex *v, treeNode *thisNode,
                 vertex **currentBest, double *sDist)
{
  if (!thisNode) return;
  // Check to see whether or not this node provides a better nearest neighbour.
  double thisDist = kdtree_squaredDistance(thisNode->point,v);
  if (thisDist < *sDist)
  {
    *sDist        = thisDist;
    *currentBest  = thisNode->point;
  }
  // Now recurse down the children, checking whether or not we should
  // go both sides of the splitting plane, or just down one side.
  int k = (thisNode->depth) % DIMENSION;
  if (R_SQR(thisNode->point->v[k] - v->v[k]) <= *sDist)
  {
   // The distance to the current spliting plane is less than our current
   // estimate for the shortest distance, we are going to have to traverse
   // both sides of the splitting plane.
    kdtree_borderCheck(v, thisNode->lChild, currentBest, sDist);
    kdtree_borderCheck(v, thisNode->rChild, currentBest, sDist);
  } else {
    // We only have to consider one side of the splitting plane.
    if (thisNode->point->v[k] > (*currentBest)->v[k])
      kdtree_borderCheck(v, thisNode->lChild, currentBest, sDist);
    else
      kdtree_borderCheck(v, thisNode->rChild, currentBest, sDist);
  }
}

/******************************************************************************/

vertex* kdtree_nearestNeighbourApprox(vertex* v, treeNode *tree) {
  int k = tree->depth % DIMENSION;
  int left = tree->point->v[k] > v->v[k];
  treeNode *child = left ? tree->lChild : tree->rChild;
  if (child == NULL) {
    return tree->point;
  }
  return kdtree_nearestNeighbourApprox(v, child);
}


void kdtree_nearestNeighbour_helper(vertex* v, treeNode *tree,
                             vertex **bestV, double *bestDist)
{
  int k = tree->depth % DIMENSION;

  int left = tree->point->v[k] > v->v[k];

  treeNode *first  = left ? tree->lChild : tree->rChild;
  treeNode *second = left ? tree->rChild : tree->lChild;

  // investigate first child if present
  if (first != NULL) {
    kdtree_nearestNeighbour_helper(v, first, bestV, bestDist);
  }

  // update result
  double thisDist = kdtree_squaredDistance(tree->point, v);
  if ((*bestV == NULL) || (thisDist < *bestDist)) {
    *bestDist = thisDist;
    *bestV  = tree->point;
  }

  // no second child to investigate
  if (second == NULL) {
    return;
  }

  // we only investigate second child if necessary
  int treek = tree->point->v[k];

  if (R_SQR(treek - v->v[k]) <= *bestDist) {
    kdtree_borderCheck(v, second, bestV, bestDist);
  }
}

vertex* kdtree_nearestNeighbour(vertex* v, treeNode *tree) {
  vertex *bestV = NULL;
  double bestDist = 0;

  kdtree_nearestNeighbour_helper(v, tree, &bestV, &bestDist);

  return bestV;
}

/******************************************************************************/
// Calculate the standard Euclidean distance between these two points
// in whatever dimension we are considering.

double kdtree_squaredDistance(vertex* a, vertex* b)
{
  int i;
  double sum = 0;
  for (i = 0; i < DIMENSION; i++) {
    sum += R_SQR(a->v[i] - b->v[i]);
  }
  return sum;
}

/******************************************************************************/

// Note we slightly modify the standard partition algorithm, so that we
// can partition based on only one dimension of the pointset.
int kdtree_partition(vertex **points, int d, int left, int right, int pivot)
{
  double pivotValue = points[pivot]->v[d];
  int i;
  int storeIndex = left;

  R_SWAP(points[pivot], points[right], vertex*);

  for (i = left; i < right; i++) {
    if (points[i]->v[d] < pivotValue) {
      R_SWAP(points[storeIndex], points[i], vertex*);
      storeIndex ++;
    }
  }
  R_SWAP(points[right], points[storeIndex], vertex*);

  return storeIndex;
}

/******************************************************************************/
// Find the median in expected linear time. - We will also pivot all the data
// about the found median, returning the integer giving the pivot value.

int kdtree_splitAboutMedian(vertex **points, int d, int left, int right)
{
  int k = (right-left)/2 +left;

  // This isn't a perfect uniform distribution, but it doesn't really matter
  // for this application.
  while (left < right)
  {
    int pivotIndex = rand() % (right-left)+left;
    int pivotNewIndex = kdtree_partition(points,d,left,right,pivotIndex);
    if (k == pivotNewIndex)
      return k;
    else if (k < pivotNewIndex)
      right = pivotNewIndex-1;
    else
      left = pivotNewIndex+1;
  }

  return left;
}

/******************************************************************************/

vertex **kdtree_loadPoints(char *filename, int *n)
{
  // Read the table with Read Table Lib, then convert it to vertex[] layout
  // We do it this way because Read Table Lib can find files in the library path
  // and because the kdtree code uses the regular vertex array layout
  t_Table table;
  Table_Init(&table, 1, 6);

  if(!Table_Read(&table, filename, 0) || table.rows <= 0) {
    // Give up!
    fprintf(stderr, "Could not open file: '%s'.\n", filename);
    *n = 0;
    Table_Free_Array(&table);
    return NULL;
  }

  // Let caller know how many rows / points we read
  *n = table.rows;

  // Allocate array of vertex pointers
  vertex **ps = calloc(table.rows, sizeof(vertex*));

  // Convert from table to array layout
  int i, j;
  for (i=0; i < table.rows; i++)
  {
    vertex *v = malloc(sizeof(vertex));
    for (j = 0; j < 3; j++) {
      v->v[j]    = Table_Index(table, i,     j);
      v->data[j] = Table_Index(table, i, 3 + j);
    }
    ps[i] = v;
  }

  // Free table (points are in ps now)
  Table_Free(&table);

  return ps;
}

/******************************************************************************/

vertex **kdtree_initPoints(double *x, double *y, double *z,
                    double *u, double *v, double *w, int n)
{
  vertex** ps = malloc(sizeof(vertex) *n);

  int i;
  for (i=0; i<n; i++)
  {
    ps[i] = malloc(sizeof(vertex));
    ps[i]->X = x[i];
    ps[i]->Y = y[i];
    ps[i]->Z = z[i];

    ps[i]->U = u[i];
    ps[i]->V = v[i];
    ps[i]->W = w[i];
  }

  return ps;
}

/******************************************************************************/

void kdtree_freePoints(vertex **ps, int n)
{
  int i;

  for (i=0; i<n;i++)
    free(ps[i]);

  free(ps);

}

/******************************************************************************/

void kdtree_writePointsToFile(vertex **ps, int n)
{
  FILE *f = fopen("./points.mat", "wt");
  int i;

  for (i=0; i<n; i++)
    fprintf(f, "%lf %lf %lf %lf %lf %lf\n", ps[i]->X, ps[i]->Y, ps[i]->Z,
                                            ps[i]->U, ps[i]->V, ps[i]->W);
  fclose(f);
}

/******************************************************************************/

// Unit testing.
#ifdef _TEST_

/******************************************************************************/

vertex* kdtree_nearestNeighbourSlow(vertex *v, vertex **ps, int n)
{
  // A first estimate of the nearest neighbour.
  vertex *best = ps[0];
  double bestDist = kdtree_squaredDistance(v, best);

  int i;
  for (i=1; i<n; i++) {
    double thisDist =  kdtree_squaredDistance(v, ps[i]);
    if (thisDist < bestDist) {
      bestDist = thisDist;
      best = ps[i];
    }
  }

  return best;
}

/******************************************************************************/

void kdtree_testNearestNeighbour(treeNode* tree, vertex **ps, int n)
{
  int i;
  for (i=0; i<1e7; i++)
  {
    vertex v = {{rand()%40 - 20, rand()%40 - 20, rand()%40 - 20},{0,0,0}};
    vertex *nearest = kdtree_nearestNeighbour(&v, tree);
    /* vertex *nearest2 = kdtree_nearestNeighbourSlow(&v, ps, n); */

    double sd1 = kdtree_squaredDistance(&v, nearest);
    double sd2 = kdtree_squaredDistance(&v, nearest);

    if (sd1 != sd2)
    {
      fprintf(stderr, "Error: %lf, %lf\n", sd1, sd2);
      fprintf(stderr, "Testing failed.\n");

      FILE *f = fopen("./nearest.mat", "wt");
      fprintf(f, "%lf %lf %lf\n", v.X, v.Y, v.Z);
      fprintf(f, "%lf %lf %lf\n", nearest->X, nearest->Y, nearest->Z);
      /* fprintf(f, "%lf %lf %lf\n", nearest2->X, nearest2->Y, nearest2->Z); */
      fclose(f);
      exit(1);
    }
  }
  printf("Passed.\n");
}

/******************************************************************************/

int main(int argc, char **argv)
{
  srand ( time(NULL) );

  int n;
  /* This must be set to the location of some appropriate test pointset */
  vertex **ps = kdtree_loadPoints("./test.mat", &n);

  treeNode* topOfTree;
  topOfTree = kdtree_addToTree(ps,0,n-1,0);
  kdtree_testNearestNeighbour(topOfTree, ps, n);

  return 0;
}

/******************************************************************************/
#endif
