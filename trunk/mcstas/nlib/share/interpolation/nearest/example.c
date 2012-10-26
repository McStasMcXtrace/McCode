#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "kdtree.h"
#include "array-utils.h"


int main(int argc, char **argv)
{
  // the number of points in our point set.
  int n = 3;

  // the points.
  double x[] = {0,1,2};
  double y[] = {3,4,5};
  double z[] = {6,7,8};

  double u[] = {0,1,2};
  double v[] = {3,4,5};
  double w[] = {6,7,8};

  // initialise the points.
  vertex** ps = initPoints(x,y,z,  u,v,w,  n);

  treeNode *tree = addToTree(ps,0,n-1,0);

  // perform interpolation.
  vertex p = {{1,2,3},{0,0,0}};

  vertex *nn =  nearestNeighbour(&p, tree);

  printf("Interpolated value at (1,2,3) is (%lf, %lf %lf).\n",
                                                          nn->U, nn->V, nn->W);


  freePoints(ps,n);

}
