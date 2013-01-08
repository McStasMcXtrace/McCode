#include<assert.h>
#include<stdlib.h>
#include<math.h>



int calcSamples(int *steps) {
  int samples = 1;
  int dim;
  for(dim = 0; dim < 3; dim++) {
    samples *= MAX(1, steps[dim]);
  }
  return samples;
}

double halton(int index, int base) {
  double result = 0;
  double f = 1.0 / base;
  while (index > 0) {
    result += f * (index % base);
    index /= base;
    f /= base;
  }
  return result;
}


double betweenH(int i, int *steps, int dim, double a, double b) {
  // Select points using halton sequences
  int base[] = {2, 3, 5};
  double r = halton(i, base[dim]);
  double v = (b - a) * r + a;
  assert(a <= v && v < b);
  return v;
}

double betweenR(int i, int *steps, int dim, double a, double b) {
  // Select points at random
  double r = (double)rand() / ((double)RAND_MAX);
  double v = (b - a) * r + a;
  assert(a <= v && v < b);
  return v;
}


int tdrt(int N) {
  // Calculate accurate third root (search down from pow(N, 1/3))
  int root = ceil(pow((double)N, 1.0/3.0));
  if ((root * root * root) > N) {
    root -= 1;
  }
  return root;
}

double betweenG(int i, int *steps, int dim, double a, double b) {
  // Select points systematically (number of points are chosen by steps - 3d)
  int top = steps[dim];
  int j = (i / (int)(pow(top, dim))) % top;
  double width = (b - a) / ((double)top - 1);

  return MAX(a, MIN(b, (a + (width * j))));
}


int reverseG(int *steps, vertex *min, vertex *max, vertex *v) {
  // Each intermediate value has one per dimension
  double xyz[3];
  double width[3];
  double index[3];

  int dim;
  for(dim = 0; dim < 3; dim++) {
    // Truncate values to fit interval
    xyz[dim] = MIN(MAX(v->v[dim], min->v[dim]), max->v[dim]);
    // Compute step-size
    width[dim] = ((max->v[dim]) - (min->v[dim])) / ((double)steps[dim] - 1);
    // Compute index for each dimension (of size width*steps)
    index[dim] = round((xyz[dim]-(min->v[dim])) / width[dim]);
    index[dim] = MIN(index[dim], width[dim] - 1);
  }

  // Combine indices to one flat index
  double arrIndex = 0;
  int base = 1;
  for(dim = 0; dim < 3; dim++) {
    arrIndex += index[dim];
    base *= steps[dim];
  }

  // Round off to nearest
  return round(arrIndex);
}


int x_compare(const void *a, const void *b) {
  const vertex* va = (const vertex *)a;
  const vertex* vb = (const vertex *)b;
  return (va->X) - (vb->X);
}


vertex **resample_f(vertex **p, int rows, int *steps,
                    double (*between)(int i, int *steps, int dim, double min, double max),
                    vertex *min, vertex* max)
{

  /* printf("preparing for sampling..\n"); */
  int i;
  int col;

  int new_points = calcSamples(steps);

  mesh *mesh = delaunay_newMesh();
  delaunay_buildMesh(p, rows, mesh);

  vertex range;
  delaunay_getRange(p, rows, min, max, &range, 0);

  p = calloc(new_points, sizeof(vertex*));

  /* printf("sampling..\n"); */
  for(i = 0; i < new_points; i++) {

    vertex *v = malloc(sizeof(vertex));

    for(col = 0; col < 3; col++) {
      // v->v[0] == v->X, v->v[1] == v->Y, v->v[2] == v->Z
      double val = between(i, steps, 0, min->v[col], max->v[col]);
      // sanitise between min and max
      v->v[col] = MAX(min->v[col], MIN(max->v[col], val));

    }

    interpolate3_3(v->X, v->Y, v->Z,
                   &(v->U), &(v->V), &(v->W),
                   mesh);

    p[i] = v;
  }

  delaunay_freeMesh(mesh);

  // points are always ordered by X
  // TODO: is it required by the kd-tree that points are ordered by x?
  qsort(p, new_points, sizeof(vertex*), x_compare);

  return p;

}


vertex **resample(vertex **p, int rows, int *steps) {
  vertex min, max;
  return resample_f(p, rows, steps, betweenG, &min, &max);
}


void interpolate3x3(treeNode *tree,
                    double x, double y, double z,
                    double *bx, double *by, double *bz)
{
  vertex v = { x, y, z, 0, 0, 0, 0, -1 };
  vertex *w;
   w=kdtree_nearestNeighbour(&v, tree);
  *bx = w->U;
  *by = w->V;
  *bz = w->W;
}



void dump_table(vertex **points, int rows, char *output_path) {
  FILE *file = fopen(output_path, "w");

  char line[256];
  snprintf(line, 256, "# %d\n", rows);
  fputs(line, file);

  int i;
  for(i = 0; i < rows; i++) {
    vertex *p = points[i];

    int size = snprintf(line, 256, "%lf %lf %lf %lf %lf %lf\n",
                        p->X, p->Y, p->Z, p->U, p->V, p->W);
    if(size >= 256) {
      fprintf(stderr, "WARNING: Line was truncated in resample, dump_tables!\n");
    }

    fputs(line, file);
  }

  fclose(file);
}


vertex** load_table(char *input_path, int *rows) {
  // the inverse of dump_table
  return kdtree_loadPoints(input_path, rows);
}


vertex **resample_file(char *input_path, int *rows,
                       int *steps, char *cache_path,
                       double (*between)(int i, int *steps, int dim, double min, double max),
                       vertex *min, vertex *max)
{

  // Use kdtree's loadPoints to read whitespace seperate lines for now
  // TODO: Consider using read_table-lib (more flexible)
  *rows = -1;
  vertex **points = kdtree_loadPoints(input_path, rows);

  if (points == NULL || rows <= 0) {
    fprintf(stderr, "Failed to read table (rows=%d): %s\n", *rows, input_path);
    return NULL;
  }

  // resample to a total of (rows + samples) points
  points = resample_f(points, *rows, steps, between, min, max);

  *rows = calcSamples(steps);

  // update cache
  if (cache_path != NULL) {
    char tmp_path[256];
    snprintf(tmp_path, 256, "%s.part", cache_path);

    // write points to temporary path, then move atomically into final path
    dump_table(points, *rows, tmp_path);
    rename(tmp_path, cache_path);
  }

  return points;

}


// Clean up macros
#undef Absolute
#undef DIMENSION
#undef Eight_Four_Sum
#undef Eight_One_Sum
#undef Eight_Two_Sum
#undef Fast_Two_Diff
#undef Fast_Two_Diff_Tail
#undef Fast_Two_Sum
#undef Fast_Two_Sum_Tail
#undef Four_Four_Sum
#undef Four_One_Product
#undef Four_One_Sum
#undef Four_Two_Sum
#undef INEXACT
#undef INTERPOLATE_DETAIL
#undef NARROWRAND
#undef NUM_TEST_POINTS
#undef OUTPUT_TO_FILE
#undef PERTURBATION_VALUE
#undef R_MAX
#undef R_MIN
#undef R_SQR
#undef R_SWAP
#undef Split
#undef Square
#undef Square_Tail
#undef Two_Diff
#undef Two_Diff_Tail
#undef Two_One_Diff
#undef Two_One_Product
#undef Two_One_Sum
#undef Two_Product
#undef Two_Product_
#undef Two_Product_Presplit
#undef Two_Product_Tail
#undef Two_Square
#undef Two_Sum
#undef Two_Sum_Tail
#undef Two_Two_Diff
#undef Two_Two_Product
#undef Two_Two_Sum
#undef UNIFORMRAND
#undef U
#undef V
#undef W
#undef X
#undef Y
#undef Z
