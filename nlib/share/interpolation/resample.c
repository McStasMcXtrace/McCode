#include<assert.h>
#include<stdlib.h>
#include<math.h>


%include "interpolation/resample.h"


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

double betweenH(int i, int base, double a, double b) {
  double r = halton(i, base);
  double v = (b - a) * r + a;
  assert(a <= v && v < b);
  return v;
}

double betweenR(double a, double b) {
  double r = (double)rand() / ((double)RAND_MAX);
  double v = (b - a) * r + a;
  assert(a <= v && v < b);
  return v;
}


int x_compare(const void *a, const void *b) {
  const vertex* va = (const vertex *)a;
  const vertex* vb = (const vertex *)b;
  return (va->X) - (vb->X);
}


vertex **resample(vertex **p, int rows, int new_points) {

  printf("preparing for sampling..\n");
  int i;
  for(i = 0; i < rows; i++) {
    p[i]->index = i;
    p[i]->voronoiVolume = -1;
  }

  mesh *mesh = delaunay_newMesh();
  delaunay_buildMesh(p, rows, mesh);

  vertex min, max, range;
  delaunay_getRange(p, rows, &min, &max, &range, 0);

  /* p = realloc(p, sizeof(vertex*) * (rows + new_points)); */
  p = malloc(sizeof(vertex*) * (rows + new_points));

  printf("sampling..\n");
  for(i = 0; i < rows + new_points; i++) {
    int index = i;

    vertex *v = malloc(sizeof(vertex));
    v->X = betweenH(i, 2, min.X, max.X);
    v->Y = betweenH(i, 3, min.Y, max.Y);
    v->Z = betweenH(i, 5, min.Z, max.Z);

    /* v->X = betweenR(min.X, max.X); */
    /* v->Y = betweenR(min.Y, max.Y); */
    /* v->Z = betweenR(min.Z, max.Z); */

    interpolate3_3(v->X, v->Y, v->Z,
                   &(v->U), &(v->V), &(v->W),
                   mesh);

    v->index = index;
    v->voronoiVolume = -1;

    p[index] = v;
  }

  delaunay_freeMesh(mesh);

  // points are always ordered by X
  // TODO: is it required by the kd-tree that points are ordered by x?
  qsort(p, rows + new_points, sizeof(vertex*), x_compare);

  return p;

}


void interpolate3x3(treeNode *tree,
                    double x, double y, double z,
                    double *bx, double *by, double *bz)
{
  vertex v = { x, y, z, 0, 0, 0, 0, -1 };
  kdtree_nearestNeighbour(&v, tree);
  *bx = v.U;
  *by = v.V;
  *bz = v.W;
}



void dump_table(vertex **points, int rows, char *output_path) {
  FILE *file = fopen(output_path, "w");

  int i;
  for(i = 0; i < rows; i++) {
    vertex *p = points[i];

    char line[256];
    int size = snprintf(line, 256, "%f %f %f %f %f %f\n",
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
                         int samples, char *cache_path)
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
  points = resample(points, *rows, samples);
  *rows = (*rows) + samples;

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
