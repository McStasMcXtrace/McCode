#ifndef RESAMPLE
#define RESAMPLE


#define R_MIN(x, y)     (x) > (y) ? (y) : (x)
#define R_MAX(x, y)     (x) < (y) ? (y) : (x)
#define R_SQR(x)        ((x) * (x))

#define R_SWAP(x, y, t) {t tmp; tmp=x; x=y; y=tmp;}



%include "interpolation/array-utils.c"

%include "interpolation/natural/delaunay.c"
%include "interpolation/natural/natural.c"
%include "interpolation/natural/utils-extra.c"

%include "interpolation/nearest/kdtree.c"


vertex **resample_file(char *input_path, int *rows,
                       int samples, char *cache_path);


void interpolate3x3(treeNode *tree,
                    double x, double y, double z,
                    double *bx, double *by, double *bz);


vertex** load_table(char *input_path, int *rows);

void dump_table(vertex **points, int rows, char *output_path);

#endif
