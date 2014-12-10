/* utility functions */

#ifndef utils_extra_h
#define utils_extra_h


//------------------------------------------------------------------------------
void             circumCenter(simplex *s, double *out);
//------------------------------------------------------------------------------
int              vercmp(vertex* v1, vertex* v2);
//------------------------------------------------------------------------------
void             vertexAdd(double *a, double *b, double *out);
//------------------------------------------------------------------------------
void             vertexByScalar(double *a, double b, double *out);
//------------------------------------------------------------------------------
void             vertexSub(double *a, double *b, double *out);
//------------------------------------------------------------------------------
void             crossProduct(double *b, double *c, double *out);
//------------------------------------------------------------------------------
double           squaredDistance(double *a);
//------------------------------------------------------------------------------
double           scalarProduct(double *a, double *b);
//------------------------------------------------------------------------------
double           volumeOfTetrahedron(double *a,double *b, double *c, double *d);


#endif
