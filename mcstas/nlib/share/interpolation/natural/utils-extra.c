/* utility functions */

int vercmp(vertex *v1, vertex *v2)
{
  int i;
  for (i=0; i<3; i++)
    if ( v1->v[i] != v2->v[i] ) return 0;
  return 1;
}


/******************************************************************************/
// This will give us the volume of the arbitrary tetrahedron formed by
// v1, v2, v3, v4
// All arguments are arrays of length three of doubles.

double volumeOfTetrahedron(double *a, double *b, double *c, double *d)
{
  double a_d[3], b_d[3], c_d[3], cross[3];

  vertexSub(a,d, a_d);
  vertexSub(b,d, b_d);
  vertexSub(c,d, c_d);

  crossProduct(b_d, c_d, cross);
  double v = scalarProduct(a_d, cross)/(double)6;

  return (v >= 0) ? v : -v;
}

/******************************************************************************/

double squaredDistance(double *a)
{
  return scalarProduct(a,a);
}

/******************************************************************************/
// Take the cross product of two verticies and put it in the vertex 'out'.
void crossProduct(double *b, double *c, double *out)
{
  out[0] = b[1] * c[2] - b[2] * c[1];
  out[1] = b[2] * c[0] - b[0] * c[2];
  out[2] = b[0] * c[1] - b[1] * c[0];
}

/******************************************************************************/

double scalarProduct(double *a, double *b)
{
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

/******************************************************************************/

void vertexSub(double *a, double *b, double *out)
{
  out[0] = a[0] - b[0];
  out[1] = a[1] - b[1];
  out[2] = a[2] - b[2];
}

/******************************************************************************/

void vertexAdd(double *a, double *b, double *out)
{
  out[0] = a[0] + b[0];
  out[1] = a[1] + b[1];
  out[2] = a[2] + b[2];
}

/******************************************************************************/
// Note that this modifies the actual value of the given vertex.

void vertexByScalar(double *a, double b, double *out)
{
  out[0] = a[0] * b;
  out[1] = a[1] * b;
  out[2] = a[2] * b;
}

/******************************************************************************/
// This function will compute the circumcenter of a given simplex.
// -it returns the radius.-

void circumCenter(simplex *s, double *out)
{
  vertex *a, *b, *c, *d;
  delaunay_getFaceVerticies(s, 0, &a, &b, &c, &d);

  double b_a[3]   , c_a[3]   , d_a[3],
         cross1[3], cross2[3], cross3[3],
         mult1[3] , mult2[3] , mult3[3],
         sum[3];
  double denominator;

  // Calculate diferences between points.
  vertexSub(b->v, a->v, b_a);
  vertexSub(c->v, a->v, c_a);
  vertexSub(d->v, a->v, d_a);

  // Calculate first cross product.
  crossProduct(b_a, c_a, cross1);

  // Calculate second cross product.
  crossProduct(d_a, b_a, cross2);

  // Calculate third cross product.
  crossProduct(c_a, d_a, cross3);

  vertexByScalar(cross1, squaredDistance(d_a), mult1);
  vertexByScalar(cross2, squaredDistance(c_a), mult2);
  vertexByScalar(cross3, squaredDistance(b_a), mult3);

  // Add up the sum of the numerator.
  vertexAdd(mult1, mult2, sum);
  vertexAdd(mult3, sum  , sum);

  // Calculate the denominator.
  denominator = 2*scalarProduct(b_a, cross3);

  // Do the division, and output to out.
  vertexByScalar(sum, 1/(double)(denominator), out);

  vertexAdd(out, a->v, out);

  // Calculate the radius of this sphere. - We don't actually need this.
  // But if we need it for debugging, we can add it back in.
  // return sqrt((double)squaredDistance(sum))/(double)denominator;
}
