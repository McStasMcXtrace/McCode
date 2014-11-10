/******************************************************************************/
/*

  delaunay.h - By Ross Hemsley Aug. 2009 - rh7223@bris.ac.uk.

  This module will compute the delaunay triangulation of a set of uniformly
  distributed points in R^3. We will use the iterative edge flipping
  algorithm to add points one at a time.

  To store the triangulation, we start by just storing simplicies with pointers
  to their respective coordinates.

  To simplify insertion, we first create a super-simplex with contains our
  entire dataset, this means that we don't have to specify any special
  conditions for the creation of our original simplicies.

  To make our algorithm robust we use Jonathan Shewchuk's Arbitrary Precision
  Floating-poing Arithmetic predicates[1]


  [1]  Routines for Arbitrary Precision Floating-point Arithmetic
       and Fast Robust Geometric Predicates. May 18, 1996.
       Jonathan Richard Shewchuk.

*/
/******************************************************************************/
#ifndef delaunay_h
#define delaunay_h

%include "interpolation/array-utils.h"
%include "interpolation/natural/vertex.h"

/*******************************************************************************
* This is how we represent a Voronoi Cell in memory.
*******************************************************************************/
typedef struct
{
  // The number of points on this cell, and the amount
  // of memory allocated for points on this cell.
  int n, nallocated;
  // The array of points on this cell.
  double **points;

  // This defines the cell, it contains a list of faces, each one is
  // consistantly oriented relative to itself (so traversing the pionts gives
  // the convex hull of the face). Each face is seperated by a NULL pointer.
  // No gaurentee is made about the consistancy of orientations between
  // different faces.
  arrayList *verticies;

} voronoiCell;

/******************************************************************************/
/* This is how we store an individual simplex: 4 pointers to the coordinates. */
/* We should try storing this without pointers probably.                      */
/******************************************************************************/
typedef struct _simplex
{
  // The verticies of this simplex.
  vertex  *p[4];
  // The neighbouring simlpicies of this simplex.
  // These are ordered in accordance with our 'get face' routine:
  // so that the i'th face is shared with the i'th neighbour.
  struct _simplex *s[4];
  // This is the node in our auxillary list structure that holds this simplex.
  // It's far from an elegant solution: but it is fast and space-efficient.
  listNode *node;
} simplex;

/******************************************************************************/
/* We want to efficiently change back the neighbour pointers when             */
/* we remove a point.                                                         */
/******************************************************************************/
typedef struct
{
  arrayStack  *ptrs;
  arrayStack  *old;
} neighbourUpdate;

/******************************************************************************/
// We will keep all details of the mesh in this structure: thus hiding
// the complexities of memory pooling from the user. We also want to store
// the lists of most recently destroyed and allocated simplicies/neighbour
// updates, so that we speed up the 'remove last point' operation,
// which is _crucial_ to fast natural neighbour interpolation.
/******************************************************************************/

typedef struct
{
  // a linked list of all the simplicies.
  linkedList    *tets;

  // The simplex which contains all of the points.
  // its verticies contain no data values.
  simplex *super;
  vertex   superVerticies[4];

  // Memory pool.
  arrayStack   *deadSimplicies;
  arrayStack   *deadVoronoiCells;

  // We modify these when a point is inserted/removed.
  arrayList       *conflicts;
  arrayList       *updates;
  neighbourUpdate *neighbourUpdates;

  // Keep count of the number of degenerecies we find in the mesh,
  // so that we can spot errors, and be aware of particularly degenerate data.
  int coplanar_degenerecies;
  int cospherical_degenerecies;

} mesh;

/******************************************************************************/
mesh*            delaunay_newMesh();
//------------------------------------------------------------------------------
void             delaunay_freeMesh(mesh *m);
//------------------------------------------------------------------------------
void             delaunay_removePoint(mesh *m);
//------------------------------------------------------------------------------
vertex*          delaunay_loadPoints(char *filename, int *n);
//------------------------------------------------------------------------------
void             delaunay_getRange(vertex **ps, int n, vertex *min,
                                   vertex *max, vertex *range, int r);
//------------------------------------------------------------------------------
void             delaunay_initSuperSimplex(vertex **ps, int n, mesh *m);
//------------------------------------------------------------------------------
void             delaunay_writePointsToFile(vertex *ps, int n);
//------------------------------------------------------------------------------
void             delaunay_writeTetsToFile(mesh *m);
//------------------------------------------------------------------------------
int              delaunay_simplexContainsPoint(simplex *s, vertex *p);
//------------------------------------------------------------------------------
void             delaunay_getFaceVerticies(simplex *s, int i, vertex **p1, vertex **p2,
                                                     vertex **p3, vertex **p4 );
//------------------------------------------------------------------------------
void             delaunay_faceTest(mesh *m);
//------------------------------------------------------------------------------
void             delaunay_orientationTest(linkedList *tets);
//------------------------------------------------------------------------------
void             delaunay_allTests(linkedList *tets);
//------------------------------------------------------------------------------
void             delaunay_addSimplexToMesh(mesh *m, simplex *s);
//------------------------------------------------------------------------------
void             delaunay_removeSimplexFromMesh(mesh *m, simplex *s);
//------------------------------------------------------------------------------
simplex*         delaunay_findContainingSimplex(mesh *m, vertex *p);
//------------------------------------------------------------------------------
int              delaunay_isDelaunay(simplex *s, vertex *p);
//------------------------------------------------------------------------------
simplex**        delaunay_swapSimplexNeighbour(simplex *s, simplex *old, simplex *new);
//------------------------------------------------------------------------------
simplex*         delaunay_findNeighbour(simplex *s, vertex *p);
//------------------------------------------------------------------------------
void             delaunay_setOrientationBits(simplex *s);
//------------------------------------------------------------------------------
void             delaunay_buildMesh(vertex** ps, int n, mesh *m);
//------------------------------------------------------------------------------
void             delaunay_addPointToMesh(vertex *p, linkedList *tets);
//------------------------------------------------------------------------------
int              delaunay_pointOnSimplex(vertex *p, simplex *s);
//------------------------------------------------------------------------------
void             delaunay_printEdge(vertex *v1, vertex* v2, FILE *stream);
//------------------------------------------------------------------------------
int              delaunay_isConvex(vertex *v1, vertex *v2, vertex *v3,
                                      vertex *t,  vertex *b);
//------------------------------------------------------------------------------
void             delaunay_setNeighbourIndex(simplex *s, int i, int newIndex);
//------------------------------------------------------------------------------
int              delaunay_getNeighbourIndex(simplex *s, int i);
//------------------------------------------------------------------------------
arrayList*       delaunay_findNeighbours(vertex *v, simplex *s);
//------------------------------------------------------------------------------
simplex*         delaunay_newSimplex(mesh *m);
//------------------------------------------------------------------------------
void             delaunay_addPoint(vertex *p, mesh *m);
//------------------------------------------------------------------------------
int              delaunay_delaunayTest(mesh *m, vertex *ps, int n);
//------------------------------------------------------------------------------
voronoiCell*     delaunay_getVoronoiCell(vertex *point, simplex *s0, mesh *m);
//------------------------------------------------------------------------------
void             delaunay_setNeighbours(arrayList *newTets);
//------------------------------------------------------------------------------
int              delaunay_shareThreePoints(simplex *s0, int i, simplex *s1);
//------------------------------------------------------------------------------
double           delaunay_voronoiCellVolume(voronoiCell *vc, vertex *p);
//------------------------------------------------------------------------------
void             delaunay_removeExternalSimplicies(mesh *m);
//------------------------------------------------------------------------------
arrayList*       delaunay_naturalNeighbours(vertex *v, mesh *m);
//------------------------------------------------------------------------------
void             delaunay_writeVoronoiCellToFile(FILE* f, voronoiCell *vc);
//------------------------------------------------------------------------------
void             delaunay_freeVoronoiCell(voronoiCell *vc, mesh *m);
//------------------------------------------------------------------------------
neighbourUpdate* delaunay_initNeighbourUpdates();
//------------------------------------------------------------------------------
void             delaunay_resetNeighbourUpdates(neighbourUpdate *nu);
//------------------------------------------------------------------------------
void             delaunay_undoNeighbourUpdates(neighbourUpdate *nu);
//------------------------------------------------------------------------------
void             delaunay_pushNeighbourUpdate(neighbourUpdate *nu, simplex **ptr,
                                                          simplex  *old);
//------------------------------------------------------------------------------
void             delaunay_freeNeighbourUpdates(neighbourUpdate *nu);
//------------------------------------------------------------------------------
simplex*         delaunay_findAnyNeighbour(vertex *v, arrayList *tets);
//------------------------------------------------------------------------------
int              delaunay_getNumSimplicies(mesh *m);
//------------------------------------------------------------------------------
void             delaunay_randomPerturbation(vertex *v, int attempt);
//------------------------------------------------------------------------------
int              delaunay_numSphericalDegenerecies(mesh *m);
//------------------------------------------------------------------------------
int              delaunay_numPlanarDegenerecies(mesh *m);
/******************************************************************************/

#endif
