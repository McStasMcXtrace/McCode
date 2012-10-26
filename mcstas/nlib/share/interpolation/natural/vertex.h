
#ifndef vertex_t
#define vertex_t


// The number of dimensions to use in the tree.
#define DIMENSION 3

/******************************************************************************/

/* These macros make code more readable. They allow us to access
   the indexed elements of verticies directly.                    */
#define X v[0]
#define Y v[1]
#define Z v[2]
#define U data[0]
#define V data[1]
#define W data[2]


typedef struct
{
  // This is the location of this point.
  double v[3];

  // These are the values for our vector field at this location.
  double data[3];

  // This is the point index in the point list.
  // We only store this so that it is convenient for using the
  // tet-mesh function in Matlab.
  // We can remove it for when the code is actually used.
  int    index;

  // We use this for caching the voronoi volume of this point.
  // it will provide a good speed-up!
  double voronoiVolume;

} vertex;

#endif
