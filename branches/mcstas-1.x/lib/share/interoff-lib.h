#ifndef EPSILON
#define EPSILON 10e-6
#endif

#ifndef DBL_MAX
#define DBL_MAX 1E+37
#endif

#define GEOMVIEW 0
#define buf 256



typedef struct intersection {
	double time;  	//time of the intersection
	Coords v;	//intersection point
	short in_out;	//1 if the ray enters the volume, -1 otherwise
	short edge;	//1 if the intersection is on the boundary of the polygon, and error is possible
} intersection;

typedef struct polygon {
  Coords* p; //vertices of the polygon in adjacent order
  int npol;  //number of Coords
} polygon;

//gives the normal vector of p
void normal(Coords* , polygon );

//scalar product
double scalarp(Coords , Coords );

//based on http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
//return 0 if the Coords is out
//	 1 if it is in
//	 -1 if on the boundary
int pnpoly(polygon , Coords );

//gives the intersection Coords between ray [a,b) and polygon p and its prametric value on (a b)
//http://geometryalgorithms.com/Archive/algorithm_0105/algorithm_0105.htm
int intersectPoly(intersection *, Coords , Coords , polygon );

/*reads the indexes at the beginning of the off file as this :
line 1  OFF
line 2  nbVertex nbFaces nbEdges
*/
void getBlocksIndex(char* , unsigned long* , unsigned long* , unsigned long*  );


double F(Coords , double , double , double , double );


//gives the equations of 2 perpandicular planes of [ab]
void init_planes(Coords , Coords , double* , double* , double* , double *, double* , double* , double* );



int sign(double );


int clip_3D_mod(intersection* , Coords , Coords , t_Table , t_Table  );


int compare (void const *, void const *);


//given an array of intesction throw those which appear several times
//returns 1 if there is a possibility of error
int cleanDouble(intersection* , int* );

//given an array of intesction throw those which enter and exit in the same time
//Meaning the ray passes very close to the volume
//returns 1 if there is a possibility of error
int cleanInOut(intersection* , int* );


