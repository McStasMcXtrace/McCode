/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2008, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/interoff.h
*
* %Identification
* Written by: Reynald Arnerin
* Date:    Jun 12, 2008
* Release: 
* Version: 
*
* Runtime system header for McStas.
*
*******************************************************************************/

#ifndef INTEROFF_LIB_H
#define INTEROFF_LIB_H "$Revision: 1.1 $"

#ifndef EPSILON
#	define EPSILON 10e-13
#endif

//#include <float.h>

#define GEOMVIEW 0
#define buf 256
#define MAX_POL_SIZE 256
/*
typedef struct vertex {
  double x,y,z;
} vertex;
*/
typedef struct intersection {
	MCNUM time;  	//time of the intersection
	Coords v;	//intersection point
	Coords normal;  //normal vector of the surface intersected
	short in_out;	//1 if the ray enters the volume, -1 otherwise
	short edge;	//1 if the intersection is on the boundary of the polygon, and error is possible
} intersection;

typedef struct polygon {
  MCNUM* p; //vertices of the polygon in adjacent order, this way : x1 | y1 | z1 | x2 | y2 | z2 ...
  int npol;  //number of vertex
  Coords normal;
} polygon;


//gives the normal vector of p
void normal(Coords* , polygon );

//scalar product
//#define scalar_prod(x1,y1,z1,x2,y2,z2) ( (x1)*(x2)+(y1)*(y2)+(z1)*(z2) )

//based on http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
//return 0 if the vertex is out
//	 1 if it is in
//	 -1 if on the boundary
int pnpoly(polygon , Coords );

//gives the intersection vertex between ray [a,b) and polygon p and its prametric value on (a b)
//http://geometryalgorithms.com/Archive/algorithm_0105/algorithm_0105.htm
int intersectPoly(intersection *, Coords , Coords , polygon );

/*reads the indexes at the beginning of the off file as this :
line 1  OFF
line 2  nbVertex nbFaces nbEdges
*/
long getBlocksIndex(char* ,  long* ,  long* ,  long* ,  long*  );


#define F(x,y,z,A,B,C,D)  ( (A)*(x) + (B)*(y) + (C)*(z) + (D) )


//gives the equations of 2 perpandicular planes of [ab]
void init_planes(Coords , Coords , MCNUM* , MCNUM* , MCNUM* , MCNUM *, MCNUM* , MCNUM* , MCNUM* );



#define sign(a) ( ((a)<0)?-1:((a)!=0) )


int clip_3D_mod(intersection* , Coords , Coords , Coords*, unsigned long, unsigned long*, unsigned long, Coords*);


int compare (void const *, void const *);


//given an array of intesction throw those which appear several times
//returns 1 if there is a possibility of error
int cleanDouble(intersection* , int* );

//given an array of intesction throw those which enter and exit in the same time
//Meaning the ray passes very close to the volume
//returns 1 if there is a possibility of error
int cleanInOut(intersection* , int* );

#endif

/* end of interoff-lib.h */
