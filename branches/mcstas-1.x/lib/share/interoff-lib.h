/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2009, All rights reserved
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
* Object File Format intersection header for McStas.
*
*******************************************************************************/

#ifndef INTEROFF_LIB_H
#define INTEROFF_LIB_H "$Revision: 1.4 $"

#ifndef EPSILON
#define EPSILON 10e-13
#endif

//#include <float.h>

#define GEOMVIEW 0
#define buf 256
#define MAX_POL_SIZE 256
#define N_VERTEX_DISPLAYED 5000
#define MAX_INTERSECTION_SIZE 256

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

typedef struct off_struct {
    long vtxSize;
    long polySize;
    long faceSize;
    Coords* vtxArray;
    Coords* normalArray;
    unsigned long* faceArray;
} off_struct;

#define F(x,y,z,A,B,C,D)  ( (A)*(x) + (B)*(y) + (C)*(z) + (D) )
#ifndef sign
#define sign(a) ( ((a)<0)?-1:((a)!=0) )
#endif

long off_init(char*, double, double, double, off_struct*);

int  off_intersect(double*, double*, double, double, double, double, double, double, off_struct);

void off_display(off_struct);

#endif

/* end of interoff-lib.h */
