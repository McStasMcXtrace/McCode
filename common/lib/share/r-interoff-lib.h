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
* Object File Format intersection header for McStas. Requires the qsort function.
*
* Such files may be obtained with e.g.
*   qhull < points.xyz Qx Qv Tv o > points.off
* where points.xyz has format:
*   3
*   <nb_points>
*   <x> <y> <z>
*   ...
* The resulting file should have its first line being changed from '3' into 'OFF'.
* It can then be displayed with geomview.
* A similar, but somewhat older solution is to use 'powercrust' with e.g.
*   powercrust -i points.xyz
* which will generate a 'pc.off' file to be renamed as suited.
*
*******************************************************************************/

#ifndef R_INTEROFF_LIB_H
#define R_INTEROFF_LIB_H "$Revision$"

#ifndef OFF_EPSILON
#define OFF_EPSILON 1e-13
#endif

#ifndef OFF_INTERSECT_MAX
#ifdef OPENACC
#define OFF_INTERSECT_MAX 100
#else
#define OFF_INTERSECT_MAX 1024
#endif
#endif

//#include <float.h>

#define N_VERTEX_DISPLAYED    200000

typedef struct r_intersection {
	MCNUM time;  	  //time of the intersection
	Coords v;	      //intersection point
	Coords normal;  //normal vector of the surface intersected
	short in_out;	  //1 if the ray enters the volume, -1 otherwise
	short edge;	    //1 if the intersection is on the boundary of the polygon, and error is possible
	unsigned long index; // index of the face
} r_intersection;

typedef struct r_polygon {
  MCNUM* p;       //vertices of the polygon in adjacent order, this way : x1 | y1 | z1 | x2 | y2 | z2 ...
  int npol;       //number of vertices
  #pragma acc shape(p[0:npol]) init_needed(npol)
  Coords normal;
} r_polygon;

typedef struct r_off_struct {
    long vtxSize;
    long polySize;
    long faceSize;
    Coords* vtxArray;
    #pragma acc shape(vtxArray[0:vtxSize]) init_needed(vtxSize)
    Coords* normalArray;
    #pragma acc shape(vtxArray[0:faceSize]) init_needed(faceSize)
    unsigned long* faceArray;
    #pragma acc shape(vtxArray[0:faceSize][0:polySize]) init_needed(faceSize,polySize)
    unsigned long* facepropsArray;
    #pragma acc shape(facepropsArray[0:faceSize]) init_needed(faceSize)
    char *filename;
    int mantidflag;
    long mantidoffset;
    intersection intersects[OFF_INTERSECT_MAX]; // After a call to r_off_intersect_all contains the list of intersections.
    int nextintersect;                 // 'Next' intersection (first t>0) solution after call to r_off_intersect_all
    int numintersect;               // Number of intersections after call to r_off_intersect_all
} r_off_struct;

/*******************************************************************************
* long r_off_init(  char *offfile, double xwidth, double yheight, double zdepth, r_off_struct* data)
* ACTION: read an OFF file, optionally center object and rescale, initialize OFF data structure
* INPUT: 'offfile' OFF file to read
*        'xwidth,yheight,zdepth' if given as non-zero, apply bounding box. 
*           Specifying only one of these will also use the same ratio on all axes
*        'notcenter' center the object to the (0,0,0) position in local frame when set to zero
* RETURN: number of polyhedra and 'data' OFF structure 
*******************************************************************************/
long r_off_init(  char *offfile, double xwidth, double yheight, double zdepth, 
                int notcenter, r_off_struct* data);

/*******************************************************************************
* int r_off_intersect_all(double* t0, double* t3, 
     Coords *n0, Coords *n3,
     unsigned long *fi0, unsigned long *fi3,
     double x, double y, double z, 
     double vx, double vy, double vz, 
     r_off_struct *data )
* ACTION: computes intersection of neutron trajectory with an object. 
* INPUT:  x,y,z and vx,vy,vz are the position and velocity of the neutron
*         data points to the OFF data structure
* RETURN: the number of polyhedra which trajectory intersects
*         t0 and t3 are the smallest incoming and outgoing intersection times
*         n0 and n3 are the corresponding normal vectors to the surface
*         data is the full OFF structure, including a list intersection type
*******************************************************************************/
#pragma acc routine
int r_off_intersect_all(double* t0, double* t3, 
     Coords *n0, Coords *n3,
     unsigned long *faceindex0, unsigned long *faceindex3,
     double x, double y, double z, 
     double vx, double vy, double vz, 
     r_off_struct *data );

/*******************************************************************************
* int r_off_intersect(double* t0, double* t3, 
     Coords *n0, Coords *n3,
     unsigned long *faceindex0, unsigned long *faceindex3,
     double x, double y, double z, 
     double vx, double vy, double vz, 
     r_off_struct data )
* ACTION: computes intersection of neutron trajectory with an object. 
* INPUT:  x,y,z and vx,vy,vz are the position and velocity of the neutron
*         data points to the OFF data structure
* RETURN: the number of polyhedra which trajectory intersects
*         t0 and t3 are the smallest incoming and outgoing intersection times
*         n0 and n3 are the corresponding normal vectors to the surface
*******************************************************************************/
#pragma acc routine
int r_off_intersect(double* t0, double* t3, 
     Coords *n0, Coords *n3,
     unsigned long *faceindex0, unsigned long *faceindex3,
     double x, double y, double z, 
     double vx, double vy, double vz, 
     r_off_struct data );

/*****************************************************************************
* int r_off_intersectx(double* l0, double* l3, 
     Coords *n0, Coords *n3,
     unsigned long *faceindex0, unsigned long *faceindex3,
     double x, double y, double z, 
     double kx, double ky, double kz, 
     r_off_struct data )
* ACTION: computes intersection of an xray trajectory with an object.
* INPUT:  x,y,z and kx,ky,kz, are spatial coordinates and wavevector of the x-ray
*         respectively. data points to the OFF data structure.
* RETURN: the number of polyhedra the trajectory intersects
*         l0 and l3 are the smallest incoming and outgoing intersection lengths
*         n0 and n3 are the corresponding normal vectors to the surface
*******************************************************************************/
#pragma acc routine
int r_off_x_intersect(double *l0,double *l3,
     Coords *n0, Coords *n3,
     unsigned long *faceindex0, unsigned long *faceindex3,
     double x,  double y,  double z, 
     double kx, double ky, double kz, 
     r_off_struct data );

/*******************************************************************************
* void r_off_display(r_off_struct data)
* ACTION: display up to N_VERTEX_DISPLAYED points from the object
*******************************************************************************/
void r_off_display(r_off_struct);

#endif

/* end of r-interoff-lib.h */
