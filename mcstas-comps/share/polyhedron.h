/****************************************************************************************
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: polyhedron.c
*
* %Identification
* Written by: Hal Lee
* Date: July, 2023
* Origin: ESS
* Release: McStas 2.7x, 3.1x
* Version: $Revision$
*
* Input a closed convex polyhedron with the surface normals pointing outwards.
* Initial formation much have point (0,0,0) on or inside the polyhedron.
* Calculates the intersects of neutron flight path through the polyhedron
* Such a geometry has up to two intersects.
* 
* The number of faces are limited to 50, otherwise the program crashes
*
* Usage: within SHARE
* %include "polyhedron"
*
* Example usage of FaceVertexIndices in MCDISPLAY
* //Declared in DECLARE: 
* //	Polyhedron poly;
* //	int i,j,n,m, k1,k2,*ifvi1; 
* //	Coords *vp1;
* MCDISPLAY
* %{
* 	n=poly.nf;
* 	vp1=poly.vp;
* 	for (i=0; i<n; i++) {
* 		m=poly.afvi[i].nfvi;
* 		ifvi1= poly.afvi[i].ifvi;
* 		k1 = ifvi1[0]; 
* 		for (j=1; j<m; j++) {
* 			k2 = ifvi1[j];
* 			line(vp1[k1].x, vp1[k1].y, vp1[k1].z,  vp1[k2].x, vp1[k2].y, vp1[k2].z);
* 			k1 = k2;
* 		}
* 		k2 = ifvi1[0]; 
* 		line(vp1[k1].x, vp1[k1].y, vp1[k1].z,  vp1[k2].x, vp1[k2].y, vp1[k2].z);
* 	}
* }%
*
****************************************************************************************/
#ifndef POLYHEDRON_H
#define POLYHEDRON_H

typedef struct PolyhedronIndexValuePair {
	int index;
	double value;
} PolyhedronIndexValuePair;

typedef struct LinePolyhedronIntersect { 
	double*a_lp_dt; 
	Coords*a_lp_dp;  
	double*a_lp_t;  
	Coords*a_lp_p; 
	int*a_lp_pn;  
	int*a_lp_ty;  
	PolyhedronIndexValuePair *idp;
} LinePolyhedronIntersect; 

typedef struct FaceVertexIndices { 
	int nfvi; //number in an array of indices of vertices forming a face boundary
	int *ifvi; //array of indices of vertices forming a face boundary
}FaceVertexIndices;

typedef struct Polyhedron{ 
	int nf; //number of faces
	Coords *fn; //face normal 
	Coords *fp; //face point

	int nv; //number of vertices
	Coords *vp; //vertex point array
	
	FaceVertexIndices *afvi; //array of arrays of indices of vertices forming each face boundary 
	
	LinePolyhedronIntersect lpi; //LinePolyhedronIntersect

} Polyhedron; 

//plane normal designation, before rotation of polyhedron
typedef enum PolyhedronPlane { 
	polyhedron_plus_y_plane = 0, //vertical up side +y, do not change this - used in supermirror calculation
	polyhedron_minus_y_plane = 1, //vertical down side -y, do not change this - used in supermirror calculation
	polyhedron_minus_z_plane = 2, //incident beam side -z
	polyhedron_plus_z_plane = 3, //exit beam side +z
	polyhedron_plus_x_plane = 4, //horizontal +x side
	polyhedron_minus_x_plane = 5 //horizontal -x side
} PolyhedronPlane;



/**********************************************************************************/
/* Form a closed convex polyhedron with surfaces specified in normal and a point. */
/* each surface norm points outwards from the inside of polyhedron.               */
/* returns the number of faces if the polyhedron is formed.                       */
/* returns 0 if error.                                                            */
/**********************************************************************************/
int form_polyhedron(int num_faces, Coords *face_normal, Coords *face_point, 
					Polyhedron*polyhedron);


/**********************************************************************************************************/
/* check a point's position with respect to the surfaces of a polyhedron                                  */
/* on plane = |distance| < maximum_on_plane_distance                                                      */
/* above plane, i.e. outside polyhedron = signed distance > DBL_EPSILON                                   */
/* below plane = signed distance < -DBL_EPSILON                                                           */
/* return 1 if succeed, 0 if fail.                                                                        */
/* INPUT:                                                                                                 */
/* lint_p(pointer)      [m,m,m]        position of neutron                                                */
/* lint_v(pointer)      [m/s,m/s,m/s]  velocity of neutron                                                */
/* polyhedron           [struct Polyhedron]  pointer to polydegron struct                                 */
/* maximum_on_plane_distance [m] max distance normal to plane which a point is considered to be on plane  */
/* skip_plane_number    [1]  array of index of planes to skip                                             */
/* num_skip_plane       [1]  number of planes in the array  of index of planes to skip                    */
/* OUTPUT:                                                                                                */
/* optional: pass integer pointers to get the following values                                            */ 
/*           send pointer address = 0 to skip                                                             */
/* n_on_plane: number of surfaces that the point is on                                                    */
/* n_behind plane: number of surfaces that the point is behind                                            */
/* n_above plane: number of surfaces that the point is above                                              */
/* is_on_inside_outside_polyhedron:                                                                       */
/*    1: point is on polyhedron surface                                                                   */
/*    0: point is inside polyhedron                                                                       */
/*   -1: point is outside polyhedron                                                                      */
/* is_crossing_plane: 1 = point is on a plane, 0 = not                                                    */
/* is_crossing_edge: 1 = point is on an edge, 0 = not                                                     */
/* is_crossing_vertex: 1 = point is on a vertex, 0 = not                                                  */
/* is_flying_on_plane: 1 = point is flying on a plane, 0 = not                                            */
/* is_flying_on_edge: 1 = point is flying on a edge, 0 = not                                              */
/**********************************************************************************************************/
int check_point_with_respect_to_polyhedron(Coords *point_in, Coords *velocity_in, Polyhedron *polyhedron, int num_skip_plane, int*skip_plane_number, double maximum_on_plane_distance, 
											int *n_on_plane, int *n_behind_plane, int *n_above_plane, 
											int *is_on_inside_outside_polyhedron,  
											int *is_crossing_plane, int *is_crossing_edge, int *is_crossing_vertex, 
											int *is_flying_on_plane, int *is_flying_on_edge); 

/**********************************************************************************************************************************/
/* line intersect polyhedron calculation                                                                                          */
/* return 1 if succeed, otherwise 0                                                                                               */
/* INPUT:                                                                                                                         */
/*   lint_t               [s]            time of neutron                                                                          */
/*   lint_p               [m,m,m]        position of neutron                                                                      */
/*   lint_v               [m/s,m/s,m/s]  velocity of neutron                                                                      */
/*   polyhedron           [struct Polyhedron]  pointer to polydegron struct                                                       */
/*   maximum_on_plane_distance [m] max distance normal to plane which a point is considered to be on plane                        */
/*   skip_plane_number    [1]  array of index of planes to skip, pass 0 in if num_skip_plane=0                                    */
/*   num_skip_plane       [1]  number of planes in the array  of index of planes to skip                                          */
/*   last_intersect_time  [s]  last intersect time, skip intersect same as last intersect time, point, plane                      */
/*   last_intersect_point [s]  last intersect point, skip intersect same as last intersect time, point, plane                     */
/*   last_intersect_plane [s]  last intersect plane, skip intersect same as last intersect time, point, plane                     */
/*   num_intersect        [1]  value = size of the output arrays below, value == 0: allocate memory to hold all intersects        */
/* OUPUT: pass the address pointer to receive output, if not use, pass 0.                                                         */
/*   num_intersect          [1]              number of intersect with intersect dtime > 0, if input value=0, output=size of array */
/*   output variables below are optional, if not used, pass 0 to the variable's pointer).                                         */
/*   intersect_dtime        [array of s]     array of intersect time difference                                                   */
/*   intersect_dpoint       [array of m,m,m] array of intersect position difference                                               */
/*   intersect_time         [array of s]     array of intersect time                                                              */
/*   intersect_point        [array of m,m,m] array of intersect position                                                          */
/*   intersect_plane_number [array of 1]     array of intersect plane                                                             */
/*   intersect_type         [array of 1]     array of intersect type, 1=x plane, 2=x edge, 3=x vertex, 4=on plane, 5=on edge      */
/* Note:                                                                                                                          */
/*   To avoid getting stuck in infinite loop at two polyhedron objects with at least one touching edge,                           */
/*   neutron incident from outside a polyhedron must have at least 2 intersects.                                                  */ 
/**********************************************************************************************************************************/
int line_polyhedron_intersect(double line_t, Coords line_p, Coords line_v, 
		Polyhedron *polyhedron, double maximum_on_plane_distance, int num_skip_plane, int*skip_plane_number,
		double last_intersect_time, Coords last_intersect_point, int last_intersect_plane, 
		int*num_intersect, double*intersect_dtime, Coords*intersect_dpoint, double*intersect_time, Coords*intersect_point, int*intersect_plane, int*intersect_type);

/****************************************************************************/
/* Rotate a polyhedron about a rotate axis by an angle, right hand rule     */
/* pass a Rotation pointer to receive the rotation matrix, 0 if not needed. */
/* returns 1 = no error, 0 = error                                          */
/****************************************************************************/
int rotate_polyhedron_about_axis (Polyhedron*polyGeo, Coords axis, double angle, Rotation*rot);

/*****************************************************/
/* Rotate a polyhedron by applying a rotation matrix */
/* returns 1 = no error, 0 = error                   */
/*****************************************************/
int rotate_polyhedron (Polyhedron*polyGeo, Rotation rot);

/***************************************************/
/* Translate a polyhedron by a displacement vector */
/* returns 1 = no error, 0 = error                 */
/***************************************************/
int translate_polyhedron (Polyhedron*polyGeo, Coords displace);

/****************************/
/* free up memory allocated */
/****************************/
void empty_polyhedron(Polyhedron*polyhedron);

#endif //end of POLYHEDRON_H
/* end of polyhedron.h */


