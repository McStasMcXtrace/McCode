/****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: plane.c
*
* %Identification
* Written by: Hal Lee
* Date: July, 2023
* Origin: ESS
* Release: McStas 2.7x, 3.1x
* Version: $Revision$
*
* plane geometry and intersection functions
*
* Usage: within SHARE
* %include "plane"
*
****************************************************************************/

#ifndef PLANE_H
#include "plane.h"
#endif

#ifndef PLANE_C
#define PLANE_C

//Calculates signed distance of a point to a plane defined by its normal and point on plane
//returns positive value if point is at the normal vector side of the plane
//returns positive value if point is behind the normal vector side of the plane
//returns 0 if point is on the plane
double point_to_plane_signed_distance(Coords point, Coords plane_normal, Coords plane_point) {
	//+ve = point at +ve side of normal 
	//  0 = point on plane 
	//-ve = point at -ve side of normal 

	double dd = -coords_sp(plane_normal, coords_sub(plane_point, point));

	return dd;

}

//calculate line-plane intersect
//input: position (line_p) and velocity (line_v), plane normal and point on plane, maximum normal distance from plane to be considered on plane
//output: user passes pointers to receive dt, dp, point_direction between position line_p to intersect
//returns 1 if line intersects plane, 0 if not
int line_plane_intersect(Coords line_p, Coords line_v, Coords plane_normal, Coords plane_point, double maximum_on_plane_distance, 
		double *dt, Coords *d_intersect_point) 
{
	
	double max_on_plane_d = fabs(maximum_on_plane_distance) > DBL_EPSILON ? fabs(maximum_on_plane_distance) : DBL_EPSILON;
	
	double n_dot_v = coords_sp(plane_normal, line_v); 

	if (fabs(n_dot_v) < DBL_EPSILON) {
		//v parallel to plane
		return 0;
	}
	
	double n_dot_dp = point_to_plane_signed_distance(line_p, plane_normal, plane_point);
	
	if (fabs(n_dot_dp) <= max_on_plane_d) {
		(*d_intersect_point).x = 0; 
		(*d_intersect_point).y = 0; 
		(*d_intersect_point).z = 0; 
		*dt = 0; 
	}
	else {
		(*d_intersect_point).x = line_v.x * (-n_dot_dp) / n_dot_v; 
		(*d_intersect_point).y = line_v.y * (-n_dot_dp) / n_dot_v; 
		(*d_intersect_point).z = line_v.z * (-n_dot_dp) / n_dot_v; 
		*dt = -n_dot_dp / n_dot_v; 
	}
}

//rotate a plane around a rotation axis by an angle, right hand rule applies
//returns 1 if operation succeed, 0 if not
int rotate_plane(Coords plane_normal_in, Coords plane_point_in, Coords rot_axis, double angle_in_degree, Coords *plane_normal_out, Coords *plane_point_out) {
	if (coords_len(rot_axis) < DBL_EPSILON)
		return 0;
	coords_norm(&(rot_axis));
	rotate( plane_normal_out->x, plane_normal_out->y, plane_normal_out->z,
			plane_normal_in.x, plane_normal_in.y, plane_normal_in.z, 
			angle_in_degree * DEG2RAD, 
			rot_axis.x, rot_axis.y, rot_axis.z);
	rotate( plane_point_out->x, plane_point_out->y, plane_point_out->z,
			plane_point_in.x, plane_point_in.y, plane_point_in.z, 
			angle_in_degree * DEG2RAD, 
			rot_axis.x, rot_axis.y, rot_axis.z);
	return 1;
}

/*********/
/* Tools */
/*********/
//Calculate the determinant of a 3x3 matrix
double det3x3(	double a11, double a12, double a13,
				double a21, double a22, double a23,
				double a31, double a32, double a33) 
{
	return    a11 * (a22 * a33 - a32 * a23)
			+ a21 * (a32 * a13 - a12 * a33)
			+ a31 * (a12 * a23 - a22 * a13);
}

//Find the vertex of three planes each defined by a normal and a point on the plane
//returns 1 if succeed, 0 if any two of the three faces are parallel
int getVertexOfThreePlanes(Coords n1,Coords p1, Coords n2,Coords p2, Coords n3,Coords p3, Coords *v) {
	double denom = det3x3(	n1.x, n1.y, n1.z, 
							n2.x, n2.y, n2.z, 
							n3.x, n3.y, n3.z);
	if (fabs(denom) < DBL_EPSILON) return 0;
	double 	d1 = coords_sp(n1,p1),
			d2 = coords_sp(n2,p2),
			d3 = coords_sp(n3,p3);
	(*v).x =  det3x3(	d1, n1.y, n1.z, 
						d2, n2.y, n2.z, 
						d3, n3.y, n3.z)/denom;
	(*v).y =  det3x3(	n1.x, d1, n1.z, 
						n2.x, d2, n2.z, 
						n3.x, d3, n3.z)/denom;
	(*v).z =  det3x3(	n1.x, n1.y, d1, 
						n2.x, n2.y, d2, 
						n3.x, n3.y, d3)/denom;
	return 1;
}

#endif //end of PLANE_C
/* end of plane.c */
