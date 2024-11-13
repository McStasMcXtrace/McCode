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
#define PLANE_H

//Calculates signed distance of a point to a plane defined by its normal and point on plane
//returns positive value if point is at the normal vector side of the plane
//returns positive value if point is behind the normal vector side of the plane
//returns 0 if point is on the plane
double point_to_plane_signed_distance(Coords point, Coords plane_normal, Coords plane_point);
		
//calculate line-plane intersect
//input: position (line_p) and velocity (line_v), plane normal and point on plane
//output: user passes pointers to receive dt, dp between position line_p to intersect
//returns 1 if line intersects plane, 0 if not
int line_plane_intersect(Coords line_p, Coords line_v, Coords plane_normal, Coords plane_point, double maximum_on_plane_distance, 
						double *dt, Coords *d_intersect_point);

//rotate a plane around a rotation axis by an angle, right hand rule applies
//returns 1 if operation succeed, 0 if not
int rotate_plane(Coords plane_normal_in, Coords plane_point_in, Coords rot_axis, double angle_in_degree, Coords *plane_normal_out, Coords *plane_point_out);

//Find the vertex of three planes each defined by a normal and a point on the plane
//returns 1 if succeed, 0 if any two of the three faces are parallel
int getVertexOfThreePlanes(	Coords plane1_normal, Coords plane1_point, 
							Coords plane2_normal, Coords plane2_point, 
							Coords plane3_normal, Coords plane3_point, 
							Coords *vertex);

#endif //end of PLANE_H
