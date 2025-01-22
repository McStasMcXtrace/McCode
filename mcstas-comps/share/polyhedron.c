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
* Example code using FaceVertexIndices in MCDISPLAY
* %{
* 	//Declared in DECLARE: 
* 	//	Polyhedron poly;
* 	//	int i,j,n,m, k1,k2,*ifvi1; 
* 	//	Coords *vp1;
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
#include "polyhedron.h"
#endif

%include "plane"

#ifndef POLYHEDRON_C
#define POLYHEDRON_C

#ifndef INDETERMINED
#define F_INDETERMINED -FLT_MAX
#define I_INDETERMINED -INT_MAX
#define INDETERMINED
#endif

/*********/
/* Tools */
/*********/
//Sort an array of index-value pairs according to the values
//sorting algorithm uses qsort. From 
//https://stackoverflow.com/questions/36714030/c-sort-float-array-while-keeping-track-of-indices
//Usage: 
//Declare a pointer to an array of the index-value pairs: PolyhedronIndexValuePair *indexValuePair;
//Allocate memory and fill the indexValuePair array with number_of_pairs of index-value pairs
//Call qsort function: qsort(indexValuePair, number_of_pairs, sizeof(indexValuePair[0]), polyhedron_qsort_compare_func);
//The sorted position of an index-value pair in the array is numbered in index. 
//Note that the original order of the index-value pair array is not changed. 

int polyhedron_qsort_compare_func(const void *index_value_pair_pointer1, const void *index_value_pair_pointer2) {
	PolyhedronIndexValuePair *index_value_pair1 = (PolyhedronIndexValuePair *)index_value_pair_pointer1;
    PolyhedronIndexValuePair *index_value_pair2 = (PolyhedronIndexValuePair *)index_value_pair_pointer2;
	if ((*index_value_pair1).value < (*index_value_pair2).value)
		return -1;
    if ((*index_value_pair1).value > (*index_value_pair2).value)
		return 1;
	else 
		return 0;
}

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
int check_point_with_respect_to_polyhedron(Coords *point_in, Coords *velocity_in, Polyhedron *polyhedron_in, int num_skip_plane, int*skip_plane_number, double maximum_on_plane_distance, 
											int *n_on_plane, int *n_behind_plane, int *n_above_plane, 
											int *is_on_inside_outside_polyhedron,  
											int *is_crossing_plane, int *is_crossing_edge, int *is_crossing_vertex, 
											int *is_flying_on_plane, int *is_flying_on_edge) 
{
	
	if (!point_in) {
		return 0;
	}
	
	if (polyhedron_in == 0) {
		return 0;
		}
	
	Coords point = *point_in;
	Polyhedron*a = polyhedron_in;
	int m_on_plane = 0, m_behind_plane = 0, m_above_plane = 0, m_on_inside_outside_polyhedron,
		b_crossing_plane = 0, b_crossing_edge = 0, b_crossing_vertex = 0, b_flying_on_plane = 0, b_flying_on_edge = 0;
	double max_d = DBL_EPSILON > fabs(maximum_on_plane_distance) ? DBL_EPSILON : fabs(maximum_on_plane_distance);
	int i,j,n,*m, skip; double dd,len; Coords *vp, dp1, dp2, dp1_cross_dp2, dp1_dot_dp2; 
	
	for (i = 0; i < a->nf; i++) {
	
		if (num_skip_plane > 0 && skip_plane_number != 0) {
			skip = 0;
			for (j = 0; j < num_skip_plane; j++) {
				if (i == skip_plane_number[j]) {
					skip = 1;
					break;
				}
			}
			if (skip) {
				continue;
			}
		}
		
		//calculate signed distance from point to plane
		dd = point_to_plane_signed_distance(point, a->fn[i], a->fp[i]);
		
		if (dd < -max_d) {
			//behind plane
			++m_behind_plane;
		}
		else if (dd > max_d) {
			++m_above_plane;
		}
		else {
			//point is on plane 
			++m_on_plane;
			if (a->afvi == 0) {
				//form_polyhedron in progress, not checking vertices yet. 
				continue;
			}
			
			//check if point already crossing plane or crossing edge or flying on plane or flying on edge
			//only when point crossing vertex does it cross multiple planes and potentially flying on edge
			if ( b_crossing_plane != 0 || b_crossing_edge != 0 || b_flying_on_plane != 0 || b_flying_on_edge != 0) {
				//skip checking if point already crossing plane or crossing edge or flying on plane or flying on edge,
				//continue to determine number behind, above, on plane
				continue;
			}
			//either not yet determine which of the 5 types it is or point is crossing vertex on other planes,
			//continue below 			
			
			if (b_crossing_vertex == 0) {
				//not already crossing vertex, i.e. not yet determine, then check if point is flying on this plane
				b_crossing_plane = 1; //default beginning setting
				if (velocity_in != 0) {
					if (fabs(coords_sp(*velocity_in, a->fn[i])) < max_d) {
						b_crossing_plane = 0;
						b_flying_on_plane = 1;
					}
				}
			}
			//check if point is on an edge or vertex on this plane
			n = (a->afvi[i]).nfvi;
			if (n > 1) {
				vp = a->vp;
				m = (a->afvi[i]).ifvi;
				skip = 0;
				for (j = 0; j < n; j++) {
					//loop through vertices associated with plane
					if (j == 0) {
						dp1 = coords_sub(vp[m[0]], point); 
					}
					else {
						dp1 = dp2;
					}
					if (j < n-1) {
						dp2 = coords_sub(vp[m[j+1]], point);
					}
					else { 
						dp2 = coords_sub(vp[m[0]], point);
					}
					
					if (coords_len(dp1) <= max_d || coords_len(dp2) <= max_d ) {
						//point is on a vertex on this plane
						b_crossing_plane = 0;
						b_flying_on_plane = 0;
						b_crossing_vertex = 1;
						skip = 1; //at the end, skip the rest
					}
					else {
						//not on either vertex, check if point is crossing an edge
						coords_norm(&dp1);
						coords_norm(&dp2);
						if (coords_len(coords_xp(dp1,dp2)) < max_d) { 
							//dp1 and dp2 are parallel 
							if (coords_sp(dp1,dp2) <= 0) { 
								//point is on edge connecting vp[m[j]] & vp[m[j+1]]
								b_crossing_plane = 0;
								b_flying_on_plane = 0;
								b_crossing_edge = 1;
								skip = 1; //at the end, skip the rest
							}
						}
					}
					
					if (velocity_in != 0) {
						if (b_crossing_vertex == 1 || b_crossing_edge == 1) {
							//check if point is flying on an edge
							if (coords_len(coords_xp(*velocity_in,dp1)) <=max_d || coords_len(coords_xp(*velocity_in,dp2)) <=max_d) {
								b_crossing_vertex = 0;
								b_crossing_edge = 0;
								b_flying_on_edge = 1; 
								skip = 1; //at the end, skip the rest
							}
						}
					}
					
					if (skip == 1) {
						break;
					}
				}
			}
		}
	}
	
	if (m_above_plane != 0) { 
		m_on_inside_outside_polyhedron = -1; //point outside polyhedron
	} 
	else if (m_on_plane != 0) { 
		m_on_inside_outside_polyhedron = 1; //point on polyhedron
	} 
	else { 
		m_on_inside_outside_polyhedron = 0; //point inside polyhedron
	} 
	
	if (n_on_plane != 0) *n_on_plane = m_on_plane; 
	if (n_behind_plane != 0) *n_behind_plane = m_behind_plane; 
	if (n_above_plane != 0) *n_above_plane = m_above_plane; 
	if (is_on_inside_outside_polyhedron != 0) *is_on_inside_outside_polyhedron = m_on_inside_outside_polyhedron; 
	if (is_crossing_plane != 0) *is_crossing_plane = b_crossing_plane; 
	if (is_crossing_edge != 0) *is_crossing_edge = b_crossing_edge; 
	if (is_crossing_vertex != 0) *is_crossing_vertex = b_crossing_vertex; 
	if (is_flying_on_plane != 0) *is_flying_on_plane = b_flying_on_plane; 
	if (is_flying_on_edge != 0) *is_flying_on_edge = b_flying_on_edge; 
	
	return 1;
}

/*********************************************************************************/
/* Form a closed convex polyhedron with surfaces specified in normal and a point */
/* each surface norm points outwards from the inside of polyhedron.              */
/* returns the number of faces if the polyhedron is formed.                      */
/* returns 0 if error.                                                           */
/*********************************************************************************/
int form_polyhedron(int nf_in, Coords *fn_in, Coords *fp_in, Polyhedron *polyhedron_in) { 

	int i,j,k,l,m,n,keep;
	
	if (nf_in <= 0 || fn_in == 0 || fp_in == 0 || polyhedron_in == 0) {
		return 0; 
	}
	
	///////////////////////////////////////////////
	// Get polyhedron faces from supplied planes //
	///////////////////////////////////////////////
	Polyhedron*a = polyhedron_in;
	int nf = 0;
	Coords *fn,*fp, nn;
	fn = calloc(nf_in, sizeof(Coords));
	fp = calloc(nf_in, sizeof(Coords));

	//loop through nf_in
	for (i = 0; i < nf_in; i++) {
		//fn_in[i] must have non-zero length
		if (coords_len(fn_in[i]) < DBL_EPSILON) {
			//error, skip this one
		}
		else {
			//normalise fn_in --> nn
			nn = fn_in[i];
			coords_norm(&nn);
			//check if fn_in[i], fp_in[i] is a duplicated plane
			keep = 1;
			if (nf > 0) {
				//loop through nf
				for (j = 0; j < nf; j++) {
					if (coords_len(coords_xp(nn, fn[j])) > DBL_EPSILON) {
						//plane normals are not parallel (even if it can be p[i] == p[j])
						continue;
					}
					if (fabs(coords_sp(nn,coords_sub(fp_in[i],fp[j]))) > DBL_EPSILON) {
						//Normals are parallel, but points are not on the same plane 
						continue;
					}
					//same face, skip one
					keep = 0;			
					break;
				}
			}
			
			if (keep) {
				fn[nf] = nn;
				fp[nf] = fp_in[i];
				++nf;
			}
		}
	}
	
	if (nf < 4) {
		return 0;
	}
	
	a->nf = nf;
	a->fn = calloc(nf, sizeof(Coords));
	a->fp = calloc(nf, sizeof(Coords));
	for (i = 0; i < nf; i++) {
		a->fn[i] = fn[i];
		a->fp[i] = fp[i];
	}
	
	free(fn);
	free(fp);
	//note: do not zero nf, useful below.
	
	////////////////////////
	// calculate vertices //
	////////////////////////
	int nv, vn;
	nv = (int)(2 + a->nf * (a->nf-3)/2); //upper limit of number of vertices: an edge is the intersection of 2 faces, then Euler's V-E+F=2
	
	Coords *vp, intersection_point;
	int *nfvi,**ifvi;
	double d1,d2,d3,denom;
	
	vp = calloc(nv, sizeof(Coords));
	nfvi = calloc(nf, sizeof(int));
	ifvi = calloc(nf, sizeof(int*));
	for (i = 0; i < nf; i++) {
		ifvi[i] = calloc(nv, sizeof(int));
	}
	
	int ii, iii[3], iiii, *eliminate_list, is_on_inside_outside_polyhedron;
	Coords l_to_n, l_to_nv, n_to_nv;
	nv = 0; vn = 0;
	// Check each three faces for intersections
	for (i = 0; i < a->nf - 2; i++) {
		for (j = i + 1; j < a->nf - 1; j++) {
			for (k = j + 1; k < a->nf; k++) {
				
				if (getVertexOfThreePlanes(a->fn[i],a->fp[i], a->fn[j],a->fp[j], a->fn[k],a->fp[k], &intersection_point) == 0) {
					// Planes are parallel or nearly parallel
					continue;
				}
			
                // Check if the intersection point is on the polyhedron
				check_point_with_respect_to_polyhedron(&intersection_point, 0, 
														a, 0, 0, 0, 
														0, 0, 0, 
														&is_on_inside_outside_polyhedron, 0, 0, 0, 0, 0); 

				if (is_on_inside_outside_polyhedron != 1) {
					continue;
				}

				vn = nv; //vn = new index of vertex nv (nv = current number of vertices)
				//Check if vertex already exist in the list
				keep = 1;
				if (nv > 0) {
					for (l = 0; l < nv; l++) {
						if (coords_len(coords_sub(intersection_point, vp[l])) < DBL_EPSILON) { 
							vn = l; //vertex already exist with vertex number l, change vn to l
							keep = 0;
							break;
						}
					}
				}

				if (keep) {
					//Store vertex
					vp[nv] = intersection_point;
					++nv;
				}

				//Check association of the new vertex with each of the planes i,j,k, respectively
				iii[0] = i; iii[1] = j; iii[2] = k;
				for (iiii = 0; iiii < 3; iiii++) {
					ii = iii[iiii]; 

					//Check first if the vertex index is already associated with the plane
					keep = 1;
					m = nfvi[ii];
					if (m > 0) {
						for (l = 0; l < m; l++) {
							if (ifvi[ii][l] == vn) {
								keep = 0;
								break;
							}
						}
					}
					if (keep == 1) {
						//Add new vertex to the association list, then do further checking
						ifvi[ii][m] = vn;
						++m;
						nfvi[ii] = m;
					}
					else {
						//vertext is already in the list of vertices associate with the plane. Skip to check association with next plane.
						continue;
					}
					
					//Check if the new vertex and any two other vertices associating with the plane are colinear
					if (m >= 3 && keep) {
						//Now start checking
						eliminate_list = (int *)calloc(m, sizeof(int)); //initialise with 0 = not eliminate assocated vertex reference
						for (l = 0; l < m-2; l++) { //loop through vertices other than the last two since each check involves 3 different vertices
							if (eliminate_list[m-1]) {
								//association with new vertext eliminated. Skip to check next plane.
								keep = 0;
								break;
							}
							if (eliminate_list[l]) {
								//association with vertext l already eliminated, check next vertex
								continue;
							}
							for (n = l + 1; n < m-1; n++) { //loop through vertices that is neither vertex l nor the last vertex - each check requires 3 different vertices
								l_to_nv = coords_sub( vp[ifvi[ii][m-1]], vp[ifvi[ii][l]] );
								n_to_nv = coords_sub( vp[ifvi[ii][m-1]], vp[ifvi[ii][n]] );
								if (coords_len(coords_xp(l_to_nv, n_to_nv)) >= DBL_EPSILON) {
									//The 3 points are non-colinear, continue to check the next point
									continue;
								}
								//The 3 points are colinear, check which one is the middle point to eliminate association
								if (coords_sp(l_to_nv, n_to_nv) < 0) {
									//new point is the middle point, eliminate association with the new point, stop checking
									eliminate_list[m-1] = 1;
									break;
								}
								l_to_n = coords_sub( vp[ifvi[ii][n]], vp[ifvi[ii][l]] );
								if (coords_sp(l_to_n, l_to_nv) < 0) {
									//point l is the middle point, eliminate point l association, stop checking point l against point n
									eliminate_list[l] = 1;
									break;
								}
								else
								{
									//point n is the middle point, eliminate point n association, continue to next point n
									eliminate_list[n] = 1;
								}
							}
						}
						//update list of vertex numbers associated with planes 
						if (keep) {
							for (l = 0, n = 0; l < m; l++) {
								if (eliminate_list[l]) {
									continue;
								}
								ifvi[ii][n]=ifvi[ii][l]; 
								++n;
							}
							nfvi[ii] = n;
						}
						free(eliminate_list);
					}
				}
			}
		}
	}

	if (nv < 4) {
		free(vp);
		for (i = 0; i < nf; i++) {
			if (ifvi[i]) {
				free(ifvi[i]);
			}
		}
		free(ifvi);
		free(nfvi);
		return 0;
	}
	
	a->nv = nv;
	if (nv > 0) {
		a->vp = calloc(nv, sizeof(Coords));
		for (i = 0; i < nv; i++) {
			a->vp[i] = vp[i];
		}
	}
	free(vp);
	//do not zero nv, useful below.
	
	////////////////////////////////////////////////////////
	// Obtain sequences of vertices along face boundaries //
	////////////////////////////////////////////////////////
	Coords p0, dp01, dp0n;
	double l1,l2,cosA;
	
	a->afvi = calloc(nf, sizeof(FaceVertexIndices));
	
	for (i = 0; i < nf; i++) {
		a->afvi[i].nfvi = nfvi[i];
		if (nfvi[i] > 0) {
			//Draw a line between point 0 vertex and point 1 vertex,
			//Find cosine of the angles between this line and other lines from vertex 0 to other vertices. 
			//Sort the points according to the cosine values.
			//The order of the vertices will go along the edge of the polyhedron face
			PolyhedronIndexValuePair *iA; 
			iA = calloc(nfvi[i], sizeof(PolyhedronIndexValuePair));
			
			k = ifvi[i][0];
			iA[0].index = k;
			iA[0].value = -2; //ensure the starting point stays the first point. 
			p0 = a->vp[k];
			k = ifvi[i][1];
			dp01 = coords_sub(a->vp[k], p0); //reference line is between the first & second point
			l1 = coords_len(dp01);
			iA[1].index = k;
			iA[1].value = 1; //angle between the line from first to second point and the reference line is 0, i.e. cosA = 1  
			
			for (j = 2; j < nfvi[i]; j++) {
				k = ifvi[i][j];
				iA[j].index = k;
				
				dp0n = coords_sub(a->vp[k], p0);
				l2 = coords_len(dp0n);
				iA[j].value = coords_sp(dp01, dp0n)/l1/l2;
			}
	
			//sort the sequence according to iA[j].value, 
			//the order of iA[j] is rearranged as a result
			//the resulting ordered index are in iA[j].index
			qsort(iA, nfvi[i], sizeof(iA[0]), polyhedron_qsort_compare_func);
			
			//store the face vertext indices in the correct order in the array of face vertex indices
			a->afvi[i].ifvi = calloc(nfvi[i],sizeof(int));
			
			for (j = 0; j < nfvi[i]; j++) {
				a->afvi[i].ifvi[j] = iA[j].index;
			}
			
			free(iA);
		}
	}
	
	for (i = 0; i < nf; i++) {
		if (ifvi[i]) {
			free(ifvi[i]);
		}
	}
	free(ifvi);
	free(nfvi);

	//////////////////
	// Final output //
	//////////////////

	a->lpi.a_lp_dt=calloc(a->nf,sizeof(double)); //intersect dtime
	a->lpi.a_lp_dp=calloc(a->nf,sizeof(Coords)); //intersect dspace
	a->lpi.a_lp_t =calloc(a->nf,sizeof(double)); //intersect time absolute
	a->lpi.a_lp_p =calloc(a->nf,sizeof(Coords)); //intersect position absolute
	a->lpi.a_lp_pn=calloc(a->nf,sizeof(int));  //intersect plane number
	a->lpi.a_lp_ty=calloc(a->nf,sizeof(int));  //intersect type
	a->lpi.idp = calloc(a->nf, sizeof(PolyhedronIndexValuePair)); //for sorting intersect according to point-plane distance

	return a->nf;

}

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
/* OUPUT:                                                                                                                         */
/*   num_intersect          [1]              number of intersect with intersect dtime > 0, if input value=0, output=size of array */
/*   output variables below are optional, if not used, pass 0 to the variable's pointer).                                         */
/*   intersect_dtime        [array of s]     array of intersect time difference                                                   */
/*   intersect_dpoint       [array of m,m,m] array of intersect position difference                                               */
/*   intersect_time         [array of s]     array of intersect time                                                              */
/*   intersect_point        [array of m,m,m] array of intersect position                                                          */
/*   intersect_plane [array of 1]     array of intersect plane                                                             */
/*   intersect_type         [array of 1]     array of intersect type, 1=x plane, 2=x edge, 3=x vertex, 4=on plane, 5=on edge      */
/* Note:                                                                                                                          */
/*   To avoid getting stuck in infinite loop at two polyhedron objects with at least one touching edge,                           */
/*   neutron incident from outside a polyhedron must have at least 2 intersects.                                                  */ 
/**********************************************************************************************************************************/
int line_polyhedron_intersect(double line_t, Coords line_p, Coords line_v, 
		Polyhedron *polyhedron_in, double maximum_on_plane_distance, 
		int num_skip_plane, int*skip_plane_number, //use 0 for int*skip_plane_number if num_skip_plane=0
		double last_intersect_time, Coords last_intersect_point, int last_intersect_plane, 
		//int*num_intersect below:
		//INPUT: *num_intersect<=0: allocate output array memory; *num_intersect>0: *num_intersect=size of output array
		//OUTPUT: number of intersects found with intersect_dtime>0. if input value==0, also output array size
		int*num_intersect, 
		//in the following, use 0 for variables that is not used:
		double*intersect_dtime, Coords*intersect_dpoint, double*intersect_time, Coords*intersect_point, int*intersect_plane, int*intersect_type
		) 
{

	if (polyhedron_in == 0) {
		return 0;
	}

	Polyhedron*a = polyhedron_in;

	if (a->nf <= 0) {
		return 0;
	}
	
	int n_intersect_found = 0;
	
	int i,j, is_on_inside_outside_polyhedron, is_crossing_plane=0, is_crossing_edge=0, is_crossing_vertex=0, is_flying_on_plane=0, is_flying_on_edge=0, skip; 
	
	double max_d = DBL_EPSILON > fabs(maximum_on_plane_distance) ? DBL_EPSILON : fabs(maximum_on_plane_distance);
	
	double lp_dt; //intersect dtime
	Coords lp_dp; //intersect dspace
	double lp_t; //intersect time absolute
	Coords lp_p; //intersect position absolute


	double *dtime = (double*)malloc(a->nf * sizeof(double));
	Coords *dpoint = (Coords*)malloc(a->nf * sizeof(Coords));
	double *time = (double*)malloc(a->nf * sizeof(double));
	Coords *point = (Coords*)malloc(a->nf * sizeof(Coords));
	int *plane_number = (int*)malloc(a->nf * sizeof(int));
	int *type = (int*)malloc(a->nf * sizeof(int));

	if (dtime == NULL || dpoint == NULL || time == NULL || point == NULL || plane_number == NULL || type == NULL) {
		// Handle memory allocation failure
		fprintf(stderr, "Memory allocation failed in line_polyhedron_intersect\n");
		free(dtime);
		free(dpoint);
		free(time);
		free(point);
		free(plane_number);
		free(type);
		return 0;
	}

	//first find all the potential intersects that happens immediately or in the future.
	LinePolyhedronIntersect *lpi = &(a->lpi);
	for (i = 0; i < a->nf; i++) { 
		
		//if num_intersect > number of polyhedron faces, something's wrong.
		if (n_intersect_found >= a->nf) { 
			break;
		}
		
		//if plane number in the skip list, skip to next plane.
		if (skip_plane_number && num_skip_plane > 0) {
			skip = 0;;
			for (j = 0; j < num_skip_plane; j++) {
				if (i == skip_plane_number[j]) { 
					skip = 1; 
					break;
				}
			}
			if (skip) {
				continue;
			}
		}
		
		//if line does not intersect plane, skip to next plane. 
		if (line_plane_intersect(line_p, line_v, a->fn[i], a->fp[i], max_d, &lp_dt, &lp_dp) == 0) {
			continue;
		}
		
		//if intersect in the past, skip to next plane.
		if (lp_dt < 0) {
			continue;
		}
		lp_t = line_t + lp_dt;
		lp_p = coords_add(line_p, lp_dp); 
		
		//if intersect is not on polyhedron, skip to next plane. 
		check_point_with_respect_to_polyhedron(	&lp_p, &line_v, a, num_skip_plane, skip_plane_number, max_d, 
												0, 0, 0, &is_on_inside_outside_polyhedron, 
												&is_crossing_plane, &is_crossing_edge, &is_crossing_vertex, &is_flying_on_plane, &is_flying_on_edge);
		if (is_on_inside_outside_polyhedron != 1) {
			continue;
		}
		
		//If same point, time, plane as last intersect, skip to next plane.
		//For edge or vertex, only check time and point,
		//for plane intersect that is neither edge or vertex, check time, point, plane
		if (fabs(last_intersect_time - lp_t) < DBL_EPSILON && 
			fabs(last_intersect_point.x - lp_p.x) < DBL_EPSILON && 
			fabs(last_intersect_point.y - lp_p.y) < DBL_EPSILON && 
			fabs(last_intersect_point.z - lp_p.z) < DBL_EPSILON && 
			(last_intersect_plane == i || is_crossing_plane == 0)) {
			continue;
		}
		
		//If intersect already in the list, skip to next plane.
		if (n_intersect_found > 1) {
			skip = 0;
			for (j = 0; j < n_intersect_found; j++) {
				if (fabs((lpi->a_lp_t)[j] - lp_t) < DBL_EPSILON && 
					fabs((lpi->a_lp_p)[j].x - lp_p.x) < DBL_EPSILON && 
					fabs((lpi->a_lp_p)[j].y - lp_p.y) < DBL_EPSILON && 
					fabs((lpi->a_lp_p)[j].z - lp_p.z) < DBL_EPSILON &&
					((lpi->a_lp_pn)[j] == i || is_crossing_plane == 0) &&
					fabs((lpi->a_lp_dt)[j] - lp_dt) < DBL_EPSILON && 
					fabs((lpi->a_lp_dp)[j].x - lp_dp.x) < DBL_EPSILON && 
					fabs((lpi->a_lp_dp)[j].y - lp_dp.y) < DBL_EPSILON && 
					fabs((lpi->a_lp_dp)[j].z - lp_dp.z) < DBL_EPSILON) {
					skip = 1;
					break;
				}
			}
			if (skip == 1) {
				continue;
			}
		}
		
		//intersect point is on polyhedron, at present or in the future, and not a repeat of last intersect or a duplicated intersect.
		//store the result of point-plane intersect in the list
		(lpi->a_lp_dt)[n_intersect_found] = lp_dt;
		(lpi->a_lp_dp)[n_intersect_found] = lp_dp;
		(lpi->a_lp_t)[n_intersect_found] = lp_t;
		(lpi->a_lp_p)[n_intersect_found] = lp_p;
		(lpi->a_lp_pn)[n_intersect_found] = i;
		(lpi->a_lp_ty)[n_intersect_found] = 0; //should not be 0
		if (is_crossing_plane == 1) (lpi->a_lp_ty)[n_intersect_found] = 1;
		if (is_crossing_edge == 1) (lpi->a_lp_ty)[n_intersect_found] = 2;
		if (is_crossing_vertex == 1) (lpi->a_lp_ty)[n_intersect_found] = 3;
		if (is_flying_on_plane == 1) (lpi->a_lp_ty)[n_intersect_found] = 4;
		if (is_flying_on_edge == 1) (lpi->a_lp_ty)[n_intersect_found] = 5;
		++(n_intersect_found);
	}
	
	if (n_intersect_found == 0) {
		//no intersect
		*num_intersect = 0;
		// Ensure to free the allocated memory after use
		free(dtime);
		free(dpoint);
		free(time);
		free(point);
		free(plane_number);
		free(type);
		return 1;
	}
	
	if (*num_intersect <= 0) {
		//allocate memory to output array
		intersect_dtime = (double*)calloc(n_intersect_found, sizeof(double));
		intersect_dpoint = (Coords*)calloc(n_intersect_found, sizeof(Coords));
		intersect_time = (double*)calloc(n_intersect_found, sizeof(double));
		intersect_point = (Coords*)calloc(n_intersect_found, sizeof(Coords));
		intersect_plane = (int*)calloc(n_intersect_found, sizeof(int));
		intersect_type = (int*)calloc(n_intersect_found, sizeof(int));
	}
	
	int n_output = MIN(n_intersect_found, *num_intersect);
	
	if (n_intersect_found == 1) {
			if (intersect_dtime != 0) intersect_dtime[0] = (lpi->a_lp_dt)[0];
			if (intersect_dpoint != 0) intersect_dpoint[0] = (lpi->a_lp_dp)[0];
			if (intersect_time != 0) intersect_time[0] = (lpi->a_lp_t)[0];
			if (intersect_point != 0) intersect_point[0] = (lpi->a_lp_p)[0];
			if (intersect_plane != 0) intersect_plane[0] = (lpi->a_lp_pn)[0];
			if (intersect_type != 0) intersect_type[0] = (lpi->a_lp_ty)[0]; 
	}
	else { //n_intersect_found > 1, sort the sequence
		PolyhedronIndexValuePair *idp = lpi->idp;
		Coords *a_lp_dp=lpi->a_lp_dp;
		for (i = 0; i < n_intersect_found; i++) {
			idp[i].index = i;
			idp[i].value = coords_len(a_lp_dp[i]);
		}
		//sort the intersect list according to the distance between point and intersect |dp|
		//sort using qsort, the order of list idp is rearranged as a result
		qsort(idp, n_intersect_found, sizeof(idp[0]), polyhedron_qsort_compare_func);
		
		for (i = 0; i < n_output; i++) {
			if (intersect_dtime != 0) intersect_dtime[i] = (lpi->a_lp_dt)[idp[i].index];
			if (intersect_dpoint != 0) intersect_dpoint[i] = (lpi->a_lp_dp)[idp[i].index];
			if (intersect_time != 0) intersect_time[i] = (lpi->a_lp_t)[idp[i].index];
			if (intersect_point != 0) intersect_point[i] = (lpi->a_lp_p)[idp[i].index];
			if (intersect_plane != 0) intersect_plane[i] = (lpi->a_lp_pn)[idp[i].index];
			if (intersect_type != 0) intersect_type[i] = (lpi->a_lp_ty)[idp[i].index];
		} 
	}
	
	*num_intersect = n_intersect_found;


  // Ensure to free the allocated memory after use
  free(dtime);
  free(dpoint);
  free(time);
  free(point);
  free(plane_number);
  free(type);
  return 1;
}

/*****************************************************/
/* Rotate a polyhedron by applying a rotation matrix */
/* returns 1 = no error, 0 = error                   */
/*****************************************************/

int rotate_polyhedron (Polyhedron*polyGeo, Rotation rot) { 
		
	Polyhedron geo = *(polyGeo); //copy polyGeo to geo
	
	int i;
	if (geo.nf > 0) {
		for (i=0; i<geo.nf; i++) {
			geo.fn[i] = rot_apply(rot, geo.fn[i]);
			geo.fp[i] = rot_apply(rot, geo.fp[i]);
		}
	}
		
	if (geo.nv > 0) {
		for (i=0; i<geo.nv; i++) {
			geo.vp[i] = rot_apply(rot, geo.vp[i]);
		}
	}

	*(polyGeo) = geo; //copy back from geo to polyGeo
	
	return 1;
}

/************************************************************************/
/* Rotate a polyhedron about a rotate axis by an angle, right hand rule */
/* pass a Rotation pointer to receive the rotation matrix, 0 if not     */
/* returns 1 = no error, 0 = error                                      */
/************************************************************************/

int rotate_polyhedron_about_axis (Polyhedron*polyGeo, Coords axis, double angle, Rotation*rot) {
	if (coords_len(axis) <= DBL_EPSILON) return 0; //axis must have length

	double 	A=angle*DEG2RAD,cosA=cos(A),sinA=sin(A),oneMinusCosA=1-cosA, 
			xx=axis.x, yy=axis.y, zz=axis.z;
	NORM(xx,yy,zz);
	Rotation rr; 
	rr[0][0] = cosA + xx * xx * oneMinusCosA;
    rr[0][1] = xx * yy * oneMinusCosA - zz * sinA;
    rr[0][2] = xx * zz * oneMinusCosA + yy * sinA;

    rr[1][0] = yy * xx * oneMinusCosA + zz * sinA;
    rr[1][1] = cosA + yy * yy * oneMinusCosA;
    rr[1][2] = yy * zz * oneMinusCosA - xx * sinA;

    rr[2][0] = zz * xx * oneMinusCosA - yy * sinA;
    rr[2][1] = zz * yy * oneMinusCosA + xx * sinA;
    rr[2][2] = cosA + zz * zz * oneMinusCosA;
	
	if (rot != 0) rot_copy(*rot, rr);
	
	Polyhedron geo = *(polyGeo); //copy polyGeo to geo
	
	int i;
	if (geo.nf > 0) 
		for (i=0; i<geo.nf; i++) {
			geo.fn[i] = rot_apply(rr, geo.fn[i]);
			geo.fp[i] = rot_apply(rr, geo.fp[i]);
		}
		
	if (geo.nv > 0) 
		for (i=0; i<geo.nv; i++) {
			geo.vp[i] = rot_apply(rr, geo.vp[i]);
		}
	
	*(polyGeo) = geo; //copy back from geo to polyGeo
	
	return 1;
}

/***************************************************/
/* Translate a polyhedron by a displacement vector */
/* returns 1 = no error, 0 = error                 */
/***************************************************/

int translate_polyhedron (Polyhedron*polyGeo, Coords displace) {
	Polyhedron geo = *(polyGeo); //copy polyGeo to geo
	
	int i;
	if (geo.nf > 0) 
		for (i=0; i<geo.nf; i++) {
			geo.fp[i] = coords_add(displace, geo.fp[i]);
		}
		
	if (geo.nv > 0) 
		for (i=0; i<geo.nv; i++) {
			geo.vp[i] = coords_add(displace, geo.vp[i]);
		}
	
	*(polyGeo) = geo; //copy back from geo to polyGeo
	
	return 1;
} 

/****************************************/
/* return to an empty polyhedron object */
/****************************************/

void empty_polyhedron(Polyhedron*polyGeo){
	//note: this function does not set polyGeo=0 as it may be declared staticly externally
	if (polyGeo) {
		Polyhedron*a = polyGeo; 
		if (a->nf > 0) {
			if (a->fn) free(a->fn);
			if (a->fp) free(a->fp);
			if (a->afvi) {
				for (int i = 0; i < a->nf; i++) {
					if (a->afvi[i].nfvi > 0) {
						if (a->afvi[i].ifvi) 
							free(a->afvi[i].ifvi);
					}
				}
				free(a->afvi);
			}
			free(a->lpi.a_lp_dt); 
			free(a->lpi.a_lp_dp);
			free(a->lpi.a_lp_t);
			free(a->lpi.a_lp_p);
			free(a->lpi.a_lp_pn);
			free(a->lpi.a_lp_ty);
			free(a->lpi.idp);
			a->nf = 0;
		}
		if (a->nv > 0) {
			free(a->vp);
			a->nv = 0;
		}
	}
}



#endif //end of POLYHEDRON_C

/* end of polyhedron.c */

