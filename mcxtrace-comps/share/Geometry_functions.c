// -------------    Geometry functions   -----------------------------------------------------------------------
/*
 * This file contains functions used for geometry.
 *
 * For each geometry A type there are:
 *  intersection function: determines intersection between straight line and the geometry type
 *  within function: determines wether a point is within the geometry, or outside
 *  geometryA_overlaps_geometryB: determines if geometry A overlaps with geometry B
 *  geometryA_inside_geometryB: determines if geometry A is completely inside geometry B
 *
 * At the end of the file, there is functions describing the logic for determining if one geometry
 *  is inside/overlaps another. It is placed here, so that all code to be expanded by adding a new
 *  geometry is within the same file.
 *
 * To add a new geometry one needs to:
 *  Write a geometry_storage_struct that contains the paramters needed to describe the geometry
 *  Add a pointer to this storage type in the geometry_parameter_union
 *  Write a function for intersection with line, using the same input scheme as for the others
 *  Write a function checking if a point is within the geometry
 *  Write a function checking if one instance of the geometry overlaps with another
 *  Write a function checking if one instance of the geometry is inside another
 *  For each exsisting geometry: 
 *      Write a function checking if an instance of this geometry overlaps with an instance of the exsisting
 *      Write a function checking if an instance of this geometry is inside an instance of the exsisting
 *      Write a function checking if an instance of an existing geometry is inside an instance of this geometry
 *
 *  Add these functions to geometry to the logic at the end of this file
 *  Write a component file similar to the exsisting ones, taking the input from the instrument file, and sending
 *   it on to the master component.
*/

struct sphere_storage{
double sph_radius;
};

struct cylinder_storage{
double cyl_radius;
double height;
Coords direction_vector;
};

struct box_storage{
double x_width1;
double y_height1;
double z_depth;
double x_width2;
double y_height2;
int is_rectangle; // Is rectangle = 1 if x_width1 = x_width2 / h1 = h2
Coords x_vector; // In main component frame
Coords y_vector;
Coords z_vector;
Coords normal_vectors[6]; // In local frame
};

struct cone_storage{
double cone_radius_top;
double cone_radius_bottom;
double height;
Coords direction_vector;
};

struct mesh_storage{
int n_facets;
int counter;
double v1_x[50000];
double v1_y[50000];
double v1_z[50000];
double v2_x[50000];
double v2_y[50000];
double v2_z[50000];
double v3_x[50000];
double v3_y[50000];
double v3_z[50000];
double normal_x[50000];
double normal_y[50000];
double normal_z[50000];
Coords direction_vector;
Coords Bounding_Box_Center;
double Bounding_Box_Radius;
};

// A number of functions below use Dot() as scalar product, replace by coords_sp define
#define Dot(a, b) coords_sp(a, b)

// Function for transforming a ray position / velocity to a local frame
Coords transform_position(Coords ray_position, Coords component_position, Rotation component_t_rotation) {

    Coords non_rotated_position = coords_sub(ray_position,component_position);
    
    // Rotate the position of the neutron around the center of the cylinder
    Coords rotated_coordinates = rot_apply(component_t_rotation,non_rotated_position);
    
    return rotated_coordinates;
}


union geometry_parameter_union allocate_box_storage_copy(union geometry_parameter_union *union_input) {
  union geometry_parameter_union union_output;
  // Allocate the space for a cylinder_storage structe in the new union_output (union as the c structre)
  union_output.p_box_storage = malloc(sizeof(struct box_storage));
  // Copy the input storage to the output
  *union_output.p_box_storage = *union_input->p_box_storage;
  
  return union_output;
}


union geometry_parameter_union allocate_cylinder_storage_copy(union geometry_parameter_union *union_input) {
  union geometry_parameter_union union_output;
  // Allocate the space for a cylinder_storage structe in the new union_output (union as the c structre)
  union_output.p_cylinder_storage = malloc(sizeof(struct cylinder_storage));
  // Copy the input storage to the output
  *union_output.p_cylinder_storage = *union_input->p_cylinder_storage;
  
  return union_output;
}

union geometry_parameter_union allocate_sphere_storage_copy(union geometry_parameter_union *union_input) {
  union geometry_parameter_union union_output;
  // Allocate the space for a cylinder_storage structe in the new union_output (union as the c structre)
  union_output.p_sphere_storage = malloc(sizeof(struct sphere_storage));
  // Copy the input storage to the output
  *union_output.p_sphere_storage = *union_input->p_sphere_storage;
  
  return union_output;
}

union geometry_parameter_union allocate_cone_storage_copy(union geometry_parameter_union *union_input) {
  union geometry_parameter_union union_output;
  // Allocate the space for a cone_storage structe in the new union_output (union as the c structre)
  union_output.p_cone_storage = malloc(sizeof(struct cone_storage));
  // Copy the input storage to the output
  *union_output.p_cone_storage = *union_input->p_cone_storage;
  
  return union_output;
}

union geometry_parameter_union allocate_mesh_storage_copy(union geometry_parameter_union *union_input) {
  union geometry_parameter_union union_output;
  // Allocate the space for a mesh_storage structe in the new union_output (union as the c structre)
  union_output.p_mesh_storage = malloc(sizeof(struct mesh_storage));
  // Copy the input storage to the output
  *union_output.p_mesh_storage = *union_input->p_mesh_storage;
  
  return union_output;
}

// -------------    Surroundings  ---------------------------------------------------------------
int r_within_surroundings(Coords pos,struct geometry_struct *geometry) {
    // The surroundings are EVERYWHERE
        return 1;
    }

// -------------    General geometry ------------------------------------------------------------

Coords point_on_circle(Coords center, Coords direction, double radius, int point_nr, int number_of_points) {

    Coords output;
    Coords cross_input = coords_set(0,1,0);
    
    if (scalar_prod(cross_input.x,cross_input.y,cross_input.z,direction.x,direction.y,direction.z) > 0.9) {
            cross_input.x = 1; cross_input.y = 0; cross_input.z = 0;
    }
    
    Coords cross_product;
    vec_prod(cross_product.x,cross_product.y,cross_product.z,direction.x,direction.y,direction.z,cross_input.x,cross_input.y,cross_input.z);

    double cross_length = length_of_position_vector(cross_product);
    double radius_over_cross_length = radius/cross_length;
    cross_product = coords_scalar_mult(cross_product,radius_over_cross_length);
    
    
    double rotate_angle = 2*PI*((double) point_nr)/((double) number_of_points);
    
    rotate(output.x,output.y,output.z,cross_product.x,cross_product.y,cross_product.z,rotate_angle,direction.x,direction.y,direction.z);
    
    output = coords_add(output,center);
    
    return output;
};

void points_on_circle(Coords *output, Coords center, Coords direction, double radius, int number_of_points) {

    Coords cross_input = coords_set(0,1,0);
    
    if (scalar_prod(cross_input.x,cross_input.y,cross_input.z,direction.x,direction.y,direction.z) > 0.9) {
            cross_input.x = 1; cross_input.y = 0; cross_input.z = 0;
    }
    
    Coords cross_product;
    vec_prod(cross_product.x,cross_product.y,cross_product.z,direction.x,direction.y,direction.z,cross_input.x,cross_input.y,cross_input.z);

    double cross_length = length_of_position_vector(cross_product);
    double radius_over_cross_length = radius/cross_length;
    cross_product = coords_scalar_mult(cross_product,radius_over_cross_length);
    
    int point_nr;
    double rotate_angle;
    for (point_nr = 0;point_nr<number_of_points;point_nr++) {
        rotate_angle = 2*PI*((double) point_nr)/((double) number_of_points);
        rotate(output[point_nr].x,output[point_nr].y,output[point_nr].z,cross_product.x,cross_product.y,cross_product.z,rotate_angle,direction.x,direction.y,direction.z);
        output[point_nr] = coords_add(output[point_nr],center);
    }
};

// -------- Brute force last resorts for within / overlap -------------------------------------

int A_within_B(struct geometry_struct *child, struct geometry_struct *parent, int resolution) {
  // This function assumes the parent (B) is a convex geoemtry
  // If all points on the shell of geometry A is within B, so are all lines between them.
  
  // Should be done first, but takes so little time and may save a longer computation
  if (parent->within_function(child->center,parent) == 0) return 0;
  
  // resolution selects the number of points to be generated on the shell.
  struct pointer_to_1d_coords_list shell_points;
  shell_points = child->shell_points(child,resolution);
  // Shell_points.elements need to be freed before leaving this function
  
  if (shell_points.num_elements > resolution || shell_points.num_elements < 0) {
    printf("\nERROR: Shell point function used in A_within_B return garbage num_elements. \n");
    exit(1);
  }
  
  int iterate;
  
  for (iterate=0;iterate<shell_points.num_elements;iterate++) {
    if (parent->within_function(shell_points.elements[iterate],parent) == 0) {
      free(shell_points.elements);
      return 0;
    }
  }
  
  free(shell_points.elements);
  
  // If all points are inside, the entire geometry is assumed inside as parent should be convex
  return 1;
}

int mesh_A_within_B(struct geometry_struct *child, struct geometry_struct *parent) {
  // This function assumes the parent (B) is a convex geoemtry
  // If all points on the shell of geometry A is within B, so are all lines between them.
  // This is modified so resolution is not set manually, but all mesh shell points are taken
  
  printf("shell points mesh A within B \n");
  // resolution selects the number of points to be generated on the shell.
  struct pointer_to_1d_coords_list shell_points;
  shell_points = child->shell_points(child, 1); // mesh shell points do not use max points
  // Shell_points.elements need to be freed before leaving this function
  //printf("\n GOT OUT TEST");
  int iterate;
  
  for (iterate=0;iterate<shell_points.num_elements;iterate++) {
    if (parent->within_function(shell_points.elements[iterate],parent) == 0) {
      free(shell_points.elements);
      return 0;
    }
  }
  
  
  // If all points are inside, the entire geometry is assumed inside as parent should be convex
  free(shell_points.elements);
  return 1;
}

/*
// Turned out to be harder to generalize the overlap functions, but at least within was doable.
int A_overlaps_B(struct geometry_struct *child, struct geometry_struct *parent) {
  // This function assumes the parent (B) is a convex geoemtry
  // Does not work, need to check lines between points
  
  // Starting this system with a simple constant 64 point generation.
  struct pointer_to_1d_coords_list shell_points;
  shell_points = child.shell_points(child,64);
  
  int iterate;
  
  for (iterate=0;iterate<shell_points.num_elements;iterate++) {
    if (parent.within_function(shell_points.elements[iterate],parent) == 1) return 1;
  }
  
  
  // This requires that the points on the shell are saved pair wise in such a way that
  //  the lines between them would cover the entire surface when the resolution goes to
  //  infinity. NOT GOING TO WORK FOR BOX
  
  
  for (iterate=0;iterate<floor(shell_points.num_elements/2);iterate = iterate + 2) {
    // check intersections with parent between the two child points
    if (existence_of_intersection(shell_points.elements[iterate],shell_points.elements[iterate+1],parent) == 1) return 1;
  }
  
  
  // If no points were inside, the geometries are assumed not to overlap as parent should be convex
  return 0;
}
*/



// -------------    Functions for box ray tracing used in trace ---------------------------------
// These functions needs to be fast, as they may be used many times for each ray
int sample_box_intersect_advanced(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry) {
    // possible approaches
    // rotate to a simple coordinate system by rotating the ray (easier to switch to McXtrace standard)
    
    // There are still many variables here that can be pre calculated and saved in the box_storage.
    
    double depth = geometry->geometry_parameters.p_box_storage->z_depth;
    double width1 = geometry->geometry_parameters.p_box_storage->x_width1;
    double width2 = geometry->geometry_parameters.p_box_storage->x_width2;
    double height1 = geometry->geometry_parameters.p_box_storage->y_height1;
    double height2 = geometry->geometry_parameters.p_box_storage->y_height2;
    
    Coords x_vector = geometry->geometry_parameters.p_box_storage->x_vector;
    Coords y_vector = geometry->geometry_parameters.p_box_storage->y_vector;
    Coords z_vector = geometry->geometry_parameters.p_box_storage->z_vector;
    
    Coords normal_vectors;
    
    // Declare variables for the function
    Coords coordinates;
    
    // Coordinate transformation
    coordinates.x = r[0] - geometry->center.x;
    coordinates.y = r[1] - geometry->center.y;
    coordinates.z = r[2] - geometry->center.z;
    
    Coords rotated_coordinates;
    // Rotate the position of the neutron around the center of the cylinder
    rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,coordinates);
    
    Coords velocity = coords_set(v[0],v[1],v[2]);
    Coords rotated_velocity;
    
    // Rotate the position of the neutron around the center of the cylinder
    rotated_velocity = rot_apply(geometry->transpose_rotation_matrix,velocity);
    
    double x_result,y_result,z_result;
    
    *num_solutions = 0;
    // normal_vectors 0 and 1 have x and y = 0;
    // normal vectors 0: point in plane [0 0 -0.5*depth]
    normal_vectors = geometry->geometry_parameters.p_box_storage->normal_vectors[0];
    t[*num_solutions] = (-0.5*depth - rotated_coordinates.z)*normal_vectors.z/(normal_vectors.z*rotated_velocity.z);
    //printf("Intersection_time for face 0 = %f\n",t[*num_solutions]);
    x_result = rotated_coordinates.x + t[*num_solutions]*rotated_velocity.x;
    y_result = rotated_coordinates.y + t[*num_solutions]*rotated_velocity.y;
    z_result = rotated_coordinates.z + t[*num_solutions]*rotated_velocity.z; // only for debug
    //printf("Test solution for face number 0: (x,y) = (%f,%f,%f)\n",x_result,y_result,z_result);
    if (x_result >= -0.5*width1 && x_result <= 0.5*width1 && y_result >= -0.5*height1 && y_result <= 0.5*height1) {
        (*num_solutions)++;
        //printf("Solution found for face number 0\n");
    }
    
    // normal vectors 1: point in plane [0 0 0.5*depth]
    normal_vectors = geometry->geometry_parameters.p_box_storage->normal_vectors[1];
    t[*num_solutions] = (0.5*depth - rotated_coordinates.z)*normal_vectors.z/(normal_vectors.z*rotated_velocity.z);
    //printf("Intersection_time for face 1 = %f\n",t[*num_solutions]);
    x_result = rotated_coordinates.x + t[*num_solutions]*rotated_velocity.x;
    y_result = rotated_coordinates.y + t[*num_solutions]*rotated_velocity.y;
    //z_result = rotated_coordinates.z + t[*num_solutions]*rotated_velocity.z; // only for debug
    //printf("Test solution for face number 1: (x,y) = (%f,%f,%f)\n",x_result,y_result,z_result);
    if (x_result >= -0.5*width2 && x_result <= 0.5*width2 && y_result >= -0.5*height2 && y_result <= 0.5*height2) {
        (*num_solutions)++;
        //printf("Solution found for face number 1\n");
    }
    // These were done first as they are fastest, and most likely to be the solutions (normal to do small depth and large width/height), and standard orientation is to have one of these faces towards the source. When the fastest and most likely are done first, there is larger chance to skip more and slower calculations
    
    if (*num_solutions != 2) {
        // normal vectors 2 and 3 have y = 0
        normal_vectors = geometry->geometry_parameters.p_box_storage->normal_vectors[2];
        t[*num_solutions] = ((0.5*width1 - rotated_coordinates.x)*normal_vectors.x + (-0.5*depth - rotated_coordinates.z)*normal_vectors.z)/(normal_vectors.x*rotated_velocity.x+normal_vectors.z*rotated_velocity.z);
        // x_result = rotated_coordinates.x + t[*num_solutions]*rotated_velocity.x;
        y_result = rotated_coordinates.y + t[*num_solutions]*rotated_velocity.y;
        z_result = rotated_coordinates.z + t[*num_solutions]*rotated_velocity.z;
        if (z_result > -0.5*depth && z_result < 0.5*depth && y_result >= -0.5*(height1+(height2-height1)*(0.5*depth+z_result)/depth) && y_result < 0.5*(height1+(height2-height1)*(0.5*depth+z_result)/depth)) {
            (*num_solutions)++;
            //printf("Solution found for face number 2\n");
        }
    }
    
    if (*num_solutions != 2) {
        // normal vectors 2 and 3 have y = 0
        normal_vectors = geometry->geometry_parameters.p_box_storage->normal_vectors[3];
        t[*num_solutions] = ((-0.5*width1 - rotated_coordinates.x)*normal_vectors.x + (-0.5*depth - rotated_coordinates.z)*normal_vectors.z)/(normal_vectors.x*rotated_velocity.x+normal_vectors.z*rotated_velocity.z);
        // x_result = rotated_coordinates.x + t[*num_solutions]*rotated_velocity.x;
        y_result = rotated_coordinates.y + t[*num_solutions]*rotated_velocity.y;
        z_result = rotated_coordinates.z + t[*num_solutions]*rotated_velocity.z;
        if (z_result > -0.5*depth && z_result < 0.5*depth && y_result > -0.5*(height1+(height2-height1)*(0.5*depth+z_result)/depth) && y_result <= 0.5*(height1+(height2-height1)*(0.5*depth+z_result)/depth)) {
            (*num_solutions)++;
            //printf("Solution found for face number 3\n");
        }
    }
    
    if (*num_solutions != 2) {
        // normal vectors 4 and 5 have x = 0
        normal_vectors = geometry->geometry_parameters.p_box_storage->normal_vectors[4];
        t[*num_solutions] = ((0.5*height1 - rotated_coordinates.y)*normal_vectors.y + (-0.5*depth - rotated_coordinates.z)*normal_vectors.z)/(normal_vectors.y*rotated_velocity.y+normal_vectors.z*rotated_velocity.z);
        x_result = rotated_coordinates.x + t[*num_solutions]*rotated_velocity.x;
        //y_result = rotated_coordinates.y + t[*num_solutions]*rotated_velocity.y;
        z_result = rotated_coordinates.z + t[*num_solutions]*rotated_velocity.z;
        if (z_result > -0.5*depth && z_result < 0.5*depth && x_result >= -0.5*(width1+(width2-width1)*(0.5*depth+z_result)/depth) && x_result < 0.5*(width1+(width2-width1)*(0.5*depth+z_result)/depth)) {
            (*num_solutions)++;
            //printf("Solution found for face number 4\n");
        }
    }
    
    if (*num_solutions != 2) {
        // normal vectors 4 and 5 have x = 0
        normal_vectors = geometry->geometry_parameters.p_box_storage->normal_vectors[5];
        t[*num_solutions] = ((-0.5*height1 - rotated_coordinates.y)*normal_vectors.y + (-0.5*depth - rotated_coordinates.z)*normal_vectors.z)/(normal_vectors.y*rotated_velocity.y+normal_vectors.z*rotated_velocity.z);
        x_result = rotated_coordinates.x + t[*num_solutions]*rotated_velocity.x;
        //y_result = rotated_coordinates.y + t[*num_solutions]*rotated_velocity.y;
        z_result = rotated_coordinates.z + t[*num_solutions]*rotated_velocity.z;
        if (z_result > -0.5*depth && z_result < 0.5*depth && x_result > -0.5*(width1+(width2-width1)*(0.5*depth+z_result)/depth) && x_result <= 0.5*(width1+(width2-width1)*(0.5*depth+z_result)/depth)) {
            (*num_solutions)++;
            //printf("Solution found for face number 5\n");
        }
    }
    
    switch(*num_solutions) {
    case 2:
        if (t[0] > t[1]) {
            double temp = t[1];
            t[1] = t[0];
            t[0] = temp;
        }
        return 1;
    case 1:
        t[1] = -1;
        return 1;
    case 0:
        t[0] = -1;
        t[1] = -1;
        return 0;
    }
    
    // Above switch will catch all solutions, but the return 0 here silences a compiler warning.
    return 0;
};

void box_corners_global_frame(Coords *corner_points, struct geometry_struct *geometry) {
    // Returns a pointer to an array containing the 8 positions of the corners of the box.
    double depth = geometry->geometry_parameters.p_box_storage->z_depth;
    double width1 = geometry->geometry_parameters.p_box_storage->x_width1;
    double width2 = geometry->geometry_parameters.p_box_storage->x_width2;
    double height1 = geometry->geometry_parameters.p_box_storage->y_height1;
    double height2 = geometry->geometry_parameters.p_box_storage->y_height2;
    
    Coords x_vector = geometry->geometry_parameters.p_box_storage->x_vector;
    Coords y_vector = geometry->geometry_parameters.p_box_storage->y_vector;
    Coords z_vector = geometry->geometry_parameters.p_box_storage->z_vector;
    
    Coords center = geometry->center;
    
    corner_points[0] = coords_add(coords_add(coords_add(center,coords_scalar_mult(z_vector,-0.5*depth)),coords_scalar_mult(x_vector,-0.5*width1)),coords_scalar_mult(y_vector,-0.5*height1));
    
    corner_points[1] = coords_add(corner_points[0],coords_scalar_mult(x_vector,width1));
    corner_points[2] = coords_add(corner_points[1],coords_scalar_mult(y_vector,height1));
    corner_points[3] = coords_add(corner_points[0],coords_scalar_mult(y_vector,height1));
    
    corner_points[4] = coords_add(coords_add(coords_add(center,coords_scalar_mult(z_vector,0.5*depth)),coords_scalar_mult(x_vector,-0.5*width2)),coords_scalar_mult(y_vector,-0.5*height2));
    
    corner_points[5] = coords_add(corner_points[4],coords_scalar_mult(x_vector,width2));
    corner_points[6] = coords_add(corner_points[5],coords_scalar_mult(y_vector,height2));
    corner_points[7] = coords_add(corner_points[4],coords_scalar_mult(y_vector,height2));
};

void box_corners_local_frame(Coords *corner_points, struct geometry_struct *geometry) {
    double depth = geometry->geometry_parameters.p_box_storage->z_depth;
    double width1 = geometry->geometry_parameters.p_box_storage->x_width1;
    double width2 = geometry->geometry_parameters.p_box_storage->x_width2;
    double height1 = geometry->geometry_parameters.p_box_storage->y_height1;
    double height2 = geometry->geometry_parameters.p_box_storage->y_height2;
    
    Coords center = geometry->center;
    Coords x_vector = coords_set(1,0,0);
    Coords y_vector = coords_set(0,1,0);
    Coords z_vector = coords_set(0,0,1);
    Coords origo = coords_set(0,0,0);

    // Bug fixed on 25/11, center was used instead of origo
    corner_points[0] = coords_add(coords_add(coords_add(origo,coords_scalar_mult(z_vector,-0.5*depth)),coords_scalar_mult(x_vector,-0.5*width1)),coords_scalar_mult(y_vector,-0.5*height1));
    
    corner_points[1] = coords_add(corner_points[0],coords_scalar_mult(x_vector,width1));
    corner_points[2] = coords_add(corner_points[1],coords_scalar_mult(y_vector,height1));
    corner_points[3] = coords_add(corner_points[0],coords_scalar_mult(y_vector,height1));
    
    corner_points[4] = coords_add(coords_add(coords_add(origo,coords_scalar_mult(z_vector,0.5*depth)),coords_scalar_mult(x_vector,-0.5*width2)),coords_scalar_mult(y_vector,-0.5*height2));
    
    corner_points[5] = coords_add(corner_points[4],coords_scalar_mult(x_vector,width2));
    corner_points[6] = coords_add(corner_points[5],coords_scalar_mult(y_vector,height2));
    corner_points[7] = coords_add(corner_points[4],coords_scalar_mult(y_vector,height2));
};

int sample_box_intersect_simple(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry) {
    double width = geometry->geometry_parameters.p_box_storage->x_width1;
    double height = geometry->geometry_parameters.p_box_storage->y_height1;
    double depth = geometry->geometry_parameters.p_box_storage->z_depth;
    
    // Declare variables for the function
    double x_new,y_new,z_new;
    
    // Coordinate transformation
    x_new = r[0] - geometry->center.x;
    y_new = r[1] - geometry->center.y;
    z_new = r[2] - geometry->center.z;
    
    Coords coordinates = coords_set(x_new,y_new,z_new);
    Coords rotated_coordinates;
    // printf("Cords coordinates = (%f,%f,%f)\n",coordinates.x,coordinates.y,coordinates.z);
    
    // Rotate the position of the neutron around the center of the cylinder
    rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,coordinates);
    // rotated_coordinates = rot_apply(rotation_matrix_debug,coordinates);
    //     printf("Cords rotated_coordinates = (%f,%f,%f)\n",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z);
    
    Coords velocity = coords_set(v[0],v[1],v[2]);
    Coords rotated_velocity;
    //     printf("Cords velocity = (%f,%f,%f)\n",velocity.x,velocity.y,velocity.z);
    
    // Rotate the position of the neutron around the center of the cylinder
    rotated_velocity = rot_apply(geometry->transpose_rotation_matrix,velocity);
    // rotated_velocity = rot_apply(rotation_matrix_debug,velocity);
    //     printf("Cords rotated_velocity = (%f,%f,%f)\n",rotated_velocity.x,rotated_velocity.y,rotated_velocity.z);
    
    int output;
    // Run McXtrasce built in box intersect funtion (box centered on origin with sides aligned with cartesian axis)
    if ((output = box_intersect(&t[0],&t[1],rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z,rotated_velocity.x,rotated_velocity.y,rotated_velocity.z,width,height,depth)) == 0) {
        *num_solutions = 0;t[0]=-1;t[1]=-1;}
    else if (t[1] != 0) *num_solutions = 2;
    else {*num_solutions = 1;t[1]=-1;} // t[2] is a memory error!
    
    return output;
};

int r_within_box_simple(Coords pos,struct geometry_struct *geometry) {
    // Unpack parameters
    double width = geometry->geometry_parameters.p_box_storage->x_width1;
    double height = geometry->geometry_parameters.p_box_storage->y_height1;
    double depth = geometry->geometry_parameters.p_box_storage->z_depth;
    
    //Coords coordinates = coords_set(x_new,y_new,z_new);
    Coords coordinates = coords_sub(pos,geometry->center);
    
    Coords rotated_coordinates;
    // printf("Cords coordinates = (%f,%f,%f)\n",coordinates.x,coordinates.y,coordinates.z);
    
    // Rotate the position of the neutron around the center of the cylinder
    rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,coordinates);
    
    // May be faster to check for one at a time to get an early return 0
    return (rotated_coordinates.x > -0.5*width && rotated_coordinates.x < 0.5*width && rotated_coordinates.y > -0.5*height && rotated_coordinates.y < 0.5*height && rotated_coordinates.z > -0.5*depth && rotated_coordinates.z < 0.5*depth);
};

int r_within_cone(Coords pos,struct geometry_struct *geometry) {
    // Is point inside cone?
// Unpack parameters
    double radius_top = geometry->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius_bottom = geometry->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height = geometry->geometry_parameters.p_cone_storage->height;
    Coords center = geometry->center;
    
    double x_new,y_new,z_new;
    
    // Coordinate transformation
    x_new = pos.x - geometry->center.x;
    y_new = pos.y - geometry->center.y;
    z_new = pos.z - geometry->center.z;
    
    Coords coordinates = coords_set(x_new,y_new,z_new);
    Coords rotated_coordinates;
   
    rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,coordinates);

    
    int verbal = 0;
    // Generate unit direction vector along center axis of cones
    
    // Start with vector that points along the cone in the simple frame, and rotate to global
    Coords simple_vector = coords_set(0,1,0);
    
    int inside = 1;
    
    if (height*0.5 < fabs(rotated_coordinates.y)) {
            if (verbal == 1) printf("point sticks out height wise \n");
            inside = 0;
    } else {

        // Test for separation radially
        // if (rSum − |Delta − Dot(W0,Delta)∗W0| < 0) seperated = 1;
        // double vector_between_cone_axis[3];
        // vector_between_cone_axis[0] = delta.x - scalar_prod1*vector1.x;
        // vector_between_cone_axis[1] = delta.y - scalar_prod1*vector1.y;
        // vector_between_cone_axis[2] = delta.z - scalar_prod1*vector1.z;

        Coords vector1 = coords_sub(rotated_coordinates,center);
        //printf("\nrotated coordinates = [%f,%f,%f]",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z);

        // Calculate radius at the y height of the tested point
        double cone_slope = (radius_top-radius_bottom)/height;
        double radius_pos = radius_bottom + cone_slope * (rotated_coordinates.y + (height/2.0)); // Here delta.y is used. Make sure that y is in the height direction of cone...
        
        //printf("\nradius_pos = %f",radius_pos);
        //printf("\nradius_pos distance = %f",sqrt(rotated_coordinates.x*rotated_coordinates.x+rotated_coordinates.z*rotated_coordinates.z));

        

        if (radius_pos *radius_pos < (rotated_coordinates.x*rotated_coordinates.x+rotated_coordinates.z*rotated_coordinates.z)) {
            if (verbal == 1) printf("Point sticks out radially \n");
            inside = 0;
        }
        //printf("\n IS INSIDE? %i",inside);
    }

    if (inside == 0) return 0;
    else return 1;
    };

int sample_cone_intersect(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry) {

    /*
    double radius_top = geometry->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius_bottom = geometry->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height = geometry->geometry_parameters.p_cone_storage->height;
    */

    double radius_top = geometry->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius_bottom = geometry->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height = geometry->geometry_parameters.p_cone_storage->height;
    
    
    
    //Coords direction = geometry->geometry_parameters.p_cone_storage->direction_vector;
    Coords center = geometry->center;

    Coords direction = coords_set(0,1,0);



    //Coords bottom_point = coords_add(center,coords_scalar_mult(direction,-0.5*height));
    //Coords top_point = coords_add(center,coords_scalar_mult(direction,0.5*height));

    // Declare variables for the function
    double x_new,y_new,z_new;
    
    // Coordinate transformation
    x_new = r[0] - geometry->center.x;
    y_new = r[1] - geometry->center.y;
    z_new = r[2] - geometry->center.z;
    
    Coords coordinates = coords_set(x_new,y_new,z_new);
    Coords rotated_coordinates;
    // printf("Cords coordinates = (%f,%f,%f)\n",coordinates.x,coordinates.y,coordinates.z);
    
    // debug
    // Rotation rotation_matrix_debug[3][3];
    // rot_set_rotation(rotation_matrix_debug,-1.0*geometry->rotation.x,-1.0*geometry->rotation.y,-1.0*geometry->rotation.z);
    // rot_transpose(geometry->rotation_matrix,rotation_matrix_debug);

    // Rotate the position of the neutron around the center of the cone
    rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,coordinates);
    // rotated_coordinates = rot_apply(rotation_matrix_debug,coordinates);
    //     printf("Cords rotated_coordinates = (%f,%f,%f)\n",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z);
    
    Coords velocity = coords_set(v[0],v[1],v[2]);
    Coords rotated_velocity;
    //     printf("Cords velocity = (%f,%f,%f)\n",velocity.x,velocity.y,velocity.z);
    
    // Rotate the position of the neutron around the center of the cone
    rotated_velocity = rot_apply(geometry->transpose_rotation_matrix,velocity);
    // rotated_velocity = rot_apply(rotation_matrix_debug,velocity);
    //     printf("Cords rotated_velocity = (%f,%f,%f)\n",rotated_velocity.x,rotated_velocity.y,rotated_velocity.z);
    

    
    // Test if the ray gets close to the cone by making a sphere around cone and check intersection
    double Y;
    double max_r;

    Y = -(0.5*height)-(radius_top*radius_top-radius_bottom*radius_bottom)/(2*height);
    if (radius_top > radius_bottom){
        max_r = radius_top;
    }else{
        max_r = radius_bottom;
    }
    double sphere_radius =  sqrt((Y+(1/2)*height)*(Y+(1/2)*height)+max_r*max_r);
    Coords sphere_pos = coords_set(center.x+direction.x*Y,center.y+direction.y*Y,center.z+direction.z*Y);
    
    double x_sphere = sphere_pos.x - geometry->center.x;
    double y_sphere = sphere_pos.y - geometry->center.y;
    double z_sphere = sphere_pos.z - geometry->center.z;
    double sphere_t[2];
    
    
    int output;
    // Run McXtrace built in sphere intersect funtion (sphere centered around origin)
    if ((output = sphere_intersect(&sphere_t[0],&sphere_t[1],x_sphere,y_sphere,z_sphere,v[0],v[1],v[2],sphere_radius)) == 0)
    

    if (sphere_t[0] > -1){
        if (sphere_t[1] > -1){
            t[0] = -1;
            t[1] = -1;
            return 0;
        }
    }
    
    
    
    double tmp;
    

    // Check if the ray intersects with the top and bottom circles
    double t_plane[2];
    t_plane[0] = (height/2 - rotated_coordinates.y) / rotated_velocity.y;
    t_plane[1] = (-height/2 - rotated_coordinates.y) / rotated_velocity.y;
    *num_solutions = 2;
    // Reduce from infinite plane to circles
            //  sqrt(xpos^2 + zpos^2) > r  => t = -1
    double xpos;
    double zpos;
    xpos=rotated_coordinates.x+t_plane[0]*rotated_velocity.x;
        zpos=rotated_coordinates.z+t_plane[0]*rotated_velocity.z;
 

    if ((xpos*xpos + zpos*zpos) > radius_top*radius_top){
        t_plane[0] = -1;
        *num_solutions = *num_solutions-1;
    }
    xpos=rotated_coordinates.x+t_plane[1]*rotated_velocity.x;
    zpos=rotated_coordinates.z+t_plane[1]*rotated_velocity.z;

    if ((xpos*xpos + zpos*zpos) > radius_bottom*radius_bottom){
        t_plane[1] = -1;
        *num_solutions = *num_solutions-1;
    }

    
    // sort solutions:
        if (t_plane[0]>t_plane[1]){
            tmp = t_plane[1];
            t_plane[1] = t_plane[0];
            t_plane[0] = tmp;
        }

    
    if (*num_solutions == 2){
        // Intersect only on planes
        t[0] = t_plane[0];
        t[1] = t_plane[1];
        
        
    } else {
        // Intersects with cone
            // Intersection with cone:
            // solve the equation: t*A+sqrt(t*B)+C = 0
        tmp = (rotated_velocity.y*radius_top/height-rotated_velocity.y*radius_bottom/height);
        double A = rotated_velocity.x*rotated_velocity.x+rotated_velocity.z*rotated_velocity.z-tmp*tmp;
        double B = 2*rotated_velocity.x*rotated_coordinates.x+2*rotated_velocity.z*rotated_coordinates.z-(2*(rotated_velocity.y*radius_top/height-rotated_velocity.y*radius_bottom/height))*((0.5)*radius_bottom+rotated_coordinates.y*radius_top/height-rotated_coordinates.y*radius_bottom/height+radius_top/2);
        tmp = (radius_bottom/2+rotated_coordinates.y*radius_top/height-rotated_coordinates.y*radius_bottom/height+radius_top/2);
        double C = rotated_coordinates.x*rotated_coordinates.x+rotated_coordinates.z*rotated_coordinates.z-tmp*tmp;
        double t_cone[2];
               t_cone[1]= -(B+sqrt(-4*A*C+B*B))/(2*A);
            t_cone[0]= -(B-sqrt(-4*A*C+B*B))/(2*A);
        //solve_2nd_order(&t_cone[0], &t_cone[1], A, B, C);

        // remove solutions on cone over top and under bottom
        if (fabs(t_cone[0]*rotated_velocity.y+rotated_coordinates.y) > height/2) {
            t_cone[0] = -1;
        }
        if (fabs(t_cone[1]*rotated_velocity.y+rotated_coordinates.y) > height/2) {
            t_cone[1] = -1;
        }

        
        // sort solutions:
        if (t_cone[0]>t_cone[1]){
            tmp = t_cone[1];
            t_cone[1] = t_cone[0];
            t_cone[0] = tmp;
        }
        
        if (*num_solutions == 1){
            t[0] = t_cone[1];
            t[1] = t_plane[1];
        }
        if (*num_solutions == 0){
            t[0] = t_cone[0];
            t[1] = t_cone[1];
        }
    }


    // Count solutions
    *num_solutions == 0;
    if (t[0] > 0){
        *num_solutions += 1;
    }else {
        t[0]=-1;
    }
    if (t[1] > 0){
        *num_solutions += 1;
    }else {
        t[1]=-1;
    }
    
    
    if (t[0] > t[1]) {
        tmp = t[1];
        t[1] = t[0];
        t[0] = tmp;
    }
switch(*num_solutions) {
    case 2:
        return 1;
    case 1:
        t[0] = -1;
        return 1;
    case 0:
        t[0] = -1;
        t[1] = -1;
        return 0;
}



/*
    int output;
    // Run McStas built in sphere intersect funtion (sphere centered around origin)
    if ((output = cone_intersect(&t[0],&t[1],rotated_coordinates,rotated_velocity,radius_top,radius_bottom,height,is_cylinder,cone_tip,center)) == 0) {
        *num_solutions = 0;t[0]=-1;t[1]=-1;}
    else if (t[1] != 0) *num_solutions = 2;
    else {*num_solutions = 1;t[1]=-1;}
 
    return output;
*/
};

int cone_intersect(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry){
/*
This function takes the inputs from a photon and calculates all intersections with the cone geometry.
Output is true or false depending on intersections will occour, and a list of time-stapms of all possible intersections.

Math used here is based on the math found on:
http://lousodrome.net/blog/light/2017/01/03/intersection-of-a-ray-and-a-cone/
But has been modified to solve the problem within the syntax needed in Union

This function was created by Martin Olsen at NBI on september 20, 2018.
*/


    double radius_top = geometry->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius_bottom = geometry->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height = geometry->geometry_parameters.p_cone_storage->height;
    Coords direction = geometry->geometry_parameters.p_cone_storage->direction_vector;
    Coords center = geometry->center;
    
    Coords bottom_point = coords_add(center,coords_scalar_mult(direction,-0.5*height));
    Coords top_point = coords_add(center,coords_scalar_mult(direction,0.5*height));

/*
    // check if this is a cylinder
    int isCylinder = 0;
    if (radius_top==radius_bottom){
        isCylinder = 1;
    }


    // Intersection with a cone
    if (isCylinder == 0){
 

    }
    // Intersection with a cylinder
    if (isCylinder == 1){


    }
*/

};

int r_within_mesh(Coords pos,struct geometry_struct *geometry) {
// Unpack parameters

    Coords center = geometry->center;
    int n_facets = geometry->geometry_parameters.p_mesh_storage->n_facets;
    double *normal_x = geometry->geometry_parameters.p_mesh_storage->normal_x;
    double *normal_y = geometry->geometry_parameters.p_mesh_storage->normal_y;
    double *normal_z = geometry->geometry_parameters.p_mesh_storage->normal_z;
    double *v1_x = geometry->geometry_parameters.p_mesh_storage->v1_x;
    double *v1_y = geometry->geometry_parameters.p_mesh_storage->v1_y;
    double *v1_z = geometry->geometry_parameters.p_mesh_storage->v1_z;
    double *v2_x = geometry->geometry_parameters.p_mesh_storage->v2_x;
    double *v2_y = geometry->geometry_parameters.p_mesh_storage->v2_y;
    double *v2_z = geometry->geometry_parameters.p_mesh_storage->v2_z;
    double *v3_x = geometry->geometry_parameters.p_mesh_storage->v3_x;
    double *v3_y = geometry->geometry_parameters.p_mesh_storage->v3_y;
    double *v3_z = geometry->geometry_parameters.p_mesh_storage->v3_z;
    
    double x_new,y_new,z_new;
    
    // Coordinate transformation
    x_new = pos.x - geometry->center.x;
    y_new = pos.y - geometry->center.y;
    z_new = pos.z - geometry->center.z;
    
    Coords coordinates = coords_set(x_new,y_new,z_new);
    Coords rotated_coordinates;
   
    rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,coordinates);

    
    int verbal = 0;
    // Generate unit direction vector along center axis of meshs
    
    // Start with vector that points along the mesh in the simple frame, and rotate to global
    Coords simple_vector = coords_set(0,1,0);
    Coords test_vector = coords_set(0,1,0);
    // Check intersections with every single facet:
    int iter =0;
    int counter=0; int neg_counter=0;
    Coords edge1,edge2,h,s,q,tmp,intersect_pos;
    double EPSILON = 1e-27;
    double this_facet_t;
    double a,f,u,V;
    //////printf("\n RWITHIN TEST 1ste");
    for (iter = 0 ; iter < n_facets ; iter++){
    /*//////printf("\n\n facet v1 = [%f,%f,%f]",v1_x[iter],v1_y[iter],v1_z[iter]);
    //////printf("\n facet v2 = [%f,%f,%f]",v2_x[iter],v2_y[iter],v2_z[iter]);
    //////printf("\n facet v3 = [%f,%f,%f]",v3_x[iter],v3_y[iter],v3_z[iter]);*/
        // Intersection with face plane (Möller–Trumbore)
        edge1 = coords_set(*(v2_x+iter)-*(v1_x+iter),*(v2_y+iter)-*(v1_y+iter),*(v2_z+iter)-*(v1_z+iter));
            //////printf("\n edge 1 = [%f,%f,%f]",edge1.x,edge1.y,edge1.z);
        edge2 = coords_set(*(v3_x+iter)-*(v1_x+iter),*(v3_y+iter)-*(v1_y+iter),*(v3_z+iter)-*(v1_z+iter));
        
        vec_prod(h.x,h.y,h.z,test_vector.x,test_vector.y,test_vector.z,edge2.x,edge2.y,edge2.z);
        
        a = Dot(edge1,h);
        //////printf("\n a=%f",a);
        if (a > -EPSILON && a < EPSILON){
            //////printf("\n Epsilon fail");
        } else{
            f = 1.0/a;
            s = coords_sub(rotated_coordinates, coords_set(*(v1_x+iter),*(v1_y+iter),*(v1_z+iter)));
            u = f * (Dot(s,h));
            if (u < 0.0 || u > 1.0){
                //////printf("\n Nope 1");
            }else{
                //q = vec_prod(s,edge1);
                vec_prod(q.x,q.y,q.z,s.x,s.y,s.z,edge1.x,edge1.y,edge1.z);
                V = f * Dot(test_vector,q);
                if (V < 0.0 || u + V > 1.0){
                    //////printf("\n Nope 2");
                } else {
                    // At this stage we can compute t to find out where the intersection point is on the line.
                    if (f* Dot(q,edge2) > 0){
                        counter++;

                    } else {
                        neg_counter++;

                    }
                    if (fabs(f* Dot(q,edge2)) > EPSILON){
                    } else { //printf("\n [%f %f %f] Failed due to being close to surface, E = %f",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z,f* Dot(q,edge2));
                     }
                    
                    
                    
                }
            }
        }
    }
    int C1 = counter;
    
    int maxC; int sameNr =0;
    ////printf("\n first iter: (%i , %i)",counter,neg_counter);
    if (counter % 2 == neg_counter % 2){
        maxC = counter;
        sameNr = 1;
    } else {
        //printf("\n not the same intersection numbers (%i , %i)",counter,neg_counter);
        maxC = counter;
        sameNr = 0;
    }

 if (sameNr == 0){
     test_vector = coords_set(0,0,1);
    iter =0;
    counter=0;
    //////printf("\n RWITHIN TEST 1ste");
    for (iter = 0 ; iter < n_facets ; iter++){
    ///////printf("\n\n facet v1 = [%f,%f,%f]",v1_x[iter],v1_y[iter],v1_z[iter]);
    //////printf("\n facet v2 = [%f,%f,%f]",v2_x[iter],v2_y[iter],v2_z[iter]);
    //////printf("\n facet v3 = [%f,%f,%f]",v3_x[iter],v3_y[iter],v3_z[iter]);
        // Intersection with face plane (Möller–Trumbore)
        edge1 = coords_set(*(v2_x+iter)-*(v1_x+iter),*(v2_y+iter)-*(v1_y+iter),*(v2_z+iter)-*(v1_z+iter));
            //////printf("\n edge 1 = [%f,%f,%f]",edge1.x,edge1.y,edge1.z);
        edge2 = coords_set(*(v3_x+iter)-*(v1_x+iter),*(v3_y+iter)-*(v1_y+iter),*(v3_z+iter)-*(v1_z+iter));
        
        vec_prod(h.x,h.y,h.z,test_vector.x,test_vector.y,test_vector.z,edge2.x,edge2.y,edge2.z);
        
        a = Dot(edge1,h);
        //////printf("\n a=%f",a);
        if (a > -EPSILON && a < EPSILON){
            //////printf("\n Epsilon fail");
        } else{
            f = 1.0/a;
            s = coords_sub(rotated_coordinates , coords_set(*(v1_x+iter),*(v1_y+iter),*(v1_z+iter)));
            u = f * (Dot(s,h));
            if (u < 0.0 || u > 1.0){
                //////printf("\n Nope 1");
            }else{
                //q = vec_prod(s,edge1);
                vec_prod(q.x,q.y,q.z,s.x,s.y,s.z,edge1.x,edge1.y,edge1.z);
                V = f * Dot(test_vector,q);
                if (V < 0.0 || u + V > 1.0){
                    //////printf("\n Nope 2");
                } else {
                    // At this stage we can compute t to find out where the intersection point is on the line.

                    if (f* Dot(q,edge2) > 0){
                        counter++;

                    } else {
                        neg_counter++;

                    }
                    if (fabs(f* Dot(q,edge2)) > EPSILON){
                    } else { printf("\n [%f %f %f] Failed due to being close to surface (2. iteration), E = %f",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z,f* Dot(q,edge2));
                     }
                    
                    
                    
                }
            }
        }
    }
    }
    
    if (counter % 2 == neg_counter % 2){
        maxC = counter;
        sameNr = 1;
    } else {
        printf("\n not the same intersection numbers (%i , %i) second iteration",counter,neg_counter);
        maxC = counter;
        sameNr = 0;
    }

    if (sameNr == 0){
     test_vector = coords_set(1,0,0);
    iter =0;
    counter=0;
    //////printf("\n RWITHIN TEST 1ste");
    for (iter = 0 ; iter < n_facets ; iter++){
    ///////printf("\n\n facet v1 = [%f,%f,%f]",v1_x[iter],v1_y[iter],v1_z[iter]);
    //////printf("\n facet v2 = [%f,%f,%f]",v2_x[iter],v2_y[iter],v2_z[iter]);
    //////printf("\n facet v3 = [%f,%f,%f]",v3_x[iter],v3_y[iter],v3_z[iter]);
        // Intersection with face plane (Möller–Trumbore)
        edge1 = coords_set(*(v2_x+iter)-*(v1_x+iter),*(v2_y+iter)-*(v1_y+iter),*(v2_z+iter)-*(v1_z+iter));
            //////printf("\n edge 1 = [%f,%f,%f]",edge1.x,edge1.y,edge1.z);
        edge2 = coords_set(*(v3_x+iter)-*(v1_x+iter),*(v3_y+iter)-*(v1_y+iter),*(v3_z+iter)-*(v1_z+iter));
        
        vec_prod(h.x,h.y,h.z,test_vector.x,test_vector.y,test_vector.z,edge2.x,edge2.y,edge2.z);
        
        a = Dot(edge1,h);
        //////printf("\n a=%f",a);
        if (a > -EPSILON && a < EPSILON){
            //////printf("\n Epsilon fail");
        } else{
            f = 1.0/a;
            s = coords_sub(rotated_coordinates , coords_set(*(v1_x+iter),*(v1_y+iter),*(v1_z+iter)));
            u = f * (Dot(s,h));
            if (u < 0.0 || u > 1.0){
                //////printf("\n Nope 1");
            }else{
                //q = vec_prod(s,edge1);
                vec_prod(q.x,q.y,q.z,s.x,s.y,s.z,edge1.x,edge1.y,edge1.z);
                V = f * Dot(test_vector,q);
                if (V < 0.0 || u + V > 1.0){
                    //////printf("\n Nope 2");
                } else {
                    // At this stage we can compute t to find out where the intersection point is on the line.

                    if (f* Dot(q,edge2) > 0){
                        counter++;

                    } else {
                        neg_counter++;

                    }
                    if (fabs(f* Dot(q,edge2)) > EPSILON){
                    } else { printf("\n [%f %f %f] Failed due to being close to surface (3. iteration), E = %f",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z,f* Dot(q,edge2));
                    }
                    
                    
                    
                }
            }
        }
    }

    }
   
    
    if (counter % 2 == neg_counter % 2){
        maxC = counter;
    } else {
        //printf("\n not the same intersection numbers (3. iteration) (%i , %i)",counter,neg_counter);
        //printf("\n this point is a bitch: [%f %f %f]",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z);
        return 0;
        
    }
    ////printf("\n test point: [%f %f %f]",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z);

////printf("\n maxC: %i",maxC);

    if ( maxC % 2 == 0) {
        ////printf("\n Even number of intersections,  %i",counter);
        return 0;
    }else{
        ////printf("\n Odd number of intersections, INSIDE! %i",counter);
        return 1;
        
    }
    
    return 0;
    };


int sample_mesh_intersect(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry) {

    /*
    double radius_top = geometry->geometry_parameters.p_mesh_storage->mesh_radius_top;
    double radius_bottom = geometry->geometry_parameters.p_mesh_storage->mesh_radius_bottom;
    double height = geometry->geometry_parameters.p_mesh_storage->height;
    */


    int n_facets = geometry->geometry_parameters.p_mesh_storage->n_facets;
    double *normal_x = geometry->geometry_parameters.p_mesh_storage->normal_x;
    double *normal_y = geometry->geometry_parameters.p_mesh_storage->normal_y;
    double *normal_z = geometry->geometry_parameters.p_mesh_storage->normal_z;
    double *v1_x = geometry->geometry_parameters.p_mesh_storage->v1_x;
    double *v1_y = geometry->geometry_parameters.p_mesh_storage->v1_y;
    double *v1_z = geometry->geometry_parameters.p_mesh_storage->v1_z;
    double *v2_x = geometry->geometry_parameters.p_mesh_storage->v2_x;
    double *v2_y = geometry->geometry_parameters.p_mesh_storage->v2_y;
    double *v2_z= geometry->geometry_parameters.p_mesh_storage->v2_z;
    double *v3_x = geometry->geometry_parameters.p_mesh_storage->v3_x;
    double *v3_y = geometry->geometry_parameters.p_mesh_storage->v3_y;
    double *v3_z = geometry->geometry_parameters.p_mesh_storage->v3_z;
    Coords Bounding_Box_Center = geometry->geometry_parameters.p_mesh_storage->Bounding_Box_Center;
    double Bounding_Box_Radius = geometry->geometry_parameters.p_mesh_storage->Bounding_Box_Radius;
    
    
    int i;
    
    
    
    
    
    
    //////printf("\n\n  TEST facet v2 = [%f,%f,%f]",v2_x[1],v2_y[1],v2_z[1]);
    for (i = 0 ; i< n_facets ; i++){
    
    ////printf("\n\n  TEST facet v2 = [%f,%f,%f]",*(v2_x+i),*(v2_y+i),*(v2_z+i));
    }
    
    
    //Coords direction = geometry->geometry_parameters.p_mesh_storage->direction_vector;
    Coords center = geometry->center;

    Coords direction = coords_set(0,1,0);



    //Coords bottom_point = coords_add(center,coords_scalar_mult(direction,-0.5*height));
    //Coords top_point = coords_add(center,coords_scalar_mult(direction,0.5*height));

    // Declare variables for the function
    double x_new,y_new,z_new;
    
    // Coordinate transformation
    x_new = r[0] - geometry->center.x;
    y_new = r[1] - geometry->center.y;
    z_new = r[2] - geometry->center.z;
    
    double x_bb,y_bb,z_bb;
    /*
    x_bb = r[0] - Bounding_Box_Center.x;
    y_bb = r[1] - Bounding_Box_Center.y;
    z_bb = r[2] - Bounding_Box_Center.z;
    */
    
    x_bb = r[0] - Bounding_Box_Center.x - geometry->center.x;
    y_bb = r[1] - Bounding_Box_Center.y - geometry->center.y;
    z_bb = r[2] - Bounding_Box_Center.z - geometry->center.z;
    
    Coords coordinates = coords_set(x_new,y_new,z_new);
    Coords rotated_coordinates;
    // ////printf("Cords coordinates = (%f,%f,%f)\n",coordinates.x,coordinates.y,coordinates.z);
    
    // debug
    // Rotation rotation_matrix_debug[3][3];
    // rot_set_rotation(rotation_matrix_debug,-1.0*geometry->rotation.x,-1.0*geometry->rotation.y,-1.0*geometry->rotation.z);
    // rot_transpose(geometry->rotation_matrix,rotation_matrix_debug);

    // Rotate the position of the photon around the center of the mesh
    rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,coordinates);
    // rotated_coordinates = rot_apply(rotation_matrix_debug,coordinates);
    //     ////printf("Cords rotated_coordinates = (%f,%f,%f)\n",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z);
    
    Coords bounding_box_coordinates = coords_set(x_bb, y_bb, z_bb);
    Coords bounding_box_rotated_coordinates;
    
    bounding_box_rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,bounding_box_coordinates);
    
    
    Coords velocity = coords_set(v[0],v[1],v[2]);
    Coords rotated_velocity;
    //     ////printf("Cords velocity = (%f,%f,%f)\n",velocity.x,velocity.y,velocity.z);
    
    // Rotate the position of the neutron around the center of the mesh
    rotated_velocity = rot_apply(geometry->transpose_rotation_matrix,velocity);
    // rotated_velocity = rot_apply(rotation_matrix_debug,velocity);
    //     ////printf("Cords rotated_velocity = (%f,%f,%f)\n",rotated_velocity.x,rotated_velocity.y,rotated_velocity.z);
    
//printf("Is testing for intersections!\n");
    int output;
    double tmpres[2];
    // Test intersection with bounding sphere // if ((output = sphere_intersect(&t[0],&t[1],x_new,y_new,z_new,v[0],v[1],v[2],radius)) == 0) {
    //if ((output = sphere_intersect(&tmpres[0],&tmpres[1],x_bb,y_bb,z_bb,rotated_velocity.x,rotated_velocity.y,rotated_velocity.z,Bounding_Box_Radius)) == 0) {
    if ((output = sphere_intersect(&tmpres[0],&tmpres[1],bounding_box_rotated_coordinates.x,bounding_box_rotated_coordinates.y,bounding_box_rotated_coordinates.z,
                                   rotated_velocity.x,rotated_velocity.y,rotated_velocity.z,Bounding_Box_Radius)) == 0) {
        t[0] = -1;
        t[1] = -1;
        *num_solutions = 0;
        return 0;
    }

    
    // Check intersections with every single facet:
    int iter =0;
    int counter=0;
    Coords edge1,edge2,h,s,q,tmp,intersect_pos;
    double EPSILON = 0.0000001;
    double this_facet_t;
    double a,f,u,V,t_intersect[n_facets];
    *num_solutions = 0;
    for (iter = 0 ; iter < n_facets ; iter++){
    /*////printf("\n\n facet v1 = [%f,%f,%f]",v1_x[iter],v1_y[iter],v1_z[iter]);
    ////printf("\n facet v2 = [%f,%f,%f]",v2_x[iter],v2_y[iter],v2_z[iter]);
    ////printf("\n facet v3 = [%f,%f,%f]",v3_x[iter],v3_y[iter],v3_z[iter]);*/
        // Intersection with face plane (Möller–Trumbore)
        edge1 = coords_set(*(v2_x+iter)-*(v1_x+iter),*(v2_y+iter)-*(v1_y+iter),*(v2_z+iter)-*(v1_z+iter));
            ////printf("\n edge 1 = [%f,%f,%f]",edge1.x,edge1.y,edge1.z);
        edge2 = coords_set(*(v3_x+iter)-*(v1_x+iter),*(v3_y+iter)-*(v1_y+iter),*(v3_z+iter)-*(v1_z+iter));
        //h = vec_prod(rotated_velocity,edge2);
        vec_prod(h.x,h.y,h.z,rotated_velocity.x,rotated_velocity.y,rotated_velocity.z,edge2.x,edge2.y,edge2.z);
        ////printf("\n h = [%f,%f,%f]",h.x,h.y,h.z);
        ////printf("\n rotated_velocity = [%f,%f,%f]",rotated_velocity.x,rotated_velocity.y,rotated_velocity.z);
        ////printf("\n edge2 = [%f,%f,%f]",edge2.x,edge2.y,edge2.z);
        //h = coord_set(rotated_velocity.y*edge2.z-rotated_velocity.z*edge2.y, rotated_velocity.z*edge2.x-rotated_velocity.x*edge2.z, rotated_velocity.x*edge2.y-rotated_velocity.y*edge2.x);
        //a = Dot(h,edge1);
        a = Dot(edge1,h);
        ////printf("\n a=%f",a);
        //if (a > -EPSILON && a < EPSILON){
            ////printf("\n Epsilon fail");
        //} else{
            f = 1.0/a;
            s = coords_sub(rotated_coordinates, coords_set(*(v1_x+iter),*(v1_y+iter),*(v1_z+iter)));
            u = f * (Dot(s,h));
            if (u < 0.0 || u > 1.0){
                ////printf("\n Nope 1");
            }else{
                //q = vec_prod(s,edge1);
                vec_prod(q.x,q.y,q.z,s.x,s.y,s.z,edge1.x,edge1.y,edge1.z);
                V = f * Dot(rotated_velocity,q);
                if (V < 0.0 || u + V > 1.0){
                    ////printf("\n Nope 2");
                } else {
                    // At this stage we can compute t to find out where the intersection point is on the line.
                    //tmp = Dot(q,edge2)
                    ////printf("\nt inside loop = %f",f* Dot(q,edge2));
                    if (f* Dot(q,edge2) > 0){
                        
                        t_intersect[counter] = f* Dot(q,edge2);
                        //printf("\nIntersects at time: t= %f\n",t_intersect[counter] );
                        counter++;
                        
                        //intersect_pos = coords_set(rotated_coordinates.x+t_intersect[counter]*rotated_velocity.x,rotated_coordinates.y+t_intersect[counter]*rotated_velocity.y,rotated_coordinates.z+t_intersect[counter]*rotated_velocity.z);
                        //////printf("\n intersects at [%f,%f,%f]",intersect_pos.x,intersect_pos.y,intersect_pos.z);
                    }
                    
                    
                    
                }
            }
        //}
    }
    
    // find two smallest non-zero intersections:
    /*
    double t_min = -1.0;
    double t_second_min= -1.0;
    for (iter = 0 ; iter < counter; iter++){
        // test
        if (t_min == -1 && t_intersect[iter] > 0.0){
            t_min = t_intersect[iter];

        } else{
            if (t_intersect[iter] > 0.0 && t_intersect[iter] < t_min) {
                t_second_min = t_min;
                t_min = t_intersect[iter];
     
            }
            if (t_intersect[iter] > 0.0 && t_intersect[iter] > t_min) {
                if (t_intersect[iter] < t_second_min || t_second_min == -1.0){
                    t_second_min = t_intersect[iter];
                }
            }
        }
     
     
    }
    
    //printf("\n number of intersections: %i\n",counter);
    
    
    
    
    if (t_second_min > 0) {
        if (counter % 2 == 0){
            t[0] = t_second_min;
            t[1] = t_min;
            *num_solutions = 2;
            //printf("\n t[0] = %f   t[1] = %f \n",t_min,t_second_min);
        } else{
            t[0] = -1;
            t[1] = t_min;
            *num_solutions = 1;
            //printf("\n t[0] = %f   t[1] = %f \n",t_min,t_second_min);
        }
            ////printf("\n t[0] = %f   t[1] = %f \n",t[0],t[1]);
     
            //printf("\n num_solutions: %i\n",*num_solutions);
            return 1;
    }else if (t_min>0) {
            t[0] = t_min;
            t[1] = -1;
            *num_solutions = 1;
            //printf("\n num_solutions: %i\n",*num_solutions);
            return 1;
    } else {
            t[0] = -1;
            t[1] = -1;
            *num_solutions = 0;
            //printf("\n num_solutions: %i\n",*num_solutions);
            return 0;
    }
    return 0;
    */


    //for (iter=0;iter<99;iter++){
    //    printf("\n t[%i] = %f",iter,t[iter]);
    //    t[iter] = -1;
    //}
    
    // Return all t
    int counter2=0;
    *num_solutions =0;
    for (iter=0; iter < counter ; iter++){
        if (t_intersect[iter] > 0.0){
            t[counter2] = t_intersect[iter];
            counter2++;
            *num_solutions = counter2;
        }
    }
    // Sort t:
    
    if (*num_solutions == 0){
        return 0;
    }
    qsort(t,*num_solutions,sizeof (double), Sample_compare_doubles);
    if (*num_solutions > 2){
        //printf("\n T(0) = %f T(1) = %f  T(2) = %f T(3) = %f T(4) = %f",t[0],t[1],t[2],t[3],t[4]);
        //printf("\n TEST");
    }
    return 1;
    
};


// #include "MeshFunctions/mesh_intersect.c" // Empty function from Martin?

int r_within_box_advanced(Coords pos,struct geometry_struct *geometry) {
    // Unpack parameters
    double width1 = geometry->geometry_parameters.p_box_storage->x_width1;
    double height1 = geometry->geometry_parameters.p_box_storage->y_height1;
    double width2 = geometry->geometry_parameters.p_box_storage->x_width2;
    double height2 = geometry->geometry_parameters.p_box_storage->y_height2;
    double depth = geometry->geometry_parameters.p_box_storage->z_depth;
    
    // Transform to the center
    Coords coordinates = coords_sub(pos,geometry->center);
    
    Coords rotated_coordinates;
    // printf("Cords coordinates = (%f,%f,%f)\n",coordinates.x,coordinates.y,coordinates.z);
    
    // Rotate the position of the neutron around the center of the cylinder
    rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,coordinates);
    
    if (rotated_coordinates.z < -0.5*depth || rotated_coordinates.z > 0.5*depth) return 0;
    
    double depth_ratio = ((rotated_coordinates.z+0.5*depth)/depth);
    double width_at_depth = width1 + (width2-width1)*depth_ratio;
    if (rotated_coordinates.x < -0.5*width_at_depth || rotated_coordinates.x > 0.5*width_at_depth) return 0;
    
    double height_at_depth = height1 + (height2-height1)*depth_ratio;
    if (rotated_coordinates.y < -0.5*height_at_depth || rotated_coordinates.y > 0.5*height_at_depth) return 0;
    
    return 1;
};

// -------------    Functions for box ray tracing used in initialize ----------------------------
// These functions does not need to be fast, as they are only used once
int box_within_box(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Is geometry child inside geometry parent?
    // For box child to be inside of box parent, all corners of box child must be inside of box parent.
    
    // Generate coordinates of corners of box2
    Coords corner_points[8];
    box_corners_global_frame(corner_points,geometry_child);
    
    // Check earch corner seperatly
    int iterate;
    for (iterate=0;iterate<8;iterate++) {
        if (geometry_parent->within_function(corner_points[iterate],geometry_parent) == 0) {
            return 0; // If a corner is outside, box 2 is not within box 1
        }
    }
    return 1; // If no corner was outside, box 2 is inside box 1
};

int existence_of_intersection(Coords point1, Coords point2, struct geometry_struct *geometry) {
    Coords vector_between = coords_sub(point2,point1);
    double start_point[3],vector_between_v[3];
    double temp_solution[2];
    int number_of_solutions;

    start_point[0] = point1.x;start_point[1] = point1.y;start_point[2] = point1.z;
    vector_between_v[0] = vector_between.x;vector_between_v[1] = vector_between.y;vector_between_v[2] = vector_between.z;
    geometry->intersect_function(temp_solution,&number_of_solutions,start_point,vector_between_v,geometry);
    if (number_of_solutions > 0) {
        if (temp_solution[0] > 0 && temp_solution[0] < 1) return 1;
        if (number_of_solutions == 2) {
            if (temp_solution[1] > 0 && temp_solution[1] < 1) return 1;
        }
    }
    return 0;
};

int box_overlaps_box(struct geometry_struct *geometry1,struct geometry_struct *geometry2) {
    // Algorithm for checking if two boxes overlap
    
    // First check if one is inside the other by checking corners and within the other
    
    // Generate coordinates of corners of box1
    Coords corner_points1[8];
    box_corners_global_frame(corner_points1,geometry1);
    
    // Check earch corner seperatly
    int iterate;
    for (iterate=0;iterate<8;iterate++) {
        if (geometry2->within_function(corner_points1[iterate],geometry2) == 1) {
            return 1; // If a corner of box 1 is inside box 2, the two boxes overlaps
        }
    }
    
    Coords corner_points2[8];
    box_corners_global_frame(corner_points2,geometry2);
    for (iterate=0;iterate<8;iterate++) {
        if (geometry1->within_function(corner_points2[iterate],geometry1) == 1) {
            return 1; // If a corner of box 2 is inside box 1, the two boxes overlaps
        }
    }
    
    // Check intersections for the lines between the corners of box1 and the box2 geometry
    // 12 sides to a box, if any one of them intersects, the volumes overlaps
    for (iterate=0;iterate<3;iterate++) { //
        if (existence_of_intersection(corner_points1[iterate],corner_points1[iterate+1],geometry2) == 1) return 1;
    }
    if (existence_of_intersection(corner_points1[3],corner_points1[0],geometry2) == 1) return 1;
    for (iterate=4;iterate<7;iterate++) {
        if (existence_of_intersection(corner_points1[iterate],corner_points1[iterate+1],geometry2) == 1) return 1;
    }
    if (existence_of_intersection(corner_points1[7],corner_points1[4],geometry2) == 1) return 1;
    for (iterate=0;iterate<4;iterate++) {
        if (existence_of_intersection(corner_points1[iterate],corner_points1[iterate+4],geometry2) == 1) return 1;
    }
    
    // Check intersections for the lines between the corners of box2 and the box1 geometry
    // 12 sides to a box, if any one of them intersects, the volumes overlaps
    for (iterate=0;iterate<3;iterate++) { //
        if (existence_of_intersection(corner_points2[iterate],corner_points2[iterate+1],geometry1) == 1) return 1;
    }
    if (existence_of_intersection(corner_points2[3],corner_points2[0],geometry1) == 1) return 1;
    for (iterate=4;iterate<7;iterate++) {
        if (existence_of_intersection(corner_points2[iterate],corner_points2[iterate+1],geometry1) == 1) return 1;
    }
    if (existence_of_intersection(corner_points2[7],corner_points2[4],geometry1) == 1) return 1;
    for (iterate=0;iterate<4;iterate++) {
        if (existence_of_intersection(corner_points2[iterate],corner_points2[iterate+4],geometry1) == 1) return 1;
    }
    
    // If none of the boxes corners are inside the other box, and none of the sides of the boxes intersect the other box, they do not overlap.
    return 0;
};


// -------------    Functions for sphere ray tracing used in trace ------------------------------
// These functions needs to be fast, as they may be used many times for each ray
int sample_sphere_intersect(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry) {
    double radius = geometry->geometry_parameters.p_sphere_storage->sph_radius;
    
    // Declare variables for the function
    double x_new,y_new,z_new;
    
    // Coordinate transformation
    x_new = r[0] - geometry->center.x;
    y_new = r[1] - geometry->center.y;
    z_new = r[2] - geometry->center.z;
    
    int output;
    // Run McXtrace built in sphere intersect funtion (sphere centered around origin)
    if ((output = sphere_intersect(&t[0],&t[1],x_new,y_new,z_new,v[0],v[1],v[2],radius)) == 0) {
        *num_solutions = 0;t[0]=-1;t[1]=-1;}
    else if (t[1] != 0) *num_solutions = 2;
    else {*num_solutions = 1;t[1]=-1;}
    
    return output;
};

int r_within_sphere(Coords pos,struct geometry_struct *geometry)
    {
    // Unpack parameters
    double radius = geometry->geometry_parameters.p_sphere_storage->sph_radius;
    
    // Calculate the distance between the center and the sphere, and the current position
    double distance = distance_between(pos,geometry->center);

    //printf("distance = %f\n",distance);
    //printf("radius   = %f\n",radius);
    //printf("current_position.x = %f,current_position.y = %f,current_position.z = %f\n",current_position.x,current_position.y,current_position.z);
    //printf("geometry.x = %f,geometry.y = %f,geometry.z = %f\n",geometry->center.x,geometry->center.y,geometry->center.z);
    //printf("return   = %d\n",(distance <= radius));
    
    return (distance < radius);
    };

// -------------    Functions for sphere ray tracing used in initialize -------------------------
// These functions does not need to be fast, as they are only used once
int sphere_overlaps_sphere(struct geometry_struct *geometry1,struct geometry_struct *geometry2) {
    // Unpack parameters
    double radius1 = geometry1->geometry_parameters.p_sphere_storage->sph_radius;
    double radius2 = geometry2->geometry_parameters.p_sphere_storage->sph_radius;

    // Calculate distance
    double distance = distance_between(geometry1->center,geometry2->center);
    
    // Return 0 if the spheres does not overlap, 1 if they do.
    // printf("Output from sphere_overlaps_sphere = %d \n",(distance <= (radius1 + radius2)));
    return (distance <= (radius1 + radius2));
};

int sphere_within_sphere(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Unpack parameters
    double radius_child = geometry_child->geometry_parameters.p_sphere_storage->sph_radius;
    double radius_parent = geometry_parent->geometry_parameters.p_sphere_storage->sph_radius;

    // Calculate distance
    double distance = distance_between(geometry_child->center,geometry_parent->center);
    
    // Return 1 if sphere child is within sphere parent, 0 if they do not.
    return (distance + radius_child <= radius_parent);
};

// -------------    Functions for cylinder ray tracing used in trace ------------------------------
// These functions needs to be fast, as they may be used many times for each ray
int sample_cylinder_intersect(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry) {
    double radius = geometry->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height = geometry->geometry_parameters.p_cylinder_storage->height;
    
    // Declare variables for the function
    double x_new,y_new,z_new;
    
    // Coordinate transformation
    x_new = r[0] - geometry->center.x;
    y_new = r[1] - geometry->center.y;
    z_new = r[2] - geometry->center.z;
    
    Coords coordinates = coords_set(x_new,y_new,z_new);
    Coords rotated_coordinates;
    // printf("Cords coordinates = (%f,%f,%f)\n",coordinates.x,coordinates.y,coordinates.z);
    
    // debug
    // Rotation rotation_matrix_debug[3][3];
    // rot_set_rotation(rotation_matrix_debug,-1.0*geometry->rotation.x,-1.0*geometry->rotation.y,-1.0*geometry->rotation.z);
    // rot_transpose(geometry->rotation_matrix,rotation_matrix_debug);

    // Rotate the position of the neutron around the center of the cylinder
    rotated_coordinates = rot_apply(geometry->transpose_rotation_matrix,coordinates);
    // rotated_coordinates = rot_apply(rotation_matrix_debug,coordinates);
    //     printf("Cords rotated_coordinates = (%f,%f,%f)\n",rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z);
    
    Coords velocity = coords_set(v[0],v[1],v[2]);
    Coords rotated_velocity;
    //     printf("Cords velocity = (%f,%f,%f)\n",velocity.x,velocity.y,velocity.z);
    
    // Rotate the position of the neutron around the center of the cylinder
    rotated_velocity = rot_apply(geometry->transpose_rotation_matrix,velocity);
    // rotated_velocity = rot_apply(rotation_matrix_debug,velocity);
    //     printf("Cords rotated_velocity = (%f,%f,%f)\n",rotated_velocity.x,rotated_velocity.y,rotated_velocity.z);
    
    
    
    // Cases where the velocity is parallel with the cylinder axis have given problems, and is checked for explicitly
    if (sqrt(rotated_velocity.x*rotated_velocity.x+rotated_velocity.z*rotated_velocity.z)/fabs(rotated_velocity.y) < 0.00001) {
      // The velocity is parallel with the cylinder axis. Either there is two solutions
      if (sqrt(rotated_coordinates.x*rotated_coordinates.x+rotated_coordinates.z*rotated_coordinates.z) > radius) {
        *num_solutions = 0;
        return 0;
      } else {
        *num_solutions = 2;
        t[0] = (0.5*height - rotated_coordinates.y)/rotated_velocity.y;
        t[1] = (-0.5*height - rotated_coordinates.y)/rotated_velocity.y;
        return 1;
      }
    }
    
    int output;
    // Run McXtrace built in cylinder intersect funtion (cylinder centered around origin and axis along y)
    if ((output = cylinder_intersect(&t[0],&t[1],rotated_coordinates.x,rotated_coordinates.y,rotated_coordinates.z,rotated_velocity.x,rotated_velocity.y,rotated_velocity.z,radius,height)) == 0) {
        *num_solutions = 0;t[0]=-1;t[1]=-1;}
    else if (t[1] != 0) *num_solutions = 2;
    else {*num_solutions = 1;t[1]=-1;}
    
    return output;
};

int r_within_cylinder(Coords pos,struct geometry_struct *geometry) {
// Unpack parameters
    double radius = geometry->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height = geometry->geometry_parameters.p_cylinder_storage->height;

    
    int verbal = 0;
    // Generate unit direction vector along center axis of cylinders
    
    // Start with vector that points along the cylinder in the simple frame, and rotate to global
    Coords simple_vector = coords_set(0,1,0);
    Coords vector1,vector2;
    if (verbal == 1) printf("Cords start_vector = (%f,%f,%f)\n",simple_vector.x,simple_vector.y,simple_vector.z);

    // Rotate the position of the neutron around the center of the cylinder
    vector1 = rot_apply(geometry->rotation_matrix,simple_vector);
    if (verbal == 1) printf("Cords vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector1.z);
    
    // The cylinders are parallel.
    int seperated = 0;
    //double delta[3];
    //delta[0] = geometry->center.x - r[0];
    //delta[1] = geometry->center.y - r[1];
    //delta[2] = geometry->center.z - r[2];
    
    Coords delta = coords_sub(geometry->center,pos);

    // Test for separation by height
    // if (h0Div2 + h1Div2 − |Dot(W0, Delta )| < 0) seperated = 1;
    
    if (verbal == 1) printf("vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector2.z);
    if (verbal == 1) printf("delta1 = (%f,%f,%f)\n",delta.x,delta.y,delta.z);
    double scalar_prod1 = scalar_prod(vector1.x,vector1.y,vector1.z,delta.x,delta.y,delta.z);
    if (verbal == 1) printf("scalar product = %f \n",scalar_prod1);
    if (verbal == 1) printf("height 1 = %f \n",height);
    
    int inside = 1;
    
    if (height*0.5 < fabs(scalar_prod1)) {
            if (verbal == 1) printf("point sticks out height wise \n");
            inside = 0;
    }

    // Test for separation radially
    // if (rSum − |Delta − Dot(W0,Delta)∗W0| < 0) seperated = 1;
    double vector_between_cyl_axis[3];
    vector_between_cyl_axis[0] = delta.x - scalar_prod1*vector1.x;
    vector_between_cyl_axis[1] = delta.y - scalar_prod1*vector1.y;
    vector_between_cyl_axis[2] = delta.z - scalar_prod1*vector1.z;
    if (verbal == 1) printf("vector_between = (%f,%f,%f)\n",vector_between_cyl_axis[0],vector_between_cyl_axis[1],vector_between_cyl_axis[2]);
    if (verbal == 1) printf("length of vector between = %f\n",length_of_3vector(vector_between_cyl_axis));
    
    if (radius < length_of_3vector(vector_between_cyl_axis)) {
            if (verbal == 1) printf("Point sticks out radially \n");
            inside = 0;
    }
    
    if (inside == 0) return 0;
    else return 1;
    };

// -------------    Functions for cylinder ray tracing used in initialize -------------------------
// These functions does not need to be fast, as they are only used once
int cylinder_overlaps_cylinder(struct geometry_struct *geometry1,struct geometry_struct *geometry2) {
    // Unpack parameters
    double radius1 = geometry1->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height1 = geometry1->geometry_parameters.p_cylinder_storage->height;
    double radius2 = geometry2->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height2 = geometry2->geometry_parameters.p_cylinder_storage->height;
    
    int verbal = 0;
    // Generate unit direction vector along center axis of cylinders
    
    // Start with vector that points along the cylinder in the simple frame, and rotate to global
    Coords simple_vector = coords_set(0,1,0);
    Coords vector1,vector2;
    if (verbal == 1) printf("Cords start_vector = (%f,%f,%f)\n",simple_vector.x,simple_vector.y,simple_vector.z);

    // Rotate the position of the neutron around the center of the cylinder
    vector1 = rot_apply(geometry1->rotation_matrix,simple_vector);
    if (verbal == 1) printf("Cords vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector1.z);
    // Rotate the position of the neutron around the center of the cylinder
    vector2 = rot_apply(geometry2->rotation_matrix,simple_vector);
    if (verbal == 1) printf("Cords vector2 = (%f,%f,%f)\n",vector2.x,vector2.y,vector2.z);
    
    // if vector1 and vector2 are parallel, the problem is simple, but if not complicated
    double cross_product1[3] = {0,0,0};
    // printf("%f\n",cross_product1[0]);
    // vec prod(&ax,&ay,&az,bx,by,bz, cx,cy,cz)
    vec_prod(cross_product1[0],cross_product1[1],cross_product1[2],vector1.x,vector1.y,vector1.z,vector2.x,vector2.y,vector2.z);
    // I get an error taking the adress of cross_product1[0], &cross_product1[0]. Took the pointer adresses instead. Works fine.
    if (verbal == 1) printf("cross_product = (%f,%f,%f)\n",cross_product1[0],cross_product1[1],cross_product1[2]);
    double cross_product_length = length_of_3vector(cross_product1);
    
    if (cross_product_length == 0) {
        // The cylinders are parallel.
        int seperated = 0;
        double delta[3];
        delta[0] = geometry1->center.x - geometry2->center.x;
        delta[1] = geometry1->center.y - geometry2->center.y;
        delta[2] = geometry1->center.z - geometry2->center.z;

        // Test for separation by height
        // if (h0Div2 + h1Div2 − |Dot(W0, Delta )| < 0) seperated = 1;
        
        if (verbal == 1) printf("vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector2.z);
        if (verbal == 1) printf("delta1 = (%f,%f,%f)\n",delta[0],delta[1],delta[2]);
        double scalar_prod1 = scalar_prod(vector1.x,vector1.y,vector1.z,delta[0],delta[1],delta[2]);
        if (verbal == 1) printf("scalar product = %f \n",scalar_prod1);
        if (verbal == 1) printf("height 1 = %f, height 2 = %f \n",height1,height2);
        if (height1*0.5 + height2*0.5 - fabs(scalar_prod1) < 0) {
                if (verbal == 1) printf("seperated by height \n");
                return 0;
                }
    
        // Test for separation radially
        // if (rSum − |Delta − Dot(W0,Delta)∗W0| < 0) seperated = 1;
        double vector_between_cyl_axis[3];
        vector_between_cyl_axis[0] = delta[0] - scalar_prod1*vector1.x;
        vector_between_cyl_axis[1] = delta[1] - scalar_prod1*vector1.y;
        vector_between_cyl_axis[2] = delta[2] - scalar_prod1*vector1.z;
        if (verbal == 1) printf("vector_between = (%f,%f,%f)\n",vector_between_cyl_axis[0],vector_between_cyl_axis[1],vector_between_cyl_axis[2]);
        if (verbal == 1) printf("length of vector between = %f\n",length_of_3vector(vector_between_cyl_axis));
        
        if (radius1+radius2 - length_of_3vector(vector_between_cyl_axis) < 0) {
                if (verbal == 1) printf("seperated radially \n");
                return 0;
                }
    
        if (verbal == 1) printf("cylinders not seperated\n");
        return 1;
        
    } else {
    
        // Todo: Speed up analysis by starting with a bounding sphere approach to avoid brute force in many cases
        
        
        // printf("The component uses a raytracing method for non parallel cylinders.\n");
        // printf("  Make sure not to give this algorithm edge cases, where cylinders just touch.\n");
        Coords cyl_direction1 = geometry1->geometry_parameters.p_cylinder_storage->direction_vector;
        
        // Doing a simple but not perfect overlap test

        // Checking cylinder sides.
        
        // Taking cylinder 1, making a vector at the base center.
        Coords base_point;
        
        base_point.x = geometry1->center.x - 0.5*height1*cyl_direction1.x;
        base_point.y = geometry1->center.y - 0.5*height1*cyl_direction1.y;
        base_point.z = geometry1->center.z - 0.5*height1*cyl_direction1.z;
        
        // Making a point at the circumference of the bottom circle of the cylinder
        double cross_input[3] = {0,1,0};
        // In case the cross input is parallel with the vector, a new is chosen. Both can't be parallel.
        if (scalar_prod(cross_input[0],cross_input[1],cross_input[2],cyl_direction1.x,cyl_direction1.y,cyl_direction1.z) > 0.99) {
            cross_input[0] = 1; cross_input[1] = 0; cross_input[2] = 0;
        }
        // print_position(make_position(cross_input),"cross input");
        
        double cross_product1[3] = {0,0,0};
        vec_prod(cross_product1[0],cross_product1[1],cross_product1[2],cyl_direction1.x,cyl_direction1.y,cyl_direction1.z,cross_input[0],cross_input[1],cross_input[2]);
        
        // print_position(make_position(cross_product1),"cross_product1");
        double cross_length = length_of_3vector(cross_product1);
        
        // printf("cross_length = %f \n",cross_length);
        cross_product1[0] /= cross_length;
        cross_product1[1] /= cross_length;
        cross_product1[2] /= cross_length;
        
        cross_product1[0] *= radius1;
        cross_product1[1] *= radius1;
        cross_product1[2] *= radius1;
        
        Coords circ_point;
        double radial_position[3],cyl_direction_pointer[3],base_point_vector[3],cyl_radial_direction[3];
        
        cyl_direction_pointer[0] = cyl_direction1.x;
        cyl_direction_pointer[1] = cyl_direction1.y;
        cyl_direction_pointer[2] = cyl_direction1.z;
        
        int iterate,number_of_solutions,solutions,number_of_positions = 300;
        double rotate_angle,temp_solution[2];
        
        // printf("length of cyl_direction_pointer = %f \n",length_of_3vector(cyl_direction_pointer));
        
        // Check intersection with cylinder 2 with that point, and cyl_direction, if there is an intersection before height1, they overlap.
        // Rotate the circumference point around the cyl_direction for a full circle to detect intersections all the way around.
        
        // Here cross_product1 is a vector from the base point to a point n the circumference
        // circ_point is a vector from the base point to the circumference rotated an angle.
        // radial_position is the actual position on the circumference on the cylinder as a vector from origo.
        for (iterate = 0;iterate < number_of_positions;iterate++) {
                rotate_angle = 2*3.14159*((double) iterate)/((double) number_of_positions);
                rotate(circ_point.x,circ_point.y,circ_point.z,cross_product1[0],cross_product1[1],cross_product1[2],rotate_angle,cyl_direction1.x,cyl_direction1.y,cyl_direction1.z);
            
                radial_position[0] = base_point.x + circ_point.x;
                radial_position[1] = base_point.y + circ_point.y;
                radial_position[2] = base_point.z + circ_point.z;
                // sample_cylinder_intersect(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry) {
                sample_cylinder_intersect(temp_solution,&number_of_solutions,radial_position,cyl_direction_pointer,geometry2);
                for (solutions = 0;solutions < number_of_solutions;solutions++) {
                    if (temp_solution[solutions] > 0 && temp_solution[solutions] < height1) {
                        // cylinders must overlap.
                        return 1;
                    }
                }
                if (number_of_solutions == 2) {
                    if (temp_solution[0] < 0 && temp_solution[1] > 0) return 1; // cylinder 1 inside cylinder 2
                    if (temp_solution[0] > 0 && temp_solution[1] < 0) return 1; // cylinder 1 inside cylinder 2
                }
            
                cyl_radial_direction[0] = circ_point.x;
                cyl_radial_direction[1] = circ_point.y;
                cyl_radial_direction[2] = circ_point.z;
                // Note it has length radius1
            
                base_point_vector[0] = base_point.x;
                base_point_vector[1] = base_point.y;
                base_point_vector[2] = base_point.z;
            
                // The vector circ_point is from the base to the circumference. This is used to check the bottom cap.
                sample_cylinder_intersect(temp_solution,&number_of_solutions,base_point_vector,cyl_radial_direction,geometry2);
                for (solutions = 0;solutions < number_of_solutions;solutions++) {
                    if (temp_solution[solutions] > 0 && temp_solution[solutions] < 1) {
                        // cylinders must overlap.
                        return 1;
                    }
                }
                if (number_of_solutions == 2) {
                    if (temp_solution[0] < 0 && temp_solution[1] > 0) return 1; // cylinder 1 inside cylinder 2
                    if (temp_solution[0] > 0 && temp_solution[1] < 0) return 1; // cylinder 1 inside cylinder 2
                }
            
                // Now check the top
                base_point_vector[0] = base_point.x + height1*cyl_direction1.x;
                base_point_vector[1] = base_point.y + height1*cyl_direction1.y;
                base_point_vector[2] = base_point.z + height1*cyl_direction1.z;
            
                // The vector circ_point is from the base to the circumference. This is used to check the bottom cap.
                sample_cylinder_intersect(temp_solution,&number_of_solutions,base_point_vector,cyl_radial_direction,geometry2);
                for (solutions = 0;solutions < number_of_solutions;solutions++) {
                    if (temp_solution[solutions] > 0 && temp_solution[solutions] < 1) {
                        // cylinders must overlap.
                        return 1;
                    }
                }
                if (number_of_solutions == 2) {
                    if (temp_solution[0] < 0 && temp_solution[1] > 0) return 1; // cylinder 1 inside cylinder 2
                    if (temp_solution[0] > 0 && temp_solution[1] < 0) return 1; // cylinder 1 inside cylinder 2
                }
        }
        // The above method is not perfect as it basicly tests a mesh grid of the cylinder aginst another perfect cylinder.
        // Can be improved.
        
        // If the entire perfect cylinder (cylinder 2) is within the meshgrid one, there will not be a solution to anything.
        // Check with a simple call to r_within_cylinder
        
        // r_within_cylinder(double *r,struct geometry_struct *geometry) {
        
        // if the center of cylinder 2 is within cylinder 1;
        
        if (r_within_cylinder(geometry2->center,geometry1)) return 1;  // if cylinder 2 is within cylinder 1, they clearly overlap.
        
        return 0;
    }

};

int cylinder_within_cylinder(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Unpack parameters
    double radius1 = geometry_parent->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height1 = geometry_parent->geometry_parameters.p_cylinder_storage->height;
    double radius2 = geometry_child->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height2 = geometry_child->geometry_parameters.p_cylinder_storage->height;
    
    int verbal = 0;
    // Generate unit direction vector along center axis of cylinders
    
    // Start with vector that points along the cylinder in the simple frame, and rotate to global
    Coords simple_vector = coords_set(0,1,0);
    Coords vector1,vector2;
    if (verbal == 1) printf("Cords start_vector = (%f,%f,%f)\n",simple_vector.x,simple_vector.y,simple_vector.z);

    // Rotate the position of the ray around the center of the cylinder
    vector1 = rot_apply(geometry_parent->rotation_matrix,simple_vector);
    if (verbal == 1) printf("Cords vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector1.z);
    // Rotate the position of the ray around the center of the cylinder
    vector2 = rot_apply(geometry_child->rotation_matrix,simple_vector);
    if (verbal == 1) printf("Cords vector2 = (%f,%f,%f)\n",vector2.x,vector2.y,vector2.z);
    
    // if vector1 and vector2 are parallel, the problem is simple, but if not complicated
    double cross_product1[3] = {0,0,0};
    // printf("%f\n",cross_product1[0]);
    // vec prod(&ax,&ay,&az,bx,by,bz, cx,cy,cz)
    vec_prod(cross_product1[0],cross_product1[1],cross_product1[2],vector1.x,vector1.y,vector1.z,vector2.x,vector2.y,vector2.z);
    // I get an error taking the adress of cross_product1[0], &cross_product1[0]. Took the pointer adresses instead. Works fine.
    if (verbal == 1) printf("cross_product = (%f,%f,%f)\n",cross_product1[0],cross_product1[1],cross_product1[2]);
    double cross_product_length = length_of_3vector(cross_product1);
    
    if (cross_product_length == 0) {
        // The cylinders are parallel.
        int seperated = 0;
        double delta[3];
        delta[0] = geometry_parent->center.x - geometry_child->center.x;
        delta[1] = geometry_parent->center.y - geometry_child->center.y;
        delta[2] = geometry_parent->center.z - geometry_child->center.z;

        // Test for separation by height
        // if (h0Div2 + h1Div2 − |Dot(W0, Delta )| < 0) seperated = 1;
        
        if (verbal == 1) printf("vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector2.z);
        if (verbal == 1) printf("delta1 = (%f,%f,%f)\n",delta[0],delta[1],delta[2]);
        double scalar_prod1 = scalar_prod(vector1.x,vector1.y,vector1.z,delta[0],delta[1],delta[2]);
        if (verbal == 1) printf("scalar product = %f \n",scalar_prod1);
        if (verbal == 1) printf("height 1 = %f, height 2 = %f \n",height1,height2);
        
        int inside = 1;
        
        if (height1*0.5 < height2*0.5 + fabs(scalar_prod1)) {
                if (verbal == 1) printf("Cylinder sticks out height wise \n");
                inside = 0;
                }
    
        // Test for separation radially
        // if (rSum − |Delta − Dot(W0,Delta)∗W0| < 0) seperated = 1;
        double vector_between_cyl_axis[3];
        vector_between_cyl_axis[0] = delta[0] - scalar_prod1*vector1.x;
        vector_between_cyl_axis[1] = delta[1] - scalar_prod1*vector1.y;
        vector_between_cyl_axis[2] = delta[2] - scalar_prod1*vector1.z;
        if (verbal == 1) printf("vector_between = (%f,%f,%f)\n",vector_between_cyl_axis[0],vector_between_cyl_axis[1],vector_between_cyl_axis[2]);
        if (verbal == 1) printf("length of vector between = %f\n",length_of_3vector(vector_between_cyl_axis));
        if (verbal == 1) printf("radius1 = %f , radius2=%f\n",radius1,radius2);
        if (radius1 < radius2 + length_of_3vector(vector_between_cyl_axis)) { // Answers: Does cylinder 2 stick out of cylinder 1?
        //if (radius1 + length_of_3vector(vector_between_cyl_axis) > radius2 ) { // Answers: Does cylinder 1 stick out of cylinder 2 radially?
                if (verbal == 1) printf("Cylinder sticks out radially \n");
                inside = 0;
                }
        
        if (inside == 0) return 0;
        else return 1;
        
    } else {
        // printf("The component uses a raytracing method for non parallel cylinders.\n");
        // printf("  Make sure not to give this algorithm edge cases, where cylinders just touch.\n");
        Coords cyl_direction2 = geometry_child->geometry_parameters.p_cylinder_storage->direction_vector;
        
        // The center point of the perfect cylinder (cylinder 2) needs to be within the meshgrid one (cylinder 1), otherwise it can not be within
        // Check with a simple call to r_within_cylinder
        
        // if the center of cylinder 2 is within cylinder 1;
        if (r_within_cylinder(geometry_child->center,geometry_parent) == 0) return 0;  // if cylinder 2 center is not within cylinder 1, it is clearly not within
        
        
        // Doing a simple but not perfect overlap test

        // Checking cylinder sides.
        
        // Taking cylinder 1, making a vector at the base center.
        Coords base_point;
        
        base_point.x = geometry_child->center.x - 0.5*height2*cyl_direction2.x;
        base_point.y = geometry_child->center.y - 0.5*height2*cyl_direction2.y;
        base_point.z = geometry_child->center.z - 0.5*height2*cyl_direction2.z;
        
        if (verbal==1) print_position(base_point,"Base point position (for inside cylinder)");
        
        // Making a point at the circumference of the bottom circle of the cylinder
        double cross_input[3] = {0,1,0};
        // In case the cross input is parallel with the vector, a new is chosen. Both can't be parallel.
        if (scalar_prod(cross_input[0],cross_input[1],cross_input[2],cyl_direction2.x,cyl_direction2.y,cyl_direction2.z) > 0.99) {
            cross_input[0] = 1; cross_input[1] = 0; cross_input[2] = 0;
        }
        // print_position(make_position(cross_input),"cross input");
        
        double cross_product1[3] = {0,0,0};
        vec_prod(cross_product1[0],cross_product1[1],cross_product1[2],cyl_direction2.x,cyl_direction2.y,cyl_direction2.z,cross_input[0],cross_input[1],cross_input[2]);
        
        // print_position(make_position(cross_product1),"cross_product1");
        double cross_length = length_of_3vector(cross_product1);
        
        // printf("cross_length = %f \n",cross_length);
        cross_product1[0] /= cross_length;
        cross_product1[1] /= cross_length;
        cross_product1[2] /= cross_length;
        
        cross_product1[0] *= radius2;
        cross_product1[1] *= radius2;
        cross_product1[2] *= radius2;
        
        Coords circ_point;
        double radial_position[3],cyl_direction_pointer[3],base_point_vector[3],cyl_radial_direction[3];
        
        cyl_direction_pointer[0] = cyl_direction2.x;
        cyl_direction_pointer[1] = cyl_direction2.y;
        cyl_direction_pointer[2] = cyl_direction2.z;
        
        //print_position(coords_set(cyl_direction_pointer[0],cyl_direction_pointer[1],cyl_direction_pointer[2]),"cylinder direction vector");
        //print_position(coords_set(cross_product1[0],cross_product1[1],cross_product1[2]),"cross product (before rotation)");
        
        int iterate,number_of_solutions,solutions,number_of_positions = 30;
        double rotate_angle,temp_solution[2],positive_solution,negative_solution;
        
        // printf("length of cyl_direction_pointer = %f \n",length_of_3vector(cyl_direction_pointer));
        
        // Check intersection with cylinder 2 with that point, and cyl_direction, if there is an intersection before height1, they overlap.
        // Rotate the circumference point around the cyl_direction for a full circle to detect intersections all the way around.
        
        // Here cross_product1 is a vector from the base point to a point n the circumference
        // circ_point is a vector from the base point to the circumference rotated an angle.
        // radial_position is the actual position on the circumference on the cylinder as a vector from origo.
        Coords radial_coords,top_coords;
        
        for (iterate = 0;iterate < number_of_positions;iterate++) {
                rotate_angle = 2*3.14159*((double) iterate)/((double) number_of_positions);
                rotate(circ_point.x,circ_point.y,circ_point.z,cross_product1[0],cross_product1[1],cross_product1[2],rotate_angle,cyl_direction2.x,cyl_direction2.y,cyl_direction2.z);
            
                radial_position[0] = base_point.x + circ_point.x;
                radial_position[1] = base_point.y + circ_point.y;
                radial_position[2] = base_point.z + circ_point.z;
                // Debug check
                radial_coords = coords_add(base_point,circ_point);
                if (r_within_cylinder(radial_coords,geometry_parent) == 0) {
                    //printf("Radial pointer number %d was not inside cylinder 1 (%f %f %f)\n",iterate,radial_position[0],radial_position[1],radial_position[2]);
                    return 0;
                }
            
                // sample_cylinder_intersect(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry) {
                sample_cylinder_intersect(temp_solution,&number_of_solutions,radial_position,cyl_direction_pointer,geometry_parent);
            
                if (number_of_solutions == 2) {
                    if (temp_solution[0]*temp_solution[1] > 0) {
                        // If both solutions are in the future or past, the point is outside the cylinder
                        if (verbal == 1) printf("Along axis: Not inside, as the solutions have the same sign: %f %f\n",temp_solution[0],temp_solution[1]);
                        return 0;
                    } else {
                        // The solutions have different signs
                        if (temp_solution[0] < 0) {
                            negative_solution = temp_solution[0];
                            positive_solution = temp_solution[1];
                        } else {
                            negative_solution = temp_solution[1];
                            positive_solution = temp_solution[0];
                        }
                        // If there is a solution before the cylinder ends, cylinder 2 can not be within cylinder 1
                        if (positive_solution < height2) {
                            if (verbal == 1) printf("Along axis: Not inside, as the positive solutions is less than the cylinder height: %f %f\n",temp_solution[0],temp_solution[1]);
                            if (verbal == 1) printf("Radial position = (%f,%f,%f) \n",radial_position[0],radial_position[1],radial_position[2]);
                            return 0;
                        }
                    }
                } else {
                    if (verbal == 1) printf("Along axis: 0 or 1 solution!\n");
                    return 0; // If there are 1 or 0 solutions, the radial position (on cylinder 2) would not be inside cylinder 1
                }
            
                cyl_radial_direction[0] = circ_point.x;
                cyl_radial_direction[1] = circ_point.y;
                cyl_radial_direction[2] = circ_point.z;
                // Note it has length radius1

                // Debug check
                if (r_within_cylinder(base_point,geometry_parent) == 0) {
                    //printf("Base point number %d was not inside cylinder 1 (%f %f %f)\n",iterate,base_point_vector[0],base_point_vector[1],base_point_vector[2]);
                    return 0;
                }
            
                // Base point in vector notation needed for intersect function.
                base_point_vector[0] = base_point.x;
                base_point_vector[1] = base_point.y;
                base_point_vector[2] = base_point.z;
            
                // The vector circ_point is from the base to the circumference. This is used to check the bottom cap.
                sample_cylinder_intersect(temp_solution,&number_of_solutions,base_point_vector,cyl_radial_direction,geometry_parent);
                
                if (number_of_solutions == 2) {
                    if (temp_solution[0]*temp_solution[1] > 0) {
                        // If both solutions are in the future or past, the point is outside the cylinder
                        if (verbal == 1) printf("Radial bottom: Not inside, as the solutions have the same sign: %f %f\n",temp_solution[0],temp_solution[1]);
                        return 0;
                    } else {
                        // The solutions have different signs
                        if (temp_solution[0] < 0) {
                            negative_solution = temp_solution[0];
                            positive_solution = temp_solution[1];
                        } else {
                            negative_solution = temp_solution[1];
                            positive_solution = temp_solution[0];
                        }
                        // If there is a solution before the line reaches the circumference from the center, cylinder 2 can not be within cylinder 1
                        if (positive_solution < 1 || negative_solution > -1) {
                            if (verbal == 1) printf("Radial bottom: Not inside, as the positive solutions is less than the cylinder radius: %f %f\n",temp_solution[0],temp_solution[1]);
                            return 0;
                        }
                    }
                } else {
                    if (verbal == 1) printf("Radially bottom: 0 or 1 solution!\n");
                    if (verbal == 1) print_position(circ_point,"current circ point");
                    if (verbal == 1) print_position(coords_set(cyl_radial_direction[0],cyl_radial_direction[1],cyl_radial_direction[2]),"current cyl_radial_direction (should be same as above)");
                    return 0; // If there are 1 or 0 solutions, the radial position (on cylinder 2) would not be inside cylinder 1
                }
            
                // Now check the top
                base_point_vector[0] = base_point.x + height2*cyl_direction2.x;
                base_point_vector[1] = base_point.y + height2*cyl_direction2.y;
                base_point_vector[2] = base_point.z + height2*cyl_direction2.z;
            
                top_coords = coords_set(base_point_vector[0],base_point_vector[1],base_point_vector[2]);
                // Debug check
                if (r_within_cylinder(top_coords,geometry_parent) == 0) {
                    //printf("Top point number %d was not inside cylinder 1 (%f %f %f)\n",iterate,base_point_vector[0],base_point_vector[1],base_point_vector[2]);
                    return 0;
                }
            
                // The vector circ_point is from the base to the circumference. This is used to check the bottom cap.
                sample_cylinder_intersect(temp_solution,&number_of_solutions,base_point_vector,cyl_radial_direction,geometry_parent);
                if (number_of_solutions == 2) {
                    if (temp_solution[0]*temp_solution[1] > 0) {
                        // If both solutions are in the future or past, the point is outside the cylinder
                        if (verbal == 1) printf("Radial top: Not inside, as the solutions have the same sign: %f %f\n",temp_solution[0],temp_solution[1]);
                        return 0;
                    } else {
                        // The solutions have different signs
                        if (temp_solution[0] < 0) {
                            negative_solution = temp_solution[0];
                            positive_solution = temp_solution[1];
                        } else {
                            negative_solution = temp_solution[1];
                            positive_solution = temp_solution[0];
                        }
                        // If there is a solution before the line reaches the circumference from the center, cylinder 2 can not be within cylinder 1
                        if (positive_solution < 1 || negative_solution > -1) {
                            if (verbal == 1) printf("Radial top: Not inside, as the positive solutions is less than the cylinder radius: %f %f\n",temp_solution[0],temp_solution[1]);
                            return 0;
                        }
                    }
                } else {
                    if (verbal == 1) printf("Radially top: 0 or 1 solution!\n");
                    return 0; // If there are 1 or 0 solutions, the radial position (on cylinder 2) would not be inside cylinder 1
                }

        }
        // The above method is not perfect as it basicly tests a mesh grid of the cylinder aginst another perfect cylinder.
        // Can be improved.
        
        // If no intersections is found and the center of cylinder 2 is within cylinder 1, cylinder 2 must be within cylinder 1.
        return 1;
    
    }

};

int cylinder_within_cylinder_backup(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
//int cylinder_within_cylinder(struct geometry_struct *geometry_parent,struct geometry_struct *geometry_child) {
    // Unpack parameters
    double radius1 = geometry_parent->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height1 = geometry_parent->geometry_parameters.p_cylinder_storage->height;
    double radius2 = geometry_child->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height2 = geometry_child->geometry_parameters.p_cylinder_storage->height;
    
    int verbal = 1;
    // Generate unit direction vector along center axis of cylinders
    
    // Start with vector that points along the cylinder in the simple frame, and rotate to global
    Coords simple_vector = coords_set(0,1,0);
    Coords vector1,vector2;
    if (verbal == 1) printf("Cords start_vector = (%f,%f,%f)\n",simple_vector.x,simple_vector.y,simple_vector.z);

    // Rotate the position of the ray around the center of the cylinder
    vector1 = rot_apply(geometry_parent->rotation_matrix,simple_vector);
    if (verbal == 1) printf("Cords vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector1.z);
    // Rotate the position of the ray around the center of the cylinder
    vector2 = rot_apply(geometry_child->rotation_matrix,simple_vector);
    if (verbal == 1) printf("Cords vector2 = (%f,%f,%f)\n",vector2.x,vector2.y,vector2.z);
    
    // if vector1 and vector2 are parallel, the problem is simple, but if not complicated
    double cross_product1[3] = {0,0,0};
    // printf("%f\n",cross_product1[0]);
    // vec prod(&ax,&ay,&az,bx,by,bz, cx,cy,cz)
    vec_prod(cross_product1[0],cross_product1[1],cross_product1[2],vector1.x,vector1.y,vector1.z,vector2.x,vector2.y,vector2.z);
    // I get an error taking the adress of cross_product1[0], &cross_product1[0]. Took the pointer adresses instead. Works fine.
    if (verbal == 1) printf("cross_product = (%f,%f,%f)\n",cross_product1[0],cross_product1[1],cross_product1[2]);
    double cross_product_length = length_of_3vector(cross_product1);
    
    if (cross_product_length == 0) {
        // The cylinders are parallel.
        int seperated = 0;
        double delta[3];
        delta[0] = geometry_parent->center.x - geometry_child->center.x;
        delta[1] = geometry_parent->center.y - geometry_child->center.y;
        delta[2] = geometry_parent->center.z - geometry_child->center.z;

        // Test for separation by height
        // if (h0Div2 + h1Div2 − |Dot(W0, Delta )| < 0) seperated = 1;
        
        if (verbal == 1) printf("vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector2.z);
        if (verbal == 1) printf("delta1 = (%f,%f,%f)\n",delta[0],delta[1],delta[2]);
        double scalar_prod1 = scalar_prod(vector1.x,vector1.y,vector1.z,delta[0],delta[1],delta[2]);
        if (verbal == 1) printf("scalar product = %f \n",scalar_prod1);
        if (verbal == 1) printf("height 1 = %f, height 2 = %f \n",height1,height2);
        
        int inside = 1;
        
        if (height1*0.5 < height2*0.5 + fabs(scalar_prod1)) {
                if (verbal == 1) printf("Cylinder sticks out height wise \n");
                inside = 0;
                }
    
        // Test for separation radially
        // if (rSum − |Delta − Dot(W0,Delta)∗W0| < 0) seperated = 1;
        double vector_between_cyl_axis[3];
        vector_between_cyl_axis[0] = delta[0] - scalar_prod1*vector1.x;
        vector_between_cyl_axis[1] = delta[1] - scalar_prod1*vector1.y;
        vector_between_cyl_axis[2] = delta[2] - scalar_prod1*vector1.z;
        if (verbal == 1) printf("vector_between = (%f,%f,%f)\n",vector_between_cyl_axis[0],vector_between_cyl_axis[1],vector_between_cyl_axis[2]);
        if (verbal == 1) printf("length of vector between = %f\n",length_of_3vector(vector_between_cyl_axis));
        if (verbal == 1) printf("radius1 = %f , radius2=%f\n",radius1,radius2);
        if (radius1 < radius2 + length_of_3vector(vector_between_cyl_axis)) { // Answers: Does cylinder 2 stick out of cylinder 1?
        //if (radius1 + length_of_3vector(vector_between_cyl_axis) > radius2 ) { // Answers: Does cylinder 1 stick out of cylinder 2 radially?
                if (verbal == 1) printf("Cylinder sticks out radially \n");
                inside = 0;
                }
        
        if (inside == 0) return 0;
        else return 1;
        
    } else {
        // printf("The component uses a raytracing method for non parallel cylinders.\n");
        // printf("  Make sure not to give this algorithm edge cases, where cylinders just touch.\n");
        Coords cyl_direction1 = geometry_parent->geometry_parameters.p_cylinder_storage->direction_vector;
        
        // The center point of the perfect cylinder (cylinder 2) needs to be within the meshgrid one (cylinder 1), otherwise it can not be within
        // Check with a simple call to r_within_cylinder
        
        // if the center of cylinder 2 is within cylinder 1;
        if (r_within_cylinder(geometry_child->center,geometry_parent) == 0) return 0;  // if cylinder 2 center is not within cylinder 1, it is clearly not within
        
        
        // Doing a simple but not perfect overlap test

        // Checking cylinder sides.
        
        // Taking cylinder 1, making a vector at the base center.
        Coords base_point;
        
        base_point.x = geometry_parent->center.x - 0.5*height1*cyl_direction1.x;
        base_point.y = geometry_parent->center.y - 0.5*height1*cyl_direction1.y;
        base_point.z = geometry_parent->center.z - 0.5*height1*cyl_direction1.z;
        
        // Making a point at the circumference of the bottom circle of the cylinder
        double cross_input[3] = {0,1,0};
        // In case the cross input is parallel with the vector, a new is chosen. Both can't be parallel.
        if (scalar_prod(cross_input[0],cross_input[1],cross_input[2],cyl_direction1.x,cyl_direction1.y,cyl_direction1.z) > 0.99) {
            cross_input[0] = 1; cross_input[1] = 0; cross_input[2] = 0;
        }
        // print_position(make_position(cross_input),"cross input");
        
        double cross_product1[3] = {0,0,0};
        vec_prod(cross_product1[0],cross_product1[1],cross_product1[2],cyl_direction1.x,cyl_direction1.y,cyl_direction1.z,cross_input[0],cross_input[1],cross_input[2]);
        
        // print_position(make_position(cross_product1),"cross_product1");
        double cross_length = length_of_3vector(cross_product1);
        
        // printf("cross_length = %f \n",cross_length);
        cross_product1[0] /= cross_length;
        cross_product1[1] /= cross_length;
        cross_product1[2] /= cross_length;
        
        cross_product1[0] *= radius1;
        cross_product1[1] *= radius1;
        cross_product1[2] *= radius1;
        
        Coords circ_point;
        double radial_position[3],cyl_direction_pointer[3],base_point_vector[3],cyl_radial_direction[3];
        
        cyl_direction_pointer[0] = cyl_direction1.x;
        cyl_direction_pointer[1] = cyl_direction1.y;
        cyl_direction_pointer[2] = cyl_direction1.z;
        
        print_position(make_position(cyl_direction_pointer),"cylinder direction vector");
        
        int iterate,number_of_solutions,solutions,number_of_positions = 30;
        double rotate_angle,temp_solution[2];
        
        // printf("length of cyl_direction_pointer = %f \n",length_of_3vector(cyl_direction_pointer));
        
        // Check intersection with cylinder 2 with that point, and cyl_direction, if there is an intersection before height1, they overlap.
        // Rotate the circumference point around the cyl_direction for a full circle to detect intersections all the way around.
        
        // Here cross_product1 is a vector from the base point to a point n the circumference
        // circ_point is a vector from the base point to the circumference rotated an angle.
        // radial_position is the actual position on the circumference on the cylinder as a vector from origo.
        for (iterate = 0;iterate < number_of_positions;iterate++) {
                rotate_angle = 2*3.14159*((double) iterate)/((double) number_of_positions);
                rotate(circ_point.x,circ_point.y,circ_point.z,cross_product1[0],cross_product1[1],cross_product1[2],rotate_angle,cyl_direction1.x,cyl_direction1.y,cyl_direction1.z);
            
                radial_position[0] = base_point.x + circ_point.x;
                radial_position[1] = base_point.y + circ_point.y;
                radial_position[2] = base_point.z + circ_point.z;
                // sample_cylinder_intersect(double *t,int *num_solutions,double *r,double *v,struct geometry_struct *geometry) {
                sample_cylinder_intersect(temp_solution,&number_of_solutions,radial_position,cyl_direction_pointer,geometry_child);
                for (solutions = 0;solutions < number_of_solutions;solutions++) {
                    if (temp_solution[solutions] > 0 && temp_solution[solutions] < height1) {
                        // cylinders must overlap.
                        return 0;
                    }
                }
                // if (number_of_solutions == 2) {
                //    if (temp_solution[0] < 0 && temp_solution[1] < 0) return 0; // cylinder 2 outside cylinder 1
                //    if (temp_solution[0] > 0 && temp_solution[1] > 0) return 0; // cylinder 2 outside cylinder 1
                //}
            
                cyl_radial_direction[0] = circ_point.x;
                cyl_radial_direction[1] = circ_point.y;
                cyl_radial_direction[2] = circ_point.z;
                // Note it has length radius1
            
                base_point_vector[0] = base_point.x;
                base_point_vector[1] = base_point.y;
                base_point_vector[2] = base_point.z;
            
                // The vector circ_point is from the base to the circumference. This is used to check the bottom cap.
                sample_cylinder_intersect(temp_solution,&number_of_solutions,base_point_vector,cyl_radial_direction,geometry_child);
                for (solutions = 0;solutions < number_of_solutions;solutions++) {
                    if (temp_solution[solutions] > 0 && temp_solution[solutions] < 1) {
                        // cylinders must overlap.
                        return 0;
                    }
                }
                // if (number_of_solutions == 2) {
                //    if (temp_solution[0] < 0 && temp_solution[1] < 0) return 0; // cylinder 2 outside cylinder 1
                //    if (temp_solution[0] > 0 && temp_solution[1] > 0) return 0; // cylinder 2 outside cylinder 1
                //}
            
                // Now check the top
                base_point_vector[0] = base_point.x + height1*cyl_direction1.x;
                base_point_vector[1] = base_point.y + height1*cyl_direction1.y;
                base_point_vector[2] = base_point.z + height1*cyl_direction1.z;
            
                // The vector circ_point is from the base to the circumference. This is used to check the bottom cap.
                sample_cylinder_intersect(temp_solution,&number_of_solutions,base_point_vector,cyl_radial_direction,geometry_child);
                for (solutions = 0;solutions < number_of_solutions;solutions++) {
                    if (temp_solution[solutions] > 0 && temp_solution[solutions] < 1) {
                        // cylinders must overlap.
                        return 0;
                    }
                }
                // if (number_of_solutions == 2) {
                //    if (temp_solution[0] < 0 && temp_solution[1] < 0) return 0; // cylinder 2 outside cylinder 1
                //    if (temp_solution[0] > 0 && temp_solution[1] > 0) return 0; // cylinder 2 outside cylinder 1
                //}
        }
        // The above method is not perfect as it basicly tests a mesh grid of the cylinder aginst another perfect cylinder.
        // Can be improved.
        
        // If no intersections is found and the center of cylinder 2 is within cylinder 1, cylinder 2 must be within cylinder 1.
        return 1;
    
    }

};

int cone_overlaps_cone(struct geometry_struct *geometry1,struct geometry_struct *geometry2) {
  // Overlap function should return 1 if the to geometries both cover some volume
  // Temporary function

    // Load Variables:
    Coords direction_1 = geometry1->geometry_parameters.p_cone_storage->direction_vector;
    Coords center_1 = geometry1->center;
    double radius_top_1 = geometry1->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius_bottom_1 = geometry1->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height_1 = geometry1->geometry_parameters.p_cone_storage->height;

    Coords direction_2 = geometry2->geometry_parameters.p_cone_storage->direction_vector;
    Coords center_2 = geometry2->center;
    double radius_top_2 = geometry2->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius_bottom_2 = geometry2->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height_2 = geometry2->geometry_parameters.p_cone_storage->height;

    double Y;
    double max_r;


    // // Simple test to see if they are far away (with smallest spheres outside)
    // Create Spheres

    /*
    Y = -(0.5*height_1)-(radius_top_1*radius_top_1-radius_bottom_1*radius_bottom_1)/(2.0*height_1);
    if (radius_top_1 > radius_bottom_1){
        max_r = radius_top_1;
    }else{
        max_r = radius_bottom_1;
    }
    double sphere_1_radius =  sqrt((Y+0.5*height_1)*(Y+0.5*height_1)+max_r*max_r);
    Coords sphere_1_pos = coords_set(center_1.x+direction_1.x*Y,center_1.y+direction_1.y*Y,center_1.z+direction_1.z*Y);
    */

    // Not sure above works, writing own version.
    
    double dist_above_bottom = 0.5*(radius_top_1*radius_top_1+height_1*height_1-radius_bottom_1*radius_bottom_1)/height_1;
    double dist_from_center = dist_above_bottom - 0.5*height_1;
    Coords sphere_1_pos = coords_set(center_1.x+direction_1.x*dist_from_center,
                                     center_1.y+direction_1.y*dist_from_center,
                                     center_1.z+direction_1.z*dist_from_center);
    double sphere_1_radius = sqrt(radius_bottom_1*radius_bottom_1+dist_above_bottom*dist_above_bottom);
    
    /*
    Y = -(0.5*height_2)-(radius_top_2*radius_top_2-radius_bottom_2*radius_bottom_2)/(2.0*height_2);
    if (radius_top_2 > radius_bottom_2){
        max_r = radius_top_2;
    }else{
        max_r = radius_bottom_2;
    }
    double sphere_2_radius =  sqrt((Y+0.5*height_2)*(Y+0.5*height_2)+max_r*max_r);
    Coords sphere_2_pos = coords_set(center_2.x+direction_2.x*Y,center_2.y+direction_2.y*Y,center_2.z+direction_2.z*Y);
    */
    
    dist_above_bottom = 0.5*(radius_top_2*radius_top_2+height_2*height_2-radius_bottom_2*radius_bottom_2)/height_2;
    dist_from_center = dist_above_bottom - 0.5*height_2;
    Coords sphere_2_pos = coords_set(center_2.x+direction_2.x*dist_from_center,
                                     center_2.y+direction_2.y*dist_from_center,
                                     center_2.z+direction_2.z*dist_from_center);
    double sphere_2_radius = sqrt(radius_bottom_2*radius_bottom_2+dist_above_bottom*dist_above_bottom);

    // Test if spheres are too long apart to have any chance of intersecting

    double dist_spheres = sqrt((sphere_1_pos.x-sphere_2_pos.x)*(sphere_1_pos.x-sphere_2_pos.x)+(sphere_1_pos.y-sphere_2_pos.y)*(sphere_1_pos.y-sphere_2_pos.y)+(sphere_1_pos.z-sphere_2_pos.z)*(sphere_1_pos.z-sphere_2_pos.z));


    if (dist_spheres > sphere_1_radius + sphere_2_radius){
        //printf("\nSpherical method determined that cones are too far away for intersection to be relevant\n");
        return 0;
    }

    // // Simple test to see if they are inside (with largest spheres inside)
    // Brute force in two steps.
    // 1. Check if any points on 1 lies within 2
    // 2. Check if any transversal lines on the mesh of 1 intersects with 2

    // Calculate needed information
    Coords cone_1_bottom_point = coords_add(center_1,coords_scalar_mult(direction_1,-0.5*height_1));
    Coords cone_1_top_point = coords_add(center_1,coords_scalar_mult(direction_1,0.5*height_1));
    Coords cone_2_bottom_point = coords_add(center_2,coords_scalar_mult(direction_2,-0.5*height_2));
    Coords cone_2_top_point = coords_add(center_2,coords_scalar_mult(direction_2,0.5*height_2));


    // Create two circles for both geometries
    int resoultuion = 500;

    struct pointer_to_1d_coords_list cone_1_points = geometry1->shell_points(geometry1,resoultuion);

    //points_on_circle(cone_1_top,cone_1_top_point,direction_1,radius_top_1,resoultuion);
    //points_on_circle(cone_1_bottom,cone_1_bottom_point,direction_1,radius_bottom_1,resoultuion);

    //printf("\nTEST\n");
    int i;
    // Test geometry 1 points inside geometry 2

    for (i = 0 ; i < cone_1_points.num_elements ; i++){
        
        if (r_within_cone(cone_1_points.elements[i],geometry2) == 1){
            //printf("\nOne point on cone 1 is inside cone 2\n");
            return 1;
        }
    }
    
    struct pointer_to_1d_coords_list cone_2_points = geometry2->shell_points(geometry2,resoultuion);

    // Test geometry 2 points inside geometry 1
    for (i = 0 ; i < cone_2_points.num_elements ; i++){
        
        if (r_within_cone(cone_2_points.elements[i],geometry1) == 1){
            //printf("\nOne point on cone 2 is inside cone 1\n");
            return 1;
        }
    }

    // Test 1 within 2


    // // Test if there is any intersection (intersection function or eqation?)

    // This is an implementation of brute force. Maybe do this with a calculated function?
    int circ_resolution = 50;  // how many lines will the be checked for
    int height_resolution = 150;

    double length_of_cone_side_1 = sqrt(pow(radius_top_1-radius_bottom_1,2)+pow(height_1,2));
    double length_of_cone_side_2 = sqrt(pow(radius_top_2-radius_bottom_2,2)+pow(height_2,2));

    double slope_1 = (radius_top_1-radius_bottom_1)/height_1;
    double slope_2 = (radius_top_2-radius_bottom_2)/height_2;

    double local_radius;

    Coords cone_1_direction = geometry1->geometry_parameters.p_cone_storage->direction_vector;
    Coords cone_2_direction = geometry2->geometry_parameters.p_cone_storage->direction_vector;

    //printf("\nlength_of_cone_side_1 = %f\n",length_of_cone_side_1);

    Coords circ_points[50];
    double circ_offset;
    Coords circ_center;

    int j;

    for (i = 0 ; i < height_resolution ; i++){
        // Calculate circ offset
        //circ_offset = i * length_of_cone_side_1 / height_resolution; // Possible bug
        circ_offset = i * height_1 / height_resolution;

        // Calculate middle point
        circ_center = coords_add(cone_1_bottom_point,coords_set(cone_1_direction.x * circ_offset,cone_1_direction.y * circ_offset,cone_1_direction.z * circ_offset));

        // Calculate radius
        local_radius = circ_offset * slope_1 + radius_bottom_1;

        // Make points on circle
        //printf("points on circle: circ_center = [%f,%f,%f] , cone_1_direction = [%f,%f,%f] , local_radius = %f , circ_resolution = %i",circ_center.x,circ_center.y,circ_center.z,cone_1_direction.x,cone_1_direction.y,cone_1_direction.z,local_radius,circ_resolution);
        points_on_circle(circ_points,circ_center,cone_1_direction,local_radius,circ_resolution);

        // Test if any points lies within geomtry 2
        for (j = 0 ; j < circ_resolution; j++){
            //printf("\ntested if point [%i] [%f,%f,%f] is inside",j,circ_points[j].x,circ_points[j].y,circ_points[j].z);
            if (r_within_cone(circ_points[j],geometry2) == 1){
                //printf("\nOne point on cone 1 is inside cone 2\n");
                return 1;
            }
        }
    }


    for (i = 0 ; i < height_resolution ; i++){
        // Calculate circ offset
        // circ_offset = i * length_of_cone_side_1 / height_resolution; // Possible bug
        circ_offset = i * height_2 / height_resolution; // Possible bug

        // Calculate middle point
        circ_center = coords_add(cone_2_bottom_point,coords_set(cone_2_direction.x * circ_offset,cone_2_direction.y * circ_offset,cone_2_direction.z * circ_offset));

        // Calculate radius
        local_radius = circ_offset * slope_2 + radius_bottom_2;

        // Make points on circle
        //printf("points on circle: circ_center = [%f,%f,%f] , cone_1_direction = [%f,%f,%f] , local_radius = %f , circ_resolution = %i",circ_center.x,circ_center.y,circ_center.z,cone_2_direction.x,cone_2_direction.y,cone_2_direction.z,local_radius,circ_resolution);
        points_on_circle(circ_points,circ_center,cone_2_direction,local_radius,circ_resolution);

        // Test if any points lies within geomtry 2
        for (j = 0 ; j < circ_resolution; j++){
            //printf("\ntested if point [%i] [%f,%f,%f] is inside",j,circ_points[j].x,circ_points[j].y,circ_points[j].z);
            if (r_within_cone(circ_points[j],geometry1) == 1){
                //printf("\nOne point on cone 2 is inside cone 1\n");
                return 1;
            }
        }
    }

    return 0;

};

int cone_within_cone(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Function returns 1 if the cone is completely within the cylinder, 0 otherwise
    // Brute force place holder
    return A_within_B(geometry_child,geometry_parent,(int) 300); // 150 points on each end cap
};

int mesh_overlaps_mesh(struct geometry_struct *geometry1,struct geometry_struct *geometry2) {
  // Overlap function should return 1 if the to geometries both cover some volume
  // Temporary function

    // Brute force check if there is one point of geometry 1 in 2 and 2 in 1.

    // Should also have a secondary check with edges intersecting on faces.
    // Could be made faster with a bounding box (or sphere)


    // Load Variables:
    struct pointer_to_1d_coords_list shell_points1 = geometry1->shell_points(geometry1,144);
    struct pointer_to_1d_coords_list shell_points2 = geometry2->shell_points(geometry2,144);

    int i;
    for (i = 0 ; i < shell_points1.num_elements ; i++){
        if (r_within_mesh(shell_points1.elements[i],geometry2)){
            free(shell_points1.elements);
            free(shell_points2.elements);
            return 1;
        }
    }
    for (i = 0 ; i < shell_points2.num_elements ; i++){
        if (r_within_mesh(shell_points2.elements[i],geometry1)){
            free(shell_points1.elements);
            free(shell_points2.elements);
            return 1;
        }
    }
    
    free(shell_points1.elements);
    free(shell_points2.elements);

    return 0;

};

int mesh_within_mesh(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Function returns 1 if the cone is completely within the cylinder, 0 otherwise
    // Brute force place holder
    return mesh_A_within_B(geometry_child,geometry_parent); // 30 points on each end cap
};

// -------------    Overlap functions for two different geometries --------------------------------

int box_overlaps_cylinder(struct geometry_struct *geometry_box,struct geometry_struct *geometry_cyl) {
    // Checking if the box and cylinder described by geometry_box and geometry_cyl overlaps.
    // Done in steps:
    // If any corner points of the box is within the cylinder, they do overlap
    // If any points on the cylinders end caps are within the box, they do overlap
    // If any of the lines describing the sides of the box intersect the cylinder, they do overlap
    // If the symmetry line of the cylinder intersect the box, they do overlap
    // If none of the above are true, they do not overlap
    
    // A problem with this algorithm is a lack of a quick exit if the volumes obviously does not overlap

    // Generate coordinates of corners of box
    Coords corner_points[8];
    box_corners_global_frame(corner_points,geometry_box);
    
    // Check earch corner seperatly
    int iterate;
    for (iterate=0;iterate<8;iterate++) {
        if (geometry_cyl->within_function(corner_points[iterate],geometry_cyl) == 1) {
            return 1; // If a corner of the box is inside the cylinder, the two volumes overlap
        }
    }
    
    Coords cyl_direction = geometry_cyl->geometry_parameters.p_cylinder_storage->direction_vector;
    Coords center = geometry_cyl->center;
    double radius = geometry_cyl->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height = geometry_cyl->geometry_parameters.p_cylinder_storage->height;
    
    Coords cyl_top_point = coords_add(center,coords_scalar_mult(cyl_direction,0.5*height));
    Coords cyl_bottom_point = coords_add(center,coords_scalar_mult(cyl_direction,-0.5*height));
    
    // Generate 100 points on the circle describing the top of the cylinder
    Coords *circle_point_array;
    int number_of_points = 150;
    circle_point_array = malloc(number_of_points * sizeof(Coords));
    
    points_on_circle(circle_point_array,cyl_top_point,cyl_direction,radius,number_of_points);
    
    // Check parts of cylinder top seperatly
    for (iterate=0;iterate<number_of_points;iterate++) {
        if (geometry_box->within_function(circle_point_array[iterate],geometry_box) == 1) {
            return 1; // If part of the cylinder is inside the box, the volumes overlap
        }
    }
    
    // Check parts of cylinder bottom seperatly
    points_on_circle(circle_point_array,cyl_bottom_point,cyl_direction,radius,number_of_points);
    for (iterate=0;iterate<number_of_points;iterate++) {
        if (geometry_box->within_function(circle_point_array[iterate],geometry_box) == 1) {
            return 1; // If part of the cylinder is inside the box, the volumes overlap
        }
    }
    free(circle_point_array);

    // Check intersections for the lines between the corners of the box and the cylinder
    // 12 sides to a box, if any one of them intersects, the volumes overlaps
    for (iterate=0;iterate<3;iterate++) { //
        if (existence_of_intersection(corner_points[iterate],corner_points[iterate+1],geometry_cyl) == 1) return 1;
    }
    if (existence_of_intersection(corner_points[3],corner_points[0],geometry_cyl) == 1) return 1;
    for (iterate=4;iterate<7;iterate++) {
        if (existence_of_intersection(corner_points[iterate],corner_points[iterate+1],geometry_cyl) == 1) return 1;
    }
    if (existence_of_intersection(corner_points[7],corner_points[4],geometry_cyl) == 1) return 1;
    for (iterate=0;iterate<4;iterate++) {
        if (existence_of_intersection(corner_points[iterate],corner_points[iterate+4],geometry_cyl) == 1) return 1;
    }
    
    // Only need to test the intersection between the symetry line of the cylinder and the box
    if (existence_of_intersection(cyl_top_point,cyl_bottom_point,geometry_box) == 1) return 1;
    
    // If all the tests change, the volumes do not overlap
    return 0;
};

int cylinder_overlaps_box(struct geometry_struct *geometry_cyl,struct geometry_struct *geometry_box) {
    // overlap functions are symetrical, but it is convinient to have both defined
    return box_overlaps_cylinder(geometry_box,geometry_cyl);
};

int cylinder_overlaps_sphere(struct geometry_struct *geometry_cyl,struct geometry_struct *geometry_sph) {
 
    // If the sphere center is inside, one can exit fast
    Coords sph_center = geometry_sph->center;
    if (geometry_cyl->within_function(sph_center,geometry_cyl) == 1) return 1;
    
    // If cylinder center is inside, one can exit fast
    Coords cyl_center = geometry_cyl->center;
    if (geometry_sph->within_function(cyl_center,geometry_sph) == 1) return 1;
    
    double cyl_radius = geometry_cyl->geometry_parameters.p_cylinder_storage->cyl_radius;
    double cyl_height = geometry_cyl->geometry_parameters.p_cylinder_storage->height;
    Coords cyl_direction = geometry_cyl->geometry_parameters.p_cylinder_storage->direction_vector;
    
    // Or cylinder top / bottom point
    Coords cyl_top_point = coords_add(cyl_center,coords_scalar_mult(cyl_direction,0.5*cyl_height));
    if (geometry_sph->within_function(cyl_top_point,geometry_sph) == 1) return 1;
    
    Coords cyl_bottom_point = coords_add(cyl_center,coords_scalar_mult(cyl_direction,-0.5*cyl_height));
    if (geometry_sph->within_function(cyl_bottom_point,geometry_sph) == 1) return 1;
    
    // Calculate distance
    double distance = distance_between(geometry_cyl->center,geometry_sph->center);
    
    double sph_radius = geometry_sph->geometry_parameters.p_sphere_storage->sph_radius;
    
    // Return 0 if the bounding sphere and the sphere do not overlap, otherwise do brute force
    if (distance > sph_radius + sqrt(cyl_radius*cyl_radius+0.25*cyl_height*cyl_height)) return 0;
    
    // Could check "inner sphere" of cylinder against sphere, if they overlap, the geometries overlap
    if (cyl_height >= 2.0*cyl_radius) {
        if (distance < sph_radius + cyl_radius) return 1;
    } else {
        if (distance < sph_radius + 0.5*cyl_height) return 1;
    }
    
    // Projection method
    // Find the distance between cylinder and sphere perpendicular to the cylinder direction.
    Coords difference = coords_sub(sph_center,cyl_center);
    
    // projection is simple as the cylinder direction vector is a normal vector
    Coords projection = coords_scalar_mult(cyl_direction,union_coords_dot(difference,cyl_direction));
    Coords perpendicular = coords_sub(difference,projection);
    
    if (length_of_position_vector(perpendicular) > sph_radius + cyl_radius) return 0;
    
    // Brute force
    // Consider enlarging the sphere slightly to decrease the probability for false negatives
    //  at the cost of some false positives. This is acceptable as false positives will not
    //  have any severe effect, but false negatives causes errors.
    
    // Random tests shows no issues with this approach, false negatives disapeared.
    struct sphere_storage temp_sph_storage;
    temp_sph_storage.sph_radius = 1.02*sph_radius;

    struct geometry_struct temp_sph;
    temp_sph.geometry_parameters.p_sphere_storage = &temp_sph_storage;
    temp_sph.center = geometry_sph->center;
    
    // temp_sph is not fully initialized, it just has geometrical information
    
    struct pointer_to_1d_coords_list shell_points;
    shell_points = geometry_sph->shell_points(&temp_sph,300*300); // using 300 rings with 300 points, works but is slow
    //shell_points = geometry_sph->shell_points(&temp_sph,70*70); // using 50 rings with 50 points
    //shell_points = geometry_sph->shell_points(&temp_sph,50*50); // using 50 rings with 50 points
    
    int iterate;
    for (iterate=0;iterate<shell_points.num_elements;iterate++) {
      if (geometry_cyl->within_function(shell_points.elements[iterate],geometry_cyl) == 1) {
        free(shell_points.elements);
        return 1;
      }
    }
    
    free(shell_points.elements);
    
    shell_points = geometry_cyl->shell_points(geometry_cyl,400); // 200 on each ring
    for (iterate=0;iterate<shell_points.num_elements;iterate++) {
      if (geometry_sph->within_function(shell_points.elements[iterate],&temp_sph) == 1) {
        free(shell_points.elements);
        return 1;
      }
    }
    
    free(shell_points.elements);
    return 0;
    
    
    /*
    // Using the actual sphere size and position
    struct pointer_to_1d_coords_list shell_points;
    shell_points = geometry_sph->shell_points(geometry_sph,250000); // using 500 rings with 500 points
    
    
    int iterate;
    for (iterate=0;iterate<shell_points.num_elements;iterate++) {
      if (geometry_cyl->within_function(shell_points.elements[iterate],geometry_cyl) == 1) {
        free(shell_points.elements);
        return 1;
      }
    }
    
    free(shell_points.elements);
    
    shell_points = geometry_cyl->shell_points(geometry_cyl,400); // 200 on each ring
    for (iterate=0;iterate<shell_points.num_elements;iterate++) {
      if (geometry_sph->within_function(shell_points.elements[iterate],geometry_sph) == 1) {
        free(shell_points.elements);
        return 1;
      }
    }
    
    free(shell_points.elements);
    return 0;
    */
};

int box_overlaps_sphere(struct geometry_struct *geometry_box,struct geometry_struct *geometry_sph) {
    
    //printf("\n checking sphere center in box\n");
    // If the sphere center is inside box, one can exit fast
    Coords sph_center = geometry_sph->center;
    if (geometry_box->within_function(sph_center,geometry_box) == 1) return 1;
    
    //printf("\n checking box center in sphere\n");
    // If the box center is inside sphere, one can exit fast
    Coords box_center = geometry_box->center;
    if (geometry_sph->within_function(box_center,geometry_sph) == 1) return 1;
    
    // Check if box corners are inside the sphere
    int iterate;
    struct pointer_to_1d_coords_list shell_points;
    shell_points = geometry_box->shell_points(geometry_box,8);
    
    for (iterate=0;iterate<shell_points.num_elements;iterate++) {
      if (geometry_sph->within_function(shell_points.elements[iterate],geometry_sph) == 1) {
        free(shell_points.elements);
        return 1;
      }
    }
    free(shell_points.elements);
    
    // Can not find elegant solution to this problem. Will use brute force.
    
    // Before brute forcing, find negative solutions for obvious cases.
    // Use circle - circle overlap algorithm, find bounding circle for box.
    
    //printf("\n checking bounding sphere approach\n");
    Coords corner_ps[8];
    
    double this_length,max_length = 0;
    
    box_corners_local_frame(corner_ps,geometry_box); // Local frame: center in (0,0,0)
    for (iterate=0;iterate<8;iterate++) {
      this_length = length_of_position_vector(corner_ps[iterate]);
      if (this_length > max_length) max_length = this_length;
    }
    // Box has a bounding circle with radius max_length and it's normal center.
    //printf("bounding sphere for box has radius = %f \n",max_length);
    
    double radius = geometry_sph->geometry_parameters.p_sphere_storage->sph_radius;

    // Calculate distance
    double distance = distance_between(geometry_box->center,geometry_sph->center);
    
    
    // Return 0 if the bounding sphere and the sphere do not overlap, otherwise do brute force
    if (distance > radius + max_length) {
      //printf("\n Bounding sphere avoided brute force method in sphere / box overlap\n");
      return 0;
    }
    
    //printf("\n doing brute force method in box overlaps sphere\n");
    // Brute force
    
    // Slightly increase size of the sphere to avoid edgecases, original value already saved
    geometry_sph->geometry_parameters.p_sphere_storage->sph_radius = 1.02*radius;
    
    // Shell points must be free'ed before leaving this function
    shell_points = geometry_sph->shell_points(geometry_sph,100*100); // using 100 rings with 100 points
  
    for (iterate=0;iterate<shell_points.num_elements;iterate++) {
      if (geometry_box->within_function(shell_points.elements[iterate],geometry_box) == 1) {
        free(shell_points.elements);
        geometry_sph->geometry_parameters.p_sphere_storage->sph_radius = radius;
        return 1;
      }
    }
    
    // Reset sphere radius to correct value
    geometry_sph->geometry_parameters.p_sphere_storage->sph_radius = radius;
    
    free(shell_points.elements);
    return 0;
    
};


// sym sphere
int sphere_overlaps_cylinder(struct geometry_struct *geometry_sph,struct geometry_struct *geometry_cyl) {
  return cylinder_overlaps_sphere(geometry_cyl,geometry_sph);
};

int sphere_overlaps_box(struct geometry_struct *geometry_sph,struct geometry_struct *geometry_box) {
  return box_overlaps_sphere(geometry_box,geometry_sph);
};

int cone_overlaps_sphere(struct geometry_struct *geometry_cone,struct geometry_struct *geometry_sph) {
  // Overlap function should return 1 if the to geometries both cover some volume
  // Temporary function
  // Load Variables:
    /*
    Coords direction_1 = geometry_cone->geometry_parameters.p_cone_storage->direction_vector;
    Coords center_1 = geometry_cone->center;
    double radius_top_1 = geometry_cone->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius_bottom_1 = geometry_cone->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height_1 = geometry_cone->geometry_parameters.p_cone_storage->height;

    Coords direction_2 = geometry_sph->geometry_parameters.p_sphere_storage->direction_vector;
    Coords center_2 = geometry_sph->center;
    double radius_2 = geometry_sph->geometry_parameters.p_sphere_storage->sph_radius;
    */

    double Y;
    double max_r;
    int resolution = 300;


    // This function is a rewritten verstion of the A_within_B.

    // This function assumes the parent (B) is a convex geoemtry
      // If all points on the shell of geometry A is within B, so are all lines between them.

    
    // FIRST CHECK IF POINTS N CONE IS INSIDE SPHERE:

      // resolution selects the number of points to be generated on the shell.
      struct pointer_to_1d_coords_list shell_points;
      shell_points = geometry_cone->shell_points(geometry_cone,resolution);
      // Shell_points.elements need to be freed before leaving this function
    
      if (shell_points.num_elements > resolution || shell_points.num_elements < 0) {
        printf("\nERROR: Shell point function used in A_within_B return garbage num_elements. \n");
        exit(1);
      }
    
      int iterate;

      for (iterate=0;iterate<shell_points.num_elements;iterate++) {
        if (geometry_sph->within_function(shell_points.elements[iterate],geometry_sph) == 1) {
          free(shell_points.elements);
          //printf("\n ONE POINT OF SPH IS INSIDE CONE\n");
          return 1;
        }
      }
    
      free(shell_points.elements);

    // CHECK IF SPHERE POINTS ARE INSIDE CONE

      // resolution selects the number of points to be generated on the shell.
      shell_points = geometry_sph->shell_points(geometry_sph,resolution);
      // Shell_points.elements need to be freed before leaving this function
    
      if (shell_points.num_elements > resolution || shell_points.num_elements < 0) {
        printf("\nERROR: Shell point function used in A_within_B return garbage num_elements. \n");
        exit(1);
      }
    


      for (iterate=0;iterate<shell_points.num_elements;iterate++) {
        if (geometry_cone->within_function(shell_points.elements[iterate],geometry_cone) == 1) {
          free(shell_points.elements);
          //printf("\n ONE POINT OF CONE IS INSIDE SPH\n");
          return 1;
        }
      }
    
      free(shell_points.elements);


    
      // If just one points is inside, the entire geometry is assumed inside as parent should be convex
      return 0;

};

int sphere_overlaps_cone(struct geometry_struct *geometry_sph,struct geometry_struct *geometry_cone) {
  // This problem is symetrical.
  return cone_overlaps_sphere(geometry_cone,geometry_sph);
};

int cone_overlaps_cylinder(struct geometry_struct *geometry_cone,struct geometry_struct *geometry_cylinder) {
  // Overlap function should return 1 if the to geometries both cover some volume
  // This now works for the simple case where the two directions are parallel. Otherwise it uses A within B.

    // Load Variables:
    Coords direction_1 = geometry_cone->geometry_parameters.p_cone_storage->direction_vector;
    Coords center_1 = geometry_cone->center;
    double radius_top_1 = geometry_cone->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius_bottom_1 = geometry_cone->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height_1 = geometry_cone->geometry_parameters.p_cone_storage->height;

    Coords direction_2 = geometry_cylinder->geometry_parameters.p_cylinder_storage->direction_vector;
    Coords center_2 = geometry_cylinder->center;
    double radius_2 = geometry_cylinder->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height_2 = geometry_cylinder->geometry_parameters.p_cylinder_storage->height;

    double radius_bottom_2 = radius_2;
    double radius_top_2 = radius_2;

    // // Simple test to see if they are far away (with smallest spheres outside)
    // Create Spheres
    
    double dist_above_bottom = 0.5*(radius_top_1*radius_top_1+height_1*height_1-radius_bottom_1*radius_bottom_1)/height_1;
    double dist_from_center = dist_above_bottom - 0.5*height_1;
    Coords sphere_1_pos = coords_set(center_1.x+direction_1.x*dist_from_center,
                                     center_1.y+direction_1.y*dist_from_center,
                                     center_1.z+direction_1.z*dist_from_center);
    double sphere_1_radius = sqrt(radius_bottom_1*radius_bottom_1+dist_above_bottom*dist_above_bottom);

    double sphere_2_radius = sqrt(radius_2*radius_2+height_2*height_2);
    Coords sphere_2_pos = center_2;

    //print_position(sphere_1_pos,"sphere_1 pos");
    //printf("sphere_1 radius = %lf \n", sphere_1_radius);
    //print_position(sphere_2_pos,"sphere_2 pos");
    //printf("sphere_2 radius = %lf \n", sphere_2_radius);
    // Test if spheres are too long apart to have any chance of intersecting

    double dist_spheres = sqrt((sphere_1_pos.x-sphere_2_pos.x)*(sphere_1_pos.x-sphere_2_pos.x)+(sphere_1_pos.y-sphere_2_pos.y)*(sphere_1_pos.y-sphere_2_pos.y)+(sphere_1_pos.z-sphere_2_pos.z)*(sphere_1_pos.z-sphere_2_pos.z));


    if (dist_spheres > sphere_1_radius + sphere_2_radius){
        printf("\nSpherical method determined that cones are too far away for intersection to be relevant\n");
        return 0;
    }

    // // Simple test to see if they are inside (with largest spheres inside)
    // Brute force in two steps.
    // 1. Check if any points on 1 lies within 2
    // 2. Check if any transversal lines on the mesh of 1 intersects with 2

    // Calculate needed information
    Coords cone_1_bottom_point = coords_add(center_1,coords_scalar_mult(direction_1,-0.5*height_1));
    Coords cone_1_top_point = coords_add(center_1,coords_scalar_mult(direction_1,0.5*height_1));
    Coords cone_2_bottom_point = coords_add(center_2,coords_scalar_mult(direction_2,-0.5*height_2));
    Coords cone_2_top_point = coords_add(center_2,coords_scalar_mult(direction_2,0.5*height_2));


    // Create two circles for both geometries
    int resoultuion = 300;

    struct pointer_to_1d_coords_list cone_1_points = geometry_cone->shell_points(geometry_cone,resoultuion);
    

    //points_on_circle(cone_1_top,cone_1_top_point,direction_1,radius_top_1,resoultuion);
    //points_on_circle(cone_1_bottom,cone_1_bottom_point,direction_1,radius_bottom_1,resoultuion);


    //printf("\nTEST\n");
    int i;
    // Test geometry 1 points inside geometry 2

    for (i = 0 ; i < cone_1_points.num_elements ; i++){
        
        if (r_within_cylinder(cone_1_points.elements[i],geometry_cylinder) == 1){
            //printf("\nOne point on cone 1 is inside cone 2\n");
            return 1;
        }
    }

    struct pointer_to_1d_coords_list cone_2_points = geometry_cylinder->shell_points(geometry_cylinder,resoultuion);

    // Test geometry 2 points inside geometry 1
    for (i = 0 ; i < cone_2_points.num_elements ; i++){
        
        if (r_within_cone(cone_2_points.elements[i],geometry_cone) == 1){
            //printf("\nOne point on cone 2 is inside cone 1\n");
            return 1;
        }
    }


    // Test 1 within 2


    // // Test if there is any intersection (intersection function or eqation?)

    // This is an implementation of brute force. Maybe do this with a calculated function?
    int circ_resolution = 150;  // how many lines will the be checked for
    int height_resolution = 300;

    double length_of_cone_side_1 = sqrt(pow(radius_top_1-radius_bottom_1,2)+pow(height_1,2));
    double length_of_cone_side_2 = sqrt(pow(radius_top_2-radius_bottom_2,2)+pow(height_2,2));

    double slope_1 = (radius_top_1-radius_bottom_1)/height_1;
    double slope_2 = (radius_top_2-radius_bottom_2)/height_2;

    double local_radius;

    Coords cone_1_direction = geometry_cone->geometry_parameters.p_cone_storage->direction_vector;
    Coords cone_2_direction = geometry_cylinder->geometry_parameters.p_cylinder_storage->direction_vector;

    //printf("\nlength_of_cone_side_1 = %f\n",length_of_cone_side_1);

    Coords circ_points[150];
    double circ_offset;
    Coords circ_center;

    int j;

    for (i = 0 ; i < height_resolution ; i++){
        // Calculate circ offset
        circ_offset = i * height_1 / height_resolution;

        // Calculate middle point
        circ_center = coords_add(cone_1_bottom_point,coords_set(cone_1_direction.x * circ_offset,cone_1_direction.y * circ_offset,cone_1_direction.z * circ_offset));

        // Calculate radius
        local_radius = circ_offset * slope_1 + radius_bottom_1;

        // Make points on circle
        //printf("points on circle: circ_center = [%f,%f,%f] , cone_1_direction = [%f,%f,%f] , local_radius = %f , circ_resolution = %i",circ_center.x,circ_center.y,circ_center.z,cone_1_direction.x,cone_1_direction.y,cone_1_direction.z,local_radius,circ_resolution);
        points_on_circle(circ_points,circ_center,cone_1_direction,local_radius,circ_resolution);

        // Test if any points lies within geomtry 2
        for (j = 0 ; j < circ_resolution; j++){
            //printf("\ntested if point [%i] [%f,%f,%f] is inside",j,circ_points[j].x,circ_points[j].y,circ_points[j].z);
            if (r_within_cylinder(circ_points[j],geometry_cylinder) == 1){
                //printf("\nOne point on cone 1 is inside cone 2\n");
                return 1;
            }
        }
    }


    for (i = 0 ; i < height_resolution ; i++){
        // Calculate circ offset
        circ_offset = i * height_2 / height_resolution;

        // Calculate middle point
        circ_center = coords_add(cone_2_bottom_point,coords_set(cone_2_direction.x * circ_offset,cone_2_direction.y * circ_offset,cone_2_direction.z * circ_offset));

        // Calculate radius
        local_radius = circ_offset * slope_2 + radius_bottom_2;

        // Make points on circle
        //printf("points on circle: circ_center = [%f,%f,%f] , cone_1_direction = [%f,%f,%f] , local_radius = %f , circ_resolution = %i",circ_center.x,circ_center.y,circ_center.z,cone_2_direction.x,cone_2_direction.y,cone_2_direction.z,local_radius,circ_resolution);
        points_on_circle(circ_points,circ_center,cone_2_direction,local_radius,circ_resolution);

        // Test if any points lies within geomtry 2
        for (j = 0 ; j < circ_resolution; j++){
            //printf("\ntested if point [%i] [%f,%f,%f] is inside",j,circ_points[j].x,circ_points[j].y,circ_points[j].z);
            if (r_within_cone(circ_points[j],geometry_cone) == 1){
                //printf("\nOne point on cone 2 is inside cone 1\n");
                return 1;
            }
        }
    }

    return 0;

};

int cylinder_overlaps_cone(struct geometry_struct *geometry_cylinder,struct geometry_struct *geometry_cone) {
  // This problem is symetrical.
  return cone_overlaps_cylinder(geometry_cone,geometry_cylinder);
};

int cone_overlaps_box(struct geometry_struct *geometry_cone,struct geometry_struct *geometry_box) {
  // Overlap function should return 1 if the to geometries both cover some volume
    //  cone_overlaps_box(struct geometry_struct *geometry_cone,struct geometry_struct *geometry_box)

    // Load Variables:
    Coords direction_cone = geometry_cone->geometry_parameters.p_cone_storage->direction_vector;
    Coords center_cone = geometry_cone->center;
    double radius_top_cone = geometry_cone->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius_bottom_cone = geometry_cone->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height_cone = geometry_cone->geometry_parameters.p_cone_storage->height;

    //Coords normal_vectors_box[6] = geometry_box->geometry_parameters.p_box_storage->normal_vectors;
    int is_rectangle =  geometry_box->geometry_parameters.p_box_storage->is_rectangle;
    double x_width1 = geometry_box->geometry_parameters.p_box_storage->x_width1;
    double y_height1 = geometry_box->geometry_parameters.p_box_storage->y_height1;
    double z_depth= geometry_box->geometry_parameters.p_box_storage->z_depth;
    double x_width2 = geometry_box->geometry_parameters.p_box_storage->x_width2;
    double y_height2 = geometry_box->geometry_parameters.p_box_storage->y_height2;
    Coords x_vector = geometry_box->geometry_parameters.p_box_storage->x_vector;
    Coords y_vector = geometry_box->geometry_parameters.p_box_storage->y_vector;
    Coords z_vector = geometry_box->geometry_parameters.p_box_storage->z_vector;
    Coords center_box = geometry_box->center;
    //Coords direction_box = geometry_cone->geometry_parameters.p_box_storage->direction;



    double Y;
    double max_r;


    // // Simple test to see if they are far away (with smallest spheres outside)
    // Create Spheres

    /*
    Y = -(0.5*height_cone)-(radius_top_cone*radius_top_cone-radius_bottom_cone*radius_bottom_cone)/(2*height_cone);
    if (radius_top_cone > radius_bottom_cone){
        max_r = radius_top_cone;
    }else{
        max_r = radius_bottom_cone;
    }
    double sphere_1_radius =  sqrt((Y+(1/2)*height_cone)*(Y+(1/2)*height_cone)+max_r*max_r);
    Coords sphere_1_pos = coords_set(center_cone.x+direction_cone.x*Y,center_cone.y+direction_cone.y*Y,center_cone.z+direction_cone.z*Y);
    */

    double dist_above_bottom = 0.5*(radius_top_cone*radius_top_cone+height_cone*height_cone-radius_bottom_cone*radius_bottom_cone)/height_cone;
    double dist_from_center = dist_above_bottom - 0.5*height_cone;
    Coords sphere_1_pos = coords_set(center_cone.x+direction_cone.x*dist_from_center,
                                     center_cone.y+direction_cone.y*dist_from_center,
                                     center_cone.z+direction_cone.z*dist_from_center);
    double sphere_1_radius = sqrt(radius_bottom_cone*radius_bottom_cone+dist_above_bottom*dist_above_bottom);


    double dist_to_corner;
    double sphere_2_radius = 0;

    dist_to_corner = sqrt(pow(x_width1,2)+pow(x_width1,2));
    if (dist_to_corner > sphere_2_radius); { sphere_2_radius = dist_to_corner ; }
    dist_to_corner = sqrt(pow(x_width1,2)+pow(x_width2,2));
    if (dist_to_corner > sphere_2_radius); { sphere_2_radius = dist_to_corner ; }
    dist_to_corner = sqrt(pow(x_width2,2)+pow(x_width1,2));
    if (dist_to_corner > sphere_2_radius); { sphere_2_radius = dist_to_corner ; }
    dist_to_corner = sqrt(pow(x_width2,2)+pow(x_width2,2));
    if (dist_to_corner > sphere_2_radius); { sphere_2_radius = dist_to_corner ; }

    Coords sphere_2_pos = center_box;


    // Test if spheres are too long apart to have any chance of intersecting

    double dist_spheres = sqrt((sphere_1_pos.x-sphere_2_pos.x)*(sphere_1_pos.x-sphere_2_pos.x)+(sphere_1_pos.y-sphere_2_pos.y)*(sphere_1_pos.y-sphere_2_pos.y)+(sphere_1_pos.z-sphere_2_pos.z)*(sphere_1_pos.z-sphere_2_pos.z));


    if (dist_spheres > sphere_1_radius + sphere_2_radius){
        //printf("\nSpherical method determined that cones are too far away for intersection to be relevant\n");
        return 0;
    }

    // // Simple test to see if they are inside (with largest spheres inside)
    // Brute force in two steps.
    // 1. Check if any points on 1 lies within 2
    // 2. Check if any transversal lines on the mesh of 1 intersects with 2

    // Calculate needed information
    Coords cone_bottom_point = coords_add(center_cone,coords_scalar_mult(direction_cone,-0.5*height_cone));
    Coords cone_top_point = coords_add(center_cone,coords_scalar_mult(direction_cone,0.5*height_cone));



    // Create two circles for both geometries
    int resoultuion = 300;



    struct pointer_to_1d_coords_list cone_points = geometry_cone->shell_points(geometry_cone,resoultuion);
    struct pointer_to_1d_coords_list box_points = geometry_box->shell_points(geometry_box,resoultuion);

    //points_on_circle(cone_1_top,cone_top_point,direction_cone,radius_top_cone,resoultuion);
    //points_on_circle(cone_1_bottom,cone_bottom_point,direction_cone,radius_bottom_cone,resoultuion);


    //printf("\nTEST\n");
    int i;
    // Test cone points inside box

    for (i = 0 ; i < cone_points.num_elements ; i++){
        
        if (r_within_box_advanced(cone_points.elements[i],geometry_box) == 1){
            //printf("\nOne point on cone is inside box\n");
            return 1;
        }
    }

    // Test box points inside cone
    for (i = 0 ; i < box_points.num_elements ; i++){
        
        if (r_within_cone(box_points.elements[i],geometry_cone) == 1){
            //printf("\nOne point on box is inside cone\n");
            return 1;
        }
    }


    // Test 1 within 2


    // // Test if there is any intersection (intersection function or eqation?)


    // // Add more points
    // This is an implementation of brute force. Maybe do this with a calculated function?
    int circ_resolution = 50;  // how many lines will the be checked for
    int height_resolution = 150;

    double length_of_cone_side = sqrt(pow(radius_top_cone-radius_bottom_cone,2)+pow(height_cone,2));
    double length_of_box_side = z_depth;

    double slope_1 = (radius_top_cone-radius_bottom_cone)/height_cone;

    double local_radius;

    Coords cone_direction = geometry_cone->geometry_parameters.p_cone_storage->direction_vector;
    //Coords box_direction = geometry_box->geometry_parameters.p_box_storage->direction_vector;

    //printf("\nlength_of_cone_side = %f\n",length_of_cone_side);

    Coords circ_points[50];
    double circ_offset;
    Coords circ_center;

    Coords square_points[8];
    double square_offset;
    Coords square_center;
    Coords box_end_point = coords_sub(coords_set(0,0,-z_depth/2),square_center);

    int j;

    for (i = 0 ; i < height_resolution ; i++){
        // Calculate circ offset
        //circ_offset = i * length_of_cone_side / height_resolution; // Possible bug
        circ_offset = i * height_cone / height_resolution; // Possible bug

        // Calculate middle point
        circ_center = coords_add(cone_bottom_point,coords_set(cone_direction.x * circ_offset,cone_direction.y * circ_offset,cone_direction.z * circ_offset));

        // Calculate radius
        local_radius = circ_offset * slope_1 + radius_bottom_cone;

        // Make points on circle
        //printf("points on circle: circ_center = [%f,%f,%f] , cone_direction = [%f,%f,%f] , local_radius = %f , circ_resolution = %i",circ_center.x,circ_center.y,circ_center.z,cone_direction.x,cone_direction.y,cone_direction.z,local_radius,circ_resolution);
        points_on_circle(circ_points,circ_center,cone_direction,local_radius,circ_resolution);

        // Test if any points lies within geomtry 2
        for (j = 0 ; j < circ_resolution; j++){
            //printf("\ntested if point [%i] [%f,%f,%f] is inside",j,circ_points[j].x,circ_points[j].y,circ_points[j].z);
            if (r_within_box_advanced(circ_points[j],geometry_box) == 1){
                //printf("\nOne point on cone 1 is inside cone 2\n");
                return 1;
            }
        }
    }

    double box_offset;

    for (i = 0 ; i < height_resolution ; i++){
        // Calculate circ offset
        box_offset = i * length_of_box_side / height_resolution;

        // Calculate middle point
        square_center = coords_add(box_end_point,coords_set(z_vector.x * box_offset,z_vector.y * box_offset,z_vector.z * box_offset));

        // Calculate radius

        // Make points on square
        square_points[0]=coords_add(square_center,coords_set(x_width1/2,0,0)); // A point on the side of the box
        square_points[1]=coords_add(square_points[0],coords_set(0,y_height1/2,0)); // Corner
        square_points[2]=coords_add(square_points[0],coords_set(0,-y_height1/2,0)); // Corner
        square_points[3]=coords_add(square_center,coords_set(-x_width1/2,0,0)); // A point on the side of the box
        square_points[4]=coords_add(square_points[3],coords_set(0,y_height1/2,0)); // Corner
        square_points[5]=coords_add(square_points[3],coords_set(0,-y_height1/2,0)); // Corner
        
        square_points[6]=coords_add(square_center,coords_set(0,y_height1/2,0)); // A point on the side of
        square_points[7]=coords_add(square_center,coords_set(0,-y_height1/2,0)); // A point on the side of

        // Test if any points lies within geomtry 2
        for (j = 0 ; j < 3; j++){
            
            if (r_within_cone(square_points[j],geometry_cone) == 1){
                //printf("\nOne point on cone 2 is inside cone 1\n");
                return 1;
            }
        }
    }
    
    return 0;

};

int box_overlaps_cone(struct geometry_struct *geometry_box,struct geometry_struct *geometry_cone) {
  // This problem is symetrical.
  return cone_overlaps_box(geometry_cone,geometry_box);
}

// -------------    Within functions for two different geometries ---------------------------------

double dist_from_point_to_plane(Coords point,Coords plane_p1, Coords plane_p2, Coords plane_p3) {

  /*
  printf("Dist from point to plane stuff ---- \n");
  print_position(point,"point");
  print_position(plane_p1,"plane_p1");
  print_position(plane_p2,"plane_p2");
  print_position(plane_p3,"plane_p3");
  */
  // transform three points into normal vector
  Coords vector_1 = coords_sub(plane_p2,plane_p1);
  Coords vector_2 = coords_sub(plane_p3,plane_p1);
  
  Coords normal_vector;
  
  vec_prod(normal_vector.x,normal_vector.y,normal_vector.z,vector_1.x,vector_1.y,vector_1.z,vector_2.x,vector_2.y,vector_2.z);
  
  double denominator = length_of_position_vector(normal_vector);
  
  normal_vector = coords_scalar_mult(normal_vector,1.0/denominator);

  //print_position(normal_vector,"normal vector in dist from point to plane");
  
  Coords diff = coords_sub(point,plane_p1);
  
  return fabs(scalar_prod(normal_vector.x,normal_vector.y,normal_vector.z,diff.x,diff.y,diff.z));
};

int box_within_cylinder(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Is geometry child inside geometry parent?
    // For box child to be inside of cylinder parent, all corners of box child must be inside of box parent.
    
    // Generate coordinates of corners of the box
    Coords corner_points[8];
    box_corners_global_frame(corner_points,geometry_child);
    
    // Check earch corner seperatly
    int iterate;
    for (iterate=0;iterate<8;iterate++) {
        if (geometry_parent->within_function(corner_points[iterate],geometry_parent) == 0) {
            return 0; // If a corner is outside, box child is not within cylinder parent
        }
    }
    return 1; // If no corner was outside, the box is inside the cylinder
};

int cylinder_within_box(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Is geometry child inside geometry parent?
    // For box child to be inside of cylinder parent, all corners of box child must be inside of box parent.
    
    Coords cyl_direction = geometry_child->geometry_parameters.p_cylinder_storage->direction_vector;
    Coords center = geometry_child->center;
    double radius = geometry_child->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height = geometry_child->geometry_parameters.p_cylinder_storage->height;
    
    Coords cyl_top_point = coords_add(center,coords_scalar_mult(cyl_direction,0.5*height));
    Coords cyl_bottom_point = coords_add(center,coords_scalar_mult(cyl_direction,-0.5*height));
    
    // quick escape: if end points of cylinder not in box, return 0
    if (geometry_parent->within_function(cyl_top_point,geometry_parent) == 0) return 0;
    if (geometry_parent->within_function(cyl_bottom_point,geometry_parent) == 0) return 0;
    
    // Generate 30 points on the circle describing the top of the cylinder
    Coords *circle_point_array;
    int number_of_points = 30;
    circle_point_array = malloc(number_of_points * sizeof(Coords));
    
    points_on_circle(circle_point_array,cyl_top_point,cyl_direction,radius,number_of_points);
    
    // Check parts of cylinder top seperatly
    int iterate;
    for (iterate=0;iterate<number_of_points;iterate++) {
        if (geometry_parent->within_function(circle_point_array[iterate],geometry_parent) == 0) {
            return 0; // If part of the cylinder is outside the box, the cylinder is not inside the box
        }
    }
    
    // Check parts of cylinder bottom seperatly
    points_on_circle(circle_point_array,cyl_bottom_point,cyl_direction,radius,number_of_points);
    for (iterate=0;iterate<number_of_points;iterate++) {
        if (geometry_parent->within_function(circle_point_array[iterate],geometry_parent) == 0) {
            return 0; // If part of the cylinder is outside the box, the cylinder is not inside the box
        }
    }
    
    free(circle_point_array);
    
    return 1; // If no part of the cylinders end caps was outside, the cylinder is inside box 1
};

int cylinder_within_sphere(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Is a cylinder within a sphere?
    
    double cyl_radius = geometry_child->geometry_parameters.p_cylinder_storage->cyl_radius;
    double cyl_height = geometry_child->geometry_parameters.p_cylinder_storage->height;
    double sph_radius = geometry_parent->geometry_parameters.p_sphere_storage->sph_radius;
    
    // Quick checks to avoid overhead from A_within_B
    // Is the height of the cylinder larger than diameter of the sphere?
    if (cyl_height > 2.0*sph_radius) return 0;
    
    // Is the radius of the cylidner larger than the radius of the sphere?
    if (cyl_radius > sph_radius) return 0;
    
    // Is the center of the cylinder so far from the center of the sphere that it cant fit?
    double distance = distance_between(geometry_child->center,geometry_parent->center);
    if (0.5*cyl_height > cyl_radius) {
        if (sqrt(distance*distance + 0.25*cyl_height*cyl_height) > sph_radius)
            return 0;
    } else {
        if (sqrt(distance*distance + cyl_radius*cyl_radius) > sph_radius)
            return 0;
    }
    
    // Reasonable to brute force solution here
    return A_within_B(geometry_child,geometry_parent,(int) 400); // 200 points on each end cap
};

int sphere_within_cylinder(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Is a sphere (child) within a cylinder (parent)?
    
    // If the center is not inside, one can exit fast
    Coords sph_center = geometry_child->center;
    if (geometry_parent->within_function(sph_center,geometry_parent) == 0) return 0;
    
    // Generate cylinder with height = height - r_s and r_c = r_c - 2*r_s and check if point is within.
    
    // Done by modifying parent cylinder.
    double original_radius = geometry_parent->geometry_parameters.p_cylinder_storage->cyl_radius;
    double original_height = geometry_parent->geometry_parameters.p_cylinder_storage->height;
    
    // Need sphere
    double sph_radius = geometry_child->geometry_parameters.p_sphere_storage->sph_radius;
    
    if (original_radius - sph_radius > 0 && original_height - 2.0*sph_radius > 0) {
      geometry_parent->geometry_parameters.p_cylinder_storage->cyl_radius = original_radius - sph_radius;
      geometry_parent->geometry_parameters.p_cylinder_storage->height = original_height - 2.0*sph_radius;
    } else return 0;
    
    int return_value = geometry_parent->within_function(sph_center,geometry_parent);
    
    // Reset the cylinder to it's original values (important not to return before)
    geometry_parent->geometry_parameters.p_cylinder_storage->cyl_radius = original_radius;
    geometry_parent->geometry_parameters.p_cylinder_storage->height = original_height;
    
    return return_value;
};

int box_within_sphere(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // If all 8 corners of the box are inside the sphere, the entire box is inside
    
    return A_within_B(geometry_child,geometry_parent,8);
};

int sphere_within_box(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Distance from any of the box sides must be greater than radius, and the center inside.
    
    // debug test, use A_within_B
    //return A_within_B(geometry_child,geometry_parent,100*100);
    
    // If the center is not inside, one can exit fast
    Coords sph_center = geometry_child->center;
    if (geometry_parent->within_function(sph_center,geometry_parent) == 0) {
      //printf("sphere not child of box because it's center is not in the box \n");
      return 0;
    }
    
    double radius = geometry_child->geometry_parameters.p_sphere_storage->sph_radius;
    
    // 6 planes
    // +z -z easy as are parallel and simple in the box's coordinate system
    Coords coordinates = coords_sub(sph_center,geometry_parent->center);
    
    // Rotate the position around the center of the box
    Coords rotated_coordinates;
    rotated_coordinates = rot_apply(geometry_parent->transpose_rotation_matrix,coordinates);
    
    double depth = geometry_parent->geometry_parameters.p_box_storage->z_depth;
    if (rotated_coordinates.z < -0.5*depth+radius || rotated_coordinates.z > 0.5*depth-radius) {
      //printf("sphere not child of box because it's center to close to z plane \n");
      return 0;
    }
    
    Coords corner_ps[8];
    box_corners_global_frame(corner_ps,geometry_parent);
    
    // The first 4 points are in the -z plane, the last 4 in the +z plane.
    
    // In the -z plane, 0 has neighbors 1 and 3, in the opposite 4
    // In the -z plane, 2 has neighbors 1 and 3, in the opposite 6
    
    // Then these are the four necessary calls for the two plans described by each group.
    double debug_dist;
    if ((debug_dist = dist_from_point_to_plane(sph_center,corner_ps[0],corner_ps[4],corner_ps[1])) < radius ) {
      //printf("sphere not child of box because it's center too close to plane 1, as distance was %f\n",debug_dist);
      return 0;
    }
    if ((debug_dist = dist_from_point_to_plane(sph_center,corner_ps[0],corner_ps[4],corner_ps[3])) < radius ) {
      //printf("sphere not child of box because it's center too close to plane 2, as distance was %f\n",debug_dist);
      return 0;
    }
    if ((debug_dist = dist_from_point_to_plane(sph_center,corner_ps[2],corner_ps[6],corner_ps[1])) < radius ) {
      //printf("sphere not child of box because it's center too close to plane 3, as distance was %f\n",debug_dist);
      return 0;
    }
    if ((debug_dist = dist_from_point_to_plane(sph_center,corner_ps[2],corner_ps[6],corner_ps[3])) < radius ) {
      //printf("sphere not child of box because it's center too close to plane 4, as distance was %f\n",debug_dist);
      return 0;
    }
    
    return 1; // If the cylinder center is inside, and more than radius away from all walls, it is inside
};

int cone_within_sphere(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Function returns 1 if the cone is completely within the sphere, 0 otherwise
    // Brute force place holder
    return A_within_B(geometry_child,geometry_parent,(int) 300); // 150 points on each end cap
};

int cone_within_cylinder(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Function returns 1 if the cone is completely within the cylinder, 0 otherwise
    // Brute force place holder
    //return A_within_B(geometry_child,geometry_parent,(int) 60); // 30 points on each end cap
    // This now works for the simple case where the two directions are parallel. Otherwise it uses A within B.

    // Unpack parameters
    double radius1 = geometry_parent->geometry_parameters.p_cylinder_storage->cyl_radius;
    double height1 = geometry_parent->geometry_parameters.p_cylinder_storage->height;
    double radius2_top = geometry_child->geometry_parameters.p_cone_storage->cone_radius_top;
    double radius2_bottom = geometry_child->geometry_parameters.p_cone_storage->cone_radius_bottom;
    double height2 = geometry_child->geometry_parameters.p_cone_storage->height;
    
    int verbal = 0;
    // Generate unit direction vector along center axis of cylinders
    
    // Start with vector that points along the cylinder in the simple frame, and rotate to global
    Coords simple_vector = coords_set(0,1,0);
    Coords vector1,vector2;
    if (verbal == 1) printf("Cords start_vector = (%f,%f,%f)\n",simple_vector.x,simple_vector.y,simple_vector.z);

    // Rotate the position of the ray around the center of the cylinder
    vector1 = rot_apply(geometry_parent->rotation_matrix,simple_vector);
    if (verbal == 1) printf("Cords vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector1.z);
    // Rotate the position of the ray around the center of the cylinder
    vector2 = rot_apply(geometry_child->rotation_matrix,simple_vector);
    if (verbal == 1) printf("Cords vector2 = (%f,%f,%f)\n",vector2.x,vector2.y,vector2.z);
    
    // if vector1 and vector2 are parallel, the problem is simple, but if not complicated
    double cross_product1[3] = {0,0,0};
    // printf("%f\n",cross_product1[0]);
    // vec prod(&ax,&ay,&az,bx,by,bz, cx,cy,cz)
    vec_prod(cross_product1[0],cross_product1[1],cross_product1[2],vector1.x,vector1.y,vector1.z,vector2.x,vector2.y,vector2.z);
    // I get an error taking the adress of cross_product1[0], &cross_product1[0]. Took the pointer adresses instead. Works fine.
    if (verbal == 1) printf("cross_product = (%f,%f,%f)\n",cross_product1[0],cross_product1[1],cross_product1[2]);
    double cross_product_length = length_of_3vector(cross_product1);
    
    if (cross_product_length == 0) {
        // The cylinders are parallel.
        int seperated = 0;
        double delta[3];
        delta[0] = geometry_parent->center.x - geometry_child->center.x;
        delta[1] = geometry_parent->center.y - geometry_child->center.y;
        delta[2] = geometry_parent->center.z - geometry_child->center.z;

        // Test for separation by height
        // if (h0Div2 + h1Div2 − |Dot(W0, Delta )| < 0) seperated = 1;
        
        if (verbal == 1) printf("vector1 = (%f,%f,%f)\n",vector1.x,vector1.y,vector2.z);
        if (verbal == 1) printf("delta1 = (%f,%f,%f)\n",delta[0],delta[1],delta[2]);
        double scalar_prod1 = scalar_prod(vector1.x,vector1.y,vector1.z,delta[0],delta[1],delta[2]);
        if (verbal == 1) printf("scalar product = %f \n",scalar_prod1);
        if (verbal == 1) printf("height 1 = %f, height 2 = %f \n",height1,height2);
        
        int inside = 1;
        
        if (height1*0.5 < height2*0.5 + fabs(scalar_prod1)) {
                if (verbal == 1) printf("Cylinder sticks out height wise \n");
                inside = 0;
                }
    
        // Test for separation radially
        // if (rSum − |Delta − Dot(W0,Delta)∗W0| < 0) seperated = 1;
        double vector_between_cyl_axis[3];
        vector_between_cyl_axis[0] = delta[0] - scalar_prod1*vector1.x;
        vector_between_cyl_axis[1] = delta[1] - scalar_prod1*vector1.y;
        vector_between_cyl_axis[2] = delta[2] - scalar_prod1*vector1.z;
        if (verbal == 1) printf("vector_between = (%f,%f,%f)\n",vector_between_cyl_axis[0],vector_between_cyl_axis[1],vector_between_cyl_axis[2]);
        if (verbal == 1) printf("length of vector between = %f\n",length_of_3vector(vector_between_cyl_axis));
        if (verbal == 1) printf("radius1 = %f , radius2_top=%f , radius2_bottom=%f\n",radius1,radius2_top,radius2_bottom);


        if (radius1 < fmax(radius2_top,radius2_bottom) + length_of_3vector(vector_between_cyl_axis)) {
                if (verbal == 1) printf("Cylinder sticks out radially \n");
                inside = 0;
                }
        
        if (inside == 0) return 0;
        else return 1;
        
    } else {
        
    // Make shell points and check if they are inside
    return A_within_B(geometry_child,geometry_parent,(int) 200); // 100 points on each end cap

    }

};

int cone_within_box(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Function returns 1 if the cone is completely within the box, 0 otherwise
    // Brute force place holder
    return A_within_B(geometry_child,geometry_parent,(int) 300); // 150 points on each end cap
};

int sphere_within_cone(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Function returns 1 if the sphere is completely within the cone, 0 otherwise
    // Brute force place holder
    return A_within_B(geometry_child,geometry_parent,(int) 300); // 150 points on each end cap
};

int cylinder_within_cone(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Function returns 1 if the cylinder is completely within the cone, 0 otherwise
    // Brute force place holder
    return A_within_B(geometry_child,geometry_parent,(int) 300); // 150 points on each end cap
};

int box_within_cone(struct geometry_struct *geometry_child,struct geometry_struct *geometry_parent) {
    // Function returns 1 if the box is completely within the cone, 0 otherwise
    // Brute force place holder
    return A_within_B(geometry_child,geometry_parent,(int) 300); // 150 points on each end cap
};
