/*******************************************************************************
*
*  McXtrace, photon ray-tracing package
*  Copyright(C) 2007 Risoe National Laboratory.
*
* %I
* Written by: Mads Bertelsen
* Date: 20.08.15
* Version: $Revision: 0.1 $
* Origin: Svanevej 19
*
* Functions and structure definitons for Union components.
*
******************************************************************************/

// -------------    Definition of data structures   ---------------------------------------------
struct intersection_time_table_struct {
int num_volumes;
int *calculated;
int *n_elements;
double **intersection_times;
};

struct line_segment{
//struct position point1;
//struct position point2;
Coords point1;
Coords point2;
int number_of_dashes;
};

struct pointer_to_1d_int_list {
int num_elements;
int *elements;
};

struct pointer_to_1d_double_list {
int num_elements;
double *elements;
};

struct pointer_to_1d_coords_list {
int num_elements;
Coords *elements;
};

struct lines_to_draw{
int number_of_lines;
struct line_segment *lines;
};

// 2D lists not needed anyway
// struct pointer_to_2d_int_list {
// int n_lists;
// struct pointer_to_1d_int_list *lists;
// }

// Todo: see if the union geometry_parameter_union and other geometry structs can be here
union geometry_parameter_union{
    struct sphere_storage   *p_sphere_storage;
    struct cylinder_storage *p_cylinder_storage;
    struct box_storage      *p_box_storage;
    struct cone_storage     *p_cone_storage;
    struct mesh_storage     *p_mesh_storage;
    // add as many pointers to structs as wanted, without increasing memory footprint.
};


struct rotation_struct{
double x;
double y;
double z;
};

struct focus_data_struct {
Coords Aim;
double angular_focus_width;
double angular_focus_height;
double spatial_focus_width;
double spatial_focus_height;
double spatial_focus_radius;
Rotation absolute_rotation;
// focusing_function creates a vector per selected criteria of focus_data_struct / selected focus function and returns solid angle
void (*focusing_function)(Coords*, double*, struct focus_data_struct*);
//                        v_out  , solid_a,
};

struct focus_data_array_struct
{
struct focus_data_struct *elements;
int num_elements;
};

struct Detector_3D_struct {
  char title_string[256];
  char string_axis_1[256];
  char string_axis_2[256];
  char string_axis_3[256];
  char Filename[256];
  double D1min;
  double D1max;
  double D2min;
  double D2max;
  double D3min;
  double D3max;
  double bins_1; // McXtrace uses doubles for bin numbers for some reason
  double bins_2;
  double bins_3;
  double ***Array_N; // McXtrace uses doubles for number of rays in each bin for some reason
  double ***Array_p;
  double ***Array_p2;
};

struct Detector_2D_struct {
  char title_string[256];
  char string_axis_1[256];
  char string_axis_2[256];
  char Filename[256];
  double D1min;
  double D1max;
  double D2min;
  double D2max;
  double bins_1; // McXtrace uses doubles for bin numbers for some reason
  double bins_2;
  double **Array_N; // McXtrace uses doubles for number of rays in each bin for some reason
  double **Array_p;
  double **Array_p2;
};

struct Detector_1D_struct {
  char title_string[256];
  char string_axis[256];
  char string_axis_short[64];
  char string_axis_value[256];
  char Filename[256];
  double min;
  double max;
  double bins; // McXtrace uses doubles for bin numbers for some reason
  double *Array_N; // McXtrace uses doubles for number of rays in each bin for some reason
  double *Array_p;
  double *Array_p2;
};


union logger_data_union{
  struct a_2DQ_storage_struct *p_2DQ_storage;
  struct a_2DS_storage_struct *p_2DS_storage;
  struct a_3DS_storage_struct *p_3DS_storage;
  struct a_1D_storage_struct *p_1D_storage;
  struct a_2DS_t_storage_struct *p_2DS_t_storage;
  struct a_2D_kf_t_storage_struct *p_2D_kf_t_storage;
  // Additional logger storage structs to be addedd
};

struct logger_with_data_struct {
  int used_elements;
  int allocated_elements;
  struct logger_struct **logger_pointers;
};

// logger_pointer_struct
// contains pointers to the different logger functions and it's data union

struct logger_pointer_set_struct {
  // The logger has two record functions, an active and an inactive. Normally the active one will be to permanent storage,
  //  but if a conditional has been defined, it can switch the two, making the active one recording to temporary, which
  //  can then be filtered based on the future path of the ray
  
  // function input Coords position, k[3], k_old[3], p, p_old, NV, NPV, N, logger_data_union, logger_with_data_struct
  void (*active_record_function)(Coords*, double*, double*, double, double, double, int, int, int, struct logger_struct*, struct logger_with_data_struct*);
  void (*inactive_record_function)(Coords*, double*, double*, double, double, double, int, int, int, struct logger_struct*, struct logger_with_data_struct*);
  
  // A clear temporary data function (for new ray)
  void (*clear_temp)(union logger_data_union*);
  
  // Write temporary to permanent is used when the record to temp function is active, and the condition is met.
  void (*temp_to_perm)(union logger_data_union*);
  
  // Write temporary final_p to permanent is used when the record to temp function is active, and the condition is met
  //  and the final weight is given to be used for all stored events in the logger.
  void (*temp_to_perm_final_p)(union logger_data_union*, double);
  
  // Select which temp_to_perm function to use
  int select_t_to_p; // 1: temp_to_perm, 2: temp_to_perm_final_p
  
};


struct conditional_standard_struct{
  // Data to be transfered to the conditional function
  double Emax;
  double Emin;
  int E_limit;
  
  double Tmin;
  double Tmax;
  int T_limit;
  
  int volume_index;
  
  double Total_scat_max;
  double Total_scat_min;
  int Total_scat_limit;
  
  double exit_volume_index;
  
  // Test
  Coords test_position;
  Rotation test_rotation;
  Rotation test_t_rotation;
};

struct conditional_PSD_struct{
  double PSD_half_xwidth;
  double PSD_half_yheight;
  
  double Tmin;
  double Tmax;
  int T_limit;
  
  // Position of the PSD
  Coords PSD_position;
  Rotation PSD_rotation;
  Rotation PSD_t_rotation;
};

union conditional_data_union {
  struct conditional_standard_struct *p_standard;
  struct conditional_PSD_struct *p_PSD;
  // Add more as conditional components are made
};

// General input for conditional functions: Position, Velocity/Wavevector, weight, time, total_scat, scattered_flag, scattered_flag_VP,
// Optional extras: data_union? tree base(s)? tree base for current ray?
typedef int (*conditional_function_pointer)(union conditional_data_union*,Coords*, Coords*, double*, double*, int*, int*, int*, int**);
//typedef int (**conditional_function_pointer_array)(union *conditional_data_union,Coords*, Coords*, double*, double*, int*, int*, int**);

struct conditional_list_struct{
  int num_elements;
  
  union conditional_data_union **p_data_unions;
  conditional_function_pointer *conditional_functions;
  //int (**conditional_functions)(Coords*, Coords*, double*, double*, int*, int*, int**);
};

struct logger_struct {
  char name[256];
  // Contains ponters to all the functions assosiated with this logger
  struct logger_pointer_set_struct function_pointers;
  // Contains hard copy of logger_data_union since the size is the same as a pointer.
  union logger_data_union data_union;
  
  int logger_extend_index; // Contain index conditional_extend_array defined in master that can be acsessed from extend section.
  
  struct conditional_list_struct conditional_list;
};


// To be stored in volume, a list of pointers to the relevant loggers corresponding to each process
struct logger_for_each_process_list {
  int num_elements;
  struct logger_struct **p_logger_process;
};

// List of logger_for_each_process_list
struct loggers_struct {
  int num_elements;
  struct logger_for_each_process_list *p_logger_volume;
};




struct geometry_struct
{
char shape[64];     // name of shape used (sphere, cylinder, box, off, ...)
double priority_value;    // priority of the geometry
Coords center;      // Center position of volume, reported by components in global frame, updated to main frame in initialize
// Rotation of this volume
Rotation rotation_matrix; // rotation matrix of volume, reported by component in global frame, updated to main frame in initialize
Rotation transpose_rotation_matrix; // As above
// Array of prrotation matrixes for processes assigned to this volume (indexed by non_isotropic_rot_index in the processes)
Rotation *process_rot_matrix_array;             // matrix that transforms from main coordinate system to local process in this specific volume
Rotation *transpose_process_rot_matrix_array;   // matrix that transforms from local process in this specific volume to main coordinate system
int process_rot_allocated;  // Keeps track of allocation status of rot_matrix_array

struct rotation_struct rotation; // Not used, is the x y and z rotation angles.
int visualization_on; // If visualization_on is true, the volume will be drawn in mcdisplay, otherwise not
int is_exit_volume; // If is exit volume = 1, the ray will exit the component when it enters this volume.
int is_mask_volume; // 1 if volume itself is a mask (masking the ones in it's mask list), otherwise 0
int mask_index;
int is_masked_volume; // 1 if this volume is being masked by another volume, the volumes that mask it is in masked_by_list
int mask_mode; // ALL/ANY 1/2. In ALL mode, only parts covered by all masks is simulated, in ANY mode, area covered by just one mask is simulated
double geometry_p_interact; // fraction of rays that interact with this volume for each scattering (between 0 and 1, 0 for disable)
union geometry_parameter_union geometry_parameters; // relevant parameters for this shape
union geometry_parameter_union (*copy_geometry_parameters)(union geometry_parameter_union*);
struct focus_data_struct focus_data; // Used for focusing from this geometry

// New focus data implementation to remove the focusing bug for non-isotripic processes
struct focus_data_array_struct focus_data_array;
struct pointer_to_1d_int_list focus_array_indices; // Add 1D integer array with indecies for correct focus_data for each process


// intersect_function takes position/velocity of ray and parameters, returns time list
int (*intersect_function)(double*,int*,double*,double*,struct geometry_struct*);
//                        t_array,n_ar,r      ,v

// within_function that checks if the ray origin is within this volume
int (*within_function)(Coords,struct geometry_struct*);
//                     r,      parameters

// mcdisplay function, draws the geometry
//void (*mcdisplay_function)(struct lines_to_draw*,int,struct Volume_struct**,int);
void (*mcdisplay_function)(struct lines_to_draw*,int,struct geometry_struct**,int);
//                                lines          index             Geometries   N

void (*initialize_from_main_function)(struct geometry_struct*);

struct pointer_to_1d_coords_list (*shell_points)(struct geometry_struct*, int maximum_number_of_points);

// List of other volumes to be check when ray starts within this volume.
struct pointer_to_1d_int_list intersect_check_list;
// List of other volumes the ray may enter, if the ray intersects the volume itself.
struct pointer_to_1d_int_list destinations_list;
// The destinations list stored as a logic list which makes some tasks quicker. OBSOLETE
//struct pointer_to_1d_int_list destinations_logic_list;
// Reduced list of other volumes the ray may enter, if the ray intersects the volume itself.
struct pointer_to_1d_int_list reduced_destinations_list;
// List of other volumes that are within this volume
struct pointer_to_1d_int_list children;
// List of other volumes that are within this volume, but does not have any parents that are children of this volume
struct pointer_to_1d_int_list direct_children;
// List of next possible volumes (only used in tagging)
struct pointer_to_1d_int_list next_volume_list;
// List of volumes masked by this volume (usually empty)
struct pointer_to_1d_int_list mask_list;
// List of volumes masking this volume
struct pointer_to_1d_int_list masked_by_list;
// List of masks masking this volume (global mask indices)
struct pointer_to_1d_int_list masked_by_mask_index_list;
// Additional intersect lists dependent on mask status
//struct indexed_mask_lists_struct mask_intersect_lists;
// Simpler way of storing the mask_intersect_lists
struct pointer_to_1d_int_list mask_intersect_list;
};

struct physics_struct
{
  char name[256]; // User defined material name
  int interact_control;
  int is_vacuum;
  double my_a;
  int number_of_elements;
  // pointer to element data structures
  struct element_data_struct *p_element_array;
  int number_of_processes;
  // pointer to array of pointers to physics_sub structures that each describe a scattering process
  struct scattering_process_struct *p_scattering_array;
};

union data_transfer_union{
    // List of pointers to storage structs for all supported physical processes
    struct Incoherent_physics_storage_struct  *pointer_to_a_Incoherent_physics_storage_struct;
    struct Powder_physics_storage_struct *pointer_to_a_Powder_physics_storage_struct;
    struct Single_crystal_physics_storage_struct *pointer_to_a_Single_crystal_physics_storage_struct;
    struct Template_physics_storage_struct *pointer_to_a_Template_physics_storage_struct;
    struct Compton_xrl_physics_storage_struct *pointer_to_a_Compton_xrl_physics_storage_struct;
    // possible to add as many structs as wanted, without increasing memory footprint.
};

struct element_data_struct
{
  char name[12];  //element name (leave room for ion things as well
  int multiplicity; // how many atoms are present per unit of the element
  double rho,Ar,Z; //mass density, Atomic weight, and atomic number
  t_Table element_table; // material constants table taken from database
};

struct scattering_process_struct
{
char name[256];                // User defined process name
double process_p_interact;     // double between 0 and 1 that describes the fraction of events forced to undergo this process. -1 for disable
int non_isotropic_rot_index;   // -1 if process is isotrpic, otherwise is the index of the process rotation matrix in the volume
Rotation rotation_matrix;      // rotation matrix of process, reported by component in local frame, transformed and moved to volume struct in main

union data_transfer_union data_transfer; // The way to reach the storage space allocated for this process (see examples in process.comp files)

// probability_for_scattering_functions calculates this probability given k_i and parameters
int (*probability_for_scattering_function)(double*,double*,union data_transfer_union,struct focus_data_struct*);
//                                         prop,   k_i,   ,parameters               , focus data / function

// A scattering_function takes k_i and parameters, returns k_f
int (*scattering_function)(double*,double*,double*,union data_transfer_union,struct focus_data_struct*);
//                         k_f,    k_i,    weight, parameters               , focus data / function
};

struct Volume_struct
{
char name[256]; // User defined volume name
struct geometry_struct geometry;   // Geometry properties (including intersect functions, generated lists)
struct physics_struct *p_physics;  // Physical properties (list of scattering processes, absorption)
struct loggers_struct loggers; // Loggers assosiated with this volume
};

// example of calling a scattering process
// volume_pointer_list[3]->physics.scattering_process[5].probability_for_scattering_function(input,volume_pointer_list[3]->physics.scattering_process[5])

struct starting_lists_struct
{
struct pointer_to_1d_int_list allowed_starting_volume_logic_list;
struct pointer_to_1d_int_list reduced_start_list;
struct pointer_to_1d_int_list start_logic_list;
struct pointer_to_1d_int_list starting_destinations_list;
};

struct global_positions_to_transform_list_struct {
int num_elements;
Coords **positions;
};

struct global_rotations_to_transform_list_struct {
int num_elements;
Rotation **rotations;
};

struct global_process_element_struct
{
char name[128]; // Name of the process
int component_index;
struct scattering_process_struct *p_scattering_process;
};

struct pointer_to_global_process_list {
int num_elements;
struct global_process_element_struct *elements;
};

struct global_material_element_struct
{
char name[128];
int component_index;
struct physics_struct *physics;
};

struct pointer_to_global_material_list {
int num_elements;
struct global_material_element_struct *elements;
};

struct global_geometry_element_struct
{
char name[128];
int component_index;
int activation_counter;
int stored_copies;
int active;
struct Volume_struct *Volume;
};

struct pointer_to_global_geometry_list {
int num_elements;
struct global_geometry_element_struct *elements;
};

struct global_logger_element_struct {
char name[128];
int component_index;
struct logger_struct *logger;
};

struct pointer_to_global_logger_list {
int num_elements;
struct global_logger_element_struct *elements;
};


struct global_tagging_conditional_element_struct {
struct conditional_list_struct conditional_list;
int extend_index;
char name[1024];
int use_status;
};

struct global_tagging_conditional_list_struct {
int num_elements;
int current_index;
struct global_tagging_conditional_element_struct *elements;
};


struct global_master_element_struct {
char name[128];
int component_index;
int stored_number_of_scattering_events; // TEST
struct conditional_list_struct *tagging_conditional_list_pointer;
};

struct pointer_to_global_master_list {
int num_elements;
struct global_master_element_struct *elements;
};


// -------------    Physics functions   ---------------------------------------------------------

//#include "Test_physics.c"
//#include "Incoherent_test.c"

// -------------    General functions   ---------------------------------------------------------
double distance_between(Coords position1,Coords position2) {
    return sqrt((position1.x-position2.x)*(position1.x-position2.x) +
                (position1.y-position2.y)*(position1.y-position2.y) +
                (position1.z-position2.z)*(position1.z-position2.z));
};



double length_of_3vector(double *r) {
        return sqrt(r[0]*r[0]+r[1]*r[1]+r[2]*r[2]);
    };

double length_of_position_vector(Coords point) {
        return sqrt(point.x*point.x+point.y*point.y+point.z*point.z);
    };

Coords make_position(double *r) {
    Coords temp;
    
    temp.x = r[0];temp.y = r[1];temp.z = r[2];
    return temp;
};

Coords coords_scalar_mult(Coords input,double scalar) {
    return coords_set(scalar*input.x,scalar*input.y,scalar*input.z);
};

double union_coords_dot(Coords vector1,Coords vector2) {
    return vector1.x*vector2.x + vector1.y*vector2.y + vector1.z*vector2.z;
}


int sum_int_list(struct pointer_to_1d_int_list list) {
    int iterate,sum = 0;
    for (iterate = 0;iterate < list.num_elements;iterate++) sum += list.elements[iterate];
    return sum;
    };
    
int on_int_list(struct pointer_to_1d_int_list list,int target) {
    int iterate,output=0;
    for (iterate = 0;iterate < list.num_elements;iterate++) {
        if (list.elements[iterate] == target) output = 1;
    }
    return output;
    };

int find_on_int_list(struct pointer_to_1d_int_list list,int target) {
    int iterate;
    for (iterate = 0;iterate < list.num_elements;iterate++) {
        if (list.elements[iterate] == target) return iterate;
    }
    return -1;
    };
    
void on_both_int_lists(struct pointer_to_1d_int_list *list1, struct pointer_to_1d_int_list *list2, struct pointer_to_1d_int_list *common) {
    // Assume common.elements is allocated to a number of int's large enough to hold the resulting list
    int iterate,used_elements=0;
    for (iterate=0;iterate<list1->num_elements;iterate++) {
        if (on_int_list(*list2,list1->elements[iterate])) common->elements[used_elements++] = list1->elements[iterate];
    }
    common->num_elements = used_elements;
    };

void remove_element_in_list_by_index(struct pointer_to_1d_int_list *list,int index) {
    if (index >= list->num_elements) {
      printf("ERROR(remove_element_in_list_by_index): trying to remove an index that wasn't allocated to begin with");
      exit(1);
    }
    else {
        int iterate;
        int *temp;
        for (iterate = index;iterate < list->num_elements -1;iterate++) {
            list->elements[iterate] = list->elements[iterate+1];
        }
        list->num_elements--;
        //if (list->num_elements==0) printf("Making empty list!\n");
        temp = malloc(list->num_elements * sizeof(int));
        for (iterate = 0;iterate < list->num_elements;iterate++) temp[iterate] = list->elements[iterate];
        free(list->elements);
        list->elements = malloc(list->num_elements * sizeof(int));
        for (iterate = 0;iterate < list->num_elements;iterate++) list->elements[iterate] = temp[iterate];
        free(temp);

    }
    };
    
void remove_element_in_list_by_value(struct pointer_to_1d_int_list *list,int value) {
    int iterate;
    for (iterate = 0;iterate < list->num_elements;iterate++) {
        if (list->elements[iterate] == value) remove_element_in_list_by_index(list,iterate);
    }
    };
    
void merge_lists(struct pointer_to_1d_int_list *result,struct pointer_to_1d_int_list *list1,struct pointer_to_1d_int_list *list2) {
    if (result->num_elements > 0) free(result->elements);
    result->num_elements = list1->num_elements + list2->num_elements;
    if (result->num_elements != 0) {
      result->elements = malloc(result->num_elements*sizeof(int));
      int iterate;
      for (iterate = 0;iterate < list1->num_elements;iterate++)
          result->elements[iterate] = list1->elements[iterate];
      for (iterate = 0;iterate < list2->num_elements;iterate++)
          result->elements[list1->num_elements+iterate] = list2->elements[iterate];
    }
    };

void add_element_to_double_list(struct pointer_to_1d_double_list *list,double value) {
    if (list->num_elements == 0) {
      list->num_elements++;
      list-> elements = malloc(list->num_elements*sizeof(double));
      list-> elements[0] = value;
    } else {
      double temp[list->num_elements];
      int iterate;
      for (iterate=0;iterate<list->num_elements;iterate++) temp[iterate] = list->elements[iterate];
      free(list->elements);
      list->num_elements++;
      list-> elements = malloc(list->num_elements*sizeof(double));
      for (iterate=0;iterate<list->num_elements-1;iterate++) list->elements[iterate] = temp[iterate];
      list->elements[list->num_elements-1] = value;
    }
    };

void add_element_to_int_list(struct pointer_to_1d_int_list *list,int value) {
    if (list->num_elements == 0) {
      list->num_elements++;
      list-> elements = malloc(list->num_elements*sizeof(int));
      list-> elements[0] = value;
    } else {
      int temp[list->num_elements];
      int iterate;
      for (iterate=0;iterate<list->num_elements;iterate++) temp[iterate] = list->elements[iterate];
      free(list->elements);
      list->num_elements++;
      list-> elements = malloc(list->num_elements*sizeof(int));
      for (iterate=0;iterate<list->num_elements-1;iterate++) list->elements[iterate] = temp[iterate];
      list->elements[list->num_elements-1] = value;
    }
    };

// Need to check if absolute_rotation is preserved correctly.
void add_element_to_focus_data_array(struct focus_data_array_struct *focus_data_array,struct focus_data_struct focus_data) {
    if (focus_data_array->num_elements == 0) {
      focus_data_array->num_elements++;
      focus_data_array-> elements = malloc(focus_data_array->num_elements*sizeof(struct focus_data_struct));
      focus_data_array-> elements[0] = focus_data;
    } else {
      struct focus_data_struct temp[focus_data_array->num_elements];
      int iterate;
      for (iterate=0;iterate<focus_data_array->num_elements;iterate++) temp[iterate] = focus_data_array->elements[iterate];
      free(focus_data_array->elements);
      focus_data_array->num_elements++;
      focus_data_array-> elements = malloc(focus_data_array->num_elements*sizeof(struct focus_data_struct));
      for (iterate=0;iterate<focus_data_array->num_elements-1;iterate++) focus_data_array->elements[iterate] = temp[iterate];
      focus_data_array->elements[focus_data_array->num_elements-1] = focus_data;
    }
    };


void add_to_logger_with_data(struct logger_with_data_struct *logger_with_data, struct logger_struct *logger) {
    // May reorder the order of the if conditions to avoid checking the == 0 for every single ray
    if (logger_with_data->allocated_elements == 0) {
        logger_with_data->allocated_elements = 5;
        logger_with_data->logger_pointers = malloc(logger_with_data->allocated_elements*sizeof(struct logger_struct*));
        logger_with_data->used_elements = 1;
        logger_with_data->logger_pointers[0] = logger;
    } else if (logger_with_data->used_elements > logger_with_data->allocated_elements-1) {
        struct logger_with_data_struct temp_logger_with_data;
        temp_logger_with_data.logger_pointers = malloc((logger_with_data->used_elements)*sizeof(struct logger_struct*));
        int iterate;
        for (iterate=0;iterate<logger_with_data->used_elements;iterate++) {
            temp_logger_with_data.logger_pointers[iterate]  = logger_with_data->logger_pointers[iterate];
        }
        free(logger_with_data->logger_pointers);
        logger_with_data->allocated_elements = logger_with_data->allocated_elements+5;
        logger_with_data->logger_pointers = malloc(logger_with_data->allocated_elements*sizeof(struct logger_struct*));
        for (iterate=0;iterate<logger_with_data->used_elements;iterate++) {
            logger_with_data->logger_pointers[iterate]  = temp_logger_with_data.logger_pointers[iterate];
        }

        logger_with_data->logger_pointers[logger_with_data->used_elements++] = logger;
        
        
    } else {
        logger_with_data->logger_pointers[logger_with_data->used_elements++] = logger;
    }
    
};


// Used typedef to avoid having to change this function later. May update others to use same phillosphy.
void add_function_to_conditional_list(struct conditional_list_struct *list,conditional_function_pointer new, union conditional_data_union *data_union) {
    if (list->num_elements == 0) {
      list->num_elements++;
      list->conditional_functions = malloc(list->num_elements*sizeof(conditional_function_pointer));
      list->p_data_unions = malloc(list->num_elements*sizeof(union conditional_data_union*));
      list->conditional_functions[0] = new;
      list->p_data_unions[0] = data_union;
    }
    else {
    conditional_function_pointer temp_fp[list->num_elements];
    union conditional_data_union *temp_du[list->num_elements];
    int iterate;
    // Could even get away with a shallow copy here instead of the loop, but it is not relevant for performance.
    for (iterate=0;iterate<list->num_elements;iterate++) {
      temp_fp[iterate] = list->conditional_functions[iterate];
      temp_du[iterate] = list->p_data_unions[iterate];
    }
    free(list->conditional_functions);
    free(list->p_data_unions);
    list->num_elements++;
    list->conditional_functions = malloc(list->num_elements*sizeof(conditional_function_pointer));
    list->p_data_unions = malloc(list->num_elements*sizeof(union conditional_data_union*));
    
    for (iterate=0;iterate<list->num_elements-1;iterate++) {
      list->conditional_functions[iterate] = temp_fp[iterate];
      list->p_data_unions[iterate] = temp_du[iterate];
    }
    list->conditional_functions[list->num_elements-1] = new;
    list->p_data_unions[list->num_elements-1] = data_union;
    }
};


// could make function that removes a element from a 1d_int_list, and have each list generation as a function that takes a copy of an overlap list

void print_1d_int_list(struct pointer_to_1d_int_list list,char *name) {
        int iterate;
        printf("LIST: ");printf("%s",name);printf(" = [");
        for (iterate = 0; iterate < list.num_elements; iterate++) {
            printf("%d",list.elements[iterate]);
            if (iterate < list.num_elements - 1) printf(",");
        }
        printf("]\n");
    };

void print_1d_double_list(struct pointer_to_1d_double_list list,char *name) {
        int iterate;
        printf("LIST: ");printf("%s",name);printf(" = [");
        for (iterate = 0; iterate < list.num_elements; iterate++) {
            printf("%f",list.elements[iterate]);
            if (iterate < list.num_elements - 1) printf(",");
        }
        printf("]\n");
    };

void print_position(Coords pos,char *name) {
    printf("POSITION: ");printf("%s",name);printf(" = (%f,%f,%f)\n",pos.x,pos.y,pos.z);
    };

void print_rotation(Rotation rot, char *name) {
    printf("ROT MATRIX: %s \n",name);
    printf("[%f %f %f]\n",rot[0][0],rot[0][1],rot[0][2]);
    printf("[%f %f %f]\n",rot[1][0],rot[1][1],rot[1][2]);
    printf("[%f %f %f]\n\n",rot[2][0],rot[2][1],rot[2][2]);
};
    
void allocate_list_from_temp(int num_elements,struct pointer_to_1d_int_list original,struct pointer_to_1d_int_list *new) {
        int iterate;
    
        new->num_elements = num_elements;
        if (num_elements > 0) {
            new->elements = malloc(num_elements*sizeof(int));
            for (iterate = 0;iterate < num_elements; iterate++) new->elements[iterate] = original.elements[iterate];
        } else new->elements = NULL;
    
    };

void allocate_logic_list_from_temp(int num_elements,struct pointer_to_1d_int_list original, struct pointer_to_1d_int_list *new) {
        // A logic list shares the same structure of a normal list, but instead of listing numbers, it is a list of yes / no (1/0)
        // Giving this function a list of [1 3 5] (and num_elements = 9) would return [0 1 0 1 0 1 0 0 0];
        int iterate;
    
        new->num_elements = num_elements;
        if (num_elements > 0) {
            new->elements = malloc(num_elements*sizeof(int));
            for (iterate = 0;iterate < num_elements;iterate++) new->elements[iterate] = 0;
            for (iterate = 0;iterate < original.num_elements;iterate++) {
                if (original.elements[iterate] < num_elements)
                    new->elements[original.elements[iterate]] = 1;
                else printf("Trying to allocate logical list without enough memory\n");
            }
        } else new->elements = NULL;
    };

/*
struct global_positions_to_transform_list_struct {
int num_elements;
Coords **positions;
}

struct global_rotations_to_transform_list_struct {
int num_elements;
Rotation **rotations;
}
*/

void add_position_pointer_to_list(struct global_positions_to_transform_list_struct *list, Coords *new_position_pointer) {
    if (list->num_elements == 0) {
      list->num_elements++;
      list->positions = malloc(list->num_elements*sizeof(Coords*));
      list->positions[0] = new_position_pointer;
    } else {
      Coords **temp;
      temp = malloc(list->num_elements*sizeof(Coords*));
      int iterate;
      for (iterate=0;iterate<list->num_elements;iterate++)
        temp[iterate] = list->positions[iterate];
      free(list->positions);
      list->num_elements++;
      list->positions = malloc(list->num_elements*sizeof(Coords*));
      
      for (iterate=0;iterate<list->num_elements-1;iterate++)
        list->positions[iterate] = temp[iterate];
      free(temp);
      list->positions[list->num_elements-1] = new_position_pointer;
    }
};

void add_rotation_pointer_to_list(struct global_rotations_to_transform_list_struct *list, Rotation *new_rotation_pointer) {
    if (list->num_elements == 0) {
      list->num_elements++;
      list->rotations = malloc(list->num_elements*sizeof(Rotation*));
      list->rotations[0] = new_rotation_pointer;
    } else {
      Rotation **temp;
      temp = malloc(list->num_elements*sizeof(Rotation*));
      int iterate;
      for (iterate=0;iterate<list->num_elements;iterate++)
        temp[iterate] = list->rotations[iterate];
      free(list->rotations);
      list->num_elements++;
      list->rotations = malloc(list->num_elements*sizeof(Rotation*));
      
      for (iterate=0;iterate<list->num_elements-1;iterate++)
        list->rotations[iterate] = temp[iterate];
      free(temp);
      list->rotations[list->num_elements-1] = new_rotation_pointer;
    }
};

void add_element_to_process_list(struct pointer_to_global_process_list *list,struct global_process_element_struct new_element) {
    if (list->num_elements == 0) {
    list->num_elements++;
    list-> elements = malloc(list->num_elements*sizeof(struct global_process_element_struct));
    list-> elements[0] = new_element;
    }
    else {
    struct global_process_element_struct temp[list->num_elements];
    int iterate;
    for (iterate=0;iterate<list->num_elements;iterate++) temp[iterate] = list->elements[iterate];
    free(list->elements);
    list->num_elements++;
    list-> elements = malloc(list->num_elements*sizeof(struct global_process_element_struct));
    for (iterate=0;iterate<list->num_elements-1;iterate++) list->elements[iterate] = temp[iterate];
    list->elements[list->num_elements-1] = new_element;
    }
};

void add_element_to_material_list(struct pointer_to_global_material_list *list,struct global_material_element_struct new_element) {
    if (list->num_elements == 0) {
    list->num_elements++;
    list-> elements = malloc(list->num_elements*sizeof(struct global_material_element_struct));
    list-> elements[0] = new_element;
    }
    else {
    struct global_material_element_struct temp[list->num_elements];
    int iterate;
    for (iterate=0;iterate<list->num_elements;iterate++) temp[iterate] = list->elements[iterate];
    free(list->elements);
    list->num_elements++;
    list-> elements = malloc(list->num_elements*sizeof(struct global_material_element_struct));
    for (iterate=0;iterate<list->num_elements-1;iterate++) list->elements[iterate] = temp[iterate];
    list->elements[list->num_elements-1] = new_element;
    }
};

void add_element_to_geometry_list(struct pointer_to_global_geometry_list *list,struct global_geometry_element_struct new_element) {
    if (list->num_elements == 0) {
    list->num_elements++;
    list-> elements = malloc(list->num_elements*sizeof(struct global_geometry_element_struct));
    list-> elements[0] = new_element;
    }
    else {
    struct global_geometry_element_struct temp[list->num_elements];
    int iterate;
    for (iterate=0;iterate<list->num_elements;iterate++) temp[iterate] = list->elements[iterate];
    free(list->elements);
    list->num_elements++;
    list-> elements = malloc(list->num_elements*sizeof(struct global_geometry_element_struct));
    for (iterate=0;iterate<list->num_elements-1;iterate++) list->elements[iterate] = temp[iterate];
    list->elements[list->num_elements-1] = new_element;
    }
};

void add_element_to_logger_list(struct pointer_to_global_logger_list *list,struct global_logger_element_struct new_element) {
    if (list->num_elements == 0) {
    list->num_elements++;
    list-> elements = malloc(list->num_elements*sizeof(struct global_logger_element_struct));
    list-> elements[0] = new_element;
    }
    else {
    struct global_logger_element_struct temp[list->num_elements];
    int iterate;
    for (iterate=0;iterate<list->num_elements;iterate++) temp[iterate] = list->elements[iterate];
    free(list->elements);
    list->num_elements++;
    list-> elements = malloc(list->num_elements*sizeof(struct global_logger_element_struct));
    for (iterate=0;iterate<list->num_elements-1;iterate++) list->elements[iterate] = temp[iterate];
    list->elements[list->num_elements-1] = new_element;
    }
};

void add_element_to_tagging_conditional_list(struct global_tagging_conditional_list_struct *list,struct global_tagging_conditional_element_struct new_element) {
    if (list->num_elements == 0) {
    list->num_elements++;
    list->elements = malloc(list->num_elements*sizeof(struct global_tagging_conditional_element_struct));
    list->elements[0] = new_element;
    }
    else {
    struct global_tagging_conditional_element_struct temp[list->num_elements];
    int iterate;
    for (iterate=0;iterate<list->num_elements;iterate++) temp[iterate] = list->elements[iterate];
    free(list->elements);
    list->num_elements++;
    list->elements = malloc(list->num_elements*sizeof(struct global_tagging_conditional_element_struct));
    for (iterate=0;iterate<list->num_elements-1;iterate++) list->elements[iterate] = temp[iterate];
    list->elements[list->num_elements-1] = new_element;
    }
};

void add_element_to_master_list(struct pointer_to_global_master_list *list,struct global_master_element_struct new_element) {
    if (list->num_elements == 0) {
    list->num_elements++;
    list->elements = malloc(list->num_elements*sizeof(struct global_master_element_struct));
    list->elements[0] = new_element;
    }
    else {
    struct global_master_element_struct temp[list->num_elements];
    int iterate;
    for (iterate=0;iterate<list->num_elements;iterate++) temp[iterate] = list->elements[iterate];
    free(list->elements);
    list->num_elements++;
    list-> elements = malloc(list->num_elements*sizeof(struct global_master_element_struct));
    for (iterate=0;iterate<list->num_elements-1;iterate++) list->elements[iterate] = temp[iterate];
    list->elements[list->num_elements-1] = new_element;
    }
};

void add_initialized_logger_in_volume(struct loggers_struct *loggers,int number_of_processes) {
  int iterate;
  if (loggers->num_elements == 0) {
    loggers->num_elements++;
    loggers->p_logger_volume = malloc(loggers->num_elements * sizeof(struct logger_for_each_process_list));
    loggers->p_logger_volume[0].num_elements = number_of_processes;
    loggers->p_logger_volume[0].p_logger_process = malloc(number_of_processes * sizeof(struct logger_struct**));
    for (iterate=0;iterate<number_of_processes;iterate++)
      loggers->p_logger_volume[0].p_logger_process[iterate] = NULL;
  } else {
    // Already some elements, store them in temp, free main, transfer back and add newest.
    struct logger_for_each_process_list temp[loggers->num_elements];
    
    for (iterate=0;iterate<loggers->num_elements;iterate++) temp[iterate] = loggers->p_logger_volume[iterate];
    free(loggers->p_logger_volume);
    loggers->num_elements++;
    loggers->p_logger_volume = malloc(loggers->num_elements*sizeof(struct logger_for_each_process_list));
    for (iterate=0;iterate<loggers->num_elements-1;iterate++) loggers->p_logger_volume[iterate] = temp[iterate];
    loggers->p_logger_volume[loggers->num_elements-1].num_elements = number_of_processes;
    loggers->p_logger_volume[loggers->num_elements-1].p_logger_process = malloc(number_of_processes * sizeof(struct logger_struct**));
    for (iterate=0;iterate<number_of_processes;iterate++) {
      loggers->p_logger_volume[loggers->num_elements-1].p_logger_process[iterate] = NULL;
    }
  }
};



// -------------    Functions used to shorten master trace    ---------------------------------------------


void update_current_mask_intersect_status(struct pointer_to_1d_int_list *current_mask_intersect_list_status, struct pointer_to_1d_int_list *mask_status_list, struct Volume_struct **Volumes, int *current_volume) {
  // This function is to be executed whenever the current volume changes, or the mask status changes
  // It updates the effective mask status for each element of a volumes mask_intersect_list
  // The effective mask takes the ALL/ANY mode assosiated with each volume into account,
  //  meaning a volume needs to be within ALL/ANY of it's masks to have a status of 1
  // In most cases this mask_intersect_list will be empty, and memory operations are avoided
  //printf("Number of elements to be check: %d \n",Volumes[*current_volume]->geometry.mask_intersect_list.num_elements);
  
  if (Volumes[*current_volume]->geometry.mask_intersect_list.num_elements > 0) {
    int iterate,this_element,*mask_start,*mask_check;
    for (iterate=0;iterate<Volumes[*current_volume]->geometry.mask_intersect_list.num_elements;iterate++) {
      this_element = Volumes[*current_volume]->geometry.mask_intersect_list.elements[iterate];
      //printf("We are investigating volume number %d from the mask_intersect_list of volume %d",this_element,*current_volume);
      if (Volumes[this_element]->geometry.mask_mode == 2) { // ANY mask mode
        //printf("We are in ANY mask mode! \n");
        current_mask_intersect_list_status->elements[iterate] = 0; // Assume the mask status is 0, but if any are 1, take that instead
        for (mask_start=mask_check=Volumes[this_element]->geometry.masked_by_mask_index_list.elements;mask_check-mask_start<Volumes[this_element]->geometry.masked_by_mask_index_list.num_elements;mask_check++) {
          //printf("Checking all the mask statuses of volumes masking %d, now mask with global mask index %d ! \n",this_element,*mask_check);
          if (mask_status_list->elements[*mask_check] == 1) {
            //printf("The status was 1, so the effective status is set to 1 and the loop is stopped");
            current_mask_intersect_list_status->elements[iterate] = 1;
            break;
          }
        }
      } else { // ALL mask mode
        //printf("We are in ALL mask mode! \n");
        current_mask_intersect_list_status->elements[iterate] = 1; // Assume the mask status is 1, but if any one is 0, take that instead
        for (mask_start=mask_check=Volumes[this_element]->geometry.masked_by_mask_index_list.elements;mask_check-mask_start<Volumes[this_element]->geometry.masked_by_mask_index_list.num_elements;mask_check++) {
          //printf("Checking all the mask statuses of volumes masking %d, now mask with global mask index %d ! \n",this_element,*mask_check);
          if (mask_status_list->elements[*mask_check] == 0) {
            //printf("The status was 0, so the effective status is set to 0 and the loop is stopped \n");
            current_mask_intersect_list_status->elements[iterate] = 0;
            break;
          }
        }
      }
    }
  }
    
  /* Original version compatible with trace scope
  for (iterate=0;iterate<Volumes[current_volume]->geometry.mask_intersect_list.num_elements;iterate++) {
    this_element = Volumes[current_volume]->geometry.mask_intersect_list.elements[iterate];
    if (Volumes[this_element]->geometry.mask_mode == 2) { // ANY mask mode
      current_mask_intersect_list_status.elements[iterate] = 0; // Assume the mask status is 0, but if any are 1, take that instead
      for (mask_start=mask_check=Volumes[this_element]->geometry.masked_by_mask_index_list.elements;mask_check-mask_start<Volumes[this_element]->geometry.masked_by_mask_index_list.num_elements;mask_check++) {
        if (mask_status_list[*mask_check] == 1) {
          current_mask_intersect_list_status.elements[iterate] = 1;
          break;
        }
      }
    } else { // ALL mask mode
      current_mask_intersect_list_status.elements[iterate] = 1; // Assume the mask status is 1, but if any one is 0, take that instead
      for (mask_start=mask_check=Volumes[this_element]->geometry.masked_by_mask_index_list.elements;mask_check-mask_start<Volumes[this_element]->geometry.masked_by_mask_index_list.num_elements;mask_check++) {
        if (mask_status_list[*mask_check] == 0) {
          current_mask_intersect_list_status.elements[iterate] = 0;
          break;
        }
      }
    }
  }
  */
}

// -------------    Tagging functions    ------------------------------------------------------------------

struct tagging_tree_node_struct {
  // Statistics:
  double intensity;
  int number_of_rays;
  // tree pointers
  struct tagging_tree_node_struct *above;   // Pointer to node above
  struct tagging_tree_node_struct **volume_branches;
  struct tagging_tree_node_struct **process_branches;
};

struct list_of_tagging_tree_node_pointers {
  struct tagging_tree_node_struct **elements;
  int num_elements;
};

struct tagging_tree_node_struct *make_tagging_tree_node(void) {
    return (struct tagging_tree_node_struct *) malloc(sizeof(struct tagging_tree_node_struct));
}


struct tagging_tree_node_struct  *simple_initialize_tagging_tree_node(struct tagging_tree_node_struct *new_node) {
    //struct tagging_tree_node_struct *dummy;
    //dummy = malloc(sizeof(struct tagging_tree_node_struct));
    //new_node = dummy;
    
    //new_node->element = (struct tagging_tree_node_struct *) malloc(sizeof(struct tagging_tree_node_struct));
    //new_node = (struct tagging_tree_node_struct *) malloc(sizeof(struct tagging_tree_node_struct));
    new_node = make_tagging_tree_node();
    
    if (new_node == NULL) printf("ERROR, Union tagging system could not allocate memory\n");
    new_node->intensity = 4.2; // (double) 4.2;
    new_node->number_of_rays = 42; //(int) 42;
    
    printf("new_node->intensity = %f, new_node->number_of_rays = %d \n",new_node->intensity,new_node->number_of_rays);
    return new_node;
};


struct tagging_tree_node_struct *initialize_tagging_tree_node(struct tagging_tree_node_struct *new_node, struct tagging_tree_node_struct *above_node, struct Volume_struct *this_volume) {
    new_node = make_tagging_tree_node();
    
    new_node->intensity = (double) 0;
    new_node->number_of_rays = (int) 0;
    new_node->above = above_node;
    
    int next_volume_list_length = this_volume->geometry.next_volume_list.num_elements;
    new_node->volume_branches = malloc(next_volume_list_length*sizeof(struct tagging_tree_node_struct*));
    int iterate;
    // Initializing pointers so that they can be checked for NULL later. Is this redundant? Does malloc return null pointers?
    for (iterate=0;iterate<next_volume_list_length;iterate++) new_node->volume_branches[iterate] = NULL;
    //new_node->volume_branches.num_elements = dest_list_length; // May be removed
    
    int number_of_processes;
    if (this_volume->p_physics == NULL) number_of_processes = 0;
    else number_of_processes = this_volume->p_physics->number_of_processes;
    
    new_node->process_branches = malloc(number_of_processes*sizeof(struct tagging_tree_node_struct*));
    // Initializing pointers so that they can be checked for NULL later. Is this redundant? Does malloc return null pointers?
    for (iterate=0;iterate<number_of_processes;iterate++) new_node->process_branches[iterate] = NULL;
    //new_node->process_branches.num_elements=number_of_processes; // May be removed
    
    return new_node;
};

struct tagging_tree_node_struct *goto_process_node(struct tagging_tree_node_struct *current_node, int process_index, struct Volume_struct *this_volume, int *stop_tagging_ray, int stop_creating_nodes) {
    // Either create a new node if it has not been created yet, or travel down the tree
    if (current_node->process_branches[process_index] == NULL) {
      if (stop_creating_nodes == 0) {
        current_node->process_branches[process_index] = initialize_tagging_tree_node(current_node->process_branches[process_index],current_node,this_volume);
        return current_node->process_branches[process_index];
      } else {
        // This stops the ray from using more goto node functions and being counted in the statistics.
        // Happens because the history limit is reached, and no new histories should be started.
        *stop_tagging_ray = 1;
        return current_node;
      }
    } else {
        current_node = current_node->process_branches[process_index];
        return current_node;
    }
    
    //return current_node;
};

struct tagging_tree_node_struct *goto_volume_node(struct tagging_tree_node_struct *current_node,int current_volume, int next_volume, struct Volume_struct **Volumes, int *stop_tagging_ray, int stop_creating_nodes) {
    // I have only allocated the number of node branches that corresponds to the current_volumes next_volume_list
    // The problem is to find where on the destination list the current volume is without doing a manual search
    
    // With the new mask system there is a risk of going from and to the same volume, which should be ignored by the tagging system
    if (current_volume == next_volume) return current_node;
    
    struct tagging_tree_node_struct *output;
    int next_volume_list_index = -1;
    // Temporary slow method for finding the correct index on the destination list
    int iterate;
    
    for (iterate=0;iterate<Volumes[current_volume]->geometry.next_volume_list.num_elements;iterate++)
        if (Volumes[current_volume]->geometry.next_volume_list.elements[iterate] == next_volume) {
            next_volume_list_index = iterate;
            break;
        }
    
    // Debug phase
    //printf("Tagging: going from volume %d to volume %d, which is index number %d on it's next volume list \n",current_volume,next_volume,next_volume_list_index);
    if (next_volume_list_index == -1) {
        printf("ERROR in Union component, tagging or destination system failed, next volume was not on next volume list\n");
        printf("current_volume = %d, next_volume = %d \n",current_volume,next_volume);
        print_1d_int_list(Volumes[current_volume]->geometry.destinations_list,"destinations_list for current volume");
        exit(1);
    }
    
    // Either create a new node if it has not been created yet, or travel down the tree
    if (current_node->volume_branches[next_volume_list_index] == NULL) {
      if (stop_creating_nodes == 0) {
        current_node->volume_branches[next_volume_list_index] =  initialize_tagging_tree_node(current_node->volume_branches[next_volume_list_index],current_node,Volumes[next_volume]);
        return current_node->volume_branches[next_volume_list_index];
      } else {
        // This stops the ray from using more goto node functions and being counted in the statistics.
        // Happens because the history limit is reached, and no new histories should be started.
        *stop_tagging_ray = 1;
        return current_node;
      }
    } else {
        //current_node = current_node->volume_branches[next_volume_list_index];
        //return current_node;
        current_node = current_node->volume_branches[next_volume_list_index];
        //printf("used allocated node \n");
        return current_node;
    }
};

void add_statistics_to_node(struct tagging_tree_node_struct *current_node, Coords *r, Coords *v, double *weight, int *counter) {
    if (current_node->number_of_rays == 0) (*counter)++;
    current_node->number_of_rays = current_node->number_of_rays + 1;
    current_node->intensity = current_node->intensity + *weight;
};

struct history_node_struct {
    int volume_index;
    int process_index;
};

struct dynamic_history_list {
  struct history_node_struct *elements;
  int used_elements;
  int allocated_elements;
};

struct saved_history_struct {
    struct history_node_struct *elements;
    int used_elements;
    double intensity;
    int number_of_rays;
};

struct total_history_struct {
    struct saved_history_struct *saved_histories;
    int used_elements;
    int allocated_elements;
};


void add_to_history(struct dynamic_history_list *history, int volume_index, int process_index) {
    //printf("Adding to history[%d]: volume_index = %d, process_index = %d \n",history->used_elements,volume_index,process_index);
    if (history->allocated_elements == 0) {
        history->elements = malloc(5*sizeof(struct history_node_struct));
        history->used_elements = 1;
        history->allocated_elements = 5;
        history->elements[0].volume_index = volume_index;
        history->elements[0].process_index = process_index;
    } else if (history->used_elements > history->allocated_elements-1) {
        struct dynamic_history_list temp_history;
        temp_history.elements = malloc((history->used_elements)*sizeof(struct history_node_struct));
        int iterate;
        for (iterate=0;iterate<history->used_elements;iterate++) {
            temp_history.elements[iterate].volume_index  = history->elements[iterate].volume_index;
            temp_history.elements[iterate].process_index = history->elements[iterate].process_index;
        }
        free(history->elements);
        history->elements = malloc((history->allocated_elements+5)*sizeof(struct history_node_struct));
        for (iterate=0;iterate<history->used_elements;iterate++) {
            history->elements[iterate].volume_index  = temp_history.elements[iterate].volume_index;
            history->elements[iterate].process_index = temp_history.elements[iterate].process_index;
        }
        
        history->allocated_elements = history->allocated_elements+5;
        
        history->elements[history->used_elements].volume_index = volume_index;
        history->elements[history->used_elements].process_index = process_index;
        history->used_elements = history->used_elements+1;
        
    } else {
        history->elements[history->used_elements].volume_index = volume_index;
        history->elements[history->used_elements].process_index = process_index;
        history->used_elements++;
    }
    
};

void printf_history(struct dynamic_history_list *history) {
    int history_iterate;
          //printf("History number %d, intensity = %f, number of rays = %d:",hist_num, search_node->intensity, search_node->number_of_rays);
          for (history_iterate=0;history_iterate<history->used_elements-1;history_iterate++) {
            if (history->elements[history_iterate].process_index == -1) {
                printf(" V%d ->",history->elements[history_iterate].volume_index);
            } else {
                printf(" P%d ->",history->elements[history_iterate].process_index);
            }
          }
          if (history->elements[history_iterate].process_index == -1) {
                printf(" V%d \n",history->elements[history_iterate].volume_index);
          } else {
                printf(" P%d \n",history->elements[history_iterate].process_index);
          }
}

void fprintf_total_history(struct saved_history_struct *history, FILE *fp) {
    fprintf(fp,"%d\t N I=%E \t", history->number_of_rays, history->intensity);
        int history_iterate;
          //printf("History number %d, intensity = %f, number of rays = %d:",hist_num, search_node->intensity, search_node->number_of_rays);
          for (history_iterate=0;history_iterate<history->used_elements-1;history_iterate++) {
            if (history->elements[history_iterate].process_index == -1) {
                fprintf(fp," V%d ->",history->elements[history_iterate].volume_index);
            } else {
                fprintf(fp," P%d ->",history->elements[history_iterate].process_index);
            }
          }
          if (history->elements[history_iterate].process_index == -1) {
                fprintf(fp," V%d \n",history->elements[history_iterate].volume_index);
          } else {
                fprintf(fp," P%d \n",history->elements[history_iterate].process_index);
          }
}

/*
int Sample_compare_doubles (const void *a, const void *b) {
  const double *da = (const double *) a;
  const double *db = (const double *) b;

  return (*da > *db) - (*da < *db);
}
*/


int Sample_compare_history_intensities (const struct saved_history_struct *a, const struct saved_history_struct *b) {
  const double *da = (const double *) &(a->intensity);
  const double *db = (const double *) &(b->intensity);

  return (*da < *db) - (*da > *db);
}

void write_tagging_tree(struct list_of_tagging_tree_node_pointers *master_list, struct Volume_struct **Volumes, int total_history_counter, int number_of_volumes) {
  // Start from top of tree, go to extremeties, take results and add to disk / database, free that node
  int volume_index,done,volume_iterate,process_iterate,current_volume,next_node_found,current_number_of_processes,history_iterate,hist_num;
  
  struct tagging_tree_node_struct *search_node;
  struct tagging_tree_node_struct **kill_candidate;
  struct dynamic_history_list history_data; // Allocate the history list struct
  struct dynamic_history_list *history;     // Use this pointer in the algorithm
  
  struct total_history_struct total_history;
  total_history.saved_histories = malloc(total_history_counter * sizeof(struct saved_history_struct));
  total_history.allocated_elements = total_history_counter;
  total_history.used_elements = 0;
  
  history = &history_data;
  
  history->used_elements = 0;
  history->allocated_elements = 0;
  
  hist_num = 0;
  for (volume_index=0;volume_index<master_list->num_elements;volume_index++) {
    
    search_node = master_list->elements[volume_index];
    
    if (volume_index != 0)
        current_number_of_processes = Volumes[volume_index]->p_physics->number_of_processes;
    else
        current_number_of_processes = 0;
    
    current_volume = volume_index;
    done = 0;
    
    history->used_elements = 0;
    
    add_to_history(history,current_volume,-1);
    while(done == 0) {
        next_node_found=0;
        for (volume_iterate=0;volume_iterate<Volumes[current_volume]->geometry.next_volume_list.num_elements;volume_iterate++) {
            //printf("searc_node->volume_branches[0]->intensity = %f\n",search_node->volume_branches[0]->intensity);
            if (search_node->volume_branches[volume_iterate] != NULL) {
                current_volume = Volumes[current_volume]->geometry.next_volume_list.elements[volume_iterate];
                if (current_volume != 0)
                    current_number_of_processes = Volumes[current_volume]->p_physics->number_of_processes;
                else
                    current_number_of_processes = 0;
                
                //search_node = &(search_node->volume_branches[volume_iterate]);
                kill_candidate = &(search_node->volume_branches[volume_iterate]);
                search_node = search_node->volume_branches[volume_iterate];
                next_node_found = 1;
                add_to_history(history,current_volume,-1);
                //printf_history(history);
                break;
            }
        }
        if (next_node_found == 0) {
          //printf("doing process loop with %d steps \n",current_number_of_processes);
          for (process_iterate=0;process_iterate<current_number_of_processes;process_iterate++) {
            //if (&(search_node->process_branches[volume_iterate]) != NULL) {
            if (search_node->process_branches[process_iterate] != NULL) {
                //printf("was not NULL (process)\n");
                //search_node = &(search_node->process_branches[process_iterate]);
                kill_candidate = &(search_node->process_branches[process_iterate]);
                search_node = search_node->process_branches[process_iterate];
                next_node_found = 1;
                add_to_history(history,current_volume,process_iterate);
                //printf_history(history);
                break;
            }
          }
        }
        
        if (next_node_found == 0) {
          // write this history to disk / memory
          hist_num++;
          //printf("Reached next_node_found == 0 \n");
          if (history->used_elements > 0 && search_node->number_of_rays > 0) {
            //printf("%d rays (I=%E) with history: \t", search_node->number_of_rays, search_node->intensity);
            //printf_history(history);
            
            total_history.saved_histories[total_history.used_elements].used_elements = history->used_elements;
            total_history.saved_histories[total_history.used_elements].elements = malloc(total_history.saved_histories[total_history.used_elements].used_elements*sizeof(struct history_node_struct));
            for (history_iterate = 0;history_iterate<history->used_elements;history_iterate++) {
                total_history.saved_histories[total_history.used_elements].elements[history_iterate] = history->elements[history_iterate];
                //printf("total_history.saved_histories[total_history.used_elements].elements[%d].volume_index \n",history_iterate,total_history.saved_histories[total_history.used_elements].elements[history_iterate].volume_index);
            }
            //total_history.saved_histories[total_history.used_elements].elements = history->elements;
            total_history.saved_histories[total_history.used_elements].intensity = search_node->intensity;
            total_history.saved_histories[total_history.used_elements].number_of_rays = search_node->number_of_rays;
            total_history.used_elements++;
          }
          
          history->used_elements = 0;
          
          // end of tree, no new nodes
          if (search_node->above == NULL) {
            done = 1;
          } else {
            // reset to the root of the tree
            *kill_candidate = NULL;
            free(search_node);
            search_node = master_list->elements[volume_index];
            
            if (volume_index != 0)
              current_number_of_processes = Volumes[volume_index]->p_physics->number_of_processes;
            else
              current_number_of_processes = 0;
    
            current_volume = volume_index;
            add_to_history(history,current_volume,-1);
          }
          
        }
    }
  }
  
  if (history->allocated_elements > 0) free(history->elements);

  qsort(total_history.saved_histories,total_history.used_elements,sizeof (struct saved_history_struct), Sample_compare_history_intensities);
  
  
  
  MPI_MASTER(
  printf("\n\n");
  printf("Top 20 most common histories. Shows the index of volumes entered (VX), and the scattering processes (PX)\n");
  for (history_iterate=0;history_iterate<total_history.used_elements && history_iterate<20;history_iterate++) {
    printf("%d\t N I=%E \t", total_history.saved_histories[history_iterate].number_of_rays, total_history.saved_histories[history_iterate].intensity);
    printf_history(&total_history.saved_histories[history_iterate]);
  }
   
  FILE *fp;
  fp = fopen("union_history.dat","w");
  
  fprintf(fp,"History file written by the McXtrace component Union_master \n");
  fprintf(fp,"When running with MPI, the results may be from just a single thread, meaning intensities are divided by number of threads\n");
  fprintf(fp,"----- Description of the used volumes -----------------------------------------------------------------------------------\n");
  
  fprintf(fp,"V0: Surrounding vacuum \n");
  for (volume_iterate=1;volume_iterate<number_of_volumes;volume_iterate++) {
    fprintf(fp,"V%d: %s  ",volume_iterate,Volumes[volume_iterate]->name);
    fprintf(fp,"Material: %s  ",Volumes[volume_iterate]->p_physics->name);
    for (process_iterate=0;process_iterate<Volumes[volume_iterate]->p_physics->number_of_processes;process_iterate++) {
      fprintf(fp," P%d: %s",process_iterate,Volumes[volume_iterate]->p_physics->p_scattering_array[process_iterate].name);
    }
    fprintf(fp,"\n");
  }
  fprintf(fp,"----- Histories sorted after intensity ----------------------------------------------------------------------------------\n");
  
  for (history_iterate=0;history_iterate<total_history.used_elements;history_iterate++) {
    fprintf_total_history(&total_history.saved_histories[history_iterate],fp);
    // Garbage collection
    if (total_history.saved_histories[history_iterate].used_elements > 0) free(total_history.saved_histories[history_iterate].elements);
  }
  fclose(fp);
  
  )
  
  // Garbage collection
  if (total_history.allocated_elements > 0) free(total_history.saved_histories);
  
  
};


// -------------    Intersection table functions   --------------------------------------------------------
int clear_intersection_table(struct intersection_time_table_struct *intersection_time_table) {
    // Resets the intersection table when a scattering have occured.
    int iterate_volumes,iterate_solutions;
    
    // Start at one because vacuum (0) does not have a listing in the intersection table
    for (iterate_volumes = 1;iterate_volumes < intersection_time_table->num_volumes;iterate_volumes++) {
        intersection_time_table->calculated[iterate_volumes] = 0;
        // This second loop is added for safty in debugging phase, but can be removed as the information should never be accesed when calculated = 0
        for (iterate_solutions = 0;iterate_solutions < intersection_time_table->n_elements[iterate_volumes];iterate_solutions++) {
            intersection_time_table->intersection_times[iterate_volumes][iterate_solutions] = -1;
        }
    }
    
    return 1;
};

void print_intersection_table(struct intersection_time_table_struct intersection_time_table) {
    int num_volumes,iterate,solutions;
    int max_number_of_solutions = 0;
    
    num_volumes = intersection_time_table.num_volumes;
    
    for (iterate = 0;iterate < num_volumes;iterate++) {
        if (max_number_of_solutions < intersection_time_table.n_elements[iterate])
            max_number_of_solutions = intersection_time_table.n_elements[iterate];
    }
    
    printf("------------------ INTERSECTION_TIME_TABLE -----------------");
    for (solutions = 2;solutions < max_number_of_solutions;solutions++) printf("------------");
    printf("\n");
    
    // printf("iterate      |");

    printf("           ");
    printf("| CALCULATED  |");
    for (solutions = 0;solutions < max_number_of_solutions;solutions++) {
        printf(" - SOLUTION %d - |",solutions);
    }

    printf("\n");
    for (iterate = 0;iterate < num_volumes;iterate++){
    // print iterate number
    printf("Volume %d   |",iterate);
    printf(" ---- %d ---- |",intersection_time_table.calculated[iterate]);
    
        for (solutions = 0;solutions < max_number_of_solutions;solutions++) {
          if (intersection_time_table.n_elements[iterate] > solutions && intersection_time_table.calculated[iterate] == 1)
            if (intersection_time_table.intersection_times[iterate][solutions] > 0)
             printf("   %1.8f   |",intersection_time_table.intersection_times[iterate][solutions]);
            else
             printf("   %1.7f   |",intersection_time_table.intersection_times[iterate][solutions]);
          else
            printf("                |");
        }
    printf("\n");
    }
    printf("------------------------------------------------------------");
    for (solutions = 2;solutions < max_number_of_solutions;solutions++) printf("------------");
    printf("\n");
    
    
    };
    
// -------------    Drawing functions   --------------------------------------------------------
void merge_lines_to_draw(struct lines_to_draw *lines_master,struct lines_to_draw *lines_new) {
    if (lines_master->number_of_lines == 0) {
    lines_master->number_of_lines = lines_new->number_of_lines;
    lines_master->lines = malloc(lines_master->number_of_lines*sizeof(struct line_segment));
    lines_master->lines = lines_new->lines;
    // One could free lines_new->lines;
    } else {
    int iterate;
    struct line_segment *temp_lines;
    temp_lines = malloc(lines_master->number_of_lines*sizeof(struct line_segment));
    for (iterate = 0;iterate < lines_master->number_of_lines;iterate++) temp_lines[iterate] = lines_master->lines[iterate];
    free(lines_master->lines);
    lines_master->lines = malloc((lines_master->number_of_lines+lines_new->number_of_lines)*sizeof(struct line_segment));
    for (iterate = 0;iterate < lines_master->number_of_lines;iterate++) lines_master->lines[iterate] = temp_lines[iterate];
    for (iterate = 0;iterate < lines_new->number_of_lines;iterate++) lines_master->lines[iterate+lines_master->number_of_lines] = lines_new->lines[iterate];
    lines_master->number_of_lines = lines_master->number_of_lines + lines_new->number_of_lines;
    free(temp_lines);
    }
    };

int r_has_highest_priority(Coords point,int N,struct geometry_struct **Geometries,int number_of_volumes) {
    // Function that test if a point in Volume N has the highest priority.
    // Returns 0 if another volume has higher priority at the point, and it's mask status is 1
    // Returns 1 if no other volume has higher priority at the point
    // (not active) Returns a larger integer if the volume N is a mask, and the point is not in the volume it is masking,
    //  which results in the drawing function making that number of dashes
    
    
    int mask_status,*mask_start,*mask_check;
    // If the volume is a mask, it does not have a priority, and is always on top
    if (Geometries[N]->is_mask_volume == 1) {
      return 1;
      // Can draw parts of the mask that is outside the volume it masks as dashed lines, but it just looks messy
      /*
      mask_status = 1;
      for (mask_check=mask_start=Geometries[N]->mask_list.elements;mask_check-mask_start<Geometries[N]->mask_list.num_elements;mask_check++) {
        if (Geometries[*mask_check]->within_function(point,Geometries[*mask_check]) == 1) {
          return 1; // If the point is within a volume the mask is masking, draw it as a solid line
        }
      }
      // As it was not inside any of the volumes the mask is masking, draw it as a dashed line
      return 5;
      */
    }
    
    // If the volume is a masked volume, check if the point is within it's masks
    if (Geometries[N]->is_masked_volume == 1) {
      if (Geometries[N]->mask_mode == 1) { // ALL mode, need to be within ALL masks
        for (mask_check=mask_start=Geometries[N]->masked_by_list.elements;mask_check-mask_start<Geometries[N]->masked_by_list.num_elements;mask_check++) {
          if (Geometries[*mask_check]->within_function(point,Geometries[*mask_check]) == 0) {
            return 0; // If the point is just outside one mask, the mask status is 0 and the point does not have highest priority
          }
        }
      } else { // ANY mode, need to be within at least one mask
        mask_status = 0;
        for (mask_check=mask_start=Geometries[N]->masked_by_list.elements;mask_check-mask_start<Geometries[N]->masked_by_list.num_elements;mask_check++) {
          if (Geometries[*mask_check]->within_function(point,Geometries[*mask_check]) == 1) {
            mask_status = 1;
            break;
          }
        }
        if (mask_status == 0) {
          return 0; // If it was not in a single of it's masks, the point did not have highest priority
        }
      }
    }
    
    int volume_index;
    double self_priority;
    self_priority = Geometries[N]->priority_value;
    
    
    for (volume_index = 1;volume_index<number_of_volumes;volume_index++) {
      if (volume_index != N && Geometries[volume_index]->is_mask_volume == 0) {
        if (Geometries[volume_index]->within_function(point,Geometries[volume_index])) {
          if (Geometries[volume_index]->is_masked_volume == 1) {
            // Since this volume is masked, the mask status need to be checked
            if (Geometries[volume_index]->mask_mode == 1) { //ALL mode, need to be within ALL masks
              mask_status = 1;
              for (mask_check=mask_start=Geometries[volume_index]->masked_by_list.elements;mask_check-mask_start<Geometries[volume_index]->masked_by_list.num_elements;mask_check++) {
                if (Geometries[*mask_check]->within_function(point,Geometries[*mask_check]) == 0) {
                  mask_status = 0;
                  break;
                }
              }
            } else { // ANY mode, need to be within at least one mask
              mask_status = 0;
              for (mask_check=mask_start=Geometries[volume_index]->masked_by_list.elements;mask_check-mask_start<Geometries[volume_index]->masked_by_list.num_elements;mask_check++) {
                if (Geometries[*mask_check]->within_function(point,Geometries[*mask_check]) == 1) {
                  mask_status = 1;
                  break;
                }
              }
            }
          } else mask_status = 1;
          if (Geometries[volume_index]->priority_value > self_priority && mask_status == 1) {
            // printf("Volume %d did not have highest priority at (%f,%f,%f) (Volume number %d was above)\n",N,point.x,point.y,point.z,volume_index);
            return 0;
          }
        }
      }
    }
    // printf("Volume %d did have highest priority at (%f,%f,%f) \n",N,point.x,point.y,point.z);
    return 1;
    }
    
void draw_line_positions(Coords point1,Coords point2) {
        //line(point1.x,point1.y,point1.z,point2.x,point2.y,point2.z);
        // sigh, can not use line in share. Need to save the information and pass to the mcdisplay part.
    }

int Sample_compare_doubles (const void *a, const void *b) {
  const double *da = (const double *) a;
  const double *db = (const double *) b;

  return (*da > *db) - (*da < *db);
}

struct lines_to_draw draw_line_with_highest_priority(Coords position1,Coords position2,int N,struct geometry_struct **Geometries,int number_of_volumes,int max_number_of_solutions) {
    
    int volume_index,iterate,permanent_list_length = 0;
    int number_of_solutions;
    double temp_intersection[max_number_of_solutions];
    double r1[3],r2[3],direction[3];
    struct pointer_to_1d_double_list intersection_list;
    
    intersection_list.num_elements = 0;
    
    // double *permanent_intersection_list,*storage; // old ways
    
    r1[0] = position1.x;
    r1[1] = position1.y;
    r1[2] = position1.z;
    r2[0] = position2.x;
    r2[1] = position2.y;
    r2[2] = position2.z;
    
    // printf("r1 = (%f,%f,%f) \n",r1[0],r1[1],r1[2]);
    // printf("r2 = (%f,%f,%f) \n",r2[0],r2[1],r2[2]);
    
    direction[0] = r2[0] - r1[0];
    direction[1] = r2[1] - r1[1];
    direction[2] = r2[2] - r1[2];
    int geometry_output;
    
    // Find intersections
    for (volume_index = 1;volume_index < number_of_volumes; volume_index++) {
        if (volume_index != N) {
            geometry_output = Geometries[volume_index]->intersect_function(temp_intersection,&number_of_solutions,r1,direction,Geometries[volume_index]);
             // printf("No solutions for intersection (Volume %d) with %d \n",N,volume_index);
                for (iterate=0;iterate<number_of_solutions;iterate++) {
                    // print_1d_double_list(intersection_list,"intersection_list");
                    if (temp_intersection[iterate] > 0 && temp_intersection[iterate] < 1) {
                        add_element_to_double_list(&intersection_list,temp_intersection[iterate]);
                        // printf("solution taken = %f\n",temp_intersection[iterate]);
                        }                         // printf("solution ignored = %f\n",temp_intersection[iterate]);
                    // print_1d_double_list(intersection_list,"intersection_list");
                
                }
                // printf("First (%d) solutions added to the solution stack, intersection with %d \n",number_of_solutions,volume_index);
                // printf(" Solutions: ");
                // for (iterate = 0;iterate < intersection_list.num_elements;iterate++) printf("%f ",intersection_list.elements[iterate]);
                // printf("\n");
            
        }
    }
    // Now we have a list of intersection distances between r1 and r2 and all volumes.
    // This list needs to be sorted before we continue!
    
    qsort(intersection_list.elements,intersection_list.num_elements,sizeof (double), Sample_compare_doubles);
    // print_1d_double_list(intersection_list,"after sorting");
    // printf(" Solutions (after sorting): ");
    // for (iterate = 0;iterate < intersection_list.num_elements;iterate++) printf("%f ",intersection_list.elements[iterate]);
    // printf("\n");
    
    Coords points[intersection_list.num_elements+2];
    points[0] = coords_set(r1[0],r1[1],r1[2]);
    points[intersection_list.num_elements+1] = coords_set(r2[0],r2[1],r2[2]);
    for (iterate = 1;iterate < intersection_list.num_elements+1;iterate++) {
        points[iterate].x = r1[0] + direction[0]*intersection_list.elements[iterate-1];
        points[iterate].y = r1[1] + direction[1]*intersection_list.elements[iterate-1];
        points[iterate].z = r1[2] + direction[2]*intersection_list.elements[iterate-1];
    }
    
    // printf("Points on the list:\n");
    // for (iterate = 0;iterate < intersection_list.num_elements + 2;iterate++) {
    // //         printf("(%f,%f,%f)\n",points[iterate].x,points[iterate].y,points[iterate].z);
    // }
    // printf("\n");
  
    struct line_segment lines[intersection_list.num_elements+1];
    int draw_logic[intersection_list.num_elements+1];
    //struct lines_to_draw draw_order;
    Coords midpoint;
    struct lines_to_draw draw_order;
    draw_order.number_of_lines = 0;
    
    // printf("test2 in function \n");
    int number_of_dashes;
    
    for (iterate = 0;iterate < intersection_list.num_elements + 1;iterate++) {
        lines[iterate].point1 = points[iterate];
        lines[iterate].point2 = points[iterate+1];
        midpoint.x = 0.5*(lines[iterate].point1.x + lines[iterate].point2.x);
        midpoint.y = 0.5*(lines[iterate].point1.y + lines[iterate].point2.y);
        midpoint.z = 0.5*(lines[iterate].point1.z + lines[iterate].point2.z);
        
        if ((number_of_dashes = r_has_highest_priority(midpoint,N,Geometries,number_of_volumes)) != 0) {
            draw_order.number_of_lines++;
            draw_logic[iterate] = number_of_dashes;
        } else draw_logic[iterate] = 0;
    }
   
    // printf("test3 in function \n");
    
    if (draw_order.number_of_lines > 0) {
        draw_order.lines = malloc(draw_order.number_of_lines*sizeof(struct line_segment));
        draw_order.number_of_lines = 0;
        for (iterate = 0;iterate < intersection_list.num_elements + 1;iterate++) {
            if (draw_logic[iterate] != 0) {
              lines[iterate].number_of_dashes = draw_logic[iterate];
              draw_order.lines[draw_order.number_of_lines++] = lines[iterate];
            }
        }
        if (intersection_list.num_elements > 0) free(intersection_list.elements);
    }

    // printf("function done \n");
    return draw_order;
    }

struct lines_to_draw draw_circle_with_highest_priority(Coords center,Coords vector,double radius,int N,struct geometry_struct **Geometries,int number_of_volumes,int max_number_of_solutions) {
    int number_of_positions = 100;

    // normalize vector
    double vector_length = length_of_position_vector(vector);
    // print_position(vector,"start vector");
    vector.x /= vector_length;
    vector.y /= vector_length;
    vector.z /= vector_length;
    // print_position(vector,"start vector normalized");
    
    // Create a vector from the center of the circle to a point on the circumference by cross product
    double cross_input[3] = {0,1,0};
    // In case the cross input is parallel with the vector, a new is chosen. Both can't be parallel.
    if (scalar_prod(cross_input[0],cross_input[1],cross_input[2],vector.x,vector.y,vector.z) > 0.99) {
        cross_input[0] = 1; cross_input[1] = 0; cross_input[2] = 0;
    }
    // print_position(make_position(cross_input),"cross input");
    
    double cross_product1[3] = {0,0,0};
    vec_prod(cross_product1[0],cross_product1[1],cross_product1[2],vector.x,vector.y,vector.z,cross_input[0],cross_input[1],cross_input[2]);
    
    // print_position(make_position(cross_product1),"cross_product1");
    
    double cross_length = length_of_3vector(cross_product1);
    
    // printf("cross_length = %f \n",cross_length);
    
    cross_product1[0] /= cross_length;
    cross_product1[1] /= cross_length;
    cross_product1[2] /= cross_length;
    
    cross_product1[0] *= radius;
    cross_product1[1] *= radius;
    cross_product1[2] *= radius;
    
    // printf("cross_product1 = (%f,%f,%f) \n",cross_product1[0],cross_product1[1],cross_product1[2]);
    
    int iterate;
    double rotate_angle;
    Coords radial_position,old_radial_position;
    struct lines_to_draw temp_draw_order,return_draw_order;
    return_draw_order.number_of_lines = 0;
    // Generate an array of positions by rotating this vector around the given vector, this corresponds to points on the circle
    old_radial_position.x = center.x + cross_product1[0];
    old_radial_position.y = center.y + cross_product1[1];
    old_radial_position.z = center.z + cross_product1[2];

    // printf("old_radial_position = (%f,%f,%f) \n",old_radial_position.x,old_radial_position.y,old_radial_position.z);
    
    for (iterate = 0;iterate < number_of_positions-1;iterate++) {
        rotate_angle = 2*3.14159*((double) iterate + 1.0)/((double) number_of_positions);
        rotate(radial_position.x,radial_position.y,radial_position.z,cross_product1[0],cross_product1[1],cross_product1[2],rotate_angle,vector.x,vector.y,vector.z);
        radial_position.x += center.x;
        radial_position.y += center.y;
        radial_position.z += center.z;
        
        // Use the draw_lines_with_highest_priority to draw get draw_orders for each line piece
        temp_draw_order = draw_line_with_highest_priority(radial_position,old_radial_position,N,Geometries,number_of_volumes,max_number_of_solutions);
       // Assemble these draw_orders to one large draw order
        merge_lines_to_draw(&return_draw_order,&temp_draw_order);

        old_radial_position.x = radial_position.x;
        old_radial_position.y = radial_position.y;
        old_radial_position.z = radial_position.z;
    }
    
    radial_position.x = center.x + cross_product1[0];
    radial_position.y = center.y + cross_product1[1];
    radial_position.z = center.z + cross_product1[2];
    // Use the draw_lines_with_highest_priority to draw get draw_orders for each line piece
    temp_draw_order = draw_line_with_highest_priority(radial_position,old_radial_position,N,Geometries,number_of_volumes,max_number_of_solutions);
    // Assemble these draw_orders to one large draw order
    merge_lines_to_draw(&return_draw_order,&temp_draw_order);
    
    // clean up
    if (temp_draw_order.number_of_lines > 0) free(temp_draw_order.lines);
    
    // return the large draw order.
    return return_draw_order;
    }

// -------------    Geometry functions   --------------------------------------------------------

#include "Geometry_functions.c"

// -------------    List generator functions   --------------------------------------------------


int within_which_volume(Coords pos, struct pointer_to_1d_int_list input_list, struct pointer_to_1d_int_list destinations_list, struct Volume_struct **Volumes, struct pointer_to_1d_int_list *mask_status_list, int number_of_volumes, int *volume_logic_copy, int *ListA, int *ListB) {
    // This function identifies in which of the volumes of the input list the position pos lies in.
    // pos: position for which the current volume should be found for
    // input list: list of potential volumes, reduced to the ones without parents (their children will be checked)
    // OLD VERSION: volume logic: A logic list of allowed volumes for lookup, volume 1 3 and 5 in a case of 10 volumes would be [0 1 0 1 0 1 0 0 0 0]
    // destinations_list: list of allowed destinations (the original destinations list)
    // Volumes: Main volumes array
    // volume_logic_copy: A pointer to a integer array with at least length "number_of_volumes" (pre alocated for speed)
    // ListA: A pointer to a integer array with at least length "number_of_volumes" (pre alocated for speed)
    // ListB: A pointer to a integer array with at least length "number_of_volumes" (pre alocated for speed)
    
    // Algorithm description
    // Check all of input list for pos being within them, those that have it within them are:
    //      checked against the current highest priority
    //          if higher, is the new highest priority and the new pick for volume
    //      have all their direct children added to the next list to be checked (but any volume can only be added to that list once)
    // Once a run have been made where the next list to check is empty, the answer for new pick for volume is taken.
    
    // Mask update:
    // Algorithm description
    // Check all of input list for pos being within them, those that have it within them are:
    //      checked against the current highest priority and mask status
    //          if higher, this is the new highest priority and the new pick for volume
    //      have all their direct children added to the next list to be checked (but any volume can only be added to that list once)
    // Once a run have been made where the next list to check is empty, the answer for new pick for volume is taken.
    
    // The advantage of the method is that potentially large numbers of children are skipped when their parents do not contain the position.
    // The overhead cost is low, as all the lists are prealocated.
    // Should be checked which of the two implementations is faster, as this is much more complicated than simply checking all possibilities.
    // No within_function call should be made twice, as the same volume number will not be checked twice because of the properties of the direct_children list and the volume_logic that removes duplicates on each level.
    
    
    // This function uses too much memory, the memory required for the volume logic list is n_volumes^2 ints, or for a MACS monochromator 127000 ints.
    // Instead the original destinations list must be used, and a function for quick lookup in a (sorted) destinations list made.
    // Still need a list of n_volumes length for control to avoid adding the same volume to the list twice.
    
    
    int ListA_length=0,ListB_length=0;
    int done = 0;
    int i,direct_children_index;
    int *temp_pointer;
    double max_priority=-1000000;
    int residing_volume=0; // 0 can be removed from the input list if default is 0
    int this_mask_status,mask_index,mask_global_index;
    
    // volume_logic_copy
    //for (i=0;i<volume_logic.num_elements;i++) volume_logic_copy[i] = volume_logic.elements[i];
    
    // low memory version of volume_logic_copy
    for (i=0;i<number_of_volumes;i++) volume_logic_copy[i] = 0;
    for (i=0;i<destinations_list.num_elements;i++) volume_logic_copy[destinations_list.elements[i]] = 1;
    //printf("within_which_volume debug\n");

    // Does one loop through the algorithm first to set up ListA instead of copying it from input_list, which takes time
    for (i=0;i<input_list.num_elements;i++) {
            if (Volumes[input_list.elements[i]]->geometry.within_function(pos,&Volumes[input_list.elements[i]]->geometry) == 1) {
                //printf("The position is inside of volume %d\n",input_list.elements[i]);
                if (Volumes[input_list.elements[i]]->geometry.is_masked_volume == 1) {
                    // if the volume is masked, I need to know if it can be a destination volume from the mask_status_list.
                    // if the masked volume is in ANY mode,
                    this_mask_status=1;
                    //print_1d_int_list(*mask_status_list,"mask status list from within_which_volume");
                    for (mask_index=0;mask_index<Volumes[input_list.elements[i]]->geometry.masked_by_mask_index_list.num_elements;mask_index++) {
                        //printf("Looking at the mask with global index %d \n",Volumes[input_list.elements[i]]->geometry.masked_by_mask_index_list.elements[mask_index]);
                        if (mask_status_list->elements[Volumes[input_list.elements[i]]->geometry.masked_by_mask_index_list.elements[mask_index]] == 1) {
                            if (Volumes[input_list.elements[i]]->geometry.mask_mode == 2) { // ANY (break if any one in)
                                this_mask_status=1;
                                break;
                            }
                            //printf("global index %d had mask status = 1 \n",Volumes[input_list.elements[i]]->geometry.masked_by_mask_index_list.elements[mask_index]);
                        } else {
                          //printf("global index %d had mask status = 0 \n",Volumes[input_list.elements[i]]->geometry.masked_by_mask_index_list.elements[mask_index]);
                          this_mask_status = 0;
                          if(Volumes[input_list.elements[i]]->geometry.mask_mode == 1) break; // ALL (break if any one out)
                        }
                    }
                    //printf("This volume is masked, and the mask status is %d\n",this_mask_status);
                } else this_mask_status = 1; // if the volume is not masked
            
                if (Volumes[input_list.elements[i]]->geometry.priority_value > max_priority && this_mask_status == 1) {
                    max_priority = Volumes[input_list.elements[i]]->geometry.priority_value;
                    residing_volume = input_list.elements[i];
                    //printf("residing volume set to %d\n",residing_volume);
                }
                    for (direct_children_index = 0;direct_children_index < Volumes[input_list.elements[i]]->geometry.direct_children.num_elements;direct_children_index++) {
                        if (volume_logic_copy[Volumes[input_list.elements[i]]->geometry.direct_children.elements[direct_children_index]] == 1) {
                            ListA[ListA_length++] = Volumes[input_list.elements[i]]->geometry.direct_children.elements[direct_children_index];
                            volume_logic_copy[Volumes[input_list.elements[i]]->geometry.direct_children.elements[direct_children_index]] = 0;
                        }
                    }
            }
    }
    //printf("Completed first loop, continued in while loop\n");
    if (ListA_length > 0) {
        while (done == 0) {
            for (i=0;i<ListA_length;i++) {
              //printf("checking element number %d of list A which is volume number %d \n",i,ListA[i]);
                if (Volumes[ListA[i]]->geometry.within_function(pos,&Volumes[ListA[i]]->geometry) == 1) {
                  //printf("ray was inside this volume \n");
                    if (Volumes[ListA[i]]->geometry.is_masked_volume == 1) {
                      //printf("it is a mask and thus need check of mask status \n");
                        // if the volume is masked, I need to know if it can be a destination volume from the mask_status_list.
                        // if the masked volume is in ANY mode,
                        this_mask_status=1;
                        for (mask_index=0;mask_index<Volumes[ListA[i]]->geometry.masked_by_mask_index_list.num_elements;mask_index++) {
                            if (mask_status_list->elements[Volumes[ListA[i]]->geometry.masked_by_mask_index_list.elements[mask_index]] == 1) {
                                if (Volumes[ListA[i]]->geometry.mask_mode == 2) { // ANY (break if any one in)
                                    this_mask_status=1;
                                    break;
                                }
                            } else {
                              this_mask_status = 0;
                              if(Volumes[ListA[i]]->geometry.mask_mode == 1) break; // ALL (break if any one out)
                            }
                        }
                    } else this_mask_status = 1;
                    //printf("the mask status is %d \n",this_mask_status);
                    if (Volumes[ListA[i]]->geometry.priority_value > max_priority && this_mask_status == 1) {
                        max_priority = Volumes[ListA[i]]->geometry.priority_value;
                        residing_volume = ListA[i];
                    }
                    //printf("Adding direct children to list B \n");
                        for (direct_children_index = 0;direct_children_index < Volumes[ListA[i]]->geometry.direct_children.num_elements;direct_children_index++) {
                            //printf("Checking direct_child number %d which is %d \n",direct_children_index,Volumes[ListA[i]]->geometry.direct_children.elements[direct_children_index]);
                            if (volume_logic_copy[Volumes[ListA[i]]->geometry.direct_children.elements[direct_children_index]] == 1) {
                                //printf("It's volume_logic was 1, and it is thus added to listB with index %d \n",ListB_length);
                                ListB[ListB_length++] = Volumes[ListA[i]]->geometry.direct_children.elements[direct_children_index];
                                volume_logic_copy[Volumes[ListA[i]]->geometry.direct_children.elements[direct_children_index]] = 0;
                            }
                        }
                    //printf("List B is now: ");
                    //for (direct_children_index=0;direct_children_index<ListB_length;direct_children_index++) printf("%d ",ListB[direct_children_index]);
                    //printf("\n");
                    
                    
                }
            }
            if (ListB_length==0) done = 1;
            else {
                
                for (i=0;i<ListB_length;i++) ListA[i] = ListB[i];
                ListA_length = ListB_length;
                ListB_length = 0;
                
                /*
                // Could do this with pointers instead to avoid this for loop (and needless copy)
                // This code block fails on the cluster in rare circumstances
                ListA = temp_pointer;
                ListA = ListB;
                ListB = temp_pointer;
                ListA_length=ListB_length;
                ListB_length=0;
                */
            }
        }
    }
    //printf("residing volume returned %d\n",residing_volume);
    return residing_volume;
};



int within_which_volume_debug(Coords pos, struct pointer_to_1d_int_list input_list, struct pointer_to_1d_int_list volume_logic, struct Volume_struct **Volumes, int *volume_logic_copy, int *ListA, int *ListB) {
    // This function identifies in which of the volumes of the input list the position pos lies in.
    // pos: position for which the current volume should be found for
    // input list: list of potential volumes, reduced to the ones without parents (their children will be checked)
    // volume logic: A logic list of allowed volumes for lookup, volume 1 3 and 5 in a case of 10 volumes would be [0 1 0 1 0 1 0 0 0 0]
    // Volumes: Main volumes array
    // volume_logic_copy: A pointer to a integer array with at least length "number_of_volumes" (pre alocated for speed)
    // ListA: A pointer to a integer array with at least length "number_of_volumes" (pre alocated for speed)
    // ListB: A pointer to a integer array with at least length "number_of_volumes" (pre alocated for speed)
    
    // Algorithm description
    // Check all of input list for pos being within them, those that have it within them are:
    //      checked against the current highest priority
    //          if higher, is the new highest priority and the new pick for volume
    //      have all their direct children added to the next list to be checked (but any volume can only be added to that list once)
    // Once a run have been made where the next list to check is empty, the answer for new pick for volume is taken.
    
    // The advantage of the method is that potentially large numbers of children are skipped when their parents do not contain the position.
    // The overhead cost is low, as all the lists are prealocated.
    // Should be checked which of the two implementations is faster, as this is much more complicated than simply checking all possibilities.
    // No within_function call should be made twice, as the same volume number will not be checked twice because of the properties of the direct_children list and the volume_logic that removes duplicates on each level.
    
    
    int ListA_length=0,ListB_length=0;
    int done = 0;
    int i,direct_children_index;
    int *temp_pointer;
    double max_priority=-1000000;
    int residing_volume=0; // 0 can be removed from the input list if default is 0
    
    // volume_logic_copy
    for (i=0;i<volume_logic.num_elements;i++) volume_logic_copy[i] = volume_logic.elements[i];
    
    // Does one loop through the algorithm first to set up ListA instead of copying it from input_list, which takes time
    for (i=0;i<input_list.num_elements;i++) {
            if (Volumes[input_list.elements[i]]->geometry.within_function(pos,&Volumes[input_list.elements[i]]->geometry) == 1) {
                if (Volumes[input_list.elements[i]]->geometry.priority_value > max_priority) {
                    max_priority = Volumes[input_list.elements[i]]->geometry.priority_value;
                    residing_volume = input_list.elements[i];
                }
                    for (direct_children_index = 0;direct_children_index < Volumes[input_list.elements[i]]->geometry.direct_children.num_elements;direct_children_index++) {
                        if (volume_logic_copy[Volumes[input_list.elements[i]]->geometry.direct_children.elements[direct_children_index]] == 1) {
                            ListA[ListA_length++] = Volumes[input_list.elements[i]]->geometry.direct_children.elements[direct_children_index];
                            volume_logic_copy[Volumes[input_list.elements[i]]->geometry.direct_children.elements[direct_children_index]] = 0;
                        }
                    }
            }
    }
    if (ListA_length > 0) {
        while (done == 0) {
        
            printf("ListA = [");
            for (i=0;i<ListA_length;i++) {
                printf("%d,",ListA[i]);
            }
            printf("]\n");
            for (i=0;i<ListA_length;i++) {
                if (Volumes[ListA[i]]->geometry.within_function(pos,&Volumes[ListA[i]]->geometry) == 1) {
                    if (Volumes[ListA[i]]->geometry.priority_value > max_priority) {
                        max_priority = Volumes[ListA[i]]->geometry.priority_value;
                        residing_volume = ListA[i];
                    }
                    //if (ListA[i]!=0) {
                        for (direct_children_index = 0;direct_children_index < Volumes[ListA[i]]->geometry.direct_children.num_elements;direct_children_index++) {
                            if (volume_logic_copy[Volumes[ListA[i]]->geometry.direct_children.elements[direct_children_index]] == 1) {
                                ListB[ListB_length++] = Volumes[ListA[i]]->geometry.direct_children.elements[direct_children_index];
                                volume_logic_copy[Volumes[ListA[i]]->geometry.direct_children.elements[direct_children_index]] = 0;
                            }
                        }
                    //}
                }
            }
            if (ListB_length==0) done = 1;
            else {
                ListA = temp_pointer;
                ListA = ListB;
                ListB = temp_pointer;
                ListA_length=ListB_length;
                ListB_length=0;
            }
        }
    }
    printf("Volume number %d had the highest priority of checked volumes\n",residing_volume);
    return residing_volume;
};

int inside_function(struct Volume_struct *parent_volume, struct Volume_struct *child_volume) {
    // Function that calls the correct within function depending on the shapes of the two volumes
    if (strcmp("sphere",parent_volume->geometry.shape) == 0 && strcmp("sphere",child_volume->geometry.shape) == 0) {
        if (sphere_within_sphere(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("cylinder",parent_volume->geometry.shape) == 0 && strcmp("cylinder",child_volume->geometry.shape) == 0) {
        if (cylinder_within_cylinder(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("box",parent_volume->geometry.shape) == 0 && strcmp("box",child_volume->geometry.shape) == 0) {
        if (box_within_box(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("cone",parent_volume->geometry.shape) == 0 && strcmp("cone",child_volume->geometry.shape) == 0) {
        if (cone_within_cone(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("mesh",parent_volume->geometry.shape) == 0 && strcmp("mesh",child_volume->geometry.shape) == 0) {
        if (mesh_within_mesh(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("box",parent_volume->geometry.shape) == 0 && strcmp("cylinder",child_volume->geometry.shape) == 0) {
        if (cylinder_within_box(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("cylinder",parent_volume->geometry.shape) == 0 && strcmp("box",child_volume->geometry.shape) == 0) {
        if (box_within_cylinder(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("box",parent_volume->geometry.shape) == 0 && strcmp("sphere",child_volume->geometry.shape) == 0) {
        if (sphere_within_box(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("sphere",parent_volume->geometry.shape) == 0 && strcmp("box",child_volume->geometry.shape) == 0) {
        if (box_within_sphere(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("sphere",parent_volume->geometry.shape) == 0 && strcmp("cylinder",child_volume->geometry.shape) == 0) {
        if (cylinder_within_sphere(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("cylinder",parent_volume->geometry.shape) == 0 && strcmp("sphere",child_volume->geometry.shape) == 0) {
        if (sphere_within_cylinder(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("cone",parent_volume->geometry.shape) == 0 && strcmp("sphere",child_volume->geometry.shape) == 0) {
        if (sphere_within_cone(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("sphere",parent_volume->geometry.shape) == 0 && strcmp("cone",child_volume->geometry.shape) == 0) {
        if (cone_within_sphere(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("cone",parent_volume->geometry.shape) == 0 && strcmp("cylinder",child_volume->geometry.shape) == 0) {
        if (cylinder_within_cone(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("cylinder",parent_volume->geometry.shape) == 0 && strcmp("cone",child_volume->geometry.shape) == 0) {
        if (cone_within_cylinder(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("cone",parent_volume->geometry.shape) == 0 && strcmp("box",child_volume->geometry.shape) == 0) {
        if (box_within_cone(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else if (strcmp("box",parent_volume->geometry.shape) == 0 && strcmp("cone",child_volume->geometry.shape) == 0) {
        if (cone_within_box(&child_volume->geometry,&parent_volume->geometry)) return 1;
    }
    else {
        printf("Need within function for type: ");
        printf("%s",parent_volume->geometry.shape);
        printf(" and type: ");
        printf("%s",child_volume->geometry.shape);
        printf(".\n");
        printf("It is not yet supported to mix mesh geometries with the basic shapes, but several mesh geometries are allowed.\n");
        exit(1);
    }
    
    return 0;
};

void generate_children_lists(struct Volume_struct **Volumes, struct pointer_to_1d_int_list **true_children_lists, int number_of_volumes, int verbal) {
  // This function generates a list of children for each volume.
  // A volume m is a child of volume n, if the entire space ocupied by volume m is inside of the space ocupied by volume n
  // A volume m is a true child of volume n, if the entire space coupied by volume m after it's masks are applied is inside the volume ocupied by volume n after it's masks are applied
  
  
  MPI_MASTER(
  if (verbal) printf("\nGenerating children lists --------------------------- \n");
  )

  // Mask update: Creating a temporary list for each volume
  struct pointer_to_1d_int_list *temporary_children_lists;
  temporary_children_lists = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));

  // The surrounding vacuum, volume 0, done outside of for loop.
  temporary_children_lists[0].num_elements = number_of_volumes - 1;
  temporary_children_lists[0].elements = malloc(temporary_children_lists[0].num_elements*sizeof(int));
  
  int parent;
  for (parent=1;parent<number_of_volumes;parent++) {
      temporary_children_lists[0].elements[parent-1] = parent;
  }
  
  //if (verbal) printf("did temporary children list \n");
  //print_1d_int_list(temporary_children_lists[0],"temp children list [0]");
  
  // Hardcoding that every volume is a child of the surrounding vacuum
  Volumes[0]->geometry.children.num_elements = number_of_volumes-1;
  Volumes[0]->geometry.children.elements = malloc((number_of_volumes-1)*sizeof(int));
  true_children_lists[0] = malloc(sizeof(struct pointer_to_1d_int_list));
  true_children_lists[0]->num_elements = number_of_volumes - 1;
  true_children_lists[0]->elements = malloc((number_of_volumes-1)*sizeof(int));
  
  //if (verbal) printf("allocated true children lists \n");
  
  for (parent=1;parent<number_of_volumes;parent++) {
      Volumes[0]->geometry.children.elements[parent-1] = parent;
      true_children_lists[0]->elements[parent-1] = parent;
  }
  
  char string_output[128];
  MPI_MASTER(
  if (verbal) sprintf(string_output,"Children for Volume %d",0);
  if (verbal) print_1d_int_list(Volumes[0]->geometry.children,string_output);
  )
  
  
  // Generating the children lists for all other volumes using the appropriate geometry functions
  struct pointer_to_1d_int_list temp_list_local;
  temp_list_local.num_elements = number_of_volumes;
  temp_list_local.elements = malloc(number_of_volumes*sizeof(int));
  
  struct pointer_to_1d_int_list true_temp_list_local;
  true_temp_list_local.num_elements = number_of_volumes;
  true_temp_list_local.elements = malloc(number_of_volumes*sizeof(int));
  
  
  int child,used_elements,used_elements_true;
  for (parent=1;parent<number_of_volumes;parent++) {
      used_elements = 0;used_elements_true = 0;
      for (child=1;child<number_of_volumes;child++) {
        if (child != parent) {
          // Call inside_function that selects the proper within function for the two volumes
          if (1 == inside_function(Volumes[parent],Volumes[child])) {
            temp_list_local.elements[used_elements++] = child;
            true_temp_list_local.elements[used_elements_true++] = child;
          }
        } else true_temp_list_local.elements[used_elements_true++] = child; // Needed when children list takes mask into account
      }
      // Temp test
      allocate_list_from_temp(used_elements,temp_list_local,&Volumes[parent]->geometry.children);
      // Assing the children list to a temporary list as the masks have yet to be taken into account
      temporary_children_lists[parent].num_elements=0;
      allocate_list_from_temp(used_elements_true,true_temp_list_local,&temporary_children_lists[parent]);
      
      
      MPI_MASTER(
      if (verbal) sprintf(string_output,"Children for Volume %d (temporary_list)",parent);
      if (verbal) print_1d_int_list(temporary_children_lists[parent],string_output);
      )
      
      MPI_MASTER(
      if (verbal) sprintf(string_output,"Children for Volume %d (permanent_list)",parent);
      if (verbal) print_1d_int_list(Volumes[parent]->geometry.children,string_output);
      )
      
  }
  
  // mask update:
  // The logical expression: (child c parent AND child c parent_mask) OR (child_mask c parent AND child_mask c parent_mask)
  //  needs to be evaluated for each child / parent combination in order to take the masks of each into account
  
  int logic_var1,logic_var2,logic_var_ANY,logic_var_ALL;
  int mask_index,mask_index_child,mask_index_parent;
  int volume_C,volume_P;
  // Loop that takes masks into account
  for (parent=1;parent<number_of_volumes;parent++) {
    used_elements = 0;
    for (child=1;child<number_of_volumes;child++) {
     if (child != parent && 0 == on_int_list(Volumes[parent]->geometry.masked_by_list,child)) {
        // The children list for each volume does not need to contain the volume itself
        //  And a parent masked by it's child can not have that mask as a child
      
      // Here c means within in the sense of a set being part of another set
      // Logical expression to be evaluated: (child c parent AND child c parent_mask) OR (child_mask c parent AND child_mask c parent_mask)
      logic_var1 = on_int_list(temporary_children_lists[parent],child);
      if (logic_var1 == 1 && Volumes[parent]->geometry.is_masked_volume == 1) {
        // if the parent volume is masked, the child also need to be inclosed in these masks to fulfill this side of the logical expression
        logic_var_ANY = 0;
        for (mask_index=0;mask_index<Volumes[parent]->geometry.masked_by_list.num_elements;mask_index++) {
          if (0 == on_int_list(temporary_children_lists[Volumes[parent]->geometry.masked_by_list.elements[mask_index]],child)) {
            if (Volumes[parent]->geometry.mask_mode == 1) {
              logic_var1 = 0;
              break;
            }
          } else logic_var_ANY = 1;
        }
        if (Volumes[parent]->geometry.mask_mode == 2) logic_var1 = logic_var_ANY;
      }
      
      if (logic_var1 == 1) true_temp_list_local.elements[used_elements++] = child;
      else if (Volumes[child]->geometry.is_masked_volume == 1) {
        // If the first side of the logical expression is false, evalute the other side
        // The other side is only relevant if the child volume is masked, otherwise it is ignored
        //printf("Second side of logical expression \n");
        
        logic_var1 = 1; // Assume true
          
        // child_mask c parent
        logic_var_ALL = 0;
        for (mask_index=0;mask_index<Volumes[child]->geometry.masked_by_list.num_elements;mask_index++) {
          if (0 == on_int_list(temporary_children_lists[parent],Volumes[child]->geometry.masked_by_list.elements[mask_index])){
            if (Volumes[child]->geometry.mask_mode == 2) {
              logic_var1 = 0;
              break;
            }
          } else logic_var_ALL = 1;
        }
        if (Volumes[child]->geometry.mask_mode == 1) logic_var1 = logic_var_ALL;
        
        // This line allows the second part of the logical expression to be true in cases where the parent volume is not masked
        if (logic_var1 == 1 && Volumes[parent]->geometry.is_masked_volume == 0) true_temp_list_local.elements[used_elements++] = child;
        
        // There is no reason to check the other part (child_mask c parent_mask) if the first part was not true
        if (logic_var1 == 1 && Volumes[parent]->geometry.is_masked_volume == 1) {
          // This last part requires both the child and the parent to be masked
          // Need to evaluate (child_mask c parent_mask), where both can be a be a list of volume with ALL/ANY modes
          
          if (Volumes[parent]->geometry.mask_mode == 1) {
            logic_var2 = 1; // Assume the logical expression (child_mask c parent_mask) is true
            for (mask_index_parent=0;mask_index_parent<Volumes[parent]->geometry.masked_by_list.num_elements;mask_index_parent++) {
              // As the parent is in ALL mode, the child masks must be within ALL parent masks
              logic_var_ANY = 0; // Assume not a single child is within this parent
              for (mask_index_child=0;mask_index_child<Volumes[child]->geometry.masked_by_list.num_elements;mask_index_child++) {
                volume_P = Volumes[parent]->geometry.masked_by_list.elements[mask_index_parent];
                volume_C = Volumes[child]->geometry.masked_by_list.elements[mask_index_child];
                // Is volume C inside volume P? If yes, volume C must be on volume P's temporary children list
                if (0 == on_int_list(temporary_children_lists[volume_P],volume_C)) {
                  if (Volumes[child]->geometry.mask_mode == 2) {
                    // If child is in ANY mode, any one mask outside is enough to make the expression false
                    logic_var2 = 0;
                    break;
                  }
                } else logic_var_ANY = 1;
              }
              // If child is in ALL mode, then if any one child were within this mask, the logic expression holds true
              if (Volumes[child]->geometry.mask_mode == 1) logic_var2 = logic_var_ANY;
              if (logic_var2 == 0) break; // No need to continue
            }
          } else if (Volumes[parent]->geometry.mask_mode == 2) {
            // If the parent is in ANY mode, it is enough if the child masks are within just 1 of the parent masks
            for (mask_index_parent=0;mask_index_parent<Volumes[parent]->geometry.masked_by_list.num_elements;mask_index_parent++) {
              logic_var2 = 1; // Assume the logical expression (child_mask c parent_mask) is true
              logic_var_ANY = 0; // Assume not a single child is within this parent
              for (mask_index_child=0;mask_index_child<Volumes[child]->geometry.masked_by_list.num_elements;mask_index_child++) {
                volume_P = Volumes[parent]->geometry.masked_by_list.elements[mask_index_parent];
                volume_C = Volumes[child]->geometry.masked_by_list.elements[mask_index_child];
                // Is volume C inside volume P? If yes, volume C must be on volume P's temporary children list
                if (0 == on_int_list(temporary_children_lists[volume_P],volume_C)) {
                  if (Volumes[child]->geometry.mask_mode == 2) {
                    // If child is in ANY mode, any one mask outside is enough to make the expression false
                    logic_var2 = 0;
                    break;
                  }
                } else logic_var_ANY = 1;
              }
              // If child is in ALL mode, then if any one child were within this mask, the logic expression holds true
              if (Volumes[child]->geometry.mask_mode == 1) logic_var2 = logic_var_ANY;
              if (logic_var2 == 1) break; // No need to continue
            }
          }
          
          // if this point is reached, and logic_var2 is true, volume[child] is a child of volume[parent]
          if (logic_var2 == 1) true_temp_list_local.elements[used_elements++] = child;
          
        }
      }
     }
    }
    
    true_children_lists[parent] = malloc(sizeof(struct pointer_to_1d_int_list));
    true_children_lists[parent]->num_elements = 0;
    allocate_list_from_temp(used_elements,true_temp_list_local,true_children_lists[parent]);
    
    MPI_MASTER(
      if (verbal) sprintf(string_output,"True children for Volume (post mask) %d",parent);
      if (verbal) print_1d_int_list(*true_children_lists[parent],string_output);
    )
  }
  
  
  // Clean up of dynamically allocated memory
  
  for(child=0;child<number_of_volumes;child++) free(temporary_children_lists[child].elements);
  free(temporary_children_lists);
  free(true_temp_list_local.elements);
  
};

void generate_overlap_lists(struct pointer_to_1d_int_list **true_overlap_lists, struct pointer_to_1d_int_list **raw_overlap_lists, struct Volume_struct **Volumes, int number_of_volumes, int verbal) {
  // This function generates overlap lists for each volume
  // Volume m overlaps volume n if there is some subset of space they both ocupy (this is called raw overlap)
  // the true overlap list takes mask into account, meaning that the masks are applied before searching for overlap

  MPI_MASTER(
  if (verbal) printf("\nGenerating overlap lists ---------------------------- \n");
  )
  
  // Manually create the overlap list for the surrounding Vacuum, as it overlaps all other volumes
  
  // temporary_overlap_lists are used to save the calculated overlaps to avoid evaluating the heavy functions more than once (twice) for each possible volume pair
  struct pointer_to_1d_int_list *temporary_overlap_lists;
  temporary_overlap_lists = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
  
  // Overlap_lists are the final result of the function, and for the surrounding volume it can be set immediatly
  true_overlap_lists[0] = malloc(sizeof(struct pointer_to_1d_int_list));
  true_overlap_lists[0]->num_elements = number_of_volumes-1;
  true_overlap_lists[0]->elements = malloc((number_of_volumes-1)*sizeof(int));
  
  raw_overlap_lists[0] =  malloc(sizeof(struct pointer_to_1d_int_list));
  raw_overlap_lists[0]->num_elements = number_of_volumes;
  raw_overlap_lists[0]->elements = malloc(number_of_volumes*sizeof(int));
  raw_overlap_lists[0]->elements[0] = 0; // Volume 0 overlaps itself
  
  int parent;
  for (parent=1;parent<number_of_volumes;parent++) {
      true_overlap_lists[0]->elements[parent-1] = parent;
      raw_overlap_lists[0]->elements[parent] = parent;
  }
  
  char string_output[128];
  MPI_MASTER(
  if (verbal) sprintf(string_output,"Overlaps for Volume %d",0);
  if (verbal) print_1d_int_list(*true_overlap_lists[0],string_output);
  )
  
  // Generate the overlap lists for the remaining volumes
  struct pointer_to_1d_int_list temp_list_local;
  temp_list_local.num_elements = number_of_volumes;
  temp_list_local.elements = malloc(number_of_volumes*sizeof(int));
  
  int child,used_elements;
  // Create overlap for the remaining volumes
  for (parent=1;parent<number_of_volumes;parent++) {
      true_overlap_lists[parent] = malloc(sizeof(struct pointer_to_1d_int_list));
      used_elements = 0;
      temp_list_local.elements[used_elements++] = 0; // Alwasy overlaps with the surrounding vacuum.
      
      for (child=1;child<number_of_volumes;child++) {
        if (child == parent) temp_list_local.elements[used_elements++] = child;
        else {
        if (strcmp("sphere",Volumes[parent]->geometry.shape) == 0 && strcmp("sphere",Volumes[child]->geometry.shape) == 0) {
            if (sphere_overlaps_sphere(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("cylinder",Volumes[parent]->geometry.shape) == 0 && strcmp("cylinder",Volumes[child]->geometry.shape) == 0) {
            if (cylinder_overlaps_cylinder(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("box",Volumes[parent]->geometry.shape) == 0 && strcmp("box",Volumes[child]->geometry.shape) == 0) {
            if (box_overlaps_box(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("cone",Volumes[parent]->geometry.shape) == 0 && strcmp("cone",Volumes[child]->geometry.shape) == 0) {
            if (cone_overlaps_cone(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("mesh",Volumes[parent]->geometry.shape) == 0 && strcmp("mesh",Volumes[child]->geometry.shape) == 0) {
            if (mesh_overlaps_mesh(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("box",Volumes[parent]->geometry.shape) == 0 && strcmp("cylinder",Volumes[child]->geometry.shape) == 0) {
            if (box_overlaps_cylinder(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("cylinder",Volumes[parent]->geometry.shape) == 0 && strcmp("box",Volumes[child]->geometry.shape) == 0) {
            if (cylinder_overlaps_box(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("box",Volumes[parent]->geometry.shape) == 0 && strcmp("sphere",Volumes[child]->geometry.shape) == 0) {
            if (box_overlaps_sphere(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("sphere",Volumes[parent]->geometry.shape) == 0 && strcmp("box",Volumes[child]->geometry.shape) == 0) {
            if (sphere_overlaps_box(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("sphere",Volumes[parent]->geometry.shape) == 0 && strcmp("cylinder",Volumes[child]->geometry.shape) == 0) {
            if (sphere_overlaps_cylinder(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("cylinder",Volumes[parent]->geometry.shape) == 0 && strcmp("sphere",Volumes[child]->geometry.shape) == 0) {
            if (cylinder_overlaps_sphere(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("cone",Volumes[parent]->geometry.shape) == 0 && strcmp("sphere",Volumes[child]->geometry.shape) == 0) {
            if (cone_overlaps_sphere(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("sphere",Volumes[parent]->geometry.shape) == 0 && strcmp("cone",Volumes[child]->geometry.shape) == 0) {
            if (sphere_overlaps_cone(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("cone",Volumes[parent]->geometry.shape) == 0 && strcmp("cylinder",Volumes[child]->geometry.shape) == 0) {
            if (cone_overlaps_cylinder(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("cylinder",Volumes[parent]->geometry.shape) == 0 && strcmp("cone",Volumes[child]->geometry.shape) == 0) {
            if (cylinder_overlaps_cone(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("cone",Volumes[parent]->geometry.shape) == 0 && strcmp("box",Volumes[child]->geometry.shape) == 0) {
            if (cone_overlaps_box(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else if (strcmp("box",Volumes[parent]->geometry.shape) == 0 && strcmp("cone",Volumes[child]->geometry.shape) == 0) {
            if (box_overlaps_cone(&Volumes[parent]->geometry,&Volumes[child]->geometry)) temp_list_local.elements[used_elements++] = child;
        }
        else {
            printf("Need overlap function for type: ");
            printf("%s",Volumes[parent]->geometry.shape);
            printf(" and type: ");
            printf("%s",Volumes[child]->geometry.shape);
            printf(".\n");
            printf("It is not yet supported to mix mesh geometries with the basic shapes, but several mesh geometries are allowed.\n");
            exit(1);
        }
        }
      }
      //allocate_list_from_temp(used_elements,temp_list_local,overlap_lists[parent]);
      allocate_list_from_temp(used_elements,temp_list_local,&temporary_overlap_lists[parent]);
      
      // Save the raw overlap data to the raw_overlap_lists[parent] list
      raw_overlap_lists[parent] = malloc(sizeof(struct pointer_to_1d_int_list));
      raw_overlap_lists[parent]->num_elements = 0;
      allocate_list_from_temp(used_elements,temp_list_local,raw_overlap_lists[parent]);
      
      if (verbal) sprintf(string_output,"Overlaps for Volume (pre mask) %d",parent);
      MPI_MASTER(
      if (verbal) print_1d_int_list(temporary_overlap_lists[parent],string_output);
      )
  }
  
  
  // The temporary_overlap_lists gives the raw overlap data for all volume pairs
  // The next tasks is to take the masks into account, so that a volume is only said to overlap another if all of these statements are true:
  // The volumes overlap each other
  // The mask's of volume 1 overlap volume 2
  // The mask's of volume 2 overlap volume 1
  // The mask's of volume 1 overlap the masks of volume 2
  
  
  int logic_var;
  int overlap_ANY,overlap_ANY_p,overlap_ANY_c;
  int mask_index,mask_index_c,mask_index_p;
  int mask_volume_index,mask_volume_index_p,mask_volume_index_c;

  for (parent=1;parent<number_of_volumes;parent++) {
    used_elements = 0;
    temp_list_local.elements[used_elements++] = 0; // Alwasy overlaps with the surrounding vacuum.
    for (child=1;child<number_of_volumes;child++) {
      if (child != parent) {
        logic_var = 1; // Assume the volumes overlap, and search for evidence that they do not.
      
        // First check if the volumes overlap
        if (0 == on_int_list(temporary_overlap_lists[parent],child)) logic_var = 0;
        
      
        // Check if child overlap with parents masks
        if (logic_var == 1 && Volumes[parent]->geometry.is_masked_volume == 1) {
          overlap_ANY = 0;
          for (mask_index=0;mask_index<Volumes[parent]->geometry.masked_by_list.num_elements;mask_index++) {
            mask_volume_index = Volumes[parent]->geometry.masked_by_list.elements[mask_index];
            if (0 == on_int_list(temporary_overlap_lists[mask_volume_index],child)) {
              if (Volumes[parent]->geometry.mask_mode == 1) {
                logic_var = 0;
                break;
              }
            } else overlap_ANY = 1;
          }
          if (Volumes[parent]->geometry.mask_mode == 2) logic_var = overlap_ANY;
        }
      
        // Check if parent overlap with childs masks
        if (logic_var == 1 && Volumes[child]->geometry.is_masked_volume == 1) {
          overlap_ANY = 0;
          for (mask_index=0;mask_index<Volumes[child]->geometry.masked_by_list.num_elements;mask_index++) {
            mask_volume_index = Volumes[child]->geometry.masked_by_list.elements[mask_index];
            if (0 == on_int_list(temporary_overlap_lists[mask_volume_index],parent)) {
              if (Volumes[child]->geometry.mask_mode == 1) {
                logic_var = 0;
                break;
              }
            } else overlap_ANY = 1;
          }
          if (Volumes[child]->geometry.mask_mode == 2) logic_var = overlap_ANY;
        }
      
        // Check if parents masks overlap childrens masks
        if (logic_var == 1 && Volumes[parent]->geometry.is_masked_volume == 1 && Volumes[child]->geometry.is_masked_volume == 1) {
          overlap_ANY = 0;
          for (mask_index_p=0;mask_index_p<Volumes[parent]->geometry.masked_by_list.num_elements;mask_index_p++) {
            mask_volume_index_p = Volumes[parent]->geometry.masked_by_list.elements[mask_index_p];
            overlap_ANY_p = 1;
            overlap_ANY_c = 0;
            for (mask_index_c=0;mask_index_c<Volumes[child]->geometry.masked_by_list.num_elements;mask_index_c++) {
              mask_volume_index_c = Volumes[child]->geometry.masked_by_list.elements[mask_index_c];
              if (0 == on_int_list(temporary_overlap_lists[mask_volume_index_p],mask_volume_index_c)) {
                if (Volumes[parent]->geometry.mask_mode == 1 && Volumes[child]->geometry.mask_mode == 1) {
                  // If both are in ALL mode and just one combination of masks does not overlap, neither does the common set
                  logic_var = 0;
                  break;
                }
                if (Volumes[parent]->geometry.mask_mode == 2 && Volumes[child]->geometry.mask_mode == 1) {
                  // If the parent is in ANY mode, but the child is in ALL, any one child not overlapping this parent mask, stops the chance for this parent mask
                  overlap_ANY_p = 0;
                  break;
                }
                
              } else {
                // Here because mask_volume_index_p and mask_volume_index_c does overlap
                if (Volumes[parent]->geometry.mask_mode == 1 && Volumes[child]->geometry.mask_mode == 2) {
                  // If the parent is in ALL mode and the child is in ANY mode, stop if a single parent volume does not overlap any child
                  overlap_ANY_c = 1;
                }
                if (Volumes[parent]->geometry.mask_mode == 2 && Volumes[child]->geometry.mask_mode == 2) {
                  // If both parent and child are in any mode, any one overlap between the masks is sufficient
                  overlap_ANY = 1;
                  // Could actually just commit to the overlap list here, and stop all loops.
                }
              }
              
            }
            if (Volumes[parent]->geometry.mask_mode == 1 && Volumes[child]->geometry.mask_mode == 2) logic_var = overlap_ANY_c;
            if (Volumes[parent]->geometry.mask_mode == 2 && Volumes[child]->geometry.mask_mode == 1 && overlap_ANY_p == 1) {
              // When parent is in any mode, and child is in ALL mode, any parent mask that overlaps all children masks is enough to end the parent loop
              logic_var = 1;
              break; // Without this break, only the last parent will matter
            }
            // if (overlap_ANY == 1) break; would speed things up a bit, but only after testing has been started and then it will be repeated
          }
          if (Volumes[parent]->geometry.mask_mode == 2 && Volumes[child]->geometry.mask_mode == 2) logic_var = overlap_ANY;
          // If both volumes have the ANY mode, just one case of overlap is enough.
        }
        
        // If all of the 4 statements above evaluate to true, the two volumes parent and child do overlap and it is added to the list.
        if (logic_var == 1) temp_list_local.elements[used_elements++] = child;
        
      }
    }
    // Allocate the actual overlap list with the new temp_list_local
    allocate_list_from_temp(used_elements,temp_list_local,true_overlap_lists[parent]);
    if (verbal) sprintf(string_output,"Overlaps for Volume (post mask) %d",parent);
    MPI_MASTER(
    if (verbal) print_1d_int_list(*true_overlap_lists[parent],string_output);
    )
  }
  
  // Clean up of dynamically allocated memory
  for(child=1;child<number_of_volumes;child++) free(temporary_overlap_lists[child].elements);
  free(temporary_overlap_lists);
  free(temp_list_local.elements);
  
};

void add_to_mask_intersect_list(struct pointer_to_1d_int_list *mask_intersect_list, int given_volume_index) {
    // A bit simpler than before
    if (on_int_list(*mask_intersect_list,given_volume_index) == 0)
      add_element_to_int_list(mask_intersect_list,given_volume_index);
}


void generate_intersect_check_lists(struct pointer_to_1d_int_list **overlap_lists,struct Volume_struct **Volumes, int number_of_volumes, int verbal) {
  // Generate intersection list for each volume
  MPI_MASTER(
  if (verbal) printf("\nGenerating intersect check lists -------------------- \n");
  )
  // Take overlap list for volume n
  // Remove entries with p < p(n)
  // Remove entries with parents on the list
  
  // Mask update: Mask volumes does not have priorities, do not do step 2 for mask volumes
  // Mask update: Instead of step 3 (Remove entries with parents on the list), move these entries to a Mask intersect list if that parent is masking the entry, otherwise remove
  
  // 1) Mask update: Take overlap list for volume n
  // 2) Mask update: Remove entries with p < p(n), except mask volumes, that are moved to another list A
  // 3) Mask update: Entries with parents on the list are removed (if these parents are masked, only remove the entry if it is a child of their parents masks)
  // 4) Possible optimization: Entries with parents on the A list, are moved to the appropriate mask list for this volume
  // 5) Mask update: Entries on list A are added again, if they are not parents of volume n
  // 6) Mask update: Entries on the main list that are masked are moved to the appropriate mask list for this volume, if their mask is on list A
  
  // Justification of the top algorithm:
  // No need to check intersections with something that does not overlap this volume, so start with intersect list and remove from that.
  // Entries with p < p(n) are "beneath" this volume, and thus doesn't cut it, mask volumes are however always "above" and are treated with care (moved to list A)
  // Entries with parents still on the list is removed, as it is impossible to intersect with them without first going through the parent
  // Entries with parents on the A list can only be seen if the ray is in that particular mask (the parent on the A list), so it can be added to the appropriate mask list for this volume
  // Now all masks that cut the volume are transfered back to the main list, the only masks that overlap and does not cut are parents of this volume, and these are thus not added
  // The entries on the list that are masked are added to the appropriate mask lists of this volume, but only if their mask is on the A list, as only masks on list A can have mask status 1 which is required for this to be relevant (this was true automatically for the ones added to the appropriate lists in the optimization step).
  
  
  // The intersect check list for volume 0 is done manually
  // Declare list for holding logic data for each index in the overlap list of this volume
  struct pointer_to_1d_int_list logic_list;
  logic_list.num_elements = overlap_lists[0]->num_elements;
  logic_list.elements = malloc(logic_list.num_elements * sizeof(int));
  
  // Declare similar list for List A
  struct pointer_to_1d_int_list mask_logic_list;
  mask_logic_list.num_elements = overlap_lists[0]->num_elements;
  mask_logic_list.elements = malloc(mask_logic_list.num_elements * sizeof(int));
  
  
  int iterate;
  int overlap_index;
  
  /*
  // Thise code is an old broken version of intersection_check_list generation only for volume 0, but this case is now included in the loop.
  // Assume all volumes on overlap list should be on intersection check list, and remove the once that should not.
  for (iterate=0;iterate<logic_list.num_elements;iterate++) logic_list.elements[iterate] = 1;
  // Asuume no volumes should go on the mask_logic_list, but add the ones that do.
  for (iterate=0;iterate<mask_logic_list.num_elements;iterate++) mask_logic_list.elements[iterate] = 0;
  
  for (overlap_index = 0;overlap_index < overlap_lists[0]->num_elements;overlap_index++) {
    // No volumes to remove with lower priority, as surrounding vacuum have the lowest.
    
    // Check if this overlap_lists[0]->elements[overlap_index] is a child of another member of the overlap list
    for (iterate = 0;iterate < overlap_lists[0]->num_elements;iterate++) {
        if (iterate != overlap_index) {
            // We are now checking if Volumes[overlap_lists[0]->elements[iterate]]->geometry.children contains overlap_lists[overlap_index]
            if (on_int_list(Volumes[overlap_lists[0]->elements[iterate]]->geometry.children,overlap_lists[0]->elements[overlap_index])) logic_list.elements[overlap_index] = 0;
        }
    }
  }

  Volumes[0]->geometry.intersect_check_list.num_elements = sum_int_list(logic_list);
  Volumes[0]->geometry.intersect_check_list.elements = malloc(Volumes[0]->geometry.intersect_check_list.num_elements * sizeof(int));
  
  iterate = 0;
  for (overlap_index = 0;overlap_index < overlap_lists[0]->num_elements;overlap_index++) {
        if (logic_list.elements[overlap_index])   Volumes[0]->geometry.intersect_check_list.elements[iterate++] = overlap_lists[0]->elements[overlap_index];
  }
  if (logic_list.num_elements > 0) free(logic_list.elements);
  if (mask_logic_list.num_elements > 0) free(mask_logic_list.elements);
  
  MPI_MASTER(
  print_1d_int_list(Volumes[0]->geometry.intersect_check_list,"Intersect check list for Volume 0 (manual)");
  )
  */
  
  // The intersect check lists for the remaining volumes are generated
  int volume_index,mask_volume_number,mask_index;
  //for (volume_index = 1;volume_index < number_of_volumes;volume_index++) {
  for (volume_index = 0;volume_index < number_of_volumes;volume_index++) {
    
      // 1) Take overlap list for volume n
      logic_list.num_elements = overlap_lists[volume_index]->num_elements;
      logic_list.elements = malloc(logic_list.num_elements * sizeof(int));
      mask_logic_list.num_elements = overlap_lists[volume_index]->num_elements;
      mask_logic_list.elements = malloc(mask_logic_list.num_elements * sizeof(int));
      
      
      
      if (Volumes[volume_index]->geometry.is_mask_volume == 1) {
        // Masks do not have any entries on their intersect check list, as they are never the current volume
        for (iterate=0;iterate<logic_list.num_elements;iterate++) logic_list.elements[iterate] = 0;
        for (iterate=0;iterate<mask_logic_list.num_elements;iterate++) mask_logic_list.elements[iterate] = 0;
      } else {
        for (iterate=0;iterate<logic_list.num_elements;iterate++) logic_list.elements[iterate] = 1;
        for (iterate=0;iterate<mask_logic_list.num_elements;iterate++) mask_logic_list.elements[iterate] = 0;
      }
      
      //2) Remove entries with p < p(n), except mask volumes, that are moved to another list A
      for (overlap_index = 0;overlap_index < overlap_lists[volume_index]->num_elements;overlap_index++) {
        
        if (overlap_lists[volume_index]->elements[overlap_index] == 0) logic_list.elements[overlap_index] = 0; // The surrounding vacuum has lower priority (and is not a mask)
        else if (Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.is_mask_volume == 1) {
           logic_list.elements[overlap_index] = 0;      // Remove this volume from the main list
           mask_logic_list.elements[overlap_index] = 1; // Add mask volumes to seperat list
        }
        else if (volume_index != 0) { // Only relevant to remove elements because of priority if the volume_index is different from 0, meaning the surrounding vacuum skips this step
           if (Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.priority_value < Volumes[volume_index]->geometry.priority_value) {
           // If the investigated volume on the overlap_list have lower priority than the volume with volume_index, remove it
           logic_list.elements[overlap_index] = 0; // Remove this volume from the list
           }
        }
      }
      
      // 3) Entries with parents on the list are removed
      for (overlap_index = 0;overlap_index < overlap_lists[volume_index]->num_elements;overlap_index++) {
        // Check if this overlap_lists[0]->elements[overlap_index] is a child of another member of the overlap list
        for (iterate = 0;iterate < overlap_lists[volume_index]->num_elements;iterate++) {
             if (iterate != overlap_index) { // Only necessary if a volume determines that it has itself as child, but a nice safety.
                // We are now checking if Volumes[overlap_lists[volume_index]->elements[iterate]]->geometry.children contains overlap_lists[overlap_index]
                // The && part is needed because we do not remove children of elements that have allready been removed because of their priority
                
                // if (on_int_list(Volumes[overlap_lists[volume_index]->elements[iterate]]->geometry.children,overlap_lists[volume_index]->elements[overlap_index]) && logic_list.elements[overlap_lists[volume_index]->elements[iterate]] == 1) {logic_list.elements[overlap_index] = 0; bug fixed on 3/4 2016
                if (on_int_list(Volumes[overlap_lists[volume_index]->elements[iterate]]->geometry.children,overlap_lists[volume_index]->elements[overlap_index]) && logic_list.elements[iterate] == 1) logic_list.elements[overlap_index] = 0;
                
                /*
                Explanation with simpler notation
                
                Overlap list of volume i = O_i
                Element j of overlap list i = O_i(j)
                
                Children list of volume i = C_i
                Element j on children list i = C_i(j)
                
                Priority of volume i p(i)
                
                for i = volumes
                    for j in O_i(j) 
                        logic(j) = 1;
                        if p(i) > p(O_i(j)) logic(j) = 0; // Remove if lower priority than the currently checked
                        
                        for k in O_i(k)
                            if (O_i(j) is contained on the list C_k && logic(k) == 1) logic(j) = 0
                */
                
                // 4) Entries with parents on the A list, are moved to the appropriate mask list for this volume
                if (on_int_list(Volumes[overlap_lists[volume_index]->elements[iterate]]->geometry.children,overlap_lists[volume_index]->elements[overlap_index]) && mask_logic_list.elements[iterate] == 1) {
                   logic_list.elements[overlap_index] = 0; // Remove from main list (Not strictly needed as it will be removed already if it is on list A)
                   // Add overlap_lists[volume_index]->elements[overlap_index] to volumes mask intersect list for mask with index overlap_lists[volume_index]->elements[iterate]
                   //add_to_mask_intersect_lists(&Volumes[volume_index]->geometry.mask_intersect_lists,overlap_lists[volume_index]->elements[iterate],overlap_lists[volume_index]->elements[overlap_index]);
                   // Simpler method that just collects all the masked intersect parts on a 1d_int list instead of seperating them for each mask
                   add_to_mask_intersect_list(&Volumes[volume_index]->geometry.mask_intersect_list,overlap_lists[volume_index]->elements[overlap_index]);
                }
                
             }
        }
      }
      
      // 5) Entries on list A are added again, if they are not parents of volume n
      // Time to add mask volumes removed back to the intersect list, if they are not parents of the current volume, meaning the current volume should not be a child of the mask
      for (overlap_index = 0;overlap_index < overlap_lists[volume_index]->num_elements;overlap_index++) {
        if (mask_logic_list.elements[overlap_index] == 1) {
          mask_volume_number = overlap_lists[volume_index]->elements[overlap_index];
          if (on_int_list(Volumes[mask_volume_number]->geometry.children,volume_index) == 0) logic_list.elements[overlap_index] = 1; // Add it back to the list
        }
      }
      
      // 6) Entries on the main list that are masked are moved to the appropriate mask list for this volume, if their mask is on list A
      int mask_index,mask_mode,found_index,logic_ALL;
      for (overlap_index = 0;overlap_index < overlap_lists[volume_index]->num_elements;overlap_index++) {
        if (logic_list.elements[overlap_index] == 1 && Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.is_masked_volume == 1) {
          logic_list.elements[overlap_index] = 0; // If the volume is masked, remove it from the intersect check list
          // Could actually keep it on the intersect list if the volume's mask is a parent of the current volume, now it will just be added in all cases
          
          // When ALL setting is used, all masks should overlap the current volume, otherwise the user probably made a mistake, this should be checked in error checking
          // When ANY setting is used, at least one mask should overlap the current volume, otherwise the user probably made a mistake, this should be checked in error checking
          // mask_mode == 1 => ALL / mask_mode == 2 => ANY
          mask_mode = Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.mask_mode;
          
          if (mask_mode == 1) {
            logic_ALL = 1;
            for (iterate=0;iterate<Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.masked_by_list.num_elements;iterate++) {
              mask_index = Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.masked_by_list.elements[iterate];
              
              if (on_int_list(*overlap_lists[volume_index],mask_index) == 0) {
                // If any one of the volumes masks do not overlap with this volume, there is no reason check intersections with the volume regardless of the mask status's
                logic_ALL = 0;
                break;
              }
            }
          if (logic_ALL == 1) {
            // If all of it's masks are overlapping the current volume, add it to all relevant mask intersect lists of this volume
            for (iterate=0;iterate<Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.masked_by_list.num_elements;iterate++) {
              mask_index = Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.masked_by_list.elements[iterate];
              //add_to_mask_intersect_lists(&Volumes[volume_index]->geometry.mask_intersect_lists,mask_index,overlap_lists[volume_index]->elements[overlap_index]);
              // Adding the masked intersect list elements to another list
              add_to_mask_intersect_list(&Volumes[volume_index]->geometry.mask_intersect_list,overlap_lists[volume_index]->elements[overlap_index]);
            }
          }
            
            
          } else if (mask_mode == 2) {
          // When in ANY mode, the problem is easier, as not all masks have to overlap the volume, and we can add each one to the list
            for (iterate=0;iterate<Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.masked_by_list.num_elements;iterate++) {
              mask_index = Volumes[overlap_lists[volume_index]->elements[overlap_index]]->geometry.masked_by_list.elements[iterate];
            
              if (on_int_list(*overlap_lists[volume_index],mask_index) == 1) {
                //add_to_mask_intersect_lists(&Volumes[volume_index]->geometry.mask_intersect_lists,mask_index,overlap_lists[volume_index]->elements[overlap_index]);
                // Adding the masked intersect list elements to another list
                add_to_mask_intersect_list(&Volumes[volume_index]->geometry.mask_intersect_list,overlap_lists[volume_index]->elements[overlap_index]);
              }
            }
            
          }
        }
      }
      
      
      Volumes[volume_index]->geometry.intersect_check_list.num_elements = sum_int_list(logic_list);
      Volumes[volume_index]->geometry.intersect_check_list.elements = malloc(Volumes[volume_index]->geometry.intersect_check_list.num_elements * sizeof(int));
      
      iterate = 0;
      for (overlap_index = 0;overlap_index < overlap_lists[volume_index]->num_elements;overlap_index++) {
            if (logic_list.elements[overlap_index])   Volumes[volume_index]->geometry.intersect_check_list.elements[iterate++] = overlap_lists[volume_index]->elements[overlap_index];
      }
      free(logic_list.elements); // Need to be careful with names for variables to be freed, as they can collide with names in main because of automatic declaration of external variables
      free(mask_logic_list.elements);
      
      
      char string_output[128];
      MPI_MASTER(
      if (verbal) sprintf(string_output,"Intersect check list for Volume %d",volume_index);
      if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.intersect_check_list,string_output);
      if (verbal) sprintf(string_output,"Mask intersect check list for Volume %d",volume_index);
      if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.mask_intersect_list,string_output);
      /*
      if (verbal) {
        for (mask_index=0;mask_index<Volumes[volume_index]->geometry.mask_intersect_lists.number_of_lists;mask_index++) {
          sprintf(string_output," - mask intersect check list for mask %d which is volume %d",mask_index,Volumes[volume_index]->geometry.mask_intersect_lists.mask_indices.elements[mask_index]);
          print_1d_int_list(Volumes[volume_index]->geometry.mask_intersect_lists.lists[mask_index],string_output);
        }
      }
      */
      
      )
  }
};

void generate_parents_lists(struct pointer_to_1d_int_list **parents_lists, struct Volume_struct **Volumes, int number_of_volumes, int verbal, int mask_mode) {
  // Function for generating parent lists for all volumes
  // A volume m has n as a parent, if volume n has volume m as a child
  
  // if mask_mode == 0, masks are ignored, if mask_mode == 1, masks are taken into account.
  
  MPI_MASTER(
  if (verbal) {
    if (mask_mode == 1)
        printf("\nGenerating parents lists ---------------------------- \n");
    else if (mask_mode == 0)
        printf("\nGenerating parents lists (ignoring masks) ----------- \n");
    else {
        printf("Error, the function parents_lists got a non defined mask_mode");
        exit(1);
    }
  }
  
  )
  // Volume iterate has volume p as a parent, if volume p has volume iterate as child.
  
  struct pointer_to_1d_int_list temp_list_local;
  temp_list_local.num_elements = number_of_volumes;
  temp_list_local.elements = malloc(number_of_volumes*sizeof(int));
  
  // Loop over
  int iterate,parent,used_elements;
  for (iterate = 0;iterate < number_of_volumes;iterate++) {
    // clear temp list
    used_elements = 0;
    parents_lists[iterate] = malloc(sizeof(struct pointer_to_1d_int_list));
    for (parent = 0;parent < number_of_volumes;parent++) {
        if (on_int_list(Volumes[parent]->geometry.children,iterate))
          if (mask_mode == 1 || (Volumes[parent]->geometry.is_mask_volume == 0 && Volumes[iterate]->geometry.is_mask_volume == 0))
            temp_list_local.elements[used_elements++] = parent;
    }
      allocate_list_from_temp(used_elements,temp_list_local,parents_lists[iterate]);
      
      char string_output[128];
      MPI_MASTER(
      if (verbal) sprintf(string_output,"Parents for Volume %d",iterate);
      if (verbal) print_1d_int_list(*parents_lists[iterate],string_output);
      )
  }
  free(temp_list_local.elements);

};

void generate_true_parents_lists(struct pointer_to_1d_int_list **parents_lists, struct pointer_to_1d_int_list **true_children_lists, struct Volume_struct **Volumes, int number_of_volumes, int verbal, int mask_mode) {
  // Function for generating parent lists for all volumes
  // A volume m has n as a parent, if volume n has volume m as a child
  
  // if mask_mode == 0, masks are ignored, if mask_mode == 1, masks are taken into account.
  
  MPI_MASTER(
  if (verbal) {
    if (mask_mode == 1)
        printf("\nGenerating parents lists ---------------------------- \n");
    else if (mask_mode == 0)
        printf("\nGenerating parents lists (ignoring masks) ----------- \n");
    else {
        printf("Error, the function parents_lists got a non defined mask_mode");
        exit(1);
    }
  }
  
  )
  // Volume iterate has volume p as a parent, if volume p has volume iterate as child.
  
  struct pointer_to_1d_int_list temp_list_local;
  temp_list_local.num_elements = number_of_volumes;
  temp_list_local.elements = malloc(number_of_volumes*sizeof(int));
  
  // Loop over
  int iterate,parent,used_elements;
  for (iterate = 0;iterate < number_of_volumes;iterate++) {
    // clear temp list
    used_elements = 0;
    parents_lists[iterate] = malloc(sizeof(struct pointer_to_1d_int_list)); // allocate_list_from_temp allocates
    for (parent = 0;parent < number_of_volumes;parent++) {
        //if (on_int_list(Volumes[parent]->geometry.children,iterate))
        if (on_int_list(*true_children_lists[parent],iterate))
          if (mask_mode == 1 || (Volumes[parent]->geometry.is_mask_volume == 0 && Volumes[iterate]->geometry.is_mask_volume == 0))
            temp_list_local.elements[used_elements++] = parent;
    }
      allocate_list_from_temp(used_elements,temp_list_local,parents_lists[iterate]);
      
      char string_output[128];
      MPI_MASTER(
      if (verbal) sprintf(string_output,"Parents for Volume %d",iterate);
      if (verbal) print_1d_int_list(*parents_lists[iterate],string_output);
      )
  }
  free(temp_list_local.elements);

};

void generate_intersect_check_lists_experimental(struct pointer_to_1d_int_list **true_overlap_lists, struct pointer_to_1d_int_list **raw_overlap_lists, struct pointer_to_1d_int_list **parents_lists, struct pointer_to_1d_int_list **true_parents_lists , struct Volume_struct **Volumes, int number_of_volumes, int verbal) {
  // Generates the intersect_check_list and mask_intersect_list for each Volume.
  /*
  Description of needed lists for volume n:
  Children list: List of volumes that is contained within volume n (is stored in the Volumes struct)
  True Children list: List of volumes that when masked by their masks is contained within volume n when masked by it's masks
  
  Parents list: List of volumes that contains volume n
  True parents list: List of volumes that when masked by their masks contains volume n when it is masked by it's masks
  
  raw overlap list: List of volumes whos geometry overlaps the geometry of volume n
  true overlap list: List of volumes whos geometry overlaps the geometry of volume n when the masks of both volumes are applied
  
  
  
  The algorithm:
  
  1) Take the true overlap list for volume n
  2) remove parents of n (normal parent list, with masks)
  3) remove volumes that do not mask n and are true parents of n
  4) remove volumes that are not masks and have lower priority than n
  5) remove volumes that have at least one true parent on the list (in step 4) that is not a mask volume
  6) split the list into two, the intersect_check_list which is all non masked volumes still on the list, and the mask_intersect_list which is the masked volumes
  7) remove volumes on the mask_intersect_list whos mask does not overlap (standard overlap list) n
  
  In step 5 the order in which the volumes are tested and removed may matter, so it is specifically stated that it is as the list looked in step 4.
  */
  
  MPI_MASTER(
  if (verbal) printf("\nGenerating intersect check lists -------------------- \n");
  )
  
  struct pointer_to_1d_int_list work_list;
  struct pointer_to_1d_int_list logic_list;
  
  int volume_index,iterate,parent,mask_index,masked_volume_index,ANY_logic,true_parent_volume_number;
  int *mask_check,*mask_start;
  for (volume_index = 0;volume_index < number_of_volumes;volume_index++) {
    
    // 1) Take the true overlap list for volume n
    // Create copy of true_overlap_lists to work with
    
    if (Volumes[volume_index]->geometry.is_mask_volume == 1) {
      // Bug fixed on 26/11/2016, do not create intersection lists for masks as they are not used, and affects destinations lists in a problematic way
      Volumes[volume_index]->geometry.intersect_check_list.num_elements = 0;
      Volumes[volume_index]->geometry.mask_intersect_list.num_elements = 0;
      
    } else {
      work_list.num_elements = true_overlap_lists[volume_index]->num_elements;
      work_list.elements = malloc(work_list.num_elements * sizeof(int));
      for (iterate=0;iterate<work_list.num_elements;iterate++) work_list.elements[iterate] = true_overlap_lists[volume_index]->elements[iterate];
    
    //if (verbal) print_1d_int_list(work_list,"After 1)");
    
    
    //2) remove parents of n (normal parent list, with masks)
    for (iterate=work_list.num_elements-1;iterate>-1;iterate--) {
      if (on_int_list(*parents_lists[volume_index],work_list.elements[iterate]))
        remove_element_in_list_by_index(&work_list,iterate);
    }
    
    //if (verbal) print_1d_int_list(work_list,"After 2)");
    
    //3) remove volumes that do not mask n and are true parents of n
    for (iterate=work_list.num_elements-1;iterate>-1;iterate--) {
      if (on_int_list(Volumes[volume_index]->geometry.masked_by_list,work_list.elements[iterate]) == 0 && on_int_list(*true_parents_lists[volume_index],work_list.elements[iterate]))
        remove_element_in_list_by_index(&work_list,iterate);
    }
    
    //if (verbal) print_1d_int_list(work_list,"After 3)");
    
    //4) remove volumes that are not masks and have lower priority than n
    for (iterate=work_list.num_elements-1;iterate>-1;iterate--) {
      if (Volumes[work_list.elements[iterate]]->geometry.is_mask_volume == 0 && Volumes[work_list.elements[iterate]]->geometry.priority_value < Volumes[volume_index]->geometry.priority_value)
        remove_element_in_list_by_index(&work_list,iterate);
    }
    
    //if (verbal) print_1d_int_list(work_list,"After 4)");
    
    //5) remove volumes that have at least one true parent on the list (in step 4) that is not a mask volume
    // Here a logic_list is used to not have the order of removal matter
    logic_list.num_elements = work_list.num_elements;
    logic_list.elements = malloc(logic_list.num_elements * sizeof(int));
    for(iterate=0;iterate<logic_list.num_elements;iterate++) logic_list.elements[iterate] = 1;
    
    for (iterate=0;iterate<work_list.num_elements;iterate++) {
      for (parent=0;parent<true_parents_lists[work_list.elements[iterate]]->num_elements;parent++) {
        true_parent_volume_number = true_parents_lists[work_list.elements[iterate]]->elements[parent];
        if (on_int_list(work_list,true_parent_volume_number) && Volumes[true_parent_volume_number]->geometry.is_mask_volume == 0) {
          // Since element number iterate on the work list have a true parent on the work list, it can be removed
          logic_list.elements[iterate] = 0;
          break;
        }
      }
    }
    
    // Now the elements marked for removal can be removed without interfering in the operation
    for (iterate=work_list.num_elements-1;iterate>-1;iterate--) {
      if (logic_list.elements[iterate] == 0) remove_element_in_list_by_index(&work_list,iterate);
    }
    free(logic_list.elements);
    
    //if (verbal) print_1d_int_list(work_list,"After 5)");
      
    //6) split the list into two, the intersect_check_list which is all non masked volumes still on the list, and the mask_intersect_list which is the masked volumes
    
    Volumes[volume_index]->geometry.intersect_check_list.num_elements = 0;
    Volumes[volume_index]->geometry.mask_intersect_list.num_elements = 0;
    
    for (iterate=0;iterate<work_list.num_elements;iterate++) {
      if (Volumes[work_list.elements[iterate]]->geometry.is_masked_volume == 1) {
        add_element_to_int_list(&Volumes[volume_index]->geometry.mask_intersect_list,work_list.elements[iterate]);
      } else {
        add_element_to_int_list(&Volumes[volume_index]->geometry.intersect_check_list,work_list.elements[iterate]);
      }
    }
    
    //if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.intersect_check_list,"After 6) intersect_check_list");
    //if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.mask_intersect_list,"After 6) mask_intersect_list");
    
    //7) remove volumes on the mask_intersect_list whos masks does not overlap (standard overlap list) n
    for (iterate=Volumes[volume_index]->geometry.mask_intersect_list.num_elements-1;iterate>-1;iterate--) {
      // Need to check if the volumes masking Volumes[volume_index]->geometry.mask_intersect_list.elements[iterate] overlaps with n
      masked_volume_index = Volumes[volume_index]->geometry.mask_intersect_list.elements[iterate];
      
      if (Volumes[masked_volume_index]->geometry.mask_mode == 1) {// All mode, if just one does not overlap, remove the element
        for (mask_start=mask_check=Volumes[masked_volume_index]->geometry.masked_by_list.elements;mask_check-mask_start<Volumes[masked_volume_index]->geometry.masked_by_list.num_elements;mask_check++) {
          if (on_int_list(*raw_overlap_lists[volume_index],*mask_check) == 0) {
            remove_element_in_list_by_index(&Volumes[volume_index]->geometry.mask_intersect_list,iterate);
            break;
          }
        }
      } else { // ANY mode, just one need to overlap in order to keep the element
        ANY_logic = 0;
        for (mask_start=mask_check=Volumes[masked_volume_index]->geometry.masked_by_list.elements;mask_check-mask_start<Volumes[masked_volume_index]->geometry.masked_by_list.num_elements;mask_check++) {
          if (on_int_list(*raw_overlap_lists[volume_index],*mask_check) == 1) {
            ANY_logic = 1;
            break;
          }
        }
        if (ANY_logic == 0) remove_element_in_list_by_index(&Volumes[volume_index]->geometry.mask_intersect_list,iterate);
      }
    }
    
    if (work_list.num_elements > 0) free(work_list.elements);
    }
    
    
    //if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.mask_intersect_list,"After 7) mask_intersect_list");
    
    char string_output[128];
    MPI_MASTER(
    if (verbal) sprintf(string_output,"Intersect check list for Volume %d",volume_index);
    if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.intersect_check_list,string_output);
    if (verbal) sprintf(string_output,"Mask intersect check list for Volume %d",volume_index);
    if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.mask_intersect_list,string_output);
    )
    
    
  }
}

void generate_grandparents_lists(struct pointer_to_1d_int_list **grandparents_lists, struct pointer_to_1d_int_list **parents_lists, int number_of_volumes, int verbal) {
  // Function for generating grandparents lists
  // Volume iterate has volume p as a grandparent, if volume p has a parent T, who has volume iterate as parent.
  // Alternertively:
  // Volume iterate has volume p as a grandparent, if volume p have a child that is volume iterate's parent.
  
  MPI_MASTER(
  if (verbal) printf("\nGenerating grandparents lists ----------------------- \n");
  )

  struct pointer_to_1d_int_list common;
  common.num_elements = number_of_volumes;
  common.elements = malloc(common.num_elements * sizeof(int)); // Maximum needed space.
  
  struct pointer_to_1d_int_list temp_list_local;
  temp_list_local.num_elements = number_of_volumes;
  temp_list_local.elements = malloc(number_of_volumes*sizeof(int));
  
  int iterate,parent,child,used_elements;
  for (iterate = 0;iterate < number_of_volumes;iterate++) {
    // clear temp list
    used_elements = 0;
    grandparents_lists[iterate] = malloc(sizeof(struct pointer_to_1d_int_list));
    
    for (parent = 0;parent < parents_lists[iterate]->num_elements;parent++) {
        // parent number p parents_lists[iterate].elements.[p] in the parent_list for iterate.
        on_both_int_lists(parents_lists[parents_lists[iterate]->elements[parent]],parents_lists[iterate],&common);
        // returns a pointer_to_1d_list, with all the elements that are in common.
        for (child = 0;child < common.num_elements;child++) {
            // Need to make sure the element is not already on the list
            if (0 == on_int_list(temp_list_local,common.elements[child])) {
              temp_list_local.elements[used_elements++] = common.elements[child];
            }
        }
    }
      allocate_list_from_temp(used_elements,temp_list_local,grandparents_lists[iterate]);
      
      char string_output[128];
      MPI_MASTER(
      if (verbal) sprintf(string_output,"Grandparents for Volume %d",iterate);
      if (verbal) print_1d_int_list(*grandparents_lists[iterate],string_output);
      )
  }
  free(temp_list_local.elements);
  free(common.elements);
};


void generate_destinations_lists_experimental(struct pointer_to_1d_int_list **true_overlap_lists, struct pointer_to_1d_int_list **true_children_lists, struct pointer_to_1d_int_list **true_parents_lists, struct pointer_to_1d_int_list **true_grandparents_lists, struct Volume_struct **Volumes, int number_of_volumes, int verbal) {
  // Generates destinations list for for all volumes
  // Current implementation uses true_parents_lists and true_grandparents_lists that are generated as if no masks were defined

  MPI_MASTER(
  if (verbal) printf("\nGenerating destinations lists ----------------------- \n");
  )
  // Volume 0 has an hardcoded empty destinations list
  Volumes[0]->geometry.destinations_list.num_elements = 0;

  struct pointer_to_1d_int_list work_list;
  int volume_index,iterate,iterate2,found_index,I_index,I_volume;
  
  for (volume_index=1;volume_index<number_of_volumes;volume_index++) {
    
    // 1) Take the true overlap list for volume n
    // Create copy of true_overlap_lists to work with
    work_list.num_elements = true_overlap_lists[volume_index]->num_elements;
    work_list.elements = malloc(work_list.num_elements * sizeof(int));
    for (iterate=0;iterate<work_list.num_elements;iterate++) work_list.elements[iterate] = true_overlap_lists[volume_index]->elements[iterate];
    
    //if (verbal) print_1d_int_list(work_list,"After 1)");
    
    // 2) Remove elements from n's intersection list
    for (iterate=work_list.num_elements-1;iterate>-1;iterate--) {
      if (on_int_list(Volumes[volume_index]->geometry.intersect_check_list,work_list.elements[iterate]))
        remove_element_in_list_by_index(&work_list,iterate);
    }
    
    //if (verbal) print_1d_int_list(work_list,"After 2)");
  
    // 3) Remove true children of non-mask elements on n's intersection list
    for (I_index=0;I_index<Volumes[volume_index]->geometry.intersect_check_list.num_elements;I_index++) {
      I_volume = Volumes[volume_index]->geometry.intersect_check_list.elements[I_index];
      if (Volumes[I_volume]->geometry.is_mask_volume == 0) {
        for (iterate=0;iterate<true_children_lists[I_volume]->num_elements;iterate++) {
          remove_element_in_list_by_value(&work_list,true_children_lists[I_volume]->elements[iterate]);
        }
      }
    }
    
    //if (verbal) print_1d_int_list(work_list,"After 3)");
    
    // 4) Remove elements from mask intersection list
    for (iterate=work_list.num_elements-1;iterate>-1;iterate--) {
      if (on_int_list(Volumes[volume_index]->geometry.mask_intersect_list,work_list.elements[iterate]))
        remove_element_in_list_by_index(&work_list,iterate);
    }
    
    //if (verbal) print_1d_int_list(work_list,"After 4)");
    
    // 5) Remove true children of elements on n's mask intersection list
    for (I_index=0;I_index<Volumes[volume_index]->geometry.mask_intersect_list.num_elements;I_index++) {
      I_volume = Volumes[volume_index]->geometry.mask_intersect_list.elements[I_index];
      if (Volumes[I_volume]->geometry.is_mask_volume == 0) {
        for (iterate=0;iterate<true_children_lists[I_volume]->num_elements;iterate++) {
          remove_element_in_list_by_value(&work_list,true_children_lists[I_volume]->elements[iterate]);
        }
      }
    }
    
    //if (verbal) print_1d_int_list(work_list,"After 5)");
    
    // 6) Remove true children of n
    for (iterate=0;iterate<true_children_lists[volume_index]->num_elements;iterate++)
      remove_element_in_list_by_value(&work_list,true_children_lists[volume_index]->elements[iterate]);
      
    //if (verbal) print_1d_int_list(work_list,"After 6)");
      
    // 7) Remove true grandparents of n
    for (iterate=0;iterate<true_grandparents_lists[volume_index]->num_elements;iterate++)
      remove_element_in_list_by_value(&work_list,true_grandparents_lists[volume_index]->elements[iterate]);
      
    //if (verbal) print_1d_int_list(work_list,"After 7)");
      
    // 8) Remove mask volumes
    for (iterate=work_list.num_elements-1;iterate>-1;iterate--) {
      if (Volumes[work_list.elements[iterate]]->geometry.is_mask_volume == 1)
        remove_element_in_list_by_index(&work_list,iterate);
    }
    
    //if (verbal) print_1d_int_list(work_list,"After 8)");
    
    // 9) Remove true parents of n on the list that has other true parents of n on the list with higher priority
    for (iterate=work_list.num_elements-1;iterate>-1;iterate--) {
      if (on_int_list(*true_parents_lists[volume_index],work_list.elements[iterate])){
        // work_list.elements[iterate] is the volume index of a volume that is a true parent of n
        for (iterate2=0;iterate2<work_list.num_elements;iterate2++) {
          if (on_int_list(*true_parents_lists[volume_index],work_list.elements[iterate2]) && iterate != iterate2) {
            if (Volumes[work_list.elements[iterate]]->geometry.priority_value < Volumes[work_list.elements[iterate2]]->geometry.priority_value) {
              //printf("Removing element number %d (V%d) because element number %d (V%d) had higher priority \n",iterate,work_list.elements[iterate],iterate2,work_list.elements[iterate2]);
              remove_element_in_list_by_index(&work_list,iterate);
              break; // Missing break inserted on 14/9/2016
            }
          }
        }
      }
    }
    
    //if (verbal) print_1d_int_list(work_list,"After 9)");
    
    // 10) The remaining list is the destinations_list
    Volumes[volume_index]->geometry.destinations_list.num_elements = work_list.num_elements;
    Volumes[volume_index]->geometry.destinations_list.elements = malloc(Volumes[volume_index]->geometry.destinations_list.num_elements*sizeof(int));
    
    for (iterate=0;iterate<work_list.num_elements;iterate++)
      Volumes[volume_index]->geometry.destinations_list.elements[iterate] = work_list.elements[iterate];
    
    // Clean up after work_list so the next can be allocated
    free(work_list.elements);
  
  
    char string_output[128];
    MPI_MASTER(
    if (verbal) sprintf(string_output,"Destinations list for Volume %d",volume_index);
    if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.destinations_list,string_output);
    )

  }

};

void generate_destinations_list(int N_volume,struct Volume_struct **Volumes,struct pointer_to_1d_int_list original_overlap_list,struct pointer_to_1d_int_list *parent_list, struct pointer_to_1d_int_list *grandparent_list) {
    // This function generates the destinations_list for a single volume index, N_volume
    // The destination list describes which volumes a ray can enter when leaving N_volume
    // Each of the 6 steps for the algorithm is commented individually, and debug print statements are available
    
    // Mask update: Mask volumes are to be removed from all destinations_list as they can not be the current volume
    // It is not that simple, as some volume will then get empty destination lists. Need to revise this algorithm.
    
    // 1) Start with the overlap list of volume N
    // 2) remove all Volumes from the overlap list of N, which is also on the intersect_check_list
    // 3) remove the children of volumes removed in step 2)
    // 4) remove the children of N
    // 5) remove the grandparents of N
    // 6) remove volumes with lower priority than parents of N still on the list
    // 7) The remaing list is the destinations list
    
    
    // The destination list system should run without masks altogether, meaning parent and grandparent lists should not use them,
    //  and all masks should be removed in step 2
    
    // 1) Start with the overlap list of volume N
    // 2) remove all masks on the list
    // 3) remove all Volumes from the overlap list of N, which is also on the intersect_check_list
    // 4) remove the children of volumes removed in step 2) (if the removed volume was not a mask )
    // 5) remove the children of N
    // 6) remove the grandparents of N
    // 7) remove volumes with lower priority than parents of N still on the list
    // 8) The remaing list is the destinations list
    
    

    // 1) Start with the overlap list of volume N
    struct pointer_to_1d_int_list overlap_list;
    overlap_list.num_elements = original_overlap_list.num_elements;
    overlap_list.elements = malloc(overlap_list.num_elements*sizeof(int));
    int iterate;
    for (iterate=0;iterate<overlap_list.num_elements;iterate++) overlap_list.elements[iterate] = original_overlap_list.elements[iterate];
    
    char string_output[124];
    // sprintf(string_output,"Destinations list for Volume %d step 1",N_volume);
    // print_1d_int_list(overlap_list,string_output);
    
    // 2) remove all masks
    for (iterate=overlap_list.num_elements-1;iterate > -1;iterate--) {
        if (Volumes[overlap_list.elements[iterate]]->geometry.is_mask_volume == 1) {
            remove_element_in_list_by_index(&overlap_list,iterate);
        }
    }
    
    // 3) remove all Volumes from the overlap list of N, which is also on the intersect_check_list
    struct pointer_to_1d_int_list removed_under_2;
    removed_under_2.num_elements = 0;
    int to_check;
    removed_under_2.elements = malloc( Volumes[N_volume]->geometry.intersect_check_list.num_elements * sizeof(int));
    for (iterate=0;iterate < Volumes[N_volume]->geometry.intersect_check_list.num_elements;iterate++) {
        to_check = Volumes[N_volume]->geometry.intersect_check_list.elements[iterate];
        if (on_int_list(overlap_list,to_check)) {
            removed_under_2.elements[removed_under_2.num_elements++] = to_check;
            remove_element_in_list_by_value(&overlap_list,to_check);
        }
    }
    
    // sprintf(string_output,"Destinations list for Volume %d step 2",N_volume);
    // print_1d_int_list(overlap_list,string_output);
    
    // 4) remove the children of volumes removed in step 2)
    int children;
    for (iterate=0;iterate<removed_under_2.num_elements;iterate++){
        for (children = 0;children < Volumes[removed_under_2.elements[iterate]]->geometry.children.num_elements;children++) {
          remove_element_in_list_by_value(&overlap_list,Volumes[removed_under_2.elements[iterate]]->geometry.children.elements[children]);
        }
    }
    
    // sprintf(string_output,"Destinations list for Volume %d step 3",N_volume);
    // print_1d_int_list(overlap_list,string_output);
    
    // 5) remove the children of N
    for (children = 0;children < Volumes[N_volume]->geometry.children.num_elements;children++) {
        remove_element_in_list_by_value(&overlap_list,Volumes[N_volume]->geometry.children.elements[children]);
    }
    
    // sprintf(string_output,"Destinations list for Volume %d step 4",N_volume);
    // print_1d_int_list(overlap_list,string_output);
    
    // 6) remove the grandparents of N
    int grandparent;
    for (grandparent = 0;grandparent < grandparent_list->num_elements;grandparent++) {
        remove_element_in_list_by_value(&overlap_list,grandparent_list->elements[grandparent]);
    }
    
    // sprintf(string_output,"Destinations list for Volume %d step 5",N_volume);
    // print_1d_int_list(overlap_list,string_output);
    
    // 7) remove volumes with lower priority than parents of N still on the list
    struct pointer_to_1d_int_list logic_list;
    logic_list.num_elements = overlap_list.num_elements;
    if (logic_list.num_elements>0) logic_list.elements = malloc(logic_list.num_elements*sizeof(int));
    for (iterate=0;iterate<logic_list.num_elements;iterate++) logic_list.elements[iterate] = 1;
    
    int parent;
    for (parent=0;parent<parent_list->num_elements;parent++) {
      if (on_int_list(overlap_list,parent_list->elements[parent])) {
        // Found a parent to N on the list, now check all other elements on the list
        for (iterate=0;iterate<overlap_list.num_elements;iterate++) {
          if (parent_list->elements[parent] != overlap_list.elements[iterate]) {
            // if the element iterate have lower priority than the parent, remove it from the list
            if (Volumes[overlap_list.elements[iterate]]->geometry.priority_value < Volumes[parent_list->elements[parent]]->geometry.priority_value) logic_list.elements[iterate] = 0;
          }
        }
      }
    }
    
    // 8) The remaing list is the destinations list
    Volumes[N_volume]->geometry.destinations_list.num_elements = sum_int_list(logic_list);
    Volumes[N_volume]->geometry.destinations_list.elements = malloc(Volumes[N_volume]->geometry.destinations_list.num_elements * sizeof(int));
    
    int overlap_index,used_elements=0;
    for (overlap_index=0;overlap_index < overlap_list.num_elements;overlap_index++)
      if (logic_list.elements[overlap_index] == 1)
        Volumes[N_volume]->geometry.destinations_list.elements[used_elements++] = overlap_list.elements[overlap_index];
    
    if (overlap_list.num_elements>0) free(overlap_list.elements);
    if (logic_list.num_elements>0) free(logic_list.elements);
    
    
    /*
    // Old version before rule 6 and 2 was added
    // 8) The remaing list is the destinations list
    Volumes[N_volume]->geometry.destinations_list.num_elements = overlap_list.num_elements;
    Volumes[N_volume]->geometry.destinations_list.elements = malloc(Volumes[N_volume]->geometry.destinations_list.num_elements * sizeof(int));
    
    int overlap_index;
    for (overlap_index=0;overlap_index < overlap_list.num_elements;overlap_index++)
        Volumes[N_volume]->geometry.destinations_list.elements[overlap_index] = overlap_list.elements[overlap_index];
    
    // sprintf(string_output,"Destination list for Volume %d step 6",N_volume);
    // print_1d_int_list(Volumes[N_volume]->geometry.destinations_list,string_output);
    
    if (overlap_list.num_elements>0) free(overlap_list.elements);
    */
    
    };

void generate_destinations_lists(struct pointer_to_1d_int_list **grandparents_lists, struct pointer_to_1d_int_list **parents_lists, struct pointer_to_1d_int_list **overlap_lists,struct Volume_struct **Volumes, int number_of_volumes, int verbal) {
    // Because of the complexity of the algortithm for generating the destinations list, the function is made for a single volume at the time to keep the notation simpler
    // This funtion runs the destinations list function for each volume
    MPI_MASTER(
    if (verbal) printf("\nGenerating destinations lists ----------------------- \n");
    )
    
    int volume_index;
    for (volume_index = 0;volume_index < number_of_volumes;volume_index++) {
      generate_destinations_list(volume_index,Volumes,*overlap_lists[volume_index],parents_lists[volume_index],grandparents_lists[volume_index]);
      
      char string_output[128];
      if (verbal) sprintf(string_output,"Destinations list for Volume %d",volume_index);
      MPI_MASTER(
      if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.destinations_list,string_output);
      )
    }
};

/* OBSOLETE
void generate_destinations_logic_lists(struct Volume_struct **Volumes,int number_of_volumes,int verbal) {
  // The destinations logic list is another way to store the destinations list that makes some tasks quicker
  
  MPI_MASTER(
  if (verbal) printf("\nGenerating destinations logic lists ----------------------- \n");
  )

  int volume_index;
  for (volume_index = 0;volume_index < number_of_volumes;volume_index++) {
      allocate_logic_list_from_temp(number_of_volumes,Volumes[volume_index]->geometry.destinations_list,&Volumes[volume_index]->geometry.destinations_logic_list);
      
      char string_output[128];
      MPI_MASTER(
      if (verbal) sprintf(string_output,"Destinations logic list for Volume %d",volume_index);
      if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.destinations_logic_list,string_output);
      )
  }

};
*/

void generate_reduced_destinations_lists(struct pointer_to_1d_int_list **parents_lists, struct Volume_struct **Volumes,int number_of_volumes,int verbal) {
    // The reduced destination list is the destination list of a volume, where each element that has a parent on the same destination list is removed
    // This list is to be fed to the which_volume function, as this funtion will automatically search through the direct children in a tree like manner
    // The optimization reduces the number of calculations of within functions in nested geometries.
    
    MPI_MASTER(
    if (verbal) printf("\nGenerating reduced destination lists ----------------------- \n");
    )
    
    struct pointer_to_1d_int_list logic_list;
  
    int volume_index,checked_dest_index,checked_dest_volume,rest_dest_index,rest_dest_volume,dest_index,iterate;
    for (volume_index = 0;volume_index < number_of_volumes;volume_index++) {
      
      //printf("Generating reduced destinations lists for volume %d\n",volume_index);
      logic_list.num_elements = Volumes[volume_index]->geometry.destinations_list.num_elements;
      logic_list.elements = malloc( (int) logic_list.num_elements * sizeof(int));
      //printf("Sucsessfully allocated space for %d integers in logic list %d\n",logic_list.num_elements,volume_index);
      for (iterate=0;iterate<logic_list.num_elements;iterate++) logic_list.elements[iterate] = 1;
      
      for (checked_dest_index=0;checked_dest_index<Volumes[volume_index]->geometry.destinations_list.num_elements;checked_dest_index++) {
            checked_dest_volume = Volumes[volume_index]->geometry.destinations_list.elements[checked_dest_index];
            for (rest_dest_index=0;rest_dest_index<Volumes[volume_index]->geometry.destinations_list.num_elements;rest_dest_index++) {
                rest_dest_volume = Volumes[volume_index]->geometry.destinations_list.elements[rest_dest_index];
                // As every volume has 0 as a parent, these are ignored. It would work to include this, but would add an extra trivial step to within_which_volume
                if (rest_dest_volume != 0) {
                    if (on_int_list(*parents_lists[checked_dest_volume],rest_dest_volume) == 1) {
                        // In this case, do not include element checked_dest_index on the reduced destinations list
                        // ADD mask check
                        if (Volumes[rest_dest_volume]->geometry.is_masked_volume == 0) {
                          logic_list.elements[checked_dest_index] = 0;
                        }
                    }
                }
            }
      }
      
      Volumes[volume_index]->geometry.reduced_destinations_list.num_elements = sum_int_list(logic_list);
      Volumes[volume_index]->geometry.reduced_destinations_list.elements = malloc((int)Volumes[volume_index]->geometry.reduced_destinations_list.num_elements * sizeof(int));
      
      iterate = 0;
      for (dest_index = 0;dest_index < Volumes[volume_index]->geometry.destinations_list.num_elements;dest_index++) {
            if (logic_list.elements[dest_index] == 1)   Volumes[volume_index]->geometry.reduced_destinations_list.elements[iterate++] = Volumes[volume_index]->geometry.destinations_list.elements[dest_index];
      }
      
      free(logic_list.elements);
      
      // Testing an optimization
      remove_element_in_list_by_value(&Volumes[volume_index]->geometry.reduced_destinations_list,0);
      
      char string_output[128];
      MPI_MASTER(
      if (verbal) sprintf(string_output,"Reduced destinations list for Volume %d",volume_index);
      if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.reduced_destinations_list,string_output);
      )
    }
};

void generate_direct_children_lists(struct pointer_to_1d_int_list **parents_lists, struct Volume_struct **Volumes,int number_of_volumes,int verbal) {

  MPI_MASTER(
  if (verbal) printf("\nGenerating direct children lists ----------------------- \n");
  )
  // A direct children of volume n is a volume that is a child of n, but no other child of n is its parent
  
  // Mask update: Need to check that this step does not bug out when the mask system interferes with child/parent systems
  
  struct pointer_to_1d_int_list logic_list;
  int volume_index,child,parent,iterate;
  for (volume_index = 0;volume_index < number_of_volumes;volume_index++) {
        // Temp elements is used, and its actual number of elements is edited even though the memory allocated is not changed. This is so that the list functions handles it correctly.
        // The free function will free all the allocated memory regardless of the value of the .num_elements structure field, it is just there for convinience.
      
        logic_list.num_elements = Volumes[volume_index]->geometry.children.num_elements;
        logic_list.elements = malloc(logic_list.num_elements * sizeof(int));
        for (iterate=0;iterate<logic_list.num_elements;iterate++) logic_list.elements[iterate] = 1;
      
        // Look through each child of volume n, and for each of those look through all the other children of n and search for one of them being a parent to the child
        for (child=0;child<Volumes[volume_index]->geometry.children.num_elements;child++) {
            for (parent=0;parent<parents_lists[Volumes[volume_index]->geometry.children.elements[child]]->num_elements;parent++) {
                if (on_int_list(Volumes[volume_index]->geometry.children,parents_lists[Volumes[volume_index]->geometry.children.elements[child]]->elements[parent]))
                    // If such a parent is found, remove that child from the list
                    logic_list.elements[child] = 0;
            }
        }
      
      Volumes[volume_index]->geometry.direct_children.num_elements = sum_int_list(logic_list);
      Volumes[volume_index]->geometry.direct_children.elements = malloc(Volumes[volume_index]->geometry.direct_children.num_elements*sizeof(int));
      
      iterate = 0;
      for (child = 0;child < Volumes[volume_index]->geometry.children.num_elements;child++) {
            if (logic_list.elements[child])   Volumes[volume_index]->geometry.direct_children.elements[iterate++] = Volumes[volume_index]->geometry.children.elements[child];
      }
      // Be careful with names in both main and a function, as they are automatically declared as external variables, which would then also free the main.
      free(logic_list.elements);
      
      char string_output[128];
      MPI_MASTER(
      if (verbal) sprintf(string_output,"Children list for Volume        %d",volume_index);
      if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.children,string_output);
      if (verbal) sprintf(string_output,"Direct_children list for Volume %d",volume_index);
      if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.direct_children,string_output);
      )
      
  }

};

void generate_starting_logic_list(struct starting_lists_struct *starting_lists, struct Volume_struct **Volumes, int number_of_volumes, int verbal) {
    // Function for generating logic list of volumes the ray can start in without an error.
    // Start with a list of all vacuum volumes
    // Remove all volumes that are children of non-vacuum volumes
    // It is still possible to have a volume on this list that is surrounded by non-vacuum volumes, but it is hard to detect these situations,
    //  meaning that it is ultimately partly the users responsibility to not send photons directly into materials.
    
    int volume_index,*start,*check;
    
    struct pointer_to_1d_int_list temp_list_local;
    temp_list_local.num_elements = number_of_volumes;
    temp_list_local.elements = malloc(number_of_volumes*sizeof(int));
    
    //if (verbal==1) printf("sucessfully allocated temp_list_local.elements, now to enter which volumes are vacuums as a logic list\n");
    temp_list_local.elements[0] = 1; // Volume 0 is a vacuum volume.
    for (volume_index = 1;volume_index < number_of_volumes;volume_index++) {
        if (Volumes[volume_index]->p_physics->is_vacuum == 1) temp_list_local.elements[volume_index] = 1;
        else temp_list_local.elements[volume_index] = 0;
    }
    // temp_list_local is now a logic list of all vacuum volumes
    //if (verbal==1) printf("list of vacuum volumes done, now to remove the children of non-vcauum volumes\n");
    for (volume_index = 1;volume_index < number_of_volumes;volume_index++) { // All volumes ...
        if (temp_list_local.elements[volume_index] == 0) { // ... that are not vacuum ...
            for (start = check = Volumes[volume_index]->geometry.children.elements;check - start < Volumes[volume_index]->geometry.children.num_elements;check++) { // ... have all their children ...
                temp_list_local.elements[*check] = 0; // .. removed from the allowed_start_logic_list
            }
        }
    }
    //if (verbal==1) printf("sucessfully removed children of non-vacuum volumes, now allocate allowed_start_logic_list\n");
    allocate_list_from_temp(number_of_volumes,temp_list_local,&starting_lists->allowed_starting_volume_logic_list);
    //if (verbal==1) printf("sucsessfully allocated allowed_start_logic_list, now freeing temp_list_local.elements. \n");
    
    free(temp_list_local.elements);
    //if (verbal==1) printf("sucessfully freed temp_list_local.elements, generate starting lists done\n");
    
    char string_output[128];
    MPI_MASTER(
    if (verbal) sprintf(string_output,"Allowed starting volume logic list");
    if (verbal) print_1d_int_list(starting_lists->allowed_starting_volume_logic_list,string_output);
    )
    
    
};

void generate_reduced_starting_destinations_list(struct starting_lists_struct *starting_lists, struct pointer_to_1d_int_list **parents_lists, struct Volume_struct **Volumes,int number_of_volumes,int verbal) {
    // The starting_destinations_list is trivial, as it contains all volumes that are not masks.


    struct pointer_to_1d_int_list logic_list;
    //printf("Generating reduced destinations lists for volume %d\n",volume_index);
    logic_list.num_elements = number_of_volumes;
    logic_list.elements = malloc( (int) logic_list.num_elements * sizeof(int));
    int iterate;
    for (iterate=0;iterate<logic_list.num_elements;iterate++) logic_list.elements[iterate] = 0;
    
    
    for (iterate=1;iterate<number_of_volumes;iterate++)
      if (Volumes[iterate]->geometry.is_mask_volume == 0) logic_list.elements[iterate] = 1;
      //else logic_list.elements[iterate] = 0;
    
    starting_lists->starting_destinations_list.num_elements = sum_int_list(logic_list);
    starting_lists->starting_destinations_list.elements = malloc(starting_lists->starting_destinations_list.num_elements*sizeof(int));
    
    int used_elements=0;
    for (iterate=1;iterate<number_of_volumes;iterate++)
      if (logic_list.elements[iterate] == 1) starting_lists->starting_destinations_list.elements[used_elements++] = iterate;
    
    MPI_MASTER(
    if (verbal) printf("\nGenerating start destinations list ------------------------------ \n");
    if (verbal) print_1d_int_list(starting_lists->starting_destinations_list,"Starting destinations list");
    )

    // The reduced starting destination list is used when a ray enters the component in the search for which volume it starts in.
    // It facilitates the same optimization as the regular destination list.
    // The start logic list is also generated, as it is very simple and does not need a seperate function
    
    // Mask update: Need to remove mask volumes from the reduced starting destination list
    
    MPI_MASTER(
    if (verbal) printf("\nGenerating reduced start destination list ----------------------- \n");
    )
    
    int checked_dest_index,checked_dest_volume,rest_dest_index,rest_dest_volume,dest_index;
    
    /* Old version before masks were introduced
    struct pointer_to_1d_int_list starting_dest_list;
    
    starting_dest_list.num_elements = number_of_volumes - 1;
    starting_dest_list.elements = malloc ( starting_dest_list.num_elements * sizeof(int));
    for (iterate=0;iterate<number_of_volumes-1;iterate++) starting_dest_list.elements[iterate] = iterate + 1; // All volumes to be checked for starting
    */
    
    logic_list.num_elements = starting_lists->starting_destinations_list.num_elements;
    free(logic_list.elements);
    logic_list.elements = malloc( (int) logic_list.num_elements * sizeof(int));
    
    //printf("Sucsessfully allocated space for %d integers in logic list %d\n",logic_list.num_elements,volume_index);
    for (iterate=0;iterate<logic_list.num_elements;iterate++) logic_list.elements[iterate] = 1;
  
    for (checked_dest_index=0;checked_dest_index<starting_lists->starting_destinations_list.num_elements;checked_dest_index++) {
        checked_dest_volume = starting_lists->starting_destinations_list.elements[checked_dest_index];
        for (rest_dest_index=0;rest_dest_index<starting_lists->starting_destinations_list.num_elements;rest_dest_index++) {
            rest_dest_volume = starting_lists->starting_destinations_list.elements[rest_dest_index];
            // As every volume has 0 as a parent, these are ignored. It would work to include this, but would add an extra trivial step to within_which_volume
            if (rest_dest_volume != 0) {
                if (on_int_list(*parents_lists[checked_dest_volume],rest_dest_volume) == 1) {
                    // In this case, do not include element checked_dest_index on the reduced destinations list
                    logic_list.elements[checked_dest_index] = 0;
                }
            }
        }
  }
  
  starting_lists->reduced_start_list.num_elements = sum_int_list(logic_list);
  starting_lists->reduced_start_list.elements = malloc((int)starting_lists->reduced_start_list.num_elements * sizeof(int));
  
  iterate = 0;
  for (dest_index = 0;dest_index < starting_lists->starting_destinations_list.num_elements;dest_index++) {
        if (logic_list.elements[dest_index] == 1) starting_lists->reduced_start_list.elements[iterate++] = starting_lists->starting_destinations_list.elements[dest_index];
  }
  
  free(logic_list.elements);
  //free(starting_dest_list.elements);
  
  MPI_MASTER(
  if (verbal) print_1d_int_list(starting_lists->reduced_start_list,"Reduced start destinations list");
  )
    
  // Making the start_logic_list.
  starting_lists->start_logic_list.num_elements = number_of_volumes;
  starting_lists->start_logic_list.elements = malloc ( starting_lists->start_logic_list.num_elements * sizeof(int));
  starting_lists->start_logic_list.elements[0] = 0;
  for (iterate=1;iterate<number_of_volumes;iterate++) starting_lists->start_logic_list.elements[iterate] = 1; // All volumes to be checked for starting volume
  MPI_MASTER(
  if (verbal) print_1d_int_list(starting_lists->start_logic_list,"Start logic list");
  )
};


void generate_next_volume_list(struct Volume_struct **Volumes, int number_of_volumes, int verbal) {
    // Generate list of volumes that can be the next volume which the ray enters. It is used for tagging, not the simulation / propagation

    // Mask update: These lists should be done as if all mask statuses are on, meaning they include all possible next volumes (will include more input to this function)
    
    // bug: next volume list is not complete and contains duplicates

    MPI_MASTER(
    if (verbal) printf("\nGenerating next volume list ------------------------------------- \n");
    )
    // Merge destinations list, intersection list and mask_intersection_list
    int volume_index,iterate,mask_index;
    struct pointer_to_1d_int_list full_intersection_list;
    full_intersection_list.num_elements=0;
    
    for (volume_index=0;volume_index<number_of_volumes;volume_index++) {
        Volumes[volume_index]->geometry.next_volume_list.num_elements = 0;
        // Before mask update
        //merge_lists(&Volumes[volume_index]->geometry.next_volume_list, &Volumes[volume_index]->geometry.destinations_list, &Volumes[volume_index]->geometry.intersect_check_list);
        
        merge_lists(&full_intersection_list, &Volumes[volume_index]->geometry.mask_intersect_list, &Volumes[volume_index]->geometry.intersect_check_list);
        merge_lists(&Volumes[volume_index]->geometry.next_volume_list,&Volumes[volume_index]->geometry.destinations_list,&full_intersection_list);
        
        /* // This complication is taken into account by adding the mask_intersect list instead
        // It is possible that the next volume is still not on this list, as when masks are encountered the next volume can be a volume they mask.
        // For each on the list, add the volumes masked by that mask (do not iterate over this, as masks can not be masked regardless)
        for (iterate=Volumes[volume_index]->geometry.next_volume_list.num_elements-1;iterate>-1;iterate--) {
          if (Volumes[Volumes[volume_index]->geometry.next_volume_list.elements[iterate]]->geometry.is_mask_volume == 1) {
            for (mask_index=0;mask_index<Volumes[Volumes[volume_index]->geometry.next_volume_list.elements[iterate]]->geometry.mask_list.num_elements;mask_index++) {
              add_element_to_int_list(&Volumes[volume_index]->geometry.next_volume_list,Volumes[Volumes[volume_index]->geometry.next_volume_list.elements[iterate]]->geometry.mask_list.elements[mask_index]);
            }
          }
        }
        */
        
        // Remove mask volumes from the next volume list, as they never occur as current_volume, and thus just create dead branches that takes up memory
        for (iterate=Volumes[volume_index]->geometry.next_volume_list.num_elements-1;iterate>-1;iterate--)
          if (Volumes[Volumes[volume_index]->geometry.next_volume_list.elements[iterate]]->geometry.is_mask_volume == 1)
            remove_element_in_list_by_index(&Volumes[volume_index]->geometry.next_volume_list,iterate);
        
        
        if (full_intersection_list.num_elements>0) free(full_intersection_list.elements);
        full_intersection_list.num_elements=0;
        
        char string_output[128];
        MPI_MASTER(
        if (verbal) sprintf(string_output,"Next volume list                %d",volume_index);
        if (verbal) print_1d_int_list(Volumes[volume_index]->geometry.next_volume_list,string_output);
        )
    }
};

void generate_lists(struct Volume_struct **Volumes, struct starting_lists_struct *starting_lists, int number_of_volumes, int verbal) {
    // Function to control the generation of lists
    // Some lists are only needed temporary, and are thus declared here to keep them out of the main scope
    // Others are stored in the volume structs as they are needed in the trace algorithm (or tagging)
    
    
    struct pointer_to_1d_int_list **true_children_lists;
    true_children_lists = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    // generate_children_lists both generate the normal children list for each volume, but also the true children list needed locally.
    generate_children_lists(Volumes, true_children_lists, number_of_volumes,verbal);
    
    
    struct pointer_to_1d_int_list **true_overlap_lists;
    true_overlap_lists = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    struct pointer_to_1d_int_list **raw_overlap_lists;
    raw_overlap_lists = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    
    generate_overlap_lists(true_overlap_lists, raw_overlap_lists, Volumes,number_of_volumes,verbal);
    
    
    //generate_intersect_check_lists(true_overlap_lists, Volumes, number_of_volumes, verbal);
    
    
    struct pointer_to_1d_int_list **parents_lists;
    parents_lists = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    generate_parents_lists(parents_lists,Volumes,number_of_volumes,verbal,1); // The last 1 means masks are taken into account
    
    // Generate version of parent list as it would be without masks
    struct pointer_to_1d_int_list **parents_lists_no_masks;
    parents_lists_no_masks = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    generate_parents_lists(parents_lists_no_masks,Volumes,number_of_volumes,verbal,0); // The last 0 means masks are NOT taken into account
    
    // Generate version of parent list using true_children instead
    struct pointer_to_1d_int_list **true_parents_lists;
    true_parents_lists = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    generate_true_parents_lists(true_parents_lists, true_children_lists, Volumes, number_of_volumes, verbal, 1);
    
    // Generate version of parent list no masks using true_children instead
    struct pointer_to_1d_int_list **true_parents_lists_no_masks;
    true_parents_lists_no_masks = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    generate_true_parents_lists(true_parents_lists_no_masks, true_children_lists, Volumes, number_of_volumes, verbal, 0);
    
    // New version of generate intersect lists
    generate_intersect_check_lists_experimental(true_overlap_lists, raw_overlap_lists, parents_lists, true_parents_lists, Volumes, number_of_volumes, verbal);
    
    struct pointer_to_1d_int_list **grandparents_lists;
    grandparents_lists = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    generate_grandparents_lists(grandparents_lists,parents_lists,number_of_volumes,verbal);
    
    // Generate version of grandparents list as it would have been if no masks were defined
    struct pointer_to_1d_int_list **grandparents_lists_no_masks;
    grandparents_lists_no_masks = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    generate_grandparents_lists(grandparents_lists_no_masks,parents_lists_no_masks,number_of_volumes,verbal);
    
    // Generate true_grandparents_lists
    struct pointer_to_1d_int_list **true_grandparents_lists;
    true_grandparents_lists = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    generate_grandparents_lists(true_grandparents_lists,true_parents_lists,number_of_volumes,verbal);
    
    struct pointer_to_1d_int_list **true_grandparents_lists_no_masks;
    true_grandparents_lists_no_masks = malloc(number_of_volumes*sizeof(struct pointer_to_1d_int_list));
    generate_grandparents_lists(true_grandparents_lists_no_masks,true_parents_lists_no_masks,number_of_volumes,verbal);
    
    // The destinations lists are generated without taking masks into account (they are removed from the overlap list in an early step)
    //generate_destinations_lists(grandparents_lists_no_masks,parents_lists_no_masks,true_overlap_lists,Volumes,number_of_volumes,verbal);
    
    generate_destinations_lists_experimental(true_overlap_lists, true_children_lists, true_parents_lists_no_masks, true_grandparents_lists_no_masks, Volumes, number_of_volumes, verbal);
    
    // Obsolete, found a way around them in within_which_volume, but need to test the performance difference
    // generate_destinations_logic_lists(Volumes,number_of_volumes,verbal);
    
    generate_reduced_destinations_lists(parents_lists,Volumes,number_of_volumes,verbal);
    
    generate_direct_children_lists(parents_lists,Volumes,number_of_volumes,verbal);
    
    //generate_starting_list(starting_lists,Volumes,number_of_volumes,verbal);
    generate_starting_logic_list(starting_lists,Volumes,number_of_volumes,verbal);
    
    generate_reduced_starting_destinations_list(starting_lists,parents_lists,Volumes,number_of_volumes,verbal);
    
    // This list is stored with the volumes for convinience, but is only used for tagging
    generate_next_volume_list(Volumes,number_of_volumes,verbal);
    
    // Garbage collection for temporary dynamically allocated lists. (Permanent lists freed from FINALLY)
    int iterate;
    for (iterate=0;iterate<number_of_volumes;iterate++) {
        //printf("freeing for volume nr %d\n",iterate);
        //printf("true_overlap_lists[iterate]->num_elements = %d \n",true_overlap_lists[iterate]->num_elements);
        if (true_overlap_lists[iterate]->num_elements > 0) free(true_overlap_lists[iterate]->elements);
        
        //printf("raw_overlap_lists[iterate]->num_elements = %d \n",raw_overlap_lists[iterate]->num_elements);
        if (raw_overlap_lists[iterate]->num_elements > 0) free(raw_overlap_lists[iterate]->elements);
        
        //printf("parents_lists[iterate]->num_elements = %d \n",parents_lists[iterate]->num_elements);
        if (parents_lists[iterate]->num_elements > 0) free(parents_lists[iterate]->elements);
        
        //printf("parents_lists_no_masks[iterate]->num_elements = %d \n",parents_lists_no_masks[iterate]->num_elements);
        if (parents_lists_no_masks[iterate]->num_elements > 0) free(parents_lists_no_masks[iterate]->elements);
        
        //printf("true_parents_lists[iterate]->num_elements = %d \n",true_parents_lists[iterate]->num_elements);
        if (true_parents_lists[iterate]->num_elements > 0) free(true_parents_lists[iterate]->elements);
        
        //printf("true_parents_lists_no_masks[iterate]->num_elements = %d \n",true_parents_lists_no_masks[iterate]->num_elements);
        if (true_parents_lists_no_masks[iterate]->num_elements > 0) free(true_parents_lists_no_masks[iterate]->elements);
        
        //printf("grandparents_lists[iterate]->num_elements = %d \n",grandparents_lists[iterate]->num_elements);
        if (grandparents_lists[iterate]->num_elements > 0) free(grandparents_lists[iterate]->elements);
        
        //printf("true_grandparents_lists[iterate]->num_elements = %d \n",true_grandparents_lists[iterate]->num_elements);
        if (true_grandparents_lists[iterate]->num_elements > 0) free(true_grandparents_lists[iterate]->elements);
        
        //printf("grandparents_lists_no_masks[iterate]->num_elements = %d \n",grandparents_lists_no_masks[iterate]->num_elements);
        if (grandparents_lists_no_masks[iterate]->num_elements > 0) free(grandparents_lists_no_masks[iterate]->elements);
        
        //printf("true_grandparents_lists_no_masks[iterate]->num_elements = %d \n",true_grandparents_lists_no_masks[iterate]->num_elements);
        if (true_grandparents_lists_no_masks[iterate]->num_elements > 0) free(true_grandparents_lists_no_masks[iterate]->elements);
        
        //printf("true_children_lists[iterate]->num_elements = %d \n",true_children_lists[iterate]->num_elements);
        if (true_children_lists[iterate]->num_elements > 0) free(true_children_lists[iterate]->elements);
    }
    //printf("generate lists volume specific free completed\n");
    free(true_overlap_lists);free(raw_overlap_lists);free(parents_lists);free(true_parents_lists);free(true_parents_lists_no_masks);
    free(parents_lists_no_masks);free(true_grandparents_lists);free(grandparents_lists);free(grandparents_lists_no_masks);free(true_grandparents_lists_no_masks);
    //printf("generate lists free completed\n");
};

// -------------    Focusing functions   --------------------------------------------------------

// The focusing_data structure is set up by the geometry component, and a pointer to the appropriate
//  focusing function is added to the Volume structure (for this reason the input of all the functions
//  need to be identical, at least in terms of types). In this way there are no if statements to check
//  which of these to be used in the trace, but the focus_data_struct will carry some redundant
//  information, as only the appropriate parameters are set:
// Angular focus on a rectangle (angular_focus_height / angular_focus_width)
// Spatial focus on a rectangle (spatial_focus_height / spatial_focus_width)
// Spatial focus on a disk (sptial_focus_radius)
// No focus (randvec in 4pi) (all set to zero, will select randvec circle as it is slightly faster
//
// When adding a new physical process focusing becomes very easy, as one just calls the master focusing
//  function assosiated with the volume (placed in the geometry struct), using the focus_data_struct
//  also found in the geometry struct, and the process then supports all the focusing modes. It is even
//  possible to add new focusing modes in the future by updating just the geometry components, and this
//  section.

// focus_data_struct definitioon shown here, defined at the start of this file
//struct focus_data_struct {
//Coords Aim;
//double angular_focus_width;
//double angular_focus_height;
//double spatial_focus_width;
//double spatial_focus_height;
//double spatial_focus_radius;
//Rotation absolute_rotation;
//// focusing_function creates a vector per selected criteria of focus_data_struct / selected focus function and returns solid angle
//void (*focusing_function)(Coords*, double*, struct focus_data_struct*);
////                        v_out  , solid_a,
//};

void randvec_target_rect_angular_union(Coords *v_out,double *solid_angle_out, struct focus_data_struct *focus_data) {
    // Calls the standard McXtrace randvec_target_rect_angular focusing function, but is with the new data input format.
    randvec_target_rect_angular(&v_out->x, &v_out->y, &v_out->z, solid_angle_out, focus_data->Aim.x,focus_data->Aim.y, focus_data->Aim.z, focus_data->angular_focus_width, focus_data->angular_focus_height,focus_data->absolute_rotation);
    //randvec_target_rect_angular(&vx, &vy, &vz, &solid_angle,aim_x, aim_y, aim_z, VarsInc.aw, VarsInc.ah, ROT_A_CURRENT_COMP);
};

void randvec_target_rect_union(Coords *v_out,double *solid_angle_out, struct focus_data_struct *focus_data) {
// Calls the standard McXtrace randvec_target_rect focusing function, but is with the new data input format.
    randvec_target_rect(&v_out->x, &v_out->y, &v_out->z, solid_angle_out, focus_data->Aim.x,focus_data->Aim.y, focus_data->Aim.z, focus_data->spatial_focus_width, focus_data->spatial_focus_height,focus_data->absolute_rotation);
    // randvec_target_rect(&vx, &vy, &vz, &solid_angle,aim_x, aim_y, aim_z, VarsInc.xw, VarsInc.yh, ROT_A_CURRENT_COMP);
};

void randvec_target_circle_union(Coords *v_out,double *solid_angle_out, struct focus_data_struct *focus_data) {
// Calls the standard McXtrace randvec_target_circle focusing function, but is with the new data input format.

    // debug input into randvec_target_circle
    //print_position(focus_data->Aim,"Aim vector input for randvec_target_circle");
    //printf("Radius input %f\n",focus_data->spatial_focus_radius);

    randvec_target_circle(&v_out->x, &v_out->y, &v_out->z, solid_angle_out, focus_data->Aim.x,focus_data->Aim.y, focus_data->Aim.z, focus_data->spatial_focus_radius);
    //randvec_target_circle(&vx, &vy, &vz, &solid_angle, aim_x, aim_y, aim_z, focus_r);
};



void focus_initialize(struct geometry_struct *geometry, Coords POS_A_TARGET, Coords POS_A_CURRENT, Rotation ROT_A_CURRENT, int target_index, double target_x, double target_y, double target_z, double angular_focus_width, double angular_focus_height, double spatial_focus_width, double spatial_focus_height, double spatial_focus_radius, char *component_name) {
    // Initialize focusing system
    // target_x/y/z needs to be double setting parameters with default value 0
    // target_index needs to be int setting parameter with default value 0
    // angular_focus_width, angular_focus_height, spatial_focus_width, spatial_focus_height, spatial_focus_radius nneds to be double setting parameters with default value 0
    // When those conditions are met, this code will identify which settings have been entered by the user and select the appropriate focusing parameters, which are loaded into the focus_data struct and the geometry struct.
    // The aim vector in the focus_data struct will be transformed from the local coordinate system of the geometry component to the coordinate system of the master component during the master component initialize

  // Input sanitation
  if (angular_focus_width < 0) {
    printf("\nERROR in Union geometry component named \"%s\", angular focus width focus_aw < 0! \n",component_name);
    exit(1);
  }
  if (angular_focus_height < 0) {
    printf("\nERROR in Union geometry component named \"%s\", angular focus width focus_ah < 0! \n",component_name);
    exit(1);
  }
  if (spatial_focus_width < 0) {
    printf("\nERROR in Union geometry component named \"%s\", spatial focus width focus_xw < 0! \n",component_name);
    exit(1);
  }
  if (spatial_focus_height < 0) {
    printf("\nERROR in Union geometry component named \"%s\", spatial focus height focus_xh < 0! \n",component_name);
    exit(1);
  }
  if (spatial_focus_radius < 0) {
    printf("\nERROR in Union geometry component named \"%s\", spatial focus radius focus_r < 0! \n",component_name);
    exit(1);
  }
  
  struct focus_data_struct focus_data;

  // Initialize focus_data_struct
  /*
  geometry->focus_data.Aim = coords_set(0,0,0);
  geometry->focus_data.angular_focus_width = 0;
  geometry->focus_data.angular_focus_height = 0;
  geometry->focus_data.spatial_focus_width = 0;
  geometry->focus_data.spatial_focus_height = 0;
  geometry->focus_data.spatial_focus_radius = 0;
  rot_copy(geometry->focus_data.absolute_rotation,ROT_A_CURRENT);
  */
  focus_data.Aim = coords_set(0,0,0);
  focus_data.angular_focus_width = 0;
  focus_data.angular_focus_height = 0;
  focus_data.spatial_focus_width = 0;
  focus_data.spatial_focus_height = 0;
  focus_data.spatial_focus_radius = 0;
  rot_copy(focus_data.absolute_rotation,ROT_A_CURRENT);

  // Built on code from Incoherent.comp by Kim Lefmann and Kristian Nielsen
  if (target_index != 0 && !target_x && !target_y && !target_z)
  {
    Coords ToTarget;
    //ToTarget = coords_sub(POS_A_COMP_INDEX(INDEX_CURRENT_COMP+target_index),POS_A_CURRENT_COMP);
    ToTarget = coords_sub(POS_A_TARGET,POS_A_CURRENT);
    //ToTarget = rot_apply(ROT_A_CURRENT_COMP, ToTarget);
    ToTarget = rot_apply(ROT_A_CURRENT, ToTarget);
    coords_get(ToTarget, &focus_data.Aim.x, &focus_data.Aim.y, &focus_data.Aim.z);
  }
  else
  { focus_data.Aim.x = target_x; focus_data.Aim.y = target_y; focus_data.Aim.z = target_z; }
  
  if (!(focus_data.Aim.x || focus_data.Aim.y || focus_data.Aim.z)) {
    // Somehow set a variable to signify scattering into 4pi
    // printf("Union %s: The target is not defined. Using scattering into 4pi.\n",NAME_CURRENT_COMP);
    focus_data.Aim.z=1; // set aim to one so that the randvec output vector has length 1 instead of 0
  }

  int focusing_model_selected = 0;
  if (angular_focus_width != 0 && angular_focus_height != 0) {
    focus_data.focusing_function = &randvec_target_rect_angular_union;
    focus_data.angular_focus_width = DEG2RAD*angular_focus_width; // Convert to radians here
    focus_data.angular_focus_height = DEG2RAD*angular_focus_height;
    focusing_model_selected = 1;
  }
  if (spatial_focus_width != 0 && spatial_focus_height != 0) {
    focus_data.focusing_function = &randvec_target_rect_union;
    focus_data.spatial_focus_width = spatial_focus_width;
    focus_data.spatial_focus_height = spatial_focus_height;
    if (focusing_model_selected) {
        printf("ERROR %s: Select either angular or spatial focusing, not both! Exiting \n",component_name);
        exit(1);
    }
    focusing_model_selected = 1;
  }
  if (spatial_focus_radius != 0) {
    focus_data.focusing_function = &randvec_target_circle_union;
    focus_data.spatial_focus_radius = spatial_focus_radius;
    if (focusing_model_selected) {
        printf("ERROR %s: Select a maximum of one focusing method (spatial rectangle or cicle, or angular rectangle! Exiting \n",component_name);
        exit(1);
    }
    focusing_model_selected = 1;
  }
  if (focusing_model_selected == 0) {
    // Select 4pi focusing
    focus_data.spatial_focus_radius = 0;
    focus_data.focusing_function = &randvec_target_circle_union;
  }
  
  // Allocate the isotropic focus_data struct
  geometry->focus_data_array.num_elements = 0;
  //geometry->focus_data_array.elements = malloc(sizeof(focus_data_struct));
  add_element_to_focus_data_array(&geometry->focus_data_array,focus_data);
};

