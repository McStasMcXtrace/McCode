/****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2003, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* In library: share/supermirror.h
* Depends on "polyhedron" (which depends on "plane")
*
* Usage: within SHARE
* %include "supermirror"
*
* Functions: 
* InitialiseStdSupermirrorFlat              Initialisation function
* InitialiseStdSupermirrorFlat_detail       Detail initialisation function
* IntersectStdSupermirrorFlat               Obtain the next intersect time and position, useful in multi-supermirror device
* StdSupermirrorFlat                        Ray-tracing function 
* EmptySupermirrorFlatData					Free up memories at the end of a McStas run
*
* %I
* Written by: Wai Tung Lee
* Date: September 2024
* Origin: ESS
*
* %D
* Calculate the neutron reflection and transmission of a flat supermirror with parallel mirror surfaces.
* The following can be specified: 
* 1. reflection from either mirror coating or substrate surface, mirror coating can be single-side, double-side, 
* 2. absorber coating:  beneath mirror coating, single-side coated, double-side coated, or uncoated,
* 3. refraction and total reflection at substrate surface,
* 4. attenuation and internal reflection inside substrate. 
*
* note: supermirror name is case-sensitive 
*       text entries of parameters below are not sensitive to case
*        default regional spelling is UK, but some paramters accept other reginal spelling
*
*****************************************************************************
* INITIALISE PARAMETERS
*****************************************************************************
* 
* STEP 1: Specify supermirror shape, supermirror coating, absorber and substrate material
*         In this step, supermirror is standing vertically, mirror coating = yz plane with long side along +z.
*
* SUPERMIRROR SHAPE -------------------------------------------------
* 1. +x: "top" mirror surface normal, horizontal transverse to beam; 
*    +y: vertical up, parallel to  mirror surface; 
*    +z: longitudinal to beam, parallel to mirror surface; 
* 2. top and bottom mirrors' surface normals are +x and -x, respectively; 
* 3. entrance-side edge normal is -z, exit-side edge normal is +z (beam direction);
* 3. normals of and points on the remaining edge surfaces at +y and -y are user-specified, 
*
* Mirror shape is specified by length, thickness, and the normals of and points on the edge surfaces at +y and -y sides. 
* 
* BELOW: APPLY WHEN USING FUNCTION InitialiseStdSupermirrorFlat
* Parameters: 
* length:              [m]        Supermirror length 
* thickness_in_mm:     [mm]       Supermirror thickness in mm
* Below - the two side-edges at +y and -y are taken to be symmetric about the horizontal xz-plane, only one needs to be specified. 
* Use InitialiseStdSupermirrorFlat_detail if not symmetric.
* side_edge_normal:    [1,1,1]    Normal vector of one of the two side-edge surface, use Coords structure, doesn't need to be normalised, default (0,1,0)
* side_edge_point:     [m,m,m]    A point on one of the two side-edge surface, use Coords structure
* 
* BELOW: APPLY WHEN USING FUNCTION InitialiseStdSupermirrorFlat_detail
* Parameters: 
* length:              [m]        Supermirror length 
* thickness_in_mm:     [mm]       Supermirror thickness in mm
* side_edge_1_normal:  [1,1,1]    Normal vector of 1st side-edge surface, use Coords structure, doesn't need to be normalised
* side_edge_1_point:   [m,m,m]    A point on 1st side-edge surface, use Coords structure
* side_edge_2_normal:  [1,1,1]    Normal vector of 2nd side-edge surface, use Coords structure, doesn't need to be normalised
* side_edge_2_point:   [m,m,m]    A point on 2nd side-edge surface, use Coords structure
* 
* 
* SUPERMIRROR COATING -------------------------------------------------
* Mirror reflectivity parameters are either specified by name or by 6 parameters {R0, Qc, alpha, m, W, beta}. 
* R0:      [1]      reflectivity below critical scattering vector
* Qc:      [AA-1]   magnetude of critical scattering vector (at m=1)
* alpha:   [AA]     slope of reflectivity
* m:       [1]      m-value of material. Zero means no reflection
* W:       [AA-1]   width of reflectivity cut-off
* beta:    [AA2]    curvature of reflectivity
* If name == "SubstrateSurface", reflectivity is calculated by 
*                   R = R0 (when q<=Qc), R0*( (q - (q^2 - Qc^2)^1/2) / (q + (q^2 - Qc^2)^1/2) )^2 (when q>Qc), with Qc=sqrt(16 Pi SLD). 
* with Qc = Qc_sub defined in SubstrateSurfaceParameters and SLD defined in SubstrateParameters,  
* otherwise reflectivity is calculated by 
*                   R = R0 (when q<=Qc), R = R0*0.5*(1-tanh((q - m*Qc)/W))*(1-alpha*(q-Qc)+beta*(q-Qc)*(q-Qc)) (when q>Qc).
* 
* BELOW: APPLY WHEN USING FUNCTION InitialiseStdSupermirrorFlat
* mirror material name either matches one of those defined in "Supermirror_reflective_coating_materials.txt", or use "SubstrateSurface", "NoReflection"
* If the mirror is non-polarising, specify the same name and m-value for spin plus and spin minus coating parameters. 
* Parameters: 
* mirror_coated_side:              [string]  Sequential keywords combinations of position and surface property.
*											 position: "Both", "Top", "Bottom";
*											 surface property: "Coated", "SubstrateSurface", "NoReflection"; Note: "NotCoated"="Empty"="SubstrateSurface" 
*											 e.g. "BothCoated", "BottomCoatedTopSubstrate",
*											 case-insensitive.
* mirror_spin_plus_material_name:  [string]  mirror spin+ material name in "Supermirror_reflective_coating_materials.txt". (case-insensitive)
* mirror_spin_plus_m:              [1]       mirror spin+ m-value
* mirror_spin_minus_material_name: [string]  mirror spin- material name in "Supermirror_reflective_coating_materials.txt". (case-insensitive)
* mirror_spin_minus_m:             [1]       mirror spin- m-value
*
* BELOW: APPLY WHEN USING FUNCTION InitialiseStdSupermirrorFlat_detail
* specify either 
*          mirror material name matching one of those defined in "Supermirror_reflective_coating_materials.txt" or "SubstrateSurface" or "NoReflection" or 
*          6 reflectivity parameters {R0, Qc, alpha, m, W, beta}.
* correctly specified mirror material name overrides reflectivity parameters passed to the function, otherwise the latter are used.  
* If the mirror is non-polarising, specify the same name and parameters for spin plus and spin minus. 
* Parameters: 
* mirror_top_spin_plus_material_name:     [string]               top mirror spin+ material name in "Supermirror_reflective_coating_materials.txt", 0 if not specified
*                                                                can also be "SubstrateSurface" or "NoReflection"
* mirror_top_spin_plus_material_spec:     [1,AA-1,AA,1,AA-1,AA2] top mirror spin+ reflectivity parameters in an array of 6, 0 if not specified 
* mirror_top_spin_plus_m:                 [1]                    top mirror spin+ m-value
* mirror_top_spin_minus_material_name:    [string]               top mirror spin- material name in "Supermirror_reflective_coating_materials.txt", 0 if not specified
*                                                                can also be "SubstrateSurface" or "NoReflection"
* mirror_top_spin_minus_material_spec:    [1,AA-1,AA,1,AA-1,AA2] top mirror spin- reflectivity parameters in an array of 6, 0 if not specified 
* mirror_top_spin_minus_m:                [1]                    top mirror spin- m-value
* mirror_bottom_spin_plus_material_name:  [string]               bottom mirror spin+ material name in "Supermirror_reflective_coating_materials.txt", 0 if not specified
*                                                                can also be "SubstrateSurface" or "NoReflection"
* mirror_bottom_spin_plus_material_spec:  [1,AA-1,AA,1,AA-1,AA2] bottom mirror spin+ reflectivity parameters in an array of 6, 0 if not specified 
* mirror_bottom_spin_plus_m:              [1]                    bottom mirror spin+ m-value
* mirror_bottom_spin_minus_material_name: [string]               bottom mirror spin- material name in "Supermirror_reflective_coating_materials.txt", 0 if not specified
*                                                                can also be "SubstrateSurface" or "NoReflection"
* mirror_bottom_spin_minus_material_spec: [1,AA-1,AA,1,AA-1,AA2] bottom mirror spin- reflectivity parameters in an array of 6, 0 if not specified 
* mirror_bottom_spin_minus_m:             [1]                    bottom mirror spin- m-value
*
*
* ABSORBER LAYER -------------------------------------------------
* Absorber parameters are either specified by name or by parameters L_abs, L_inc and the coating thickness. 
* L_abs:                         [cm]          absorption 1/e attenuation length for 1 Å neutrons (complete & immediate absorption: 0, no absorption: -1)
* L_inc:                         [cm]          incoherent scattering 1/e attenuation length (complete & immediate attenuation: 0, no attenuation: -1)
* absorber_thickness_in_micron:  [micrometer]  absorber coating thickness in micrometer
* 
* BELOW: APPLY WHEN USING FUNCTION InitialiseStdSupermirrorFlat
* If there is absorber coating on both sides, they have the same material and thickness. If they are different, use InitialiseStdSupermirrorFlat_detail instead.
* Parameter: 
* absorber_coated_side:         [string]      "BothNotCoated", "BothCoated", "TopCoated", "BottomCoated".
* absorber_material_name:       [string]      absorber material name in "Supermirror_absorber_coating_materials.txt" or "Empty", 0="Empty",
* absorber_thickness_in_micron: [micrometer]  absorber coating thickness in micrometer
* 
* BELOW: APPLY WHEN USING FUNCTION InitialiseStdSupermirrorFlat_detail
* specify either 
*          absorber material name matching one of those defined in "Supermirror_absorber_coating_materials.txt" or "Empty", or 
*          2 absorber parameters {L_abs, L_inc}
* correctly specified absorber material name overrides absorber parameter passed to the function, otherwise the latter is used.  
* Parameters: 
* absorber_top_material_name:           [string]      Top absorber material name in "Supermirror_absorber_coating_materials.txt" or "Empty", 0=use parametric values below.
* absorber_top_L_abs:                   [cm]          Top absorber L_abs (complete & immediate absorption: 0, no absorption: -1)
* absorber_top_L_inc:                   [cm]          Top absorber L_inc (complete & immediate attenuation: 0, no attenuation: -1)
* absorber_top_thickness_in_micron:     [micrometer]  Top absorber thickness in microns/micrometers
* absorber_bottom_material_name:        [string]      Bottom absorber material name in "Supermirror_absorber_coating_materials.txt" or "Empty", 0=use parametric values below.
* absorber_bottom_L_abs:                [cm]          Bottom absorber L_abs (complete & immediate absorption: 0, no absorption: -1)
* absorber_bottom_L_inc:                [cm]          Bottom absorber L_inc (complete & immediate attenuation: 0, no attenuation: -1)
* absorber_bottom_thickness_in_micron:  [micrometer]  Bottom absorber thickness in microns/micrometers
*
*
* SUBSTRATE -------------------------------------------------
* Substrate parameters are either specified by name or by 3 parameters {L_abs, L_inc, SLD}. 
* L_abs:   [cm]     absorption 1/e attenuation length for 1 Å neutrons (complete & immediate absorption: 0, no absorption: -1)
* L_inc:   [cm]     spin-incoherent scattering 1/e attenuation length  (complete & immediate attenuation: 0, no attenuation: -1)
* SLD:     [AA-2]   Coherent scattering length density
* 
* BELOW: APPLY WHEN USING FUNCTION InitialiseStdSupermirrorFlat
* substrate_material_name: [string]  substrate material name in "Supermirror_substrate_materials.txt" or "Empty", 0="Empty".
*
* BELOW: APPLY WHEN USING FUNCTION InitialiseStdSupermirrorFlat_detail
* specify either 
*          substrate material name 
*          3 substrate parameters {L_abs, L_inc, SLD}
* if given and found in the file, substrate material name overrides substrate parameters passed to the function, otherwise the latter are used.  
* substrate_material_name: [string] substrate material name in "Supermirror_substrate_materials.txt" or "Empty", 0=use parametric values below.
* substrate_L_abs:         [cm]     substrate L_abs (complete & immediate absorption: 0, no absorption: -1)
* substrate_L_inc:         [cm]     substrate L_inc (complete & immediate attenuation: 0, no attenuation: -1)
* substrate_SLD:           [AA-2]   substrate SLD
*
*
*****************************************************************************
* STEP 2: Orient and position the as-defined supermirror in the McStas module XYZ coordinates. 
*
* 1. Specify initially a location on the front side of the mirror (see parameter "initial_placement_at_origin" below). 
*    The supermirror is shifted so that the selected point coincides with the origin of the McStas module XYZ coordinates
* 2. Then the orientation and position of the supermirror are specified by the movements in sequence as
*    1st, tilting about an axis in +y direction at a selected location (see parameters "tilt_y_axis_location" and "tilt_about_y_first_in_degree" below), 
*    2nd, translation, 
*    3rd, rotation about the z-axis of the McStas module XYZ coordinates.
* Parameters: 
* initial_placement_at_module_origin:	[string] "TopFrontEdgeCentre","FrontSubstrateCentre","BottomFrontEdgeCentre" 
*												 (insensitive to case and reginal English spelling)
* tilt_y_axis_location:					[string] "TopFrontEdge","TopMirrorCentre","TopBackEdge"
*												 "FrontSubstrateCentre","SubstrateCentre","BackSubstrateCentre", 
*												 "BottomFrontEdge","BottomMirrorCentre","BottomBackEdge" 
*												 (insensitive to case and reginal English spelling)
* tilt_about_y_first_in_degree:			[°]		 First: tilt about the y-axis at its selected location
* translation_second:					[m,m,m]	 Second: translate, use Coords structure
* rot_about_z_third_in_degree			[°]		 Third: rotate about z-axis (beam axis) of the McStas module XYZ coordinates 
*
*
* 
*****************************************************************************
* OUTPUT: 
* struct Supermirror with all parameter values entered and initialised
* User declares "Supermirror supermirror;" or its equivalence, 
* passes pointer "&supermirror" to function.
* Parameter: 
* sm:	[struct Supermirror]  Supermirror structure
*
* 
*****************************************************************************
* End INITIALISE PARAMETERS
*****************************************************************************
*
* 
*****************************************************************************
* RAY-TRACING PARAMETERS: 
*****************************************************************************
*
* FUNCTION IntersectStdSupermirrorFlat 
* INPUT: 
* 	neutron parameters: w_sm=p, t_sm=t, p_sm=coords_set(x,y,z), v_sm=coords_set(vx,vy,vz), s_sm=coords_set(sx,sy,sz)
* 	last_exit_time			[s]       time of last exit from a supermirror, use F_INDETERMINED if not determined. (F_INDETERMINED defined in this file)
* 	last_exit_point			[m,m,m]   position of last exit from a supermirror, use coords_set(F_INDETERMINED,F_INDETERMINED,F_INDETERMINED) if not determined.
* 	last_exit_plane			[1]       plane of last exit from a supermirror, use I_INDETERMINEDif not determined.
* 	sm:						[struct]  Supermirror structure
* OUTPUT: 
* 	num_intersect:          [1]		number of intersects through supermirror
* 	First intersect time, point, plane if there is intersect.
* 	User declare one or more parameters, e.g. "int num_intersect; double first_intersect_time; Coords first_intersect_point; int first_intersect_plane;", 
* 	then passes pointers "&num_intersect, &first_intersect_time, &first_intersect_point, &first_intersect_plane" to function.
* 	Pass 0 as pointer if not needed.
* 	first_intersect_dtime:  [s]		time difference from neutron to first intersect
* 	first_intersect_dpoint: [m,m,m]	position difference from neutron to point of first intersect
* 	first_intersect_time:   [s]		time of first intersect
* 	first_intersect_point:  [m,m,m]	point of first intersect
* 	first_intersect_plane:  [1]		plane of first intersect
* RETRUN: 
* 	sm_Intersected, sm_Missed, sm_Error
* 		
* 
* FUNCTION StdSupermirrorFlat
* INPUT:
* 	sm						[struct]  Supermirror structure
* INPUT & OUTPUT: 
* 	neutron parameters: w_sm=p, t_sm=t, p_sm=coords_set(x,y,z), v_sm=coords_set(vx,vy,vz), s_sm=coords_set(sx,sy,sz)
* 	last_exit_time			[s]       time of last exit from a supermirror, use F_INDETERMINED if not determined. (F_INDETERMINED defined in this file)
* 	last_exit_point			[m,m,m]   position of last exit from a supermirror, use coords_set(F_INDETERMINED,F_INDETERMINED,F_INDETERMINED) if not determined.
* 	last_exit_plane			[1]       plane of last exit from a supermirror, use I_INDETERMINEDif not determined.
* RETRUN: 
* 	sm_Exited, sm_Absorbed, sm_Error
* 
* 
*****************************************************************************
* End RAY-TRACING PARAMETERS 
*****************************************************************************
*
*
*****************************************************************************
* struct Supermirror: Stores a supermirror's geometry, material, and computation process parameters.
*****************************************************************************
* 
* Call function "InitialiseStdSupermirrorFlat" or "InitialiseStdSupermirrorFlat_detail" to set the values.
*
* Supermirror: char name[CHAR_BUF_LENGTH]; Polyhedron geo; CoordOp co; SupermirrorMaterials mat; SupermirrorProcess proc;
* |___Polyhedron: stores the surface normal and point-on-surface of the 6 planes defining the supermirror shape, defined in Polyhedron.h
* |___CoordOp: Coords fa (field axis); Rotation ry, rz; Coords tr; Rotation rry, rrz; Coords rtr; (rotation & translation of supermirror)
* |___SupermirrorMaterials: ReflectionParameters mir[SM_Num_Mirror_Planes][SM_Num_Spin_States]; 
* |   |						AbsorberParameters abs[SM_Num_Mirror_Planes];
* |   |						SubstrateSurfaceParameters subsurface[SM_Num_Mirror_Planes];
* |   |						SubstrateParameters sub;
* |   |___ReflectionParameters: char name[CHAR_BUF_LENGTH]; int refl_type; double R0, Qc, m, W, alpha, beta;
* |   |___AbsorberParameters : char name[CHAR_BUF_LENGTH]; int abs_type; double L_abs, L_inc, thickness_in_micron;
* |   |___SubstrateSurfaceParameters: char name[CHAR_BUF_LENGTH]; int subsurface_type; double Qc_sub, delta_n_2;
* |   |___SubstrateParameters: char name[CHAR_BUF_LENGTH]; int sub_type; double L_abs, L_inc, SLD;
* |___SupermirrorProcess: int is_tracking; char side[3][20]; char plane[6][20]; char layer[5][30]; char event[10][20];
*    |___is_tracking: 1=tracking, 0=not tracking
*    |___side: "ReflectedSide", "TransmittedSide", "EdgeSide"
*    |___plane: "Surface1", "Surface2", "EdgeFront", "EdgeBack", "EdgeSide1", "EdgeSide2"
*    |___layer: "MirrorLayer", "AbsorberLayer", "SubstrateSurfaceLayer", "SubstrateEdgeLayer", "SubstrateLayer"
*    |___event: "Error", "Exited", "Absorbed", "Intersected", "Missed", "Reflected", "Transmitted", "Refracted", "NotRefracted", "InternalReflection"
* 
*****************************************************************************
* End struct Supermirror description 
*****************************************************************************
*
*
* %L
*
* %E
*******************************************************************************/

#ifndef SUPERMIRROR_LIB_H
#define SUPERMIRROR_LIB_H "$Revision$"

//Max distance in meters normal to plane which a point is considered to be on plane.
//Equal to max(Maximum_On_Plane_Distance, DBL_EPISILON)
//DBL_EPISILON can be too small and can cause spurious reflections due to rounding error
#ifndef Maximum_On_Plane_Distance
#define Maximum_On_Plane_Distance 1e-10
#endif

#ifndef INDETERMINED
#define D_INDETERMINED -DBL_MAX
#define F_INDETERMINED -FLT_MAX
#define I_INDETERMINED -INT_MAX
#define INDETERMINED
#endif

#ifndef AMS
#define AMS 3956.034 //[Å m/s] neutron velocity -- wavelength coversion
#endif

#ifndef POLYHEDRON_LIB
#define POLYHEDRON_LIB
%include "polyhedron"
#endif

//record for SCATTER 
typedef struct NeutronRecord {
	int    nr_n; //neutron number in simulation
	double nr_w;  //neutron weight
	double nr_t;  //time of event
	Coords nr_p;  //neutron position
	Coords nr_v;  //neutron velocity
	Coords nr_s;  //neutron 3D polarisation vector
	void*  nr_nhn; //reserved, unused
} NeutronRecord;
 
//Supermirror event code returned by StdSupermirrorFlat and IntersectStdSupermirrorFlat
typedef enum SupermirrorEventCode { 
	sm_Error = 0, //both IntersectStdSupermirrorFlat and StdSupermirrorFlat
	sm_Exited = 1, //StdSupermirrorFlat
	sm_Absorbed = 2, //StdSupermirrorFlat
	sm_Intersected = 3, //IntersectStdSupermirrorFlat
	sm_Missed = 4 //IntersectStdSupermirrorFlat
} SupermirrorEventCode; 

//Structs below are contained inside Struct Supermirror and mostly for internal use, 
//Allocated automatically when struct Supermirror is allocated. 
//Values are set by calling InitialiseStdSupermirrorFlat or InitialiseStdSupermirrorFlat_detail functions
//Users can access the internal struct to obtain the parameter values but should not assign the values directly.
	typedef struct CoordOp {Coords fa, ty, rty, tr, rtr; Rotation ry, rz, rry, rrz; } CoordOp; 
	typedef struct ReflectionParameters {char name[CHAR_BUF_LENGTH]; int refl_type; double R0, Qc, m, W, alpha, beta;} ReflectionParameters; 
	typedef struct AbsorberParameters {char name[CHAR_BUF_LENGTH]; int abs_type; double L_abs, L_inc, thickness_in_micron; } AbsorberParameters; 
	typedef struct SubstrateSurfaceParameters {char name[CHAR_BUF_LENGTH]; int subsurface_type; double Qc_sub, delta_n_2; } SubstrateSurfaceParameters;
	typedef struct SubstrateParameters {char name[CHAR_BUF_LENGTH]; int sub_type; double L_abs, L_inc, SLD; } SubstrateParameters; 
	#define SM_Num_Mirror_Planes 2 
	#define SM_Num_Spin_States 2 
	typedef struct SupermirrorMaterials {	ReflectionParameters mir[SM_Num_Mirror_Planes][SM_Num_Spin_States]; 
											AbsorberParameters abs[SM_Num_Mirror_Planes]; 
											SubstrateSurfaceParameters subsurface[SM_Num_Mirror_Planes];
											SubstrateParameters sub; 
										} SupermirrorMaterials; 
	typedef struct SupermirrorProcess { 
		int is_tracking; //1=tracking, 0=not tracking
		NeutronRecord*nr; //neutron records
		int n_nr; //number of neutron history records stored
		int n_nr_allocated; //number of neutron history records allocated
		int nr_allocation_size; //number of neutron records to allocate each time
		
		char side[3][20]; //"ReflectedSide", "TransmittedSide", "EdgeSide"
		char plane[6][20]; //name of the planes of supermirror, "Surface1", "Surface2", "EdgeFront", "EdgeBack", "EdgeSide1", "EdgeSide2"
		char layer[5][30]; //name of layers associated with the planes, "MirrorLayer", "AbsorberLayer", "SubstrateSurfaceLayer", "SubstrateEdgeLayer", "SubstrateLayer"
		char event[10][20]; //events: "Error", "Exited", "Absorbed", "Intersected", "Missed", "Reflected", "Transmitted", "Refracted", "sm_NotRefracted", "InternalReflection"
		int initialised; //1=initialised, 0=not initialised or initialisation failed
	} SupermirrorProcess; 
		
//struct Supermirror is allocated by user and provided to the initialisation and ray-tracing functions
//Example: 
//1. Supermirror sm;
//2. Supermirror *sm = (Supermirror *)malloc(sizeof(Supermirror));
//3. An array: Supermirror *sm = (Supermirror *)calloc(number_of_supermirrors, sizeof(Supermirror));
typedef struct Supermirror { char name[CHAR_BUF_LENGTH]; 
							 Polyhedron geo; CoordOp co; 
							 SupermirrorMaterials mat; 
							 SupermirrorProcess proc; 
							 } Supermirror;

/*************/
/* functions */
/*************/

//Supermirror initialisation function
//return: 1=succeed, 0=failed
int InitialiseStdSupermirrorFlat( 
		
		char *name, //Supermirror name, do not use '/' in the name, do not use "all"
		
		////////////////////////////////////////////////////////////////////////////////
		//STEP 1: Specify supermirror shape, supermirror coating, absorber and substrate material
		//In this step, supermirror is standing vertically, mirror coating = yz plane with long side along +z.
		double length, //m, use SI unit unless specified
		double thickness_in_mm, //mm - note the unit
		Coords side_edge_normal, Coords side_edge_point, //one edge at +y or -y side, edges are mirror-image of one another. 
		
		char*mirror_coated_side, //Sequential combination of keywords of
								 //position: "Both", "Top", "Bottom";
								 //surface property: "Coated", "SubstrateSurface", "NoReflection"; 
								 //e.g. "BothCoated", "BottomCoatedTopSubstrateSurface",
		char*mirror_spin_plus_material_name, //defined in "Supermirror_reflective_coating_materials.txt", non-polarising: use same for spin minus
		double mirror_spin_plus_m, //if non-polarising: use same for spin minus 
		char*mirror_spin_minus_material_name, //defined in "Supermirror_reflective_coating_materials.txt", non-polarising: use same for spin plus
		double mirror_spin_minus_m, //if non-polarising: use same for spin plus 
		
		char*absorber_coated_side, //"BothCoated", "TopCoated", "BottomCoated", "BothNotCoated"
		char*absorber_material_name, //defined in "Supermirror_absorber_coating_materials.txt"
		double absorber_thickness_in_micron, //micrometer
		
		char*substrate_material_name, //defined in "Supermirror_substrate_materials.txt"
		
		////////////////////////////////////////////////////////////////////////////////
		//STEP 2: Orient and position the supermirror 
		char*initial_placement_at_origin,	 //"TopFrontEdgeCentre","FrontSubstrateCentre","BottomFrontEdgeCentre"
		char*tilt_y_axis_location, 			 //"TopFrontEdge","TopMirrorCentre","TopBackEdge"
											 //"FrontSubstrateCentre","SubstrateCentre","BackSubstrateCentre", 
											 //"BottomFrontEdge","BottomMirrorCentre","BottomBackEdge"
		double tilt_about_y_first_in_degree, //degree, first, tilt about y-axis at selected location 
		Coords translation_second,			 //second, translate reference point 
		double rot_about_z_third_in_degree,	 //third, rotate about global z-axis 

		////////////////////////////////////////////////////////////////////////////////
		//simulation process control parameters
		int is_tracking, //1=tracking, 0=not tracking

		////////////////////////////////////////////////////////////////////////////////
		//OUTPUT: Supermirror struct with all parameter values entered and initialised
		//User declares "Supermirror supermirror;" or its equivalence, 
		//then passes pointer "&supermirror" to function.
		Supermirror*sm
		); 

//Supermirror initialisation functions, detail specification
//return: 1=succeed, 0=failed
int InitialiseStdSupermirrorFlat_detail( 
		
		char *name, //Supermirror name, do not use '/' in the name, do not use "all"
		
		////////////////////////////////////////////////////////////////////////////////
		//STEP 1: Specify the supermirror shape, mirror coatings and substrate material
		//In this step, supermirror is standing vertically, mirror coating = yz plane with long side along +z.
		double length, //m, use SI unit unless specified
		double thickness_in_mm, //in mm - note the unit
		Coords side_edge_1_normal, Coords side_edge_1_point, //edge in +x or -x side 
		Coords side_edge_2_normal, Coords side_edge_2_point, //edge in +x or -x side 
		
		//If materials name is specified and matching those defined in "Supermirror_reflective_coating_materials.txt" 
		//material name overrides material spec, otherwise 6-parameter material spec is used.  
		//Use the same for spin plus and spin minus if the mirror is non-polarising
		char*mirror_top_spin_plus_material_name, double*mirror_top_spin_plus_material_spec, double mirror_top_spin_plus_m, 
		char*mirror_top_spin_minus_material_name, double*mirror_top_spin_minus_material_spec, double mirror_top_spin_minus_m, 
		char*mirror_bottom_spin_plus_material_name, double*mirror_bottom_spin_plus_material_spec, double mirror_bottom_spin_plus_m, 
		char*mirror_bottom_spin_minus_material_name, double*mirror_bottom_spin_minus_material_spec, double mirror_bottom_spin_minus_m, 

		//INPUT: Absorber parameters are absorption paramter and thickness in microns: 
		//If materials name is specified and matching those defined in "Supermirror_absorber_coating_materials.txt"  
		//material name overrides material spec, otherwise 1-parameter material spec is used.  
		char*absorber_top_material_name, double absorber_top_L_abs, double absorber_top_L_inc, double absorber_top_thickness_in_micron,
		char*absorber_bottom_material_name, double absorber_bottom_L_abs, double absorber_bottom_L_inc, double absorber_bottom_thickness_in_micron,

		//INPUT: Substrate parameters are substrate attenuation paramters and SLD: 
		//If materials name is specified and matching those defined in "Supermirror_substrate_materials.txt"  
		//material name overrides material spec, otherwise 3-parameter material spec is used.  
		char*substrate_material_name, double substrate_L_abs, double substrate_L_inc, double substrate_SLD,

		////////////////////////////////////////////////////////////////////////////////
		//STEP 2: Orient and position the supermirror 
		char*initial_placement_at_origin,	 //"TopFrontEdgeCentre","FrontSubstrateCentre","BottomFrontEdgeCentre"
		char*tilt_y_axis_location, 			 //"TopFrontEdge","TopMirrorCentre","TopBackEdge"
											 //"FrontSubstrateCentre","SubstrateCentre","BackSubstrateCentre", 
											 //"BottomFrontEdge","BottomMirrorCentre","BottomBackEdge"
		double tilt_about_y_first_in_degree, //degree, first, tilt about y-axis at selected location 
		Coords translation_second,			 //second, translate reference point 
		double rot_about_z_third_in_degree,	 //third, rotate about global z-axis 

		////////////////////////////////////////////////////////////////////////////////
		//simulation process control parameters
		int is_tracking, //1=tracking, 0=not tracking

		////////////////////////////////////////////////////////////////////////////////
		//OUTPUT: Supermirror struct with all parameter values entered and initialised
		//User declares "Supermirror supermirror;" or its equivalence, 
		//then passes pointer "&supermirror" to function.
		Supermirror*sm
		); 


//Supermirror intersect-finding function
//return values: sm_Intersected, sm_Missed, sm_Error defined in "typedef enum SupermirrorEventCode" above
//note: sm_Missed = neutron either skips supermirror, or intersect only once (i.e. one edge or one vertex), or flies on plane or edge
int IntersectStdSupermirrorFlat(
		//INPUT
		double time, Coords position, Coords velocity, 
		double last_exit_time, Coords last_exit_point, int last_exit_plane, 
		Supermirror*sm,
		//OUTPUT 
		//User declare one or more parameters, e.g. "double intersect_time_out; Coords intersect_point_out; int intersect_plane_out;", 
		//then passes pointers "&intersect_time_out, &intersect_point_out, &intersect_plane_out" to function.
		//Pass 0 if not needed.
		int*num_intersect, 
		double*intersect_dtime_out, Coords*intersect_dpoint_out, double*intersect_time_out, Coords*intersect_point_out, int*intersect_plane_out);


//Supermirror ray-tracing function
//return: sm_Exited, sm_Absorbed, sm_Error defined in "typedef enum SupermirrorEventCode" above 
int StdSupermirrorFlat(
		//INPUT & OUTPUT
		double *intensity, double *time, Coords *position, Coords *velocity, Coords *spin, 
		double*last_exit_time, Coords*last_exit_point, int*last_exit_plane, 
		//INPUT
		Supermirror *sm
		);

//Finishing - release allocated memories
void EmptySupermirrorFlatData(Supermirror*sm);

#endif

/* end of ref-lib.h */