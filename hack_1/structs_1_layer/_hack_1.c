/* Automatically generated file. Do not edit. 
 * Format:     ANSI C source code
 * Creator:    McStas <http://www.mcstas.org>
 * Instrument: hack_1.instr (Minimal)
 * Date:       Thu Oct 17 12:07:07 2019
 * File:       hack_1.c
 * CFLAGS=
 */

#define MCCODE_STRING "McStas 3.0-dev - Oct. 17, 2019"
#define FLAVOR        "mcstas"
#define FLAVOR_UPPER  "MCSTAS"

#define MC_USE_DEFAULT_MAIN
#define PI 3.14159265358979323846
#ifndef M_PI
#define M_PI PI
#endif
#ifndef M_2_PI
#define M_2_PI 0.63661977236758134308
#endif
#ifndef M_PI_2
#define M_PI_2 1.57079632679489661923  /* pi/2 */
#endif
#ifndef M_SQRT2
#define M_SQRT2 1.4142135623730951  /* sqrt(2) */
#endif

#ifdef USE_PGI
#undef MC_TRACE_ENABLED
#include <openacc_curand.h>
#endif

struct _struct_particle {
  double x,y,z; /* position [m] */
  double vx,vy,vz; /* velocity [m/s] */
  double sx,sy,sz; /* spin [0-1] */
#ifdef USE_PGI
  curandState_t MCRANDstate; /* CUDA RNG state */
#endif
  double t, p;    /* time, event weight */
  long long _uid;  /* event ID */
  long _index;     /* component index where to send this event */
  long _absorbed;  /* flag set to TRUE when this event is to be removed/ignored */
  long _scattered; /* flag set to TRUE when this event has interacted with the last component instance */
  long _restore;   /* set to true if neutron event must be restored */
};
typedef struct _struct_particle _class_particle;

#define MC_EMBEDDED_RUNTIME
#include "_mccode-r.h"
/* End of file "mccode-r.h". */

#include "_mcstas-r.h"
/* End of file "mcstas-r.h". */

#include "_mccode-r.c"
/* End of file "mccode-r.c". */

#include "_mcstas-r.c"
/* End of file "mcstas-r.c". */


/* *****************************************************************************
* Start of instrument 'Minimal' generated code
***************************************************************************** */

#ifdef MC_TRACE_ENABLED
int traceenabled = 1;
#else
int traceenabled = 0;
#endif
#define MCSTAS "/usr/share/mcstas/3.0-dev/"
int   defaultmain         = 1;
char  instrument_name[]   = "Minimal";
char  instrument_source[] = "hack_1.instr";
char *instrument_exe      = NULL; /* will be set to argv[0] in main */
char  instrument_code[]   = "Instrument Minimal source code hack_1.instr is not embedded in this executable.\n  Use --source option when running McStas.\n";

int main(int argc, char *argv[]){return mccode_main(argc, argv);}

/* *****************************************************************************
* instrument 'Minimal' and components DECLARE
***************************************************************************** */

/* Instrument parameters: structure and a table for the initialisation
   (Used in e.g. inputparse and I/O function (e.g. detector_out) */

struct _struct_instrument_parameters {
  MCNUM _Par1;
};
typedef struct _struct_instrument_parameters _class_instrument_parameters;

struct _instrument_struct {
  char   _name[256]; /* the name of this instrument e.g. 'Minimal' */
/* Counters per component instance */
  double counter_AbsorbProp[6]; /* absorbed events in PROP routines */
  double counter_N[6], counter_P[6], counter_P2[6]; /* event counters after each component instance */
  _class_particle _trajectory[6]; /* current trajectory for STORE/RESTORE */
/* Components position table (absolute and relative coords) */
  Coords _position_relative[6]; /* positions of all components */
  Coords _position_absolute[6];
  _class_instrument_parameters _parameters; /* instrument parameters */
} _instrument_var;
struct _instrument_struct *instrument = & _instrument_var;
#pragma acc declare create ( _instrument_var )
#pragma acc declare create ( instrument )

int numipar = 1;
struct mcinputtable_struct mcinputtable[] = {
  "Par1", &(_instrument_var._parameters._Par1), instr_type_double, "1", 
  NULL, NULL, instr_type_double, ""
};


/* ********************** component definition declarations. **************** */

/* component arm=Arm() [1] DECLARE */
/* Parameter definition for component type 'Arm' */
//struct _struct_Arm_parameters {
//  char Arm_has_no_parameters;
//}; /* _struct_Arm_parameters */
//typedef struct _struct_Arm_parameters _class_Arm_parameters;

/* Parameters for component type 'Arm' */
struct _struct_Arm {
  char     _name[256]; /* e.g. arm */
  char     _type[256]; /* Arm */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  char Arm_has_no_parameters;
  //_class_Arm_parameters _parameters;
};
typedef struct _struct_Arm _class_Arm;
_class_Arm _arm_var;
_class_Arm *_arm = &_arm_var;
#pragma acc declare create ( _arm_var )
#pragma acc declare create ( _arm )

/* component source=Source_simple() [2] DECLARE */
/* Parameter definition for component type 'Source_simple' */
//struct _struct_Source_simple_parameters {
  /* Component type 'Source_simple' setting parameters */
//  MCNUM radius;
//  MCNUM yheight;
//  MCNUM xwidth;
//  MCNUM dist;
//  MCNUM focus_xw;
//  MCNUM focus_yh;
//  MCNUM E0;
//  MCNUM dE;
//  MCNUM lambda0;
//  MCNUM dlambda;
//  MCNUM flux;
//  MCNUM gauss;
//  long target_index;
  /* Component type 'Source_simple' private parameters */
  /* Component type 'Source_simple' DECLARE code stored as structure members */
//double pmul, srcArea;
//int square;
//}; /* _struct_Source_simple_parameters */
//typedef struct _struct_Source_simple_parameters _class_Source_simple_parameters;

/* Parameters for component type 'Source_simple' */
struct _struct_Source_simple {
  char     _name[256]; /* e.g. source */
  char     _type[256]; /* Source_simple */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
    MCNUM radius;
  MCNUM yheight;
  MCNUM xwidth;
  MCNUM dist;
  MCNUM focus_xw;
  MCNUM focus_yh;
  MCNUM E0;
  MCNUM dE;
  MCNUM lambda0;
  MCNUM dlambda;
  MCNUM flux;
  MCNUM gauss;
  long target_index;
  /* Component type 'Source_simple' private parameters */
  /* Component type 'Source_simple' DECLARE code stored as structure members */
  double pmul, srcArea;
  int square;
  //_class_Source_simple_parameters _parameters;
};
typedef struct _struct_Source_simple _class_Source_simple;
_class_Source_simple _source_var;
_class_Source_simple *_source = &_source_var;
#pragma acc declare create ( _source_var )
#pragma acc declare create ( _source )

/* /\* component coll2=Slit() [3] DECLARE *\/ */
/* /\* Parameter definition for component type 'Slit' *\/ */
/* struct _struct_Slit_parameters { */
/*   /\* Component type 'Slit' setting parameters *\/ */
/*   MCNUM xmin; */
/*   MCNUM xmax; */
/*   MCNUM ymin; */
/*   MCNUM ymax; */
/*   MCNUM radius; */
/*   MCNUM xwidth; */
/*   MCNUM yheight; */
/* }; /\* _struct_Slit_parameters *\/ */
/* typedef struct _struct_Slit_parameters _class_Slit_parameters; */

/* Parameters for component type 'Slit' */
struct _struct_Slit {
  char     _name[256]; /* e.g. coll2 */
  char     _type[256]; /* Slit */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM radius;
  MCNUM xwidth;
  MCNUM yheight;
  //_class_Slit_parameters _parameters;
};
typedef struct _struct_Slit _class_Slit;
_class_Slit _coll2_var;
_class_Slit *_coll2 = &_coll2_var;
#pragma acc declare create ( _coll2_var )
#pragma acc declare create ( _coll2 )

/* component detector=L_monitor() [4] DECLARE */
/* Parameter definition for component type 'L_monitor' */
/* struct _struct_L_monitor_parameters { */
/*   /\* Component type 'L_monitor' setting parameters *\/ */
/*   MCNUM nL; */
/*   char filename[16384]; */
/*   MCNUM xmin; */
/*   MCNUM xmax; */
/*   MCNUM ymin; */
/*   MCNUM ymax; */
/*   MCNUM xwidth; */
/*   MCNUM yheight; */
/*   MCNUM Lmin; */
/*   MCNUM Lmax; */
/*   MCNUM restore_neutron; */
/*   /\* Component type 'L_monitor' private parameters *\/ */
/*   /\* Component type 'L_monitor' DECLARE code stored as structure members *\/ */
/*   DArray1d L_N; */
/*   DArray1d L_p; */
/*   DArray1d L_p2; */
/* }; /\* _struct_L_monitor_parameters *\/ */
/* typedef struct _struct_L_monitor_parameters _class_L_monitor_parameters; */

/* Parameters for component type 'L_monitor' */
struct _struct_L_monitor {
  char     _name[256]; /* e.g. detector */
  char     _type[256]; /* L_monitor */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  MCNUM nL;
  char filename[16384];
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM xwidth;
  MCNUM yheight;
  MCNUM Lmin;
  MCNUM Lmax;
  MCNUM restore_neutron;
  /* Component type 'L_monitor' private parameters */
  /* Component type 'L_monitor' DECLARE code stored as structure members */
  MCNUM L_N[128];
  MCNUM L_p[128];
  MCNUM L_p2[128];
  #pragma acc shape( L_N[0:127],L_p[0:127],L_p2[0:127] )
  
  //_class_L_monitor_parameters _parameters;
};
typedef struct _struct_L_monitor _class_L_monitor;
_class_L_monitor _detector_var;
_class_L_monitor *_detector = &_detector_var;
#pragma acc declare create ( _detector_var )
#pragma acc declare create ( _detector )

int mcNUMCOMP = 4;

/* User declarations from instrument definition. Can define functions. */

#undef compcurname
#undef compcurtype
#undef compcurindex
/* end of instrument 'Minimal' and components DECLARE */

/* *****************************************************************************
* instrument 'Minimal' and components INITIALISE
***************************************************************************** */

/* component arm=Arm() SETTING, POSITION/ROTATION */
int _arm_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_arm_setpos] component arm=Arm() SETTING [Arm:0]");
  stracpy(_arm->_name, "arm", 16384);
  stracpy(_arm->_type, "Arm", 16384);
  _arm->_index=1;
  /* component arm=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(_arm->_rotation_absolute,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_copy(_arm->_rotation_relative, _arm->_rotation_absolute);
    _arm->_rotation_is_identity =  rot_test_identity(_arm->_rotation_relative);
    _arm->_position_absolute = coords_set(
      0, 0, 0);
    tc1 = coords_neg(_arm->_position_absolute);
    _arm->_position_relative = rot_apply(_arm->_rotation_absolute, tc1);
  } /* arm=Arm() AT ROTATED */
  DEBUG_COMPONENT("arm", _arm->_position_absolute, _arm->_rotation_absolute);
  instrument->_position_absolute[1] = _arm->_position_absolute;
  instrument->_position_relative[1] = _arm->_position_relative;
  instrument->counter_N[1]  = instrument->counter_P[1] = instrument->counter_P2[1] = 0;
  instrument->counter_AbsorbProp[1]= 0;
  return(0);
} /* _arm_setpos */

/* component source=Source_simple() SETTING, POSITION/ROTATION */
int _source_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_source_setpos] component source=Source_simple() SETTING [/usr/share/mcstas/3.0-dev/sources/Source_simple.comp:64]");
  stracpy(_source->_name, "source", 16384);
  stracpy(_source->_type, "Source_simple", 16384);
  _source->_index=2;
  _source->radius = 0.02;
  #define radius (_source->radius)
  _source->yheight = 0;
  #define yheight (_source->yheight)
  _source->xwidth = 0;
  #define xwidth (_source->xwidth)
  _source->dist = 3;
  #define dist (_source->dist)
  _source->focus_xw = 0.01;
  #define focus_xw (_source->focus_xw)
  _source->focus_yh = 0.01;
  #define focus_yh (_source->focus_yh)
  _source->E0 = 0;
  #define E0 (_source->E0)
  _source->dE = 0;
  #define dE (_source->dE)
  _source->lambda0 = 6.0;
  #define lambda0 (_source->lambda0)
  _source->dlambda = 0.5;
  #define dlambda (_source->dlambda)
  _source->flux = 1e8;
  #define flux (_source->flux)
  _source->gauss = 0;
  #define gauss (_source->gauss)
  _source->target_index = + 1;
  #define target_index (_source->target_index)

  #define pmul (_source->pmul)
  #define square (_source->square)
  #define srcArea (_source->srcArea)

  #undef radius
  #undef yheight
  #undef xwidth
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef E0
  #undef dE
  #undef lambda0
  #undef dlambda
  #undef flux
  #undef gauss
  #undef target_index
  #undef pmul
  #undef square
  #undef srcArea
  /* component source=Source_simple() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _arm->_rotation_absolute, _source->_rotation_absolute);
    rot_transpose(_arm->_rotation_absolute, tr1);
    rot_mul(_source->_rotation_absolute, tr1, _source->_rotation_relative);
    _source->_rotation_is_identity =  rot_test_identity(_source->_rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_arm->_rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _source->_position_absolute = coords_add(_arm->_position_absolute, tc2);
    tc1 = coords_sub(_arm->_position_absolute, _source->_position_absolute);
    _source->_position_relative = rot_apply(_source->_rotation_absolute, tc1);
  } /* source=Source_simple() AT ROTATED */
  DEBUG_COMPONENT("source", _source->_position_absolute, _source->_rotation_absolute);
  instrument->_position_absolute[2] = _source->_position_absolute;
  instrument->_position_relative[2] = _source->_position_relative;
  instrument->counter_N[2]  = instrument->counter_P[2] = instrument->counter_P2[2] = 0;
  instrument->counter_AbsorbProp[2]= 0;
  return(0);
} /* _source_setpos */

/* component coll2=Slit() SETTING, POSITION/ROTATION */
int _coll2_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_coll2_setpos] component coll2=Slit() SETTING [/usr/share/mcstas/3.0-dev/optics/Slit.comp:47]");
  stracpy(_coll2->_name, "coll2", 16384);
  stracpy(_coll2->_type, "Slit", 16384);
  _coll2->_index=3;
  _coll2->xmin = -0.01;
  #define xmin (_coll2->xmin)
  _coll2->xmax = 0.01;
  #define xmax (_coll2->xmax)
  _coll2->ymin = -0.01;
  #define ymin (_coll2->ymin)
  _coll2->ymax = 0.01;
  #define ymax (_coll2->ymax)
  _coll2->radius = 0.01;
  #define radius (_coll2->radius)
  _coll2->xwidth = 0;
  #define xwidth (_coll2->xwidth)
  _coll2->yheight = 0;
  #define yheight (_coll2->yheight)

  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  /* component coll2=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _arm->_rotation_absolute, _coll2->_rotation_absolute);
    rot_transpose(_source->_rotation_absolute, tr1);
    rot_mul(_coll2->_rotation_absolute, tr1, _coll2->_rotation_relative);
    _coll2->_rotation_is_identity =  rot_test_identity(_coll2->_rotation_relative);
    tc1 = coords_set(
      0, 0, 6);
    rot_transpose(_arm->_rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _coll2->_position_absolute = coords_add(_arm->_position_absolute, tc2);
    tc1 = coords_sub(_source->_position_absolute, _coll2->_position_absolute);
    _coll2->_position_relative = rot_apply(_coll2->_rotation_absolute, tc1);
  } /* coll2=Slit() AT ROTATED */
  DEBUG_COMPONENT("coll2", _coll2->_position_absolute, _coll2->_rotation_absolute);
  instrument->_position_absolute[3] = _coll2->_position_absolute;
  instrument->_position_relative[3] = _coll2->_position_relative;
  instrument->counter_N[3]  = instrument->counter_P[3] = instrument->counter_P2[3] = 0;
  instrument->counter_AbsorbProp[3]= 0;
  return(0);
} /* _coll2_setpos */

/* component detector=L_monitor() SETTING, POSITION/ROTATION */
int _detector_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_detector_setpos] component detector=L_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/L_monitor.comp:65]");
  stracpy(_detector->_name, "detector", 16384);
  stracpy(_detector->_type, "L_monitor", 16384);
  _detector->_index=4;
  _detector->nL = 128;
  #define nL (_detector->nL)
  if("L.dat" && strlen("L.dat"))
    stracpy(_detector->filename, "L.dat" ? "L.dat" : "", 16384);
  else 
  _detector->filename[0]='\0';
  #define filename (_detector->filename)
  _detector->xmin = -0.1;
  #define xmin (_detector->xmin)
  _detector->xmax = 0.1;
  #define xmax (_detector->xmax)
  _detector->ymin = -0.1;
  #define ymin (_detector->ymin)
  _detector->ymax = 0.1;
  #define ymax (_detector->ymax)
  _detector->xwidth = 0;
  #define xwidth (_detector->xwidth)
  _detector->yheight = 0;
  #define yheight (_detector->yheight)
  _detector->Lmin = 5;
  #define Lmin (_detector->Lmin)
  _detector->Lmax = 7;
  #define Lmax (_detector->Lmax)
  _detector->restore_neutron = 0;
  #define restore_neutron (_detector->restore_neutron)

  #define L_N (_detector->L_N)
  #define L_p (_detector->L_p)
  #define L_p2 (_detector->L_p2)

  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  /* component detector=L_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _arm->_rotation_absolute, _detector->_rotation_absolute);
    rot_transpose(_coll2->_rotation_absolute, tr1);
    rot_mul(_detector->_rotation_absolute, tr1, _detector->_rotation_relative);
    _detector->_rotation_is_identity =  rot_test_identity(_detector->_rotation_relative);
    tc1 = coords_set(
      0, 0, 9.01);
    rot_transpose(_arm->_rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _detector->_position_absolute = coords_add(_arm->_position_absolute, tc2);
    tc1 = coords_sub(_coll2->_position_absolute, _detector->_position_absolute);
    _detector->_position_relative = rot_apply(_detector->_rotation_absolute, tc1);
  } /* detector=L_monitor() AT ROTATED */
  DEBUG_COMPONENT("detector", _detector->_position_absolute, _detector->_rotation_absolute);
  instrument->_position_absolute[4] = _detector->_position_absolute;
  instrument->_position_relative[4] = _detector->_position_relative;
  instrument->counter_N[4]  = instrument->counter_P[4] = instrument->counter_P2[4] = 0;
  instrument->counter_AbsorbProp[4]= 0;
  return(0);
} /* _detector_setpos */

_class_Source_simple *class_Source_simple_init(_class_Source_simple *_comp
) {
  #define radius (_comp->radius)
  #define yheight (_comp->yheight)
  #define xwidth (_comp->xwidth)
  #define dist (_comp->dist)
  #define focus_xw (_comp->focus_xw)
  #define focus_yh (_comp->focus_yh)
  #define E0 (_comp->E0)
  #define dE (_comp->dE)
  #define lambda0 (_comp->lambda0)
  #define dlambda (_comp->dlambda)
  #define flux (_comp->flux)
  #define gauss (_comp->gauss)
  #define target_index (_comp->target_index)
  #define pmul (_comp->pmul)
  #define square (_comp->square)
  #define srcArea (_comp->srcArea)
square = 0;
/* Determine source area */
if (radius && !yheight && !xwidth ) {
    square = 0;
    srcArea = PI*radius*radius;
  } else if(yheight && xwidth) {
    square = 1;
    srcArea = xwidth * yheight;
  }

  if (flux) {
    pmul=flux*1e4*srcArea/mcget_ncount();
    if (dlambda)
      pmul *= 2*dlambda;
    else if (dE)
      pmul *= 2*dE;
  } else {
    gauss = 0;
    pmul=1.0/(mcget_ncount()*4*PI);
  }

  if (target_index && !dist)
  {
    Coords ToTarget;
    double tx,ty,tz;
    ToTarget = coords_sub(POS_A_COMP_INDEX(INDEX_CURRENT_COMP+target_index),POS_A_CURRENT_COMP);
    ToTarget = rot_apply(ROT_A_CURRENT_COMP, ToTarget);
    coords_get(ToTarget, &tx, &ty, &tz);
    dist=sqrt(tx*tx+ty*ty+tz*tz);
  }

  if (srcArea <= 0) {
    printf("Source_simple: %s: Source area is <= 0 !\n ERROR - Exiting\n",
           NAME_CURRENT_COMP);
    exit(0);
  }
  if (dist <= 0 || focus_xw <= 0 || focus_yh <= 0) {
    printf("Source_simple: %s: Target area unmeaningful! (negative dist / focus_xw / focus_yh)\n ERROR - Exiting\n",
           NAME_CURRENT_COMP);
    exit(0);
  }

  if ((!lambda0 && !E0 && !dE && !dlambda)) {
    printf("Source_simple: %s: You must specify either a wavelength or energy range!\n ERROR - Exiting\n",
           NAME_CURRENT_COMP);
    exit(0);
  }
  if ((!lambda0 && !dlambda && (E0 <= 0 || dE < 0 || E0-dE <= 0))
    || (!E0 && !dE && (lambda0 <= 0 || dlambda < 0 || lambda0-dlambda <= 0))) {
    printf("Source_simple: %s: Unmeaningful definition of wavelength or energy range!\n ERROR - Exiting\n",
           NAME_CURRENT_COMP);
      exit(0);
  }
  #undef radius
  #undef yheight
  #undef xwidth
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef E0
  #undef dE
  #undef lambda0
  #undef dlambda
  #undef flux
  #undef gauss
  #undef target_index
  #undef pmul
  #undef square
  #undef srcArea
  return(_comp);
} /* class_Source_simple_init */

_class_Slit *class_Slit_init(_class_Slit *_comp
) {
  #define xmin (_comp->xmin)
  #define xmax (_comp->xmax)
  #define ymin (_comp->ymin)
  #define ymax (_comp->ymax)
  #define radius (_comp->radius)
  #define xwidth (_comp->xwidth)
  #define yheight (_comp->yheight)
if (xwidth > 0)  { xmax=xwidth/2;  xmin=-xmax; }
  if (yheight > 0) { ymax=yheight/2; ymin=-ymax; }
  if (xmin == 0 && xmax == 0 && ymin == 0 && ymax == 0 && radius == 0)
    { fprintf(stderr,"Slit: %s: Error: give geometry\n", NAME_CURRENT_COMP); exit(-1); }

  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_init */

_class_L_monitor *class_L_monitor_init(_class_L_monitor *_comp
) {
  #define nL (_comp->nL)
  #define filename (_comp->filename)
  #define xmin (_comp->xmin)
  #define xmax (_comp->xmax)
  #define ymin (_comp->ymin)
  #define ymax (_comp->ymax)
  #define xwidth (_comp->xwidth)
  #define yheight (_comp->yheight)
  #define Lmin (_comp->Lmin)
  #define Lmax (_comp->Lmax)
  #define restore_neutron (_comp->restore_neutron)
  #define L_N (_comp->L_N)
  #define L_p (_comp->L_p)
  #define L_p2 (_comp->L_p2)
  if (xwidth  > 0) { xmax = xwidth/2;  xmin = -xmax; }
  if (yheight > 0) { ymax = yheight/2; ymin = -ymax; }

  if ((xmin >= xmax) || (ymin >= ymax)) {
    printf("L_monitor: %s: Null detection area !\n"
      "ERROR      (xwidth,yheight,xmin,xmax,ymin,ymax). Exiting",
      NAME_CURRENT_COMP);
    exit(0);
  }

  /*L_N = create_darr1d(nL);
  L_p = create_darr1d(nL);
  L_p2 = create_darr1d(nL);*/

  int i;
  for (i=0; i<nL; i++)
  {
    L_N[i] = 0;
    L_p[i] = 0;
    L_p2[i] = 0;
  }
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_init */



int init(void) { /* called by mccode_main for Minimal:INITIALISE */
  DEBUG_INSTR();

  /* code_main/parseoptions/readparams sets instrument parameters value */
  stracpy(instrument->_name, "Minimal", 256);

  _arm_setpos(); /* type Arm */
  _source_setpos(); /* type Source_simple */
  _coll2_setpos(); /* type Slit */
  _detector_setpos(); /* type L_monitor */

  /* call iteratively all components INITIALISE */

  class_Source_simple_init(_source);

  class_Slit_init(_coll2);

  class_L_monitor_init(_detector);

  if (mcdotrace) display();
  DEBUG_INSTR_END();
#ifdef USE_PGI
#include <openacc.h>
acc_attach( (void**)&_arm );
acc_attach( (void**)&_source );
acc_attach( (void**)&_coll2 );
acc_attach( (void**)&_detector );
acc_attach( (void**)&instrument );
#endif

  return(0);
} /* init */

/*******************************************************************************
* components TRACE
*******************************************************************************/

#define x (_particle->x)
#define y (_particle->y)
#define z (_particle->z)
#define vx (_particle->vx)
#define vy (_particle->vy)
#define vz (_particle->vz)
#define t (_particle->t)
#define sx (_particle->sx)
#define sy (_particle->sy)
#define sz (_particle->sz)
#define p (_particle->p)
// user variables:

#define SCATTERED (_particle->_scattered)
#define RESTORE (_particle->_restore)
#define RESTORE_NEUTRON(_index, ...) _particle->_restore = _index;
#define ABSORBED (_particle->_absorbed)
#define ABSORB0 do { DEBUG_STATE(); DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; return(_comp); } while(0)
#define ABSORB ABSORB0
#pragma acc routine seq nohost
_class_Source_simple *class_Source_simple_trace(_class_Source_simple *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define radius (_source_var.radius)
  #define yheight (_source_var.yheight)
  #define xwidth (_source_var.xwidth)
  #define dist (_source_var.dist)
  #define focus_xw (_source_var.focus_xw)
  #define focus_yh (_source_var.focus_yh)
  #define E0 (_source_var.E0)
  #define dE (_source_var.dE)
  #define lambda0 (_source_var.lambda0)
  #define dlambda (_source_var.dlambda)
  #define flux (_source_var.flux)
  #define gauss (_source_var.gauss)
  #define target_index (_source_var.target_index)
  #define pmul (_source_var.pmul)
  #define square (_source_var.square)
  #define srcArea (_source_var.srcArea)
 double chi,E,lambda,v,r, xf, yf, rf, dx, dy, pdir;

 t=0;
 z=0;

 if (square == 1) {
   x = xwidth * (rand01() - 0.5);
   y = yheight * (rand01() - 0.5);
 } else {
   chi=2*PI*rand01();                          /* Choose point on source */
   r=sqrt(rand01())*radius;                    /* with uniform distribution. */
   x=r*cos(chi);
   y=r*sin(chi);
 }
 randvec_target_rect_real(&xf, &yf, &rf, &pdir,
			  0, 0, dist, focus_xw, focus_yh, ROT_A_CURRENT_COMP, x, y, z, 2);

 dx = xf-x;
 dy = yf-y;
 rf = sqrt(dx*dx+dy*dy+dist*dist);

 p = pdir*pmul;

 if(lambda0==0) {
   if (!gauss) {
     E=E0+dE*randpm1();              /*  Choose from uniform distribution */
   } else {
     E=E0+randnorm()*dE;
   }
   v=sqrt(E)*SE2V;
 } else {
   if (!gauss) {
     lambda=lambda0+dlambda*randpm1();
   } else {
     lambda=lambda0+randnorm()*dlambda;
   }
   v = K2V*(2*PI/lambda);
 }

 vz=v*dist/rf;
 vy=v*dy/rf;
 vx=v*dx/rf;
  #undef radius
  #undef yheight
  #undef xwidth
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef E0
  #undef dE
  #undef lambda0
  #undef dlambda
  #undef flux
  #undef gauss
  #undef target_index
  #undef pmul
  #undef square
  #undef srcArea
  return(_comp);
} /* class_Source_simple_trace */

#pragma acc routine seq nohost
_class_Slit *class_Slit_trace(_class_Slit *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define xmin (_coll2_var.xmin)
  #define xmax (_coll2_var.xmax)
  #define ymin (_coll2_var.ymin)
  #define ymax (_coll2_var.ymax)
  #define radius (_coll2_var.radius)
  #define xwidth (_coll2_var.xwidth)
  #define yheight (_coll2_var.yheight)
  /*  mcPROP_Z0;
    if (((radius == 0) && (x<xmin || x>xmax || y<ymin || y>ymax))
    || ((radius != 0) && (x*x + y*y > radius*radius)))
      ABSORB;
    else
        SCATTER;
  */
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_trace */

#pragma acc routine seq nohost
_class_L_monitor *class_L_monitor_trace(_class_L_monitor *_comp
					, _class_particle *_particle, double *L_N, double *L_p, double *L_p2) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define nL (_detector_var.nL)
  #define filename (_detector_var.filename)
  #define xmin (_detector_var.xmin)
  #define xmax (_detector_var.xmax)
  #define ymin (_detector_var.ymin)
  #define ymax (_detector_var.ymax)
  #define xwidth (_detector_var.xwidth)
  #define yheight (_detector_var.yheight)
  #define Lmin (_detector_var.Lmin)
  #define Lmax (_detector_var.Lmax)
  #define restore_neutron (_detector_var.restore_neutron)
  /*  #define L_N (_comp->L_N)
  #define L_p (_comp->L_p)
  #define L_p2 (_comp->L_p2)*/
  //vz=1000;
  PROP_Z0;
  //L_p[0]+=1;
  if (x>xmin && x<xmax && y>ymin && y<ymax)
  {
    double L = (2*PI/V2K)/sqrt(vx*vx + vy*vy + vz*vz);
    int i = floor((L-Lmin)*nL/(Lmax-Lmin));
    if(i >= 0 && i < nL)
    {
      L_N[i]++;
      L_p[i] += p;
      L_p2[i] += p*p;
      SCATTER;
    }
  }
  if (restore_neutron) {
    RESTORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
  }
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_trace */

/* *****************************************************************************
* instrument 'Minimal' TRACE
***************************************************************************** */

#pragma acc routine seq nohost
int raytrace(_class_particle* _particle, double *L_N, double *L_p, double *L_p2) { /* called by mccode_main for Minimal:TRACE */

  /* init variables and counters for TRACE */
  #undef ABSORB0
  #undef ABSORB
  #define ABSORB0 do { DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; return(ABSORBED);} while(0)
  #define ABSORB ABSORB0
  DEBUG_ENTER();
  DEBUG_STATE();
  /* the main iteration loop for one incoming neutron */
  while (!ABSORBED) { /* iterate neutron event until absorbed */
    _class_particle _particle_save;
    /* send neutron event to component instance, one after the other */
    char flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 1) {
      /* begin component arm=Arm() [1] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_arm->_rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _arm->_position_relative),&x, &y, &z);
        } else
          mccoordschange(_arm->_position_relative, _arm->_rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_arm->_name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component arm [1] */
    if (!ABSORBED && _particle->_index == 2) {
      /* begin component source=Source_simple() [2] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_source->_rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _source->_position_relative),&x, &y, &z);
        } else
          mccoordschange(_source->_position_relative, _source->_rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_source->_name);
      DEBUG_STATE();
      class_Source_simple_trace(_source, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component source [2] */
    if (!ABSORBED && _particle->_index == 3) {
      /* begin component coll2=Slit() [3] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_coll2->_rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _coll2->_position_relative),&x, &y, &z);
        } else
          mccoordschange(_coll2->_position_relative, _coll2->_rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_coll2->_name);
      DEBUG_STATE();
      class_Slit_trace(_coll2, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component coll2 [3] */
    if (!ABSORBED && _particle->_index == 4) {
      /* begin component detector=L_monitor() [4] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_detector->_rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _detector->_position_relative),&x, &y, &z);
        } else
          mccoordschange(_detector->_position_relative, _detector->_rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_detector->_name);
      DEBUG_STATE();
      class_L_monitor_trace(_detector, _particle, L_N, L_p, L_p2);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component detector [4] */
    if (_particle->_index > 4)
      ABSORBED++; /* absorbed when passed all components */
  } /* while !ABSORBED */

  DEBUG_LEAVE()
  DEBUG_STATE()

  return(_particle->_index);
} /* raytrace */
#undef x
#undef y
#undef z
#undef vx
#undef vy
#undef vz
#undef t
#undef sx
#undef sy
#undef sz
#undef p
// user variables:

#undef SCATTERED
#undef RESTORE
#undef RESTORE_NEUTRON
#undef STORE_NEUTRON
#undef ABSORBED
#undef ABSORB
#undef ABSORB0
/* *****************************************************************************
* instrument 'Minimal' and components SAVE
***************************************************************************** */

_class_L_monitor *class_L_monitor_save(_class_L_monitor *_comp
) {
  #define nL (_comp->nL)
  #define filename (_comp->filename)
  #define xmin (_comp->xmin)
  #define xmax (_comp->xmax)
  #define ymin (_comp->ymin)
  #define ymax (_comp->ymax)
  #define xwidth (_comp->xwidth)
  #define yheight (_comp->yheight)
  #define Lmin (_comp->Lmin)
  #define Lmax (_comp->Lmax)
  #define restore_neutron (_comp->restore_neutron)
  #define L_N (_comp->L_N)
  #define L_p (_comp->L_p)
  #define L_p2 (_comp->L_p2)
  DETECTOR_OUT_1D(
    "Wavelength monitor",
    "Wavelength [AA]",
    "Intensity",
    "L", Lmin, Lmax, nL,
    &L_N[0],&L_p[0],&L_p2[0],
    filename);
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_save */



int save(FILE *handle) { /* called by mccode_main for Minimal:SAVE */
  if (!handle) siminfo_init(NULL);

  /* call iteratively all components SAVE */



  class_L_monitor_save(_detector);

  if (!handle) siminfo_close(); 

  return(0);
} /* save */

/* *****************************************************************************
* instrument 'Minimal' and components FINALLY
***************************************************************************** */

_class_L_monitor *class_L_monitor_finally(_class_L_monitor *_comp
) {
  #define nL (_comp->nL)
  #define filename (_comp->filename)
  #define xmin (_comp->xmin)
  #define xmax (_comp->xmax)
  #define ymin (_comp->ymin)
  #define ymax (_comp->ymax)
  #define xwidth (_comp->xwidth)
  #define yheight (_comp->yheight)
  #define Lmin (_comp->Lmin)
  #define Lmax (_comp->Lmax)
  #define restore_neutron (_comp->restore_neutron)
  #define L_N (_comp->L_N)
  #define L_p (_comp->L_p)
  #define L_p2 (_comp->L_p2)
  /* destroy_darr1d(L_N); */
  /* destroy_darr1d(L_p); */
  /* destroy_darr1d(L_p2); */
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_finally */



int finally(void) { /* called by mccode_main for Minimal:FINALLY */
  siminfo_init(NULL);
  save(siminfo_file); /* save data when simulation ends */

  /* call iteratively all components FINALLY */



  class_L_monitor_finally(_detector);

  siminfo_close(); 

  return(0);
} /* finally */

/* *****************************************************************************
* instrument 'Minimal' and components DISPLAY
***************************************************************************** */

  #define magnify     mcdis_magnify
  #define line        mcdis_line
  #define dashed_line mcdis_dashed_line
  #define multiline   mcdis_multiline
  #define rectangle   mcdis_rectangle
  #define box         mcdis_box
  #define circle      mcdis_circle
  #define cylinder    mcdis_cylinder
  #define sphere      mcdis_sphere
_class_Arm *class_Arm_display(_class_Arm *_comp
) {
  printf("MCDISPLAY: component %s\n", _comp->_name);
  /* A bit ugly; hard-coded dimensions. */
  
  line(0,0,0,0.2,0,0);
  line(0,0,0,0,0.2,0);
  line(0,0,0,0,0,0.2);
  return(_comp);
} /* class_Arm_display */

_class_Source_simple *class_Source_simple_display(_class_Source_simple *_comp
) {
  #define radius (_comp->radius)
  #define yheight (_comp->yheight)
  #define xwidth (_comp->xwidth)
  #define dist (_comp->dist)
  #define focus_xw (_comp->focus_xw)
  #define focus_yh (_comp->focus_yh)
  #define E0 (_comp->E0)
  #define dE (_comp->dE)
  #define lambda0 (_comp->lambda0)
  #define dlambda (_comp->dlambda)
  #define flux (_comp->flux)
  #define gauss (_comp->gauss)
  #define target_index (_comp->target_index)
  #define pmul (_comp->pmul)
  #define square (_comp->square)
  #define srcArea (_comp->srcArea)
  printf("MCDISPLAY: component %s\n", _comp->_name);
  if (square == 1) {
    
    rectangle("xy",0,0,0,xwidth,yheight);
  } else {
    
    circle("xy",0,0,0,radius);
  }
  if (dist) {
    dashed_line(0,0,0, -focus_xw/2,-focus_yh/2,dist, 4);
    dashed_line(0,0,0,  focus_xw/2,-focus_yh/2,dist, 4);
    dashed_line(0,0,0,  focus_xw/2, focus_yh/2,dist, 4);
    dashed_line(0,0,0, -focus_xw/2, focus_yh/2,dist, 4);
  }
  #undef radius
  #undef yheight
  #undef xwidth
  #undef dist
  #undef focus_xw
  #undef focus_yh
  #undef E0
  #undef dE
  #undef lambda0
  #undef dlambda
  #undef flux
  #undef gauss
  #undef target_index
  #undef pmul
  #undef square
  #undef srcArea
  return(_comp);
} /* class_Source_simple_display */

_class_Slit *class_Slit_display(_class_Slit *_comp
) {
  #define xmin (_comp->xmin)
  #define xmax (_comp->xmax)
  #define ymin (_comp->ymin)
  #define ymax (_comp->ymax)
  #define radius (_comp->radius)
  #define xwidth (_comp->xwidth)
  #define yheight (_comp->yheight)
  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  if (radius == 0) {
    double xw, yh;
    xw = (xmax - xmin)/2.0;
    yh = (ymax - ymin)/2.0;
    multiline(3, xmin-xw, (double)ymax, 0.0,
              (double)xmin, (double)ymax, 0.0,
              (double)xmin, ymax+yh, 0.0);
    multiline(3, xmax+xw, (double)ymax, 0.0,
              (double)xmax, (double)ymax, 0.0,
              (double)xmax, ymax+yh, 0.0);
    multiline(3, xmin-xw, (double)ymin, 0.0,
              (double)xmin, (double)ymin, 0.0,
              (double)xmin, ymin-yh, 0.0);
    multiline(3, xmax+xw, (double)ymin, 0.0,
              (double)xmax, (double)ymin, 0.0,
              (double)xmax, ymin-yh, 0.0);
  } else {
    circle("xy",0,0,0,radius);
  }
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_display */

_class_L_monitor *class_L_monitor_display(_class_L_monitor *_comp
) {
  #define nL (_comp->nL)
  #define filename (_comp->filename)
  #define xmin (_comp->xmin)
  #define xmax (_comp->xmax)
  #define ymin (_comp->ymin)
  #define ymax (_comp->ymax)
  #define xwidth (_comp->xwidth)
  #define yheight (_comp->yheight)
  #define Lmin (_comp->Lmin)
  #define Lmax (_comp->Lmax)
  #define restore_neutron (_comp->restore_neutron)
  #define L_N (_comp->L_N)
  #define L_p (_comp->L_p)
  #define L_p2 (_comp->L_p2)
  printf("MCDISPLAY: component %s\n", _comp->_name);
  multiline(5, (double)xmin, (double)ymin, 0.0,
               (double)xmax, (double)ymin, 0.0,
               (double)xmax, (double)ymax, 0.0,
               (double)xmin, (double)ymax, 0.0,
               (double)xmin, (double)ymin, 0.0);
  #undef nL
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef Lmin
  #undef Lmax
  #undef restore_neutron
  #undef L_N
  #undef L_p
  #undef L_p2
  return(_comp);
} /* class_L_monitor_display */


  #undef magnify
  #undef line
  #undef dashed_line
  #undef multiline
  #undef rectangle
  #undef box
  #undef circle
  #undef cylinder
  #undef sphere

int display(void) { /* called by mccode_main for Minimal:DISPLAY */
  printf("MCDISPLAY: start\n");

  /* call iteratively all components DISPLAY */
  class_Arm_display(_arm);

  class_Source_simple_display(_source);

  class_Slit_display(_coll2);

  class_L_monitor_display(_detector);

  printf("MCDISPLAY: end\n");

  return(0);
} /* display */

void* _getvar_parameters(char* compname)
/* enables settings parameters based use of the GETPAR macro */
{
  /* if (!strcmp(compname, "arm")) return (void *) &(_arm->); */
  /* if (!strcmp(compname, "source")) return (void *) &(_source->); */
  /* if (!strcmp(compname, "coll2")) return (void *) &(_coll2->); */
  /* if (!strcmp(compname, "detector")) return (void *) &(_detector->); */
  return NULL;
}

void* _get_particle_var(char *token, _class_particle *p)
/* enables setpars based use of GET_PARTICLE_DVAR macro and similar */
{
  return 0;
}

#include "_mccode_main.c"
/* End of file "mccode_main.c". */

/* end of generated C code hack_1.c */
