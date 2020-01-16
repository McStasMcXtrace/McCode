/* Automatically generated file. Do not edit.
 * Format:     ANSI C source code
 * Creator:    McStas <http://www.mcstas.org>
 * Instrument: oned_mini.instr (Minimal)
 * Date:       Wed Jan 15 13:37:55 2020
 * File:       oned_mini.c
 * CFLAGS=
 */

#define MCCODE_STRING "McStas 3.0-dev - Jan. 13, 2020"
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

#ifdef DISABLE_TRACE
#undef MC_TRACE_ENABLED
#endif

#ifdef USE_PGI
#undef MC_TRACE_ENABLED
#include <openacc_curand.h>
#endif

struct _struct_particle {
  double x,y,z; /* position [m] */
  double vx,vy,vz; /* velocity [m/s] */
  double sx,sy,sz; /* spin [0-1] */
  unsigned long randstate[7];
  double t, p;    /* time, event weight */
  long long _uid;  /* event ID */
  long _index;     /* component index where to send this event */
  long _absorbed;  /* flag set to TRUE when this event is to be removed/ignored */
  long _scattered; /* flag set to TRUE when this event has interacted with the last component instance */
  long _restore;   /* set to true if neutron event must be restored */
};
typedef struct _struct_particle _class_particle;

_class_particle _particle_global_randnbuse_var;
_class_particle* _particle = &_particle_global_randnbuse_var;

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
char  instrument_source[] = "oned_mini.instr";
char *instrument_exe      = NULL; /* will be set to argv[0] in main */
char  instrument_code[]   = "Instrument Minimal source code oned_mini.instr is not embedded in this executable.\n  Use --source option when running McStas.\n";

int main(int argc, char *argv[]){return mccode_main(argc, argv);}

/* *****************************************************************************
* instrument 'Minimal' and components DECLARE
***************************************************************************** */

/* Instrument parameters: structure and a table for the initialisation
   (Used in e.g. inputparse and I/O function (e.g. detector_out) */

struct _struct_instrument_parameters {
  MCNUM _dummy;
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
  "dummy", &(_instrument_var._parameters._dummy), instr_type_double, "0",
  NULL, NULL, instr_type_double, ""
};


/* ********************** component definition declarations. **************** */

/* component arm=Arm() [1] DECLARE */
/* Parameter definition for component type 'Arm' */
struct _struct_Arm_parameters {
  char Arm_has_no_parameters;
}; /* _struct_Arm_parameters */
typedef struct _struct_Arm_parameters _class_Arm_parameters;

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
  _class_Arm_parameters _parameters;
};
typedef struct _struct_Arm _class_Arm;
_class_Arm _arm_var;
#pragma acc declare create ( _arm_var )

/* component source=Source_simple() [2] DECLARE */
/* Parameter definition for component type 'Source_simple' */
struct _struct_Source_simple_parameters {
  /* Component type 'Source_simple' setting parameters */
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
}; /* _struct_Source_simple_parameters */
typedef struct _struct_Source_simple_parameters _class_Source_simple_parameters;

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
  _class_Source_simple_parameters _parameters;
};
typedef struct _struct_Source_simple _class_Source_simple;
_class_Source_simple _source_var;
#pragma acc declare create ( _source_var )

/* component coll2=Slit() [3] DECLARE */
/* Parameter definition for component type 'Slit' */
struct _struct_Slit_parameters {
  /* Component type 'Slit' setting parameters */
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM radius;
  MCNUM xwidth;
  MCNUM yheight;
}; /* _struct_Slit_parameters */
typedef struct _struct_Slit_parameters _class_Slit_parameters;

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
  _class_Slit_parameters _parameters;
};
typedef struct _struct_Slit _class_Slit;
_class_Slit _coll2_var;
#pragma acc declare create ( _coll2_var )

/* component detector=L_monitor() [4] DECLARE */
/* Parameter definition for component type 'L_monitor' */
struct _struct_L_monitor_parameters {
  /* Component type 'L_monitor' setting parameters */
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
  DArray1d L_N;
  DArray1d L_p;
  DArray1d L_p2;
}; /* _struct_L_monitor_parameters */
typedef struct _struct_L_monitor_parameters _class_L_monitor_parameters;
double* g_L_N;
double* g_L_p;
double* g_L_p2;
#pragma acc declare create(g_L_N)
#pragma acc declare create(g_L_p)
#pragma acc declare create(g_L_p2)

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
  _class_L_monitor_parameters _parameters;
};
typedef struct _struct_L_monitor _class_L_monitor;
_class_L_monitor _detector_var;
#pragma acc declare create ( _detector_var )

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
  stracpy(_arm_var._name, "arm", 16384);
  stracpy(_arm_var._type, "Arm", 16384);
  _arm_var._index=1;
  /* component arm=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(_arm_var._rotation_absolute,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_copy(_arm_var._rotation_relative, _arm_var._rotation_absolute);
    _arm_var._rotation_is_identity =  rot_test_identity(_arm_var._rotation_relative);
    _arm_var._position_absolute = coords_set(
      0, 0, 0);
    tc1 = coords_neg(_arm_var._position_absolute);
    _arm_var._position_relative = rot_apply(_arm_var._rotation_absolute, tc1);
  } /* arm=Arm() AT ROTATED */
  DEBUG_COMPONENT("arm", _arm_var._position_absolute, _arm_var._rotation_absolute);
  instrument->_position_absolute[1] = _arm_var._position_absolute;
  instrument->_position_relative[1] = _arm_var._position_relative;
  instrument->counter_N[1]  = instrument->counter_P[1] = instrument->counter_P2[1] = 0;
  instrument->counter_AbsorbProp[1]= 0;
  return(0);
} /* _arm_setpos */

/* component source=Source_simple() SETTING, POSITION/ROTATION */
int _source_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_source_setpos] component source=Source_simple() SETTING [/usr/share/mcstas/3.0-dev/sources/Source_simple.comp:64]");
  stracpy(_source_var._name, "source", 16384);
  stracpy(_source_var._type, "Source_simple", 16384);
  _source_var._index=2;
  _source_var._parameters.radius = 0.02;
  #define radius (_source_var._parameters.radius)
  _source_var._parameters.yheight = 0;
  #define yheight (_source_var._parameters.yheight)
  _source_var._parameters.xwidth = 0;
  #define xwidth (_source_var._parameters.xwidth)
  _source_var._parameters.dist = 3;
  #define dist (_source_var._parameters.dist)
  _source_var._parameters.focus_xw = 0.01;
  #define focus_xw (_source_var._parameters.focus_xw)
  _source_var._parameters.focus_yh = 0.01;
  #define focus_yh (_source_var._parameters.focus_yh)
  _source_var._parameters.E0 = 0;
  #define E0 (_source_var._parameters.E0)
  _source_var._parameters.dE = 0;
  #define dE (_source_var._parameters.dE)
  _source_var._parameters.lambda0 = 6.0;
  #define lambda0 (_source_var._parameters.lambda0)
  _source_var._parameters.dlambda = 0.05;
  #define dlambda (_source_var._parameters.dlambda)
  _source_var._parameters.flux = 1e8;
  #define flux (_source_var._parameters.flux)
  _source_var._parameters.gauss = 0;
  #define gauss (_source_var._parameters.gauss)
  _source_var._parameters.target_index = + 1;
  #define target_index (_source_var._parameters.target_index)

  #define pmul (_source_var._parameters.pmul)
  #define square (_source_var._parameters.square)
  #define srcArea (_source_var._parameters.srcArea)

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
    rot_mul(tr1, _arm_var._rotation_absolute, _source_var._rotation_absolute);
    rot_transpose(_arm_var._rotation_absolute, tr1);
    rot_mul(_source_var._rotation_absolute, tr1, _source_var._rotation_relative);
    _source_var._rotation_is_identity =  rot_test_identity(_source_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_arm_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _source_var._position_absolute = coords_add(_arm_var._position_absolute, tc2);
    tc1 = coords_sub(_arm_var._position_absolute, _source_var._position_absolute);
    _source_var._position_relative = rot_apply(_source_var._rotation_absolute, tc1);
  } /* source=Source_simple() AT ROTATED */
  DEBUG_COMPONENT("source", _source_var._position_absolute, _source_var._rotation_absolute);
  instrument->_position_absolute[2] = _source_var._position_absolute;
  instrument->_position_relative[2] = _source_var._position_relative;
  instrument->counter_N[2]  = instrument->counter_P[2] = instrument->counter_P2[2] = 0;
  instrument->counter_AbsorbProp[2]= 0;
  return(0);
} /* _source_setpos */

/* component coll2=Slit() SETTING, POSITION/ROTATION */
int _coll2_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_coll2_setpos] component coll2=Slit() SETTING [/usr/share/mcstas/3.0-dev/optics/Slit.comp:47]");
  stracpy(_coll2_var._name, "coll2", 16384);
  stracpy(_coll2_var._type, "Slit", 16384);
  _coll2_var._index=3;
  _coll2_var._parameters.xmin = -0.01;
  #define xmin (_coll2_var._parameters.xmin)
  _coll2_var._parameters.xmax = 0.01;
  #define xmax (_coll2_var._parameters.xmax)
  _coll2_var._parameters.ymin = -0.01;
  #define ymin (_coll2_var._parameters.ymin)
  _coll2_var._parameters.ymax = 0.01;
  #define ymax (_coll2_var._parameters.ymax)
  _coll2_var._parameters.radius = 0.01;
  #define radius (_coll2_var._parameters.radius)
  _coll2_var._parameters.xwidth = 0;
  #define xwidth (_coll2_var._parameters.xwidth)
  _coll2_var._parameters.yheight = 0;
  #define yheight (_coll2_var._parameters.yheight)

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
    rot_mul(tr1, _arm_var._rotation_absolute, _coll2_var._rotation_absolute);
    rot_transpose(_source_var._rotation_absolute, tr1);
    rot_mul(_coll2_var._rotation_absolute, tr1, _coll2_var._rotation_relative);
    _coll2_var._rotation_is_identity =  rot_test_identity(_coll2_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 6);
    rot_transpose(_arm_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _coll2_var._position_absolute = coords_add(_arm_var._position_absolute, tc2);
    tc1 = coords_sub(_source_var._position_absolute, _coll2_var._position_absolute);
    _coll2_var._position_relative = rot_apply(_coll2_var._rotation_absolute, tc1);
  } /* coll2=Slit() AT ROTATED */
  DEBUG_COMPONENT("coll2", _coll2_var._position_absolute, _coll2_var._rotation_absolute);
  instrument->_position_absolute[3] = _coll2_var._position_absolute;
  instrument->_position_relative[3] = _coll2_var._position_relative;
  instrument->counter_N[3]  = instrument->counter_P[3] = instrument->counter_P2[3] = 0;
  instrument->counter_AbsorbProp[3]= 0;
  return(0);
} /* _coll2_setpos */

/* component detector=L_monitor() SETTING, POSITION/ROTATION */
int _detector_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_detector_setpos] component detector=L_monitor() SETTING [/usr/share/mcstas/3.0-dev/monitors/L_monitor.comp:65]");
  stracpy(_detector_var._name, "detector", 16384);
  stracpy(_detector_var._type, "L_monitor", 16384);
  _detector_var._index=4;
  _detector_var._parameters.nL = 128;
  #define nL (_detector_var._parameters.nL)
  if("WL.dat" && strlen("WL.dat"))
    stracpy(_detector_var._parameters.filename, "WL.dat" ? "WL.dat" : "", 16384);
  else
  _detector_var._parameters.filename[0]='\0';
  #define filename (_detector_var._parameters.filename)
  _detector_var._parameters.xmin = -0.1;
  #define xmin (_detector_var._parameters.xmin)
  _detector_var._parameters.xmax = 0.1;
  #define xmax (_detector_var._parameters.xmax)
  _detector_var._parameters.ymin = -0.1;
  #define ymin (_detector_var._parameters.ymin)
  _detector_var._parameters.ymax = 0.1;
  #define ymax (_detector_var._parameters.ymax)
  _detector_var._parameters.xwidth = 0;
  #define xwidth (_detector_var._parameters.xwidth)
  _detector_var._parameters.yheight = 0;
  #define yheight (_detector_var._parameters.yheight)
  _detector_var._parameters.Lmin = 5.90;
  #define Lmin (_detector_var._parameters.Lmin)
  _detector_var._parameters.Lmax = 6.1;
  #define Lmax (_detector_var._parameters.Lmax)
  _detector_var._parameters.restore_neutron = 0;
  #define restore_neutron (_detector_var._parameters.restore_neutron)

  #define L_N (_detector_var._parameters.L_N)
  #define L_p (_detector_var._parameters.L_p)
  #define L_p2 (_detector_var._parameters.L_p2)

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
    rot_mul(tr1, _arm_var._rotation_absolute, _detector_var._rotation_absolute);
    rot_transpose(_coll2_var._rotation_absolute, tr1);
    rot_mul(_detector_var._rotation_absolute, tr1, _detector_var._rotation_relative);
    _detector_var._rotation_is_identity =  rot_test_identity(_detector_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 9.01);
    rot_transpose(_arm_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _detector_var._position_absolute = coords_add(_arm_var._position_absolute, tc2);
    tc1 = coords_sub(_coll2_var._position_absolute, _detector_var._position_absolute);
    _detector_var._position_relative = rot_apply(_detector_var._rotation_absolute, tc1);
  } /* detector=L_monitor() AT ROTATED */
  DEBUG_COMPONENT("detector", _detector_var._position_absolute, _detector_var._rotation_absolute);
  instrument->_position_absolute[4] = _detector_var._position_absolute;
  instrument->_position_relative[4] = _detector_var._position_relative;
  instrument->counter_N[4]  = instrument->counter_P[4] = instrument->counter_P2[4] = 0;
  instrument->counter_AbsorbProp[4]= 0;
  return(0);
} /* _detector_setpos */

_class_Source_simple *class_Source_simple_init(_class_Source_simple *_comp
) {
  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define dist (_comp->_parameters.dist)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define E0 (_comp->_parameters.E0)
  #define dE (_comp->_parameters.dE)
  #define lambda0 (_comp->_parameters.lambda0)
  #define dlambda (_comp->_parameters.dlambda)
  #define flux (_comp->_parameters.flux)
  #define gauss (_comp->_parameters.gauss)
  #define target_index (_comp->_parameters.target_index)
  #define pmul (_comp->_parameters.pmul)
  #define square (_comp->_parameters.square)
  #define srcArea (_comp->_parameters.srcArea)
  SIG_MESSAGE("[_source_init] component source=Source_simple() INITIALISE [/usr/share/mcstas/3.0-dev/sources/Source_simple.comp:64]");
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
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_coll2_init] component coll2=Slit() INITIALISE [/usr/share/mcstas/3.0-dev/optics/Slit.comp:47]");
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
  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_detector_init] component detector=L_monitor() INITIALISE [/usr/share/mcstas/3.0-dev/monitors/L_monitor.comp:65]");
  if (xwidth  > 0) { xmax = xwidth/2;  xmin = -xmax; }
  if (yheight > 0) { ymax = yheight/2; ymin = -ymax; }

  if ((xmin >= xmax) || (ymin >= ymax)) {
    printf("L_monitor: %s: Null detection area !\n"
      "ERROR      (xwidth,yheight,xmin,xmax,ymin,ymax). Exiting",
      NAME_CURRENT_COMP);
    exit(0);
  }

  g_L_N = calloc(nL, sizeof(double));
  g_L_p = calloc(nL, sizeof(double));
  g_L_p2 = calloc(nL, sizeof(double));

  int i;
  for (i=0; i<nL; i++)
  {
    g_L_N[i] = 0;
    g_L_p[i] = 0;
    g_L_p2[i] = 0;
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

  class_Source_simple_init(&_source_var);

  class_Slit_init(&_coll2_var);

  class_L_monitor_init(&_detector_var);

  if (mcdotrace) display();
  DEBUG_INSTR_END();

#ifdef USE_PGI
#  include <openacc.h>
acc_attach( (void*)&_arm_var );
acc_attach( (void*)&_source_var );
acc_attach( (void*)&_coll2_var );
acc_attach( (void*)&_detector_var );
#pragma acc update device(_arm_var)
#pragma acc update device(_source_var)
#pragma acc update device(_coll2_var)
#pragma acc update device(_detector_var)

//#define _nL (_detector_var._parameters.nL)
#pragma acc enter data create(g_L_N[0:128])
#pragma acc enter data create(g_L_p[0:128])
#pragma acc enter data create(g_L_p2[0:128])

acc_attach( (void*)&_instrument_var );
#pragma acc update device(_instrument_var)
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

/* if on GPU, nullify sprintf,fprintf,printfs */
#ifdef USE_PGI
/*#define fprintf(stderr,...) printf(__VA_ARGS__)
#define sprintf(string,...) printf(__VA_ARGS__)
#define printf(...) noprintf()
#define exit(...) noprintf()
*/
#define strcmp(a,b) str_comp(a,b)
#define strlen(a) str_len(a)
#endif
#define SCATTERED (_particle->_scattered)
#define RESTORE (_particle->_restore)
#define RESTORE_NEUTRON(_index, ...) _particle->_restore = _index;
#define ABSORBED (_particle->_absorbed)
#define ABSORB0 do { DEBUG_STATE(); DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; return(_comp); } while(0)
#define ABSORB ABSORB0
#pragma acc routine seq
_class_Source_simple *class_Source_simple_trace(_class_Source_simple *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define dist (_comp->_parameters.dist)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define E0 (_comp->_parameters.E0)
  #define dE (_comp->_parameters.dE)
  #define lambda0 (_comp->_parameters.lambda0)
  #define dlambda (_comp->_parameters.dlambda)
  #define flux (_comp->_parameters.flux)
  #define gauss (_comp->_parameters.gauss)
  #define target_index (_comp->_parameters.target_index)
  #define pmul (_comp->_parameters.pmul)
  #define square (_comp->_parameters.square)
  #define srcArea (_comp->_parameters.srcArea)
  SIG_MESSAGE("[_source_trace] component source=Source_simple() TRACE [/usr/share/mcstas/3.0-dev/sources/Source_simple.comp:120]");
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

#pragma acc routine seq
_class_Slit *class_Slit_trace(_class_Slit *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_coll2_trace] component coll2=Slit() TRACE [/usr/share/mcstas/3.0-dev/optics/Slit.comp:56]");
    mcPROP_Z0;
    if (((radius == 0) && (x<xmin || x>xmax || y<ymin || y>ymax))
    || ((radius != 0) && (x*x + y*y > radius*radius)))
      ABSORB;
    else
        SCATTER;
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_trace */

#pragma acc routine seq
_class_L_monitor *class_L_monitor_trace(_class_L_monitor *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;

  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_detector_trace] component detector=L_monitor() TRACE [/usr/share/mcstas/3.0-dev/monitors/L_monitor.comp:90]");
  PROP_Z0;
  if (x>xmin && x<xmax && y>ymin && y<ymax)
  {
    double L = (2*PI/V2K)/sqrt(vx*vx + vy*vy + vz*vz);
    int i = floor((L-Lmin)*nL/(Lmax-Lmin));
    if(i >= 0 && i < nL)
    {
      double p2 = p*p;
      #pragma acc atomic
      g_L_N[i] = g_L_N[i] + 1;
      #pragma acc atomic
      g_L_p[i] = g_L_p[i] + p;
      #pragma acc atomic
      g_L_p2[i] = g_L_p2[i] + p2;
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

#pragma acc routine seq
int raytrace(_class_particle* _particle) { /* single event propagation, called by mccode_main for Minimal:TRACE */

  /* init variables and counters for TRACE */
  #undef ABSORB0
  #undef ABSORB
  #define ABSORB0 do { DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; return(ABSORBED);} while(0)
  #define ABSORB ABSORB0
  DEBUG_ENTER();
  DEBUG_STATE();
  /* the main iteration loop for one incoming event */
  while (!ABSORBED) { /* iterate event until absorbed */
    _class_particle _particle_save;
    /* send particle event to component instance, one after the other */
    char flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 1) {
      /* begin component arm=Arm() [1] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_arm_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _arm_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_arm_var._position_relative, _arm_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_arm_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component arm [1] */
    if (!ABSORBED && _particle->_index == 2) {
      /* begin component source=Source_simple() [2] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_source_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _source_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_source_var._position_relative, _source_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_source_var._name);
      DEBUG_STATE();
      class_Source_simple_trace(&_source_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component source [2] */
    if (!ABSORBED && _particle->_index == 3) {
      /* begin component coll2=Slit() [3] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_coll2_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _coll2_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_coll2_var._position_relative, _coll2_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_coll2_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_coll2_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component coll2 [3] */
    if (!ABSORBED && _particle->_index == 4) {
      /* begin component detector=L_monitor() [4] */
      if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
        if (_detector_var._rotation_is_identity) {
          coords_get(coords_add(coords_set(x,y,z), _detector_var._position_relative),&x, &y, &z);
        } else
          mccoordschange(_detector_var._position_relative, _detector_var._rotation_relative, _particle);
      } else flag_nocoordschange=0;
      _particle_save = *_particle;
      DEBUG_COMP(_detector_var._name);
      DEBUG_STATE();
      class_L_monitor_trace(&_detector_var, _particle);
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

/* loop to generate events and call raytrace() propagate them */
void raytrace_all(unsigned long long ncount, unsigned long seed) {

  /* CPU-loop */
  unsigned long long loops;
  long innerloop=2147483647;
  loops = ceil((double)ncount/innerloop);
  if (ncount>innerloop) {
    printf("Defining %llu CPU loops around kernel and adjusting ncount\n",loops);
    mcset_ncount(loops*2147483647);
  } else {
    loops=1;
    innerloop = ncount;
  }

  for (unsigned long long cloop=0; cloop<loops; cloop++) {
    if (loops>1) fprintf(stdout, "%d..", (int)cloop); fflush(stdout);

    #pragma acc parallel loop
    for (unsigned long pidx=0 ; pidx < innerloop ; pidx++) {
      _class_particle particleN = mcgenstate(); // initial particle
      _class_particle* _particle = &particleN;
      particleN._uid = pidx;

      long seq = pidx + seed;
      srandom(_hash(pidx + seed));

      raytrace(_particle);
    } /* inner for */
    seed = seed+innerloop;
  } /* CPU for */
  printf("\n");
} /* raytrace_all */

_class_particle* particles;
#pragma acc declare device_resident(particles)

// Alternative raytrace algorithm which "funnels" particles through
// the instrument, enabling a higher degree of parallelization.
void raytrace_all_funnel(unsigned long long ncount, unsigned long seed) {

  // set up outer (CPU) loop / particle batches
  unsigned long long loops;
  long innerloop=1024*1024*10;//2147483647;
  loops = ceil((double)ncount/innerloop);
  if (ncount>innerloop) {
    printf("Defining %llu CPU loops around kernel and adjusting ncount\n",loops);
    mcset_ncount(loops*2147483647);
  } else {
    loops=1;
    innerloop = ncount;
  }

  // outer loop / particle batches
  for (unsigned long long cloop=0; cloop<loops; cloop++) {
    if (loops>1) fprintf(stdout, "%d..", (int)cloop); fflush(stdout);

    // create particles memory block and pointer array (buffer and sorted)
    //_class_particle* 
    particles = acc_malloc(innerloop*sizeof(_class_particle));
    #pragma acc enter data create(particles[0:innerloop])
    // TODO: _class_particle** psorted = malloc(innerloop*sizeof(_class_particle*));
    // TODO: _class_particle** pbuffer = malloc(innerloop*sizeof(_class_particle*));

    // TODO: do we need a GPU data section for the above buffers here?

    // set up, generate particles
    #pragma acc kernels present(particles)
    for (unsigned long pidx=0 ; pidx < innerloop ; pidx++) {
      // generate particle state, set loop index and seed
      particles[pidx] = mcgenstate();
      /*_class_particle* _particle = particles + pidx;
      _particle->_uid = pidx;
      srandom(_hash(pidx + seed)); // _particle->state usage built into srandom macro
      */
    }
    // iterate components


    //TODO: innerloop = sort_absorb_last(particles, innerloop);

    // arm
   
    #pragma acc kernels present(particles)
    for (unsigned long pidx=0 ; pidx < innerloop ; pidx++) {
      _class_particle* _particle = particles + pidx;
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_arm_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _arm_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_arm_var._position_relative, _arm_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }
    }

    //TODO: innerloop = sort_absorb_last(particles, innerloop);≈


    // source
    #pragma acc kernels present(particles)
    for (unsigned long pidx=0 ; pidx < innerloop ; pidx++) {
      _class_particle* _particle = particles + pidx;
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_source_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _source_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_source_var._position_relative, _source_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Source_simple_trace(&_source_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //TODO: innerloop = sort_absorb_last(particles, innerloop);

    // coll2
    #pragma acc kernels present(particles)
    for (unsigned long pidx=0 ; pidx < innerloop ; pidx++) {
      _class_particle* _particle = particles + pidx;
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_coll2_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _coll2_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_coll2_var._position_relative, _coll2_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_coll2_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    //TODO: innerloop = sort_absorb_last(particles, innerloop);

    // detector
    #pragma acc kernels present(particles)
    for (unsigned long pidx=0 ; pidx < innerloop ; pidx++) {
      _class_particle* _particle = particles + pidx;
      _class_particle _particle_save;
      if (!ABSORBED) {
        if (_detector_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _detector_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_detector_var._position_relative, _detector_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_L_monitor_trace(&_detector_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }
    }

    // finalize particle batch
    #pragma acc kernels present(particles)
    for (unsigned long pidx=0 ; pidx < innerloop ; pidx++) {
      _class_particle* _particle = particles + pidx;
      if (_particle->_index > 4)
        ABSORBED++; /* absorbed when passed all components */
    }
    // jump to next viable seed

    seed = seed+innerloop;
    //#pragma acc exit data delete(particles)
    acc_free(particles);
  } // outer loop / particle batches

  printf("\n");
} /* raytrace_all_funnel */

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

#ifdef USE_PGI
#undef strlen
#undef strcmp
#undef exit
#undef printf
#undef sprintf
#undef fprintf
#endif
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
  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_detector_save] component detector=L_monitor() SAVE [/usr/share/mcstas/3.0-dev/monitors/L_monitor.comp:114]");
  DETECTOR_OUT_1D(
    "Wavelength monitor",
    "Wavelength [AA]",
    "Intensity",
    "L", Lmin, Lmax, nL,
    &g_L_N[0],
    &g_L_p[0],
    &g_L_p2[0],
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



  class_L_monitor_save(&_detector_var);

  if (!handle) siminfo_close();

  return(0);
} /* save */

/* *****************************************************************************
* instrument 'Minimal' and components FINALLY
***************************************************************************** */

_class_L_monitor *class_L_monitor_finally(_class_L_monitor *_comp
) {
  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_detector_finally] component detector=L_monitor() FINALLY [/usr/share/mcstas/3.0-dev/monitors/L_monitor.comp:125]");
  free(g_L_N);
  free(g_L_p);
  free(g_L_p2);
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

#pragma acc update host(_arm_var)
#pragma acc update host(_source_var)
#pragma acc update host(_coll2_var)
#pragma acc update host(_detector_var)

#define _nL (_detector_var._parameters.nL)
#pragma acc exit data copyout(g_L_N[0:128])
#pragma acc exit data copyout(g_L_p[0:128])
#pragma acc exit data copyout(g_L_p2[0:128])

#pragma acc update host(_instrument_var)

  siminfo_init(NULL);
  save(siminfo_file); /* save data when simulation ends */

  /* call iteratively all components FINALLY */



  class_L_monitor_finally(&_detector_var);

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
  SIG_MESSAGE("[_arm_display] component arm=Arm() DISPLAY [/usr/share/mcstas/3.0-dev/optics/Arm.comp:40]");
  printf("MCDISPLAY: component %s\n", _comp->_name);
  /* A bit ugly; hard-coded dimensions. */

  line(0,0,0,0.2,0,0);
  line(0,0,0,0,0.2,0);
  line(0,0,0,0,0,0.2);
  return(_comp);
} /* class_Arm_display */

_class_Source_simple *class_Source_simple_display(_class_Source_simple *_comp
) {
  #define radius (_comp->_parameters.radius)
  #define yheight (_comp->_parameters.yheight)
  #define xwidth (_comp->_parameters.xwidth)
  #define dist (_comp->_parameters.dist)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define E0 (_comp->_parameters.E0)
  #define dE (_comp->_parameters.dE)
  #define lambda0 (_comp->_parameters.lambda0)
  #define dlambda (_comp->_parameters.dlambda)
  #define flux (_comp->_parameters.flux)
  #define gauss (_comp->_parameters.gauss)
  #define target_index (_comp->_parameters.target_index)
  #define pmul (_comp->_parameters.pmul)
  #define square (_comp->_parameters.square)
  #define srcArea (_comp->_parameters.srcArea)
  SIG_MESSAGE("[_source_display] component source=Source_simple() DISPLAY [/usr/share/mcstas/3.0-dev/sources/Source_simple.comp:166]");
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
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_coll2_display] component coll2=Slit() DISPLAY [/usr/share/mcstas/3.0-dev/optics/Slit.comp:66]");
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
  #define nL (_comp->_parameters.nL)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define Lmin (_comp->_parameters.Lmin)
  #define Lmax (_comp->_parameters.Lmax)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define L_N (_comp->_parameters.L_N)
  #define L_p (_comp->_parameters.L_p)
  #define L_p2 (_comp->_parameters.L_p2)
  SIG_MESSAGE("[_detector_display] component detector=L_monitor() DISPLAY [/usr/share/mcstas/3.0-dev/monitors/L_monitor.comp:132]");
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
  class_Arm_display(&_arm_var);

  class_Source_simple_display(&_source_var);

  class_Slit_display(&_coll2_var);

  class_L_monitor_display(&_detector_var);

  printf("MCDISPLAY: end\n");

  return(0);
} /* display */

void* _getvar_parameters(char* compname)
/* enables settings parameters based use of the GETPAR macro */
{
  if (!strcmp(compname, "arm")) return (void *) &(_arm_var._parameters);
  if (!strcmp(compname, "source")) return (void *) &(_source_var._parameters);
  if (!strcmp(compname, "coll2")) return (void *) &(_coll2_var._parameters);
  if (!strcmp(compname, "detector")) return (void *) &(_detector_var._parameters);
}

void* _get_particle_var(char *token, _class_particle *p)
/* enables setpars based use of GET_PARTICLE_DVAR macro and similar */
{
  return 0;
}

#include "_mccode_main.c"
/* End of file "mccode_main.c". */

/* end of generated C code oned_mini.c */
