/* Automatically generated file. Do not edit. 
 * Format:     ANSI C source code
 * Creator:    McStas <http://www.mcstas.org>
 * Instrument: BNL_H8.instr (BNL_H8)
 * Date:       Tue Oct  5 15:04:47 2021
 * File:       ./BNL_H8.c
 * CFLAGS=
 */

#define MCCODE_STRING "McStas 3.0-dev - Oct. 05, 2021"
#define FLAVOR        "mcstas"
#define FLAVOR_UPPER  "MCSTAS"

#define MC_USE_DEFAULT_MAIN
#define MC_TRACE_ENABLED

#include <string.h>

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

typedef double MCNUM;
typedef struct {MCNUM x, y, z;} Coords;
typedef MCNUM Rotation[3][3];
#define MCCODE_BASE_TYPES

#ifdef OPENACC
#undef MC_TRACE_ENABLED
#endif

#ifndef MC_NUSERVAR
#define MC_NUSERVAR 10
#endif

struct _struct_particle {
  double x,y,z; /* position [m] */
  double vx,vy,vz; /* velocity [m/s] */
  double sx,sy,sz; /* spin [0-1] */
  int mcgravitation; /* gravity-state */
  void *mcMagnet;    /* precession-state */
  int allow_backprop; /* allow backprop */
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

#pragma acc routine
_class_particle mcgenstate(void);
#pragma acc routine
_class_particle mcsetstate(double x, double y, double z, double vx, double vy, double vz,
			   double t, double sx, double sy, double sz, double p, int mcgravitation, void *mcMagnet, int mcallowbackprop);

extern int mcgravitation;      /* flag to enable gravitation */
#pragma acc declare create ( mcgravitation )
int mcallowbackprop;        
#pragma acc declare create ( mcallowbackprop )

_class_particle mcgenstate(void) {
  _class_particle particle = mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, mcgravitation, NULL, mcallowbackprop);
  return(particle);
}
/*Generated user variable handlers:*/

#pragma acc routine
double particle_getvar(_class_particle *p, char *name, int *suc);

#ifdef OPENACC
#pragma acc routine
int str_comp(char *str1, char *str2);
#endif

double particle_getvar(_class_particle *p, char *name, int *suc){
#ifndef OPENACC
#define str_comp strcmp
#endif
  int s=1;
  double rval=0;
  if(!str_comp("x",name)){rval=p->x;s=0;}
  if(!str_comp("y",name)){rval=p->y;s=0;}
  if(!str_comp("z",name)){rval=p->z;s=0;}
  if(!str_comp("vx",name)){rval=p->vx;s=0;}
  if(!str_comp("vy",name)){rval=p->vy;s=0;}
  if(!str_comp("vz",name)){rval=p->vz;s=0;}
  if(!str_comp("sx",name)){rval=p->sx;s=0;}
  if(!str_comp("sy",name)){rval=p->sy;s=0;}
  if(!str_comp("sz",name)){rval=p->sz;s=0;}
  if(!str_comp("t",name)){rval=p->t;s=0;}
  if(!str_comp("p",name)){rval=p->p;s=0;}
  if (suc!=0x0) {*suc=s;}
  return rval;
}

#pragma acc routine
double particle_getuservar_byid(_class_particle *p, int id, int *suc){
  int s=1;
  double rval=0;
  switch(id){
  }
  if (suc!=0x0) {*suc=s;}
  return rval;
}

#pragma acc routine
void particle_uservar_init(_class_particle *p){
}

#define MC_EMBEDDED_RUNTIME
/* embedding file "mccode-r.h" */

/*******************************************************************************
*
* McCode, neutron/xray ray-tracing package
*         Copyright (C) 1997-2009, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/mccode-r.h
*
* %Identification
* Written by: KN
* Date:    Aug 29, 1997
* Release: McStas 3.0-dev
* Version: $Revision$
*
* Runtime system header for McStas/McXtrace.
*
* In order to use this library as an external library, the following variables
* and macros must be declared (see details in the code)
*
*   struct mcinputtable_struct mcinputtable[];
*   int numipar;
*   char instrument_name[], instrument_source[];
*   int traceenabled, defaultmain;
*   extern MCNUM  mccomp_storein[];
*   extern MCNUM  mcAbsorbProp[];
*   extern MCNUM  mcScattered;
*   #define MCCODE_STRING "the McStas/McXtrace version"
*
* Usage: Automatically embbeded in the c code.
*
* $Id$
*
*******************************************************************************/

#ifndef MCCODE_R_H
#define MCCODE_R_H "$Revision$"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <limits.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <float.h>
#include <inttypes.h>
#include <stdint.h>
#ifdef OPENACC
#include <openacc.h>
#ifndef GCCOFFLOAD
#include <accelmath.h>
#else
#include <math.h>
#endif
#pragma acc routine
int noprintf();
#pragma acc routine
size_t str_len(const char *s);
#else
#include <math.h>
#endif

/* If the runtime is embedded in the simulation program, some definitions can
   be made static. */

#ifdef MC_EMBEDDED_RUNTIME
#  define mcstatic
#else
#  define mcstatic
#endif

#ifdef __dest_os
#  if (__dest_os == __mac_os)
#    define MAC
#  endif
#endif

#ifdef __FreeBSD__
#  define NEED_STAT_H
#endif

#if defined(__APPLE__) && defined(__GNUC__)
#  define NEED_STAT_H
#endif

#ifdef NEED_STAT_H
#  include <sys/stat.h>
#endif

#ifndef MC_PATHSEP_C
#  ifdef WIN32
#    define MC_PATHSEP_C '\\'
#    define MC_PATHSEP_S "\\"
#  else  /* !WIN32 */
#    define MC_PATHSEP_C '/'
#    define MC_PATHSEP_S "/"
#  endif /* !WIN32 */
#endif /* MC_PATHSEP_C */



/* the version string is replaced when building distribution with mkdist */
#ifndef MCCODE_STRING
#  define MCCODE_STRING "McStas 3.0-dev - Oct. 05, 2021"
#endif

#ifndef MCCODE_DATE
#  define MCCODE_DATE "Oct. 05, 2021"
#endif

#ifndef MCCODE_VERSION
#  define MCCODE_VERSION "3.0-dev"
#endif

#ifndef MCCODE_NAME
#  define MCCODE_NAME "McStas"
#endif

#ifndef MCCODE_PARTICLE
#  define MCCODE_PARTICLE "neutron"
#endif

#ifndef MCCODE_PARTICLE_CODE
#  define MCCODE_PARTICLE_CODE 2112
#endif

#ifndef MCCODE_LIBENV
#  define MCCODE_LIBENV "MCSTAS"
#endif

#ifndef FLAVOR_UPPER
#  define FLAVOR_UPPER MCCODE_NAME
#endif

#ifdef MC_PORTABLE
#  ifndef NOSIGNALS
#    define NOSIGNALS 1
#  endif
#endif

#ifdef MAC
#  ifndef NOSIGNALS
#    define NOSIGNALS 1
#  endif
#endif

#if (USE_MPI == 0)
#  undef USE_MPI
#endif

#ifdef USE_MPI  /* default is to disable signals with MPI, as MPICH uses them to communicate */
#  ifndef NOSIGNALS
#    define NOSIGNALS 1
#  endif
#endif

#ifdef OPENACC  /* default is to disable signals with PGI/OpenACC */
#  ifndef NOSIGNALS
#    define NOSIGNALS 1
#  endif
#endif

#ifndef OPENACC
#  ifndef USE_OFF  /* default is to enable OFF when not using PGI/OpenACC */
#    define USE_OFF
#  endif
#  ifndef CPUFUNNEL  /* allow to enable FUNNEL-mode on CPU */
#  ifdef FUNNEL      /* by default disable FUNNEL-mode when not using PGI/OpenACC */
#    undef FUNNEL
#  endif
#  endif
#endif

#if (NOSIGNALS == 0)
#  undef NOSIGNALS
#endif

/* Note: the enum instr_formal_types definition MUST be kept
   synchronized with the one in mccode.h and with the
   instr_formal_type_names array in cogen.c. */
enum instr_formal_types
  {
    instr_type_int,
    instr_type_string, instr_type_char,
    instr_type_vector, instr_type_double
  };
struct mcinputtable_struct { /* defines instrument parameters */
  char *name; /* name of parameter */
  void *par;  /* pointer to instrument parameter (variable) */
  enum instr_formal_types type;
  char *val;  /* default value */
};

#ifndef MCCODE_BASE_TYPES
typedef double MCNUM;
typedef struct {MCNUM x, y, z;} Coords;
typedef MCNUM Rotation[3][3];
#endif

/* the following variables are defined in the McStas generated C code
   but should be defined externally in case of independent library usage */
#ifndef DANSE
extern struct mcinputtable_struct mcinputtable[];         /* list of instrument parameters */
extern int    numipar;                                    /* number of instrument parameters */
extern char   instrument_name[], instrument_source[]; /* instrument name and filename */
extern char  *instrument_exe;                           /* executable path = argv[0] or NULL */
extern char   instrument_code[];                        /* contains the initial 'instr' file */

#ifndef MC_ANCIENT_COMPATIBILITY
extern int traceenabled, defaultmain;
#endif
#endif


/* Useful macros ============================================================ */


/* SECTION: Dynamic Arrays */
typedef double* DArray1d;
DArray1d create_darr1d(int n);
void destroy_darr1d(DArray1d a);

typedef double** DArray2d;
DArray2d create_darr2d(int nx, int ny);
void destroy_darr2d(DArray2d a);

typedef double*** DArray3d;
DArray3d create_darr3d(int nx, int ny, int nz);
void destroy_darr3d(DArray3d a);


/* MPI stuff */
#ifdef USE_MPI
#include "mpi.h"

#ifdef OMPI_MPI_H  /* openmpi does not use signals: we may install our sighandler */
#ifndef OPENACC    /* ... but only if we are not also running on GPU */
#undef NOSIGNALS
#endif
#endif

/*
 * MPI_MASTER(i):
 * execution of i only on master node
 */
#define MPI_MASTER(statement) { \
  if(mpi_node_rank == mpi_node_root)\
  { statement; } \
}

#ifndef MPI_REDUCE_BLOCKSIZE
#define MPI_REDUCE_BLOCKSIZE 1000
#endif

int mc_MPI_Sum(double* buf, long count);
int mc_MPI_Send(void *sbuf, long count, MPI_Datatype dtype, int dest);
int mc_MPI_Recv(void *rbuf, long count, MPI_Datatype dtype, int source);

/* MPI_Finalize exits gracefully and should be preferred to MPI_Abort */
#define exit(code) do {                                   \
    MPI_Finalize();                                       \
    exit(code);                                           \
  } while(0)

#else /* !USE_MPI */
#define MPI_MASTER(instr) instr
#endif /* USE_MPI */


#ifdef USE_MPI
static int mpi_node_count;
#endif

#ifdef USE_THREADS  /* user want threads */
#error Threading (USE_THREADS) support has been removed for very poor efficiency. Use MPI/SSH grid instead.
#endif


void   mcset_ncount(unsigned long long count);    /* wrapper to get mcncount */
#pragma acc routine
unsigned long long int mcget_ncount(void);            /* wrapper to set mcncount */
unsigned long long mcget_run_num(void);           /* wrapper to get mcrun_num=0:mcncount-1 */

#pragma acc routine
_class_particle mcgetstate(_class_particle mcneutron, double *x, double *y, double *z,
                           double *vx, double *vy, double *vz, double *t,
                           double *sx, double *sy, double *sz, double *p);

/* Following part is only embedded when not redundant with mccode.h ========= */

#ifndef MCCODE_H

#ifndef NOSIGNALS
#include <signal.h>
char  *mcsig_message;
#define SIG_MESSAGE(msg) mcsig_message=(char *)(msg);
#else
#define SIG_MESSAGE(...)
#endif /* !NOSIGNALS */


/* Useful macros and constants ============================================== */


#ifndef FLT_MAX
#define FLT_MAX         3.40282347E+38F /* max decimal value of a "float" */
#endif

#ifndef MIN
#define MIN(a, b)  (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a, b)  (((a) > (b)) ? (a) : (b))
#endif
#ifndef SQR
#define SQR(x) ( (x) * (x) )
#endif
#ifndef SIGN
#define SIGN(x) (((x)>0.0)?(1):(-1))
#endif

#ifndef PI
# ifdef M_PI
#  define PI M_PI
# else
/* When using c99 in the CFLAGS, some of these consts
   are lost... Perhaps we should in fact include everything from
   https://www.gnu.org/software/libc/manual/html_node/Mathematical-Constants.html
*/
#  define PI 3.14159265358979323846
#  define M_PI PI
#  define M_PI_2 M_PI/2.0
#  define M_PI_4 M_PI/4.0
#  define M_1_PI 1.0/M_PI
#  define M_2_PI 2*M_1_PI
# endif
#endif

#define RAD2MIN  ((180*60)/PI)
#define MIN2RAD  (PI/(180*60))
#define DEG2RAD  (PI/180)
#define RAD2DEG  (180/PI)
#define FWHM2RMS 0.424660900144    /* Convert between full-width-half-max and */
#define RMS2FWHM 2.35482004503     /* root-mean-square (standard deviation) */
#define HBAR     1.05457168e-34    /* [Js] h bar Planck constant CODATA 2002 */
#define MNEUTRON 1.67492728e-27    /* [kg] mass of neutron CODATA 2002 */
#define GRAVITY  9.81              /* [m/s^2] gravitational acceleration */
#define NA       6.02214179e23     /* [#atoms/g .mole] Avogadro's number*/


/* wrapper to get absolute and relative position of comp */
/* mccomp_posa and mccomp_posr are defined in McStas generated C code */
#define POS_A_COMP_INDEX(index) (instrument->_position_absolute[index])
#define POS_R_COMP_INDEX(index) (instrument->_position_relative[index])

/* setting parameters based COMP_GETPAR (returned as pointer)         */
/* compname must be given as a string, type and par are symbols.      */
#define COMP_GETPAR3(type, compname, par) \
    &( ((_class_ ## type ##_parameters *) _getvar_parameters(compname))->par )
/* the body of this function depends on component instances, and is cogen'd */
void* _getvar_parameters(char* compname);

/* Note: The two-stage approach to COMP_GETPAR is NOT redundant; without it,
* after #define C sample, COMP_GETPAR(C,x) would refer to component C, not to
* component sample. Such are the joys of ANSI C.

* Anyway the usage of COMP_GETPAR requires that we use sometimes bare names...
* NOTE: This can ONLY be used in instrument descriptions, not components.
*/
#define COMP_GETPAR2(comp, par) (_ ## comp ## _var._parameters.par)
#define COMP_GETPAR(comp, par) COMP_GETPAR2(comp,par)

#define INSTRUMENT_GETPAR(par) (_instrument_var._parameters.par)

/* Current component name, index, position and orientation */
/* These macros work because, using class-based functions, "comp" is usually
*  the local variable of the active/current component. */
#define INDEX_CURRENT_COMP (_comp->_index)
#define NAME_CURRENT_COMP (_comp->_name)
#define TYPE_CURRENT_COMP (_comp->_type)
#define POS_A_CURRENT_COMP (_comp->_position_absolute)
#define POS_R_CURRENT_COMP (_comp->_position_relative)
#define ROT_A_CURRENT_COMP (_comp->_rotation_absolute)
#define ROT_R_CURRENT_COMP (_comp->_rotation_relative)

#define NAME_INSTRUMENT (instrument->_name)


/* MCDISPLAY/trace and debugging message sent to stdout */
#ifdef MC_TRACE_ENABLED
#define DEBUG
#endif

#ifdef DEBUG
#define DEBUG_INSTR() if(!mcdotrace); else { printf("INSTRUMENT:\n"); printf("Instrument '%s' (%s)\n", instrument_name, instrument_source); }
#define DEBUG_COMPONENT(name,c,t) if(!mcdotrace); else {\
  printf("COMPONENT: \"%s\"\n" \
         "POS: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
         name, c.x, c.y, c.z, t[0][0], t[0][1], t[0][2], \
         t[1][0], t[1][1], t[1][2], t[2][0], t[2][1], t[2][2]); \
  printf("Component %30s AT (%g,%g,%g)\n", name, c.x, c.y, c.z); \
  }
#define DEBUG_INSTR_END() if(!mcdotrace); else printf("INSTRUMENT END:\n");
#define DEBUG_ENTER() if(!mcdotrace); else printf("ENTER:\n");
#define DEBUG_COMP(c) if(!mcdotrace); else printf("COMP: \"%s\"\n", c);
#define DEBUG_LEAVE() if(!mcdotrace); else printf("LEAVE:\n");
#define DEBUG_ABSORB() if(!mcdotrace); else printf("ABSORB:\n");
#else
#define DEBUG_INSTR()
#define DEBUG_COMPONENT(name,c,t)
#define DEBUG_INSTR_END()
#define DEBUG_ENTER()
#define DEBUG_COMP(c)
#define DEBUG_LEAVE()
#define DEBUG_ABSORB()
#endif

// mcDEBUG_STATE and mcDEBUG_SCATTER are defined by mcstas-r.h and mcxtrace-r.h



#ifdef TEST
#define test_printf printf
#else
#define test_printf while(0) printf
#endif

/* send MCDISPLAY message to stdout to show gemoetry */
void mcdis_magnify(char *what);
void mcdis_line(double x1, double y1, double z1,
                double x2, double y2, double z2);
void mcdis_dashed_line(double x1, double y1, double z1,
		       double x2, double y2, double z2, int n);
void mcdis_multiline(int count, ...);
void mcdis_rectangle(char* plane, double x, double y, double z,
		     double width, double height);
void mcdis_box(double x, double y, double z,
	       double width, double height, double length);
void mcdis_circle(char *plane, double x, double y, double z, double r);
void mcdis_Circle(double x, double y, double z, double r, double nx, double ny, double nz);
void mcdis_cylinder( double x, double y, double z,
        double r, double height, int N, double nx, double ny, double nz);
void mcdis_sphere(double x, double y, double z, double r, int N);


/* random number generation. ================================================ */

/* available random number generators */
#define _RNG_ALG_MT         1
#define _RNG_ALG_KISS       2
#define _RNG_ALG_FAST_KISS  3
#define _RNG_ALG_CUDA       4

/* selection of random number generator */
#ifndef RNG_ALG
#  define RNG_ALG  _RNG_ALG_KISS
#endif


#if RNG_ALG == _RNG_ALG_MT  // MT (currently not functional for gpu)
#  define MC_RAND_MAX ((unsigned long)0xffffffff)
#  define randstate_t unsigned long // this could be anything
#  define RANDSTATE_LEN 1
#  define srandom(seed) mt_srandom_empty()
#  define random() mt_random()
#  define _random() mt_random()
#elif RNG_ALG == _RNG_ALG_KISS  // KISS
#  ifndef ULONG_MAX
#    define ULONG_MAX ((unsigned long)0xffffffffffffffffUL)
#  endif
#  define MC_RAND_MAX ULONG_MAX
#  define randstate_t unsigned long
#  define RANDSTATE_LEN 7
#  define srandom(seed) kiss_srandom(_particle->randstate, seed)
#  define random() kiss_random(_particle->randstate)
#  define _random() kiss_random(state)
#elif RNG_ALG == _RNG_ALG_FAST_KISS  // FAST KISS Hundt impl. (not yet functional)
#  define randstate_t uint32_t
#  define RANDSTATE_LEN 5
#  define srandom(seed) fast_kiss_srandom(_particle->randstate, seed)
#  define random() fast_kiss_random(_particle->randstate)
#  define _random() kiss_random(state)
#  define _rand01
#elif RNG_ALG == _RNG_ALG_CUDA
#  include <openacc_curand.h>
#  define randstate_t curandState_t /* CUDA RNG state */
#  define srandom(seed) curand_init(_particle->_uid, seed, 0ULL, _particle->randstate);
#  define random() curand_uniform(_particle->randstate)
#  define _random() curand_uniform(state);
#endif

#pragma acc routine
double _randnorm2(randstate_t* state);


// component writers interface
#define randnorm() _randnorm2(_particle->randstate) // NOTE: can not use _randnorm on gpu
#define rand01() _rand01(_particle->randstate)
#define randpm1() _randpm1(_particle->randstate)
#define rand0max(p1) _rand0max(p1, _particle->randstate)
#define randminmax(p1, p2) _randminmax(p1, p2, _particle->randstate)
#define randtriangle() _randtriangle(_particle->randstate)

// Mersenne Twister rng
unsigned long mt_random(void);
void mt_srandom (unsigned long x);
void mt_srandom_empty();

// KISS rng
#pragma acc routine
unsigned long *kiss_srandom(unsigned long state[7], unsigned long seed);
#pragma acc routine
unsigned long kiss_random(unsigned long state[7]);

// FAST KISS rng
#pragma acc routine seq
randstate_t _hash(randstate_t x);
#pragma acc routine seq
randstate_t fast_kiss(randstate_t * state);
#pragma acc routine seq
randstate_t * fast_kiss_seed(randstate_t * state, randstate_t seed);

// internal RNG (transforms) interface
#pragma acc routine
double _rand01(randstate_t* state);
#pragma acc routine
double _randpm1(randstate_t* state);
#pragma acc routine
double _rand0max(double max, randstate_t* state);
#pragma acc routine
double _randminmax(double min, double max, randstate_t* state);
#pragma acc routine
double _randtriangle(randstate_t* state);
// some rngs use this package instead
#pragma acc routine
double _uniform_double(randstate_t * state);
#pragma acc routine
double _gaussian_double(randstate_t * state);


#ifdef USE_OPENCL
#include "opencl-lib.h"
#include "opencl-lib.c"
#endif

#ifndef DANSE
int init(void);
int raytrace(_class_particle*);
int save(FILE *);
int finally(void);
int display(void);
#endif


/* GPU related algorithms =================================================== */

/*
*  Divide-and-conquer strategy for parallel sort absorbed last.
*/
#ifdef FUNNEL
long sort_absorb_last(_class_particle* particles, _class_particle* pbuffer, long len, long buffer_len, long flag_split, long* multiplier);
#endif
long sort_absorb_last_serial(_class_particle* particles, long len);


/* simple vector algebra ==================================================== */


#define vec_prod(x, y, z, x1, y1, z1, x2, y2, z2) \
	vec_prod_func(&x, &y, &z, x1, y1, z1, x2, y2, z2)
#pragma acc routine seq
mcstatic void vec_prod_func(double *x, double *y, double *z,
		double x1, double y1, double z1, double x2, double y2, double z2);

#pragma acc routine seq
mcstatic double scalar_prod(
		double x1, double y1, double z1, double x2, double y2, double z2);

#pragma acc routine seq
mcstatic void norm_func(double *x, double *y, double *z);
#define NORM(x,y,z)	norm_func(&x, &y, &z)

#pragma acc routine seq
void normal_vec(double *nx, double *ny, double *nz,
    double x, double y, double z);

/**
 * Rotate the vector vx,vy,vz psi radians around the vector ax,ay,az
 * and put the result in x,y,z.
 */
#define rotate(x, y, z, vx, vy, vz, phi, ax, ay, az) \
  do { \
    double mcrt_tmpx = (ax), mcrt_tmpy = (ay), mcrt_tmpz = (az); \
    double mcrt_vp, mcrt_vpx, mcrt_vpy, mcrt_vpz; \
    double mcrt_vnx, mcrt_vny, mcrt_vnz, mcrt_vn1x, mcrt_vn1y, mcrt_vn1z; \
    double mcrt_bx, mcrt_by, mcrt_bz; \
    double mcrt_cos, mcrt_sin; \
    NORM(mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); \
    mcrt_vp = scalar_prod((vx), (vy), (vz), mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); \
    mcrt_vpx = mcrt_vp*mcrt_tmpx; \
    mcrt_vpy = mcrt_vp*mcrt_tmpy; \
    mcrt_vpz = mcrt_vp*mcrt_tmpz; \
    mcrt_vnx = (vx) - mcrt_vpx; \
    mcrt_vny = (vy) - mcrt_vpy; \
    mcrt_vnz = (vz) - mcrt_vpz; \
    vec_prod(mcrt_bx, mcrt_by, mcrt_bz, \
             mcrt_tmpx, mcrt_tmpy, mcrt_tmpz, mcrt_vnx, mcrt_vny, mcrt_vnz); \
    mcrt_cos = cos((phi)); mcrt_sin = sin((phi)); \
    mcrt_vn1x = mcrt_vnx*mcrt_cos + mcrt_bx*mcrt_sin; \
    mcrt_vn1y = mcrt_vny*mcrt_cos + mcrt_by*mcrt_sin; \
    mcrt_vn1z = mcrt_vnz*mcrt_cos + mcrt_bz*mcrt_sin; \
    (x) = mcrt_vpx + mcrt_vn1x; \
    (y) = mcrt_vpy + mcrt_vn1y; \
    (z) = mcrt_vpz + mcrt_vn1z; \
  } while(0)

/**
 * Mirror (xyz) in the plane given by the point (rx,ry,rz) and normal (nx,ny,nz)
 *
 * TODO: This define is seemingly never used...
 */
#define mirror(x,y,z,rx,ry,rz,nx,ny,nz) \
  do { \
    double mcrt_tmpx= (nx), mcrt_tmpy = (ny), mcrt_tmpz = (nz); \
    double mcrt_tmpt; \
    NORM(mcrt_tmpx, mcrt_tmpy, mcrt_tmpz); \
    mcrt_tmpt=scalar_prod((rx),(ry),(rz),mcrt_tmpx,mcrt_tmpy,mcrt_tmpz); \
    (x) = rx -2 * mcrt_tmpt*mcrt_rmpx; \
    (y) = ry -2 * mcrt_tmpt*mcrt_rmpy; \
    (z) = rz -2 * mcrt_tmpt*mcrt_rmpz; \
  } while (0)

#pragma acc routine
Coords coords_set(MCNUM x, MCNUM y, MCNUM z);
#pragma acc routine
Coords coords_get(Coords a, MCNUM *x, MCNUM *y, MCNUM *z);
#pragma acc routine
Coords coords_add(Coords a, Coords b);
#pragma acc routine
Coords coords_sub(Coords a, Coords b);
#pragma acc routine
Coords coords_neg(Coords a);
#pragma acc routine
Coords coords_scale(Coords b, double scale);
#pragma acc routine
double coords_sp(Coords a, Coords b);
#pragma acc routine
Coords coords_xp(Coords b, Coords c);
#pragma acc routine
double coords_len(Coords a);
#pragma acc routine seq
void   coords_print(Coords a);
#pragma acc routine seq
mcstatic void coords_norm(Coords* c);

#pragma acc routine seq
void rot_set_rotation(Rotation t, double phx, double phy, double phz);
#pragma acc routine seq
int  rot_test_identity(Rotation t);
#pragma acc routine seq
void rot_mul(Rotation t1, Rotation t2, Rotation t3);
#pragma acc routine seq
void rot_copy(Rotation dest, Rotation src);
#pragma acc routine seq
void rot_transpose(Rotation src, Rotation dst);
#pragma acc routine seq
Coords rot_apply(Rotation t, Coords a);

#pragma acc routine seq
void mccoordschange(Coords a, Rotation t, _class_particle *particle);
#pragma acc routine seq
void mccoordschange_polarisation(Rotation t, double *sx, double *sy, double *sz);

double mcestimate_error(double N, double p1, double p2);
void mcreadparams(void);

/* this is now in mcstas-r.h and mcxtrace-r.h as the number of state parameters
is no longer equal */

_class_particle mcgenstate(void);

// trajectory/shape intersection routines
#pragma acc routine seq
int inside_rectangle(double, double, double, double);
#pragma acc routine seq
int box_intersect(double *dt_in, double *dt_out, double x, double y, double z,
      double vx, double vy, double vz, double dx, double dy, double dz);
#pragma acc routine seq
int cylinder_intersect(double *t0, double *t1, double x, double y, double z,
      double vx, double vy, double vz, double r, double h);
#pragma acc routine seq
int sphere_intersect(double *t0, double *t1, double x, double y, double z,
      double vx, double vy, double vz, double r);
// second order equation roots
#pragma acc routine seq
int solve_2nd_order(double *t1, double *t2,
      double A,  double B,  double C);

// random vector generation to shape
// defines silently introducing _particle as the last argument
#define randvec_target_circle(xo, yo, zo, solid_angle, xi, yi, zi, radius) \
  _randvec_target_circle(xo, yo, zo, solid_angle, xi, yi, zi, radius, _particle)
#define randvec_target_rect_angular(xo, yo, zo, solid_angle, xi, yi, zi, height, width, A) \
  _randvec_target_rect_angular(xo, yo, zo, solid_angle, xi, yi, zi, height, width, A, _particle)
#define randvec_target_rect_real(xo, yo, zo, solid_angle, xi, yi, zi, height, width, A, lx, ly, lz, order) \
  _randvec_target_rect_real(xo, yo, zo, solid_angle, xi, yi, zi, height, width, A, lx, ly, lz, order, _particle)
// defines forwarding to "inner" functions
#define randvec_target_sphere randvec_target_circle
#define randvec_target_rect(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9) \
  randvec_target_rect_real(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,0,0,0,1)
// headers for randvec
#pragma acc routine seq
void _randvec_target_circle(double *xo, double *yo, double *zo,
  double *solid_angle, double xi, double yi, double zi, double radius,
  _class_particle* _particle);
#pragma acc routine seq
void _randvec_target_rect_angular(double *xo, double *yo, double *zo,
  double *solid_angle, double xi, double yi, double zi, double height,
  double width, Rotation A,
  _class_particle* _particle);
#pragma acc routine seq
void _randvec_target_rect_real(double *xo, double *yo, double *zo, double *solid_angle,
  double xi, double yi, double zi, double height, double width, Rotation A,
  double lx, double ly, double lz, int order,
  _class_particle* _particle);


// this is the main()
int mccode_main(int argc, char *argv[]);


#endif /* !MCCODE_H */

#ifndef MCCODE_R_IO_H
#define MCCODE_R_IO_H "$Revision$"

#if (USE_NEXUS == 0)
#undef USE_NEXUS
#endif

#ifndef CHAR_BUF_LENGTH
#define CHAR_BUF_LENGTH 1024
#endif


/* I/O section part ========================================================= */

/* ========================================================================== */

/*                               MCCODE_R_IO_C                                */

/* ========================================================================== */


/* main DETECTOR structure which stores most information to write to data files */
struct mcdetector_struct {
  char   filename[CHAR_BUF_LENGTH];   /* file name of monitor */
  char   position[CHAR_BUF_LENGTH];   /* position of detector component */
  char   component[CHAR_BUF_LENGTH];  /* component instance name */
  char   instrument[CHAR_BUF_LENGTH]; /* instrument name */
  char   type[CHAR_BUF_LENGTH];       /* data type, e.g. 0d, 1d, 2d, 3d */
  char   user[CHAR_BUF_LENGTH];       /* user name, e.g. HOME */
  char   date[CHAR_BUF_LENGTH];       /* date of simulation end/write time */
  char   title[CHAR_BUF_LENGTH];      /* title of detector */
  char   xlabel[CHAR_BUF_LENGTH];     /* X axis label */
  char   ylabel[CHAR_BUF_LENGTH];     /* Y axis label */
  char   zlabel[CHAR_BUF_LENGTH];     /* Z axis label */
  char   xvar[CHAR_BUF_LENGTH];       /* X variable name */
  char   yvar[CHAR_BUF_LENGTH];       /* Y variable name */
  char   zvar[CHAR_BUF_LENGTH];       /* Z variable name */
  char   ncount[CHAR_BUF_LENGTH];     /* number of events initially generated */
  char   limits[CHAR_BUF_LENGTH];     /* X Y Z limits, e.g. [xmin xmax ymin ymax zmin zmax] */
  char   variables[CHAR_BUF_LENGTH];  /* variables written into data block */
  char   statistics[CHAR_BUF_LENGTH]; /* center, mean and half width along axis */
  char   signal[CHAR_BUF_LENGTH];     /* min max and mean of signal (data block) */
  char   values[CHAR_BUF_LENGTH];     /* integrated values e.g. [I I_err N] */
  double xmin,xmax;                   /* min max of axes */
  double ymin,ymax;
  double zmin,zmax;
  double intensity;                   /* integrated values for data block */
  double error;
  double events;
  double min;                         /* statistics for data block */
  double max;
  double mean;
  double centerX;                     /* statistics for axes */
  double halfwidthX;
  double centerY;
  double halfwidthY;
  int    rank;                        /* dimensionaly of monitor, e.g. 0 1 2 3 */
  char   istransposed;                /* flag to transpose matrix for some formats */

  long   m,n,p;                       /* dimensions of data block and along axes */
  long   date_l;                      /* same as date, but in sec since 1970 */

  double *p0, *p1, *p2;               /* pointers to saved data, NULL when freed */
  char   format[CHAR_BUF_LENGTH];    /* format for file generation */
};

typedef struct mcdetector_struct MCDETECTOR;

static   char *dirname             = NULL;      /* name of output directory */
static   char *siminfo_name        = "mccode";  /* default output sim file name */
char    *mcformat                    = NULL;      /* NULL (default) or a specific format */

/* file I/O definitions and function prototypes */

#ifndef MC_EMBEDDED_RUNTIME /* the mcstatic variables (from mccode-r.c) */
extern FILE * siminfo_file;     /* handle to the output siminfo file */
extern int    mcgravitation;      /* flag to enable gravitation */
extern int    mcdotrace;          /* flag to print MCDISPLAY messages */
#else
mcstatic FILE *siminfo_file        = NULL;
#endif

/* I/O function prototypes ================================================== */

// from msysgit: https://code.google.com/p/msysgit/source/browse/compat/strcasestr.c
char *strcasestr(const char *haystack, const char *needle);

/* output functions */
MCDETECTOR mcdetector_out_0D(char *t, double p0, double p1, double p2, char *c, Coords pos);
MCDETECTOR mcdetector_out_1D(char *t, char *xl, char *yl,
                  char *xvar, double x1, double x2, long n,
                  double *p0, double *p1, double *p2, char *f, char *c, Coords pos);
MCDETECTOR mcdetector_out_2D(char *t, char *xl, char *yl,
                  double x1, double x2, double y1, double y2, long m,
                  long n, double *p0, double *p1, double *p2, char *f,
                  char *c, Coords pos);
MCDETECTOR mcdetector_out_list(char *t, char *xl, char *yl,
                  long m, long n,
                  double *p1, char *f,
                  char *c, Coords posa);

/* wrappers to output functions, that automatically set NAME and POSITION */
#define DETECTOR_OUT(p0,p1,p2) mcdetector_out_0D(NAME_CURRENT_COMP,p0,p1,p2,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_0D(t,p0,p1,p2) mcdetector_out_0D(t,p0,p1,p2,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f) \
     mcdetector_out_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f) \
     mcdetector_out_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)

#ifdef USE_NEXUS
#include "napi.h"
NXhandle nxhandle;
#endif

#endif /* ndef MCCODE_R_IO_H */

#endif /* MCCODE_R_H */
/* End of file "mccode-r.h". */

/* embedding file "mcstas-r.h" */

/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2009, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/mcstas-r.h
*
* %Identification
* Written by: KN
* Date:    Aug 29, 1997
* Release: McStas X.Y
* Version: $Revision$
*
* Runtime system header for McStas.
*
* In order to use this library as an external library, the following variables
* and macros must be declared (see details in the code)
*
*   struct mcinputtable_struct mcinputtable[];
*   int mcnumipar;
*   char instrument_name[], instrument_source[];
*   int traceenabled, defaultmain;
*   extern MCNUM  mccomp_storein[];
*   extern MCNUM  instrument.counter_AbsorbProp[];
*   extern MCNUM  mcScattered;
*   #define MCCODE_STRING "the McStas version"
*
* Usage: Automatically embbeded in the c code.
*
* $Id$
*
*******************************************************************************/

#ifndef MCSTAS_R_H
#define MCSTAS_R_H "$Revision$"

/* Following part is only embedded when not redundent with mcstas.h ========= */

#ifndef MCCODE_H

#define AA2MS    629.622368        /* Convert k[1/AA] to v[m/s] */
#define MS2AA    1.58825361e-3     /* Convert v[m/s] to k[1/AA] */
#define K2V      AA2MS
#define V2K      MS2AA
#define Q2V      AA2MS
#define V2Q      MS2AA
#define SE2V     437.393377        /* Convert sqrt(E)[meV] to v[m/s] */
#define VS2E     5.22703725e-6     /* Convert (v[m/s])**2 to E[meV] */

#define SCATTER0 do {DEBUG_SCATTER(); SCATTERED++;} while(0)
#define SCATTER SCATTER0

#define JUMPTOCOMP(comp) mcneutron->_index = INDEX_COMP(comp);

#define MAGNET_ON \
  do { \
    mcMagnet = 1; \
  } while(0)

#define MAGNET_OFF \
  do { \
    mcMagnet = 0; \
  } while(0)

#define ALLOW_BACKPROP \
  do { \
    mcallowbackprop = 1; \
  } while(0)

#define DISALLOW_BACKPROP \
  do { \
    mcallowbackprop = 0; \
  } while(0)

#define PROP_MAGNET(dt) \
  do { \
  } while (0)
    /* change coordinates from local system to magnet system */
/*    Rotation rotLM, rotTemp; \
      Coords   posLM = coords_sub(POS_A_CURRENT_COMP, mcMagnetPos); \
      rot_transpose(ROT_A_CURRENT_COMP, rotTemp); \
      rot_mul(rotTemp, mcMagnetRot, rotLM); \
      mcMagnetPrecession(x, y, z, t, vx, vy, vz, \
               &sx, &sy, &sz, dt, posLM, rotLM); \
      } while(0)
*/

#define mcPROP_DT(dt) \
  do { \
    if (mcMagnet && dt > 0) PROP_MAGNET(dt);\
    x += vx*(dt); \
    y += vy*(dt); \
    z += vz*(dt); \
    t += (dt); \
    if (isnan(p) || isinf(p)) { ABSORB; }\
  } while(0)

/* ADD: E. Farhi, Aug 6th, 2001 PROP_GRAV_DT propagation with acceleration */
#define PROP_GRAV_DT(dt, Ax, Ay, Az) \
  do { \
    if(dt < 0 && mcallowbackprop == 0) { ABSORB; }\
    if (mcMagnet) /*printf("Spin precession gravity\n")*/; \
    x  += vx*(dt) + (Ax)*(dt)*(dt)/2; \
    y  += vy*(dt) + (Ay)*(dt)*(dt)/2; \
    z  += vz*(dt) + (Az)*(dt)*(dt)/2; \
    vx += (Ax)*(dt); \
    vy += (Ay)*(dt); \
    vz += (Az)*(dt); \
    t  += (dt); \
    DISALLOW_BACKPROP;\
  } while(0)


#define PROP_DT(dt) \
  do { \
    if(dt < 0) { RESTORE=1; ABSORB; }; \
    if (mcgravitation) { Coords mcLocG; double mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    PROP_GRAV_DT(dt, mc_gx, mc_gy, mc_gz); } \
    else mcPROP_DT(dt); \
    DISALLOW_BACKPROP;\
  } while(0)


#define PROP_Z0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = solve_2nd_order(&mc_dt, NULL, -mc_gz/2, -vz, -z); \
    if (mc_ret) {PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); z=0;}\
    else if (mcallowbackprop == 0 && mc_dt < 0) { ABSORB; }; } \
    else mcPROP_Z0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define mcPROP_Z0 \
  do { \
    double mc_dt; \
    if(vz == 0) { ABSORB; }; \
    mc_dt = -z/vz; \
    if(mc_dt < 0 && mcallowbackprop == 0) { ABSORB; }; \
    mcPROP_DT(mc_dt); \
    z = 0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define PROP_X0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = solve_2nd_order(&mc_dt, NULL, -mc_gx/2, -vx, -x); \
    if (mc_ret) {PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); x=0;}\
    else if (mcallowbackprop == 0 && mc_dt < 0) { ABSORB; }; } \
    else mcPROP_X0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define mcPROP_X0 \
  do { \
    double mc_dt; \
    if(vx == 0) { ABSORB; }; \
    mc_dt = -x/vx; \
    if(mc_dt < 0 && mcallowbackprop == 0) { ABSORB; }; \
    mcPROP_DT(mc_dt); \
    x = 0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define PROP_Y0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = solve_2nd_order(&mc_dt, NULL, -mc_gy/2, -vy, -y); \
    if (mc_ret) {PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); y=0;}\
    else if (mcallowbackprop == 0 && mc_dt < 0) { ABSORB; }; } \
    else mcPROP_Y0; \
    DISALLOW_BACKPROP;\
  } while(0)


#define mcPROP_Y0 \
  do { \
    double mc_dt; \
    if(vy == 0) { ABSORB; }; \
    mc_dt = -y/vy; \
    if(mc_dt < 0 && mcallowbackprop == 0) { ABSORB; }; \
    mcPROP_DT(mc_dt); \
    y = 0; \
    DISALLOW_BACKPROP; \
  } while(0)


#ifdef DEBUG

#define DEBUG_STATE() if(!mcdotrace); else \
  printf("STATE: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
         x,y,z,vx,vy,vz,t,sx,sy,sz,p);
#define DEBUG_SCATTER() if(!mcdotrace); else \
  printf("SCATTER: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
         x,y,z,vx,vy,vz,t,sx,sy,sz,p);

#else

#define DEBUG_STATE()
#define DEBUG_SCATTER()

#endif

#endif /* !MCCODE_H */

#endif /* MCSTAS_R_H */
/* End of file "mcstas-r.h". */

/* embedding file "mccode-r.c" */

/*******************************************************************************
*
* McCode, neutron/xray ray-tracing package
*         Copyright (C) 1997-2009, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/mccode-r.c
*
* %Identification
* Written by: KN
* Date:    Aug 29, 1997
* Release: McStas X.Y/McXtrace X.Y
* Version: $Revision$
*
* Runtime system for McStas and McXtrace.
* Embedded within instrument in runtime mode.
* Contains SECTIONS:
*   MPI handling (sum, send, recv)
*   format definitions
*   I/O
*   mcdisplay support
*   random numbers
*   coordinates handling
*   vectors math (solve 2nd order, normals, randvec...)
*   parameter handling
*   signal and main handlers
*
* Usage: Automatically embbeded in the c code whenever required.
*
* $Id$
*
*******************************************************************************/

/*******************************************************************************
* The I/O format definitions and functions
*******************************************************************************/


/** Include header files to avoid implicit declarations (not allowed on LLVM) */
#include <ctype.h>
#include <sys/types.h>

// UNIX specific headers (non-Windows)
#if defined(__unix__) || defined(__APPLE__)
#include <unistd.h>
#include <sys/stat.h>
#endif


#ifndef DANSE
#ifdef MC_ANCIENT_COMPATIBILITY
int traceenabled = 0;
int defaultmain  = 0;
#endif
/* else defined directly in the McCode generated C code */

static   long mcseed                 = 0; /* seed for random generator */
#pragma acc declare create ( mcseed )
static   long mcstartdate            = 0; /* start simulation time */
static   int  mcdisable_output_files = 0; /* --no-output-files */
mcstatic int  mcgravitation          = 0; /* use gravitation flag, for PROP macros */
mcstatic int  mcdotrace              = 0; /* flag for --trace and messages for DISPLAY */
int      mcallowbackprop             = 0;         /* flag to enable negative/backprop */

/* OpenACC-related segmentation parameters: */
int vecsize = 128;
int numgangs = 7813;
long gpu_innerloop = 2147483647;

/* Number of particle histories to simulate. */
#ifdef NEUTRONICS
mcstatic unsigned long long int mcncount             = 1;
mcstatic unsigned long long int mcrun_num            = 0;
#else
#ifdef MCDEFAULT_NCOUNT
mcstatic unsigned long long int mcncount             = MCDEFAULT_NCOUNT;
#else
mcstatic unsigned long long int mcncount             = 1000000;
#endif
#pragma acc declare create ( mcncount )
mcstatic unsigned long long int mcrun_num            = 0;
#pragma acc declare create ( mcrun_num )
#endif /* NEUTRONICS */

#else
#include "mcstas-globals.h"
#endif /* !DANSE */

/* String nullification on GPU and other replacements */
#ifdef OPENACC
int noprintf() {
  return 0;
}

int str_comp(char *str1, char *str2) {
  while (*str1 && *str1 == *str2) {
    str1++;
    str2++;
  }
  return (*str1 - *str2);
}

size_t str_len(const char *s)
{
  size_t len = 0;
  if(s != NULL)
  {
    while(*s != '\0')
    {
      ++len;
      ++s;
    }
  }
  return len;
}

#endif


/* SECTION: Dynamic Arrays ================================================== */

DArray1d create_darr1d(int n){
  DArray1d arr2d;
  arr2d = calloc(n, sizeof(double));
  return arr2d;
}

void destroy_darr1d(DArray1d a){
  free(a);
}

DArray2d create_darr2d(int nx, int ny){
  DArray2d arr2d;
  arr2d = calloc(nx, sizeof(double *));

  double *p1;
  p1 = calloc(nx*ny, sizeof(double));

  int i;
  for (i=0; i<nx; i++){
    arr2d[i] = &(p1[i*ny]);
  }
  return arr2d;
}

void destroy_darr2d(DArray2d a){
  free(a[0]);
  free(a);
}

DArray3d create_darr3d(int nx, int ny, int nz){
  DArray3d arr3d;

  int i, j;

  // 1d
  arr3d = calloc(nx, sizeof(double **));

  // d2
  double **p1;
  p1 = calloc(nx*ny, sizeof(double *));

  for (i=0; i<nx; i++){
    arr3d[i] = &(p1[i*ny]);
  }

  // 3d
  double *p2;
  p2 = calloc(nx*ny*nz, sizeof(double));
  for (i=0; i<nx; i++){
    for (j=0; j<ny; j++){
      arr3d[i][j] = &(p2[(i*ny+j)*nz]);
    }
  }
  return arr3d;
}

void destroy_darr3d(DArray3d a){
  free(a[0][0]);
  free(a[0]);
  free(a);
}


/* SECTION: MPI handling ==================================================== */

#ifdef USE_MPI
/* MPI rank */
static int mpi_node_rank;
static int mpi_node_root = 0;


/*******************************************************************************
* mc_MPI_Reduce: Gathers arrays from MPI nodes using Reduce function.
*******************************************************************************/
int mc_MPI_Sum(double *sbuf, long count)
{
  if (!sbuf || count <= 0) return(MPI_SUCCESS); /* nothing to reduce */
  else {
    /* we must cut the buffer into blocks not exceeding the MPI max buffer size of 32000 */
    long   offset=0;
    double *rbuf=NULL;
    int    length=MPI_REDUCE_BLOCKSIZE; /* defined in mccode-r.h */
    int    i=0;
    rbuf = calloc(count, sizeof(double));
    if (!rbuf)
      exit(-fprintf(stderr, "Error: Out of memory %li (mc_MPI_Sum)\n", count*sizeof(double)));
    while (offset < count) {
      if (!length || offset+length > count-1) length=count-offset;
      else length=MPI_REDUCE_BLOCKSIZE;
      if (MPI_Allreduce((double*)(sbuf+offset), (double*)(rbuf+offset),
              length, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD) != MPI_SUCCESS)
        return MPI_ERR_COUNT;
      offset += length;
    }

    for (i=0; i<count; i++) sbuf[i] = rbuf[i];
    free(rbuf);
  }
  return MPI_SUCCESS;
} /* mc_MPI_Sum */

/*******************************************************************************
* mc_MPI_Send: Send array to MPI node by blocks to avoid buffer limit
*******************************************************************************/
int mc_MPI_Send(void *sbuf,
                  long count, MPI_Datatype dtype,
                  int dest)
{
  int dsize;
  long offset=0;
  int  tag=1;
  int  length=MPI_REDUCE_BLOCKSIZE; /* defined in mccode-r.h */

  if (!sbuf || count <= 0) return(MPI_SUCCESS); /* nothing to send */
  MPI_Type_size(dtype, &dsize);

  while (offset < count) {
    if (offset+length > count-1) length=count-offset;
    else length=MPI_REDUCE_BLOCKSIZE;
    if (MPI_Send((void*)(sbuf+offset*dsize), length, dtype, dest, tag++, MPI_COMM_WORLD) != MPI_SUCCESS)
      return MPI_ERR_COUNT;
    offset += length;
  }

  return MPI_SUCCESS;
} /* mc_MPI_Send */

/*******************************************************************************
* mc_MPI_Recv: Receives arrays from MPI nodes by blocks to avoid buffer limit
*             the buffer must have been allocated previously.
*******************************************************************************/
int mc_MPI_Recv(void *sbuf,
                  long count, MPI_Datatype dtype,
                  int source)
{
  int dsize;
  long offset=0;
  int  tag=1;
  int  length=MPI_REDUCE_BLOCKSIZE; /* defined in mccode-r.h */

  if (!sbuf || count <= 0) return(MPI_SUCCESS); /* nothing to recv */
  MPI_Type_size(dtype, &dsize);

  while (offset < count) {
    if (offset+length > count-1) length=count-offset;
    else length=MPI_REDUCE_BLOCKSIZE;
    if (MPI_Recv((void*)(sbuf+offset*dsize), length, dtype, source, tag++,
            MPI_COMM_WORLD, MPI_STATUS_IGNORE) != MPI_SUCCESS)
      return MPI_ERR_COUNT;
    offset += length;
  }

  return MPI_SUCCESS;
} /* mc_MPI_Recv */

#endif /* USE_MPI */

/* SECTION: parameters handling ============================================= */

/* Instrument input parameter type handling. */
/*******************************************************************************
* mcparm_double: extract double value from 's' into 'vptr'
*******************************************************************************/
static int
mcparm_double(char *s, void *vptr)
{
  char *p;
  double *v = (double *)vptr;

  if (!s) { *v = 0; return(1); }
  *v = strtod(s, &p);
  if(*s == '\0' || (p != NULL && *p != '\0') || errno == ERANGE)
    return 0;                        /* Failed */
  else
    return 1;                        /* Success */
}

/*******************************************************************************
* mcparminfo_double: display parameter type double
*******************************************************************************/
static char *
mcparminfo_double(char *parmname)
{
  return "double";
}

/*******************************************************************************
* mcparmerror_double: display error message when failed extract double
*******************************************************************************/
static void
mcparmerror_double(char *parm, char *val)
{
  fprintf(stderr, "Error: Invalid value '%s' for floating point parameter %s (mcparmerror_double)\n",
          val, parm);
}

/*******************************************************************************
* mcparmprinter_double: convert double to string
*******************************************************************************/
static void
mcparmprinter_double(char *f, void *vptr)
{
  double *v = (double *)vptr;
  sprintf(f, "%g", *v);
}

/*******************************************************************************
* mcparm_int: extract int value from 's' into 'vptr'
*******************************************************************************/
static int
mcparm_int(char *s, void *vptr)
{
  char *p;
  int *v = (int *)vptr;
  long x;

  if (!s) { *v = 0; return(1); }
  *v = 0;
  x = strtol(s, &p, 10);
  if(x < INT_MIN || x > INT_MAX)
    return 0;                        /* Under/overflow */
  *v = x;
  if(*s == '\0' || (p != NULL && *p != '\0') || errno == ERANGE)
    return 0;                        /* Failed */
  else
    return 1;                        /* Success */
}

/*******************************************************************************
* mcparminfo_int: display parameter type int
*******************************************************************************/
static char *
mcparminfo_int(char *parmname)
{
  return "int";
}

/*******************************************************************************
* mcparmerror_int: display error message when failed extract int
*******************************************************************************/
static void
mcparmerror_int(char *parm, char *val)
{
  fprintf(stderr, "Error: Invalid value '%s' for integer parameter %s (mcparmerror_int)\n",
          val, parm);
}

/*******************************************************************************
* mcparmprinter_int: convert int to string
*******************************************************************************/
static void
mcparmprinter_int(char *f, void *vptr)
{
  int *v = (int *)vptr;
  sprintf(f, "%d", *v);
}

/*******************************************************************************
* mcparm_string: extract char* value from 's' into 'vptr' (copy)
*******************************************************************************/
static int
mcparm_string(char *s, void *vptr)
{
  char **v = (char **)vptr;
  if (!s) { *v = NULL; return(1); }
  *v = (char *)malloc(strlen(s) + 1);
  if(*v == NULL)
  {
    exit(-fprintf(stderr, "Error: Out of memory %li (mcparm_string).\n", (long)strlen(s) + 1));
  }
  strcpy(*v, s);
  return 1;                        /* Success */
}

/*******************************************************************************
* mcparminfo_string: display parameter type string
*******************************************************************************/
static char *
mcparminfo_string(char *parmname)
{
  return "string";
}

/*******************************************************************************
* mcparmerror_string: display error message when failed extract string
*******************************************************************************/
static void
mcparmerror_string(char *parm, char *val)
{
  fprintf(stderr, "Error: Invalid value '%s' for string parameter %s (mcparmerror_string)\n",
          val, parm);
}

/*******************************************************************************
* mcparmprinter_string: convert string to string (including esc chars)
*******************************************************************************/
static void
mcparmprinter_string(char *f, void *vptr)
{
  char **v = (char **)vptr;
  char *p;

  if (!*v) { *f='\0'; return; }
  strcpy(f, "");
  for(p = *v; *p != '\0'; p++)
  {
    switch(*p)
    {
      case '\n':
        strcat(f, "\\n");
        break;
      case '\r':
        strcat(f, "\\r");
        break;
      case '"':
        strcat(f, "\\\"");
        break;
      case '\\':
        strcat(f, "\\\\");
        break;
      default:
        strncat(f, p, 1);
    }
  }
  /* strcat(f, "\""); */
} /* mcparmprinter_string */

/* now we may define the parameter structure, using previous functions */
static struct
  {
    int (*getparm)(char *, void *);
    char * (*parminfo)(char *);
    void (*error)(char *, char *);
    void (*printer)(char *, void *);
} mcinputtypes[] = {
  {
    mcparm_int, mcparminfo_int, mcparmerror_int,
    mcparmprinter_int
  }, {
    mcparm_string, mcparminfo_string, mcparmerror_string,
    mcparmprinter_string
  }, {
    mcparm_string, mcparminfo_string, mcparmerror_string,
    mcparmprinter_string
  }, {
    mcparm_double, mcparminfo_double, mcparmerror_double,
    mcparmprinter_double
  }, {
    mcparm_double, mcparminfo_double, mcparmerror_double,
    mcparmprinter_double
  }
};

/*******************************************************************************
* mcestimate_error: compute sigma from N,p,p2 in Gaussian large numbers approx
*******************************************************************************/
double mcestimate_error(double N, double p1, double p2)
{
  double pmean, n1;
  if(N <= 1)
    return p1;
  pmean = p1 / N;
  n1 = N - 1;
  /* Note: underflow may cause p2 to become zero; the fabs() below guards
     against this. */
  return sqrt((N/n1)*fabs(p2 - pmean*pmean));
}

double (*mcestimate_error_p)
  (double V2, double psum, double p2sum)=mcestimate_error;

/* ========================================================================== */

/*                               MCCODE_R_IO_C                                */

/* ========================================================================== */

#ifndef MCCODE_R_IO_C
#define MCCODE_R_IO_C "$Revision$"

/* SECTION: file i/o handling ================================================ */

#ifndef HAVE_STRCASESTR
// from msysgit: https://code.google.com/p/msysgit/source/browse/compat/strcasestr.c
char *strcasestr(const char *haystack, const char *needle)
{
  int nlen = strlen(needle);
  int hlen = strlen(haystack) - nlen + 1;
  int i;

  for (i = 0; i < hlen; i++) {
    int j;
    for (j = 0; j < nlen; j++) {
            unsigned char c1 = haystack[i+j];
            unsigned char c2 = needle[j];
            if (toupper(c1) != toupper(c2))
                    goto next;
    }
    return (char *) haystack + i;
  next:
    ;
  }
  return NULL;
}


#endif
#ifndef HAVE_STRCASECMP
int strcasecmp( const char *s1, const char *s2 )
{
  int c1, c2;
  do {
    c1 = tolower( (unsigned char) *s1++ );
    c2 = tolower( (unsigned char) *s2++ );
  } while (c1 == c2 && c1 != 0);
  return c2 > c1 ? -1 : c1 > c2;
}
#endif

#ifndef STRACPY
/* this is a replacement to strncpy, but ensures that the copy ends with NULL */
/* http://stracpy.blogspot.fr/2011/04/stracpy-strncpy-replacement.html */
#define STRACPY
char *stracpy(char *destination, const char *source, size_t amount)
{
        if (!destination || !source || !amount) return(NULL);
        while(amount--)
          if((*destination++ = *source++) == '\0') break;
        *destination = '\0';
        return destination;
}
#endif

/*******************************************************************************
* mcfull_file: allocates a full file name=dirname+file. Catenate extension if missing.
*******************************************************************************/
char *mcfull_file(char *name, char *ext)
{
  int   dirlen=0;
  char *mem   =NULL;

  dirlen = dirname ? strlen(dirname) : 0;
  mem = (char*)malloc(dirlen + strlen(name) + CHAR_BUF_LENGTH);
  if(!mem) {
    exit(-fprintf(stderr, "Error: Out of memory %li (mcfull_file)\n", (long)(dirlen + strlen(name) + 256)));
  }
  strcpy(mem, "");

  /* prepend directory name to path if name does not contain a path */
  if (dirlen > 0 && !strchr(name, MC_PATHSEP_C)) {
    strcat(mem, dirname);
    strcat(mem, MC_PATHSEP_S);
  } /* dirlen */

  strcat(mem, name);
  if (!strchr(name, '.') && ext && strlen(ext))
  { /* add extension if not in file name already */
    strcat(mem, ".");
    strcat(mem, ext);
  }
  return(mem);
} /* mcfull_file */

/*******************************************************************************
* mcnew_file: opens a new file within dirname if non NULL
*             the file is opened in "a" (append, create if does not exist)
*             the extension 'ext' is added if the file name does not include one.
*             the last argument is set to 0 if file did not exist, else to 1.
*******************************************************************************/
FILE *mcnew_file(char *name, char *ext, int *exists)
{
  char *mem;
  FILE *file=NULL;

  if (!name || strlen(name) == 0 || mcdisable_output_files) return(NULL);

  mem  = mcfull_file(name, ext); /* create dirname/name.ext */

  /* check for existence */
  file = fopen(mem, "r"); /* for reading -> fails if does not exist */
  if (file) {
    fclose(file);
    *exists=1;
  } else
    *exists=0;

  /* open the file for writing/appending */
#ifdef USE_NEXUS
  if (mcformat && strcasestr(mcformat, "NeXus")) {
    /* NXhandle nxhandle is defined in the .h with USE_NEXUS */
    NXaccess mode = (*exists ? NXACC_CREATE5 | NXACC_RDWR : NXACC_CREATE5);

    if (NXopen(mem, mode, &nxhandle) != NX_OK)
      file = NULL;
    else
      file = (FILE*)&nxhandle; /* to make it non NULL */
  } else
#endif
    file = fopen(mem, "a+");

  if(!file)
    fprintf(stderr, "Warning: could not open output file '%s' for %s (mcnew_file)\n",
      mem, *exists ? "append" : "create");
  free(mem);

  return file;
} /* mcnew_file */

/*******************************************************************************
* mcdetector_statistics: compute detector statistics, error bars, [x I I_err N] 1D
* RETURN:            updated detector structure
* Used by: detector_import
*******************************************************************************/
MCDETECTOR mcdetector_statistics(
  MCDETECTOR detector)
{

  if (!detector.p1 || !detector.m || !detector.filename)
    return(detector);

  /* compute statistics and update MCDETECTOR structure ===================== */
  double sum_z  = 0, min_z  = 0, max_z  = 0;
  double fmon_x =0,  smon_x = 0, fmon_y =0, smon_y=0, mean_z=0;
  double Nsum=0, P2sum=0;

  double sum_xz = 0, sum_yz = 0, sum_x = 0, sum_y = 0, sum_x2z = 0, sum_y2z = 0;
  int    i,j;
  char   hasnan=0, hasinf=0;
  char   israw = ((char*)strcasestr(detector.format,"raw") != NULL);
  double *this_p1=NULL; /* new 1D McCode array [x I E N]. Freed after writing data */

  /* if McCode/PGPLOT and rank==1 we create a new m*4 data block=[x I E N] */
  if (detector.rank == 1 && strcasestr(detector.format,"McCode")) {
    this_p1 = (double *)calloc(detector.m*detector.n*detector.p*4, sizeof(double));
    if (!this_p1)
      exit(-fprintf(stderr, "Error: Out of memory creating %li 1D " MCCODE_STRING " data set for file '%s' (detector_import)\n",
        detector.m*detector.n*detector.p*4*sizeof(double*), detector.filename));
  }

  max_z = min_z = detector.p1[0];

  /* compute sum and moments (not for lists) */
  if (!strcasestr(detector.format,"list") && detector.m)
  for(j = 0; j < detector.n*detector.p; j++)
  {
    for(i = 0; i < detector.m; i++)
    {
      double x,y,z;
      double N, E;
      long   index= !detector.istransposed ? i*detector.n*detector.p + j : i+j*detector.m;
      char   hasnaninf=0;

      if (detector.m)
        x = detector.xmin + (i + 0.5)/detector.m*(detector.xmax - detector.xmin);
      else x = 0;
      if (detector.n && detector.p)
        y = detector.ymin + (j + 0.5)/detector.n/detector.p*(detector.ymax - detector.ymin);
      else y = 0;
      z = detector.p1[index];
      N = detector.p0 ? detector.p0[index] : 1;
      E = detector.p2 ? detector.p2[index] : 0;
      if (detector.p2 && !israw)
        detector.p2[index] = (*mcestimate_error_p)(detector.p0[index],detector.p1[index],detector.p2[index]); /* set sigma */

      if (detector.rank == 1 && this_p1 && strcasestr(detector.format,"McCode")) {
        /* fill-in 1D McCode array [x I E N] */
        this_p1[index*4]   = x;
        this_p1[index*4+1] = z;
        this_p1[index*4+2] = detector.p2 ? detector.p2[index] : 0;
        this_p1[index*4+3] = N;
      }

      if (isnan(z) || isnan(E) || isnan(N)) hasnaninf=hasnan=1;
      if (isinf(z) || isinf(E) || isinf(N)) hasnaninf=hasinf=1;

      /* compute stats integrals */
      if (!hasnaninf) {
        sum_xz += x*z;
        sum_yz += y*z;
        sum_x  += x;
        sum_y  += y;
        sum_z  += z;
        sum_x2z += x*x*z;
        sum_y2z += y*y*z;
        if (z > max_z) max_z = z;
        if (z < min_z) min_z = z;

        Nsum += N;
        P2sum += E;
      }

    }
  } /* for j */

  /* compute 1st and 2nd moments. For lists, sum_z=0 so this is skipped. */
  if (sum_z && detector.n*detector.m*detector.p)
  {
    fmon_x = sum_xz/sum_z;
    fmon_y = sum_yz/sum_z;
    smon_x = sum_x2z/sum_z-fmon_x*fmon_x; smon_x = smon_x > 0 ? sqrt(smon_x) : 0;
    smon_y = sum_y2z/sum_z-fmon_y*fmon_y; smon_y = smon_y > 0 ? sqrt(smon_y) : 0;
    mean_z = sum_z/detector.n/detector.m/detector.p;
  }
  /* store statistics into detector */
  detector.intensity = sum_z;
  detector.error     = Nsum ? (*mcestimate_error_p)(Nsum, sum_z, P2sum) : 0;
  detector.events    = Nsum;
  detector.min       = min_z;
  detector.max       = max_z;
  detector.mean      = mean_z;
  detector.centerX   = fmon_x;
  detector.halfwidthX= smon_x;
  detector.centerY   = fmon_y;
  detector.halfwidthY= smon_y;

  /* if McCode/PGPLOT and rank==1 replace p1 with new m*4 1D McCode and clear others */
  if (detector.rank == 1 && this_p1 && strcasestr(detector.format,"McCode")) {

    detector.p1 = this_p1;
    detector.n  = detector.m; detector.m  = 4;
    detector.p0 = detector.p2 = NULL;
    detector.istransposed = 1;
  }

  if (detector.n*detector.m*detector.p > 1)
    snprintf(detector.signal, CHAR_BUF_LENGTH,
      "Min=%g; Max=%g; Mean=%g;", detector.min, detector.max, detector.mean);
  else
    strcpy(detector.signal, "None");
  snprintf(detector.values, CHAR_BUF_LENGTH,
    "%g %g %g", detector.intensity, detector.error, detector.events);

  switch (detector.rank) {
    case 1:  snprintf(detector.statistics, CHAR_BUF_LENGTH, "X0=%g; dX=%g;",
      detector.centerX, detector.halfwidthX); break;
    case 2:
    case 3:  snprintf(detector.statistics, CHAR_BUF_LENGTH, "X0=%g; dX=%g; Y0=%g; dY=%g;",
      detector.centerX, detector.halfwidthX, detector.centerY, detector.halfwidthY);
      break;
    default: strcpy(detector.statistics, "None");
  }

  if (hasnan)
    printf("WARNING: Nan detected in component/file %s %s\n",
      detector.component, strlen(detector.filename) ? detector.filename : "");
  if (hasinf)
    printf("WARNING: Inf detected in component/file %s %s\n",
      detector.component, strlen(detector.filename) ? detector.filename : "");

  return(detector);

} /* mcdetector_statistics */

/*******************************************************************************
* detector_import: build detector structure, merge non-lists from MPI
*                    compute basic stat, write "Detector:" line
* RETURN:            detector structure. Invalid data if detector.p1 == NULL
*                    Invalid detector sets m=0 and filename=""
*                    Simulation data  sets m=0 and filename=siminfo_name
* This function is equivalent to the old 'mcdetector_out', returning a structure
*******************************************************************************/
MCDETECTOR detector_import(
  char *format,
  char *component, char *title,
  long m, long n,  long p,
  char *xlabel, char *ylabel, char *zlabel,
  char *xvar, char *yvar, char *zvar,
  double x1, double x2, double y1, double y2, double z1, double z2,
  char *filename,
  double *p0, double *p1, double *p2,
  Coords position)
{
  time_t t;       /* for detector.date */
  long   date_l;  /* date as a long number */
  char   istransposed=0;
  char   c[CHAR_BUF_LENGTH]; /* temp var for signal label */

  MCDETECTOR detector;

  /* build MCDETECTOR structure ============================================= */
  /* make sure we do not have NULL for char fields */

  /* these also apply to simfile */
  strncpy (detector.filename,  filename ? filename : "",        CHAR_BUF_LENGTH);
  strncpy (detector.format,    format   ? format   : "McCode" , CHAR_BUF_LENGTH);
  /* add extension if missing */
  if (strlen(detector.filename) && !strchr(detector.filename, '.'))
  { /* add extension if not in file name already */
    strcat(detector.filename, ".dat");
  }
  strncpy (detector.component, component ? component : MCCODE_STRING " component", CHAR_BUF_LENGTH);

  snprintf(detector.instrument, CHAR_BUF_LENGTH, "%s (%s)", instrument_name, instrument_source);
  snprintf(detector.user, CHAR_BUF_LENGTH,      "%s on %s",
        getenv("USER") ? getenv("USER") : MCCODE_NAME,
        getenv("HOST") ? getenv("HOST") : "localhost");
  time(&t);         /* get current write time */
  date_l = (long)t; /* same but as a long */
  snprintf(detector.date, CHAR_BUF_LENGTH, "%s", ctime(&t));
  if (strlen(detector.date))   detector.date[strlen(detector.date)-1] = '\0'; /* remove last \n in date */
  detector.date_l = date_l;

  if (!mcget_run_num() || mcget_run_num() >= mcget_ncount())
    snprintf(detector.ncount, CHAR_BUF_LENGTH, "%llu", mcget_ncount()
#ifdef USE_MPI
*mpi_node_count
#endif
  );
  else
    snprintf(detector.ncount, CHAR_BUF_LENGTH, "%g/%g", (double)mcget_run_num(), (double)mcget_ncount());

  detector.p0         = p0;
  detector.p1         = p1;
  detector.p2         = p2;

  /* handle transposition (not for NeXus) */
  if (!strcasestr(detector.format, "NeXus")) {
    if (m<0 || n<0 || p<0)             istransposed = !istransposed;
    if (strcasestr(detector.format, "transpose")) istransposed = !istransposed;
    if (istransposed) { /* do the swap once for all */
      long i=m; m=n; n=i;
    }
  }

  m=abs(m); n=abs(n); p=abs(p); /* make sure dimensions are positive */
  detector.istransposed = istransposed;

  /* determine detector rank (dimensionality) */
  if (!m || !n || !p || !p1) detector.rank = 4; /* invalid: exit with m=0 filename="" */
  else if (m*n*p == 1)       detector.rank = 0; /* 0D */
  else if (n == 1 || m == 1) detector.rank = 1; /* 1D */
  else if (p == 1)           detector.rank = 2; /* 2D */
  else                       detector.rank = 3; /* 3D */

  /* from rank, set type */
  switch (detector.rank) {
    case 0:  strcpy(detector.type,  "array_0d"); m=n=p=1; break;
    case 1:  snprintf(detector.type, CHAR_BUF_LENGTH, "array_1d(%ld)", m*n*p); m *= n*p; n=p=1; break;
    case 2:  snprintf(detector.type, CHAR_BUF_LENGTH, "array_2d(%ld, %ld)", m, n*p); n *= p; p=1; break;
    case 3:  snprintf(detector.type, CHAR_BUF_LENGTH, "array_3d(%ld, %ld, %ld)", m, n, p); break;
    default: m=0; strcpy(detector.type, ""); strcpy(detector.filename, "");/* invalid */
  }

  detector.m    = m;
  detector.n    = n;
  detector.p    = p;

  /* these only apply to detector files ===================================== */

  snprintf(detector.position, CHAR_BUF_LENGTH, "%g %g %g", position.x, position.y, position.z);
  /* may also store actual detector orientation in the future */

  strncpy(detector.title,      title && strlen(title) ? title : component,       CHAR_BUF_LENGTH);
  strncpy(detector.xlabel,     xlabel && strlen(xlabel) ? xlabel : "X", CHAR_BUF_LENGTH); /* axis labels */
  strncpy(detector.ylabel,     ylabel && strlen(ylabel) ? ylabel : "Y", CHAR_BUF_LENGTH);
  strncpy(detector.zlabel,     zlabel && strlen(zlabel) ? zlabel : "Z", CHAR_BUF_LENGTH);
  strncpy(detector.xvar,       xvar && strlen(xvar) ? xvar :       "x", CHAR_BUF_LENGTH); /* axis variables */
  strncpy(detector.yvar,       yvar && strlen(yvar) ? yvar :       detector.xvar, CHAR_BUF_LENGTH);
  strncpy(detector.zvar,       zvar && strlen(zvar) ? zvar :       detector.yvar, CHAR_BUF_LENGTH);

  /* set "variables" as e.g. "I I_err N" */
  strcpy(c, "I ");
  if (strlen(detector.zvar))      strncpy(c, detector.zvar,32);
  else if (strlen(detector.yvar)) strncpy(c, detector.yvar,32);
  else if (strlen(detector.xvar)) strncpy(c, detector.xvar,32);

  if (detector.rank == 1)
    snprintf(detector.variables, CHAR_BUF_LENGTH, "%s %s %s_err N", detector.xvar, c, c);
  else
    snprintf(detector.variables, CHAR_BUF_LENGTH, "%s %s_err N", c, c);

  /* limits */
  detector.xmin = x1;
  detector.xmax = x2;
  detector.ymin = y1;
  detector.ymax = y2;
  detector.zmin = z1;
  detector.zmax = z2;
  if (abs(detector.rank) == 1)
    snprintf(detector.limits, CHAR_BUF_LENGTH, "%g %g", x1, x2);
  else if (detector.rank == 2)
    snprintf(detector.limits, CHAR_BUF_LENGTH, "%g %g %g %g", x1, x2, y1, y2);
  else
    snprintf(detector.limits, CHAR_BUF_LENGTH, "%g %g %g %g %g %g", x1, x2, y1, y2, z1, z2);

  /* if MPI and nodes_nb > 1: reduce data sets when using MPI =============== */
#ifdef USE_MPI
  if (!strcasestr(detector.format,"list") && mpi_node_count > 1 && m) {
    /* we save additive data: reduce everything into mpi_node_root */
    if (p0) mc_MPI_Sum(p0, m*n*p);
    if (p1) mc_MPI_Sum(p1, m*n*p);
    if (p2) mc_MPI_Sum(p2, m*n*p);
    if (!p0) {  /* additive signal must be then divided by the number of nodes */
      int i;
      for (i=0; i<m*n*p; i++) {
        p1[i] /= mpi_node_count;
        if (p2) p2[i] /= mpi_node_count;
      }
    }
  }
#endif /* USE_MPI */

  /* compute statistics, Nsum, intensity, Error bars */
  detector = mcdetector_statistics(detector);

#ifdef USE_MPI
  /* slaves are done */
  if(mpi_node_rank != mpi_node_root) {
    return detector;
  }
#endif

  /* output "Detector:" line ================================================ */
  /* when this is a detector written by a component (not the SAVE from instrument),
     not an event lists */
  if (!m) return(detector);
  if (!strcasestr(detector.format,"list")) {
    if (!strcmp(detector.component, instrument_name)) {
      if (strlen(detector.filename))  /* we name it from its filename, or from its title */
        strncpy(c, detector.filename, CHAR_BUF_LENGTH);
      else
        snprintf(c, CHAR_BUF_LENGTH, "%s", instrument_name);
    } else
      strncpy(c, detector.component, CHAR_BUF_LENGTH);  /* usual detectors written by components */

    printf("Detector: %s_I=%g %s_ERR=%g %s_N=%g",
           c, detector.intensity,
           c, detector.error,
           c, detector.events);
    printf(" \"%s\"\n", strlen(detector.filename) ? detector.filename : detector.component);
  }


  return(detector);
} /* detector_import */

/* end MCDETECTOR import section ============================================ */

















/* ========================================================================== */

/*                               ASCII output                                 */
/*     The SIM file is YAML based, the data files have '#' headers            */

/* ========================================================================== */


/*******************************************************************************
* mcinfo_out: output instrument tags/info (only in SIM)
* Used in: siminfo_init (ascii), mcinfo(stdout)
*******************************************************************************/
static void mcinfo_out(char *pre, FILE *f)
{
  char Parameters[CHAR_BUF_LENGTH] = "";
  int  i;

  if (!f || mcdisable_output_files) return;

  /* create parameter string ================================================ */
  for(i = 0; i < numipar; i++)
  {
    char ThisParam[CHAR_BUF_LENGTH];
    if (strlen(mcinputtable[i].name) > CHAR_BUF_LENGTH) break;
    snprintf(ThisParam, CHAR_BUF_LENGTH, " %s(%s)", mcinputtable[i].name,
            (*mcinputtypes[mcinputtable[i].type].parminfo)
                (mcinputtable[i].name));
    strcat(Parameters, ThisParam);
    if (strlen(Parameters) >= CHAR_BUF_LENGTH-64) break;
  }

  /* output data ============================================================ */
  if (f != stdout)
    fprintf(f, "%sFile: %s%c%s\n",    pre, dirname, MC_PATHSEP_C, siminfo_name);
  else
    fprintf(f, "%sCreator: %s\n",     pre, MCCODE_STRING);

  fprintf(f, "%sSource: %s\n",   pre, instrument_source);
  fprintf(f, "%sParameters: %s\n",    pre, Parameters);

  fprintf(f, "%sTrace_enabled: %s\n", pre, traceenabled ? "yes" : "no");
  fprintf(f, "%sDefault_main: %s\n",  pre, defaultmain ?  "yes" : "no");
  fprintf(f, "%sEmbedded_runtime: %s\n", pre,
#ifdef MC_EMBEDDED_RUNTIME
         "yes"
#else
         "no"
#endif
         );

  fflush(f);
} /* mcinfo_out */

/*******************************************************************************
* mcruninfo_out: output simulation tags/info (both in SIM and data files)
* Used in: siminfo_init (ascii case), mcdetector_out_xD_ascii
*******************************************************************************/
static void mcruninfo_out(char *pre, FILE *f)
{
  int i;
  char Parameters[CHAR_BUF_LENGTH];

  if (!f || mcdisable_output_files) return;

  fprintf(f, "%sFormat: %s%s\n",      pre,
    mcformat && strlen(mcformat) ? mcformat : MCCODE_NAME,
    mcformat && strcasestr(mcformat,"McCode") ? " with text headers" : "");
  fprintf(f, "%sURL: %s\n",         pre, "http://www.mccode.org");
  fprintf(f, "%sCreator: %s\n",     pre, MCCODE_STRING);
  fprintf(f, "%sInstrument: %s\n", pre, instrument_source);
  fprintf(f, "%sNcount: %llu\n",        pre, mcget_ncount());
  fprintf(f, "%sTrace: %s\n",       pre, mcdotrace ? "yes" : "no");
  fprintf(f, "%sGravitation: %s\n", pre, mcgravitation ? "yes" : "no");
  snprintf(Parameters, CHAR_BUF_LENGTH, "%ld", mcseed);
  fprintf(f, "%sSeed: %s\n",        pre, Parameters);
  fprintf(f, "%sDirectory: %s\n",        pre, dirname ? dirname : ".");
#ifdef USE_MPI
  if (mpi_node_count > 1)
    fprintf(f, "%sNodes: %i\n",        pre, mpi_node_count);
#endif

  /* output parameter string ================================================ */
  for(i = 0; i < numipar; i++) {
      if (mcinputtable[i].par){
	/* Parameters with a default value */
	if(mcinputtable[i].val && strlen(mcinputtable[i].val)){
	  (*mcinputtypes[mcinputtable[i].type].printer)(Parameters, mcinputtable[i].par);
	  fprintf(f, "%sParam: %s=%s\n", pre, mcinputtable[i].name, Parameters);
        /* ... and those without */
	}else{
	  fprintf(f, "%sParam: %s=NULL\n", pre, mcinputtable[i].name);
	}
      }
  }
  fflush(f);
} /* mcruninfo_out */

/*******************************************************************************
* siminfo_out:    wrapper to fprintf(siminfo_file)
*******************************************************************************/
void siminfo_out(char *format, ...)
{
  va_list ap;

  if(siminfo_file && !mcdisable_output_files)
  {
    va_start(ap, format);
    vfprintf(siminfo_file, format, ap);
    va_end(ap);
  }
} /* siminfo_out */


/*******************************************************************************
* mcdatainfo_out: output detector header
*   mcdatainfo_out(prefix, file_handle, detector) writes info to data file
*******************************************************************************/
static void
mcdatainfo_out(char *pre, FILE *f, MCDETECTOR detector)
{
  if (!f || !detector.m || mcdisable_output_files) return;

  /* output data ============================================================ */
  fprintf(f, "%sDate: %s (%li)\n",       pre, detector.date, detector.date_l);
  fprintf(f, "%stype: %s\n",       pre, detector.type);
  fprintf(f, "%sSource: %s\n",     pre, detector.instrument);
  fprintf(f, "%scomponent: %s\n",  pre, detector.component);
  fprintf(f, "%sposition: %s\n",   pre, detector.position);

  fprintf(f, "%stitle: %s\n",      pre, detector.title);
  fprintf(f, !mcget_run_num() || mcget_run_num() >= mcget_ncount() ?
             "%sNcount: %s\n" :
             "%sratio: %s\n",  pre, detector.ncount);

  if (strlen(detector.filename)) {
    fprintf(f, "%sfilename: %s\n", pre, detector.filename);
  }

  fprintf(f, "%sstatistics: %s\n", pre, detector.statistics);
  fprintf(f, "%ssignal: %s\n",     pre, detector.signal);
  fprintf(f, "%svalues: %s\n",     pre, detector.values);

  if (detector.rank >= 1)
  {
    fprintf(f, "%sxvar: %s\n",     pre, detector.xvar);
    fprintf(f, "%syvar: %s\n",     pre, detector.yvar);
    fprintf(f, "%sxlabel: %s\n",   pre, detector.xlabel);
    fprintf(f, "%sylabel: %s\n",   pre, detector.ylabel);
    if (detector.rank > 1) {
      fprintf(f, "%szvar: %s\n",   pre, detector.zvar);
      fprintf(f, "%szlabel: %s\n", pre, detector.zlabel);
    }
  }

  fprintf(f,
    abs(detector.rank)==1 ?
             "%sxlimits: %s\n" :
             "%sxylimits: %s\n", pre, detector.limits);
  fprintf(f, "%svariables: %s\n", pre,
    strcasestr(detector.format, "list") ? detector.ylabel : detector.variables);

  fflush(f);

} /* mcdatainfo_out */

/* mcdetector_out_array_ascii: output a single array to a file
 *   m: columns
 *   n: rows
 *   p: array
 *   f: file handle (already opened)
 */
static void mcdetector_out_array_ascii(long m, long n, double *p, FILE *f, char istransposed)
{
  if(f)
  {
    int i,j;
    for(j = 0; j < n; j++)
    {
      for(i = 0; i < m; i++)
      {
          fprintf(f, "%.10g ", p[!istransposed ? i*n + j : j*m+i]);
      }
      fprintf(f,"\n");
    }
  }
} /* mcdetector_out_array_ascii */

/*******************************************************************************
* mcdetector_out_0D_ascii: called by mcdetector_out_0D for ascii output
*******************************************************************************/
MCDETECTOR mcdetector_out_0D_ascii(MCDETECTOR detector)
{
  int exists=0;
  FILE *outfile = NULL;

  /* Write data set information to simulation description file. */
  MPI_MASTER(
    siminfo_out("\nbegin data\n"); // detector.component
    mcdatainfo_out("  ", siminfo_file, detector);
    siminfo_out("end data\n");
    /* Don't write if filename is NULL: mcnew_file handles this (return NULL) */
    outfile = mcnew_file(detector.component, "dat", &exists);
    if(outfile)
    {
      /* write data file header and entry in simulation description file */
      mcruninfo_out( "# ", outfile);
      mcdatainfo_out("# ", outfile, detector);
      /* write I I_err N */
      fprintf(outfile, "%g %g %g\n",
        detector.intensity, detector.error, detector.events);
      fclose(outfile);
    }
  ); /* MPI_MASTER */
  return(detector);
} /* mcdetector_out_0D_ascii */

/*******************************************************************************
* mcdetector_out_1D_ascii: called by mcdetector_out_1D for ascii output
*******************************************************************************/
MCDETECTOR mcdetector_out_1D_ascii(MCDETECTOR detector)
{
  int exists=0;
  FILE *outfile = NULL;

  MPI_MASTER(
    /* Write data set information to simulation description file. */
    siminfo_out("\nbegin data\n"); // detector.filename
    mcdatainfo_out("  ", siminfo_file, detector);
    siminfo_out("end data\n");
    /* Loop over array elements, writing to file. */
    /* Don't write if filename is NULL: mcnew_file handles this (return NULL) */
    outfile = mcnew_file(detector.filename, "dat", &exists);
    if(outfile)
    {
      /* write data file header and entry in simulation description file */
      mcruninfo_out( "# ", outfile);
      mcdatainfo_out("# ", outfile, detector);
      /* output the 1D array columns */
      mcdetector_out_array_ascii(detector.m, detector.n, detector.p1, outfile, detector.istransposed);

      fclose(outfile);
    }
  ); /* MPI_MASTER */
  return(detector);

}  /* mcdetector_out_1D_ascii */

/*******************************************************************************
* mcdetector_out_2D_ascii: called by mcdetector_out_2D for ascii output
*******************************************************************************/
MCDETECTOR mcdetector_out_2D_ascii(MCDETECTOR detector)
{
  int exists=0;
  FILE *outfile = NULL;

  MPI_MASTER(
    /* Loop over array elements, writing to file. */
    /* Don't write if filename is NULL: mcnew_file handles this (return NULL) */
    outfile = mcnew_file(detector.filename, "dat", &exists);
    if(outfile)
    {
      /* write header only if file has just been created (not appending) */
      if (!exists) {
        /* Write data set information to simulation description file. */
        siminfo_out("\nbegin data\n"); // detector.filename
        mcdatainfo_out("  ", siminfo_file, detector);
        siminfo_out("end data\n");

        mcruninfo_out( "# ", outfile);
        mcdatainfo_out("# ", outfile,   detector);
        fprintf(outfile, "# Data [%s/%s] %s:\n", detector.component, detector.filename, detector.zvar);
      }
      mcdetector_out_array_ascii(detector.m, detector.n*detector.p, detector.p1,
        outfile, detector.istransposed);
      if (detector.p2) {
        fprintf(outfile, "# Errors [%s/%s] %s_err:\n", detector.component, detector.filename, detector.zvar);
        mcdetector_out_array_ascii(detector.m, detector.n*detector.p, detector.p2,
          outfile, detector.istransposed);
      }
      if (detector.p0) {
        fprintf(outfile, "# Events [%s/%s] N:\n", detector.component, detector.filename);
        mcdetector_out_array_ascii(detector.m, detector.n*detector.p, detector.p0,
          outfile, detector.istransposed);
      }
      fclose(outfile);

      if (!exists) {
        if (strcasestr(detector.format, "list"))
          printf("Events:   \"%s\"\n",
            strlen(detector.filename) ? detector.filename : detector.component);
      }
    } /* if outfile */
  ); /* MPI_MASTER */
#ifdef USE_MPI
  if (strcasestr(detector.format, "list") && mpi_node_count > 1) {
    int node_i=0;
    /* loop along MPI nodes to write sequentially */
    for(node_i=0; node_i<mpi_node_count; node_i++) {
      /* MPI: slaves wait for the master to write its block, then append theirs */
      MPI_Barrier(MPI_COMM_WORLD);
      if (node_i != mpi_node_root && node_i == mpi_node_rank) {
        if(strlen(detector.filename) && !mcdisable_output_files)	/* Don't write if filename is NULL */
          outfile = mcnew_file(detector.filename, "dat", &exists);
        if (!exists)
          fprintf(stderr, "Warning: [MPI node %i] file '%s' does not exist yet, "
                          "MASTER should have opened it before.\n",
            mpi_node_rank, detector.filename);
        if(outfile) {
          mcdetector_out_array_ascii(detector.m, detector.n*detector.p, detector.p1,
            outfile, detector.istransposed);
          fclose(outfile);
        }
      }
    }
  } /* if strcasestr list */
#endif
  return(detector);
} /* mcdetector_out_2D_ascii */

/*******************************************************************************
* strcpy_valid: makes a valid string for variable names.
*   copy 'original' into 'valid', replacing invalid characters by '_'
*   char arrays must be pre-allocated
*******************************************************************************/
static char *strcpy_valid(char *valid, char *original)
{
  long i;
  int  n=CHAR_BUF_LENGTH; /* max length of valid names */

  if (original == NULL || !strlen(original)) return(NULL);

  if (n > strlen(original)) n = strlen(original);
  else original += strlen(original)-n;
  strncpy(valid, original, n);

  for (i=0; i < n; i++)
  {
    if ( (valid[i] > 122)
      || (valid[i] < 32)
      || (strchr("!\"#$%&'()*+,-.:;<=>?@[\\]^`/ \n\r\t", valid[i]) != NULL) )
    {
      if (i) valid[i] = '_'; else valid[i] = 'm';
    }
  }
  valid[i] = '\0';

  return(valid);
} /* strcpy_valid */

/* end ascii output section ================================================= */







#ifdef USE_NEXUS

/* ========================================================================== */

/*                               NeXus output                                 */

/* ========================================================================== */

#define nxprintf(...)    nxstr('d', __VA_ARGS__)
#define nxprintattr(...) nxstr('a', __VA_ARGS__)

/*******************************************************************************
* nxstr: output a tag=value data set (char) in NeXus/current group
*   when 'format' is larger that 1024 chars it is used as value for the 'tag'
*   else the value is assembled with format and following arguments.
*   type='d' -> data set
*        'a' -> attribute for current data set
*******************************************************************************/
static int nxstr(char type, NXhandle *f, char *tag, char *format, ...)
{
  va_list ap;
  char value[CHAR_BUF_LENGTH];
  int  i;
  int  ret=NX_OK;

  if (!tag || !format || !strlen(tag) || !strlen(format)) return(NX_OK);

  /* assemble the value string */
  if (strlen(format) < CHAR_BUF_LENGTH) {
    va_start(ap, format);
    ret = vsnprintf(value, CHAR_BUF_LENGTH, format, ap);
    va_end(ap);

    i = strlen(value);
  } else {
    i = strlen(format);
  }

  if (type == 'd') {
    /* open/put/close data set */
    if (NXmakedata (f, tag, NX_CHAR, 1, &i) != NX_OK) return(NX_ERROR);
    NXopendata (f, tag);
    if (strlen(format) < CHAR_BUF_LENGTH)
      ret = NXputdata  (f, value);
    else
      ret = NXputdata  (f, format);
    NXclosedata(f);
  } else {
    if (strlen(format) < CHAR_BUF_LENGTH)
      ret = NXputattr  (f, tag, value, strlen(value), NX_CHAR);
    else
      ret = NXputattr  (f, tag, format, strlen(format), NX_CHAR);
  }

  return(ret);

} /* nxstr */

/*******************************************************************************
* mcinfo_readfile: read a full file into a string buffer which is allocated
*   Think to free the buffer after use.
* Used in: mcinfo_out_nexus (nexus)
*******************************************************************************/
char *mcinfo_readfile(char *filename)
{
  FILE *f = fopen(filename, "rb");
  if (!f) return(NULL);
  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);
  rewind(f);
  char *string = malloc(fsize + 1);
  if (string) {
    int n = fread(string, fsize, 1, f);
    fclose(f);

    string[fsize] = 0;
  }
  return(string);
}

/*******************************************************************************
* mcinfo_out: output instrument/simulation groups in NeXus file
* Used in: siminfo_init (nexus)
*******************************************************************************/
static void mcinfo_out_nexus(NXhandle f)
{
  FILE  *fid;     /* for intrument source code/C/IDF */
  char  *buffer=NULL;
  time_t t     =time(NULL); /* for date */
  char   entry0[CHAR_BUF_LENGTH];
  int    count=0;
  char   name[CHAR_BUF_LENGTH];
  char   class[CHAR_BUF_LENGTH];

  if (!f || mcdisable_output_files) return;

  /* write NeXus NXroot attributes */
  /* automatically added: file_name, HDF5_Version, file_time, NeXus_version */
  nxprintattr(f, "creator",   "%s generated with " MCCODE_STRING, instrument_name);

  /* count the number of existing NXentry and create the next one */
  NXgetgroupinfo(f, &count, name, class);
  sprintf(entry0, "entry%i", count+1);

  /* create the main NXentry (mandatory in NeXus) */
  if (NXmakegroup(f, entry0, "NXentry") == NX_OK)
  if (NXopengroup(f, entry0, "NXentry") == NX_OK) {

    nxprintf(nxhandle, "program_name", MCCODE_STRING);
    nxprintf(f, "start_time", ctime(&t));
    nxprintf(f, "title", "%s%s%s simulation generated by instrument %s",
      dirname && strlen(dirname) ? dirname : ".", MC_PATHSEP_S, siminfo_name,
      instrument_name);
    nxprintattr(f, "program_name", MCCODE_STRING);
    nxprintattr(f, "instrument",   instrument_name);
    nxprintattr(f, "simulation",   "%s%s%s",
        dirname && strlen(dirname) ? dirname : ".", MC_PATHSEP_S, siminfo_name);

    /* write NeXus instrument group */
    if (NXmakegroup(f, "instrument", "NXinstrument") == NX_OK)
    if (NXopengroup(f, "instrument", "NXinstrument") == NX_OK) {
      int   i;
      char *string=NULL;

      /* write NeXus parameters(types) data =================================== */
      string = (char*)malloc(CHAR_BUF_LENGTH);
      if (string) {
        strcpy(string, "");
        for(i = 0; i < numipar; i++)
        {
          char ThisParam[CHAR_BUF_LENGTH];
          snprintf(ThisParam, CHAR_BUF_LENGTH, " %s(%s)", mcinputtable[i].name,
                  (*mcinputtypes[mcinputtable[i].type].parminfo)
                      (mcinputtable[i].name));
          if (strlen(string) + strlen(ThisParam) < CHAR_BUF_LENGTH)
            strcat(string, ThisParam);
        }
        nxprintattr(f, "Parameters",    string);
        free(string);
      }

      nxprintattr(f, "name",          instrument_name);
      nxprintf   (f, "name",          instrument_name);
      nxprintattr(f, "Source",        instrument_source);

      nxprintattr(f, "Trace_enabled", traceenabled ? "yes" : "no");
      nxprintattr(f, "Default_main",  defaultmain ?  "yes" : "no");
      nxprintattr(f, "Embedded_runtime",
  #ifdef MC_EMBEDDED_RUNTIME
           "yes"
  #else
           "no"
  #endif
           );

      /* add instrument source code when available */
      buffer = mcinfo_readfile(instrument_source);
      if (buffer && strlen(buffer)) {
        long length=strlen(buffer);
        nxprintf (f, "description", buffer);
        NXopendata(f,"description");
        nxprintattr(f, "file_name", instrument_source);
        nxprintattr(f, "file_size", "%li", length);
        nxprintattr(f, "MCCODE_STRING", MCCODE_STRING);
        NXclosedata(f);
        nxprintf (f,"instrument_source", "%s " MCCODE_NAME " " MCCODE_PARTICLE " Monte Carlo simulation", instrument_name);
        free(buffer);
      } else
        nxprintf (f, "description", "File %s not found (instrument description %s is missing)",
          instrument_source, instrument_name);

      /* add Mantid/IDF.xml when available */
      char *IDFfile=NULL;
      IDFfile = (char*)malloc(CHAR_BUF_LENGTH);
      sprintf(IDFfile,"%s%s",instrument_source,".xml");
      buffer = mcinfo_readfile(IDFfile);
      if (buffer && strlen(buffer)) {
        NXmakegroup (nxhandle, "instrument_xml", "NXnote");
        NXopengroup (nxhandle, "instrument_xml", "NXnote");
        nxprintf(f, "data", buffer);
        nxprintf(f, "description", "IDF.xml file found with instrument %s", instrument_source);
        nxprintf(f, "type", "text/xml");
        NXclosegroup(f); /* instrument_xml */
        free(buffer);
      }
      free(IDFfile);
      NXclosegroup(f); /* instrument */
    } /* NXinstrument */

    /* write NeXus simulation group */
    if (NXmakegroup(f, "simulation", "NXnote") == NX_OK)
    if (NXopengroup(f, "simulation", "NXnote") == NX_OK) {

      nxprintattr(f, "name",   "%s%s%s",
        dirname && strlen(dirname) ? dirname : ".", MC_PATHSEP_S, siminfo_name);

      nxprintf   (f, "name",      "%s",     siminfo_name);
      nxprintattr(f, "Format",    mcformat && strlen(mcformat) ? mcformat : MCCODE_NAME);
      nxprintattr(f, "URL",       "http://www.mccode.org");
      nxprintattr(f, "program",   MCCODE_STRING);
      nxprintattr(f, "Instrument",instrument_source);
      nxprintattr(f, "Trace",     mcdotrace ?     "yes" : "no");
      nxprintattr(f, "Gravitation",mcgravitation ? "yes" : "no");
      nxprintattr(f, "Seed",      "%li", mcseed);
      nxprintattr(f, "Directory", dirname);
    #ifdef USE_MPI
      if (mpi_node_count > 1)
        nxprintf(f, "Nodes", "%i",        mpi_node_count);
    #endif

      /* output parameter string ================================================ */
      if (NXmakegroup(f, "Param", "NXparameters") == NX_OK)
      if (NXopengroup(f, "Param", "NXparameters") == NX_OK) {
        int i;
        char string[CHAR_BUF_LENGTH];
        for(i = 0; i < numipar; i++) {
          if (mcget_run_num() || (mcinputtable[i].val && strlen(mcinputtable[i].val))) {
            if (mcinputtable[i].par == NULL)
              strncpy(string, (mcinputtable[i].val ? mcinputtable[i].val : ""), CHAR_BUF_LENGTH);
            else
              (*mcinputtypes[mcinputtable[i].type].printer)(string, mcinputtable[i].par);

            nxprintf(f,  mcinputtable[i].name, "%s", string);
            nxprintattr(f, mcinputtable[i].name, string);
          }
        }
        NXclosegroup(f); /* Param */
      } /* NXparameters */

      NXclosegroup(f); /* simulation */
    } /* NXsimulation */

    /* create a group to hold all monitors */
    NXmakegroup(f, "data", "NXdetector");

    /* leave the NXentry opened (closed at exit) */
  } /* NXentry */
} /* mcinfo_out_nexus */

/*******************************************************************************
* mcdatainfo_out_nexus: output detector header
*   mcdatainfo_out_nexus(detector) create group and write info to NeXus data file
*   open data:NXdetector then filename:NXdata and write headers/attributes
*   requires: NXentry to be opened
*******************************************************************************/
static void
mcdatainfo_out_nexus(NXhandle f, MCDETECTOR detector)
{
  char data_name[CHAR_BUF_LENGTH];
  if (!f || !detector.m || mcdisable_output_files) return;

  strcpy_valid(data_name,
    detector.filename && strlen(detector.filename) ?
      detector.filename : detector.component);

  /* the NXdetector group has been created in mcinfo_out_nexus (siminfo_init) */
  if (NXopengroup(f, "data", "NXdetector") == NX_OK) {

    /* create and open the data group */
    /* this may fail when appending to list -> ignore/skip */
    NXMDisableErrorReporting(); /* unactivate NeXus error messages, as creation may fail */

    if (NXmakegroup(f, data_name, "NXdata") == NX_OK)
    if (NXopengroup(f, data_name, "NXdata") == NX_OK) {

      /* output metadata (as attributes) ======================================== */
      nxprintattr(f, "Date",       detector.date);
      nxprintattr(f, "type",       detector.type);
      nxprintattr(f, "Source",     detector.instrument);
      nxprintattr(f, "component",  detector.component);
      nxprintattr(f, "position",   detector.position);

      nxprintattr(f, "title",      detector.title);
      nxprintattr(f, !mcget_run_num() || mcget_run_num() >= mcget_ncount() ?
                 "Ncount" :
                 "ratio",  detector.ncount);

      if (strlen(detector.filename)) {
        nxprintattr(f, "filename", detector.filename);
      }

      nxprintattr(f, "statistics", detector.statistics);
      nxprintattr(f, "signal",     detector.signal);
      nxprintattr(f, "values",     detector.values);

      if (detector.rank >= 1)
      {
        nxprintattr(f, "xvar",     detector.xvar);
        nxprintattr(f, "yvar",     detector.yvar);
        nxprintattr(f, "xlabel",   detector.xlabel);
        nxprintattr(f, "ylabel",   detector.ylabel);
        if (detector.rank > 1) {
          nxprintattr(f, "zvar",   detector.zvar);
          nxprintattr(f, "zlabel", detector.zlabel);
        }
      }

      nxprintattr(f, abs(detector.rank)==1 ?
                 "xlimits" :
                 "xylimits", detector.limits);
      nxprintattr(f, "variables",
        strcasestr(detector.format, "list") ? detector.ylabel : detector.variables);
      nxprintf(f, "distance", detector.position);
      nxprintf(f, "acquisition_mode",
        strcasestr(detector.format, "list") ? "event" : "summed");

      NXclosegroup(f);
    } /* NXdata (filename) */
    NXMEnableErrorReporting();  /* re-enable NeXus error messages */
    NXclosegroup(f);
  } /* NXdetector (data) */

} /* mcdatainfo_out_nexus */

/*******************************************************************************
* mcdetector_out_axis_nexus: write detector axis into current NXdata
*   requires: NXdata to be opened
*******************************************************************************/
int mcdetector_out_axis_nexus(NXhandle f, char *label, char *var, int rank, long length, double min, double max)
{
  if (!f || length <= 1 || mcdisable_output_files || max == min) return(NX_OK);
  else {
    double axis[length];
    char valid[CHAR_BUF_LENGTH];
    int dim=(int)length;
    int i;
    int nprimary=1;
    /* create an axis from [min:max] */
    for(i = 0; i < length; i++)
      axis[i] = min+(max-min)*(i+0.5)/length;
    /* create the data set */
    strcpy_valid(valid, label);
    NXcompmakedata(f, valid, NX_FLOAT64, 1, &dim, NX_COMP_LZW, &dim);
    /* open it */
    if (NXopendata(f, valid) != NX_OK) {
      fprintf(stderr, "Warning: could not open axis rank %i '%s' (NeXus)\n",
        rank, valid);
      return(NX_ERROR);
    }
    /* put the axis and its attributes */
    NXputdata  (f, axis);
    nxprintattr(f, "long_name",  label);
    nxprintattr(f, "short_name", var);
    NXputattr  (f, "axis",       &rank,     1, NX_INT32);
    nxprintattr(f, "units",      var);
    NXputattr  (f, "primary",    &nprimary, 1, NX_INT32);
    NXclosedata(f);

    return(NX_OK);
  }
} /* mcdetector_out_axis_nexus */

/*******************************************************************************
* mcdetector_out_array_nexus: write detector array into current NXdata (1D,2D)
*   requires: NXdata to be opened
*******************************************************************************/
int mcdetector_out_array_nexus(NXhandle f, char *part, double *data, MCDETECTOR detector)
{

  int dims[3]={detector.m,detector.n,detector.p};  /* number of elements to write */
  int signal=1;
  int exists=0;
  int current_dims[3]={0,0,0};
  int ret=NX_OK;

  if (!f || !data || !detector.m || mcdisable_output_files) return(NX_OK);

  /* when this is a list, we set 1st dimension to NX_UNLIMITED for creation */
  if (strcasestr(detector.format, "list")) dims[0] = NX_UNLIMITED;

  /* create the data set in NXdata group */
  NXMDisableErrorReporting(); /* unactivate NeXus error messages, as creation may fail */
  /* NXcompmakedata fails with NX_UNLIMITED */
  if (strcasestr(detector.format, "list"))
    ret = NXmakedata(    f, part, NX_FLOAT64, detector.rank, dims);
  else
    ret = NXcompmakedata(f, part, NX_FLOAT64, detector.rank, dims, NX_COMP_LZW, dims);
  if (ret != NX_OK) {
    /* failed: data set already exists */
    int datatype=0;
    int rank=0;
    exists=1;
    /* inquire current size of data set (nb of events stored) */
    NXopendata(f, part);
    NXgetinfo(f, &rank, current_dims, &datatype);
    NXclosedata(f);
  }
  NXMEnableErrorReporting();  /* re-enable NeXus error messages */
  dims[0] = detector.m; /* restore actual dimension from data writing */

  /* open the data set */
  if (NXopendata(f, part) == NX_ERROR) {
    fprintf(stderr, "Warning: could not open DataSet %s '%s' (NeXus)\n",
      part, detector.title);
    return(NX_ERROR);
  }
  if (strcasestr(detector.format, "list")) {
    current_dims[1] = current_dims[2] = 0; /* set starting location for writing slab */
    NXputslab(f, data, current_dims, dims);
    if (!exists)
      printf("Events:   \"%s\"\n",
        strlen(detector.filename) ? detector.filename : detector.component);
  } else {
    NXputdata (f, data);
  }

  if (strstr(part,"data") || strstr(part, "events")) {
    NXputattr(f, "signal", &signal, 1, NX_INT32);
    nxprintattr(f, "short_name", detector.filename && strlen(detector.filename) ?
      detector.filename : detector.component);
  }
  nxprintattr(f, "long_name", "%s '%s'", part, detector.title);
  NXclosedata(f);

  return(NX_OK);
} /* mcdetector_out_array_nexus */

/*******************************************************************************
* mcdetector_out_data_nexus: write detector axes+data into current NXdata
*   The data:NXdetector is opened, then filename:NXdata
*   requires: NXentry to be opened
*******************************************************************************/
int mcdetector_out_data_nexus(NXhandle f, MCDETECTOR detector)
{
  char data_name[CHAR_BUF_LENGTH];

  if (!f || !detector.m || mcdisable_output_files) return(NX_OK);

  strcpy_valid(data_name,
    detector.filename && strlen(detector.filename) ?
      detector.filename : detector.component);

  /* the NXdetector group has been created in mcinfo_out_nexus (siminfo_init) */
  if (NXopengroup(f, "data", "NXdetector") == NX_OK) {

    /* the NXdata group has been created in mcdatainfo_out_nexus */
    if (NXopengroup(f, data_name, "NXdata") == NX_OK) {

      /* write axes, for histogram data sets, not for lists */
      if (!strcasestr(detector.format, "list")) {
        mcdetector_out_axis_nexus(f, detector.xlabel, detector.xvar,
          1, detector.m, detector.xmin, detector.xmax);

        mcdetector_out_axis_nexus(f, detector.ylabel, detector.yvar,
          2, detector.n, detector.ymin, detector.ymax);

        mcdetector_out_axis_nexus(f, detector.zlabel, detector.zvar,
          3, detector.p, detector.zmin, detector.zmax);

      } /* !list */

      /* write the actual data (appended if already exists) */
      if (!strcasestr(detector.format, "list")) {
        mcdetector_out_array_nexus(f, "data", detector.p1, detector);
        mcdetector_out_array_nexus(f, "errors", detector.p2, detector);
        mcdetector_out_array_nexus(f, "ncount", detector.p0, detector);
      } else
        mcdetector_out_array_nexus(  f, "events", detector.p1, detector);

      NXclosegroup(f);
    } /* NXdata */
    NXclosegroup(f);
  } /* NXdetector */

  return(NX_OK);
} /* mcdetector_out_array_nexus */

#ifdef USE_MPI
/*******************************************************************************
* mcdetector_out_list_slaves: slaves send their list data to master which writes
*   requires: NXentry to be opened
* WARNING: this method has a flaw: it requires all nodes to flush the lists
*   the same number of times. In case one node is just below the buffer size
*   when finishing (e.g. monitor_nd), it may not trigger save but others may.
*   Then the number of recv/send is not constant along nodes, and simulation stalls.
*******************************************************************************/
MCDETECTOR mcdetector_out_list_slaves(MCDETECTOR detector)
{
  int     node_i=0;
  MPI_MASTER(
	     printf("\n** MPI master gathering slave node list data ** \n");
  );

  if (mpi_node_rank != mpi_node_root) {
    /* MPI slave: slaves send their data to master: 2 MPI_Send calls */
    /* m, n, p must be sent first, since all slaves do not have the same number of events */
    int mnp[3]={detector.m,detector.n,detector.p};

    if (mc_MPI_Send(mnp, 3, MPI_INT, mpi_node_root)!= MPI_SUCCESS)
      fprintf(stderr, "Warning: proc %i to master: MPI_Send mnp list error (mcdetector_out_list_slaves)\n", mpi_node_rank);
    if (!detector.p1
     || mc_MPI_Send(detector.p1, mnp[0]*mnp[1]*mnp[2], MPI_DOUBLE, mpi_node_root) != MPI_SUCCESS)
      fprintf(stderr, "Warning: proc %i to master: MPI_Send p1 list error: mnp=%i (mcdetector_out_list_slaves)\n", mpi_node_rank, abs(mnp[0]*mnp[1]*mnp[2]));
    /* slaves are done: sent mnp and p1 */
    return (detector);
  } /* end slaves */

  /* MPI master: receive data from slaves sequentially: 2 MPI_Recv calls */

  if (mpi_node_rank == mpi_node_root) {
    for(node_i=0; node_i<mpi_node_count; node_i++) {
      double *this_p1=NULL;                               /* buffer to hold the list from slaves */
      int     mnp[3]={0,0,0};  /* size of this buffer */
      if (node_i != mpi_node_root) { /* get data from slaves */
	if (mc_MPI_Recv(mnp, 3, MPI_INT, node_i) != MPI_SUCCESS)
	  fprintf(stderr, "Warning: master from proc %i: "
		  "MPI_Recv mnp list error (mcdetector_write_data)\n", node_i);
	if (mnp[0]*mnp[1]*mnp[2]) {
	  this_p1 = (double *)calloc(mnp[0]*mnp[1]*mnp[2], sizeof(double));
	  if (!this_p1 || mc_MPI_Recv(this_p1, abs(mnp[0]*mnp[1]*mnp[2]), MPI_DOUBLE, node_i)!= MPI_SUCCESS)
	    fprintf(stderr, "Warning: master from proc %i: "
		    "MPI_Recv p1 list error: mnp=%i (mcdetector_write_data)\n", node_i, mnp[0]*mnp[1]*mnp[2]);
	  else {
	    printf(". MPI master writing data for slave node %i\n",node_i);
	    detector.p1 = this_p1;
	    detector.m  = mnp[0]; detector.n  = mnp[1]; detector.p  = mnp[2];

	    mcdetector_out_data_nexus(nxhandle, detector);
	  }
	}
      } /* if not master */
    } /* for */
  MPI_MASTER(
	     printf("\n** Done ** \n");
  );
  }
}
#endif

MCDETECTOR mcdetector_out_0D_nexus(MCDETECTOR detector)
{
  /* Write data set information to NeXus file. */
  MPI_MASTER(
    mcdatainfo_out_nexus(nxhandle, detector);
  );

  return(detector);
} /* mcdetector_out_0D_ascii */

MCDETECTOR mcdetector_out_1D_nexus(MCDETECTOR detector)
{
  MPI_MASTER(
  mcdatainfo_out_nexus(nxhandle, detector);
  mcdetector_out_data_nexus(nxhandle, detector);
  );
  return(detector);
} /* mcdetector_out_1D_ascii */

MCDETECTOR mcdetector_out_2D_nexus(MCDETECTOR detector)
{
  MPI_MASTER(
  mcdatainfo_out_nexus(nxhandle, detector);
  mcdetector_out_data_nexus(nxhandle, detector);
  );

#ifdef USE_MPI // and USE_NEXUS
  /* NeXus: slave nodes have master write their lists */
  if (strcasestr(detector.format, "list") && mpi_node_count > 1) {
    mcdetector_out_list_slaves(detector);
  }
#endif /* USE_MPI */

  return(detector);
} /* mcdetector_out_2D_nexus */

#endif /* USE_NEXUS*/








/* ========================================================================== */

/*                            Main input functions                            */
/*            DETECTOR_OUT_xD function calls -> ascii or NeXus                */

/* ========================================================================== */

/*******************************************************************************
* siminfo_init:   open SIM and write header
*******************************************************************************/
FILE *siminfo_init(FILE *f)
{
  int exists=0;

  /* check format */
  if (!mcformat || !strlen(mcformat)
   || !strcasecmp(mcformat, "MCSTAS") || !strcasecmp(mcformat, "MCXTRACE")
   || !strcasecmp(mcformat, "PGPLOT") || !strcasecmp(mcformat, "GNUPLOT") || !strcasecmp(mcformat, "MCCODE")
   || !strcasecmp(mcformat, "MATLAB")) {
    mcformat="McCode";
#ifdef USE_NEXUS
  } else if (strcasestr(mcformat, "NeXus")) {
    /* Do nothing */
#endif
  } else {
    fprintf(stderr,
	    "Warning: You have requested the output format %s which is unsupported by this binary. Resetting to standard %s format.\n",mcformat ,"McCode");
    mcformat="McCode";
  }

  /* open the SIM file if not defined yet */
  if (siminfo_file || mcdisable_output_files)
    return (siminfo_file);

#ifdef USE_NEXUS
  /* only master writes NeXus header: calls NXopen(nxhandle) */
  if (mcformat && strcasestr(mcformat, "NeXus")) {
	  MPI_MASTER(
	  siminfo_file = mcnew_file(siminfo_name, "h5", &exists);
    if(!siminfo_file)
      fprintf(stderr,
	      "Warning: could not open simulation description file '%s'\n",
	      siminfo_name);
	  else
	    mcinfo_out_nexus(nxhandle);
	  );
    return(siminfo_file); /* points to nxhandle */
  }
#endif

  /* write main description file (only MASTER) */
  MPI_MASTER(

  siminfo_file = mcnew_file(siminfo_name, "sim", &exists);
  if(!siminfo_file)
    fprintf(stderr,
	    "Warning: could not open simulation description file '%s'\n",
	    siminfo_name);
  else
  {
    /* write SIM header */
    time_t t=time(NULL);
    siminfo_out("%s simulation description file for %s.\n",
      MCCODE_NAME, instrument_name);
    siminfo_out("Date:    %s", ctime(&t)); /* includes \n */
    siminfo_out("Program: %s\n\n", MCCODE_STRING);

    siminfo_out("begin instrument: %s\n", instrument_name);
    mcinfo_out(   "  ", siminfo_file);
    siminfo_out("end instrument\n");

    siminfo_out("\nbegin simulation: %s\n", dirname);
    mcruninfo_out("  ", siminfo_file);
    siminfo_out("end simulation\n");

  }
  return (siminfo_file);

  ); /* MPI_MASTER */

} /* siminfo_init */

/*******************************************************************************
*   siminfo_close:  close SIM
*******************************************************************************/
void siminfo_close()
{
#ifdef USE_MPI
  if(mpi_node_rank == mpi_node_root) {
#endif
  if(siminfo_file && !mcdisable_output_files) {
#ifdef USE_NEXUS
    if (mcformat && strcasestr(mcformat, "NeXus")) {
      time_t t=time(NULL);
      nxprintf(nxhandle, "end_time", ctime(&t));
      nxprintf(nxhandle, "duration", "%li", (long)t-mcstartdate);
      NXclosegroup(nxhandle); /* NXentry */
      NXclose(&nxhandle);
    } else {
#endif
      fclose(siminfo_file);
#ifdef USE_NEXUS
    }
#endif
#ifdef USE_MPI
  }
#endif
    siminfo_file = NULL;
  }
} /* siminfo_close */

/*******************************************************************************
* mcdetector_out_0D: wrapper for 0D (single value).
*   Output single detector/monitor data (p0, p1, p2).
*   Title is t, component name is c.
*******************************************************************************/
MCDETECTOR mcdetector_out_0D(char *t, double p0, double p1, double p2,
                         char *c, Coords posa)
{
  /* import and perform basic detector analysis (and handle MPI reduce) */
  MCDETECTOR detector = detector_import(mcformat,
    c, (t ? t : MCCODE_STRING " data"),
    1, 1, 1,
    "I", "", "",
    "I", "", "",
    0, 0, 0, 0, 0, 0, "",
    &p0, &p1, &p2, posa); /* write Detector: line */

#ifdef USE_NEXUS
  if (strcasestr(detector.format, "NeXus"))
    return(mcdetector_out_0D_nexus(detector));
  else
#endif
    return(mcdetector_out_0D_ascii(detector));

} /* mcdetector_out_0D */



/*******************************************************************************
* mcdetector_out_1D: wrapper for 1D.
*   Output 1d detector data (p0, p1, p2) for n bins linearly
*   distributed across the range x1..x2 (x1 is lower limit of first
*   bin, x2 is upper limit of last bin). Title is t, axis labels are xl
*   and yl. File name is f, component name is c.
*
*   t:    title
*   xl:   x-label
*   yl:   y-label
*   xvar: measured variable length
*   x1:   x axus min
*   x2:   x axis max
*   n:    1d data vector lenght
*   p0:   pntr to start of data block#0
*   p1:   pntr to start of data block#1
*   p2:   pntr to start of data block#2
*   f:    filename
*
*   Not included in the macro, and here forwarded to detector_import:
*   c:    ?
*   posa: ?
*******************************************************************************/
MCDETECTOR mcdetector_out_1D(char *t, char *xl, char *yl,
        char *xvar, double x1, double x2,
        long n,
        double *p0, double *p1, double *p2, char *f,
        char *c, Coords posa)
{
  /* import and perform basic detector analysis (and handle MPI_Reduce) */
  MCDETECTOR detector = detector_import(mcformat,
    c, (t ? t : MCCODE_STRING " 1D data"),
    n, 1, 1,
    xl, yl, (n > 1 ? "Signal per bin" : " Signal"),
    xvar, "(I,I_err)", "I",
    x1, x2, 0, 0, 0, 0, f,
    p0, p1, p2, posa); /* write Detector: line */
  if (!detector.p1 || !detector.m) return(detector);

#ifdef USE_NEXUS
  if (strcasestr(detector.format, "NeXus"))
    return(mcdetector_out_1D_nexus(detector));
  else
#endif
    return(mcdetector_out_1D_ascii(detector));

} /* mcdetector_out_1D */

/*******************************************************************************
* mcdetector_out_2D: wrapper for 2D.
*   Special case for list: master creates file first, then slaves append their
*   blocks without header-
*
*   t:    title
*   xl:   x-label
*   yl:   y-label
*   x1:   x axus min
*   x2:   x axis max
*   y1:   y axis min
*   y2:   y axis max
*   m:    dim 1 (x) size
*   n:    dim 2 (y) size
*   p0:   pntr to start of data block#0
*   p1:   pntr to start of data block#1
*   p2:   pntr to start of data block#2
*   f:    filename
*
*   Not included in the macro, and here forwarded to detector_import:
*   c:    ?
*   posa: ?
*******************************************************************************/
MCDETECTOR mcdetector_out_2D(char *t, char *xl, char *yl,
                  double x1, double x2, double y1, double y2,
                  long m, long n,
                  double *p0, double *p1, double *p2, char *f,
                  char *c, Coords posa)
{
  char xvar[CHAR_BUF_LENGTH];
  char yvar[CHAR_BUF_LENGTH];

  /* create short axes labels */
  if (xl && strlen(xl)) { strncpy(xvar, xl, CHAR_BUF_LENGTH); xvar[2]='\0'; }
  else strcpy(xvar, "x");
  if (yl && strlen(yl)) { strncpy(yvar, yl, CHAR_BUF_LENGTH); yvar[2]='\0'; }
  else strcpy(yvar, "y");

  MCDETECTOR detector;

  /* import and perform basic detector analysis (and handle MPI_Reduce) */
  if (abs(m) == 1) {/* n>1 on Y, m==1 on X: 1D, no X axis*/
    detector = detector_import(mcformat,
      c, (t ? t : MCCODE_STRING " 1D data"),
      n, 1, 1,
      yl, "", "Signal per bin",
      yvar, "(I,Ierr)", "I",
      y1, y2, x1, x2, 0, 0, f,
      p0, p1, p2, posa); /* write Detector: line */
  } else if (abs(n)==1) {/* m>1 on X, n==1 on Y: 1D, no Y axis*/
    detector = detector_import(mcformat,
      c, (t ? t : MCCODE_STRING " 1D data"),
      m, 1, 1,
      xl, "", "Signal per bin",
      xvar, "(I,Ierr)", "I",
      x1, x2, y1, y2, 0, 0, f,
      p0, p1, p2, posa); /* write Detector: line */
  }else {
    detector = detector_import(mcformat,
      c, (t ? t : MCCODE_STRING " 2D data"),
      m, n, 1,
      xl, yl, "Signal per bin",
      xvar, yvar, "I",
      x1, x2, y1, y2, 0, 0, f,
      p0, p1, p2, posa); /* write Detector: line */
  }

  if (!detector.p1 || !detector.m) return(detector);

#ifdef USE_NEXUS
  if (strcasestr(detector.format, "NeXus"))
    return(mcdetector_out_2D_nexus(detector));
  else
#endif
    return(mcdetector_out_2D_ascii(detector));

} /* mcdetector_out_2D */

/*******************************************************************************
* mcdetector_out_list: wrapper for list output (calls out_2D with mcformat+"list").
*   m=number of events, n=size of each event
*******************************************************************************/
MCDETECTOR mcdetector_out_list(char *t, char *xl, char *yl,
                  long m, long n,
                  double *p1, char *f,
                  char *c, Coords posa)
{
  char       format_new[CHAR_BUF_LENGTH];
  char      *format_org;
  MCDETECTOR detector;

  format_org = mcformat;
  strcpy(format_new, mcformat);
  strcat(format_new, " list");
  mcformat = format_new;

  detector = mcdetector_out_2D(t, xl, yl,
                  1,abs(m),1,abs(n),
                  m,n,
                  NULL, p1, NULL, f,
                  c, posa);

  mcformat = format_org;
  return(detector);
}

/*******************************************************************************
 * mcuse_dir: set data/sim storage directory and create it,
 * or exit with error if exists
 ******************************************************************************/
static void
mcuse_dir(char *dir)
{
  if (!dir || !strlen(dir)) return;
#ifdef MC_PORTABLE
  fprintf(stderr, "Error: "
          "Directory output cannot be used with portable simulation (mcuse_dir)\n");
  exit(1);
#else  /* !MC_PORTABLE */
  /* handle file://directory URL type */
  if (strncmp(dir, "file://", strlen("file://")))
    dirname = dir;
  else
    dirname = dir+strlen("file://");


#ifdef USE_MPI
  if(mpi_node_rank == mpi_node_root) {
#endif
    if(mkdir(dirname, 0777)) {
#ifndef DANSE
      fprintf(stderr, "Error: unable to create directory '%s' (mcuse_dir)\n", dir);
      fprintf(stderr, "(Maybe the directory already exists?)\n");
#endif
#ifdef USE_MPI
      MPI_Abort(MPI_COMM_WORLD, -1);
#endif
    exit(-1);
    }
#ifdef USE_MPI
    }
#endif

  /* remove trailing PATHSEP (if any) */
  while (strlen(dirname) && dirname[strlen(dirname) - 1] == MC_PATHSEP_C)
    dirname[strlen(dirname) - 1]='\0';
#endif /* !MC_PORTABLE */
} /* mcuse_dir */

/*******************************************************************************
* mcinfo: display instrument simulation info to stdout and exit
*******************************************************************************/
static void
mcinfo(void)
{
  fprintf(stdout, "begin instrument: %s\n", instrument_name);
  mcinfo_out("  ", stdout);
  fprintf(stdout, "end instrument\n");
  fprintf(stdout, "begin simulation: %s\n", dirname ? dirname : ".");
  mcruninfo_out("  ", stdout);
  fprintf(stdout, "end simulation\n");
  exit(0); /* includes MPI_Finalize in MPI mode */
} /* mcinfo */

#endif /* ndef MCCODE_R_IO_C */

/* end of the I/O section =================================================== */







/*******************************************************************************
* mcset_ncount: set total number of rays to generate
*******************************************************************************/
void mcset_ncount(unsigned long long int count)
{
  mcncount = count;
}

/* mcget_ncount: get total number of rays to generate */
unsigned long long int mcget_ncount(void)
{
  return mcncount;
}

/* mcget_run_num: get curent number of rays */
/* Within the TRACE scope we are now using _particle->uid directly */
unsigned long long int mcget_run_num() // shuld be (_class_particle* _particle) somehow
{
  /* This function only remains for the few cases outside TRACE where we need to know
     the number of simulated particles */
  return mcrun_num;
}

/* mcsetn_arg: get ncount from a string argument */
static void
mcsetn_arg(char *arg)
{
  mcset_ncount((long long int) strtod(arg, NULL));
}

/* mcsetseed: set the random generator seed from a string argument */
static void
mcsetseed(char *arg)
{
  mcseed = atol(arg);
  if(!mcseed) {
  //  srandom(mcseed);
  //} else {
    fprintf(stderr, "Error: seed must not be zero (mcsetseed)\n");
    exit(1);
  }
}

/* Following part is only embedded when not redundent with mccode-r.h ========= */

#ifndef MCCODE_H

/* SECTION: MCDISPLAY support. =============================================== */

/*******************************************************************************
* Just output MCDISPLAY keywords to be caught by an external plotter client.
*******************************************************************************/

void mcdis_magnify(char *what){
  // Do nothing here, better use interactive zoom from the tools
}

void mcdis_line(double x1, double y1, double z1,
                double x2, double y2, double z2){
  printf("MCDISPLAY: multiline(2,%g,%g,%g,%g,%g,%g)\n",
         x1,y1,z1,x2,y2,z2);
}

void mcdis_dashed_line(double x1, double y1, double z1,
		       double x2, double y2, double z2, int n){
  int i;
  const double dx = (x2-x1)/(2*n+1);
  const double dy = (y2-y1)/(2*n+1);
  const double dz = (z2-z1)/(2*n+1);

  for(i = 0; i < n+1; i++)
    mcdis_line(x1 + 2*i*dx,     y1 + 2*i*dy,     z1 + 2*i*dz,
	       x1 + (2*i+1)*dx, y1 + (2*i+1)*dy, z1 + (2*i+1)*dz);
}

void mcdis_multiline(int count, ...){
  va_list ap;
  double x,y,z;

  printf("MCDISPLAY: multiline(%d", count);
  va_start(ap, count);
  while(count--)
    {
    x = va_arg(ap, double);
    y = va_arg(ap, double);
    z = va_arg(ap, double);
    printf(",%g,%g,%g", x, y, z);
    }
  va_end(ap);
  printf(")\n");
}

void mcdis_rectangle(char* plane, double x, double y, double z,
		     double width, double height){
  /* draws a rectangle in the plane           */
  /* x is ALWAYS width and y is ALWAYS height */
  if (strcmp("xy", plane)==0) {
    mcdis_multiline(5,
		    x - width/2, y - height/2, z,
		    x + width/2, y - height/2, z,
		    x + width/2, y + height/2, z,
		    x - width/2, y + height/2, z,
		    x - width/2, y - height/2, z);
  } else if (strcmp("xz", plane)==0) {
    mcdis_multiline(5,
		    x - width/2, y, z - height/2,
		    x + width/2, y, z - height/2,
		    x + width/2, y, z + height/2,
		    x - width/2, y, z + height/2,
		    x - width/2, y, z - height/2);
  } else if (strcmp("yz", plane)==0) {
    mcdis_multiline(5,
		    x, y - height/2, z - width/2,
		    x, y - height/2, z + width/2,
		    x, y + height/2, z + width/2,
		    x, y + height/2, z - width/2,
		    x, y - height/2, z - width/2);
  } else {

    fprintf(stderr, "Error: Definition of plane %s unknown\n", plane);
    exit(1);
  }
}

/*  draws a box with center at (x, y, z) and
    width (deltax), height (deltay), length (deltaz) */
void mcdis_box(double x, double y, double z,
	       double width, double height, double length){

  mcdis_rectangle("xy", x, y, z-length/2, width, height);
  mcdis_rectangle("xy", x, y, z+length/2, width, height);
  mcdis_line(x-width/2, y-height/2, z-length/2,
	     x-width/2, y-height/2, z+length/2);
  mcdis_line(x-width/2, y+height/2, z-length/2,
	     x-width/2, y+height/2, z+length/2);
  mcdis_line(x+width/2, y-height/2, z-length/2,
	     x+width/2, y-height/2, z+length/2);
  mcdis_line(x+width/2, y+height/2, z-length/2,
	     x+width/2, y+height/2, z+length/2);
}

void mcdis_circle(char *plane, double x, double y, double z, double r){
  printf("MCDISPLAY: circle('%s',%g,%g,%g,%g)\n", plane, x, y, z, r);
}

/* Draws a circle with center (x,y,z), radius (r), and in the plane
 * with normal (nx,ny,nz)*/
void mcdis_Circle(double x, double y, double z, double r, double nx, double ny, double nz){
    int i;
    if(nx==0 && ny && nz==0){
        for (i=0;i<24; i++){
            mcdis_line(x+r*sin(i*2*PI/24),y,z+r*cos(i*2*PI/24),
                    x+r*sin((i+1)*2*PI/24),y,z+r*cos((i+1)*2*PI/24));
        }
    }else{
        double mx,my,mz;
        /*generate perpendicular vector using (nx,ny,nz) and (0,1,0)*/
        vec_prod(mx,my,mz, 0,1,0, nx,ny,nz);
        NORM(mx,my,mz);
        /*draw circle*/
        for (i=0;i<24; i++){
            double ux,uy,uz;
            double wx,wy,wz;
            rotate(ux,uy,uz, mx,my,mz, i*2*PI/24, nx,ny,nz);
            rotate(wx,wy,wz, mx,my,mz, (i+1)*2*PI/24, nx,ny,nz);
            mcdis_line(x+ux*r,y+uy*r,z+uz*r,
                    x+wx*r,y+wy*r,z+wz*r);
        }
    }
}

/* Draws a cylinder with center at (x,y,z) with extent (r,height).
 * The cylinder axis is along the vector nx,ny,nz.
 * N determines how many vertical lines are drawn.*/
void mcdis_cylinder( double x, double y, double z,
        double r, double height, int N, double nx, double ny, double nz){
    int i;
    /*no lines make little sense - so trigger the default*/
    if(N<=0) N=5;

    NORM(nx,ny,nz);
    double h_2=height/2.0;
    mcdis_Circle(x+nx*h_2,y+ny*h_2,z+nz*h_2,r,nx,ny,nz);
    mcdis_Circle(x-nx*h_2,y-ny*h_2,z-nz*h_2,r,nx,ny,nz);

    double mx,my,mz;
    /*generate perpendicular vector using (nx,ny,nz) and (0,1,0)*/
    if(nx==0 && ny && nz==0){
        mx=my=0;mz=1;
    }else{
        vec_prod(mx,my,mz, 0,1,0, nx,ny,nz);
        NORM(mx,my,mz);
    }
    /*draw circle*/
    for (i=0; i<24; i++){
        double ux,uy,uz;
        rotate(ux,uy,uz, mx,my,mz, i*2*PI/24, nx,ny,nz);
        mcdis_line(x+nx*h_2+ux*r, y+ny*h_2+uy*r, z+nz*h_2+uz*r,
                 x-nx*h_2+ux*r, y-ny*h_2+uy*r, z-nz*h_2+uz*r);
    }
}

/* draws a sphere with center at (x,y,z) with extent (r)
 * The sphere is drawn using N longitudes and N latitudes.*/
void mcdis_sphere(double x, double y, double z, double r, int N){
    double nx,ny,nz;
    int i;
    /*no lines make little sense - so trigger the default*/
    if(N<=0) N=5;

    nx=0;ny=0;nz=1;
    mcdis_Circle(x,y,z,r,nx,ny,nz);
    for (i=1;i<N;i++){
        rotate(nx,ny,nz, nx,ny,nz, PI/N, 0,1,0);
        mcdis_Circle(x,y,z,r,nx,ny,nz);
    }
    /*lastly draw a great circle perpendicular to all N circles*/
    //mcdis_Circle(x,y,z,radius,1,0,0);

    for (i=1;i<=N;i++){
        double yy=-r+ 2*r*((double)i/(N+1));
        mcdis_Circle(x,y+yy ,z,  sqrt(r*r-yy*yy) ,0,1,0);
    }
}

/* SECTION: coordinates handling ============================================ */

/*******************************************************************************
* Since we use a lot of geometric calculations using Cartesian coordinates,
* we collect some useful routines here. However, it is also permissible to
* work directly on the underlying struct coords whenever that is most
* convenient (that is, the type Coords is not abstract).
*
* Coordinates are also used to store rotation angles around x/y/z axis.
*
* Since coordinates are used much like a basic type (such as double), the
* structure itself is passed and returned, rather than a pointer.
*
* At compile-time, the values of the coordinates may be unknown (for example
* a motor position). Hence coordinates are general expressions and not simple
* numbers. For this we used the type Coords_exp which has three CExp
* fields. For runtime (or calculations possible at compile time), we use
* Coords which contains three double fields.
*******************************************************************************/

/* coords_set: Assign coordinates. */
Coords coords_set(MCNUM x, MCNUM y, MCNUM z)
{
  Coords a;

  a.x = x;
  a.y = y;
  a.z = z;
  return a;
}

/* coords_get: get coordinates. Required when 'x','y','z' are #defined as ray pars */
Coords coords_get(Coords a, MCNUM *x, MCNUM *y, MCNUM *z)
{
  *x = a.x;
  *y = a.y;
  *z = a.z;
  return a;
}

/* coords_add: Add two coordinates. */
Coords coords_add(Coords a, Coords b)
{
  Coords c;

  c.x = a.x + b.x;
  c.y = a.y + b.y;
  c.z = a.z + b.z;
  if (fabs(c.z) < 1e-14) c.z=0.0;
  return c;
}

/* coords_sub: Subtract two coordinates. */
Coords coords_sub(Coords a, Coords b)
{
  Coords c;

  c.x = a.x - b.x;
  c.y = a.y - b.y;
  c.z = a.z - b.z;
  if (fabs(c.z) < 1e-14) c.z=0.0;
  return c;
}

/* coords_neg: Negate coordinates. */
Coords coords_neg(Coords a)
{
  Coords b;

  b.x = -a.x;
  b.y = -a.y;
  b.z = -a.z;
  return b;
}

/* coords_scale: Scale a vector. */
Coords coords_scale(Coords b, double scale) {
  Coords a;

  a.x = b.x*scale;
  a.y = b.y*scale;
  a.z = b.z*scale;
  return a;
}

/* coords_sp: Scalar product: a . b */
double coords_sp(Coords a, Coords b) {
  double value;

  value = a.x*b.x + a.y*b.y + a.z*b.z;
  return value;
}

/* coords_xp: Cross product: a = b x c. */
Coords coords_xp(Coords b, Coords c) {
  Coords a;

  a.x = b.y*c.z - c.y*b.z;
  a.y = b.z*c.x - c.z*b.x;
  a.z = b.x*c.y - c.x*b.y;
  return a;
}

/* coords_len: Gives length of coords set. */
double coords_len(Coords a) {
  return sqrt(a.x*a.x + a.y*a.y + a.z*a.z);
}

/* coords_mirror: Mirror a in plane (through the origin) defined by normal n*/
Coords coords_mirror(Coords a, Coords n) {
  double t = scalar_prod(n.x, n.y, n.z, n.x, n.y, n.z);
  Coords b;
  if (t!=1) {
    t = sqrt(t);
    n.x /= t;
    n.y /= t;
    n.z /= t;
  }
  t=scalar_prod(a.x, a.y, a.z, n.x, n.y, n.z);
  b.x = a.x-2*t*n.x;
  b.y = a.y-2*t*n.y;
  b.z = a.z-2*t*n.z;
  return b;
}

/* coords_print: Print out vector values. */
void coords_print(Coords a) {
  #ifndef OPENACC
  fprintf(stdout, "(%f, %f, %f)\n", a.x, a.y, a.z);
  #endif
  return;
}

mcstatic void coords_norm(Coords* c) {
	double temp = coords_sp(*c,*c);

	// Skip if we will end dividing by zero
	if (temp == 0) return;

	temp = sqrt(temp);

	c->x /= temp;
	c->y /= temp;
	c->z /= temp;
}

/*******************************************************************************
* The Rotation type implements a rotation transformation of a coordinate
* system in the form of a double[3][3] matrix.
*
* Contrary to the Coords type in coords.c, rotations are passed by
* reference. Functions that yield new rotations do so by writing to an
* explicit result parameter; rotations are not returned from functions. The
* reason for this is that arrays cannot by returned from functions (though
* structures can; thus an alternative would have been to wrap the
* double[3][3] array up in a struct). Such are the ways of C programming.
*
* A rotation represents the tranformation of the coordinates of a vector when
* changing between coordinate systems that are rotated with respect to each
* other. For example, suppose that coordinate system Q is rotated 45 degrees
* around the Z axis with respect to coordinate system P. Let T be the
* rotation transformation representing a 45 degree rotation around Z. Then to
* get the coordinates of a vector r in system Q, apply T to the coordinates
* of r in P. If r=(1,0,0) in P, it will be (sqrt(1/2),-sqrt(1/2),0) in
* Q. Thus we should be careful when interpreting the sign of rotation angles:
* they represent the rotation of the coordinate systems, not of the
* coordinates (which has opposite sign).
*******************************************************************************/

/*******************************************************************************
* rot_set_rotation: Get transformation for rotation first phx around x axis,
* then phy around y, then phz around z.
*******************************************************************************/
void rot_set_rotation(Rotation t, double phx, double phy, double phz)
{
  if ((phx == 0) && (phy == 0) && (phz == 0)) {
    t[0][0] = 1.0;
    t[0][1] = 0.0;
    t[0][2] = 0.0;
    t[1][0] = 0.0;
    t[1][1] = 1.0;
    t[1][2] = 0.0;
    t[2][0] = 0.0;
    t[2][1] = 0.0;
    t[2][2] = 1.0;
  } else {
    double cx = cos(phx);
    double sx = sin(phx);
    double cy = cos(phy);
    double sy = sin(phy);
    double cz = cos(phz);
    double sz = sin(phz);

    t[0][0] = cy*cz;
    t[0][1] = sx*sy*cz + cx*sz;
    t[0][2] = sx*sz - cx*sy*cz;
    t[1][0] = -cy*sz;
    t[1][1] = cx*cz - sx*sy*sz;
    t[1][2] = sx*cz + cx*sy*sz;
    t[2][0] = sy;
    t[2][1] = -sx*cy;
    t[2][2] = cx*cy;
  }
}

/*******************************************************************************
* rot_test_identity: Test if rotation is identity
*******************************************************************************/
int rot_test_identity(Rotation t)
{
  return (t[0][0] + t[1][1] + t[2][2] == 3);
}

/*******************************************************************************
* rot_mul: Matrix multiplication of transformations (this corresponds to
* combining transformations). After rot_mul(T1, T2, T3), doing T3 is
* equal to doing first T2, then T1.
* Note that T3 must not alias (use the same array as) T1 or T2.
*******************************************************************************/
void rot_mul(Rotation t1, Rotation t2, Rotation t3)
{
  if (rot_test_identity(t1)) {
    rot_copy(t3, t2);
  } else if (rot_test_identity(t2)) {
    rot_copy(t3, t1);
  } else {
    int i,j;
    for(i = 0; i < 3; i++)
      for(j = 0; j < 3; j++)
	t3[i][j] = t1[i][0]*t2[0][j] + t1[i][1]*t2[1][j] + t1[i][2]*t2[2][j];
  }
}

/*******************************************************************************
* rot_copy: Copy a rotation transformation (arrays cannot be assigned in C).
*******************************************************************************/
void rot_copy(Rotation dest, Rotation src)
{
  int i,j;
  for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++)
      dest[i][j] = src[i][j];
}

/*******************************************************************************
* rot_transpose: Matrix transposition, which is inversion for Rotation matrices
*******************************************************************************/
void rot_transpose(Rotation src, Rotation dst)
{
  dst[0][0] = src[0][0];
  dst[0][1] = src[1][0];
  dst[0][2] = src[2][0];
  dst[1][0] = src[0][1];
  dst[1][1] = src[1][1];
  dst[1][2] = src[2][1];
  dst[2][0] = src[0][2];
  dst[2][1] = src[1][2];
  dst[2][2] = src[2][2];
}

/*******************************************************************************
* rot_apply: returns t*a
*******************************************************************************/
Coords rot_apply(Rotation t, Coords a)
{
  Coords b;
  if (rot_test_identity(t)) {
    return a;
  } else {
    b.x = t[0][0]*a.x + t[0][1]*a.y + t[0][2]*a.z;
    b.y = t[1][0]*a.x + t[1][1]*a.y + t[1][2]*a.z;
    b.z = t[2][0]*a.x + t[2][1]*a.y + t[2][2]*a.z;
    return b;
  }
}

/**
 * Pretty-printing of rotation matrices.
 */
void rot_print(Rotation rot) {
	printf("[ %4.2f %4.2f %4.2f ]\n",
			rot[0][0], rot[0][1], rot[0][2]);
	printf("[ %4.2f %4.2f %4.2f ]\n",
			rot[1][0], rot[1][1], rot[1][2]);
	printf("[ %4.2f %4.2f %4.2f ]\n\n",
			rot[2][0], rot[2][1], rot[2][2]);
}

/**
 * Vector product: used by vec_prod (mccode-r.h). Use coords_xp for Coords.
 */
void vec_prod_func(double *x, double *y, double *z,
		double x1, double y1, double z1,
		double x2, double y2, double z2) {
    *x = (y1)*(z2) - (y2)*(z1);
    *y = (z1)*(x2) - (z2)*(x1);
    *z = (x1)*(y2) - (x2)*(y1);
}

/**
 * Scalar product: use coords_sp for Coords.
 */
double scalar_prod(
		double x1, double y1, double z1,
		double x2, double y2, double z2) {
	return ((x1 * x2) + (y1 * y2) + (z1 * z2));
}

mcstatic void norm_func(double *x, double *y, double *z) {
	double temp = (*x * *x) + (*y * *y) + (*z * *z);
	if (temp != 0) {
		temp = sqrt(temp);
		*x /= temp;
		*y /= temp;
		*z /= temp;
	}
}


/* SECTION: GPU algorithms ================================================== */


/*
*  Divide-and-conquer strategy for parallelizing this task: Sort absorbed
*  particles last.
*
*   particles:  the particle array, required to checking _absorbed
*   pbuffer:    same-size particle buffer array required for parallel sort
*   len:        sorting area-of-interest size (e.g. from previous calls)
*   buffer_len: total array size
*   flag_split: if set, multiply live particles into absorbed slots, up to buffer_len
*   multiplier: output arg, becomes the  SPLIT multiplier if flag_split is set
*/
#ifdef FUNNEL
long sort_absorb_last(_class_particle* particles, _class_particle* pbuffer, long len, long buffer_len, long flag_split, long* multiplier) {
  #define SAL_THREADS 1024 // num parallel sections
  if (len<SAL_THREADS) return sort_absorb_last_serial(particles, len);

  if (multiplier != NULL) *multiplier = -1; // set default out value for multiplier
  long newlen = 0;
  long los[SAL_THREADS]; // target array startidxs
  long lens[SAL_THREADS]; // target array sublens
  long l = floor(len/(SAL_THREADS-1)); // subproblem_len
  long ll = len - l*(SAL_THREADS-1); // last_subproblem_len

  // TODO: The l vs ll is too simplistic, since ll can become much larger
  // than l, resulting in idling. We should distribute lengths more evenly.

  // step 1: sort sub-arrays
  #pragma acc parallel loop present(particles, pbuffer)
  for (unsigned long tidx=0; tidx<SAL_THREADS; tidx++) {
    long lo = l*tidx;
    long loclen = l;
    if (tidx==(SAL_THREADS-1)) loclen = ll; // last sub-problem special case
    long i = lo;
    long j = lo + loclen - 1;

    // write into pbuffer at i and j
    #pragma acc loop seq
    while (i < j) {
      #pragma acc loop seq
      while (!particles[i]._absorbed && i<j) {
        pbuffer[i] = particles[i];
        i++;
      }
      #pragma acc loop seq
      while (particles[j]._absorbed && i<j) {
        pbuffer[j] = particles[j];
        j--;
      }
      if (i < j) {
        pbuffer[j] = particles[i];
        pbuffer[i] = particles[j];
        i++;
        j--;
      }
    }
    // transfer edge case
    if (i==j)
      pbuffer[i] = particles[i];

    lens[tidx] = i - lo;
    if (i==j && !particles[i]._absorbed) lens[tidx]++;
  }

  // determine lo's
  long accumlen = 0;
  #pragma acc loop seq
  for (long idx=0; idx<SAL_THREADS; idx++) {
    los[idx] = accumlen;
    accumlen = accumlen + lens[idx];
  }

  // step 2: write non-absorbed sub-arrays to psorted/output from the left
  #pragma acc parallel loop present(pbuffer)
  for (unsigned long tidx=0; tidx<SAL_THREADS; tidx++) {
    long j, k;
    #pragma acc loop seq
    for (long i=0; i<lens[tidx]; i++) {
      j = i + l*tidx;
      k = i + los[tidx];
      particles[k] = pbuffer[j];
    }
  }
  //for (int ii=0;ii<accumlen;ii++) printf("%ld ", (psorted[ii]->_absorbed));

  // return (no SPLIT)
  if (flag_split != 1)
    return accumlen;

  // SPLIT - repeat the non-absorbed block N-1 times, where len % accumlen = N + R
  int mult = buffer_len / accumlen; // TODO: possibly use a new arg, bufferlen, rather than len

  // not enough space for full-block split, return
  if (mult <= 1)
    return accumlen;

  // copy non-absorbed block
  #pragma acc parallel loop present(particles)
  for (long tidx = 0; tidx < accumlen; tidx++) { // tidx: thread index
    unsigned long randstate[7];
    _class_particle sourcebuffer;
    _class_particle targetbuffer;
    // assign reduced weight to all particles
    particles[tidx].p=particles[tidx].p/mult;
    #pragma acc loop seq
    for (long bidx = 1; bidx < mult; bidx++) { // bidx: block index
      // preserve absorbed particle (for randstate)
      sourcebuffer = particles[bidx*accumlen + tidx];
      // buffer full particle struct
      targetbuffer = particles[tidx];
      // reassign previous randstate
      targetbuffer.randstate[0] = sourcebuffer.randstate[0];
      targetbuffer.randstate[1] = sourcebuffer.randstate[1];
      targetbuffer.randstate[2] = sourcebuffer.randstate[2];
      targetbuffer.randstate[3] = sourcebuffer.randstate[3];
      targetbuffer.randstate[4] = sourcebuffer.randstate[4];
      targetbuffer.randstate[5] = sourcebuffer.randstate[5];
      targetbuffer.randstate[6] = sourcebuffer.randstate[6];
      // apply
      particles[bidx*accumlen + tidx] = targetbuffer;
    }
  }

  // set out split multiplier value
  *multiplier = mult;

  // return expanded array size
  return accumlen * mult;
}

#endif

/*
*  Fallback serial version of the one above.
*/
long sort_absorb_last_serial(_class_particle* particles, long len) {
  long i = 0;
  long j = len - 1;
  _class_particle pbuffer;

  // bubble
  while (i < j) {
    while (!particles[i]._absorbed && i<j) i++;
    while (particles[j]._absorbed && i<j) j--;
    if (i < j) {
      pbuffer = particles[j];
      particles[j] = particles[i];
      particles[i] = pbuffer;
      i++;
      j--;
    }
  }

  // return new length
  if (i==j && !particles[i]._absorbed)
    return i + 1;
  else
    return i;
}

/*******************************************************************************
* mccoordschange: applies rotation to (x y z) and (vx vy vz) and Spin (sx,sy,sz)
*******************************************************************************/
void mccoordschange(Coords a, Rotation t, _class_particle *particle)
{
  Coords b, c;

  b.x = particle->x;
  b.y = particle->y;
  b.z = particle->z;
  c = rot_apply(t, b);
  b = coords_add(c, a);
  particle->x = b.x;
  particle->y = b.y;
  particle->z = b.z;

#if MCCODE_PARTICLE_CODE == 2112
    if (particle->vz != 0.0 || particle->vx != 0.0 || particle->vy != 0.0)
      mccoordschange_polarisation(t, &(particle->vx), &(particle->vy), &(particle->vz));

    if (particle->sz != 0.0 || particle->sx != 0.0 || particle->sy != 0.0)
      mccoordschange_polarisation(t, &(particle->sx), &(particle->sy), &(particle->sz));
#elif MCCODE_PARTICLE_CODE == 22
    if (particle->kz != 0.0 || particle->kx != 0.0 || particle->ky != 0.0)
      mccoordschange_polarisation(t, &(particle->kx), &(particle->ky), &(particle->kz));

    if (particle->Ez != 0.0 || particle->Ex != 0.0 || particle->Ey != 0.0)
      mccoordschange_polarisation(t, &(particle->Ex), &(particle->Ey), &(particle->Ez));
#endif
}

/*******************************************************************************
* mccoordschange_polarisation: applies rotation to vector (sx sy sz)
*******************************************************************************/
void mccoordschange_polarisation(Rotation t, double *sx, double *sy, double *sz)
{
  Coords b, c;

  b.x = *sx;
  b.y = *sy;
  b.z = *sz;
  c = rot_apply(t, b);
  *sx = c.x;
  *sy = c.y;
  *sz = c.z;
}

/* SECTION: vector math  ==================================================== */

/* normal_vec_func: Compute normal vector to (x,y,z). */
void normal_vec(double *nx, double *ny, double *nz,
                double x, double y, double z)
{
  double ax = fabs(x);
  double ay = fabs(y);
  double az = fabs(z);
  double l;
  if(x == 0 && y == 0 && z == 0)
  {
    *nx = 0;
    *ny = 0;
    *nz = 0;
    return;
  }
  if(ax < ay)
  {
    if(ax < az)
    {                           /* Use X axis */
      l = sqrt(z*z + y*y);
      *nx = 0;
      *ny = z/l;
      *nz = -y/l;
      return;
    }
  }
  else
  {
    if(ay < az)
    {                           /* Use Y axis */
      l = sqrt(z*z + x*x);
      *nx = z/l;
      *ny = 0;
      *nz = -x/l;
      return;
    }
  }
  /* Use Z axis */
  l = sqrt(y*y + x*x);
  *nx = y/l;
  *ny = -x/l;
  *nz = 0;
} /* normal_vec */

/*******************************************************************************
 * solve_2nd_order: second order equation solve: A*t^2 + B*t + C = 0
 * solve_2nd_order(&t1, NULL, A,B,C)
 *   returns 0 if no solution was found, or set 't1' to the smallest positive
 *   solution.
 * solve_2nd_order(&t1, &t2, A,B,C)
 *   same as with &t2=NULL, but also returns the second solution.
 * EXAMPLE usage for intersection of a trajectory with a plane in gravitation
 * field (gx,gy,gz):
 * The neutron starts at point r=(x,y,z) with velocityv=(vx vy vz). The plane
 * has a normal vector n=(nx,ny,nz) and contains the point W=(wx,wy,wz).
 * The problem consists in solving the 2nd order equation:
 *      1/2.n.g.t^2 + n.v.t + n.(r-W) = 0
 * so that A = 0.5 n.g; B = n.v; C = n.(r-W);
 * Without acceleration, t=-n.(r-W)/n.v
 ******************************************************************************/
int solve_2nd_order_old(double *t1, double *t2,
                  double A,  double B,  double C)
{
  int ret=0;

  if (!t1) return 0;
  *t1 = 0;
  if (t2) *t2=0;

  if (fabs(A) < 1E-10) /* approximate to linear equation: A ~ 0 */
  {
    if (B) {  *t1 = -C/B; ret=1; if (t2) *t2=*t1; }
    /* else no intersection: A=B=0 ret=0 */
  }
  else
  {
    double D;
    D = B*B - 4*A*C;
    if (D >= 0) /* Delta > 0: two solutions */
    {
      double sD, dt1, dt2;
      sD = sqrt(D);
      dt1 = (-B + sD)/2/A;
      dt2 = (-B - sD)/2/A;
      /* we identify very small values with zero */
      if (fabs(dt1) < 1e-10) dt1=0.0;
      if (fabs(dt2) < 1e-10) dt2=0.0;

      /* now we choose the smallest positive solution */
      if      (dt1<=0.0 && dt2>0.0) ret=2; /* dt2 positive */
      else if (dt2<=0.0 && dt1>0.0) ret=1; /* dt1 positive */
      else if (dt1> 0.0 && dt2>0.0)
      {  if (dt1 < dt2) ret=1; else ret=2; } /* all positive: min(dt1,dt2) */
      /* else two solutions are negative. ret=-1 */
      if (ret==1) { *t1 = dt1;  if (t2) *t2=dt2; }
      else        { *t1 = dt2;  if (t2) *t2=dt1; }
      ret=2;  /* found 2 solutions and t1 is the positive one */
    } /* else Delta <0: no intersection. ret=0 */
  }
  return(ret);
} /* solve_2nd_order */

int solve_2nd_order(double *t0, double *t1, double A, double B, double C){
  int retval=0;
  double sign=copysign(1.0,B);
  double dt0,dt1;

  dt0=0;
  dt1=0;
  if(t1){ *t1=0;}

  /*protect against rounding errors by locally equating DBL_EPSILON with 0*/
  if (fabs(A)<DBL_EPSILON){
    A=0;
  }
  if (fabs(B)<DBL_EPSILON){
    B=0;
  }
  if (fabs(C)<DBL_EPSILON){
    C=0;
  }

  /*check if coefficient are sane*/
  if( A==0  && B==0){
    retval=0;
  }else{
    if(A==0){
      /*equation is linear*/
      dt0=-C/B;
      retval=1;
    }else if (C==0){
      /*one root is 0*/
      if(sign<0){
        dt0=0;dt1=-B/A;
      }else{
        dt0=-B/A;dt1=0;
      }
      retval=2;
    }else{
      /*a regular 2nd order eq. Also works out fine for B==0.*/
      double D;
      D=B*B-4*A*C;
      if (D>=0){
        dt0=(-B - sign*sqrt(B*B-4*A*C))/(2*A);
        dt1=C/(A*dt0);
        retval=2;
      }else{
        /*no real roots*/
        retval=0;
      }
    }
    /*sort the solutions*/
    if (retval==1){
      /*put both solutions in t0 and t1*/
      *t0=dt0;
      if(t1) *t1=dt1;
    }else{
      /*we have two solutions*/
      /*swap if both are positive and t1 smaller than t0 or t1 the only positive*/
      int swap=0;
      if(dt1>0 && ( dt1<dt0 || dt0<=0) ){
        swap=1;
      }
      if (swap){
        *t0=dt1;
        if(t1) *t1=dt0;
      }else{
        *t0=dt0;
        if(t1) *t1=dt0;
      }
    }

  }
  return retval;

} /*solve_2nd_order_improved*/


/*******************************************************************************
 * randvec_target_circle: Choose random direction towards target at (x,y,z)
 * with given radius.
 * If radius is zero, choose random direction in full 4PI, no target.
 ******************************************************************************/
void _randvec_target_circle(double *xo, double *yo, double *zo, double *solid_angle,
        double xi, double yi, double zi, double radius,
        _class_particle* _particle)
{
  double l2, phi, theta, nx, ny, nz, xt, yt, zt, xu, yu, zu;

  if(radius == 0.0)
  {
    /* No target, choose uniformly a direction in full 4PI solid angle. */
    theta = acos(1 - rand0max(2));
    phi = rand0max(2 * PI);
    if(solid_angle)
      *solid_angle = 4*PI;
    nx = 1;
    ny = 0;
    nz = 0;
    yi = sqrt(xi*xi+yi*yi+zi*zi);
    zi = 0;
    xi = 0;
  }
  else
  {
    double costheta0;
    l2 = xi*xi + yi*yi + zi*zi; /* sqr Distance to target. */
    costheta0 = sqrt(l2/(radius*radius+l2));
    if (radius < 0) costheta0 *= -1;
    if(solid_angle)
    {
      /* Compute solid angle of target as seen from origin. */
        *solid_angle = 2*PI*(1 - costheta0);
    }

    /* Now choose point uniformly on circle surface within angle theta0 */
    theta = acos (1 - rand0max(1 - costheta0)); /* radius on circle */
    phi = rand0max(2 * PI); /* rotation on circle at given radius */
    /* Now, to obtain the desired vector rotate (xi,yi,zi) angle theta around a
       perpendicular axis u=i x n and then angle phi around i. */
    if(xi == 0 && zi == 0)
    {
      nx = 1;
      ny = 0;
      nz = 0;
    }
    else
    {
      nx = -zi;
      nz = xi;
      ny = 0;
    }
  }

  /* [xyz]u = [xyz]i x n[xyz] (usually vertical) */
  vec_prod(xu,  yu,  zu, xi, yi, zi,        nx, ny, nz);
  /* [xyz]t = [xyz]i rotated theta around [xyz]u */
  rotate  (xt,  yt,  zt, xi, yi, zi, theta, xu, yu, zu);
  /* [xyz]o = [xyz]t rotated phi around n[xyz] */
  rotate (*xo, *yo, *zo, xt, yt, zt, phi, xi, yi, zi);
}
/* randvec_target_circle */

/*******************************************************************************
 * randvec_target_rect_angular: Choose random direction towards target at
 * (xi,yi,zi) with given ANGULAR dimension height x width. height=phi_x=[0,PI],
 * width=phi_y=[0,2*PI] (radians)
 * If height or width is zero, choose random direction in full 4PI, no target.
 *******************************************************************************/
void _randvec_target_rect_angular(double *xo, double *yo, double *zo, double *solid_angle,
        double xi, double yi, double zi, double width, double height, Rotation A,
        _class_particle* _particle)
{
  double theta, phi, nx, ny, nz, xt, yt, zt, xu, yu, zu;
  Coords tmp;
  Rotation Ainverse;

  rot_transpose(A, Ainverse);

  if(height == 0.0 || width == 0.0)
  {
    randvec_target_circle(xo, yo, zo, solid_angle, xi, yi, zi, 0);
    return;
  }
  else
  {
    if(solid_angle)
    {
      /* Compute solid angle of target as seen from origin. */
      *solid_angle = 2*fabs(width*sin(height/2));
    }

    /* Go to global coordinate system */

    tmp = coords_set(xi, yi, zi);
    tmp = rot_apply(Ainverse, tmp);
    coords_get(tmp, &xi, &yi, &zi);

    /* Now choose point uniformly on the unit sphere segment with angle theta/phi */
    phi   = width*randpm1()/2.0;
    theta = asin(randpm1()*sin(height/2.0));
    /* Now, to obtain the desired vector rotate (xi,yi,zi) angle theta around
       n, and then phi around u. */
    if(xi == 0 && zi == 0)
    {
      nx = 1;
      ny = 0;
      nz = 0;
    }
    else
    {
      nx = -zi;
      nz = xi;
      ny = 0;
    }
  }

  /* [xyz]u = [xyz]i x n[xyz] (usually vertical) */
  vec_prod(xu,  yu,  zu, xi, yi, zi,        nx, ny, nz);
  /* [xyz]t = [xyz]i rotated theta around [xyz]u */
  rotate  (xt,  yt,  zt, xi, yi, zi, theta, nx, ny, nz);
  /* [xyz]o = [xyz]t rotated phi around n[xyz] */
  rotate (*xo, *yo, *zo, xt, yt, zt, phi, xu,  yu,  zu);

  /* Go back to local coordinate system */
  tmp = coords_set(*xo, *yo, *zo);
  tmp = rot_apply(A, tmp);
  coords_get(tmp, &*xo, &*yo, &*zo);
}
/* randvec_target_rect_angular */

/*******************************************************************************
 * randvec_target_rect_real: Choose random direction towards target at (xi,yi,zi)
 * with given dimension height x width (in meters !).
 *
 * Local emission coordinate is taken into account and corrected for 'order' times.
 * (See remarks posted to mcstas-users by George Apostolopoulus <gapost@ipta.demokritos.gr>)
 *
 * If height or width is zero, choose random direction in full 4PI, no target.
 *
 * Traditionally, this routine had the name randvec_target_rect - this is now a
 * a define (see mcstas-r.h) pointing here. If you use the old rouine, you are NOT
 * taking the local emmission coordinate into account.
*******************************************************************************/
void _randvec_target_rect_real(double *xo, double *yo, double *zo, double *solid_angle,
        double xi, double yi, double zi,
        double width, double height, Rotation A,
        double lx, double ly, double lz, int order,
        _class_particle* _particle)
{
  double dx, dy, dist, dist_p, nx, ny, nz, mx, my, mz, n_norm, m_norm;
  double cos_theta;
  Coords tmp;
  Rotation Ainverse;

  rot_transpose(A, Ainverse);

  if(height == 0.0 || width == 0.0)
  {
    randvec_target_circle(xo, yo, zo, solid_angle,
               xi, yi, zi, 0);
    return;
  }
  else
  {
    /* Now choose point uniformly on rectangle within width x height */
    dx = width*randpm1()/2.0;
    dy = height*randpm1()/2.0;

    /* Determine distance to target plane*/
    dist = sqrt(xi*xi + yi*yi + zi*zi);
    /* Go to global coordinate system */

    tmp = coords_set(xi, yi, zi);
    tmp = rot_apply(Ainverse, tmp);
    coords_get(tmp, &xi, &yi, &zi);

    /* Determine vector normal to trajectory axis (z) and gravity [0 1 0] */
    vec_prod(nx, ny, nz, xi, yi, zi, 0, 1, 0);

    /* This now defines the x-axis, normalize: */
    n_norm=sqrt(nx*nx + ny*ny + nz*nz);
    nx = nx/n_norm;
    ny = ny/n_norm;
    nz = nz/n_norm;

    /* Now, determine our y-axis (vertical in many cases...) */
    vec_prod(mx, my, mz, xi, yi, zi, nx, ny, nz);
    m_norm=sqrt(mx*mx + my*my + mz*mz);
    mx = mx/m_norm;
    my = my/m_norm;
    mz = mz/m_norm;

    /* Our output, random vector can now be defined by linear combination: */

    *xo = xi + dx * nx + dy * mx;
    *yo = yi + dx * ny + dy * my;
    *zo = zi + dx * nz + dy * mz;

    /* Go back to local coordinate system */
    tmp = coords_set(*xo, *yo, *zo);
    tmp = rot_apply(A, tmp);
    coords_get(tmp, &*xo, &*yo, &*zo);

    /* Go back to local coordinate system */
    tmp = coords_set(xi, yi, zi);
    tmp = rot_apply(A, tmp);
    coords_get(tmp, &xi, &yi, &zi);

    if (solid_angle) {
      /* Calculate vector from local point to remote random point */
      lx = *xo - lx;
      ly = *yo - ly;
      lz = *zo - lz;
      dist_p = sqrt(lx*lx + ly*ly + lz*lz);

      /* Adjust the 'solid angle' */
      /* 1/r^2 to the chosen point times cos(\theta) between the normal */
      /* vector of the target rectangle and direction vector of the chosen point. */
      cos_theta = (xi * lx + yi * ly + zi * lz) / (dist * dist_p);
      *solid_angle = width * height / (dist_p * dist_p);
      int counter;
      for (counter = 0; counter < order; counter++) {
        *solid_angle = *solid_angle * cos_theta;
      }
    }
  }
}
/* randvec_target_rect_real */


/* SECTION: random numbers ==================================================

  How to add a new RNG:

  - Use an rng with a manegable state vector, e.g. of lengt 4 or 7. The state
  will sit on the particle struct as a "randstate_t state[RANDSTATE_LEN]"
  - If the rng has a long state (as MT), set an empty "srandom" and initialize
  it explicitly using the appropriate define (RNG_ALG)
  - Add a seed and a random function (the transforms will be reused)
  - Write the proper defines in mccode-r.h, e.g. randstate_t and RANDSTATE_LEN,
  srandom and random.
  - Compile using -DRNG_ALG=<selector int value>

============================================================================= */


/* "Mersenne Twister", by Makoto Matsumoto and Takuji Nishimura. */
/* See http://www.math.keio.ac.jp/~matumoto/emt.html for original source. */
/*
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using mt_srandom(seed)
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote
        products derived from this software without specific prior written
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.keio.ac.jp/matumoto/emt.html
   email: matumoto@math.keio.ac.jp
*/
#include <stdio.h>
/* Period parameters */
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

unsigned long mt[N]; /* the array for the state vector  */
int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

// required for common rng alg interface (see RNG_ALG usage in mccode-r.h)
void mt_srandom_empty() {}

// initializes mt[N] with a seed
void mt_srandom(unsigned long s)
{
    mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt[mti] =
            (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti);
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
}
/* Initialize by an array with array-length.
   Init_key is the array for initializing keys.
   key_length is its length. */
void init_by_array(unsigned long init_key[], unsigned long key_length)
{
    int i, j, k;
    mt_srandom(19650218UL);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
          + init_key[j] + j; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
          - i; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }

    mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */
}
/* generates a random number on [0,0xffffffff]-interval */
unsigned long mt_random(void)
{
    unsigned long y;
    unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if mt_srandom() has not been called, */
            mt_srandom(5489UL); /* a default initial seed is used */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        mti = 0;
    }

    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}
#undef N
#undef M
#undef MATRIX_A
#undef UPPER_MASK
#undef LOWER_MASK
/* End of "Mersenne Twister". */


/*
KISS

 From: http://www.helsbreth.org/random/rng_kiss.html
 Scott Nelson 1999

 Based on Marsaglia's KISS or (KISS+SWB) <http://www.cs.yorku.ca/~oz/marsaglia-
rng.html>

 KISS - Keep it Simple Stupid PRNG

 the idea is to use simple, fast, individually promising
 generators to get a composite that will be fast, easy to code
 have a very long period and pass all the tests put to it.
 The three components of KISS are
        x(n)=a*x(n-1)+1 mod 2^32
        y(n)=y(n-1)(I+L^13)(I+R^17)(I+L^5),
        z(n)=2*z(n-1)+z(n-2) +carry mod 2^32
 The y's are a shift register sequence on 32bit binary vectors
 period 2^32-1;
 The z's are a simple multiply-with-carry sequence with period
 2^63+2^32-1.  The period of KISS is thus
      2^32*(2^32-1)*(2^63+2^32-1) > 2^127
*/

/* the KISS state is stored as a vector of 7 unsigned long        */
/*   0  1  2  3  4      5  6   */
/* [ x, y, z, w, carry, k, m ] */

unsigned long *kiss_srandom(unsigned long state[7], unsigned long seed) {
  if (seed == 0) seed = 1;
  state[0] = seed | 1; // x
  state[1] = seed | 2; // y
  state[2] = seed | 4; // z
  state[3] = seed | 8; // w
  state[4] = 0;        // carry
  return 0;
}

unsigned long kiss_random(unsigned long state[7]) {
    state[0] = state[0] * 69069 + 1;
    state[1] ^= state[1] << 13;
    state[1] ^= state[1] >> 17;
    state[1] ^= state[1] << 5;
    state[5] = (state[2] >> 2) + (state[3] >> 3) + (state[4] >> 2);
    state[6] = state[3] + state[3] + state[2] + state[4];
    state[2] = state[3];
    state[3] = state[6];
    state[4] = state[5] >> 30;
    return state[0] + state[1] + state[3];
}
/* end of "KISS" rng */


/* FAST KISS in another implementation (Hundt) */

//////////////////////////////////////////////////////////////////////////////
// fast keep it simple stupid generator
//////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Thomas Mueller hash for initialization of rngs
// http://stackoverflow.com/questions/664014/
//        what-integer-hash-function-are-good-that-accepts-an-integer-hash-key
//////////////////////////////////////////////////////////////////////////////
randstate_t _hash(randstate_t x) {
  x = ((x >> 16) ^ x) * 0x45d9f3b;
  x = ((x >> 16) ^ x) * 0x45d9f3b;
  x = ((x >> 16) ^ x);
  return x;
}

// fast kiss state is a pointer to 5 long [x,y,z,w,c]
randstate_t fast_kiss(randstate_t * state) {

    state[1] ^= state[1] << 5;
    state[1] ^= state[1] >> 7;
    state[1] ^= state[1] << 22;

    randstate_t t = state[2]+state[3]+state[4];
    state[2]  = state[3];
    state[4]  = t < 0;
    state[3]  = t & 2147483647;
    state[0] += 1411392427;

    // combine
    return state[0]+state[1]+state[3];
}

randstate_t * fast_kiss_seed(randstate_t * state, randstate_t seed) {

  randstate_t num_iters=8;

  state[3] = !seed ? 4294967295 : seed;
  for (randstate_t iter = 0; iter < num_iters; iter++) {
      state[0] = _hash(state[3]);
      state[1] = _hash(state[0]);
      state[2] = _hash(state[1]);
      state[3] = _hash(state[2]);
  }
  return state;
}


// SECTION: random number transforms ==========================================



// generate a random number from normal law
double _randnorm(randstate_t* state)
{
  static double v1, v2, s; /* removing static breaks comparison with McStas <= 2.5 */
  static int phase = 0;
  double X, u1, u2;

  if(phase == 0)
  {
    do
    {
      u1 = _rand01(state);
      u2 = _rand01(state);
      v1 = 2*u1 - 1;
      v2 = 2*u2 - 1;
      s = v1*v1 + v2*v2;
    } while(s >= 1 || s == 0);

    X = v1*sqrt(-2*log(s)/s);
  }
  else
  {
    X = v2*sqrt(-2*log(s)/s);
  }

  phase = 1 - phase;
  return X;
}
// another one
double _randnorm2(randstate_t* state) {
  double x, y, r;
  do {
      x = 2.0 * _rand01(state) - 1.0;
      y = 2.0 * _rand01(state) - 1.0;
      r = x*x + y*y;
  } while (r == 0.0 || r >= 1.0);
  return x * sqrt((-2.0 * log(r)) / r);
}

double _gaussian_double(randstate_t * state) {

    double x, y, r;

    do {
        x = 2.0 * _uniform_double(state) - 1.0;
        y = 2.0 * _uniform_double(state) - 1.0;
        r = x*x + y*y;
    } while (r == 0.0 || r >= 1.0);

    return x * sqrt((-2.0 * log(r)) / r);
}
// Generate a random number from -1 to 1 with triangle distribution
double _randtriangle(randstate_t* state) {
	double randnum = _rand01(state);
	if (randnum>0.5) return(1-sqrt(2*(randnum-0.5)));
	else return(sqrt(2*randnum)-1);
}
// Random number between 0.0 and 1.0
double _uniform_double(randstate_t * state) {

    randstate_t a = fast_kiss(state) >> 6;
    randstate_t b = fast_kiss(state) >> 5;
    double x = (a * 134217728.0 + b) / 9007199254740992.0;

    return x;
}
double _rand01(randstate_t* state) {
	double randnum;
	randnum = (double) _random();
  // TODO: can we mult instead of div?
	randnum /= (double) MC_RAND_MAX + 1;
	return randnum;
}
// Return a random number between 1 and -1
double _randpm1(randstate_t* state) {
	double randnum;
	randnum = (double) _random();
	randnum /= ((double) MC_RAND_MAX + 1) / 2;
	randnum -= 1;
	return randnum;
}
// Return a random number between 0 and max.
double _rand0max(double max, randstate_t* state) {
	double randnum;
	randnum = (double) _random();
	randnum /= ((double) MC_RAND_MAX + 1) / max;
	return randnum;
}
// Return a random number between min and max.
double _randminmax(double min, double max, randstate_t* state) {
	return _rand0max(max - min, state) + max;
}


/* SECTION: main and signal handlers ======================================== */

/*******************************************************************************
* mchelp: displays instrument executable help with possible options
*******************************************************************************/
static void
mchelp(char *pgmname)
{
  int i;

  fprintf(stderr, "%s (%s) instrument simulation, generated with " MCCODE_STRING " (" MCCODE_DATE ")\n", instrument_name, instrument_source);
  fprintf(stderr, "Usage: %s [options] [parm=value ...]\n", pgmname);
  fprintf(stderr,
"Options are:\n"
"  -s SEED   --seed=SEED      Set random seed (must be != 0)\n"
"  -n COUNT  --ncount=COUNT   Set number of particles to simulate.\n"
"  -d DIR    --dir=DIR        Put all data files in directory DIR.\n"
"  -t        --trace          Enable trace of " MCCODE_PARTICLE "s through instrument.\n"
"  -g        --gravitation    Enable gravitation for all trajectories.\n"
"  --no-output-files          Do not write any data files.\n"
"  -h        --help           Show this help message.\n"
"  -i        --info           Detailed instrument information.\n"
"  --source                   Show the instrument code which was compiled.\n"
#ifdef OPENACC
"\n"
"  --vecsize                  OpenACC vector-size (default: 128)\n"
"  --numgangs                 Number of OpenACC gangs (default: 7813)\n"
"  --gpu_innerloop            Maximum rays to process pr. OpenACC \n"
"                             kernel run (default: 2147483647)\n"
"\n"
#endif
"  --format=FORMAT            Output data files using FORMAT="
   FLAVOR_UPPER
#ifdef USE_NEXUS
   " NEXUS"
#endif
"\n\n"
);
#ifdef USE_MPI
  fprintf(stderr,
  "This instrument has been compiled with MPI support.\n  Use 'mpirun %s [options] [parm=value ...]'.\n", pgmname);
#endif
#ifdef OPENACC
  fprintf(stderr,
  "This instrument has been compiled with NVIDIA GPU support through OpenACC.\n  Running on systems without such devices will lead to segfaults.\nFurter, fprintf, sprintf and printf have been removed from any component TRACE.\n");
#endif

  if(numipar > 0)
  {
    fprintf(stderr, "Instrument parameters are:\n");
    for(i = 0; i < numipar; i++)
      if (mcinputtable[i].val && strlen(mcinputtable[i].val))
        fprintf(stderr, "  %-16s(%s) [default='%s']\n", mcinputtable[i].name,
        (*mcinputtypes[mcinputtable[i].type].parminfo)(mcinputtable[i].name),
        mcinputtable[i].val);
      else
        fprintf(stderr, "  %-16s(%s)\n", mcinputtable[i].name,
        (*mcinputtypes[mcinputtable[i].type].parminfo)(mcinputtable[i].name));
  }

#ifndef NOSIGNALS
  fprintf(stderr, "Known signals are: "
#ifdef SIGUSR1
  "USR1 (status) "
#endif
#ifdef SIGUSR2
  "USR2 (save) "
#endif
#ifdef SIGBREAK
  "BREAK (save) "
#endif
#ifdef SIGTERM
  "TERM (save and exit)"
#endif
  "\n");
#endif /* !NOSIGNALS */
} /* mchelp */


/* mcshowhelp: show help and exit with 0 */
static void
mcshowhelp(char *pgmname)
{
  mchelp(pgmname);
  exit(0);
}

/* mcusage: display usage when error in input arguments and exit with 1 */
static void
mcusage(char *pgmname)
{
  fprintf(stderr, "Error: incorrect command line arguments\n");
  mchelp(pgmname);
  exit(1);
}

/* mcenabletrace: enable trace/mcdisplay or error if requires recompile */
static void
mcenabletrace(void)
{
 if(traceenabled)
  mcdotrace = 1;
 else
 {
   fprintf(stderr,
           "Error: trace not enabled (mcenabletrace)\n"
           "Please re-run the " MCCODE_NAME " compiler "
                   "with the --trace option, or rerun the\n"
           "C compiler with the MC_TRACE_ENABLED macro defined.\n");
   exit(1);
 }
}

/*******************************************************************************
* mcreadparams: request parameters from the prompt (or use default)
*******************************************************************************/
void
mcreadparams(void)
{
  int i,j,status;
  static char buf[CHAR_BUF_LENGTH];
  char *p;
  int len;

  MPI_MASTER(printf("Instrument parameters for %s (%s)\n",
                    instrument_name, instrument_source));

  for(i = 0; mcinputtable[i].name != 0; i++)
  {
    do
    {
      MPI_MASTER(
                 if (mcinputtable[i].val && strlen(mcinputtable[i].val))
                   printf("Set value of instrument parameter %s (%s) [default='%s']:\n",
                          mcinputtable[i].name,
                          (*mcinputtypes[mcinputtable[i].type].parminfo)
                          (mcinputtable[i].name), mcinputtable[i].val);
                 else
                   printf("Set value of instrument parameter %s (%s):\n",
                          mcinputtable[i].name,
                          (*mcinputtypes[mcinputtable[i].type].parminfo)
                          (mcinputtable[i].name));
                 fflush(stdout);
                 );
#ifdef USE_MPI
      if(mpi_node_rank == mpi_node_root)
        {
          p = fgets(buf, CHAR_BUF_LENGTH, stdin);
          if(p == NULL)
            {
              fprintf(stderr, "Error: empty input for paramater %s (mcreadparams)\n", mcinputtable[i].name);
              exit(1);
            }
        }
      else
        p = buf;
      MPI_Bcast(buf, CHAR_BUF_LENGTH, MPI_CHAR, mpi_node_root, MPI_COMM_WORLD);
#else /* !USE_MPI */
      p = fgets(buf, CHAR_BUF_LENGTH, stdin);
      if(p == NULL)
        {
          fprintf(stderr, "Error: empty input for paramater %s (mcreadparams)\n", mcinputtable[i].name);
          exit(1);
        }
#endif /* USE_MPI */
      len = strlen(buf);
      if (!len || (len == 1 && (buf[0] == '\n' || buf[0] == '\r')))
      {
        if (mcinputtable[i].val && strlen(mcinputtable[i].val)) {
          strncpy(buf, mcinputtable[i].val, CHAR_BUF_LENGTH);  /* use default value */
          len = strlen(buf);
        }
      }
      for(j = 0; j < 2; j++)
      {
        if(len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r'))
        {
          len--;
          buf[len] = '\0';
        }
      }

      status = (*mcinputtypes[mcinputtable[i].type].getparm)
                   (buf, mcinputtable[i].par);
      if(!status)
      {
        (*mcinputtypes[mcinputtable[i].type].error)(mcinputtable[i].name, buf);
        if (!mcinputtable[i].val || strlen(mcinputtable[i].val)) {
          fprintf(stderr, "       Change %s default value in instrument definition.\n", mcinputtable[i].name);
          exit(1);
        }
      }
    } while(!status);
  }
} /* mcreadparams */

/*******************************************************************************
* mcparseoptions: parse command line arguments (options, parameters)
*******************************************************************************/
void
mcparseoptions(int argc, char *argv[])
{
  int i, j;
  char *p;
  int paramset = 0, *paramsetarray;
  char *usedir=NULL;

  /* Add one to numipar to avoid allocating zero size memory block. */
  paramsetarray = (int*)malloc((numipar + 1)*sizeof(*paramsetarray));
  if(paramsetarray == NULL)
  {
    fprintf(stderr, "Error: insufficient memory (mcparseoptions)\n");
    exit(1);
  }
  for(j = 0; j < numipar; j++)
    {
      paramsetarray[j] = 0;
      if (mcinputtable[j].val != NULL && strlen(mcinputtable[j].val))
      {
        int  status;
        char buf[CHAR_BUF_LENGTH];
        strncpy(buf, mcinputtable[j].val, CHAR_BUF_LENGTH);
        status = (*mcinputtypes[mcinputtable[j].type].getparm)
                   (buf, mcinputtable[j].par);
        if(!status) fprintf(stderr, "Invalid '%s' default value %s in instrument definition (mcparseoptions)\n", mcinputtable[j].name, buf);
        else paramsetarray[j] = 1;
      } else {
        (*mcinputtypes[mcinputtable[j].type].getparm)
          (NULL, mcinputtable[j].par);
        paramsetarray[j] = 0;
      }
    }
  for(i = 1; i < argc; i++)
  {
    if(!strcmp("-s", argv[i]) && (i + 1) < argc)
      mcsetseed(argv[++i]);
    else if(!strncmp("-s", argv[i], 2))
      mcsetseed(&argv[i][2]);
    else if(!strcmp("--seed", argv[i]) && (i + 1) < argc)
      mcsetseed(argv[++i]);
    else if(!strncmp("--seed=", argv[i], 7))
      mcsetseed(&argv[i][7]);
    else if(!strcmp("-n", argv[i]) && (i + 1) < argc)
      mcsetn_arg(argv[++i]);
    else if(!strncmp("-n", argv[i], 2))
      mcsetn_arg(&argv[i][2]);
    else if(!strcmp("--ncount", argv[i]) && (i + 1) < argc)
      mcsetn_arg(argv[++i]);
    else if(!strncmp("--ncount=", argv[i], 9))
      mcsetn_arg(&argv[i][9]);
    else if(!strcmp("-d", argv[i]) && (i + 1) < argc)
      usedir=argv[++i];  /* will create directory after parsing all arguments (end of this function) */
    else if(!strncmp("-d", argv[i], 2))
      usedir=&argv[i][2];
    else if(!strcmp("--dir", argv[i]) && (i + 1) < argc)
      usedir=argv[++i];
    else if(!strncmp("--dir=", argv[i], 6))
      usedir=&argv[i][6];
    else if(!strcmp("-h", argv[i]))
      mcshowhelp(argv[0]);
    else if(!strcmp("--help", argv[i]) || !strcmp("--version", argv[i]))
      mcshowhelp(argv[0]);
    else if(!strcmp("-i", argv[i])) {
      mcformat=FLAVOR_UPPER;
      mcinfo();
    }
    else if(!strcmp("--info", argv[i]))
      mcinfo();
    else if(!strcmp("-t", argv[i]))
      mcenabletrace();
    else if(!strcmp("--trace", argv[i]) || !strcmp("--verbose", argv[i]))
      mcenabletrace();
    else if(!strcmp("--gravitation", argv[i]))
      mcgravitation = 1;
    else if(!strcmp("-g", argv[i]))
      mcgravitation = 1;
    else if(!strncmp("--format=", argv[i], 9)) {
      mcformat=&argv[i][9];
    }
    else if(!strcmp("--format", argv[i]) && (i + 1) < argc) {
      mcformat=argv[++i];
    }
    else if(!strncmp("--vecsize=", argv[i], 10)) {
      vecsize=atoi(&argv[i][10]);
    }    
    else if(!strcmp("--vecsize", argv[i]) && (i + 1) < argc) {
      vecsize=atoi(argv[++i]);
    }
    else if(!strncmp("--numgangs=", argv[i], 11)) {
      numgangs=atoi(&argv[i][11]);
    }
    else if(!strcmp("--numgangs", argv[i]) && (i + 1) < argc) {
      numgangs=atoi(argv[++i]);
    }
    else if(!strncmp("--gpu_innerloop=", argv[i], 16)) {
      gpu_innerloop=(long)strtod(&argv[i][16], NULL);
    }
    else if(!strcmp("--gpu_innerloop", argv[i]) && (i + 1) < argc) {
      gpu_innerloop=(long)strtod(argv[++i], NULL);
    }

    else if(!strcmp("--no-output-files", argv[i]))
      mcdisable_output_files = 1;
    else if(!strcmp("--source", argv[i])) {
      printf("/* Source code %s from %s: */\n"
        "/******************************************************************************/\n"
        "%s\n"
        "/******************************************************************************/\n"
        "/* End of source code %s from %s */\n",
        instrument_name, instrument_source, instrument_code,
        instrument_name, instrument_source);
      exit(1);
    }
    else if(argv[i][0] != '-' && (p = strchr(argv[i], '=')) != NULL)
    {
      *p++ = '\0';

      for(j = 0; j < numipar; j++)
        if(!strcmp(mcinputtable[j].name, argv[i]))
        {
          int status;
          status = (*mcinputtypes[mcinputtable[j].type].getparm)(p,
                        mcinputtable[j].par);
          if(!status || !strlen(p))
          {
            (*mcinputtypes[mcinputtable[j].type].error)
              (mcinputtable[j].name, p);
            exit(1);
          }
          paramsetarray[j] = 1;
          paramset = 1;
          break;
        }
      if(j == numipar)
      {                                /* Unrecognized parameter name */
        fprintf(stderr, "Error: unrecognized parameter %s (mcparseoptions)\n", argv[i]);
        exit(1);
      }
    }
    else if(argv[i][0] == '-') {
      fprintf(stderr, "Error: unrecognized option argument %s (mcparseoptions). Ignored.\n", argv[i++]);
    }
    else {
      fprintf(stderr, "Error: unrecognized argument %s (mcparseoptions). Aborting.\n", argv[i]);
      mcusage(argv[0]);
    }
  }
  if(!paramset)
    mcreadparams();                /* Prompt for parameters if not specified. */
  else
  {
    for(j = 0; j < numipar; j++)
      if(!paramsetarray[j])
      {
        fprintf(stderr, "Error: Instrument parameter %s left unset (mcparseoptions)\n",
                mcinputtable[j].name);
        exit(1);
      }
  }
  free(paramsetarray);
#ifdef USE_MPI
  if (mcdotrace) mpi_node_count=1; /* disable threading when in trace mode */
#endif
  if (usedir && strlen(usedir) && !mcdisable_output_files) mcuse_dir(usedir);
} /* mcparseoptions */

#ifndef NOSIGNALS
/*******************************************************************************
* sighandler: signal handler that makes simulation stop, and save results
*******************************************************************************/
void sighandler(int sig)
{
  /* MOD: E. Farhi, Sep 20th 2001: give more info */
  time_t t1, t0;
#define SIG_SAVE 0
#define SIG_TERM 1
#define SIG_STAT 2
#define SIG_ABRT 3

  printf("\n# " MCCODE_STRING ": [pid %i] Signal %i detected", getpid(), sig);
#ifdef USE_MPI
  printf(" [proc %i]", mpi_node_rank);
#endif
#if defined(SIGUSR1) && defined(SIGUSR2) && defined(SIGKILL)
  if (!strcmp(mcsig_message, "sighandler") && (sig != SIGUSR1) && (sig != SIGUSR2))
  {
    printf("\n# Fatal : unrecoverable loop ! Suicide (naughty boy).\n");
    kill(0, SIGKILL); /* kill myself if error occurs within sighandler: loops */
  }
#endif
  switch (sig) {
#ifdef SIGINT
    case SIGINT : printf(" SIGINT (interrupt from terminal, Ctrl-C)"); sig = SIG_TERM; break;
#endif
#ifdef SIGILL
    case SIGILL  : printf(" SIGILL (Illegal instruction)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGFPE
    case SIGFPE  : printf(" SIGFPE (Math Error)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGSEGV
    case SIGSEGV : printf(" SIGSEGV (Mem Error)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGTERM
    case SIGTERM : printf(" SIGTERM (Termination)"); sig = SIG_TERM; break;
#endif
#ifdef SIGABRT
    case SIGABRT : printf(" SIGABRT (Abort)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGQUIT
    case SIGQUIT : printf(" SIGQUIT (Quit from terminal)"); sig = SIG_TERM; break;
#endif
#ifdef SIGTRAP
    case SIGTRAP : printf(" SIGTRAP (Trace trap)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGPIPE
    case SIGPIPE : printf(" SIGPIPE (Broken pipe)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGUSR1
    case SIGUSR1 : printf(" SIGUSR1 (Display info)"); sig = SIG_STAT; break;
#endif
#ifdef SIGUSR2
    case SIGUSR2 : printf(" SIGUSR2 (Save simulation)"); sig = SIG_SAVE; break;
#endif
#ifdef SIGHUP
    case SIGHUP  : printf(" SIGHUP (Hangup/update)"); sig = SIG_SAVE; break;
#endif
#ifdef SIGBUS
    case SIGBUS  : printf(" SIGBUS (Bus error)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGURG
    case SIGURG  : printf(" SIGURG (Urgent socket condition)"); sig = SIG_ABRT; break;
#endif
#ifdef SIGBREAK
    case SIGBREAK: printf(" SIGBREAK (Break signal, Ctrl-Break)"); sig = SIG_SAVE; break;
#endif
    default : printf(" (look at signal list for signification)"); sig = SIG_ABRT; break;
  }
  printf("\n");
  printf("# Simulation: %s (%s) \n", instrument_name, instrument_source);
  printf("# Breakpoint: %s ", mcsig_message);
  if (strstr(mcsig_message, "Save") && (sig == SIG_SAVE))
    sig = SIG_STAT;
  SIG_MESSAGE("sighandler");
  if (mcget_ncount() == 0)
    printf("(0 %%)\n" );
  else
  {
    printf("%.2f %% (%10.1f/%10.1f)\n", 100.0*mcget_run_num()/mcget_ncount(), 1.0*mcget_run_num(), 1.0*mcget_ncount());
  }
  t0 = (time_t)mcstartdate;
  t1 = time(NULL);
  printf("# Date:      %s", ctime(&t1));
  printf("# Started:   %s", ctime(&t0));

  if (sig == SIG_STAT)
  {
    printf("# " MCCODE_STRING ": Resuming simulation (continue)\n");
    fflush(stdout);
    return;
  }
  else
  if (sig == SIG_SAVE)
  {
    printf("# " MCCODE_STRING ": Saving data and resume simulation (continue)\n");
    save(NULL);
    fflush(stdout);
    return;
  }
  else
  if (sig == SIG_TERM)
  {
    printf("# " MCCODE_STRING ": Finishing simulation (save results and exit)\n");
    finally();
    exit(0);
  }
  else
  {
    fflush(stdout);
    perror("# Last I/O Error");
    printf("# " MCCODE_STRING ": Simulation stop (abort).\n");
// This portion of the signal handling only works on UNIX
#if defined(__unix__) || defined(__APPLE__)
    signal(sig, SIG_DFL); /* force to use default sighandler now */
    kill(getpid(), sig);  /* and trigger it with the current signal */
#endif
    exit(-1);
  }
#undef SIG_SAVE
#undef SIG_TERM
#undef SIG_STAT
#undef SIG_ABRT

} /* sighandler */
#endif /* !NOSIGNALS */

#ifdef NEUTRONICS
/*Main neutronics function steers the McStas calls, initializes parameters etc */
/* Only called in case NEUTRONICS = TRUE */
void neutronics_main_(float *inx, float *iny, float *inz, float *invx, float *invy, float *invz, float *intime, float *insx, float *insy, float *insz, float *inw, float *outx, float *outy, float *outz, float *outvx, float *outvy, float *outvz, float *outtime, float *outsx, float *outsy, float *outsz, float *outwgt)
{

  extern double mcnx, mcny, mcnz, mcnvx, mcnvy, mcnvz;
  extern double mcnt, mcnsx, mcnsy, mcnsz, mcnp;

  /* External code governs iteration - McStas is iterated once per call to neutronics_main. I.e. below counter must be initiancated for each call to neutronics_main*/
  mcrun_num=0;

  time_t t;
  t = (time_t)mcstartdate;
  mcstartdate = t;  /* set start date before parsing options and creating sim file */
  init();

  /* *** parse options *** */
  SIG_MESSAGE("[" __FILE__ "] main START");
  mcformat=getenv(FLAVOR_UPPER "_FORMAT") ?
           getenv(FLAVOR_UPPER "_FORMAT") : FLAVOR_UPPER;

  /* Set neutron state based on input from neutronics code */
  mcsetstate(*inx,*iny,*inz,*invx,*invy,*invz,*intime,*insx,*insy,*insz,*inw);

  /* main neutron event loop - runs only one iteration */

  //mcstas_raytrace(&mcncount); /* prior to McStas 1.12 */

  mcallowbackprop = 1; //avoid absorbtion from negative dt
  int argc=1;
  char *argv[0];
  int dummy = mccode_main(argc, argv);

  *outx =  mcnx;
  *outy =  mcny;
  *outz =  mcnz;
  *outvx =  mcnvx;
  *outvy =  mcnvy;
  *outvz =  mcnvz;
  *outtime =  mcnt;
  *outsx =  mcnsx;
  *outsy =  mcnsy;
  *outsz =  mcnsz;
  *outwgt =  mcnp;

  return;
} /* neutronics_main */

#endif /*NEUTRONICS*/

#endif /* !MCCODE_H */
/* End of file "mccode-r.c". */
/* End of file "mccode-r.c". */

/* embedding file "mcstas-r.c" */

/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2009, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/mcstas-r.c
*
* %Identification
* Written by: KN
* Date:    Aug 29, 1997
* Release: McStas X.Y
* Version: $Revision$
*
* Runtime system for McStas.
* Embedded within instrument in runtime mode.
*
* Usage: Automatically embbeded in the c code whenever required.
*
* $Id$
*
*******************************************************************************/

#ifndef MCSTAS_R_H
#include "mcstas-r.h"
#endif
#ifdef DANSE
#include "mcstas-globals.h"
#endif

/*******************************************************************************
* The I/O format definitions and functions
*******************************************************************************/

/*the magnet stack*/
#ifdef MC_POL_COMPAT
void (*mcMagnetPrecession) (double, double, double, double, double, double,
    double, double*, double*, double*, double, Coords, Rotation)=NULL;
Coords   mcMagnetPos;
Rotation mcMagnetRot;
double*  mcMagnetData                = NULL;
/* mcMagneticField(x, y, z, t, Bx, By, Bz) */
int (*mcMagneticField) (double, double, double, double,
    double*, double*, double*, void *) = NULL;
#endif

#ifndef MCSTAS_H

/*******************************************************************************
* mcsetstate: transfer parameters into global McStas variables
*******************************************************************************/
_class_particle mcsetstate(double x, double y, double z, double vx, double vy, double vz,
			   double t, double sx, double sy, double sz, double p, int mcgravitation, void *mcMagnet, int mcallowbackprop)
{
  _class_particle mcneutron;

  mcneutron.x  = x;
  mcneutron.y  = y;
  mcneutron.z  = z;
  mcneutron.vx = vx;
  mcneutron.vy = vy;
  mcneutron.vz = vz;
  mcneutron.t  = t;
  mcneutron.sx = sx;
  mcneutron.sy = sy;
  mcneutron.sz = sz;
  mcneutron.p  = p;
  mcneutron.mcgravitation = mcgravitation;
  mcneutron.mcMagnet = mcMagnet;
  mcneutron.allow_backprop = mcallowbackprop;
  mcneutron._uid       = 0;
  mcneutron._index     = 1;
  mcneutron._absorbed  = 0;
  mcneutron._restore   = 0;
  mcneutron._scattered = 0;

  return(mcneutron);
} /* mcsetstate */

/*******************************************************************************
* mcgetstate: get neutron parameters from particle structure
*******************************************************************************/
_class_particle mcgetstate(_class_particle mcneutron, double *x, double *y, double *z,
               double *vx, double *vy, double *vz, double *t,
               double *sx, double *sy, double *sz, double *p)
{
  *x  =  mcneutron.x;
  *y  =  mcneutron.y;
  *z  =  mcneutron.z;
  *vx =  mcneutron.vx;
  *vy =  mcneutron.vy;
  *vz =  mcneutron.vz;
  *t  =  mcneutron.t;
  *sx =  mcneutron.sx;
  *sy =  mcneutron.sy;
  *sz =  mcneutron.sz;
  *p  =  mcneutron.p;

  return(mcneutron);
} /* mcgetstate */


/*******************************************************************************
* mcgenstate: set default neutron parameters
*******************************************************************************/
// Moved to generated code
/* #pragma acc routine seq */
/* _class_particle mcgenstate(void) */
/* { */
/*   return(mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, mcgravitation, mcMagnet, mcallowbackprop)); */
/* } */

/*******************************************************************************
* mccoordschanges: old style rotation routine rot -> (x y z) ,(vx vy vz),(sx,sy,sz)
*******************************************************************************/
void
mccoordschanges(Coords a, Rotation t, double *x, double *y, double *z,
               double *vx, double *vy, double *vz, double *sx, double *sy, double *sz)
{
  Coords b, c;

  b.x = *x;
  b.y = *y;
  b.z = *z;
  c = rot_apply(t, b);
  b = coords_add(c, a);
  *x = b.x;
  *y = b.y;
  *z = b.z;

  if ( (vz && vy  && vx) && (*vz != 0.0 || *vx != 0.0 || *vy != 0.0) )
    mccoordschange_polarisation(t, vx, vy, vz);

  if ( (sz && sy  && sx) && (*sz != 0.0 || *sx != 0.0 || *sy != 0.0) )
    mccoordschange_polarisation(t, sx, sy, sz);

}

/* intersection routines ==================================================== */

/*******************************************************************************
* inside_rectangle: Check if (x,y) is inside rectangle (xwidth, yheight)
* return 0 if outside and 1 if inside
*******************************************************************************/
int inside_rectangle(double x, double y, double xwidth, double yheight)
{
  if (x>-xwidth/2 && x<xwidth/2 && y>-yheight/2 && y<yheight/2)
    return 1;
  else
    return 0;
}

/*******************************************************************************
 * box_intersect: compute time intersection with a box
 * returns 0 when no intersection is found
 *      or 1 in case of intersection with resulting times dt_in and dt_out
 * This function written by Stine Nyborg, 1999.
 *******************************************************************************/
int box_intersect(double *dt_in, double *dt_out,
                  double x, double y, double z,
                  double vx, double vy, double vz,
                  double dx, double dy, double dz)
{
  double x_in, y_in, z_in, tt, t[6], a, b;
  int i, count, s;

      /* Calculate intersection time for each of the six box surface planes
       *  If the box surface plane is not hit, the result is zero.*/

  if(vx != 0)
   {
    tt = -(dx/2 + x)/vx;
    y_in = y + tt*vy;
    z_in = z + tt*vz;
    if( y_in > -dy/2 && y_in < dy/2 && z_in > -dz/2 && z_in < dz/2)
      t[0] = tt;
    else
      t[0] = 0;

    tt = (dx/2 - x)/vx;
    y_in = y + tt*vy;
    z_in = z + tt*vz;
    if( y_in > -dy/2 && y_in < dy/2 && z_in > -dz/2 && z_in < dz/2)
      t[1] = tt;
    else
      t[1] = 0;
   }
  else
    t[0] = t[1] = 0;

  if(vy != 0)
   {
    tt = -(dy/2 + y)/vy;
    x_in = x + tt*vx;
    z_in = z + tt*vz;
    if( x_in > -dx/2 && x_in < dx/2 && z_in > -dz/2 && z_in < dz/2)
      t[2] = tt;
    else
      t[2] = 0;

    tt = (dy/2 - y)/vy;
    x_in = x + tt*vx;
    z_in = z + tt*vz;
    if( x_in > -dx/2 && x_in < dx/2 && z_in > -dz/2 && z_in < dz/2)
      t[3] = tt;
    else
      t[3] = 0;
   }
  else
    t[2] = t[3] = 0;

  if(vz != 0)
   {
    tt = -(dz/2 + z)/vz;
    x_in = x + tt*vx;
    y_in = y + tt*vy;
    if( x_in > -dx/2 && x_in < dx/2 && y_in > -dy/2 && y_in < dy/2)
      t[4] = tt;
    else
      t[4] = 0;

    tt = (dz/2 - z)/vz;
    x_in = x + tt*vx;
    y_in = y + tt*vy;
    if( x_in > -dx/2 && x_in < dx/2 && y_in > -dy/2 && y_in < dy/2)
      t[5] = tt;
    else
      t[5] = 0;
   }
  else
    t[4] = t[5] = 0;

  /* The intersection is evaluated and *dt_in and *dt_out are assigned */

  a = b = s = 0;
  count = 0;

  for( i = 0; i < 6; i = i + 1 )
    if( t[i] == 0 )
      s = s+1;
    else if( count == 0 )
    {
      a = t[i];
      count = 1;
    }
    else
    {
      b = t[i];
      count = 2;
    }

  if ( a == 0 && b == 0 )
    return 0;
  else if( a < b )
  {
    *dt_in = a;
    *dt_out = b;
    return 1;
  }
  else
  {
    *dt_in = b;
    *dt_out = a;
    return 1;
  }

} /* box_intersect */

/*******************************************************************************
 * cylinder_intersect: compute intersection with a cylinder
 * returns 0 when no intersection is found
 *      or 2/4/8/16 bits depending on intersection,
 *     and resulting times t0 and t1
 * Written by: EM,NB,ABA 4.2.98
  *******************************************************************************/
int cylinder_intersect(double *t0, double *t1, double x, double y, double z,
                   double vx, double vy, double vz, double r, double h)
{
  double D, t_in, t_out, y_in, y_out;
  int ret=1;

  D = (2*vx*x + 2*vz*z)*(2*vx*x + 2*vz*z)
    - 4*(vx*vx + vz*vz)*(x*x + z*z - r*r);

  if (D>=0)
  {
    if (vz*vz + vx*vx) {
      t_in  = (-(2*vz*z + 2*vx*x) - sqrt(D))/(2*(vz*vz + vx*vx));
      t_out = (-(2*vz*z + 2*vx*x) + sqrt(D))/(2*(vz*vz + vx*vx));
    } else if (vy) { /* trajectory parallel to cylinder axis */
      t_in = (-h/2-y)/vy;
      t_out = (h/2-y)/vy;
      if (t_in>t_out){
        double tmp=t_in;
        t_in=t_out;t_out=tmp;
      }
    } else return 0;
    y_in = vy*t_in + y;
    y_out =vy*t_out + y;

    if ( (y_in > h/2 && y_out > h/2) || (y_in < -h/2 && y_out < -h/2) )
      return 0;
    else
    {
      if (y_in > h/2)
        { t_in = ((h/2)-y)/vy; ret += 2; }
      else if (y_in < -h/2)
        { t_in = ((-h/2)-y)/vy; ret += 4; }
      if (y_out > h/2)
        { t_out = ((h/2)-y)/vy; ret += 8; }
      else if (y_out < -h/2)
        { t_out = ((-h/2)-y)/vy; ret += 16; }
    }
    *t0 = t_in;
    *t1 = t_out;
    return ret;
  }
  else
  {
    *t0 = *t1 = 0;
    return 0;
  }
} /* cylinder_intersect */


/*******************************************************************************
 * sphere_intersect: Calculate intersection between a line and a sphere.
 * returns 0 when no intersection is found
 *      or 1 in case of intersection with resulting times t0 and t1
 *******************************************************************************/
int sphere_intersect(double *t0, double *t1, double x, double y, double z,
                 double vx, double vy, double vz, double r)
{
  double A, B, C, D, v;

  v = sqrt(vx*vx + vy*vy + vz*vz);
  A = v*v;
  B = 2*(x*vx + y*vy + z*vz);
  C = x*x + y*y + z*z - r*r;
  D = B*B - 4*A*C;
  if(D < 0)
    return 0;
  D = sqrt(D);
  *t0 = (-B - D) / (2*A);
  *t1 = (-B + D) / (2*A);
  return 1;
} /* sphere_intersect */

/*******************************************************************************
 * plane_intersect: Calculate intersection between a plane and a line.
 * returns 0 when no intersection is found (i.e. line is parallel to the plane)
 * returns 1 or -1 when intersection time is positive and negative respectively
 *******************************************************************************/
int plane_intersect(double *t, double x, double y, double z,
                 double vx, double vy, double vz, double nx, double ny, double nz, double wx, double wy, double wz)
{
  double s;
  if (fabs(s=scalar_prod(nx,ny,nz,vx,vy,vz))<FLT_EPSILON) return 0;
  *t = - scalar_prod(nx,ny,nz,x-wx,y-wy,z-wz)/s;
  if (*t<0) return -1;
  else return 1;
} /* plane_intersect */

#endif /* !MCSTAS_H */
/* End of file "mcstas-r.c". */


/* *****************************************************************************
* Start of instrument 'BNL_H8' generated code
***************************************************************************** */

#ifdef MC_TRACE_ENABLED
int traceenabled = 1;
#else
int traceenabled = 0;
#endif
#define MCSTAS "/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../"
int   defaultmain         = 1;
char  instrument_name[]   = "BNL_H8";
char  instrument_source[] = "BNL_H8.instr";
char *instrument_exe      = NULL; /* will be set to argv[0] in main */
char  instrument_code[]   = "Instrument BNL_H8 source code BNL_H8.instr is not embedded in this executable.\n  Use --source option when running McStas.\n";

int main(int argc, char *argv[]){return mccode_main(argc, argv);}

/* *****************************************************************************
* instrument 'BNL_H8' and components DECLARE
***************************************************************************** */

/* Instrument parameters: structure and a table for the initialisation
   (Used in e.g. inputparse and I/O function (e.g. detector_out) */

struct _struct_instrument_parameters {
  MCNUM lambda;
};
typedef struct _struct_instrument_parameters _class_instrument_parameters;

/* instrument SPLIT, GROUP and JUMP control logic */
struct instrument_logic_struct {
  long Split_PG1Xtal; /* this is the SPLIT counter decremented down to 0 */
  _class_particle Split_PG1Xtal_particle; /* this is the particle to duplicate */
  long Split_Sample; /* this is the SPLIT counter decremented down to 0 */
  _class_particle Split_Sample_particle; /* this is the particle to duplicate */
  long Split_PG2Xtal; /* this is the SPLIT counter decremented down to 0 */
  _class_particle Split_PG2Xtal_particle; /* this is the particle to duplicate */
};

struct _instrument_struct {
  char   _name[256]; /* the name of this instrument e.g. 'BNL_H8' */
/* Counters per component instance */
  double counter_AbsorbProp[30]; /* absorbed events in PROP routines */
  double counter_N[30], counter_P[30], counter_P2[30]; /* event counters after each component instance */
  _class_particle _trajectory[30]; /* current trajectory for STORE/RESTORE */
/* Components position table (absolute and relative coords) */
  Coords _position_relative[30]; /* positions of all components */
  Coords _position_absolute[30];
  _class_instrument_parameters _parameters; /* instrument parameters */
  struct instrument_logic_struct logic; /* instrument logic */
} _instrument_var;
struct _instrument_struct *instrument = & _instrument_var;
#pragma acc declare create ( _instrument_var )
#pragma acc declare create ( instrument )

int numipar = 1;
struct mcinputtable_struct mcinputtable[] = {
  "lambda", &(_instrument_var._parameters.lambda), instr_type_double, "2.36", 
  NULL, NULL, instr_type_double, ""
};


/* ************************************************************************** */
/*             SHARE user declarations for all components                     */
/* ************************************************************************** */

/* Shared user declarations for all components types 'Guide'. */
/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/read_table-lib.h
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas 1.6
* Version: $Revision$
*
* This file is to be imported by components that may read data from table files
* It handles some shared functions.
*
* This library may be used directly as an external library. It has no dependency
*
* Usage: within SHARE
* %include "read_table-lib"
*
*******************************************************************************/

#ifndef READ_TABLE_LIB_H
#define READ_TABLE_LIB_H "$Revision$"

#define READ_TABLE_STEPTOL  0.04 /* tolerancy for constant step approx */

#ifndef MC_PATHSEP_C
#ifdef WIN32
#define MC_PATHSEP_C '\\'
#define MC_PATHSEP_S "\\"
#else  /* !WIN32 */
#ifdef MAC
#define MC_PATHSEP_C ':'
#define MC_PATHSEP_S ":"
#else  /* !MAC */
#define MC_PATHSEP_C '/'
#define MC_PATHSEP_S "/"
#endif /* !MAC */
#endif /* !WIN32 */
#endif /* !MC_PATHSEP_C */

#ifndef MCSTAS
#ifdef WIN32
#define MCSTAS "C:\\mcstas\\lib"
#else  /* !WIN32 */
#ifdef MAC
#define MCSTAS ":mcstas:lib" /* ToDo: What to put here? */
#else  /* !MAC */
#define MCSTAS "/usr/local/lib/mcstas"
#endif /* !MAC */
#endif /* !WIN32 */
#endif /* !MCSTAS */

#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

  typedef struct struct_table
  {
    char    filename[1024];
    long    filesize;
    char   *header;  /* text header, e.g. comments */
    double *data;    /* vector { x[0], y[0], ... x[n-1], y[n-1]... } */
    double  min_x;   /* min value of first column */
    double  max_x;   /* max value of first column */
    double  step_x;  /* minimal step value of first column */
    long    rows;    /* number of rows in matrix block */
    long    columns; /* number of columns in matrix block */

    long    begin;   /* start fseek index of block */
    long    end;     /* stop  fseek index of block */
    long    block_number;  /* block index. 0 is catenation of all */
    long    array_length;  /* number of elements in the t_Table array */
    char    monotonic;     /* true when 1st column/vector data is monotonic */
    char    constantstep;  /* true when 1st column/vector data has constant step */
    char    method[32];    /* interpolation method: nearest, linear */
  } t_Table;

/*maximum number of rows to rebin a table = 1M*/
enum { mcread_table_rebin_maxsize = 1000000 };

typedef struct t_Read_table_file_item {
    int ref_count;
    t_Table *table_ref;
} t_Read_table_file_item;

typedef enum enum_Read_table_file_actions {STORE,FIND,GC}  t_Read_table_file_actions;

/* read_table-lib function prototypes */
/* ========================================================================= */

/* 'public' functions */
long     Table_Read              (t_Table *Table, char *File, long block_number);
long     Table_Read_Offset       (t_Table *Table, char *File, long block_number,
                                  long *offset, long max_lines);
long     Table_Read_Offset_Binary(t_Table *Table, char *File, char *Type,
                                  long *Offset, long Rows, long Columns);
long     Table_Rebin(t_Table *Table); /* rebin table with regular 1st column and interpolate all columns 2:end */
long     Table_Info (t_Table Table);
#pragma acc routine
double   Table_Index(t_Table Table,   long i, long j); /* get indexed value */
#pragma acc routine
double   Table_Value(t_Table Table, double X, long j); /* search X in 1st column and return interpolated value in j-column */
t_Table *Table_Read_Array(char *File, long *blocks);
void     Table_Free_Array(t_Table *Table);
long     Table_Info_Array(t_Table *Table);
int      Table_SetElement(t_Table *Table, long i, long j, double value);
long     Table_Init(t_Table *Table, long rows, long columns); /* create a Table */
#pragma acc routine
double   Table_Value2d(t_Table Table, double X, double Y);    /* same as Table_Index with non-integer indices and 2d interpolation */
MCDETECTOR Table_Write(t_Table Table, char*file, char*xl, char*yl, 
           double x1, double x2, double y1, double y2); /* write Table to disk */
void * Table_File_List_Handler(t_Read_table_file_actions action, void *item, void *item_modifier);
t_Table *Table_File_List_find(char *name, int block, int offset);
int Table_File_List_gc(t_Table *tab);
void *Table_File_List_store(t_Table *tab);

#define Table_ParseHeader(header, ...) \
  Table_ParseHeader_backend(header,__VA_ARGS__,NULL);

char **Table_ParseHeader_backend(char *header, ...);

/* private functions */
void Table_Free(t_Table *Table);
long Table_Read_Handle(t_Table *Table, FILE *fid, long block_number, long max_lines, char *name);
static void Table_Stat(t_Table *Table);
#pragma acc routine
double Table_Interp1d(double x, double x1, double y1, double x2, double y2);
#pragma acc routine
double Table_Interp1d_nearest(double x, double x1, double y1, double x2, double y2);
#pragma acc routine
double Table_Interp2d(double x, double y, double x1, double y1, double x2, double y2,
double z11, double z12, double z21, double z22);


#endif

/* end of read_table-lib.h */
/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2009, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/read_table-lib.c
*
* %Identification
* Written by: EF
* Date: Aug 28, 2002
* Origin: ILL
* Release: McStas CVS_090504
* Version: $Revision$
*
* This file is to be imported by components that may read data from table files
* It handles some shared functions. Embedded within instrument in runtime mode.
*
* Usage: within SHARE
* %include "read_table-lib"
*
*******************************************************************************/

#ifndef READ_TABLE_LIB_H
#include "read_table-lib.h"
#endif


/*******************************************************************************
 * void *Table_File_List_Handler(action, item, item_modifier)
 *   ACTION: handle file entries in the read_table-lib file list. If a file is read - it is supposed to be
 *   stored in a list such that we can avoid reading the same file many times.
 *   input  action: FIND, STORE, GC. check if file exists in the list, store an item in the list, or check if it can be garbage collected.
 *   input item: depends on the action.
 *    FIND)  item is a filename, and item_modifier is the block number
 *    STORE) item is the Table to store - item_modifier is ignored
 *    GC)    item is the Table to check. If it has a ref_count >1 then this is simply decremented.
 *   return  depends on the action
 *    FIND)  return a reference to a table+ref_count item if found - NULL otherwise. I.e. NULL means the file has not been read before and must be read again.
 *    STORE) return NULL always
 *    GC)    return NULL if no garbage collection is needed, return an adress to the t_Table which should be garbage collected. 0x1 is returned if
 *           the item is not found in the list
*******************************************************************************/
void * Table_File_List_Handler(t_Read_table_file_actions action, void *item, void *item_modifier){

    /* logic here is Read_Table should include a call to FIND. If found the return value should just be used as
     * if the table had been read from disk. If not found then read the table and STORE.
     * Table_Free should include a call to GC. If this returns non-NULL then we should proceed with freeing the memory
     * associated with the table item - otherwise only decrement the reference counter since there are more references
     * that may need it.*/

    static t_Read_table_file_item read_table_file_list[1024];  
    static int read_table_file_count=0;

    t_Read_table_file_item *tr;
    switch(action){
        case FIND:
            /*interpret data item as a filename, if it is found return a pointer to the table and increment refcount.
             * if not found return the item itself*/
            tr=read_table_file_list;
            while ( tr->table_ref!=NULL ){
                int i=*((int*) item_modifier);
                int j=*( ((int*) item_modifier)+1);
                if ( !strcmp(tr->table_ref->filename,(char *) item) &&
                        tr->table_ref->block_number==i && tr->table_ref->begin==j ){
                    tr->ref_count++;
                    return (void *) tr;
                }
                tr++;
            }
            return NULL;
        case STORE:
            /*find an available slot and store references to table there*/
            tr=&(read_table_file_list[read_table_file_count++]);
            tr->table_ref = ((t_Table *) item);
            tr->ref_count++;
            return NULL;
        case GC:
            /* Should this item be garbage collected (freed) - if so scratch the entry and return the address of the item - 
             * else decrement ref_count and return NULL.
             * A non-NULL return expects the item to actually be freed afterwards.*/
            tr=read_table_file_list;
            while ( tr->table_ref!=NULL ){
                if ( tr->table_ref->data ==((t_Table *)item)->data && 
                        tr->table_ref->block_number == ((t_Table *)item)->block_number){
                    /*matching item found*/
                    if (tr->ref_count>1){
                        /*the item is found and no garbage collection needed*/
                        tr->ref_count--;
                        return NULL;
                    }else{
                        /* The item is found and the reference counter is 1.
                         * This means we should garbage collect. Move remaining list items up one slot,
                         * and return the table for garbage collection by caller*/
                        while (tr->table_ref!=NULL){
                            *tr=*(tr+1);
                            tr++;
                        }
                        read_table_file_count--;
                        return (t_Table *) item;
                    }
                }
                tr++;
            }
            /* item not found, and so should be garbage collected. This could be the case if freeing a
             * Table that has been constructed from code - not read from file. Return 0x1 to flag it for
             * collection.*/
            return (void *) 0x1 ;
    }
}

/* Access functions to the handler*/

/********************************************
 * t_Table *Table_File_List_find(char *name, int block, int offset)
 * input name: filename to search for in the file list
 * input block: data block in the file as each file may contain more than 1 data block.
 * return a ref. to a table if it is found (you may use this pointer and skip reading the file), NULL otherwise (i.e. go ahead and read the file)
*********************************************/
t_Table *Table_File_List_find(char *name, int block, int offset){
    int vars[2]={block,offset};
    t_Read_table_file_item *item = Table_File_List_Handler(FIND,name, vars);
    if (item == NULL){
        return NULL;
    }else{
        return item->table_ref;
    }
}
/********************************************
 * int Table_File_List_gc(t_Table *tab)
 * input tab: the table to check for references.
 * return 0: no garbage collection needed
 *        1: Table's data and header (at least) should be freed.
*********************************************/
int Table_File_List_gc(t_Table *tab){
    void *rval=Table_File_List_Handler(GC,tab,0);
    if (rval==NULL) return 0;
    else return 1;
}


/*****************************************************************************
 * void *Table_File_List_store(t_Table *tab)
 * input tab: pointer to table to store.
 * return None. 
*******************************************************************************/
void *Table_File_List_store(t_Table *tab){
    return Table_File_List_Handler(STORE,tab,0);
}


/*******************************************************************************
* FILE *Open_File(char *name, char *Mode, char *path)
*   ACTION: search for a file and open it. Optionally return the opened path.
*   input   name:  file name from which table should be extracted
*           mode: "r", "w", "a" or any valid fopen mode
*           path:  NULL or a pointer to at least 1024 allocated chars
*   return  initialized file handle or NULL in case of error
*******************************************************************************/

  FILE *Open_File(char *File, const char *Mode, char *Path)
  {
    char path[1024];
    FILE *hfile = NULL;
    
    if (!File || File[0]=='\0')                     return(NULL);
    if (!strcmp(File,"NULL") || !strcmp(File,"0"))  return(NULL);
    
    /* search in current or full path */
    strncpy(path, File, 1024);
    hfile = fopen(path, Mode);
    if(!hfile)
    {
      char dir[1024];

      if (!hfile && instrument_source[0] != '\0' && strlen(instrument_source)) /* search in instrument source location */
      {
        char *path_pos   = NULL;
        /* extract path: searches for last file separator */
        path_pos    = strrchr(instrument_source, MC_PATHSEP_C);  /* last PATHSEP */
        if (path_pos) {
          long path_length = path_pos +1 - instrument_source;  /* from start to path+sep */
          if (path_length) {
            strncpy(dir, instrument_source, path_length);
            dir[path_length] = '\0';
            snprintf(path, 1024, "%s%c%s", dir, MC_PATHSEP_C, File);
            hfile = fopen(path, Mode);
          }
        }
      }
      if (!hfile && instrument_exe[0] != '\0' && strlen(instrument_exe)) /* search in PWD instrument executable location */
      {
        char *path_pos   = NULL;
        /* extract path: searches for last file separator */
        path_pos    = strrchr(instrument_exe, MC_PATHSEP_C);  /* last PATHSEP */
        if (path_pos) {
          long path_length = path_pos +1 - instrument_exe;  /* from start to path+sep */
          if (path_length) {
            strncpy(dir, instrument_exe, path_length);
            dir[path_length] = '\0';
            snprintf(path, 1024, "%s%c%s", dir, MC_PATHSEP_C, File);
            hfile = fopen(path, Mode);
          }
        }
      }
      if (!hfile) /* search in HOME or . */
      {
        strcpy(dir, getenv("HOME") ? getenv("HOME") : ".");
        snprintf(path, 1024, "%s%c%s", dir, MC_PATHSEP_C, File);
        hfile = fopen(path, Mode);
      }
      if (!hfile) /* search in MCSTAS/data */
      {
        strcpy(dir, getenv(FLAVOR_UPPER) ? getenv(FLAVOR_UPPER) : MCSTAS);
        snprintf(path, 1024, "%s%c%s%c%s", dir, MC_PATHSEP_C, "data", MC_PATHSEP_C, File);
        hfile = fopen(path, Mode);
      }
      if (!hfile) /* search in MVCSTAS/contrib */
      {
        strcpy(dir, getenv(FLAVOR_UPPER) ? getenv(FLAVOR_UPPER) : MCSTAS);
        snprintf(path, 1024, "%s%c%s%c%s", dir, MC_PATHSEP_C, "contrib", MC_PATHSEP_C, File);
        hfile = fopen(path, Mode);
      }
      if(!hfile)
      {
        fprintf(stderr, "Error: Could not open input file '%s' (Open_File)\n", File);
        return (NULL);
      }
    }
    if (Path) strncpy(Path, path, 1024);
    return(hfile);
  } /* end Open_File */

/*******************************************************************************
* long Read_Table(t_Table *Table, char *name, int block_number)
*   ACTION: read a single Table from a text file
*   input   Table: pointer to a t_Table structure
*           name:  file name from which table should be extracted
*           block_number: if the file does contain more than one
*                 data block, then indicates which one to get (from index 1)
*                 a 0 value means append/catenate all
*   return  initialized single Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
* The routine stores any line starting with '#', '%' and ';' into the header
* File is opened, read and closed
* Other lines are interpreted as numerical data, and stored.
* Data block should be a rectangular matrix or vector.
* Data block may be rebinned with Table_Rebin (also sort in ascending order)
*******************************************************************************/
  long Table_Read(t_Table *Table, char *File, long block_number)
  { /* reads all or a single data block from 'file' and returns a Table structure  */
    return(Table_Read_Offset(Table, File, block_number, NULL, 0));
  } /* end Table_Read */

/*******************************************************************************
* long Table_Read_Offset(t_Table *Table, char *name, int block_number, long *offset
*                        long max_rows)
*   ACTION: read a single Table from a text file, starting at offset
*     Same as Table_Read(..) except:
*   input   offset:    pointer to an offset (*offset should be 0 at start)
*           max_rows: max number of data rows to read from file (0 means all)
*   return  initialized single Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
*           updated *offset position (where end of reading occured)
*******************************************************************************/
  long Table_Read_Offset(t_Table *Table, char *File,
                         long block_number, long *offset,
                         long max_rows)
  { /* reads all/a data block in 'file' and returns a Table structure  */
    FILE *hfile;
    long  nelements=0;
    long  begin=0;
    long  filesize=0;
    char  name[1024];
    char  path[1024];
    struct stat stfile;

    /*Need to be able to store the pointer*/
    if (!Table) return(-1);
    
    //if (offset && *offset) snprintf(name, 1024, "%s@%li", File, *offset);
    //else                   
    strncpy(name, File, 1024);
    if(offset && *offset){
        begin=*offset;
    }
    /* Check if the table has already been read from file.
     * If so just reuse the table, if not (this is flagged by returning NULL
     * set up a new table and read the data into it */
    t_Table *tab_p= Table_File_List_find(name,block_number,begin);
    if ( tab_p!=NULL ){
        /*table was found in the Table_File_List*/
        // printf("Reusing input file '%s' (Table_Read_Offset)\n", name);
        *Table=*tab_p;
        return Table->rows*Table->columns;
    }

    /* open the file */
    hfile = Open_File(File, "r", path);
    if (!hfile) return(-1);
    else {
      MPI_MASTER(
      printf("Opening input file '%s' (Table_Read_Offset)\n", path);
      );
    }
    
    /* read file state */
    stat(path,&stfile); filesize = stfile.st_size;
    if (offset && *offset) fseek(hfile, *offset, SEEK_SET);
    begin     = ftell(hfile);
    
    Table_Init(Table, 0, 0);

    /* read file content and set the Table */
    nelements = Table_Read_Handle(Table, hfile, block_number, max_rows, name);
    Table->begin = begin;
    Table->end   = ftell(hfile);
    Table->filesize = (filesize>0 ? filesize : 0);
    Table_Stat(Table);
    
    Table_File_List_store(Table);

    if (offset) *offset=Table->end;
    fclose(hfile);
    return(nelements);

  } /* end Table_Read_Offset */

/*******************************************************************************
* long Table_Read_Offset_Binary(t_Table *Table, char *File, char *type,
*                               long *offset, long rows, long columns)
*   ACTION: read a single Table from a binary file, starting at offset
*     Same as Table_Read_Offset(..) except that it handles binary files.
*   input   type: may be "float"/NULL or "double"
*           offset: pointer to an offset (*offset should be 0 at start)
*           rows   : number of rows (0 means read all)
*           columns: number of columns
*   return  initialized single Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
*           updated *offset position (where end of reading occured)
*******************************************************************************/
  long Table_Read_Offset_Binary(t_Table *Table, char *File, char *type,
                                long *offset, long rows, long columns)
  { /* reads all/a data block in binary 'file' and returns a Table structure  */
    long    nelements, sizeofelement;
    long    filesize;
    FILE   *hfile;
    char    path[1024];
    struct stat stfile;
    double *data;
    long    i;
    long    begin;

    if (!Table) return(-1);

    Table_Init(Table, 0, 0);
    
    /* open the file */
    hfile = Open_File(File, "r", path);
    if (!hfile) return(-1);
    else {
      MPI_MASTER(
      printf("Opening input file '%s' (Table_Read, Binary)\n", path);
      );
    }
    
    /* read file state */
    stat(File,&stfile);
    filesize = stfile.st_size;
    Table->filesize=filesize;
    
    /* read file content */
    if (type && !strcmp(type,"double")) sizeofelement = sizeof(double);
    else  sizeofelement = sizeof(float);
    if (offset && *offset) fseek(hfile, *offset, SEEK_SET);
    begin     = ftell(hfile);
    if (rows && filesize > sizeofelement*columns*rows)
      nelements = columns*rows;
    else nelements = (long)(filesize/sizeofelement);
    if (!nelements || filesize <= *offset) return(0);
    data    = (double*)malloc(nelements*sizeofelement);
    if (!data) {
      fprintf(stderr,"Error: allocating %ld elements for %s file '%s'. Too big (Table_Read_Offset_Binary).\n", nelements, type, File);
      exit(-1);
    }
    nelements = fread(data, sizeofelement, nelements, hfile);

    if (!data || !nelements)
    {
      fprintf(stderr,"Error: reading %ld elements from %s file '%s' (Table_Read_Offset_Binary)\n", nelements, type, File);
      exit(-1);
    }
    Table->begin   = begin;
    Table->end     = ftell(hfile);
    if (offset) *offset=Table->end;
    fclose(hfile);
    data = (double*)realloc(data, (double)nelements*sizeofelement);
    /* copy file data into Table */
    if (type && !strcmp(type,"double")) Table->data = data;
    else {
      float  *s;
      double *dataf;
      s     = (float*)data;
      dataf = (double*)malloc(sizeof(double)*nelements);
      for (i=0; i<nelements; i++)
        dataf[i]=s[i];
      free(data);
      Table->data = dataf;
    }
    strncpy(Table->filename, File, 1024);
    Table->rows    = nelements/columns;
    Table->columns = columns;
    Table->array_length = 1;
    Table->block_number = 1;

    Table_Stat(Table);

    return(nelements);
  } /* end Table_Read_Offset_Binary */

/*******************************************************************************
* long Table_Read_Handle(t_Table *Table, FILE *fid, int block_number, long max_rows, char *name)
*   ACTION: read a single Table from a text file handle (private)
*   input   Table:pointer to a t_Table structure
*           fid:  pointer to FILE handle
*           block_number: if the file does contain more than one
*                 data block, then indicates which one to get (from index 1)
*                 a 0 value means append/catenate all
*           max_rows: if non 0, only reads that number of lines
*   return  initialized single Table t_Table structure containing data, header, ...
*           modified Table t_Table structure containing data, header, ...
*           number of read elements (-1: error, 0:header only)
* The routine stores any line starting with '#', '%' and ';' into the header
* Other lines are interpreted as numerical data, and stored.
* Data block should be a rectangular matrix or vector.
* Data block may be rebined with Table_Rebin (also sort in ascending order)
*******************************************************************************/
  long Table_Read_Handle(t_Table *Table, FILE *hfile,
                         long block_number, long max_rows, char *name)
  { /* reads all/a data block from 'file' handle and returns a Table structure  */
    double *Data;
    char *Header              = NULL;
    long  malloc_size         = CHAR_BUF_LENGTH;
    long  malloc_size_h       = 4096;
    long  Rows = 0,   Columns = 0;
    long  count_in_array      = 0;
    long  count_in_header     = 0;
    long  block_Current_index = 0;
    char  flag_End_row_loop   = 0;

    if (!Table) return(-1);
    Table_Init(Table, 0, 0);
    if (name && name[0]!='\0') strncpy(Table->filename, name, 1024);

    if(!hfile) {
       fprintf(stderr, "Error: File handle is NULL (Table_Read_Handle).\n");
       return (-1);
    }
    Header = (char*)  calloc(malloc_size_h, sizeof(char));
    Data   = (double*)calloc(malloc_size,   sizeof(double));
    if ((Header == NULL) || (Data == NULL)) {
       fprintf(stderr, "Error: Could not allocate Table and Header (Table_Read_Handle).\n");
       return (-1);
    }

    int flag_In_array = 0;
    do { /* while (!flag_End_row_loop) */
      char  line[1024*CHAR_BUF_LENGTH];
      long  back_pos=0;   /* ftell start of line */

      back_pos = ftell(hfile);
      if (fgets(line, 1024*CHAR_BUF_LENGTH, hfile) != NULL) { /* analyse line */
        /* first skip blank and tabulation characters */
        int i = strspn(line, " \t");

        /* handle comments: stored in header */
        if (NULL != strchr("#%;/", line[i]))
        { /* line is a comment */
          count_in_header += strlen(line);
          if (count_in_header >= malloc_size_h) {
            /* if succeed and in array : add (and realloc if necessary) */
            malloc_size_h = count_in_header+4096;
            Header        = (char*)realloc(Header, malloc_size_h*sizeof(char));
          }
          strncat(Header, line, 4096);
          flag_In_array=0;
          /* exit line and file if passed desired block */
          if (block_number > 0 && block_number == block_Current_index) {
            flag_End_row_loop = 1;
          }

          /* Continue with next line */
          continue;
        }

        /* get the number of columns splitting line with strtok */
        char  *lexeme;
        char  flag_End_Line = 0;
        long  block_Num_Columns = 0;
        const char seps[] = " ,;\t\n\r";

        lexeme = strtok(line, seps);
        while (!flag_End_Line) {
          if ((lexeme != NULL) && (lexeme[0] != '\0')) {
            /* reading line: the token is not empty */
            double X;
            int    count=1;
            /* test if we have 'NaN','Inf' */
            if (!strncasecmp(lexeme,"NaN",3))
              X = 0;
            else if (!strncasecmp(lexeme,"Inf",3) || !strncasecmp(lexeme,"+Inf",4))
              X = FLT_MAX;
            else if (!strncasecmp(lexeme,"-Inf",4))
              X = -FLT_MAX;
            else
              count = sscanf(lexeme,"%lg",&X);
            if (count == 1) {
              /* reading line: the token is a number in the line */
              if (!flag_In_array) {
                /* reading num: not already in a block: starts a new data block */
                block_Current_index++;
                flag_In_array    = 1;
                block_Num_Columns= 0;
                if (block_number > 0) {
                  /* initialise a new data block */
                  Rows = 0;
                  count_in_array = 0;
                } /* else append */
              }
              /* reading num: all blocks or selected block */
              if (flag_In_array && (block_number == 0 ||
                  block_number == block_Current_index)) {
                /* starting block: already the desired number of rows ? */
                if (block_Num_Columns == 0 &&
                    max_rows > 0 && Rows >= max_rows) {
                  flag_End_Line      = 1;
                  flag_End_row_loop  = 1;
                  flag_In_array      = 0;
                  /* reposition to begining of line (ignore line) */
                  fseek(hfile, back_pos, SEEK_SET);
                } else { /* store into data array */
                  if (count_in_array >= malloc_size) {
                    /* realloc data buffer if necessary */
                    malloc_size = count_in_array*1.5;
                    Data = (double*) realloc(Data, malloc_size*sizeof(double));
                    if (Data == NULL) {
                      fprintf(stderr, "Error: Can not re-allocate memory %li (Table_Read_Handle).\n",
                              malloc_size*sizeof(double));
                      return (-1);
                    }
                  }
                  if (0 == block_Num_Columns) Rows++;
                  Data[count_in_array] = X;
                  count_in_array++;
                  block_Num_Columns++;
                }
              } /* reading num: end if flag_In_array */
            } /* end reading num: end if sscanf lexeme -> numerical */
            else {
              /* reading line: the token is not numerical in that line. end block */
              if (block_Current_index == block_number) {
                flag_End_Line = 1;
                flag_End_row_loop = 1;
              } else {
                flag_In_array = 0;
                flag_End_Line = 1;
              }
            }
          }
          else {
            /* no more tokens in line */
            flag_End_Line = 1;
            if (block_Num_Columns > 0) Columns = block_Num_Columns;
          }

          // parse next token
          lexeme = strtok(NULL, seps);

        } /* while (!flag_End_Line) */
      } /* end: if fgets */
      else flag_End_row_loop = 1; /* else fgets : end of file */

    } while (!flag_End_row_loop); /* end while flag_End_row_loop */

    Table->block_number = block_number;
    Table->array_length = 1;

    // shrink header to actual size (plus terminating 0-byte)
    if (count_in_header) {
      Header = (char*)realloc(Header, count_in_header*sizeof(char) + 1);
    }
    Table->header = Header;

    if (count_in_array*Rows*Columns == 0)
    {
      Table->rows         = 0;
      Table->columns      = 0;
      free(Data);
      return (0);
    }
    if (Rows * Columns != count_in_array)
    {
      fprintf(stderr, "Warning: Read_Table :%s %s Data has %li values that should be %li x %li\n",
        (Table->filename[0] != '\0' ? Table->filename : ""),
        (!block_number ? " catenated" : ""),
        count_in_array, Rows, Columns);
      Columns = count_in_array; Rows = 1;
    }
    Data     = (double*)realloc(Data, count_in_array*sizeof(double));
    Table->data         = Data;
    Table->rows         = Rows;
    Table->columns      = Columns;

    return (count_in_array);

  } /* end Table_Read_Handle */

/*******************************************************************************
* long Table_Rebin(t_Table *Table)
*   ACTION: rebin a single Table, sorting 1st column in ascending order
*   input   Table: single table containing data.
*                  The data block is reallocated in this process
*   return  updated Table with increasing, evenly spaced first column (index 0)
*           number of data elements (-1: error, 0:empty data)
*******************************************************************************/
  long Table_Rebin(t_Table *Table)
  {
    double new_step=0;
    long   i;
    /* performs linear interpolation on X axis (0-th column) */

    if (!Table) return(-1);
    if (!Table->data 
    || Table->rows*Table->columns == 0 || !Table->step_x)
      return(0);
    Table_Stat(Table); /* recompute statitstics and minimal step */
    new_step = Table->step_x; /* minimal step in 1st column */

    if (!(Table->constantstep)) /* not already evenly spaced */
    {
      long Length_Table;
      double *New_Table;

      Length_Table = ceil(fabs(Table->max_x - Table->min_x)/new_step)+1;
      /*return early if the rebinned table will become too large*/
      if (Length_Table > mcread_table_rebin_maxsize){
        fprintf(stderr,"WARNING: (Table_Rebin): Rebinning table from %s would exceed 1M rows. Skipping.\n", Table->filename); 
        return(Table->rows*Table->columns);
      }
      New_Table    = (double*)malloc(Length_Table*Table->columns*sizeof(double));

      for (i=0; i < Length_Table; i++)
      {
        long   j;
        double X;
        X = Table->min_x + i*new_step;
        New_Table[i*Table->columns] = X;
        for (j=1; j < Table->columns; j++)
          New_Table[i*Table->columns+j]
                = Table_Value(*Table, X, j);
      } /* end for i */

      Table->rows = Length_Table;
      Table->step_x = new_step;
      Table->max_x = Table->min_x + (Length_Table-1)*new_step; 
      /*max might not be the same anymore
       * Use Length_Table -1 since the first and laset rows are the limits of the defined interval.*/
      free(Table->data);
      Table->data = New_Table;
      Table->constantstep=1;
    } /* end else (!constantstep) */
    return (Table->rows*Table->columns);
  } /* end Table_Rebin */

/*******************************************************************************
* double Table_Index(t_Table Table, long i, long j)
*   ACTION: read an element [i,j] of a single Table
*   input   Table: table containing data
*           i : index of row      (0:Rows-1)
*           j : index of column   (0:Columns-1)
*   return  Value = data[i][j]
* Returns Value from the i-th row, j-th column of Table
* Tests are performed on indexes i,j to avoid errors
*******************************************************************************/

#ifndef MIN
#define MIN(a, b)  (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a, b)  (((a) > (b)) ? (a) : (b))
#endif

double Table_Index(t_Table Table, long i, long j)
{
  long AbsIndex;

  if (Table.rows == 1 || Table.columns == 1) {
    /* vector */
    j = MIN(MAX(0, i+j), Table.columns*Table.rows - 1);
    i = 0;
  } else {
    /* matrix */
    i = MIN(MAX(0, i), Table.rows - 1);
    j = MIN(MAX(0, j), Table.columns - 1);
  }

  /* handle vectors specifically */
  AbsIndex = i*(Table.columns)+j;

  if (Table.data != NULL)
    return (Table.data[AbsIndex]);
  else
    return 0;
} /* end Table_Index */

/*******************************************************************************
* void Table_SetElement(t_Table *Table, long i, long j, double value)
*   ACTION: set an element [i,j] of a single Table
*   input   Table: table containing data
*           i : index of row      (0:Rows-1)
*           j : index of column   (0:Columns-1)
*           value = data[i][j]
* Returns 0 in case of error
* Tests are performed on indexes i,j to avoid errors
*******************************************************************************/
int Table_SetElement(t_Table *Table, long i, long j,
                     double value)
{
  long AbsIndex;

  if (Table->rows == 1 || Table->columns == 1) {
    /* vector */
    j = MIN(MAX(0, i+j), Table->columns*Table->rows - 1); i=0;
  } else {
    /* matrix */
    i = MIN(MAX(0, i), Table->rows - 1);
    j = MIN(MAX(0, j), Table->columns - 1);
  }

  AbsIndex = i*(Table->columns)+j;
  if (Table->data != NULL) {
    Table->data[AbsIndex] = value;
    return 1;
  }

  return 0;
} /* end Table_SetElement */

/*******************************************************************************
* double Table_Value(t_Table Table, double X, long j)
*   ACTION: read column [j] of a single Table at row which 1st column is X
*   input   Table: table containing data.
*           X : data value in the first column (index 0)
*           j : index of column from which is extracted the Value (0:Columns-1)
*   return  Value = data[index for X][j] with linear interpolation
* Returns Value from the j-th column of Table corresponding to the
* X value for the 1st column (index 0)
* Tests are performed (within Table_Index) on indexes i,j to avoid errors
* NOTE: data should rather be monotonic, and evenly sampled.
*******************************************************************************/
double Table_Value(t_Table Table, double X, long j)
{
  long   Index = -1;
  double X1=0, Y1=0, X2=0, Y2=0;
  double ret=0;

  if (X > Table.max_x) return Table_Index(Table,Table.rows-1  ,j);
  if (X < Table.min_x) return Table_Index(Table,0  ,j);

  // Use constant-time lookup when possible
  if(Table.constantstep) {
    Index = (long)floor(
              (X - Table.min_x) / (Table.max_x - Table.min_x) * (Table.rows-1));
    X1 = Table_Index(Table,Index-1,0);
    X2 = Table_Index(Table,Index  ,0);
  }
  // Use binary search on large, monotonic tables
  else if(Table.monotonic && Table.rows > 100) {
    long left = Table.min_x;
    long right = Table.max_x;

    while (!((X1 <= X) && (X < X2)) && (right - left > 1)) {
      Index = (left + right) / 2;

      X1 = Table_Index(Table, Index-1, 0);
      X2 = Table_Index(Table, Index,   0);

      if (X < X1) {
        right = Index;
      } else {
        left  = Index;
      }
    }
  }

  // Fall back to linear search, if no-one else has set X1, X2 correctly
  if (!((X1 <= X) && (X < X2))) {
    /* look for index surrounding X in the table -> Index */
    for (Index=1; Index <= Table.rows-1; Index++) {
        X1 = Table_Index(Table, Index-1,0);
        X2 = Table_Index(Table, Index  ,0);
        if ((X1 <= X) && (X < X2)) break;
      } /* end for Index */
  }

  Y1 = Table_Index(Table,Index-1, j);
  Y2 = Table_Index(Table,Index  , j);

#ifdef OPENACC
#define strcmp(a,b) str_comp(a,b)
#endif

  if (!strcmp(Table.method,"linear")) {
    ret = Table_Interp1d(X, X1,Y1, X2,Y2);
  }
  else if (!strcmp(Table.method,"nearest")) {
    ret = Table_Interp1d_nearest(X, X1,Y1, X2,Y2);
  }

#ifdef OPENACC
#ifdef strcmp
#undef strcmp
#endif
#endif

  return ret;
} /* end Table_Value */

/*******************************************************************************
* double Table_Value2d(t_Table Table, double X, double Y)
*   ACTION: read element [X,Y] of a matrix Table
*   input   Table: table containing data.
*           X : row index, may be non integer
*           Y : column index, may be non integer
*   return  Value = data[index X][index Y] with bi-linear interpolation
* Returns Value for the indices [X,Y]
* Tests are performed (within Table_Index) on indexes i,j to avoid errors
* NOTE: data should rather be monotonic, and evenly sampled.
*******************************************************************************/
double Table_Value2d(t_Table Table, double X, double Y)
  {
    long   x1,x2,y1,y2;
    double z11,z12,z21,z22;
    double ret=0;

    x1 = (long)floor(X);
    y1 = (long)floor(Y);

    if (x1 > Table.rows-1 || x1 < 0) {
      x2 = x1;
    } else {
      x2 = x1 + 1;
    }

    if (y1 > Table.columns-1 || y1 < 0) {
      y2 = y1;
    } else {
      y2 = y1 + 1;
    }

    z11 = Table_Index(Table, x1, y1);

    if (y2 != y1) z12=Table_Index(Table, x1, y2); else z12 = z11;
    if (x2 != x1) z21=Table_Index(Table, x2, y1); else z21 = z11;
    if (y2 != y1) z22=Table_Index(Table, x2, y2); else z22 = z21;

#ifdef OPENACC
#define strcmp(a,b) str_comp(a,b)
#endif

    if (!strcmp(Table.method,"linear"))
      ret = Table_Interp2d(X,Y, x1,y1,x2,y2, z11,z12,z21,z22);
#ifdef OPENACC
#ifdef strcmp
#undef strcmp
#endif
#endif
    else {
      if (fabs(X-x1) < fabs(X-x2)) {
        if (fabs(Y-y1) < fabs(Y-y2)) ret = z11; else ret = z12;
      } else {
        if (fabs(Y-y1) < fabs(Y-y2)) ret = z21; else ret = z22;
      }
    }
    return ret;
  } /* end Table_Value2d */


/*******************************************************************************
* void Table_Free(t_Table *Table)
*   ACTION: free a single Table. First Call Table_File_list_gc. If this returns
*   non-zero it means there are more refernces to the table, and so the table
*   should not bee freed.
*   return: empty Table
*******************************************************************************/
  void Table_Free(t_Table *Table)
  {
    if( !Table_File_List_gc(Table) ){
       return;
    } 
    if (!Table) return;
    if (Table->data   != NULL) free(Table->data);
    if (Table->header != NULL) free(Table->header);
    Table->data   = NULL;
    Table->header = NULL;
  } /* end Table_Free */

/******************************************************************************
* void Table_Info(t_Table Table)
*    ACTION: print informations about a single Table
*******************************************************************************/
  long Table_Info(t_Table Table)
  {
    char buffer[256];
    long ret=0;

    if (!Table.block_number) strcpy(buffer, "catenated");
    else sprintf(buffer, "block %li", Table.block_number);
    printf("Table from file '%s' (%s)",
      Table.filename[0] != '\0' ? Table.filename : "", buffer);
    if ((Table.data != NULL) && (Table.rows*Table.columns))
    {
      printf(" is %li x %li ", Table.rows, Table.columns);
      if (Table.rows*Table.columns > 1)
        printf("(x=%g:%g)", Table.min_x, Table.max_x);
      else printf("(x=%g) ", Table.min_x);
      ret = Table.rows*Table.columns;
      if (Table.monotonic)    printf(", monotonic");
      if (Table.constantstep) printf(", constant step");
      printf(". interpolation: %s\n", Table.method);
    }
    else printf(" is empty.\n");

    if (Table.header && strlen(Table.header)) {
      char *header;
      int  i;
      header = malloc(80);
      if (!header) return(ret);
      for (i=0; i<80; header[i++]=0);
      strncpy(header, Table.header, 75);
      if (strlen(Table.header) > 75) {
        strcat( header, " ...");
      }
      for (i=0; i<strlen(header); i++)
        if (header[i] == '\n' || header[i] == '\r') header[i] = ';';
      printf("  '%s'\n", header);
      free(header);
    }

    return(ret);
  } /* end Table_Info */

/******************************************************************************
* long Table_Init(t_Table *Table, m, n)
*   ACTION: initialise a Table to empty m by n table
*   return: empty Table
******************************************************************************/
long Table_Init(t_Table *Table, long rows, long columns)
{
  double *data=NULL;
  long   i;

  if (!Table) return(0);

  Table->header  = NULL;
  Table->filename[0]= '\0';
  Table->filesize= 0;
  Table->min_x   = 0;
  Table->max_x   = 0;
  Table->step_x  = 0;
  Table->block_number = 0;
  Table->array_length = 0;
  Table->monotonic    = 0;
  Table->constantstep = 0;
  Table->begin   = 0;
  Table->end     = 0;
  strcpy(Table->method,"linear");

  if (rows*columns >= 1) {
    data    = (double*)malloc(rows*columns*sizeof(double));
    if (data) for (i=0; i < rows*columns; data[i++]=0);
    else {
      fprintf(stderr,"Error: allocating %ld double elements."
                     "Too big (Table_Init).\n", rows*columns);
      rows = columns = 0;
    }
  }
  Table->rows    = (rows >= 1 ? rows : 0);
  Table->columns = (columns >= 1 ? columns : 0);
  Table->data    = data;
  return(Table->rows*Table->columns);
} /* end Table_Init */

/******************************************************************************
* long Table_Write(t_Table Table, char *file, x1,x2, y1,y2)
*   ACTION: write a Table to disk (ascii).
*     when x1=x2=0 or y1=y2=0, the table default limits are used.
*   return: 0=all is fine, non-0: error
*******************************************************************************/
MCDETECTOR Table_Write(t_Table Table, char *file, char *xl, char *yl, 
  double x1, double x2, double y1, double y2)
{
  MCDETECTOR detector;

  if ((Table.data == NULL) && (Table.rows*Table.columns)) {
    detector.m = 0;
    return(detector); /* Table is empty - nothing to do */
  }
  if (!x1 && !x2) {
    x1 = Table.min_x;
    x2 = Table.max_x;
  }
  if (!y1 && !y2) {
    y1 = 1;
    y2 = Table.columns;
  }

  /* transfer content of the Table into a 2D detector */
  Coords coords = { 0, 0, 0};

  if (Table.rows == 1 || Table.columns == 1) {
    detector = mcdetector_out_1D(Table.filename,
                      xl ? xl : "", yl ? yl : "",
                      "x", x1, x2,
                      Table.rows * Table.columns,
                      NULL, Table.data, NULL,
                      file, file, coords);
  } else {
    detector = mcdetector_out_2D(Table.filename,
                      xl ? xl : "", yl ? yl : "",
                      x1, x2, y1, y2,
                      Table.rows, Table.columns,
                      NULL, Table.data, NULL,
                      file, file, coords);
  }
  return(detector);
}

/******************************************************************************
* void Table_Stat(t_Table *Table)
*   ACTION: computes min/max/mean step of 1st column for a single table (private)
*   return: updated Table
*******************************************************************************/
  static void Table_Stat(t_Table *Table)
  {
    long   i;
    double max_x, min_x;
    double row=1;
    char   monotonic=1;
    char   constantstep=1;
    double step=0;
    long n;

    if (!Table) return;
    if (!Table->rows || !Table->columns) return;
    if (Table->rows == 1) row=0; // single row
    max_x = -FLT_MAX;
    min_x =  FLT_MAX;
    n     = (row ? Table->rows : Table->columns);
    /* get min and max of first column/vector */
    for (i=0; i < n; i++)
    {
      double X;
      X = (row ? Table_Index(*Table,i  ,0)
                               : Table_Index(*Table,0, i));
      if (X < min_x) min_x = X;
      if (X > max_x) max_x = X;
    } /* for */
    
    /* test for monotonicity and constant step if the table is an XY or single vector */
    if (n > 1) {
      /* mean step */
      step = (max_x - min_x)/(n-1);
      /* now test if table is monotonic on first column, and get minimal step size */
      for (i=0; i < n-1; i++) {
        double X, diff;;
        X    = (row ? Table_Index(*Table,i  ,0)
                    : Table_Index(*Table,0,  i));
        diff = (row ? Table_Index(*Table,i+1,0)
                    : Table_Index(*Table,0,  i+1)) - X;
        if (diff && fabs(diff) < fabs(step)) step = diff;
        /* change sign ? */
        if ((max_x - min_x)*diff < 0 && monotonic)
          monotonic = 0;
      } /* end for */
      
      /* now test if steps are constant within READ_TABLE_STEPTOL */
      if(!step){
        /*means there's a disconitnuity -> not constantstep*/
        constantstep=0;
      }else if (monotonic) {
        for (i=0; i < n-1; i++) {
          double X, diff;
          X    = (row ? Table_Index(*Table,i  ,0)
              : Table_Index(*Table,0,  i));
          diff = (row ? Table_Index(*Table,i+1,0)
              : Table_Index(*Table,0,  i+1)) - X;
          if ( fabs(step)*(1+READ_TABLE_STEPTOL) < fabs(diff) ||
                fabs(diff) < fabs(step)*(1-READ_TABLE_STEPTOL) )
          { constantstep = 0; break; }
        }
      }

    }
    Table->step_x= step;
    Table->max_x = max_x;
    Table->min_x = min_x;
    Table->monotonic = monotonic;
    Table->constantstep = constantstep;
  } /* end Table_Stat */

/******************************************************************************
* t_Table *Table_Read_Array(char *File, long *blocks)
*   ACTION: read as many data blocks as available, iteratively from file
*   return: initialized t_Table array, last element is an empty Table.
*           the number of extracted blocks in non NULL pointer *blocks
*******************************************************************************/
  t_Table *Table_Read_Array(char *File, long *blocks)
  {
    t_Table *Table_Array=NULL;
    long offset=0;
    long block_number=0;
    long allocated=256;
    long nelements=1;

    /* first allocate an initial empty t_Table array */
    Table_Array = (t_Table *)malloc(allocated*sizeof(t_Table));
    if (!Table_Array) {
      fprintf(stderr, "Error: Can not allocate memory %li (Table_Read_Array).\n",
         allocated*sizeof(t_Table));
      *blocks = 0;
      return (NULL);
    }

    while (nelements > 0)
    {
      t_Table Table;

      /* if ok, set t_Table block number else exit loop */
      block_number++;
      Table.block_number = block_number;
      
      /* access file at offset and get following block. Block number is from the set offset
       * hence the hardcoded 1 - i.e. the next block counted from offset.*/
      nelements = Table_Read_Offset(&Table, File, 1, &offset,0);
      /*if the block is empty - don't store it*/
      if (nelements>0){
          /* if t_Table array is not long enough, expand and realocate */
          if (block_number >= allocated-1) {
              allocated += 256;
              Table_Array = (t_Table *)realloc(Table_Array,
                      allocated*sizeof(t_Table));
              if (!Table_Array) {
                  fprintf(stderr, "Error: Can not re-allocate memory %li (Table_Read_Array).\n",
                          allocated*sizeof(t_Table));
                  *blocks = 0;
                  return (NULL);
              }
          }
          /* store it into t_Table array */
          //snprintf(Table.filename, 1024, "%s#%li", File, block_number-1);
          Table_Array[block_number-1] = Table;
      }
      /* continues until we find an empty block */
    }
    /* send back number of extracted blocks */
    if (blocks) *blocks = block_number-1;

    /* now store total number of elements in Table array */
    for (offset=0; offset < block_number;
      Table_Array[offset++].array_length = block_number-1);

    return(Table_Array);
  } /* end Table_Read_Array */
/*******************************************************************************
* void Table_Free_Array(t_Table *Table)
*   ACTION: free a Table array
*******************************************************************************/
  void Table_Free_Array(t_Table *Table)
  {
    long index;
    if (!Table) return;
    for (index=0;index < Table[0].array_length; index++){
            Table_Free(&Table[index]);
    }
    free(Table);
  } /* end Table_Free_Array */

/******************************************************************************
* long Table_Info_Array(t_Table *Table)
*    ACTION: print informations about a Table array
*    return: number of elements in the Table array
*******************************************************************************/
  long Table_Info_Array(t_Table *Table)
  {
    long index=0;

    if (!Table) return(-1);
    while (index < Table[index].array_length
       && (Table[index].data || Table[index].header)
       && (Table[index].rows*Table[index].columns) ) {
      Table_Info(Table[index]);
      index++;
    }
    printf("This Table array contains %li elements\n", index);
    return(index);
  } /* end Table_Info_Array */

/******************************************************************************
* char **Table_ParseHeader(char *header, symbol1, symbol2, ..., NULL)
*    ACTION: search for char* symbols in header and return their value or NULL
*            the search is not case sensitive.
*            Last argument MUST be NULL
*    return: array of char* with line following each symbol, or NULL if not found
*******************************************************************************/
#ifndef MyNL_ARGMAX
#define MyNL_ARGMAX 50
#endif

char **Table_ParseHeader_backend(char *header, ...){
  va_list ap;
  char exit_flag=0;
  int counter   =0;
  char **ret    =NULL;
  if (!header || header[0]=='\0') return(NULL);

  ret = (char**)calloc(MyNL_ARGMAX, sizeof(char*));
  if (!ret) {
    printf("Table_ParseHeader: Cannot allocate %i values array for Parser (Table_ParseHeader).\n",
      MyNL_ARGMAX);
    return(NULL);
  }
  for (counter=0; counter < MyNL_ARGMAX; ret[counter++] = NULL);
  counter=0;

  va_start(ap, header);
  while(!exit_flag && counter < MyNL_ARGMAX-1)
  {
    char *arg_char=NULL;
    char *pos     =NULL;
    /* get variable argument value as a char */
    arg_char = va_arg(ap, char *);
    if (!arg_char || arg_char[0]=='\0'){
      exit_flag = 1; break;
    }
    /* search for the symbol in the header */
    pos = (char*)strcasestr(header, arg_char);
    if (pos) {
      char *eol_pos;
      eol_pos = strchr(pos+strlen(arg_char), '\n');
      if (!eol_pos)
        eol_pos = strchr(pos+strlen(arg_char), '\r');
      if (!eol_pos)
        eol_pos = pos+strlen(pos)-1;
      ret[counter] = (char*)malloc(eol_pos - pos);
      if (!ret[counter]) {
        printf("Table_ParseHeader: Cannot allocate value[%i] array for Parser searching for %s (Table_ParseHeader).\n",
          counter, arg_char);
        exit_flag = 1; break;
      }
      strncpy(ret[counter], pos+strlen(arg_char), eol_pos - pos - strlen(arg_char));
      ret[counter][eol_pos - pos - strlen(arg_char)]='\0';
    }
    counter++;
  }
  va_end(ap);
  return(ret);
} /* Table_ParseHeader */

/******************************************************************************
* double Table_Interp1d(x, x1, y1, x2, y2)
*    ACTION: interpolates linearly at x between y1=f(x1) and y2=f(x2)
*    return: y=f(x) value
*******************************************************************************/
double Table_Interp1d(double x,
  double x1, double y1,
  double x2, double y2)
{
  double slope;
  if (x2 == x1) return (y1+y2)/2;
  if (y1 == y2) return  y1;
  slope = (y2 - y1)/(x2 - x1);
  return y1+slope*(x - x1);
} /* Table_Interp1d */

/******************************************************************************
* double Table_Interp1d_nearest(x, x1, y1, x2, y2)
*    ACTION: table lookup with nearest method at x between y1=f(x1) and y2=f(x2)
*    return: y=f(x) value
*******************************************************************************/
double Table_Interp1d_nearest(double x,
  double x1, double y1,
  double x2, double y2)
{
  if (fabs(x-x1) < fabs(x-x2)) return (y1);
  else return(y2);
} /* Table_Interp1d_nearest */

/******************************************************************************
* double Table_Interp2d(x,y, x1,y1, x2,y2, z11,z12,z21,z22)
*    ACTION: interpolates bi-linearly at (x,y) between z1=f(x1,y1) and z2=f(x2,y2)
*    return: z=f(x,y) value
*    x,y |   x1   x2
*    ----------------
*     y1 |   z11  z21
*     y2 |   z12  z22
*******************************************************************************/
double Table_Interp2d(double x, double y,
  double x1, double y1,
  double x2, double y2,
  double z11, double z12, double z21, double z22)
{
  double ratio_x, ratio_y;
  if (x2 == x1) return Table_Interp1d(y, y1,z11, y2,z12);
  if (y1 == y2) return Table_Interp1d(x, x1,z11, x2,z21);

  ratio_y = (y - y1)/(y2 - y1);
  ratio_x = (x - x1)/(x2 - x1);
  return (1-ratio_x)*(1-ratio_y)*z11 + ratio_x*(1-ratio_y)*z21
    + ratio_x*ratio_y*z22         + (1-ratio_x)*ratio_y*z12;
} /* Table_Interp2d */

/* end of read_table-lib.c */

/*****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/ref-lib.h
*
* %Identification
* Written by: Peter Christiansen
* Date: August, 2006
* Origin: RISOE
* Release: McStas 1.10
* Version: $Revision$
*
* Commonly used reflection functions are declared in this file which
* are used by some guide and mirror components.
*
* Depends on read_table-lib
*
* Usage: within SHARE
* %include "ref-lib"
*
****************************************************************************/


#ifndef REF_LIB_H
#define REF_LIB_H "$Revision$"

void StdReflecFunc(double, double*, double*);
void TableReflecFunc(double, t_Table*, double*);

#endif

/* end of ref-lib.h */
/****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/ref-lib.c
*
* %Identification
* Written by: Peter Christiansen
* Date: August, 2006
* Origin: RISOE
* Release: McStas 1.10
* Version: $Revision$
*
* Commonly used reflection functions are declared in this file which
* are used by some guide and mirror components.
*
* Variable names have prefix 'mc_ref_' for 'McStas Reflection'
* to avoid conflicts
*
* Usage: within SHARE
* %include "ref-lib"
*
****************************************************************************/

#ifndef REF_LIB_H
#include "ref-lib.h"
#endif

#ifndef READ_TABLE_LIB_H
#include "read_table-lib.h"
#include "read_table-lib.c"
#endif

/****************************************************************************
* void StdReflecFunc(double q, double *par, double *r)
*
* The McStas standard analytic parametrization of the reflectivity.
* The parameters are:
* R0:      [1]    Low-angle reflectivity
* Qc:      [AA-1] Critical scattering vector
* alpha:   [AA]   Slope of reflectivity
* m:       [1]    m-value of material. Zero means completely absorbing.
* W:       [AA-1] Width of supermirror cut-off
*****************************************************************************/
#pragma acc routine seq
void StdReflecFunc(double mc_pol_q, double *mc_pol_par, double *mc_pol_r) {
    double R0    = mc_pol_par[0];
    double Qc    = mc_pol_par[1];
    double alpha = mc_pol_par[2];
    double m     = mc_pol_par[3];
    double W     = mc_pol_par[4];
    double beta  = 0;
    mc_pol_q     = fabs(mc_pol_q);
    double arg;

    /* Simpler parametrization from Henrik Jacobsen uses these values that depend on m only.
       double m_value=m*0.9853+0.1978;
       double W=-0.0002*m_value+0.0022;
       double alpha=0.2304*m_value+5.0944;
       double beta=-7.6251*m_value+68.1137;
       If W and alpha are set to 0, use Henrik's approach for estimating these parameters
       and apply the formulation:
       arg = R0*0.5*(1-tanh(arg))*(1-alpha*(q-Qc)+beta*(q-Qc)*(q-Qc));
    */
    if (W==0 && alpha==0) {
      m=m*0.9853+0.1978;
      W=-0.0002*m+0.0022;
      alpha=0.2304*m+5.0944;
      beta=-7.6251*m+68.1137;
      if (m<=3) {
	alpha=m;
	beta=0;
      }
    }

    arg = W > 0 ? (mc_pol_q - m*Qc)/W : 11;

    if (arg > 10 || m <= 0 || Qc <=0 || R0 <= 0) {
      *mc_pol_r = 0;
      return;
    }

    if (m < 1) { Qc *= m; m=1; }

    if(mc_pol_q <= Qc) {
      *mc_pol_r = R0;
      return;
    }


    *mc_pol_r = R0*0.5*(1 - tanh(arg))*(1 - alpha*(mc_pol_q - Qc) + beta*(mc_pol_q - Qc)*(mc_pol_q - Qc));

    return;
  }

/****************************************************************************
* void TableReflecFunc(double q, t_Table *par, double *r) {
*
* Looks up the reflectivity in a table using the routines in read_table-lib.
*****************************************************************************/
#pragma acc routine seq
void TableReflecFunc(double mc_pol_q, t_Table *mc_pol_par, double *mc_pol_r) {

  *mc_pol_r = Table_Value(*mc_pol_par, mc_pol_q, 1);
  if(*mc_pol_r>1)
    *mc_pol_r = 1;
  return;
}

/* end of ref-lib.c */


/* Shared user declarations for all components types 'Monochromator_flat'. */
#ifndef GAUSS
/* Define these arrays only once for all instances. */
/* Values for Gauss quadrature. Taken from Brice Carnahan, H. A. Luther and
James O Wilkes, "Applied numerical methods", Wiley, 1969, page 103.
This reference is available from the Copenhagen UB2 library */
double Gauss_X[] = {-0.987992518020485, -0.937273392400706, -0.848206583410427,
-0.724417731360170, -0.570972172608539, -0.394151347077563,
-0.201194093997435, 0, 0.201194093997435,
0.394151347077563, 0.570972172608539, 0.724417731360170,
0.848206583410427, 0.937273392400706, 0.987992518020485};
double Gauss_W[] = {0.030753241996117, 0.070366047488108, 0.107159220467172,
0.139570677926154, 0.166269205816994, 0.186161000115562,
0.198431485327111, 0.202578241925561, 0.198431485327111,
0.186161000115562, 0.166269205816994, 0.139570677926154,
0.107159220467172, 0.070366047488108, 0.030753241996117};
#pragma acc declare create ( Gauss_X )
#pragma acc declare create ( Gauss_W )

#define GAUSS(x,mean,rms) \
  (exp(-((x)-(mean))*((x)-(mean))/(2*(rms)*(rms)))/(sqrt(2*PI)*(rms)))
#endif

/* Shared user declarations for all components types 'V_sample'. */
struct StructVarsV
{
double  sigma_a; /* Absorption cross section per atom (barns) */
    double  sigma_i; /* Incoherent scattering cross section per atom (barns) */
    double  rho;     /* Density of atoms (AA-3) */
    double  my_s;
    double  my_a_v;
    int     shapetyp;    /* 0 double well cylynder, 1 box,  3 sphere */
    double  distance;    /* when non zero, gives rect target distance */
    double  aw,ah;       /* rectangular angular dimensions */
    double  xw,yh;       /* rectangular metrical dimensions */
    double  tx,ty,tz;    /* target coords */
  };



/* ************************************************************************** */
/*             End of SHARE user declarations for all components              */
/* ************************************************************************** */


/* ********************** component definition declarations. **************** */

/* component Origin=Progress_bar() [1] DECLARE */
/* Parameter definition for component type 'Progress_bar' */
struct _struct_Progress_bar_parameters {
  /* Component type 'Progress_bar' setting parameters */
  char profile[16384];
  MCNUM percent;
  MCNUM flag_save;
  MCNUM minutes;
  /* Component type 'Progress_bar' private parameters */
  double  IntermediateCnts;
  time_t  StartTime;
  time_t  EndTime;
  time_t  CurrentTime;
}; /* _struct_Progress_bar_parameters */
typedef struct _struct_Progress_bar_parameters _class_Progress_bar_parameters;

/* Parameters for component type 'Progress_bar' */
struct _struct_Progress_bar {
  char     _name[256]; /* e.g. Origin */
  char     _type[256]; /* Progress_bar */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Progress_bar_parameters _parameters;
};
typedef struct _struct_Progress_bar _class_Progress_bar;
_class_Progress_bar _Origin_var;
#pragma acc declare create ( _Origin_var )

/* component Source=Source_simple() [2] DECLARE */
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
  double  pmul;
  double  srcArea;
  int  square;
  double  tx;
  double  ty;
  double  tz;
}; /* _struct_Source_simple_parameters */
typedef struct _struct_Source_simple_parameters _class_Source_simple_parameters;

/* Parameters for component type 'Source_simple' */
struct _struct_Source_simple {
  char     _name[256]; /* e.g. Source */
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
_class_Source_simple _Source_var;
#pragma acc declare create ( _Source_var )

/* component D0_Source=PSD_monitor() [3] DECLARE */
/* Parameter definition for component type 'PSD_monitor' */
struct _struct_PSD_monitor_parameters {
  /* Component type 'PSD_monitor' setting parameters */
  MCNUM nx;
  MCNUM ny;
  char filename[16384];
  MCNUM xmin;
  MCNUM xmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM xwidth;
  MCNUM yheight;
  MCNUM restore_neutron;
  long nowritefile;
  /* Component type 'PSD_monitor' private parameters */
  DArray2d  PSD_N;
  DArray2d  PSD_p;
  DArray2d  PSD_p2;
}; /* _struct_PSD_monitor_parameters */
typedef struct _struct_PSD_monitor_parameters _class_PSD_monitor_parameters;

/* Parameters for component type 'PSD_monitor' */
struct _struct_PSD_monitor {
  char     _name[256]; /* e.g. D0_Source */
  char     _type[256]; /* PSD_monitor */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_PSD_monitor_parameters _parameters;
};
typedef struct _struct_PSD_monitor _class_PSD_monitor;
_class_PSD_monitor _D0_Source_var;
#pragma acc declare create ( _D0_Source_var )

/* component SC1=Guide() [4] DECLARE */
/* Parameter definition for component type 'Guide' */
struct _struct_Guide_parameters {
  /* Component type 'Guide' setting parameters */
  char reflect[16384];
  MCNUM w1;
  MCNUM h1;
  MCNUM w2;
  MCNUM h2;
  MCNUM l;
  MCNUM R0;
  MCNUM Qc;
  MCNUM alpha;
  MCNUM m;
  MCNUM W;
  /* Component type 'Guide' private parameters */
  t_Table  pTable;
  int  table_present;
}; /* _struct_Guide_parameters */
typedef struct _struct_Guide_parameters _class_Guide_parameters;

/* Parameters for component type 'Guide' */
struct _struct_Guide {
  char     _name[256]; /* e.g. SC1 */
  char     _type[256]; /* Guide */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Guide_parameters _parameters;
};
typedef struct _struct_Guide _class_Guide;
_class_Guide _SC1_var;
#pragma acc declare create ( _SC1_var )

_class_PSD_monitor _D1_SC1_Out_var;
#pragma acc declare create ( _D1_SC1_Out_var )

/* component As1=Slit() [6] DECLARE */
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
  char     _name[256]; /* e.g. As1 */
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
_class_Slit _As1_var;
#pragma acc declare create ( _As1_var )

_class_Slit _As2_var;
#pragma acc declare create ( _As2_var )

_class_Slit _As3_var;
#pragma acc declare create ( _As3_var )

_class_Slit _As4_var;
#pragma acc declare create ( _As4_var )

_class_PSD_monitor _D2_A4_var;
#pragma acc declare create ( _D2_A4_var )

/* component Mono_Cradle=Arm() [11] DECLARE */
/* Parameter definition for component type 'Arm' */
struct _struct_Arm_parameters {
  char Arm_has_no_parameters;
}; /* _struct_Arm_parameters */
typedef struct _struct_Arm_parameters _class_Arm_parameters;

/* Parameters for component type 'Arm' */
struct _struct_Arm {
  char     _name[256]; /* e.g. Mono_Cradle */
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
_class_Arm _Mono_Cradle_var;
#pragma acc declare create ( _Mono_Cradle_var )

/* component PG1Xtal=Monochromator_flat() [12] DECLARE */
/* Parameter definition for component type 'Monochromator_flat' */
struct _struct_Monochromator_flat_parameters {
  /* Component type 'Monochromator_flat' setting parameters */
  MCNUM zmin;
  MCNUM zmax;
  MCNUM ymin;
  MCNUM ymax;
  MCNUM zwidth;
  MCNUM yheight;
  MCNUM mosaich;
  MCNUM mosaicv;
  MCNUM r0;
  MCNUM Q;
  MCNUM DM;
  /* Component type 'Monochromator_flat' private parameters */
  double  mos_rms_y;
  double  mos_rms_z;
  double  mos_rms_max;
  double  mono_Q;
}; /* _struct_Monochromator_flat_parameters */
typedef struct _struct_Monochromator_flat_parameters _class_Monochromator_flat_parameters;

/* Parameters for component type 'Monochromator_flat' */
struct _struct_Monochromator_flat {
  char     _name[256]; /* e.g. PG1Xtal */
  char     _type[256]; /* Monochromator_flat */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_Monochromator_flat_parameters _parameters;
};
typedef struct _struct_Monochromator_flat _class_Monochromator_flat;
_class_Monochromator_flat _PG1Xtal_var;
#pragma acc declare create ( _PG1Xtal_var )

_class_Arm _Mono_Out_var;
#pragma acc declare create ( _Mono_Out_var )

_class_PSD_monitor _D4_SC2_In_var;
#pragma acc declare create ( _D4_SC2_In_var )

_class_Guide _SC2_var;
#pragma acc declare create ( _SC2_var )

_class_PSD_monitor _D5_SC2_Out_var;
#pragma acc declare create ( _D5_SC2_Out_var )

_class_Arm _Sample_Cradle_var;
#pragma acc declare create ( _Sample_Cradle_var )

_class_Arm _Sample_Out_var;
#pragma acc declare create ( _Sample_Out_var )

/* component Sample=V_sample() [19] DECLARE */
/* Parameter definition for component type 'V_sample' */
struct _struct_V_sample_parameters {
  /* Component type 'V_sample' setting parameters */
  MCNUM radius;
  MCNUM thickness;
  MCNUM zdepth;
  MCNUM Vc;
  MCNUM sigma_abs;
  MCNUM sigma_inc;
  MCNUM radius_i;
  MCNUM radius_o;
  MCNUM h;
  MCNUM focus_r;
  MCNUM pack;
  MCNUM frac;
  MCNUM f_QE;
  MCNUM gamma;
  MCNUM target_x;
  MCNUM target_y;
  MCNUM target_z;
  MCNUM focus_xw;
  MCNUM focus_yh;
  MCNUM focus_aw;
  MCNUM focus_ah;
  MCNUM xwidth;
  MCNUM yheight;
  MCNUM zthick;
  MCNUM rad_sphere;
  MCNUM sig_a;
  MCNUM sig_i;
  MCNUM V0;
  long target_index;
  MCNUM multiples;
  /* Component type 'V_sample' private parameters */
  struct StructVarsV  VarsV;
}; /* _struct_V_sample_parameters */
typedef struct _struct_V_sample_parameters _class_V_sample_parameters;

/* Parameters for component type 'V_sample' */
struct _struct_V_sample {
  char     _name[256]; /* e.g. Sample */
  char     _type[256]; /* V_sample */
  long     _index; /* e.g. 2 index in TRACE list */
  Coords   _position_absolute;
  Coords   _position_relative; /* wrt PREVIOUS */
  Rotation _rotation_absolute;
  Rotation _rotation_relative; /* wrt PREVIOUS */
  int      _rotation_is_identity;
  _class_V_sample_parameters _parameters;
};
typedef struct _struct_V_sample _class_V_sample;
_class_V_sample _Sample_var;
#pragma acc declare create ( _Sample_var )

_class_PSD_monitor _D7_SC3_In_var;
#pragma acc declare create ( _D7_SC3_In_var )

_class_Guide _SC3_var;
#pragma acc declare create ( _SC3_var )

_class_PSD_monitor _D8_SC3_Out_var;
#pragma acc declare create ( _D8_SC3_Out_var )

_class_Arm _Ana_Cradle_var;
#pragma acc declare create ( _Ana_Cradle_var )

_class_Monochromator_flat _PG2Xtal_var;
#pragma acc declare create ( _PG2Xtal_var )

_class_Arm _Ana_Out_var;
#pragma acc declare create ( _Ana_Out_var )

_class_PSD_monitor _D10_SC4_In_var;
#pragma acc declare create ( _D10_SC4_In_var )

_class_Guide _SC4_var;
#pragma acc declare create ( _SC4_var )

_class_PSD_monitor _He3H_var;
#pragma acc declare create ( _He3H_var )

int mcNUMCOMP = 28;

/* User declarations from instrument definition. Can define functions. */
  double DM         = 3.3539;   /* Monochromator d-spacing in Angs */
                                /* PG002 Orders : 1st 3.355 2e 1.6775, 3e 1.1183 */

/* to compute */
  double A1,A2;
  double A3,A4;
  double A5,A6;
  double mono_q, Ei;

#undef compcurname
#undef compcurtype
#undef compcurindex
/* end of instrument 'BNL_H8' and components DECLARE */

/* *****************************************************************************
* instrument 'BNL_H8' and components INITIALISE
***************************************************************************** */

/* component Origin=Progress_bar() SETTING, POSITION/ROTATION */
int _Origin_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Origin_setpos] component Origin=Progress_bar() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../misc/Progress_bar.comp:57]");
  stracpy(_Origin_var._name, "Origin", 16384);
  stracpy(_Origin_var._type, "Progress_bar", 16384);
  _Origin_var._index=1;
  if("NULL" && strlen("NULL"))
    stracpy(_Origin_var._parameters.profile, "NULL" ? "NULL" : "", 16384);
  else 
  _Origin_var._parameters.profile[0]='\0';
  _Origin_var._parameters.percent = 10;
  _Origin_var._parameters.flag_save = 0;
  _Origin_var._parameters.minutes = 0;


  /* component Origin=Progress_bar() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(_Origin_var._rotation_absolute,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_copy(_Origin_var._rotation_relative, _Origin_var._rotation_absolute);
    _Origin_var._rotation_is_identity =  rot_test_identity(_Origin_var._rotation_relative);
    _Origin_var._position_absolute = coords_set(
      0, 0, 0);
    tc1 = coords_neg(_Origin_var._position_absolute);
    _Origin_var._position_relative = rot_apply(_Origin_var._rotation_absolute, tc1);
  } /* Origin=Progress_bar() AT ROTATED */
  DEBUG_COMPONENT("Origin", _Origin_var._position_absolute, _Origin_var._rotation_absolute);
  instrument->_position_absolute[1] = _Origin_var._position_absolute;
  instrument->_position_relative[1] = _Origin_var._position_relative;
  instrument->counter_N[1]  = instrument->counter_P[1] = instrument->counter_P2[1] = 0;
  instrument->counter_AbsorbProp[1]= 0;
  return(0);
} /* _Origin_setpos */

/* component Source=Source_simple() SETTING, POSITION/ROTATION */
int _Source_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Source_setpos] component Source=Source_simple() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../sources/Source_simple.comp:68]");
  stracpy(_Source_var._name, "Source", 16384);
  stracpy(_Source_var._type, "Source_simple", 16384);
  _Source_var._index=2;
  _Source_var._parameters.radius = 0.10;
  _Source_var._parameters.yheight = 0;
  _Source_var._parameters.xwidth = 0;
  _Source_var._parameters.dist = 2.7473;
  _Source_var._parameters.focus_xw = 0.031;
  _Source_var._parameters.focus_yh = 0.054;
  _Source_var._parameters.E0 = Ei;
  _Source_var._parameters.dE = 0.03 * Ei;
  _Source_var._parameters.lambda0 = 0;
  _Source_var._parameters.dlambda = 0;
  _Source_var._parameters.flux = 1;
  _Source_var._parameters.gauss = 0;
  _Source_var._parameters.target_index = + 1;


  /* component Source=Source_simple() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(_Source_var._rotation_absolute,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_transpose(_Origin_var._rotation_absolute, tr1);
    rot_mul(_Source_var._rotation_absolute, tr1, _Source_var._rotation_relative);
    _Source_var._rotation_is_identity =  rot_test_identity(_Source_var._rotation_relative);
    _Source_var._position_absolute = coords_set(
      0, 0, 0);
    tc1 = coords_sub(_Origin_var._position_absolute, _Source_var._position_absolute);
    _Source_var._position_relative = rot_apply(_Source_var._rotation_absolute, tc1);
  } /* Source=Source_simple() AT ROTATED */
  DEBUG_COMPONENT("Source", _Source_var._position_absolute, _Source_var._rotation_absolute);
  instrument->_position_absolute[2] = _Source_var._position_absolute;
  instrument->_position_relative[2] = _Source_var._position_relative;
  instrument->counter_N[2]  = instrument->counter_P[2] = instrument->counter_P2[2] = 0;
  instrument->counter_AbsorbProp[2]= 0;
  return(0);
} /* _Source_setpos */

/* component D0_Source=PSD_monitor() SETTING, POSITION/ROTATION */
int _D0_Source_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D0_Source_setpos] component D0_Source=PSD_monitor() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");
  stracpy(_D0_Source_var._name, "D0_Source", 16384);
  stracpy(_D0_Source_var._type, "PSD_monitor", 16384);
  _D0_Source_var._index=3;
  _D0_Source_var._parameters.nx = 20;
  _D0_Source_var._parameters.ny = 20;
  if("D0_Source.psd" && strlen("D0_Source.psd"))
    stracpy(_D0_Source_var._parameters.filename, "D0_Source.psd" ? "D0_Source.psd" : "", 16384);
  else 
  _D0_Source_var._parameters.filename[0]='\0';
  _D0_Source_var._parameters.xmin = -0.05;
  _D0_Source_var._parameters.xmax = 0.05;
  _D0_Source_var._parameters.ymin = -0.05;
  _D0_Source_var._parameters.ymax = 0.05;
  _D0_Source_var._parameters.xwidth = 0.03;
  _D0_Source_var._parameters.yheight = 0.054;
  _D0_Source_var._parameters.restore_neutron = 0;
  _D0_Source_var._parameters.nowritefile = 0;


  /* component D0_Source=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _D0_Source_var._rotation_absolute);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    rot_mul(_D0_Source_var._rotation_absolute, tr1, _D0_Source_var._rotation_relative);
    _D0_Source_var._rotation_is_identity =  rot_test_identity(_D0_Source_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.0001);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D0_Source_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_Source_var._position_absolute, _D0_Source_var._position_absolute);
    _D0_Source_var._position_relative = rot_apply(_D0_Source_var._rotation_absolute, tc1);
  } /* D0_Source=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D0_Source", _D0_Source_var._position_absolute, _D0_Source_var._rotation_absolute);
  instrument->_position_absolute[3] = _D0_Source_var._position_absolute;
  instrument->_position_relative[3] = _D0_Source_var._position_relative;
  instrument->counter_N[3]  = instrument->counter_P[3] = instrument->counter_P2[3] = 0;
  instrument->counter_AbsorbProp[3]= 0;
  return(0);
} /* _D0_Source_setpos */

/* component SC1=Guide() SETTING, POSITION/ROTATION */
int _SC1_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_SC1_setpos] component SC1=Guide() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Guide.comp:74]");
  stracpy(_SC1_var._name, "SC1", 16384);
  stracpy(_SC1_var._type, "Guide", 16384);
  _SC1_var._index=4;
  _SC1_var._parameters.reflect[0]='\0';
  _SC1_var._parameters.w1 = 0.031;
  _SC1_var._parameters.h1 = 0.054;
  _SC1_var._parameters.w2 = 0;
  _SC1_var._parameters.h2 = 0;
  _SC1_var._parameters.l = 0.9144;
  _SC1_var._parameters.R0 = 1.0;
  _SC1_var._parameters.Qc = 0.021;
  _SC1_var._parameters.alpha = 6;
  _SC1_var._parameters.m = 1;
  _SC1_var._parameters.W = 0.0003;


  /* component SC1=Guide() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _SC1_var._rotation_absolute);
    rot_transpose(_D0_Source_var._rotation_absolute, tr1);
    rot_mul(_SC1_var._rotation_absolute, tr1, _SC1_var._rotation_relative);
    _SC1_var._rotation_is_identity =  rot_test_identity(_SC1_var._rotation_relative);
    tc1 = coords_set(
      0.0, 0, 2.7473);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _SC1_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_D0_Source_var._position_absolute, _SC1_var._position_absolute);
    _SC1_var._position_relative = rot_apply(_SC1_var._rotation_absolute, tc1);
  } /* SC1=Guide() AT ROTATED */
  DEBUG_COMPONENT("SC1", _SC1_var._position_absolute, _SC1_var._rotation_absolute);
  instrument->_position_absolute[4] = _SC1_var._position_absolute;
  instrument->_position_relative[4] = _SC1_var._position_relative;
  instrument->counter_N[4]  = instrument->counter_P[4] = instrument->counter_P2[4] = 0;
  instrument->counter_AbsorbProp[4]= 0;
  return(0);
} /* _SC1_setpos */

/* component D1_SC1_Out=PSD_monitor() SETTING, POSITION/ROTATION */
int _D1_SC1_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D1_SC1_Out_setpos] component D1_SC1_Out=PSD_monitor() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");
  stracpy(_D1_SC1_Out_var._name, "D1_SC1_Out", 16384);
  stracpy(_D1_SC1_Out_var._type, "PSD_monitor", 16384);
  _D1_SC1_Out_var._index=5;
  _D1_SC1_Out_var._parameters.nx = 20;
  _D1_SC1_Out_var._parameters.ny = 20;
  if("D1_SC1_Out.psd" && strlen("D1_SC1_Out.psd"))
    stracpy(_D1_SC1_Out_var._parameters.filename, "D1_SC1_Out.psd" ? "D1_SC1_Out.psd" : "", 16384);
  else 
  _D1_SC1_Out_var._parameters.filename[0]='\0';
  _D1_SC1_Out_var._parameters.xmin = -0.05;
  _D1_SC1_Out_var._parameters.xmax = 0.05;
  _D1_SC1_Out_var._parameters.ymin = -0.05;
  _D1_SC1_Out_var._parameters.ymax = 0.05;
  _D1_SC1_Out_var._parameters.xwidth = 0.03;
  _D1_SC1_Out_var._parameters.yheight = 0.054;
  _D1_SC1_Out_var._parameters.restore_neutron = 0;
  _D1_SC1_Out_var._parameters.nowritefile = 0;


  /* component D1_SC1_Out=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _SC1_var._rotation_absolute, _D1_SC1_Out_var._rotation_absolute);
    rot_transpose(_SC1_var._rotation_absolute, tr1);
    rot_mul(_D1_SC1_Out_var._rotation_absolute, tr1, _D1_SC1_Out_var._rotation_relative);
    _D1_SC1_Out_var._rotation_is_identity =  rot_test_identity(_D1_SC1_Out_var._rotation_relative);
    tc1 = coords_set(
      0.0, 0, 0.9145);
    rot_transpose(_SC1_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D1_SC1_Out_var._position_absolute = coords_add(_SC1_var._position_absolute, tc2);
    tc1 = coords_sub(_SC1_var._position_absolute, _D1_SC1_Out_var._position_absolute);
    _D1_SC1_Out_var._position_relative = rot_apply(_D1_SC1_Out_var._rotation_absolute, tc1);
  } /* D1_SC1_Out=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D1_SC1_Out", _D1_SC1_Out_var._position_absolute, _D1_SC1_Out_var._rotation_absolute);
  instrument->_position_absolute[5] = _D1_SC1_Out_var._position_absolute;
  instrument->_position_relative[5] = _D1_SC1_Out_var._position_relative;
  instrument->counter_N[5]  = instrument->counter_P[5] = instrument->counter_P2[5] = 0;
  instrument->counter_AbsorbProp[5]= 0;
  return(0);
} /* _D1_SC1_Out_setpos */

/* component As1=Slit() SETTING, POSITION/ROTATION */
int _As1_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_As1_setpos] component As1=Slit() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Slit.comp:50]");
  stracpy(_As1_var._name, "As1", 16384);
  stracpy(_As1_var._type, "Slit", 16384);
  _As1_var._index=6;
  _As1_var._parameters.xmin = 0;
  _As1_var._parameters.xmax = 0;
  _As1_var._parameters.ymin = 0;
  _As1_var._parameters.ymax = 0;
  _As1_var._parameters.radius = 0;
  _As1_var._parameters.xwidth = 0.04450;
  _As1_var._parameters.yheight = 0.0635;

  /* component As1=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _As1_var._rotation_absolute);
    rot_transpose(_D1_SC1_Out_var._rotation_absolute, tr1);
    rot_mul(_As1_var._rotation_absolute, tr1, _As1_var._rotation_relative);
    _As1_var._rotation_is_identity =  rot_test_identity(_As1_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 3.6998);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _As1_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_D1_SC1_Out_var._position_absolute, _As1_var._position_absolute);
    _As1_var._position_relative = rot_apply(_As1_var._rotation_absolute, tc1);
  } /* As1=Slit() AT ROTATED */
  DEBUG_COMPONENT("As1", _As1_var._position_absolute, _As1_var._rotation_absolute);
  instrument->_position_absolute[6] = _As1_var._position_absolute;
  instrument->_position_relative[6] = _As1_var._position_relative;
  instrument->counter_N[6]  = instrument->counter_P[6] = instrument->counter_P2[6] = 0;
  instrument->counter_AbsorbProp[6]= 0;
  return(0);
} /* _As1_setpos */

/* component As2=Slit() SETTING, POSITION/ROTATION */
int _As2_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_As2_setpos] component As2=Slit() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Slit.comp:50]");
  stracpy(_As2_var._name, "As2", 16384);
  stracpy(_As2_var._type, "Slit", 16384);
  _As2_var._index=7;
  _As2_var._parameters.xmin = 0;
  _As2_var._parameters.xmax = 0;
  _As2_var._parameters.ymin = 0;
  _As2_var._parameters.ymax = 0;
  _As2_var._parameters.radius = 0;
  _As2_var._parameters.xwidth = 0.04450;
  _As2_var._parameters.yheight = 0.0635;

  /* component As2=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _As2_var._rotation_absolute);
    rot_transpose(_As1_var._rotation_absolute, tr1);
    rot_mul(_As2_var._rotation_absolute, tr1, _As2_var._rotation_relative);
    _As2_var._rotation_is_identity =  rot_test_identity(_As2_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 4.0808);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _As2_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_As1_var._position_absolute, _As2_var._position_absolute);
    _As2_var._position_relative = rot_apply(_As2_var._rotation_absolute, tc1);
  } /* As2=Slit() AT ROTATED */
  DEBUG_COMPONENT("As2", _As2_var._position_absolute, _As2_var._rotation_absolute);
  instrument->_position_absolute[7] = _As2_var._position_absolute;
  instrument->_position_relative[7] = _As2_var._position_relative;
  instrument->counter_N[7]  = instrument->counter_P[7] = instrument->counter_P2[7] = 0;
  instrument->counter_AbsorbProp[7]= 0;
  return(0);
} /* _As2_setpos */

/* component As3=Slit() SETTING, POSITION/ROTATION */
int _As3_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_As3_setpos] component As3=Slit() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Slit.comp:50]");
  stracpy(_As3_var._name, "As3", 16384);
  stracpy(_As3_var._type, "Slit", 16384);
  _As3_var._index=8;
  _As3_var._parameters.xmin = 0;
  _As3_var._parameters.xmax = 0;
  _As3_var._parameters.ymin = 0;
  _As3_var._parameters.ymax = 0;
  _As3_var._parameters.radius = 0;
  _As3_var._parameters.xwidth = 0.04450;
  _As3_var._parameters.yheight = 0.0635;

  /* component As3=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _As3_var._rotation_absolute);
    rot_transpose(_As2_var._rotation_absolute, tr1);
    rot_mul(_As3_var._rotation_absolute, tr1, _As3_var._rotation_relative);
    _As3_var._rotation_is_identity =  rot_test_identity(_As3_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 4.1189);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _As3_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_As2_var._position_absolute, _As3_var._position_absolute);
    _As3_var._position_relative = rot_apply(_As3_var._rotation_absolute, tc1);
  } /* As3=Slit() AT ROTATED */
  DEBUG_COMPONENT("As3", _As3_var._position_absolute, _As3_var._rotation_absolute);
  instrument->_position_absolute[8] = _As3_var._position_absolute;
  instrument->_position_relative[8] = _As3_var._position_relative;
  instrument->counter_N[8]  = instrument->counter_P[8] = instrument->counter_P2[8] = 0;
  instrument->counter_AbsorbProp[8]= 0;
  return(0);
} /* _As3_setpos */

/* component As4=Slit() SETTING, POSITION/ROTATION */
int _As4_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_As4_setpos] component As4=Slit() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Slit.comp:50]");
  stracpy(_As4_var._name, "As4", 16384);
  stracpy(_As4_var._type, "Slit", 16384);
  _As4_var._index=9;
  _As4_var._parameters.xmin = 0;
  _As4_var._parameters.xmax = 0;
  _As4_var._parameters.ymin = 0;
  _As4_var._parameters.ymax = 0;
  _As4_var._parameters.radius = 0;
  _As4_var._parameters.xwidth = 0.04450;
  _As4_var._parameters.yheight = 0.0635;

  /* component As4=Slit() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _As4_var._rotation_absolute);
    rot_transpose(_As3_var._rotation_absolute, tr1);
    rot_mul(_As4_var._rotation_absolute, tr1, _As4_var._rotation_relative);
    _As4_var._rotation_is_identity =  rot_test_identity(_As4_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 4.4141);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _As4_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_As3_var._position_absolute, _As4_var._position_absolute);
    _As4_var._position_relative = rot_apply(_As4_var._rotation_absolute, tc1);
  } /* As4=Slit() AT ROTATED */
  DEBUG_COMPONENT("As4", _As4_var._position_absolute, _As4_var._rotation_absolute);
  instrument->_position_absolute[9] = _As4_var._position_absolute;
  instrument->_position_relative[9] = _As4_var._position_relative;
  instrument->counter_N[9]  = instrument->counter_P[9] = instrument->counter_P2[9] = 0;
  instrument->counter_AbsorbProp[9]= 0;
  return(0);
} /* _As4_setpos */

/* component D2_A4=PSD_monitor() SETTING, POSITION/ROTATION */
int _D2_A4_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D2_A4_setpos] component D2_A4=PSD_monitor() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");
  stracpy(_D2_A4_var._name, "D2_A4", 16384);
  stracpy(_D2_A4_var._type, "PSD_monitor", 16384);
  _D2_A4_var._index=10;
  _D2_A4_var._parameters.nx = 20;
  _D2_A4_var._parameters.ny = 20;
  if("D2_A4.psd" && strlen("D2_A4.psd"))
    stracpy(_D2_A4_var._parameters.filename, "D2_A4.psd" ? "D2_A4.psd" : "", 16384);
  else 
  _D2_A4_var._parameters.filename[0]='\0';
  _D2_A4_var._parameters.xmin = -0.05;
  _D2_A4_var._parameters.xmax = 0.05;
  _D2_A4_var._parameters.ymin = -0.05;
  _D2_A4_var._parameters.ymax = 0.05;
  _D2_A4_var._parameters.xwidth = 0.04450;
  _D2_A4_var._parameters.yheight = 0.0635;
  _D2_A4_var._parameters.restore_neutron = 0;
  _D2_A4_var._parameters.nowritefile = 0;


  /* component D2_A4=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _As4_var._rotation_absolute, _D2_A4_var._rotation_absolute);
    rot_transpose(_As4_var._rotation_absolute, tr1);
    rot_mul(_D2_A4_var._rotation_absolute, tr1, _D2_A4_var._rotation_relative);
    _D2_A4_var._rotation_is_identity =  rot_test_identity(_D2_A4_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.0001);
    rot_transpose(_As4_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D2_A4_var._position_absolute = coords_add(_As4_var._position_absolute, tc2);
    tc1 = coords_sub(_As4_var._position_absolute, _D2_A4_var._position_absolute);
    _D2_A4_var._position_relative = rot_apply(_D2_A4_var._rotation_absolute, tc1);
  } /* D2_A4=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D2_A4", _D2_A4_var._position_absolute, _D2_A4_var._rotation_absolute);
  instrument->_position_absolute[10] = _D2_A4_var._position_absolute;
  instrument->_position_relative[10] = _D2_A4_var._position_relative;
  instrument->counter_N[10]  = instrument->counter_P[10] = instrument->counter_P2[10] = 0;
  instrument->counter_AbsorbProp[10]= 0;
  return(0);
} /* _D2_A4_setpos */

/* component Mono_Cradle=Arm() SETTING, POSITION/ROTATION */
int _Mono_Cradle_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Mono_Cradle_setpos] component Mono_Cradle=Arm() SETTING [Arm:0]");
  stracpy(_Mono_Cradle_var._name, "Mono_Cradle", 16384);
  stracpy(_Mono_Cradle_var._type, "Arm", 16384);
  _Mono_Cradle_var._index=11;
  /* component Mono_Cradle=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A1)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _Mono_Cradle_var._rotation_absolute);
    rot_transpose(_D2_A4_var._rotation_absolute, tr1);
    rot_mul(_Mono_Cradle_var._rotation_absolute, tr1, _Mono_Cradle_var._rotation_relative);
    _Mono_Cradle_var._rotation_is_identity =  rot_test_identity(_Mono_Cradle_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 5.2746);
    rot_transpose(_Source_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Mono_Cradle_var._position_absolute = coords_add(_Source_var._position_absolute, tc2);
    tc1 = coords_sub(_D2_A4_var._position_absolute, _Mono_Cradle_var._position_absolute);
    _Mono_Cradle_var._position_relative = rot_apply(_Mono_Cradle_var._rotation_absolute, tc1);
  } /* Mono_Cradle=Arm() AT ROTATED */
  DEBUG_COMPONENT("Mono_Cradle", _Mono_Cradle_var._position_absolute, _Mono_Cradle_var._rotation_absolute);
  instrument->_position_absolute[11] = _Mono_Cradle_var._position_absolute;
  instrument->_position_relative[11] = _Mono_Cradle_var._position_relative;
  instrument->counter_N[11]  = instrument->counter_P[11] = instrument->counter_P2[11] = 0;
  instrument->counter_AbsorbProp[11]= 0;
  return(0);
} /* _Mono_Cradle_setpos */

/* component PG1Xtal=Monochromator_flat() SETTING, POSITION/ROTATION */
int _PG1Xtal_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PG1Xtal_setpos] component PG1Xtal=Monochromator_flat() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Monochromator_flat.comp:103]");
  stracpy(_PG1Xtal_var._name, "PG1Xtal", 16384);
  stracpy(_PG1Xtal_var._type, "Monochromator_flat", 16384);
  _PG1Xtal_var._index=12;
  _PG1Xtal_var._parameters.zmin = -0.05;
  _PG1Xtal_var._parameters.zmax = 0.05;
  _PG1Xtal_var._parameters.ymin = -0.05;
  _PG1Xtal_var._parameters.ymax = 0.05;
  _PG1Xtal_var._parameters.zwidth = 0.1;
  _PG1Xtal_var._parameters.yheight = 0.08;
  _PG1Xtal_var._parameters.mosaich = 40;
  _PG1Xtal_var._parameters.mosaicv = 40;
  _PG1Xtal_var._parameters.r0 = 0.7;
  _PG1Xtal_var._parameters.Q = mono_q;
  _PG1Xtal_var._parameters.DM = 0;


  /* component PG1Xtal=Monochromator_flat() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Mono_Cradle_var._rotation_absolute, _PG1Xtal_var._rotation_absolute);
    rot_transpose(_Mono_Cradle_var._rotation_absolute, tr1);
    rot_mul(_PG1Xtal_var._rotation_absolute, tr1, _PG1Xtal_var._rotation_relative);
    _PG1Xtal_var._rotation_is_identity =  rot_test_identity(_PG1Xtal_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.0001);
    rot_transpose(_Mono_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PG1Xtal_var._position_absolute = coords_add(_Mono_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_Mono_Cradle_var._position_absolute, _PG1Xtal_var._position_absolute);
    _PG1Xtal_var._position_relative = rot_apply(_PG1Xtal_var._rotation_absolute, tc1);
  } /* PG1Xtal=Monochromator_flat() AT ROTATED */
  DEBUG_COMPONENT("PG1Xtal", _PG1Xtal_var._position_absolute, _PG1Xtal_var._rotation_absolute);
  instrument->_position_absolute[12] = _PG1Xtal_var._position_absolute;
  instrument->_position_relative[12] = _PG1Xtal_var._position_relative;
  instrument->counter_N[12]  = instrument->counter_P[12] = instrument->counter_P2[12] = 0;
  instrument->counter_AbsorbProp[12]= 0;
  return(0);
} /* _PG1Xtal_setpos */

/* component Mono_Out=Arm() SETTING, POSITION/ROTATION */
int _Mono_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Mono_Out_setpos] component Mono_Out=Arm() SETTING [Arm:0]");
  stracpy(_Mono_Out_var._name, "Mono_Out", 16384);
  stracpy(_Mono_Out_var._type, "Arm", 16384);
  _Mono_Out_var._index=13;
  /* component Mono_Out=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A2)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Source_var._rotation_absolute, _Mono_Out_var._rotation_absolute);
    rot_transpose(_PG1Xtal_var._rotation_absolute, tr1);
    rot_mul(_Mono_Out_var._rotation_absolute, tr1, _Mono_Out_var._rotation_relative);
    _Mono_Out_var._rotation_is_identity =  rot_test_identity(_Mono_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.0002);
    rot_transpose(_Mono_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Mono_Out_var._position_absolute = coords_add(_Mono_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_PG1Xtal_var._position_absolute, _Mono_Out_var._position_absolute);
    _Mono_Out_var._position_relative = rot_apply(_Mono_Out_var._rotation_absolute, tc1);
  } /* Mono_Out=Arm() AT ROTATED */
  DEBUG_COMPONENT("Mono_Out", _Mono_Out_var._position_absolute, _Mono_Out_var._rotation_absolute);
  instrument->_position_absolute[13] = _Mono_Out_var._position_absolute;
  instrument->_position_relative[13] = _Mono_Out_var._position_relative;
  instrument->counter_N[13]  = instrument->counter_P[13] = instrument->counter_P2[13] = 0;
  instrument->counter_AbsorbProp[13]= 0;
  return(0);
} /* _Mono_Out_setpos */

/* component D4_SC2_In=PSD_monitor() SETTING, POSITION/ROTATION */
int _D4_SC2_In_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D4_SC2_In_setpos] component D4_SC2_In=PSD_monitor() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");
  stracpy(_D4_SC2_In_var._name, "D4_SC2_In", 16384);
  stracpy(_D4_SC2_In_var._type, "PSD_monitor", 16384);
  _D4_SC2_In_var._index=14;
  _D4_SC2_In_var._parameters.nx = 20;
  _D4_SC2_In_var._parameters.ny = 20;
  if("D4_SC2_In.psd" && strlen("D4_SC2_In.psd"))
    stracpy(_D4_SC2_In_var._parameters.filename, "D4_SC2_In.psd" ? "D4_SC2_In.psd" : "", 16384);
  else 
  _D4_SC2_In_var._parameters.filename[0]='\0';
  _D4_SC2_In_var._parameters.xmin = -0.05;
  _D4_SC2_In_var._parameters.xmax = 0.05;
  _D4_SC2_In_var._parameters.ymin = -0.05;
  _D4_SC2_In_var._parameters.ymax = 0.05;
  _D4_SC2_In_var._parameters.xwidth = 0.0318;
  _D4_SC2_In_var._parameters.yheight = 0.0495;
  _D4_SC2_In_var._parameters.restore_neutron = 0;
  _D4_SC2_In_var._parameters.nowritefile = 0;


  /* component D4_SC2_In=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Mono_Out_var._rotation_absolute, _D4_SC2_In_var._rotation_absolute);
    rot_transpose(_Mono_Out_var._rotation_absolute, tr1);
    rot_mul(_D4_SC2_In_var._rotation_absolute, tr1, _D4_SC2_In_var._rotation_relative);
    _D4_SC2_In_var._rotation_is_identity =  rot_test_identity(_D4_SC2_In_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.2222);
    rot_transpose(_Mono_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D4_SC2_In_var._position_absolute = coords_add(_Mono_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_Mono_Out_var._position_absolute, _D4_SC2_In_var._position_absolute);
    _D4_SC2_In_var._position_relative = rot_apply(_D4_SC2_In_var._rotation_absolute, tc1);
  } /* D4_SC2_In=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D4_SC2_In", _D4_SC2_In_var._position_absolute, _D4_SC2_In_var._rotation_absolute);
  instrument->_position_absolute[14] = _D4_SC2_In_var._position_absolute;
  instrument->_position_relative[14] = _D4_SC2_In_var._position_relative;
  instrument->counter_N[14]  = instrument->counter_P[14] = instrument->counter_P2[14] = 0;
  instrument->counter_AbsorbProp[14]= 0;
  return(0);
} /* _D4_SC2_In_setpos */

/* component SC2=Guide() SETTING, POSITION/ROTATION */
int _SC2_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_SC2_setpos] component SC2=Guide() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Guide.comp:74]");
  stracpy(_SC2_var._name, "SC2", 16384);
  stracpy(_SC2_var._type, "Guide", 16384);
  _SC2_var._index=15;
  _SC2_var._parameters.reflect[0]='\0';
  _SC2_var._parameters.w1 = 0.0318;
  _SC2_var._parameters.h1 = 0.0495;
  _SC2_var._parameters.w2 = 0;
  _SC2_var._parameters.h2 = 0;
  _SC2_var._parameters.l = 0.6096;
  _SC2_var._parameters.R0 = 1.0;
  _SC2_var._parameters.Qc = 0.021;
  _SC2_var._parameters.alpha = 6;
  _SC2_var._parameters.m = 1;
  _SC2_var._parameters.W = 0.0003;


  /* component SC2=Guide() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Mono_Out_var._rotation_absolute, _SC2_var._rotation_absolute);
    rot_transpose(_D4_SC2_In_var._rotation_absolute, tr1);
    rot_mul(_SC2_var._rotation_absolute, tr1, _SC2_var._rotation_relative);
    _SC2_var._rotation_is_identity =  rot_test_identity(_SC2_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.2223);
    rot_transpose(_Mono_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _SC2_var._position_absolute = coords_add(_Mono_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D4_SC2_In_var._position_absolute, _SC2_var._position_absolute);
    _SC2_var._position_relative = rot_apply(_SC2_var._rotation_absolute, tc1);
  } /* SC2=Guide() AT ROTATED */
  DEBUG_COMPONENT("SC2", _SC2_var._position_absolute, _SC2_var._rotation_absolute);
  instrument->_position_absolute[15] = _SC2_var._position_absolute;
  instrument->_position_relative[15] = _SC2_var._position_relative;
  instrument->counter_N[15]  = instrument->counter_P[15] = instrument->counter_P2[15] = 0;
  instrument->counter_AbsorbProp[15]= 0;
  return(0);
} /* _SC2_setpos */

/* component D5_SC2_Out=PSD_monitor() SETTING, POSITION/ROTATION */
int _D5_SC2_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D5_SC2_Out_setpos] component D5_SC2_Out=PSD_monitor() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");
  stracpy(_D5_SC2_Out_var._name, "D5_SC2_Out", 16384);
  stracpy(_D5_SC2_Out_var._type, "PSD_monitor", 16384);
  _D5_SC2_Out_var._index=16;
  _D5_SC2_Out_var._parameters.nx = 20;
  _D5_SC2_Out_var._parameters.ny = 20;
  if("D5_SC2_Out.psd" && strlen("D5_SC2_Out.psd"))
    stracpy(_D5_SC2_Out_var._parameters.filename, "D5_SC2_Out.psd" ? "D5_SC2_Out.psd" : "", 16384);
  else 
  _D5_SC2_Out_var._parameters.filename[0]='\0';
  _D5_SC2_Out_var._parameters.xmin = -0.05;
  _D5_SC2_Out_var._parameters.xmax = 0.05;
  _D5_SC2_Out_var._parameters.ymin = -0.05;
  _D5_SC2_Out_var._parameters.ymax = 0.05;
  _D5_SC2_Out_var._parameters.xwidth = 0.0318;
  _D5_SC2_Out_var._parameters.yheight = 0.0495;
  _D5_SC2_Out_var._parameters.restore_neutron = 0;
  _D5_SC2_Out_var._parameters.nowritefile = 0;


  /* component D5_SC2_Out=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _SC2_var._rotation_absolute, _D5_SC2_Out_var._rotation_absolute);
    rot_transpose(_SC2_var._rotation_absolute, tr1);
    rot_mul(_D5_SC2_Out_var._rotation_absolute, tr1, _D5_SC2_Out_var._rotation_relative);
    _D5_SC2_Out_var._rotation_is_identity =  rot_test_identity(_D5_SC2_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.6097);
    rot_transpose(_SC2_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D5_SC2_Out_var._position_absolute = coords_add(_SC2_var._position_absolute, tc2);
    tc1 = coords_sub(_SC2_var._position_absolute, _D5_SC2_Out_var._position_absolute);
    _D5_SC2_Out_var._position_relative = rot_apply(_D5_SC2_Out_var._rotation_absolute, tc1);
  } /* D5_SC2_Out=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D5_SC2_Out", _D5_SC2_Out_var._position_absolute, _D5_SC2_Out_var._rotation_absolute);
  instrument->_position_absolute[16] = _D5_SC2_Out_var._position_absolute;
  instrument->_position_relative[16] = _D5_SC2_Out_var._position_relative;
  instrument->counter_N[16]  = instrument->counter_P[16] = instrument->counter_P2[16] = 0;
  instrument->counter_AbsorbProp[16]= 0;
  return(0);
} /* _D5_SC2_Out_setpos */

/* component Sample_Cradle=Arm() SETTING, POSITION/ROTATION */
int _Sample_Cradle_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Sample_Cradle_setpos] component Sample_Cradle=Arm() SETTING [Arm:0]");
  stracpy(_Sample_Cradle_var._name, "Sample_Cradle", 16384);
  stracpy(_Sample_Cradle_var._type, "Arm", 16384);
  _Sample_Cradle_var._index=17;
  /* component Sample_Cradle=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A3)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Mono_Out_var._rotation_absolute, _Sample_Cradle_var._rotation_absolute);
    rot_transpose(_D5_SC2_Out_var._rotation_absolute, tr1);
    rot_mul(_Sample_Cradle_var._rotation_absolute, tr1, _Sample_Cradle_var._rotation_relative);
    _Sample_Cradle_var._rotation_is_identity =  rot_test_identity(_Sample_Cradle_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.7811);
    rot_transpose(_D5_SC2_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Sample_Cradle_var._position_absolute = coords_add(_D5_SC2_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D5_SC2_Out_var._position_absolute, _Sample_Cradle_var._position_absolute);
    _Sample_Cradle_var._position_relative = rot_apply(_Sample_Cradle_var._rotation_absolute, tc1);
  } /* Sample_Cradle=Arm() AT ROTATED */
  DEBUG_COMPONENT("Sample_Cradle", _Sample_Cradle_var._position_absolute, _Sample_Cradle_var._rotation_absolute);
  instrument->_position_absolute[17] = _Sample_Cradle_var._position_absolute;
  instrument->_position_relative[17] = _Sample_Cradle_var._position_relative;
  instrument->counter_N[17]  = instrument->counter_P[17] = instrument->counter_P2[17] = 0;
  instrument->counter_AbsorbProp[17]= 0;
  return(0);
} /* _Sample_Cradle_setpos */

/* component Sample_Out=Arm() SETTING, POSITION/ROTATION */
int _Sample_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Sample_Out_setpos] component Sample_Out=Arm() SETTING [Arm:0]");
  stracpy(_Sample_Out_var._name, "Sample_Out", 16384);
  stracpy(_Sample_Out_var._type, "Arm", 16384);
  _Sample_Out_var._index=18;
  /* component Sample_Out=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A4)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Mono_Out_var._rotation_absolute, _Sample_Out_var._rotation_absolute);
    rot_transpose(_Sample_Cradle_var._rotation_absolute, tr1);
    rot_mul(_Sample_Out_var._rotation_absolute, tr1, _Sample_Out_var._rotation_relative);
    _Sample_Out_var._rotation_is_identity =  rot_test_identity(_Sample_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_Sample_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Sample_Out_var._position_absolute = coords_add(_Sample_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_Sample_Cradle_var._position_absolute, _Sample_Out_var._position_absolute);
    _Sample_Out_var._position_relative = rot_apply(_Sample_Out_var._rotation_absolute, tc1);
  } /* Sample_Out=Arm() AT ROTATED */
  DEBUG_COMPONENT("Sample_Out", _Sample_Out_var._position_absolute, _Sample_Out_var._rotation_absolute);
  instrument->_position_absolute[18] = _Sample_Out_var._position_absolute;
  instrument->_position_relative[18] = _Sample_Out_var._position_relative;
  instrument->counter_N[18]  = instrument->counter_P[18] = instrument->counter_P2[18] = 0;
  instrument->counter_AbsorbProp[18]= 0;
  return(0);
} /* _Sample_Out_setpos */

/* component Sample=V_sample() SETTING, POSITION/ROTATION */
int _Sample_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Sample_setpos] component Sample=V_sample() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../obsolete/V_sample.comp:121]");
  stracpy(_Sample_var._name, "Sample", 16384);
  stracpy(_Sample_var._type, "V_sample", 16384);
  _Sample_var._index=19;
  _Sample_var._parameters.radius = 0.0064;
  _Sample_var._parameters.thickness = 0;
  _Sample_var._parameters.zdepth = 0;
  _Sample_var._parameters.Vc = 13.827;
  _Sample_var._parameters.sigma_abs = 5.08;
  _Sample_var._parameters.sigma_inc = 5.08;
  _Sample_var._parameters.radius_i = 0;
  _Sample_var._parameters.radius_o = 0;
  _Sample_var._parameters.h = 0;
  _Sample_var._parameters.focus_r = 0;
  _Sample_var._parameters.pack = 1;
  _Sample_var._parameters.frac = 1;
  _Sample_var._parameters.f_QE = 0;
  _Sample_var._parameters.gamma = 0;
  _Sample_var._parameters.target_x = 0;
  _Sample_var._parameters.target_y = 0;
  _Sample_var._parameters.target_z = 0;
  _Sample_var._parameters.focus_xw = 0.0478;
  _Sample_var._parameters.focus_yh = 0.049;
  _Sample_var._parameters.focus_aw = 0;
  _Sample_var._parameters.focus_ah = 0;
  _Sample_var._parameters.xwidth = 0;
  _Sample_var._parameters.yheight = 0.0254;
  _Sample_var._parameters.zthick = 0;
  _Sample_var._parameters.rad_sphere = 0;
  _Sample_var._parameters.sig_a = 0;
  _Sample_var._parameters.sig_i = 0;
  _Sample_var._parameters.V0 = 0;
  _Sample_var._parameters.target_index = + 1;
  _Sample_var._parameters.multiples = 1;


  /* component Sample=V_sample() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _Sample_var._rotation_absolute);
    rot_transpose(_Sample_Out_var._rotation_absolute, tr1);
    rot_mul(_Sample_var._rotation_absolute, tr1, _Sample_var._rotation_relative);
    _Sample_var._rotation_is_identity =  rot_test_identity(_Sample_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_Sample_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Sample_var._position_absolute = coords_add(_Sample_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_Sample_Out_var._position_absolute, _Sample_var._position_absolute);
    _Sample_var._position_relative = rot_apply(_Sample_var._rotation_absolute, tc1);
  } /* Sample=V_sample() AT ROTATED */
  DEBUG_COMPONENT("Sample", _Sample_var._position_absolute, _Sample_var._rotation_absolute);
  instrument->_position_absolute[19] = _Sample_var._position_absolute;
  instrument->_position_relative[19] = _Sample_var._position_relative;
  instrument->counter_N[19]  = instrument->counter_P[19] = instrument->counter_P2[19] = 0;
  instrument->counter_AbsorbProp[19]= 0;
  return(0);
} /* _Sample_setpos */

/* component D7_SC3_In=PSD_monitor() SETTING, POSITION/ROTATION */
int _D7_SC3_In_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D7_SC3_In_setpos] component D7_SC3_In=PSD_monitor() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");
  stracpy(_D7_SC3_In_var._name, "D7_SC3_In", 16384);
  stracpy(_D7_SC3_In_var._type, "PSD_monitor", 16384);
  _D7_SC3_In_var._index=20;
  _D7_SC3_In_var._parameters.nx = 20;
  _D7_SC3_In_var._parameters.ny = 20;
  if("D7_SC3_In.psd" && strlen("D7_SC3_In.psd"))
    stracpy(_D7_SC3_In_var._parameters.filename, "D7_SC3_In.psd" ? "D7_SC3_In.psd" : "", 16384);
  else 
  _D7_SC3_In_var._parameters.filename[0]='\0';
  _D7_SC3_In_var._parameters.xmin = -0.05;
  _D7_SC3_In_var._parameters.xmax = 0.05;
  _D7_SC3_In_var._parameters.ymin = -0.05;
  _D7_SC3_In_var._parameters.ymax = 0.05;
  _D7_SC3_In_var._parameters.xwidth = 0.0478;
  _D7_SC3_In_var._parameters.yheight = 0.049;
  _D7_SC3_In_var._parameters.restore_neutron = 0;
  _D7_SC3_In_var._parameters.nowritefile = 0;


  /* component D7_SC3_In=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _D7_SC3_In_var._rotation_absolute);
    rot_transpose(_Sample_var._rotation_absolute, tr1);
    rot_mul(_D7_SC3_In_var._rotation_absolute, tr1, _D7_SC3_In_var._rotation_relative);
    _D7_SC3_In_var._rotation_is_identity =  rot_test_identity(_D7_SC3_In_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.2349);
    rot_transpose(_Sample_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D7_SC3_In_var._position_absolute = coords_add(_Sample_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_Sample_var._position_absolute, _D7_SC3_In_var._position_absolute);
    _D7_SC3_In_var._position_relative = rot_apply(_D7_SC3_In_var._rotation_absolute, tc1);
  } /* D7_SC3_In=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D7_SC3_In", _D7_SC3_In_var._position_absolute, _D7_SC3_In_var._rotation_absolute);
  instrument->_position_absolute[20] = _D7_SC3_In_var._position_absolute;
  instrument->_position_relative[20] = _D7_SC3_In_var._position_relative;
  instrument->counter_N[20]  = instrument->counter_P[20] = instrument->counter_P2[20] = 0;
  instrument->counter_AbsorbProp[20]= 0;
  return(0);
} /* _D7_SC3_In_setpos */

/* component SC3=Guide() SETTING, POSITION/ROTATION */
int _SC3_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_SC3_setpos] component SC3=Guide() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Guide.comp:74]");
  stracpy(_SC3_var._name, "SC3", 16384);
  stracpy(_SC3_var._type, "Guide", 16384);
  _SC3_var._index=21;
  _SC3_var._parameters.reflect[0]='\0';
  _SC3_var._parameters.w1 = 0.0478;
  _SC3_var._parameters.h1 = 0.0490;
  _SC3_var._parameters.w2 = 0;
  _SC3_var._parameters.h2 = 0;
  _SC3_var._parameters.l = 0.3048;
  _SC3_var._parameters.R0 = 1.0;
  _SC3_var._parameters.Qc = 0.021;
  _SC3_var._parameters.alpha = 6;
  _SC3_var._parameters.m = 1;
  _SC3_var._parameters.W = 0.0003;


  /* component SC3=Guide() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _SC3_var._rotation_absolute);
    rot_transpose(_D7_SC3_In_var._rotation_absolute, tr1);
    rot_mul(_SC3_var._rotation_absolute, tr1, _SC3_var._rotation_relative);
    _SC3_var._rotation_is_identity =  rot_test_identity(_SC3_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.2350);
    rot_transpose(_Sample_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _SC3_var._position_absolute = coords_add(_Sample_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D7_SC3_In_var._position_absolute, _SC3_var._position_absolute);
    _SC3_var._position_relative = rot_apply(_SC3_var._rotation_absolute, tc1);
  } /* SC3=Guide() AT ROTATED */
  DEBUG_COMPONENT("SC3", _SC3_var._position_absolute, _SC3_var._rotation_absolute);
  instrument->_position_absolute[21] = _SC3_var._position_absolute;
  instrument->_position_relative[21] = _SC3_var._position_relative;
  instrument->counter_N[21]  = instrument->counter_P[21] = instrument->counter_P2[21] = 0;
  instrument->counter_AbsorbProp[21]= 0;
  return(0);
} /* _SC3_setpos */

/* component D8_SC3_Out=PSD_monitor() SETTING, POSITION/ROTATION */
int _D8_SC3_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D8_SC3_Out_setpos] component D8_SC3_Out=PSD_monitor() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");
  stracpy(_D8_SC3_Out_var._name, "D8_SC3_Out", 16384);
  stracpy(_D8_SC3_Out_var._type, "PSD_monitor", 16384);
  _D8_SC3_Out_var._index=22;
  _D8_SC3_Out_var._parameters.nx = 20;
  _D8_SC3_Out_var._parameters.ny = 20;
  if("D8_SC3_Out.psd" && strlen("D8_SC3_Out.psd"))
    stracpy(_D8_SC3_Out_var._parameters.filename, "D8_SC3_Out.psd" ? "D8_SC3_Out.psd" : "", 16384);
  else 
  _D8_SC3_Out_var._parameters.filename[0]='\0';
  _D8_SC3_Out_var._parameters.xmin = -0.05;
  _D8_SC3_Out_var._parameters.xmax = 0.05;
  _D8_SC3_Out_var._parameters.ymin = -0.05;
  _D8_SC3_Out_var._parameters.ymax = 0.05;
  _D8_SC3_Out_var._parameters.xwidth = 0.0478;
  _D8_SC3_Out_var._parameters.yheight = 0.049;
  _D8_SC3_Out_var._parameters.restore_neutron = 0;
  _D8_SC3_Out_var._parameters.nowritefile = 0;


  /* component D8_SC3_Out=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _SC3_var._rotation_absolute, _D8_SC3_Out_var._rotation_absolute);
    rot_transpose(_SC3_var._rotation_absolute, tr1);
    rot_mul(_D8_SC3_Out_var._rotation_absolute, tr1, _D8_SC3_Out_var._rotation_relative);
    _D8_SC3_Out_var._rotation_is_identity =  rot_test_identity(_D8_SC3_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.3047);
    rot_transpose(_SC3_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D8_SC3_Out_var._position_absolute = coords_add(_SC3_var._position_absolute, tc2);
    tc1 = coords_sub(_SC3_var._position_absolute, _D8_SC3_Out_var._position_absolute);
    _D8_SC3_Out_var._position_relative = rot_apply(_D8_SC3_Out_var._rotation_absolute, tc1);
  } /* D8_SC3_Out=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D8_SC3_Out", _D8_SC3_Out_var._position_absolute, _D8_SC3_Out_var._rotation_absolute);
  instrument->_position_absolute[22] = _D8_SC3_Out_var._position_absolute;
  instrument->_position_relative[22] = _D8_SC3_Out_var._position_relative;
  instrument->counter_N[22]  = instrument->counter_P[22] = instrument->counter_P2[22] = 0;
  instrument->counter_AbsorbProp[22]= 0;
  return(0);
} /* _D8_SC3_Out_setpos */

/* component Ana_Cradle=Arm() SETTING, POSITION/ROTATION */
int _Ana_Cradle_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Ana_Cradle_setpos] component Ana_Cradle=Arm() SETTING [Arm:0]");
  stracpy(_Ana_Cradle_var._name, "Ana_Cradle", 16384);
  stracpy(_Ana_Cradle_var._type, "Arm", 16384);
  _Ana_Cradle_var._index=23;
  /* component Ana_Cradle=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A5)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _Ana_Cradle_var._rotation_absolute);
    rot_transpose(_D8_SC3_Out_var._rotation_absolute, tr1);
    rot_mul(_Ana_Cradle_var._rotation_absolute, tr1, _Ana_Cradle_var._rotation_relative);
    _Ana_Cradle_var._rotation_is_identity =  rot_test_identity(_Ana_Cradle_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.1397);
    rot_transpose(_D8_SC3_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Ana_Cradle_var._position_absolute = coords_add(_D8_SC3_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D8_SC3_Out_var._position_absolute, _Ana_Cradle_var._position_absolute);
    _Ana_Cradle_var._position_relative = rot_apply(_Ana_Cradle_var._rotation_absolute, tc1);
  } /* Ana_Cradle=Arm() AT ROTATED */
  DEBUG_COMPONENT("Ana_Cradle", _Ana_Cradle_var._position_absolute, _Ana_Cradle_var._rotation_absolute);
  instrument->_position_absolute[23] = _Ana_Cradle_var._position_absolute;
  instrument->_position_relative[23] = _Ana_Cradle_var._position_relative;
  instrument->counter_N[23]  = instrument->counter_P[23] = instrument->counter_P2[23] = 0;
  instrument->counter_AbsorbProp[23]= 0;
  return(0);
} /* _Ana_Cradle_setpos */

/* component PG2Xtal=Monochromator_flat() SETTING, POSITION/ROTATION */
int _PG2Xtal_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_PG2Xtal_setpos] component PG2Xtal=Monochromator_flat() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Monochromator_flat.comp:103]");
  stracpy(_PG2Xtal_var._name, "PG2Xtal", 16384);
  stracpy(_PG2Xtal_var._type, "Monochromator_flat", 16384);
  _PG2Xtal_var._index=24;
  _PG2Xtal_var._parameters.zmin = -0.05;
  _PG2Xtal_var._parameters.zmax = 0.05;
  _PG2Xtal_var._parameters.ymin = -0.05;
  _PG2Xtal_var._parameters.ymax = 0.05;
  _PG2Xtal_var._parameters.zwidth = 0.10;
  _PG2Xtal_var._parameters.yheight = 0.08;
  _PG2Xtal_var._parameters.mosaich = 40;
  _PG2Xtal_var._parameters.mosaicv = 40;
  _PG2Xtal_var._parameters.r0 = 0.7;
  _PG2Xtal_var._parameters.Q = mono_q;
  _PG2Xtal_var._parameters.DM = 0;


  /* component PG2Xtal=Monochromator_flat() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Ana_Cradle_var._rotation_absolute, _PG2Xtal_var._rotation_absolute);
    rot_transpose(_Ana_Cradle_var._rotation_absolute, tr1);
    rot_mul(_PG2Xtal_var._rotation_absolute, tr1, _PG2Xtal_var._rotation_relative);
    _PG2Xtal_var._rotation_is_identity =  rot_test_identity(_PG2Xtal_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_Ana_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _PG2Xtal_var._position_absolute = coords_add(_Ana_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_Ana_Cradle_var._position_absolute, _PG2Xtal_var._position_absolute);
    _PG2Xtal_var._position_relative = rot_apply(_PG2Xtal_var._rotation_absolute, tc1);
  } /* PG2Xtal=Monochromator_flat() AT ROTATED */
  DEBUG_COMPONENT("PG2Xtal", _PG2Xtal_var._position_absolute, _PG2Xtal_var._rotation_absolute);
  instrument->_position_absolute[24] = _PG2Xtal_var._position_absolute;
  instrument->_position_relative[24] = _PG2Xtal_var._position_relative;
  instrument->counter_N[24]  = instrument->counter_P[24] = instrument->counter_P2[24] = 0;
  instrument->counter_AbsorbProp[24]= 0;
  return(0);
} /* _PG2Xtal_setpos */

/* component Ana_Out=Arm() SETTING, POSITION/ROTATION */
int _Ana_Out_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_Ana_Out_setpos] component Ana_Out=Arm() SETTING [Arm:0]");
  stracpy(_Ana_Out_var._name, "Ana_Out", 16384);
  stracpy(_Ana_Out_var._type, "Arm", 16384);
  _Ana_Out_var._index=25;
  /* component Ana_Out=Arm() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0)*DEG2RAD, (A6)*DEG2RAD, (0)*DEG2RAD);
    rot_mul(tr1, _Sample_Out_var._rotation_absolute, _Ana_Out_var._rotation_absolute);
    rot_transpose(_PG2Xtal_var._rotation_absolute, tr1);
    rot_mul(_Ana_Out_var._rotation_absolute, tr1, _Ana_Out_var._rotation_relative);
    _Ana_Out_var._rotation_is_identity =  rot_test_identity(_Ana_Out_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0);
    rot_transpose(_Ana_Cradle_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _Ana_Out_var._position_absolute = coords_add(_Ana_Cradle_var._position_absolute, tc2);
    tc1 = coords_sub(_PG2Xtal_var._position_absolute, _Ana_Out_var._position_absolute);
    _Ana_Out_var._position_relative = rot_apply(_Ana_Out_var._rotation_absolute, tc1);
  } /* Ana_Out=Arm() AT ROTATED */
  DEBUG_COMPONENT("Ana_Out", _Ana_Out_var._position_absolute, _Ana_Out_var._rotation_absolute);
  instrument->_position_absolute[25] = _Ana_Out_var._position_absolute;
  instrument->_position_relative[25] = _Ana_Out_var._position_relative;
  instrument->counter_N[25]  = instrument->counter_P[25] = instrument->counter_P2[25] = 0;
  instrument->counter_AbsorbProp[25]= 0;
  return(0);
} /* _Ana_Out_setpos */

/* component D10_SC4_In=PSD_monitor() SETTING, POSITION/ROTATION */
int _D10_SC4_In_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_D10_SC4_In_setpos] component D10_SC4_In=PSD_monitor() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");
  stracpy(_D10_SC4_In_var._name, "D10_SC4_In", 16384);
  stracpy(_D10_SC4_In_var._type, "PSD_monitor", 16384);
  _D10_SC4_In_var._index=26;
  _D10_SC4_In_var._parameters.nx = 20;
  _D10_SC4_In_var._parameters.ny = 20;
  if("D10_SC4_In.psd" && strlen("D10_SC4_In.psd"))
    stracpy(_D10_SC4_In_var._parameters.filename, "D10_SC4_In.psd" ? "D10_SC4_In.psd" : "", 16384);
  else 
  _D10_SC4_In_var._parameters.filename[0]='\0';
  _D10_SC4_In_var._parameters.xmin = -0.05;
  _D10_SC4_In_var._parameters.xmax = 0.05;
  _D10_SC4_In_var._parameters.ymin = -0.05;
  _D10_SC4_In_var._parameters.ymax = 0.05;
  _D10_SC4_In_var._parameters.xwidth = 0.0478;
  _D10_SC4_In_var._parameters.yheight = 0.049;
  _D10_SC4_In_var._parameters.restore_neutron = 0;
  _D10_SC4_In_var._parameters.nowritefile = 0;


  /* component D10_SC4_In=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Ana_Out_var._rotation_absolute, _D10_SC4_In_var._rotation_absolute);
    rot_transpose(_Ana_Out_var._rotation_absolute, tr1);
    rot_mul(_D10_SC4_In_var._rotation_absolute, tr1, _D10_SC4_In_var._rotation_relative);
    _D10_SC4_In_var._rotation_is_identity =  rot_test_identity(_D10_SC4_In_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.3365);
    rot_transpose(_Ana_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _D10_SC4_In_var._position_absolute = coords_add(_Ana_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_Ana_Out_var._position_absolute, _D10_SC4_In_var._position_absolute);
    _D10_SC4_In_var._position_relative = rot_apply(_D10_SC4_In_var._rotation_absolute, tc1);
  } /* D10_SC4_In=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("D10_SC4_In", _D10_SC4_In_var._position_absolute, _D10_SC4_In_var._rotation_absolute);
  instrument->_position_absolute[26] = _D10_SC4_In_var._position_absolute;
  instrument->_position_relative[26] = _D10_SC4_In_var._position_relative;
  instrument->counter_N[26]  = instrument->counter_P[26] = instrument->counter_P2[26] = 0;
  instrument->counter_AbsorbProp[26]= 0;
  return(0);
} /* _D10_SC4_In_setpos */

/* component SC4=Guide() SETTING, POSITION/ROTATION */
int _SC4_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_SC4_setpos] component SC4=Guide() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Guide.comp:74]");
  stracpy(_SC4_var._name, "SC4", 16384);
  stracpy(_SC4_var._type, "Guide", 16384);
  _SC4_var._index=27;
  _SC4_var._parameters.reflect[0]='\0';
  _SC4_var._parameters.w1 = 0.0478;
  _SC4_var._parameters.h1 = 0.0490;
  _SC4_var._parameters.w2 = 0;
  _SC4_var._parameters.h2 = 0;
  _SC4_var._parameters.l = 0.3048;
  _SC4_var._parameters.R0 = 1.0;
  _SC4_var._parameters.Qc = 0.021;
  _SC4_var._parameters.alpha = 6;
  _SC4_var._parameters.m = 1;
  _SC4_var._parameters.W = 0.0003;


  /* component SC4=Guide() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _Ana_Out_var._rotation_absolute, _SC4_var._rotation_absolute);
    rot_transpose(_D10_SC4_In_var._rotation_absolute, tr1);
    rot_mul(_SC4_var._rotation_absolute, tr1, _SC4_var._rotation_relative);
    _SC4_var._rotation_is_identity =  rot_test_identity(_SC4_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.3366);
    rot_transpose(_Ana_Out_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _SC4_var._position_absolute = coords_add(_Ana_Out_var._position_absolute, tc2);
    tc1 = coords_sub(_D10_SC4_In_var._position_absolute, _SC4_var._position_absolute);
    _SC4_var._position_relative = rot_apply(_SC4_var._rotation_absolute, tc1);
  } /* SC4=Guide() AT ROTATED */
  DEBUG_COMPONENT("SC4", _SC4_var._position_absolute, _SC4_var._rotation_absolute);
  instrument->_position_absolute[27] = _SC4_var._position_absolute;
  instrument->_position_relative[27] = _SC4_var._position_relative;
  instrument->counter_N[27]  = instrument->counter_P[27] = instrument->counter_P2[27] = 0;
  instrument->counter_AbsorbProp[27]= 0;
  return(0);
} /* _SC4_setpos */

/* component He3H=PSD_monitor() SETTING, POSITION/ROTATION */
int _He3H_setpos(void)
{ /* sets initial component parameters, position and rotation */
  SIG_MESSAGE("[_He3H_setpos] component He3H=PSD_monitor() SETTING [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");
  stracpy(_He3H_var._name, "He3H", 16384);
  stracpy(_He3H_var._type, "PSD_monitor", 16384);
  _He3H_var._index=28;
  _He3H_var._parameters.nx = 20;
  _He3H_var._parameters.ny = 20;
  if("He3.psd" && strlen("He3.psd"))
    stracpy(_He3H_var._parameters.filename, "He3.psd" ? "He3.psd" : "", 16384);
  else 
  _He3H_var._parameters.filename[0]='\0';
  _He3H_var._parameters.xmin = -0.05;
  _He3H_var._parameters.xmax = 0.05;
  _He3H_var._parameters.ymin = -0.05;
  _He3H_var._parameters.ymax = 0.05;
  _He3H_var._parameters.xwidth = 0.0508;
  _He3H_var._parameters.yheight = 0.0857;
  _He3H_var._parameters.restore_neutron = 0;
  _He3H_var._parameters.nowritefile = 0;


  /* component He3H=PSD_monitor() AT ROTATED */
  {
    Coords tc1, tc2;
    Rotation tr1;
    rot_set_rotation(tr1,
      (0.0)*DEG2RAD, (0.0)*DEG2RAD, (0.0)*DEG2RAD);
    rot_mul(tr1, _SC4_var._rotation_absolute, _He3H_var._rotation_absolute);
    rot_transpose(_SC4_var._rotation_absolute, tr1);
    rot_mul(_He3H_var._rotation_absolute, tr1, _He3H_var._rotation_relative);
    _He3H_var._rotation_is_identity =  rot_test_identity(_He3H_var._rotation_relative);
    tc1 = coords_set(
      0, 0, 0.3049);
    rot_transpose(_SC4_var._rotation_absolute, tr1);
    tc2 = rot_apply(tr1, tc1);
    _He3H_var._position_absolute = coords_add(_SC4_var._position_absolute, tc2);
    tc1 = coords_sub(_SC4_var._position_absolute, _He3H_var._position_absolute);
    _He3H_var._position_relative = rot_apply(_He3H_var._rotation_absolute, tc1);
  } /* He3H=PSD_monitor() AT ROTATED */
  DEBUG_COMPONENT("He3H", _He3H_var._position_absolute, _He3H_var._rotation_absolute);
  instrument->_position_absolute[28] = _He3H_var._position_absolute;
  instrument->_position_relative[28] = _He3H_var._position_relative;
  instrument->counter_N[28]  = instrument->counter_P[28] = instrument->counter_P2[28] = 0;
  instrument->counter_AbsorbProp[28]= 0;
  return(0);
} /* _He3H_setpos */

_class_Progress_bar *class_Progress_bar_init(_class_Progress_bar *_comp
) {
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_Origin_init] component Origin=Progress_bar() INITIALISE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../misc/Progress_bar.comp:57]");

IntermediateCnts=0;
StartTime=0;
EndTime=0;
CurrentTime=0;

fprintf(stdout, "[%s] Initialize\n", instrument_name);
  if (percent*mcget_ncount()/100 < 1e5) {
    percent=1e5*100.0/mcget_ncount();
  }
  #ifdef OPENACC
  time(&StartTime);
  #endif
  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_init */

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
  #define srcArea (_comp->_parameters.srcArea)
  #define square (_comp->_parameters.square)
  #define tx (_comp->_parameters.tx)
  #define ty (_comp->_parameters.ty)
  #define tz (_comp->_parameters.tz)
  SIG_MESSAGE("[_Source_init] component Source=Source_simple() INITIALISE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../sources/Source_simple.comp:68]");

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
    ToTarget = coords_sub(POS_A_COMP_INDEX(INDEX_CURRENT_COMP+target_index),POS_A_CURRENT_COMP);
    ToTarget = rot_apply(ROT_A_CURRENT_COMP, ToTarget);
    coords_get(ToTarget, &tx, &ty, &tz);
    dist=sqrt(tx*tx+ty*ty+tz*tz);
  } else if (dist) {
    tx = 0;
    ty = 0;
    tz = dist;
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
  #undef srcArea
  #undef square
  #undef tx
  #undef ty
  #undef tz
  return(_comp);
} /* class_Source_simple_init */

_class_PSD_monitor *class_PSD_monitor_init(_class_PSD_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define ny (_comp->_parameters.ny)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define nowritefile (_comp->_parameters.nowritefile)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_D0_Source_init] component D0_Source=PSD_monitor() INITIALISE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:62]");

  if (xwidth  > 0) { xmax = xwidth/2;  xmin = -xmax; }
  if (yheight > 0) { ymax = yheight/2; ymin = -ymax; }

  if ((xmin >= xmax) || (ymin >= ymax)){
    printf("PSD_monitor: %s: Null detection area !\n"
           "ERROR        (xwidth,yheight,xmin,xmax,ymin,ymax). Exiting",
    NAME_CURRENT_COMP);
    exit(0);
  }

  PSD_N = create_darr2d(nx, ny);
  PSD_p = create_darr2d(nx, ny);
  PSD_p2 = create_darr2d(nx, ny);

  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef nowritefile
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_init */

_class_Guide *class_Guide_init(_class_Guide *_comp
) {
  #define reflect (_comp->_parameters.reflect)
  #define w1 (_comp->_parameters.w1)
  #define h1 (_comp->_parameters.h1)
  #define w2 (_comp->_parameters.w2)
  #define h2 (_comp->_parameters.h2)
  #define l (_comp->_parameters.l)
  #define R0 (_comp->_parameters.R0)
  #define Qc (_comp->_parameters.Qc)
  #define alpha (_comp->_parameters.alpha)
  #define m (_comp->_parameters.m)
  #define W (_comp->_parameters.W)
  #define pTable (_comp->_parameters.pTable)
  #define table_present (_comp->_parameters.table_present)
  SIG_MESSAGE("[_SC1_init] component SC1=Guide() INITIALISE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Guide.comp:74]");

if (mcgravitation) fprintf(stderr,"WARNING: Guide: %s: "
    "This component produces wrong results with gravitation !\n"
    "Use Guide_gravity.\n",
    NAME_CURRENT_COMP);

  if (!w2) w2=w1;
  if (!h2) h2=h1;

  if (reflect && strlen(reflect) && strcmp(reflect,"NULL") && strcmp(reflect,"0")) {
    if (Table_Read(&pTable, reflect, 1) <= 0) /* read 1st block data from file into pTable */
      exit(fprintf(stderr,"Guide: %s: can not read file %s\n", NAME_CURRENT_COMP, reflect));
    table_present=1;
  } else {
    table_present=0;
    if (W < 0 || R0 < 0 || Qc < 0 || m < 0)
    { fprintf(stderr,"Guide: %s: W R0 Qc must be >0.\n", NAME_CURRENT_COMP);
      exit(-1); }
  }
  #undef reflect
  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  #undef pTable
  #undef table_present
  return(_comp);
} /* class_Guide_init */

_class_Slit *class_Slit_init(_class_Slit *_comp
) {
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_As1_init] component As1=Slit() INITIALISE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Slit.comp:50]");

if (xwidth > 0)  { 
  if (!xmin && !xmax) {
    xmax=xwidth/2;  xmin=-xmax;
  } else {
    fprintf(stderr,"Slit: %s: Error: please specify EITHER xmin & xmax or xwidth\n", NAME_CURRENT_COMP); exit(-1);
  }
 }
 if (yheight > 0) { 
   if (!ymin && !ymax) {
     ymax=yheight/2; ymin=-ymax; 
   } else {
     fprintf(stderr,"Slit: %s: Error: please specify EITHER ymin & ymax or ywidth\n", NAME_CURRENT_COMP); exit(-1);
   }
 }
 if (xmin == 0 && xmax == 0 && ymin == 0 && ymax == 0 && radius == 0)
    { fprintf(stderr,"Slit: %s: Warning: Running with CLOSED slit - is this intentional?? \n", NAME_CURRENT_COMP); }

  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_init */

_class_Monochromator_flat *class_Monochromator_flat_init(_class_Monochromator_flat *_comp
) {
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zwidth (_comp->_parameters.zwidth)
  #define yheight (_comp->_parameters.yheight)
  #define mosaich (_comp->_parameters.mosaich)
  #define mosaicv (_comp->_parameters.mosaicv)
  #define r0 (_comp->_parameters.r0)
  #define Q (_comp->_parameters.Q)
  #define DM (_comp->_parameters.DM)
  #define mos_rms_y (_comp->_parameters.mos_rms_y)
  #define mos_rms_z (_comp->_parameters.mos_rms_z)
  #define mos_rms_max (_comp->_parameters.mos_rms_max)
  #define mono_Q (_comp->_parameters.mono_Q)
  SIG_MESSAGE("[_PG1Xtal_init] component PG1Xtal=Monochromator_flat() INITIALISE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Monochromator_flat.comp:103]");

  mos_rms_y = MIN2RAD*mosaicv/sqrt(8*log(2));
  mos_rms_z = MIN2RAD*mosaich/sqrt(8*log(2));
  mos_rms_max = mos_rms_y > mos_rms_z ? mos_rms_y : mos_rms_z;

  mono_Q = Q;
  if (DM != 0) mono_Q = 2*PI/DM;

  if (zwidth>0)  { zmax = zwidth/2;  zmin=-zmax; }
  if (yheight>0) { ymax = yheight/2; ymin=-ymax; }

  if (zmin==zmax || ymin==ymax)
    exit(fprintf(stderr, "Monochromator_flat: %s : Surface is null (zmin,zmax,ymin,ymax)\n", NAME_CURRENT_COMP));
  #undef zmin
  #undef zmax
  #undef ymin
  #undef ymax
  #undef zwidth
  #undef yheight
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef DM
  #undef mos_rms_y
  #undef mos_rms_z
  #undef mos_rms_max
  #undef mono_Q
  return(_comp);
} /* class_Monochromator_flat_init */

_class_V_sample *class_V_sample_init(_class_V_sample *_comp
) {
  #define radius (_comp->_parameters.radius)
  #define thickness (_comp->_parameters.thickness)
  #define zdepth (_comp->_parameters.zdepth)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define radius_i (_comp->_parameters.radius_i)
  #define radius_o (_comp->_parameters.radius_o)
  #define h (_comp->_parameters.h)
  #define focus_r (_comp->_parameters.focus_r)
  #define pack (_comp->_parameters.pack)
  #define frac (_comp->_parameters.frac)
  #define f_QE (_comp->_parameters.f_QE)
  #define gamma (_comp->_parameters.gamma)
  #define target_x (_comp->_parameters.target_x)
  #define target_y (_comp->_parameters.target_y)
  #define target_z (_comp->_parameters.target_z)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define focus_aw (_comp->_parameters.focus_aw)
  #define focus_ah (_comp->_parameters.focus_ah)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zthick (_comp->_parameters.zthick)
  #define rad_sphere (_comp->_parameters.rad_sphere)
  #define sig_a (_comp->_parameters.sig_a)
  #define sig_i (_comp->_parameters.sig_i)
  #define V0 (_comp->_parameters.V0)
  #define target_index (_comp->_parameters.target_index)
  #define multiples (_comp->_parameters.multiples)
  #define VarsV (_comp->_parameters.VarsV)
  SIG_MESSAGE("[_Sample_init] component Sample=V_sample() INITIALISE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../obsolete/V_sample.comp:121]");

  /* Backward compatibility */
  if (radius) radius_o = radius;
  if (thickness) radius_i = radius_o - thickness;
  if (zdepth) zthick = zdepth;
  if (yheight) h = yheight;
  if (Vc) V0 = Vc;
  if (sigma_abs) sig_a = sigma_abs;
  if (sigma_inc) sig_i = sigma_inc;

  VarsV.shapetyp = -1;
  if (xwidth && yheight && zdepth)  VarsV.shapetyp=1; /* box */
  else if (radius > 0 && yheight)        VarsV.shapetyp=0; /* cylinder */
  else if (radius && !yheight)           VarsV.shapetyp=2; /* sphere */
  
  if (VarsV.shapetyp < 0)
    exit(fprintf(stderr,"V_sample: %s: sample has invalid dimensions. Please check parameter values.\n", NAME_CURRENT_COMP));

  VarsV.sigma_a=sig_a;
  VarsV.sigma_i=sig_i;
  VarsV.rho = (pack/V0);
  VarsV.my_s=(VarsV.rho * 100 * VarsV.sigma_i);
  VarsV.my_a_v=(VarsV.rho * 100 * VarsV.sigma_a);

  /* now compute target coords if a component index is supplied */
  VarsV.tx= VarsV.ty=VarsV.tz=0;
  if (target_index)
  {
    Coords ToTarget;
    ToTarget = coords_sub(POS_A_COMP_INDEX(INDEX_CURRENT_COMP+target_index),POS_A_CURRENT_COMP);
    ToTarget = rot_apply(ROT_A_CURRENT_COMP, ToTarget);
    coords_get(ToTarget, &VarsV.tx, &VarsV.ty, &VarsV.tz);
  }
  else
  { VarsV.tx = target_x; VarsV.ty = target_y; VarsV.tz = target_z; }

  if (!(VarsV.tx || VarsV.ty || VarsV.tz))
    printf("V_sample: %s: The target is not defined. Using direct beam (Z-axis).\n",
      NAME_CURRENT_COMP);

  VarsV.distance=sqrt(VarsV.tx*VarsV.tx+VarsV.ty*VarsV.ty+VarsV.tz*VarsV.tz);

  /* different ways of setting rectangular area */
  VarsV.aw  = VarsV.ah = 0;
  if (focus_xw) {
  VarsV.xw = focus_xw;
  }
  if (focus_yh) {
    VarsV.yh = focus_yh;
  }
  if (focus_aw) {
    VarsV.aw = DEG2RAD*focus_aw;
  }
  if (focus_ah) {
    VarsV.ah = DEG2RAD*focus_ah;
  }
  #undef radius
  #undef thickness
  #undef zdepth
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef radius_i
  #undef radius_o
  #undef h
  #undef focus_r
  #undef pack
  #undef frac
  #undef f_QE
  #undef gamma
  #undef target_x
  #undef target_y
  #undef target_z
  #undef focus_xw
  #undef focus_yh
  #undef focus_aw
  #undef focus_ah
  #undef xwidth
  #undef yheight
  #undef zthick
  #undef rad_sphere
  #undef sig_a
  #undef sig_i
  #undef V0
  #undef target_index
  #undef multiples
  #undef VarsV
  return(_comp);
} /* class_V_sample_init */



int init(void) { /* called by mccode_main for BNL_H8:INITIALISE */
  DEBUG_INSTR();

  /* code_main/parseoptions/readparams sets instrument parameters value */
  stracpy(instrument->_name, "BNL_H8", 256);

  /* Instrument 'BNL_H8' INITIALISE */
  SIG_MESSAGE("[BNL_H8] INITIALISE [BNL_H8.instr:52]");
  #define lambda (instrument->_parameters.lambda)
{
  int    ORDER = 1;
  double vi, Ki;
  int    SM,SS,SA;
  char hostname[256];

  /* SM : scattering at mono to the right (-1)/left(+1) */
  /* SS : scattering at sample to the right (-1)/left(+1) */
  /* SA : scattering at analyser to the right (-1)/left(+1) */
  SM = 1; SS = -1; SA = 1;

/*  SM = 0; SS = -0; SA = 0; */

  mono_q = 2*PI*ORDER/DM;  /* Q mono in Angs-1 */

  Ki = 2*PI/lambda;
  vi = K2V*fabs(Ki);
  Ei = VS2E*vi*vi;

  A2 = asin(mono_q/2/Ki)*RAD2DEG*2;
  A4 = A2; A6 = A2;

  A2 *= SM;       /* A1 : mono theta (crystal) */
  A1 = A2/2;    /* A2 : mono 2 theta (arm to sample) */
  A4 *= SS;       /* A3 : sample theta */
  A3 = A4/2;    /* A4 : sample 2 theta (arm to analyser) */
  A6 *= SA;       /* A5 : analyser theta (crystal) */
  A5 = A6/2;    /* A6 : analyser 2 theta (arm to Dector) */

  strcpy(hostname, getenv("HOSTNAME") ? getenv("HOSTNAME") : "localhost");

  printf("Instrument:     BNL_H8 on %s.\n", hostname);
  printf("Monochromator : DM = %g\n",DM);
  printf("A1 = %.2f, A2 = %.2f (deg)\n",A1,A2);
  printf("Ki = %.4g Angs-1 Energy = %.4g meV\nVelocity = %.4g m/s, lambda = %.4g Angs\n", Ki, Ei, vi,
lambda);
}
  #undef lambda
  _Origin_setpos(); /* type Progress_bar */
  _Source_setpos(); /* type Source_simple */
  _D0_Source_setpos(); /* type PSD_monitor */
  _SC1_setpos(); /* type Guide */
  _D1_SC1_Out_setpos(); /* type PSD_monitor */
  _As1_setpos(); /* type Slit */
  _As2_setpos(); /* type Slit */
  _As3_setpos(); /* type Slit */
  _As4_setpos(); /* type Slit */
  _D2_A4_setpos(); /* type PSD_monitor */
  _Mono_Cradle_setpos(); /* type Arm */
  _PG1Xtal_setpos(); /* type Monochromator_flat */
  _Mono_Out_setpos(); /* type Arm */
  _D4_SC2_In_setpos(); /* type PSD_monitor */
  _SC2_setpos(); /* type Guide */
  _D5_SC2_Out_setpos(); /* type PSD_monitor */
  _Sample_Cradle_setpos(); /* type Arm */
  _Sample_Out_setpos(); /* type Arm */
  _Sample_setpos(); /* type V_sample */
  _D7_SC3_In_setpos(); /* type PSD_monitor */
  _SC3_setpos(); /* type Guide */
  _D8_SC3_Out_setpos(); /* type PSD_monitor */
  _Ana_Cradle_setpos(); /* type Arm */
  _PG2Xtal_setpos(); /* type Monochromator_flat */
  _Ana_Out_setpos(); /* type Arm */
  _D10_SC4_In_setpos(); /* type PSD_monitor */
  _SC4_setpos(); /* type Guide */
  _He3H_setpos(); /* type PSD_monitor */

  /* call iteratively all components INITIALISE */
  class_Progress_bar_init(&_Origin_var);

  class_Source_simple_init(&_Source_var);

  class_PSD_monitor_init(&_D0_Source_var);

  class_Guide_init(&_SC1_var);

  class_PSD_monitor_init(&_D1_SC1_Out_var);

  class_Slit_init(&_As1_var);

  class_Slit_init(&_As2_var);

  class_Slit_init(&_As3_var);

  class_Slit_init(&_As4_var);

  class_PSD_monitor_init(&_D2_A4_var);


  class_Monochromator_flat_init(&_PG1Xtal_var);


  class_PSD_monitor_init(&_D4_SC2_In_var);

  class_Guide_init(&_SC2_var);

  class_PSD_monitor_init(&_D5_SC2_Out_var);



  class_V_sample_init(&_Sample_var);

  class_PSD_monitor_init(&_D7_SC3_In_var);

  class_Guide_init(&_SC3_var);

  class_PSD_monitor_init(&_D8_SC3_Out_var);


  class_Monochromator_flat_init(&_PG2Xtal_var);


  class_PSD_monitor_init(&_D10_SC4_In_var);

  class_Guide_init(&_SC4_var);

  class_PSD_monitor_init(&_He3H_var);

  if (mcdotrace) display();
  DEBUG_INSTR_END();

#ifdef OPENACC
#include <openacc.h>
#pragma acc update device(_Origin_var)
#pragma acc update device(_Source_var)
#pragma acc update device(_D0_Source_var)
#pragma acc update device(_SC1_var)
#pragma acc update device(_D1_SC1_Out_var)
#pragma acc update device(_As1_var)
#pragma acc update device(_As2_var)
#pragma acc update device(_As3_var)
#pragma acc update device(_As4_var)
#pragma acc update device(_D2_A4_var)
#pragma acc update device(_Mono_Cradle_var)
#pragma acc update device(_PG1Xtal_var)
#pragma acc update device(_Mono_Out_var)
#pragma acc update device(_D4_SC2_In_var)
#pragma acc update device(_SC2_var)
#pragma acc update device(_D5_SC2_Out_var)
#pragma acc update device(_Sample_Cradle_var)
#pragma acc update device(_Sample_Out_var)
#pragma acc update device(_Sample_var)
#pragma acc update device(_D7_SC3_In_var)
#pragma acc update device(_SC3_var)
#pragma acc update device(_D8_SC3_Out_var)
#pragma acc update device(_Ana_Cradle_var)
#pragma acc update device(_PG2Xtal_var)
#pragma acc update device(_Ana_Out_var)
#pragma acc update device(_D10_SC4_In_var)
#pragma acc update device(_SC4_var)
#pragma acc update device(_He3H_var)
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
#define mcgravitation (_particle->mcgravitation)
#define mcMagnet (_particle->mcMagnet)
#define allow_backprop (_particle->allow_backprop)
/* if on GPU, globally nullify sprintf,fprintf,printfs   */
/* (Similar defines are available in each comp trace but */
/*  those are not enough to handle external libs etc. )  */
#ifdef OPENACC
#ifndef MULTICORE
#define fprintf(stderr,...) printf(__VA_ARGS__)
#define sprintf(string,...) printf(__VA_ARGS__)
#define printf(...) noprintf()
#define exit(...) noprintf()
#define strcmp(a,b) str_comp(a,b)
#define strlen(a) str_len(a)
#endif
#endif
#define SCATTERED (_particle->_scattered)
#define RESTORE (_particle->_restore)
#define RESTORE_NEUTRON(_index, ...) _particle->_restore = _index;
#define ABSORBED (_particle->_absorbed)
#define mcget_run_num() _particle->_uid
#define ABSORB0 do { DEBUG_STATE(); DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; return(_comp); } while(0)
#define ABSORB ABSORB0
#pragma acc routine
_class_Progress_bar *class_Progress_bar_trace(_class_Progress_bar *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_Origin_trace] component Origin=Progress_bar() TRACE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../misc/Progress_bar.comp:73]");

#ifndef OPENACC
  double ncount;
  ncount = mcget_run_num();
  if (!StartTime) {
    time(&StartTime); /* compute starting time */
    IntermediateCnts = 1e3;
  }
  time_t NowTime;
  time(&NowTime);
  /* compute initial estimate of computation duration */
  if (!EndTime && ncount >= IntermediateCnts) {
    CurrentTime = NowTime;
    if (difftime(NowTime,StartTime) > 10 && ncount) { /* wait 10 sec before writing ETA */
      EndTime = StartTime + (time_t)(difftime(NowTime,StartTime)
				     *(double)mcget_ncount()/ncount);
      IntermediateCnts = 0;
      fprintf(stdout, "\nTrace ETA ");
      if (difftime(EndTime,StartTime) < 60.0)
        fprintf(stdout, "%g [s] %% ", difftime(EndTime,StartTime));
      else if (difftime(EndTime,StartTime) > 3600.0)
        fprintf(stdout, "%g [h] %% ", difftime(EndTime,StartTime)/3600.0);
      else
        fprintf(stdout, "%g [min] %% ", difftime(EndTime,StartTime)/60.0);
    } else IntermediateCnts += 1e3;
    fflush(stdout);
  }

  /* display percentage when percent or minutes have reached step */
  if (EndTime && mcget_ncount() &&
    (    (minutes && difftime(NowTime,CurrentTime) > minutes*60)
      || (percent && !minutes && ncount >= IntermediateCnts))   )
  {
    fprintf(stdout, "%d ", (int)(ncount*100.0/mcget_ncount())); fflush(stdout);
    CurrentTime = NowTime;

    IntermediateCnts = ncount + percent*mcget_ncount()/100;
    /* check that next intermediate ncount check is a multiple of the desired percentage */
    IntermediateCnts = floor(IntermediateCnts*100/percent/mcget_ncount())*percent*mcget_ncount()/100;
    /* raise flag to indicate that we did something */
    SCATTER;
    if (flag_save) save(NULL);
  }
#endif
#ifndef NOABSORB_INF_NAN
  /* Check for nan or inf particle parms */ 
  if(isnan(p)  ||  isinf(p)) ABSORB;
  if(isnan(t)  ||  isinf(t)) ABSORB;
  if(isnan(vx) || isinf(vx)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(x)  ||  isinf(x)) ABSORB;
  if(isnan(y)  ||  isinf(y)) ABSORB;
  if(isnan(z)  ||  isinf(z)) ABSORB;
#else
  if(isnan(p)  ||  isinf(p)) printf("NAN or INF found in p,  %s\n",_comp->_name);
  if(isnan(t)  ||  isinf(t)) printf("NAN or INF found in t,  %s\n",_comp->_name);
  if(isnan(vx) || isinf(vx)) printf("NAN or INF found in vx, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vy, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vz, %s\n",_comp->_name);
  if(isnan(x)  ||  isinf(x)) printf("NAN or INF found in x,  %s\n",_comp->_name);
  if(isnan(y)  ||  isinf(y)) printf("NAN or INF found in y,  %s\n",_comp->_name);
  if(isnan(z)  ||  isinf(z)) printf("NAN or INF found in z,  %s\n",_comp->_name);
#endif
  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_trace */

#pragma acc routine
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
  #define srcArea (_comp->_parameters.srcArea)
  #define square (_comp->_parameters.square)
  #define tx (_comp->_parameters.tx)
  #define ty (_comp->_parameters.ty)
  #define tz (_comp->_parameters.tz)
  SIG_MESSAGE("[_Source_trace] component Source=Source_simple() TRACE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../sources/Source_simple.comp:127]");

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
			  tx, ty, tz, focus_xw, focus_yh, ROT_A_CURRENT_COMP, x, y, z, 2);

 dx = xf-x;
 dy = yf-y;
 rf = sqrt(dx*dx+dy*dy+rf*rf);

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
#ifndef NOABSORB_INF_NAN
  /* Check for nan or inf particle parms */ 
  if(isnan(p)  ||  isinf(p)) ABSORB;
  if(isnan(t)  ||  isinf(t)) ABSORB;
  if(isnan(vx) || isinf(vx)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(x)  ||  isinf(x)) ABSORB;
  if(isnan(y)  ||  isinf(y)) ABSORB;
  if(isnan(z)  ||  isinf(z)) ABSORB;
#else
  if(isnan(p)  ||  isinf(p)) printf("NAN or INF found in p,  %s\n",_comp->_name);
  if(isnan(t)  ||  isinf(t)) printf("NAN or INF found in t,  %s\n",_comp->_name);
  if(isnan(vx) || isinf(vx)) printf("NAN or INF found in vx, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vy, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vz, %s\n",_comp->_name);
  if(isnan(x)  ||  isinf(x)) printf("NAN or INF found in x,  %s\n",_comp->_name);
  if(isnan(y)  ||  isinf(y)) printf("NAN or INF found in y,  %s\n",_comp->_name);
  if(isnan(z)  ||  isinf(z)) printf("NAN or INF found in z,  %s\n",_comp->_name);
#endif
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
  #undef srcArea
  #undef square
  #undef tx
  #undef ty
  #undef tz
  return(_comp);
} /* class_Source_simple_trace */

#pragma acc routine
_class_PSD_monitor *class_PSD_monitor_trace(_class_PSD_monitor *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define nx (_comp->_parameters.nx)
  #define ny (_comp->_parameters.ny)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define nowritefile (_comp->_parameters.nowritefile)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_D0_Source_trace] component D0_Source=PSD_monitor() TRACE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:80]");

  PROP_Z0;
  if (x>xmin && x<xmax && y>ymin && y<ymax){
    int i = floor((x - xmin)*nx/(xmax - xmin));
    int j = floor((y - ymin)*ny/(ymax - ymin));

    double p2 = p*p;
    #pragma acc atomic
    PSD_N[i][j] = PSD_N[i][j]+1;

    #pragma acc atomic
    PSD_p[i][j] = PSD_p[i][j]+p;
    
    #pragma acc atomic
    PSD_p2[i][j] = PSD_p2[i][j] + p2;
    
    SCATTER;
  }
  if (restore_neutron) {
    RESTORE_NEUTRON(INDEX_CURRENT_COMP, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
  }
#ifndef NOABSORB_INF_NAN
  /* Check for nan or inf particle parms */ 
  if(isnan(p)  ||  isinf(p)) ABSORB;
  if(isnan(t)  ||  isinf(t)) ABSORB;
  if(isnan(vx) || isinf(vx)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(x)  ||  isinf(x)) ABSORB;
  if(isnan(y)  ||  isinf(y)) ABSORB;
  if(isnan(z)  ||  isinf(z)) ABSORB;
#else
  if(isnan(p)  ||  isinf(p)) printf("NAN or INF found in p,  %s\n",_comp->_name);
  if(isnan(t)  ||  isinf(t)) printf("NAN or INF found in t,  %s\n",_comp->_name);
  if(isnan(vx) || isinf(vx)) printf("NAN or INF found in vx, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vy, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vz, %s\n",_comp->_name);
  if(isnan(x)  ||  isinf(x)) printf("NAN or INF found in x,  %s\n",_comp->_name);
  if(isnan(y)  ||  isinf(y)) printf("NAN or INF found in y,  %s\n",_comp->_name);
  if(isnan(z)  ||  isinf(z)) printf("NAN or INF found in z,  %s\n",_comp->_name);
#endif
  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef nowritefile
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_trace */

#pragma acc routine
_class_Guide *class_Guide_trace(_class_Guide *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define reflect (_comp->_parameters.reflect)
  #define w1 (_comp->_parameters.w1)
  #define h1 (_comp->_parameters.h1)
  #define w2 (_comp->_parameters.w2)
  #define h2 (_comp->_parameters.h2)
  #define l (_comp->_parameters.l)
  #define R0 (_comp->_parameters.R0)
  #define Qc (_comp->_parameters.Qc)
  #define alpha (_comp->_parameters.alpha)
  #define m (_comp->_parameters.m)
  #define W (_comp->_parameters.W)
  #define pTable (_comp->_parameters.pTable)
  #define table_present (_comp->_parameters.table_present)
  SIG_MESSAGE("[_SC1_trace] component SC1=Guide() TRACE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Guide.comp:96]");

  double t1,t2;                                 /* Intersection times. */
  double av,ah,bv,bh,cv1,cv2,ch1,ch2,d;         /* Intermediate values */
  double weight;                                /* Internal probability weight */
  double vdotn_v1,vdotn_v2,vdotn_h1,vdotn_h2;   /* Dot products. */
  int i;                                        /* Which mirror hit? */
  double q;                                     /* Q [1/AA] of reflection */
  double nlen2;                                 /* Vector lengths squared */
  double par[5] = {R0, Qc, alpha, m, W};
  
  /* ToDo: These could be precalculated. */
  double ww = .5*(w2 - w1), hh = .5*(h2 - h1);
  double whalf = .5*w1, hhalf = .5*h1;

  /* Propagate neutron to guide entrance. */
  PROP_Z0;
  /* Scatter here to ensure that fully transmitted neutrons will not be
     absorbed in a GROUP construction, e.g. all neutrons - even the
     later absorbed ones are scattered at the guide entry. */
  SCATTER;
  if(x <= -whalf || x >= whalf || y <= -hhalf || y >= hhalf)
    ABSORB;
  for(;;)
  {
    /* Compute the dot products of v and n for the four mirrors. */
    av = l*vx; bv = ww*vz;
    ah = l*vy; bh = hh*vz;
    vdotn_v1 = bv + av;         /* Left vertical */
    vdotn_v2 = bv - av;         /* Right vertical */
    vdotn_h1 = bh + ah;         /* Lower horizontal */
    vdotn_h2 = bh - ah;         /* Upper horizontal */
    /* Compute the dot products of (O - r) and n as c1+c2 and c1-c2 */
    cv1 = -whalf*l - z*ww; cv2 = x*l;
    ch1 = -hhalf*l - z*hh; ch2 = y*l;
    /* Compute intersection times. */
    t1 = (l - z)/vz;
    i = 0;
    if(vdotn_v1 < 0 && (t2 = (cv1 - cv2)/vdotn_v1) < t1)
    {
      t1 = t2;
      i = 1;
    }
    if(vdotn_v2 < 0 && (t2 = (cv1 + cv2)/vdotn_v2) < t1)
    {
      t1 = t2;
      i = 2;
    }
    if(vdotn_h1 < 0 && (t2 = (ch1 - ch2)/vdotn_h1) < t1)
    {
      t1 = t2;
      i = 3;
    }
    if(vdotn_h2 < 0 && (t2 = (ch1 + ch2)/vdotn_h2) < t1)
    {
      t1 = t2;
      i = 4;
    }
    if(i == 0)
      break;                    /* Neutron left guide. */
    PROP_DT(t1);
    switch(i)
    {
      case 1:                   /* Left vertical mirror */
        nlen2 = l*l + ww*ww;
        q = V2Q*(-2)*vdotn_v1/sqrt(nlen2);
        d = 2*vdotn_v1/nlen2;
        vx = vx - d*l;
        vz = vz - d*ww;
        break;
      case 2:                   /* Right vertical mirror */
        nlen2 = l*l + ww*ww;
        q = V2Q*(-2)*vdotn_v2/sqrt(nlen2);
        d = 2*vdotn_v2/nlen2;
        vx = vx + d*l;
        vz = vz - d*ww;
        break;
      case 3:                   /* Lower horizontal mirror */
        nlen2 = l*l + hh*hh;
        q = V2Q*(-2)*vdotn_h1/sqrt(nlen2);
        d = 2*vdotn_h1/nlen2;
        vy = vy - d*l;
        vz = vz - d*hh;
        break;
      case 4:                   /* Upper horizontal mirror */
        nlen2 = l*l + hh*hh;
        q = V2Q*(-2)*vdotn_h2/sqrt(nlen2);
        d = 2*vdotn_h2/nlen2;
        vy = vy + d*l;
        vz = vz - d*hh;
        break;
    }
    /* Now compute reflectivity. */
    weight = 1.0; /* Initial internal weight factor */
    if(m == 0)
      ABSORB;
    if (reflect && table_present==1)
       TableReflecFunc(q, &pTable, &weight);
    else {
      StdReflecFunc(q, par, &weight);
    }
    if (weight > 0)
      p *= weight;
    else ABSORB;
    SCATTER;
  }
#ifndef NOABSORB_INF_NAN
  /* Check for nan or inf particle parms */ 
  if(isnan(p)  ||  isinf(p)) ABSORB;
  if(isnan(t)  ||  isinf(t)) ABSORB;
  if(isnan(vx) || isinf(vx)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(x)  ||  isinf(x)) ABSORB;
  if(isnan(y)  ||  isinf(y)) ABSORB;
  if(isnan(z)  ||  isinf(z)) ABSORB;
#else
  if(isnan(p)  ||  isinf(p)) printf("NAN or INF found in p,  %s\n",_comp->_name);
  if(isnan(t)  ||  isinf(t)) printf("NAN or INF found in t,  %s\n",_comp->_name);
  if(isnan(vx) || isinf(vx)) printf("NAN or INF found in vx, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vy, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vz, %s\n",_comp->_name);
  if(isnan(x)  ||  isinf(x)) printf("NAN or INF found in x,  %s\n",_comp->_name);
  if(isnan(y)  ||  isinf(y)) printf("NAN or INF found in y,  %s\n",_comp->_name);
  if(isnan(z)  ||  isinf(z)) printf("NAN or INF found in z,  %s\n",_comp->_name);
#endif
  #undef reflect
  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  #undef pTable
  #undef table_present
  return(_comp);
} /* class_Guide_trace */

#pragma acc routine
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
  SIG_MESSAGE("[_As1_trace] component As1=Slit() TRACE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Slit.comp:71]");

    PROP_Z0;
    if (((radius == 0) && (x<xmin || x>xmax || y<ymin || y>ymax))
    || ((radius != 0) && (x*x + y*y > radius*radius)))
      ABSORB;
    else
        SCATTER;
#ifndef NOABSORB_INF_NAN
  /* Check for nan or inf particle parms */ 
  if(isnan(p)  ||  isinf(p)) ABSORB;
  if(isnan(t)  ||  isinf(t)) ABSORB;
  if(isnan(vx) || isinf(vx)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(x)  ||  isinf(x)) ABSORB;
  if(isnan(y)  ||  isinf(y)) ABSORB;
  if(isnan(z)  ||  isinf(z)) ABSORB;
#else
  if(isnan(p)  ||  isinf(p)) printf("NAN or INF found in p,  %s\n",_comp->_name);
  if(isnan(t)  ||  isinf(t)) printf("NAN or INF found in t,  %s\n",_comp->_name);
  if(isnan(vx) || isinf(vx)) printf("NAN or INF found in vx, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vy, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vz, %s\n",_comp->_name);
  if(isnan(x)  ||  isinf(x)) printf("NAN or INF found in x,  %s\n",_comp->_name);
  if(isnan(y)  ||  isinf(y)) printf("NAN or INF found in y,  %s\n",_comp->_name);
  if(isnan(z)  ||  isinf(z)) printf("NAN or INF found in z,  %s\n",_comp->_name);
#endif
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef radius
  #undef xwidth
  #undef yheight
  return(_comp);
} /* class_Slit_trace */

#pragma acc routine
_class_Monochromator_flat *class_Monochromator_flat_trace(_class_Monochromator_flat *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zwidth (_comp->_parameters.zwidth)
  #define yheight (_comp->_parameters.yheight)
  #define mosaich (_comp->_parameters.mosaich)
  #define mosaicv (_comp->_parameters.mosaicv)
  #define r0 (_comp->_parameters.r0)
  #define Q (_comp->_parameters.Q)
  #define DM (_comp->_parameters.DM)
  #define mos_rms_y (_comp->_parameters.mos_rms_y)
  #define mos_rms_z (_comp->_parameters.mos_rms_z)
  #define mos_rms_max (_comp->_parameters.mos_rms_max)
  #define mono_Q (_comp->_parameters.mono_Q)
  SIG_MESSAGE("[_PG1Xtal_trace] component PG1Xtal=Monochromator_flat() TRACE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Monochromator_flat.comp:119]");

  double y1,z1,t1,dt,kix,kiy,kiz,ratio,order,q0x,k,q0,theta;
  double bx,by,bz,kux,kuy,kuz,ax,ay,az,phi;
  double cos_2theta,k_sin_2theta,cos_phi,sin_phi,q_x,q_y,q_z;
  double delta,p_reflect,total,c1x,c1y,c1z,width,mos_sample;
  int i;

  if(vx != 0.0 && (dt = -x/vx) >= 0.0)
  {                             /* Moving towards crystal? */
    y1 = y + vy*dt;             /* Propagate to crystal plane */
    z1 = z + vz*dt;
    t1 = t + dt;
    if (z1>zmin && z1<zmax && y1>ymin && y1<ymax)
    {                           /* Intersect the crystal? */
      kix = V2K*vx;             /* Initial wave vector */
      kiy = V2K*vy;
      kiz = V2K*vz;
      /* Get reflection order and corresponding nominal scattering vector q0
         of correct length and direction. Only the order with the closest
         scattering vector is considered */
      ratio = -2*kix/mono_Q;
      order = floor(ratio + .5);
      if(order == 0.0)
        order = ratio < 0 ? -1 : 1;
      /* Order will be negative when the neutron enters from the back, in
         which case the direction of Q0 is flipped. */
      if(order < 0)
        order = -order;
      /* Make sure the order is small enough to allow Bragg scattering at the
         given neutron wavelength */
      k = sqrt(kix*kix + kiy*kiy + kiz*kiz);
      kux = kix/k;              /* Unit vector along ki */
      kuy = kiy/k;
      kuz = kiz/k;
      if(order > 2*k/mono_Q)
        order--;
      if(order > 0)             /* Bragg scattering possible? */
      {
        q0 = order*mono_Q;
        q0x = ratio < 0 ? -q0 : q0;
        theta = asin(q0/(2*k)); /* Actual bragg angle */
        /* Make MC choice: reflect or transmit? */
        delta = asin(fabs(kux)) - theta;
        p_reflect = r0*exp(-kiy*kiy/(kiy*kiy + kiz*kiz)*(delta*delta)/
                           (2*mos_rms_y*mos_rms_y))*
                       exp(-kiz*kiz/(kiy*kiy + kiz*kiz)*(delta*delta)/
                           (2*mos_rms_z*mos_rms_z));
        if(rand01() < p_reflect)
        {                       /* Reflect */
          cos_2theta = cos(2*theta);
          k_sin_2theta = k*sin(2*theta);
          /* Get unit normal to plane containing ki and most probable kf */
          vec_prod(bx, by, bz, kix, kiy, kiz, q0x, 0, 0);
          NORM(bx,by,bz);
          bx *= k_sin_2theta;
          by *= k_sin_2theta;
          bz *= k_sin_2theta;
          /* Get unit vector normal to ki and b */
          vec_prod(ax, ay, az, bx, by, bz, kux, kuy, kuz);
          /* Compute the total scattering probability at this ki */
          total = 0;
          /* Choose width of Gaussian distribution to sample the angle
           * phi on the Debye-Scherrer cone for the scattered neutron.
           * The radius of the Debye-Scherrer cone is smaller by a
           * factor 1/cos(theta) than the radius of the (partial) sphere
           * describing the possible orientations of Q due to mosaicity, so we
           * start with a width 1/cos(theta) greater than the largest of
           * the two mosaics. */
          mos_sample = mos_rms_max/cos(theta);
          c1x = kix*(cos_2theta-1);
          c1y = kiy*(cos_2theta-1);
          c1z = kiz*(cos_2theta-1);
          /* Loop, repeatedly reducing the sample width until it is small
           * enough to avoid sampling scattering directions with
           * ridiculously low scattering probability.
           * Use a cut-off at 5 times the gauss width for considering
           * scattering probability as well as for integration limits
           * when integrating the sampled distribution below. */
          for(i=0; i<100; i++) {
            width = 5*mos_sample;
            cos_phi = cos(width);
            sin_phi = sin(width);
            q_x = c1x + cos_phi*ax + sin_phi*bx;
            q_y = (c1y + cos_phi*ay + sin_phi*by)/mos_rms_y;
            q_z = (c1z + cos_phi*az + sin_phi*bz)/mos_rms_z;
            /* Stop when we get near a factor of 25=5^2. */
            if(q_z*q_z + q_y*q_y < (25/(2.0/3.0))*(q_x*q_x))
              break;
            mos_sample *= (2.0/3.0);
          }
          /* Now integrate the chosen sampling distribution, using a
           * cut-off at five times sigma. */
          for(i = 0; i < (sizeof(Gauss_X)/sizeof(double)); i++)
          {
            phi = width*Gauss_X[i];
            cos_phi = cos(phi);
            sin_phi = sin(phi);
            q_x = c1x + cos_phi*ax + sin_phi*bx;
            q_y = c1y + cos_phi*ay + sin_phi*by;
            q_z = c1z + cos_phi*az + sin_phi*bz;
            p_reflect = GAUSS((q_y/q_x),0,mos_rms_y)*
                        GAUSS((q_z/q_x),0,mos_rms_z);
            total += Gauss_W[i]*p_reflect;
          }
          total *= width;
          /* Choose point on Debye-Scherrer cone. Sample from a Gaussian of
           * width 1/cos(theta) greater than the mosaic and correct for any
           * error by adjusting the neutron weight later. */
          phi = mos_sample*randnorm();
          /* Compute final wave vector kf and scattering vector q = ki - kf */
          cos_phi = cos(phi);
          sin_phi = sin(phi);
          q_x = c1x + cos_phi*ax + sin_phi*bx;
          q_y = c1y + cos_phi*ay + sin_phi*by;
          q_z = c1z + cos_phi*az + sin_phi*bz;
          p_reflect = GAUSS((q_y/q_x),0,mos_rms_y)*
                      GAUSS((q_z/q_x),0,mos_rms_z);
          x = 0;
          y = y1;
          z = z1;
          t = t1;
          vx = K2V*(kix+q_x);
          vy = K2V*(kiy+q_y);
          vz = K2V*(kiz+q_z);
          p_reflect /= total*GAUSS(phi,0,mos_sample);
          if (p_reflect <= 0) ABSORB;
          if (p_reflect > 1)  p_reflect = 1;
          p *= p_reflect;
          SCATTER;
        } /* End MC choice to reflect or transmit neutron */
      } /* End bragg scattering possible */
    } /* End intersect the crystal */
  } /* End neutron moving towards crystal */
#ifndef NOABSORB_INF_NAN
  /* Check for nan or inf particle parms */ 
  if(isnan(p)  ||  isinf(p)) ABSORB;
  if(isnan(t)  ||  isinf(t)) ABSORB;
  if(isnan(vx) || isinf(vx)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(x)  ||  isinf(x)) ABSORB;
  if(isnan(y)  ||  isinf(y)) ABSORB;
  if(isnan(z)  ||  isinf(z)) ABSORB;
#else
  if(isnan(p)  ||  isinf(p)) printf("NAN or INF found in p,  %s\n",_comp->_name);
  if(isnan(t)  ||  isinf(t)) printf("NAN or INF found in t,  %s\n",_comp->_name);
  if(isnan(vx) || isinf(vx)) printf("NAN or INF found in vx, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vy, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vz, %s\n",_comp->_name);
  if(isnan(x)  ||  isinf(x)) printf("NAN or INF found in x,  %s\n",_comp->_name);
  if(isnan(y)  ||  isinf(y)) printf("NAN or INF found in y,  %s\n",_comp->_name);
  if(isnan(z)  ||  isinf(z)) printf("NAN or INF found in z,  %s\n",_comp->_name);
#endif
  #undef zmin
  #undef zmax
  #undef ymin
  #undef ymax
  #undef zwidth
  #undef yheight
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef DM
  #undef mos_rms_y
  #undef mos_rms_z
  #undef mos_rms_max
  #undef mono_Q
  return(_comp);
} /* class_Monochromator_flat_trace */

#pragma acc routine
_class_V_sample *class_V_sample_trace(_class_V_sample *_comp
  , _class_particle *_particle) {
  ABSORBED=SCATTERED=RESTORE=0;
  #define radius (_comp->_parameters.radius)
  #define thickness (_comp->_parameters.thickness)
  #define zdepth (_comp->_parameters.zdepth)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define radius_i (_comp->_parameters.radius_i)
  #define radius_o (_comp->_parameters.radius_o)
  #define h (_comp->_parameters.h)
  #define focus_r (_comp->_parameters.focus_r)
  #define pack (_comp->_parameters.pack)
  #define frac (_comp->_parameters.frac)
  #define f_QE (_comp->_parameters.f_QE)
  #define gamma (_comp->_parameters.gamma)
  #define target_x (_comp->_parameters.target_x)
  #define target_y (_comp->_parameters.target_y)
  #define target_z (_comp->_parameters.target_z)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define focus_aw (_comp->_parameters.focus_aw)
  #define focus_ah (_comp->_parameters.focus_ah)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zthick (_comp->_parameters.zthick)
  #define rad_sphere (_comp->_parameters.rad_sphere)
  #define sig_a (_comp->_parameters.sig_a)
  #define sig_i (_comp->_parameters.sig_i)
  #define V0 (_comp->_parameters.V0)
  #define target_index (_comp->_parameters.target_index)
  #define multiples (_comp->_parameters.multiples)
  #define VarsV (_comp->_parameters.VarsV)
  SIG_MESSAGE("[_Sample_trace] component Sample=V_sample() TRACE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../obsolete/V_sample.comp:180]");

  double t0, t3;                /* Entry/exit time for outer cylinder */
  double t1, t2;                /* Entry/exit time for inner cylinder */
  double v;                     /* Neutron velocity */
  double dt0, dt1, dt2, dt;     /* Flight times through sample */
  double l_full;                /* Flight path length for non-scattered neutron */
  double l_i, l_o=0;            /* Flight path lenght in/out for scattered neutron */
  double my_a=0;                  /* Velocity-dependent attenuation factor */
  double solid_angle=0;         /* Solid angle of target as seen from scattering point */
  double aim_x=0, aim_y=0, aim_z=1;   /* Position of target relative to scattering point */
  double v_i, v_f, E_i, E_f; /* initial and final energies and velocities */
  double dE;                 /* Energy transfer */
  int    intersect=0;

  if (VarsV.shapetyp == 2)
    intersect = sphere_intersect(&t0, &t3, x, y, z, vx, vy, vz, rad_sphere);
  else
    if (VarsV.shapetyp == 1)
      intersect = box_intersect(&t0, &t3, x, y, z, vx, vy, vz, xwidth, yheight, zthick);
  else
    intersect = cylinder_intersect(&t0, &t3, x, y, z, vx, vy, vz, radius_o, h);
  if(intersect)
  {
    if(t0 < 0) ABSORB; /* we already passed the sample; this is illegal */
    /* Neutron enters at t=t0. */
    if(VarsV.shapetyp == 1 || VarsV.shapetyp == 2)
      t1 = t2 = t3;
    else
      if(!radius_i || !cylinder_intersect(&t1, &t2, x, y, z, vx, vy, vz, radius_i, h))
        t1 = t2 = t3;

    dt0 = t1-t0;                /* Time in sample, ingoing */
    dt1 = t2-t1;                /* Time in hole */
    dt2 = t3-t2;                /* Time in sample, outgoing */
    v = sqrt(vx*vx + vy*vy + vz*vz);
    l_full = v * (dt0 + dt2);   /* Length of full path through sample */
    if (v) my_a = VarsV.my_a_v*(2200/v);

    if (frac >= 1 || rand01()<frac)          /* Scattering */
    {
      dt = rand01()*(dt0+dt2);    /* Time of scattering (relative to t0) */
      l_i = v*dt;                 /* Penetration in sample: scattering+abs */
      if (dt > dt0)
        dt += dt1;                /* jump to 2nd side of cylinder */

      PROP_DT(dt+t0);             /* Point of scattering */

      if ((VarsV.tx || VarsV.ty || VarsV.tz)) {
        aim_x = VarsV.tx-x;       /* Vector pointing at target (anal./det.) */
        aim_y = VarsV.ty-y;
        aim_z = VarsV.tz-z;
      }
      if(VarsV.aw && VarsV.ah) {
        randvec_target_rect_angular(&vx, &vy, &vz, &solid_angle,
          aim_x, aim_y, aim_z, VarsV.aw, VarsV.ah, ROT_A_CURRENT_COMP);
      } else if(VarsV.xw && VarsV.yh) {
        randvec_target_rect(&vx, &vy, &vz, &solid_angle,
          aim_x, aim_y, aim_z, VarsV.xw, VarsV.yh, ROT_A_CURRENT_COMP);
      } else {
        randvec_target_circle(&vx, &vy, &vz, &solid_angle, aim_x, aim_y, aim_z, focus_r);
      }
      NORM(vx, vy, vz);

      v_i = v;          /* Store initial velocity in case of quasielastic */
      if (rand01()<f_QE)	/* Quasielastic contribution */
	{
          E_i = VS2E*v_i*v_i;
          dE = gamma*tan(PI/2*randpm1());
          E_f = E_i + dE;
          if (E_f <= 0)
            ABSORB;
	  v_f = SE2V*sqrt(E_f);
          v = v_f;
	  /*          printf("vi: %g Ei: %g dE: %g Ef %g vf: %g v: %g \n",
		      v_i,E_i,dE,E_f,v_f,v); */
	}

      vx *= v;
      vy *= v;
      vz *= v;

      if(VarsV.shapetyp == 0) {
        if(!cylinder_intersect(&t0, &t3, x, y, z, vx, vy, vz, radius_o, h)) {
          /* ??? did not hit cylinder */
          printf("FATAL ERROR: Did not hit cylinder from inside.\n");
          //exit(1);
        }
        dt = t3; /* outgoing point */
        if(cylinder_intersect(&t1, &t2, x, y, z, vx, vy, vz, radius_i, h) &&
           t2 > 0)
          dt -= (t2-t1);            /* Subtract hollow part */
      }
      else {
        if(VarsV.shapetyp == 1) {
	      if(!box_intersect(&t0, &t3, x, y, z, vx, vy, vz, xwidth, yheight, zthick)) {
            /* ??? did not hit box */
            printf("FATAL ERROR: Did not hit box from inside.\n");
            //exit(1);
          }
          dt = t3;
        }
        else {
	      if(!sphere_intersect(&t0, &t3, x, y, z, vx, vy, vz, rad_sphere)) {
            /* ??? did not hit sphere */
            printf("FATAL ERROR: Did not hit sphere from inside.\n");
            //exit(1);
          }
          dt = t3;  
        }
      }
      l_o = v*dt; /* trajectory after scattering point: absorption only */

      p *= v/v_i*l_full*VarsV.my_s*exp(-my_a*(l_i+v_i/v*l_o)-VarsV.my_s*l_i);
      if (!multiples) {
	/* If no "multiples", correct by applying scattering cross-sec and
	   implicitly "absorb" further scattering (as in PowderN) 
	   We are currently (august 2007) having a debate on which solution 
	   is the most reasonable */
	p *= exp(-VarsV.my_s*l_o);
      }
      /* We do not consider scattering from 2nd part (outgoing) */
      p /= 4*PI/solid_angle;
      p /= frac;

      /* Polarisation part (1/3 NSF, 2/3 SF) */
      sx *= -1.0/3.0;
      sy *= -1.0/3.0;
      sz *= -1.0/3.0;

      SCATTER;
    }
    else /* Transmitting; always elastic */
    {
      p *= exp(-(my_a+VarsV.my_s)*l_full);
      p /= (1-frac);
    }
  }
#ifndef NOABSORB_INF_NAN
  /* Check for nan or inf particle parms */ 
  if(isnan(p)  ||  isinf(p)) ABSORB;
  if(isnan(t)  ||  isinf(t)) ABSORB;
  if(isnan(vx) || isinf(vx)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(vy) || isinf(vy)) ABSORB;
  if(isnan(x)  ||  isinf(x)) ABSORB;
  if(isnan(y)  ||  isinf(y)) ABSORB;
  if(isnan(z)  ||  isinf(z)) ABSORB;
#else
  if(isnan(p)  ||  isinf(p)) printf("NAN or INF found in p,  %s\n",_comp->_name);
  if(isnan(t)  ||  isinf(t)) printf("NAN or INF found in t,  %s\n",_comp->_name);
  if(isnan(vx) || isinf(vx)) printf("NAN or INF found in vx, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vy, %s\n",_comp->_name);
  if(isnan(vy) || isinf(vy)) printf("NAN or INF found in vz, %s\n",_comp->_name);
  if(isnan(x)  ||  isinf(x)) printf("NAN or INF found in x,  %s\n",_comp->_name);
  if(isnan(y)  ||  isinf(y)) printf("NAN or INF found in y,  %s\n",_comp->_name);
  if(isnan(z)  ||  isinf(z)) printf("NAN or INF found in z,  %s\n",_comp->_name);
#endif
  #undef radius
  #undef thickness
  #undef zdepth
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef radius_i
  #undef radius_o
  #undef h
  #undef focus_r
  #undef pack
  #undef frac
  #undef f_QE
  #undef gamma
  #undef target_x
  #undef target_y
  #undef target_z
  #undef focus_xw
  #undef focus_yh
  #undef focus_aw
  #undef focus_ah
  #undef xwidth
  #undef yheight
  #undef zthick
  #undef rad_sphere
  #undef sig_a
  #undef sig_i
  #undef V0
  #undef target_index
  #undef multiples
  #undef VarsV
  return(_comp);
} /* class_V_sample_trace */

/* *****************************************************************************
* instrument 'BNL_H8' TRACE
***************************************************************************** */

#ifndef FUNNEL
#pragma acc routine
int raytrace(_class_particle* _particle) { /* single event propagation, called by mccode_main for BNL_H8:TRACE */

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
    /* begin component Origin=Progress_bar() [1] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_Origin_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _Origin_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_Origin_var._position_relative, _Origin_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 1) {
      _particle_save = *_particle;
      DEBUG_COMP(_Origin_var._name);
      DEBUG_STATE();
      class_Progress_bar_trace(&_Origin_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Origin [1] */
    /* begin component Source=Source_simple() [2] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_Source_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _Source_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_Source_var._position_relative, _Source_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 2) {
      _particle_save = *_particle;
      DEBUG_COMP(_Source_var._name);
      DEBUG_STATE();
      class_Source_simple_trace(&_Source_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Source [2] */
    /* begin component D0_Source=PSD_monitor() [3] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_D0_Source_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _D0_Source_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_D0_Source_var._position_relative, _D0_Source_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 3) {
      _particle_save = *_particle;
      DEBUG_COMP(_D0_Source_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D0_Source_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D0_Source [3] */
    /* begin component SC1=Guide() [4] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_SC1_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _SC1_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_SC1_var._position_relative, _SC1_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 4) {
      _particle_save = *_particle;
      DEBUG_COMP(_SC1_var._name);
      DEBUG_STATE();
      class_Guide_trace(&_SC1_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component SC1 [4] */
    /* begin component D1_SC1_Out=PSD_monitor() [5] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_D1_SC1_Out_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _D1_SC1_Out_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_D1_SC1_Out_var._position_relative, _D1_SC1_Out_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 5) {
      _particle_save = *_particle;
      DEBUG_COMP(_D1_SC1_Out_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D1_SC1_Out_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D1_SC1_Out [5] */
    /* begin component As1=Slit() [6] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_As1_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _As1_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_As1_var._position_relative, _As1_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 6) {
      _particle_save = *_particle;
      DEBUG_COMP(_As1_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_As1_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component As1 [6] */
    /* begin component As2=Slit() [7] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_As2_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _As2_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_As2_var._position_relative, _As2_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 7) {
      _particle_save = *_particle;
      DEBUG_COMP(_As2_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_As2_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component As2 [7] */
    /* begin component As3=Slit() [8] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_As3_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _As3_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_As3_var._position_relative, _As3_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 8) {
      _particle_save = *_particle;
      DEBUG_COMP(_As3_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_As3_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component As3 [8] */
    /* begin component As4=Slit() [9] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_As4_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _As4_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_As4_var._position_relative, _As4_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 9) {
      _particle_save = *_particle;
      DEBUG_COMP(_As4_var._name);
      DEBUG_STATE();
      class_Slit_trace(&_As4_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component As4 [9] */
    /* begin component D2_A4=PSD_monitor() [10] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_D2_A4_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _D2_A4_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_D2_A4_var._position_relative, _D2_A4_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 10) {
      _particle_save = *_particle;
      DEBUG_COMP(_D2_A4_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D2_A4_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D2_A4 [10] */
    /* begin component Mono_Cradle=Arm() [11] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_Mono_Cradle_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _Mono_Cradle_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_Mono_Cradle_var._position_relative, _Mono_Cradle_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 11) {
      _particle_save = *_particle;
      DEBUG_COMP(_Mono_Cradle_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Mono_Cradle [11] */
#ifndef NOSPLIT
    /* start SPLIT at PG1Xtal */
    if (!ABSORBED) {
    _class_particle Split_PG1Xtal_particle=*_particle;
    int Split_PG1Xtal_counter;
    int SplitS_PG1Xtal = 10;
    for (Split_PG1Xtal_counter = 0; Split_PG1Xtal_counter< SplitS_PG1Xtal; Split_PG1Xtal_counter++) {
      randstate_t randbackup = *_particle->randstate;
      *_particle=Split_PG1Xtal_particle;
      *_particle->randstate = randbackup;
      srandom(_hash(_particle->_uid* 12 *(1+Split_PG1Xtal_counter)));
      p /= SplitS_PG1Xtal > 0 ? SplitS_PG1Xtal : 1;
#endif
    /* begin component PG1Xtal=Monochromator_flat() [12] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_PG1Xtal_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _PG1Xtal_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_PG1Xtal_var._position_relative, _PG1Xtal_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 12) {
      _particle_save = *_particle;
      DEBUG_COMP(_PG1Xtal_var._name);
      DEBUG_STATE();
      class_Monochromator_flat_trace(&_PG1Xtal_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component PG1Xtal [12] */
    /* begin component Mono_Out=Arm() [13] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_Mono_Out_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _Mono_Out_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_Mono_Out_var._position_relative, _Mono_Out_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 13) {
      _particle_save = *_particle;
      DEBUG_COMP(_Mono_Out_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Mono_Out [13] */
    /* begin component D4_SC2_In=PSD_monitor() [14] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_D4_SC2_In_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _D4_SC2_In_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_D4_SC2_In_var._position_relative, _D4_SC2_In_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 14) {
      _particle_save = *_particle;
      DEBUG_COMP(_D4_SC2_In_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D4_SC2_In_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D4_SC2_In [14] */
    /* begin component SC2=Guide() [15] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_SC2_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _SC2_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_SC2_var._position_relative, _SC2_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 15) {
      _particle_save = *_particle;
      DEBUG_COMP(_SC2_var._name);
      DEBUG_STATE();
      class_Guide_trace(&_SC2_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component SC2 [15] */
    /* begin component D5_SC2_Out=PSD_monitor() [16] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_D5_SC2_Out_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _D5_SC2_Out_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_D5_SC2_Out_var._position_relative, _D5_SC2_Out_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 16) {
      _particle_save = *_particle;
      DEBUG_COMP(_D5_SC2_Out_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D5_SC2_Out_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D5_SC2_Out [16] */
    /* begin component Sample_Cradle=Arm() [17] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_Sample_Cradle_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _Sample_Cradle_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_Sample_Cradle_var._position_relative, _Sample_Cradle_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 17) {
      _particle_save = *_particle;
      DEBUG_COMP(_Sample_Cradle_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Sample_Cradle [17] */
    /* begin component Sample_Out=Arm() [18] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_Sample_Out_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _Sample_Out_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_Sample_Out_var._position_relative, _Sample_Out_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 18) {
      _particle_save = *_particle;
      DEBUG_COMP(_Sample_Out_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Sample_Out [18] */
#ifndef NOSPLIT
    /* start SPLIT at Sample */
    if (!ABSORBED) {
    _class_particle Split_Sample_particle=*_particle;
    int Split_Sample_counter;
    int SplitS_Sample = 10;
    for (Split_Sample_counter = 0; Split_Sample_counter< SplitS_Sample; Split_Sample_counter++) {
      randstate_t randbackup = *_particle->randstate;
      *_particle=Split_Sample_particle;
      *_particle->randstate = randbackup;
      srandom(_hash(_particle->_uid* 19 *(1+Split_Sample_counter)));
      p /= SplitS_Sample > 0 ? SplitS_Sample : 1;
#endif
    /* begin component Sample=V_sample() [19] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_Sample_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _Sample_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_Sample_var._position_relative, _Sample_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 19) {
      _particle_save = *_particle;
      DEBUG_COMP(_Sample_var._name);
      DEBUG_STATE();
      class_V_sample_trace(&_Sample_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Sample [19] */
    /* begin component D7_SC3_In=PSD_monitor() [20] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_D7_SC3_In_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _D7_SC3_In_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_D7_SC3_In_var._position_relative, _D7_SC3_In_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 20) {
      _particle_save = *_particle;
      DEBUG_COMP(_D7_SC3_In_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D7_SC3_In_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D7_SC3_In [20] */
    /* begin component SC3=Guide() [21] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_SC3_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _SC3_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_SC3_var._position_relative, _SC3_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 21) {
      _particle_save = *_particle;
      DEBUG_COMP(_SC3_var._name);
      DEBUG_STATE();
      class_Guide_trace(&_SC3_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component SC3 [21] */
    /* begin component D8_SC3_Out=PSD_monitor() [22] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_D8_SC3_Out_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _D8_SC3_Out_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_D8_SC3_Out_var._position_relative, _D8_SC3_Out_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 22) {
      _particle_save = *_particle;
      DEBUG_COMP(_D8_SC3_Out_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D8_SC3_Out_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D8_SC3_Out [22] */
    /* begin component Ana_Cradle=Arm() [23] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_Ana_Cradle_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _Ana_Cradle_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_Ana_Cradle_var._position_relative, _Ana_Cradle_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 23) {
      _particle_save = *_particle;
      DEBUG_COMP(_Ana_Cradle_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Ana_Cradle [23] */
#ifndef NOSPLIT
    /* start SPLIT at PG2Xtal */
    if (!ABSORBED) {
    _class_particle Split_PG2Xtal_particle=*_particle;
    int Split_PG2Xtal_counter;
    int SplitS_PG2Xtal = 10;
    for (Split_PG2Xtal_counter = 0; Split_PG2Xtal_counter< SplitS_PG2Xtal; Split_PG2Xtal_counter++) {
      randstate_t randbackup = *_particle->randstate;
      *_particle=Split_PG2Xtal_particle;
      *_particle->randstate = randbackup;
      srandom(_hash(_particle->_uid* 24 *(1+Split_PG2Xtal_counter)));
      p /= SplitS_PG2Xtal > 0 ? SplitS_PG2Xtal : 1;
#endif
    /* begin component PG2Xtal=Monochromator_flat() [24] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_PG2Xtal_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _PG2Xtal_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_PG2Xtal_var._position_relative, _PG2Xtal_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 24) {
      _particle_save = *_particle;
      DEBUG_COMP(_PG2Xtal_var._name);
      DEBUG_STATE();
      class_Monochromator_flat_trace(&_PG2Xtal_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component PG2Xtal [24] */
    /* begin component Ana_Out=Arm() [25] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_Ana_Out_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _Ana_Out_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_Ana_Out_var._position_relative, _Ana_Out_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 25) {
      _particle_save = *_particle;
      DEBUG_COMP(_Ana_Out_var._name);
      DEBUG_STATE();
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component Ana_Out [25] */
    /* begin component D10_SC4_In=PSD_monitor() [26] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_D10_SC4_In_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _D10_SC4_In_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_D10_SC4_In_var._position_relative, _D10_SC4_In_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 26) {
      _particle_save = *_particle;
      DEBUG_COMP(_D10_SC4_In_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_D10_SC4_In_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component D10_SC4_In [26] */
    /* begin component SC4=Guide() [27] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_SC4_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _SC4_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_SC4_var._position_relative, _SC4_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 27) {
      _particle_save = *_particle;
      DEBUG_COMP(_SC4_var._name);
      DEBUG_STATE();
      class_Guide_trace(&_SC4_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component SC4 [27] */
    /* begin component He3H=PSD_monitor() [28] */
    if (!flag_nocoordschange) { // flag activated by JUMP to pass coords change
      if (_He3H_var._rotation_is_identity) {
        coords_get(coords_add(coords_set(x,y,z), _He3H_var._position_relative),&x, &y, &z);
      } else
          mccoordschange(_He3H_var._position_relative, _He3H_var._rotation_relative, _particle);
    } else flag_nocoordschange=0;
    if (!ABSORBED && _particle->_index == 28) {
      _particle_save = *_particle;
      DEBUG_COMP(_He3H_var._name);
      DEBUG_STATE();
      class_PSD_monitor_trace(&_He3H_var, _particle);
      if (_particle->_restore)
        *_particle = _particle_save;
      _particle->_index++;
      if (!ABSORBED) DEBUG_STATE();
    } /* end component He3H [28] */
#ifndef NOSPLIT
    } /* end SPLIT at PG2Xtal */
    } /* if (!ABSORBED) relating to SPLIT at PG2Xtal */
#endif
#ifndef NOSPLIT
    } /* end SPLIT at Sample */
    } /* if (!ABSORBED) relating to SPLIT at Sample */
#endif
#ifndef NOSPLIT
    } /* end SPLIT at PG1Xtal */
    } /* if (!ABSORBED) relating to SPLIT at PG1Xtal */
#endif
    if (_particle->_index > 28)
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
  loops = ceil((double)ncount/gpu_innerloop);
  /* if on GPU, printf has been globally nullified, re-enable here */
  #ifdef OPENACC
  #ifndef MULTICORE
  #undef printf
  #endif
  #endif

  #ifdef OPENACC
  if (ncount>gpu_innerloop) {
    printf("Defining %llu CPU loops around GPU kernel and adjusting ncount\n",loops);
    mcset_ncount(loops*gpu_innerloop);
  } else {
    #endif
    loops=1;
    gpu_innerloop = ncount;
    #ifdef OPENACC
  }
    #endif

  for (unsigned long long cloop=0; cloop<loops; cloop++) {
    #ifdef OPENACC
    if (loops>1) fprintf(stdout, "%d..", (int)cloop); fflush(stdout);
    #endif

    /* if on GPU, re-nullify printf */
    #ifdef OPENACC
    #ifndef MULTICORE
    #define printf(...) noprintf()
    #endif
    #endif

    #pragma acc parallel loop num_gangs(numgangs) vector_length(vecsize)
    for (unsigned long pidx=0 ; pidx < gpu_innerloop ; pidx++) {
      _class_particle particleN = mcgenstate(); // initial particle
      _class_particle* _particle = &particleN;
      particleN._uid = pidx;

      srandom(_hash((pidx+1)*(seed+1)));
      particle_uservar_init(_particle);

      raytrace(_particle);
    } /* inner for */
    seed = seed+gpu_innerloop;
    mcncount += gpu_innerloop;
  } /* CPU for */
  /* if on GPU, printf has been globally nullified, re-enable here */
  #ifdef OPENACC
  #ifndef MULTICORE
  #undef printf
  #endif
  #endif
  MPI_MASTER(
  printf("*** TRACE end *** \n");
  );
} /* raytrace_all */

#endif //no-FUNNEL

#ifdef FUNNEL
// Alternative raytrace algorithm which iterates all particles through
// one component at the time, can remove absorbs from the next loop and
// switch between cpu/gpu.
void raytrace_all_funnel(unsigned long long ncount, unsigned long seed) {

  // set up outer (CPU) loop / particle batches
  unsigned long long loops;

  /* if on GPU, printf has been globally nullified, re-enable here */
  #ifdef OPENACC
  #ifndef MULTICORE
  #undef printf
  #endif
  #endif

  #ifdef OPENACC
  loops = ceil((double)ncount/gpu_innerloop);
  if (ncount>gpu_innerloop) {
    printf("Defining %llu CPU loops around kernel and adjusting ncount\n",loops);
    mcset_ncount(loops*gpu_innerloop);
  } else {
  #endif
    loops=1;
    gpu_innerloop = ncount;
  #ifdef OPENACC
  }
  #endif

  // create particles struct and pointer arrays (same memory used by all batches)
  _class_particle* particles = malloc(gpu_innerloop*sizeof(_class_particle));
  _class_particle* pbuffer = malloc(gpu_innerloop*sizeof(_class_particle));
  long livebatchsize = gpu_innerloop;

  #undef ABSORB0
  #undef ABSORB
  #define ABSORB0 do { DEBUG_ABSORB(); MAGNET_OFF; ABSORBED++; } while(0)
  #define ABSORB ABSORB0
  // outer loop / particle batches
  for (unsigned long long cloop=0; cloop<loops; cloop++) {
    if (loops>1) fprintf(stdout, "%d..", (int)cloop); fflush(stdout);

    // init particles
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      // generate particle state, set loop index and seed
      particles[pidx] = mcgenstate();
      _class_particle* _particle = particles + pidx;
      _particle->_uid = pidx;
      srandom(_hash((pidx+1)*(seed+1))); // _particle->state usage built into srandom macro
      particle_uservar_init(_particle);
    }

    // iterate components

    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = &particles[pidx];
      _class_particle _particle_save;

      // Origin
    if (!ABSORBED && _particle->_index == 1) {
        if (_Origin_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Origin_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Origin_var._position_relative, _Origin_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Progress_bar_trace(&_Origin_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // Source
    if (!ABSORBED && _particle->_index == 2) {
        if (_Source_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Source_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Source_var._position_relative, _Source_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Source_simple_trace(&_Source_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // D0_Source
    if (!ABSORBED && _particle->_index == 3) {
        if (_D0_Source_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _D0_Source_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_D0_Source_var._position_relative, _D0_Source_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_D0_Source_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // SC1
    if (!ABSORBED && _particle->_index == 4) {
        if (_SC1_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _SC1_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_SC1_var._position_relative, _SC1_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Guide_trace(&_SC1_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // D1_SC1_Out
    if (!ABSORBED && _particle->_index == 5) {
        if (_D1_SC1_Out_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _D1_SC1_Out_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_D1_SC1_Out_var._position_relative, _D1_SC1_Out_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_D1_SC1_Out_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // As1
    if (!ABSORBED && _particle->_index == 6) {
        if (_As1_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _As1_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_As1_var._position_relative, _As1_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_As1_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // As2
    if (!ABSORBED && _particle->_index == 7) {
        if (_As2_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _As2_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_As2_var._position_relative, _As2_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_As2_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // As3
    if (!ABSORBED && _particle->_index == 8) {
        if (_As3_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _As3_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_As3_var._position_relative, _As3_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_As3_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // As4
    if (!ABSORBED && _particle->_index == 9) {
        if (_As4_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _As4_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_As4_var._position_relative, _As4_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Slit_trace(&_As4_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // D2_A4
    if (!ABSORBED && _particle->_index == 10) {
        if (_D2_A4_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _D2_A4_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_D2_A4_var._position_relative, _D2_A4_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_D2_A4_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // Mono_Cradle
    if (!ABSORBED && _particle->_index == 11) {
        if (_Mono_Cradle_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Mono_Cradle_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Mono_Cradle_var._position_relative, _Mono_Cradle_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }
    }

    // SPLIT with available livebatchsize 
    long mult_PG1Xtal;
    livebatchsize = sort_absorb_last(particles, pbuffer, livebatchsize, gpu_innerloop, 1, &mult_PG1Xtal);
    //printf("livebatchsize: %ld, split: %ld\n",  livebatchsize, mult);

    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = &particles[pidx];
      _class_particle _particle_save;

      // PG1Xtal
    if (!ABSORBED && _particle->_index == 12) {
        if (_PG1Xtal_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _PG1Xtal_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_PG1Xtal_var._position_relative, _PG1Xtal_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Monochromator_flat_trace(&_PG1Xtal_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // Mono_Out
    if (!ABSORBED && _particle->_index == 13) {
        if (_Mono_Out_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Mono_Out_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Mono_Out_var._position_relative, _Mono_Out_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }

      // D4_SC2_In
    if (!ABSORBED && _particle->_index == 14) {
        if (_D4_SC2_In_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _D4_SC2_In_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_D4_SC2_In_var._position_relative, _D4_SC2_In_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_D4_SC2_In_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // SC2
    if (!ABSORBED && _particle->_index == 15) {
        if (_SC2_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _SC2_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_SC2_var._position_relative, _SC2_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Guide_trace(&_SC2_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // D5_SC2_Out
    if (!ABSORBED && _particle->_index == 16) {
        if (_D5_SC2_Out_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _D5_SC2_Out_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_D5_SC2_Out_var._position_relative, _D5_SC2_Out_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_D5_SC2_Out_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // Sample_Cradle
    if (!ABSORBED && _particle->_index == 17) {
        if (_Sample_Cradle_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Sample_Cradle_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Sample_Cradle_var._position_relative, _Sample_Cradle_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }

      // Sample_Out
    if (!ABSORBED && _particle->_index == 18) {
        if (_Sample_Out_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Sample_Out_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Sample_Out_var._position_relative, _Sample_Out_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }
    }

    // SPLIT with available livebatchsize 
    long mult_Sample;
    livebatchsize = sort_absorb_last(particles, pbuffer, livebatchsize, gpu_innerloop, 1, &mult_Sample);
    //printf("livebatchsize: %ld, split: %ld\n",  livebatchsize, mult);

    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = &particles[pidx];
      _class_particle _particle_save;

      // Sample
    if (!ABSORBED && _particle->_index == 19) {
        if (_Sample_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Sample_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Sample_var._position_relative, _Sample_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_V_sample_trace(&_Sample_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // D7_SC3_In
    if (!ABSORBED && _particle->_index == 20) {
        if (_D7_SC3_In_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _D7_SC3_In_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_D7_SC3_In_var._position_relative, _D7_SC3_In_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_D7_SC3_In_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // SC3
    if (!ABSORBED && _particle->_index == 21) {
        if (_SC3_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _SC3_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_SC3_var._position_relative, _SC3_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Guide_trace(&_SC3_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // D8_SC3_Out
    if (!ABSORBED && _particle->_index == 22) {
        if (_D8_SC3_Out_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _D8_SC3_Out_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_D8_SC3_Out_var._position_relative, _D8_SC3_Out_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_D8_SC3_Out_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // Ana_Cradle
    if (!ABSORBED && _particle->_index == 23) {
        if (_Ana_Cradle_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Ana_Cradle_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Ana_Cradle_var._position_relative, _Ana_Cradle_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }
    }

    // SPLIT with available livebatchsize 
    long mult_PG2Xtal;
    livebatchsize = sort_absorb_last(particles, pbuffer, livebatchsize, gpu_innerloop, 1, &mult_PG2Xtal);
    //printf("livebatchsize: %ld, split: %ld\n",  livebatchsize, mult);

    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = &particles[pidx];
      _class_particle _particle_save;

      // PG2Xtal
    if (!ABSORBED && _particle->_index == 24) {
        if (_PG2Xtal_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _PG2Xtal_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_PG2Xtal_var._position_relative, _PG2Xtal_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Monochromator_flat_trace(&_PG2Xtal_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // Ana_Out
    if (!ABSORBED && _particle->_index == 25) {
        if (_Ana_Out_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _Ana_Out_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_Ana_Out_var._position_relative, _Ana_Out_var._rotation_relative, _particle);
        _particle_save = *_particle;
        _particle->_index++;
      }

      // D10_SC4_In
    if (!ABSORBED && _particle->_index == 26) {
        if (_D10_SC4_In_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _D10_SC4_In_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_D10_SC4_In_var._position_relative, _D10_SC4_In_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_D10_SC4_In_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // SC4
    if (!ABSORBED && _particle->_index == 27) {
        if (_SC4_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _SC4_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_SC4_var._position_relative, _SC4_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_Guide_trace(&_SC4_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

      // He3H
    if (!ABSORBED && _particle->_index == 28) {
        if (_He3H_var._rotation_is_identity)
          coords_get(coords_add(coords_set(x,y,z), _He3H_var._position_relative),&x, &y, &z);
        else
          mccoordschange(_He3H_var._position_relative, _He3H_var._rotation_relative, _particle);
        _particle_save = *_particle;
        class_PSD_monitor_trace(&_He3H_var, _particle);
        if (_particle->_restore)
          *_particle = _particle_save;
        _particle->_index++;
      }

    }

    // finalize particle batch
    #pragma acc parallel loop present(particles)
    for (unsigned long pidx=0 ; pidx < livebatchsize ; pidx++) {
      _class_particle* _particle = &particles[pidx];
      if (_particle->_index > 4)
        ABSORBED++; /* absorbed when passed all components */
    }

    // jump to next viable seed
    seed = seed + gpu_innerloop;
  } // outer loop / particle batches

  free(particles);
  free(pbuffer);

  printf("\n");
} /* raytrace_all_funnel */
#endif // FUNNEL

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
#undef mcgravitation
#undef mcMagnet
#undef allow_backprop
#ifdef OPENACC
#ifndef MULTICORE
#undef strlen
#undef strcmp
#undef exit
#undef printf
#undef sprintf
#undef fprintf
#endif
#endif
#undef SCATTERED
#undef RESTORE
#undef RESTORE_NEUTRON
#undef STORE_NEUTRON
#undef ABSORBED
#undef ABSORB
#undef ABSORB0
/* *****************************************************************************
* instrument 'BNL_H8' and components SAVE
***************************************************************************** */

_class_Progress_bar *class_Progress_bar_save(_class_Progress_bar *_comp
) {
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_Origin_save] component Origin=Progress_bar() SAVE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../misc/Progress_bar.comp:120]");

  MPI_MASTER(fprintf(stdout, "\nSave [%s]\n", instrument_name););
  if (profile && strlen(profile) && strcmp(profile,"NULL") && strcmp(profile,"0")) {
    char filename[256];
    if (!strlen(profile) || !strcmp(profile,"NULL") || !strcmp(profile,"0")) strcpy(filename, instrument_name);
    else strcpy(filename, profile);
    DETECTOR_OUT_1D(
        "Intensity profiler",
        "Component index [1]",
        "Intensity",
        "prof", 1, mcNUMCOMP, mcNUMCOMP-1,
        &(instrument->counter_N[1]),&(instrument->counter_P[1]),&(instrument->counter_P2[1]),
        filename);

  }
  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_save */

_class_PSD_monitor *class_PSD_monitor_save(_class_PSD_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define ny (_comp->_parameters.ny)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define nowritefile (_comp->_parameters.nowritefile)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_D0_Source_save] component D0_Source=PSD_monitor() SAVE [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:104]");

    if (!nowritefile) {
      DETECTOR_OUT_2D(
          "PSD monitor",
          "X position [cm]",
          "Y position [cm]",
          xmin*100.0, xmax*100.0, ymin*100.0, ymax*100.0,
          nx, ny,
          &PSD_N[0][0],&PSD_p[0][0],&PSD_p2[0][0],
          filename);
    }
  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef nowritefile
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_save */



int save(FILE *handle) { /* called by mccode_main for BNL_H8:SAVE */
  if (!handle) siminfo_init(NULL);

  /* call iteratively all components SAVE */
  class_Progress_bar_save(&_Origin_var);


  class_PSD_monitor_save(&_D0_Source_var);


  class_PSD_monitor_save(&_D1_SC1_Out_var);





  class_PSD_monitor_save(&_D2_A4_var);




  class_PSD_monitor_save(&_D4_SC2_In_var);


  class_PSD_monitor_save(&_D5_SC2_Out_var);




  class_PSD_monitor_save(&_D7_SC3_In_var);


  class_PSD_monitor_save(&_D8_SC3_Out_var);




  class_PSD_monitor_save(&_D10_SC4_In_var);


  class_PSD_monitor_save(&_He3H_var);

  if (!handle) siminfo_close(); 

  return(0);
} /* save */

/* *****************************************************************************
* instrument 'BNL_H8' and components FINALLY
***************************************************************************** */

_class_Progress_bar *class_Progress_bar_finally(_class_Progress_bar *_comp
) {
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_Origin_finally] component Origin=Progress_bar() FINALLY [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../misc/Progress_bar.comp:138]");

  time_t NowTime;
  time(&NowTime);
  fprintf(stdout, "\nFinally [%s: %s]. Time: ", instrument_name, dirname ? dirname : ".");
  if (difftime(NowTime,StartTime) < 60.0)
    fprintf(stdout, "%g [s] ", difftime(NowTime,StartTime));
  else if (difftime(NowTime,StartTime) > 3600.0)
    fprintf(stdout, "%g [h] ", difftime(NowTime,StartTime)/3660.0);
  else
    fprintf(stdout, "%g [min] ", difftime(NowTime,StartTime)/60.0);
  fprintf(stdout, "\n");
  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_finally */



int finally(void) { /* called by mccode_main for BNL_H8:FINALLY */
#pragma acc update host(_Origin_var)
#pragma acc update host(_Source_var)
#pragma acc update host(_D0_Source_var)
#pragma acc update host(_SC1_var)
#pragma acc update host(_D1_SC1_Out_var)
#pragma acc update host(_As1_var)
#pragma acc update host(_As2_var)
#pragma acc update host(_As3_var)
#pragma acc update host(_As4_var)
#pragma acc update host(_D2_A4_var)
#pragma acc update host(_Mono_Cradle_var)
#pragma acc update host(_PG1Xtal_var)
#pragma acc update host(_Mono_Out_var)
#pragma acc update host(_D4_SC2_In_var)
#pragma acc update host(_SC2_var)
#pragma acc update host(_D5_SC2_Out_var)
#pragma acc update host(_Sample_Cradle_var)
#pragma acc update host(_Sample_Out_var)
#pragma acc update host(_Sample_var)
#pragma acc update host(_D7_SC3_In_var)
#pragma acc update host(_SC3_var)
#pragma acc update host(_D8_SC3_Out_var)
#pragma acc update host(_Ana_Cradle_var)
#pragma acc update host(_PG2Xtal_var)
#pragma acc update host(_Ana_Out_var)
#pragma acc update host(_D10_SC4_In_var)
#pragma acc update host(_SC4_var)
#pragma acc update host(_He3H_var)
#pragma acc update host(_instrument_var)

  siminfo_init(NULL);
  save(siminfo_file); /* save data when simulation ends */

  /* call iteratively all components FINALLY */
  class_Progress_bar_finally(&_Origin_var);




























  siminfo_close(); 

  return(0);
} /* finally */

/* *****************************************************************************
* instrument 'BNL_H8' and components DISPLAY
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
_class_Progress_bar *class_Progress_bar_display(_class_Progress_bar *_comp
) {
  #define profile (_comp->_parameters.profile)
  #define percent (_comp->_parameters.percent)
  #define flag_save (_comp->_parameters.flag_save)
  #define minutes (_comp->_parameters.minutes)
  #define IntermediateCnts (_comp->_parameters.IntermediateCnts)
  #define StartTime (_comp->_parameters.StartTime)
  #define EndTime (_comp->_parameters.EndTime)
  #define CurrentTime (_comp->_parameters.CurrentTime)
  SIG_MESSAGE("[_Origin_display] component Origin=Progress_bar() DISPLAY [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../misc/Progress_bar.comp:152]");

  printf("MCDISPLAY: component %s\n", _comp->_name);

  #undef profile
  #undef percent
  #undef flag_save
  #undef minutes
  #undef IntermediateCnts
  #undef StartTime
  #undef EndTime
  #undef CurrentTime
  return(_comp);
} /* class_Progress_bar_display */

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
  #define srcArea (_comp->_parameters.srcArea)
  #define square (_comp->_parameters.square)
  #define tx (_comp->_parameters.tx)
  #define ty (_comp->_parameters.ty)
  #define tz (_comp->_parameters.tz)
  SIG_MESSAGE("[_Source_display] component Source=Source_simple() DISPLAY [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../sources/Source_simple.comp:173]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  if (square == 1) {
    
    rectangle("xy",0,0,0,xwidth,yheight);
  } else {
    
    circle("xy",0,0,0,radius);
  }
  if (dist) {
    dashed_line(0,0,0, -focus_xw/2+tx,-focus_yh/2+ty,tz, 4);
    dashed_line(0,0,0,  focus_xw/2+tx,-focus_yh/2+ty,tz, 4);
    dashed_line(0,0,0,  focus_xw/2+tx, focus_yh/2+ty,tz, 4);
    dashed_line(0,0,0, -focus_xw/2+tx, focus_yh/2+ty,tz, 4);
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
  #undef srcArea
  #undef square
  #undef tx
  #undef ty
  #undef tz
  return(_comp);
} /* class_Source_simple_display */

_class_PSD_monitor *class_PSD_monitor_display(_class_PSD_monitor *_comp
) {
  #define nx (_comp->_parameters.nx)
  #define ny (_comp->_parameters.ny)
  #define filename (_comp->_parameters.filename)
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define restore_neutron (_comp->_parameters.restore_neutron)
  #define nowritefile (_comp->_parameters.nowritefile)
  #define PSD_N (_comp->_parameters.PSD_N)
  #define PSD_p (_comp->_parameters.PSD_p)
  #define PSD_p2 (_comp->_parameters.PSD_p2)
  SIG_MESSAGE("[_D0_Source_display] component D0_Source=PSD_monitor() DISPLAY [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../monitors/PSD_monitor.comp:118]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  multiline(5, (double)xmin, (double)ymin, 0.0,
               (double)xmax, (double)ymin, 0.0,
               (double)xmax, (double)ymax, 0.0,
               (double)xmin, (double)ymax, 0.0,
               (double)xmin, (double)ymin, 0.0);
  #undef nx
  #undef ny
  #undef filename
  #undef xmin
  #undef xmax
  #undef ymin
  #undef ymax
  #undef xwidth
  #undef yheight
  #undef restore_neutron
  #undef nowritefile
  #undef PSD_N
  #undef PSD_p
  #undef PSD_p2
  return(_comp);
} /* class_PSD_monitor_display */

_class_Guide *class_Guide_display(_class_Guide *_comp
) {
  #define reflect (_comp->_parameters.reflect)
  #define w1 (_comp->_parameters.w1)
  #define h1 (_comp->_parameters.h1)
  #define w2 (_comp->_parameters.w2)
  #define h2 (_comp->_parameters.h2)
  #define l (_comp->_parameters.l)
  #define R0 (_comp->_parameters.R0)
  #define Qc (_comp->_parameters.Qc)
  #define alpha (_comp->_parameters.alpha)
  #define m (_comp->_parameters.m)
  #define W (_comp->_parameters.W)
  #define pTable (_comp->_parameters.pTable)
  #define table_present (_comp->_parameters.table_present)
  SIG_MESSAGE("[_SC1_display] component SC1=Guide() DISPLAY [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Guide.comp:204]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  multiline(5,
            -w1/2.0, -h1/2.0, 0.0,
             w1/2.0, -h1/2.0, 0.0,
             w1/2.0,  h1/2.0, 0.0,
            -w1/2.0,  h1/2.0, 0.0,
            -w1/2.0, -h1/2.0, 0.0);
  multiline(5,
            -w2/2.0, -h2/2.0, (double)l,
             w2/2.0, -h2/2.0, (double)l,
             w2/2.0,  h2/2.0, (double)l,
            -w2/2.0,  h2/2.0, (double)l,
            -w2/2.0, -h2/2.0, (double)l);
  line(-w1/2.0, -h1/2.0, 0, -w2/2.0, -h2/2.0, (double)l);
  line( w1/2.0, -h1/2.0, 0,  w2/2.0, -h2/2.0, (double)l);
  line( w1/2.0,  h1/2.0, 0,  w2/2.0,  h2/2.0, (double)l);
  line(-w1/2.0,  h1/2.0, 0, -w2/2.0,  h2/2.0, (double)l);
  #undef reflect
  #undef w1
  #undef h1
  #undef w2
  #undef h2
  #undef l
  #undef R0
  #undef Qc
  #undef alpha
  #undef m
  #undef W
  #undef pTable
  #undef table_present
  return(_comp);
} /* class_Guide_display */

_class_Slit *class_Slit_display(_class_Slit *_comp
) {
  #define xmin (_comp->_parameters.xmin)
  #define xmax (_comp->_parameters.xmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define radius (_comp->_parameters.radius)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  SIG_MESSAGE("[_As1_display] component As1=Slit() DISPLAY [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Slit.comp:81]");

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

_class_Arm *class_Arm_display(_class_Arm *_comp
) {
  SIG_MESSAGE("[_Mono_Cradle_display] component Mono_Cradle=Arm() DISPLAY [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Arm.comp:40]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  /* A bit ugly; hard-coded dimensions. */
  
  line(0,0,0,0.2,0,0);
  line(0,0,0,0,0.2,0);
  line(0,0,0,0,0,0.2);
  return(_comp);
} /* class_Arm_display */

_class_Monochromator_flat *class_Monochromator_flat_display(_class_Monochromator_flat *_comp
) {
  #define zmin (_comp->_parameters.zmin)
  #define zmax (_comp->_parameters.zmax)
  #define ymin (_comp->_parameters.ymin)
  #define ymax (_comp->_parameters.ymax)
  #define zwidth (_comp->_parameters.zwidth)
  #define yheight (_comp->_parameters.yheight)
  #define mosaich (_comp->_parameters.mosaich)
  #define mosaicv (_comp->_parameters.mosaicv)
  #define r0 (_comp->_parameters.r0)
  #define Q (_comp->_parameters.Q)
  #define DM (_comp->_parameters.DM)
  #define mos_rms_y (_comp->_parameters.mos_rms_y)
  #define mos_rms_z (_comp->_parameters.mos_rms_z)
  #define mos_rms_max (_comp->_parameters.mos_rms_max)
  #define mono_Q (_comp->_parameters.mono_Q)
  SIG_MESSAGE("[_PG1Xtal_display] component PG1Xtal=Monochromator_flat() DISPLAY [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../optics/Monochromator_flat.comp:255]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  multiline(5, 0.0, (double)ymin, (double)zmin,
               0.0, (double)ymax, (double)zmin,
               0.0, (double)ymax, (double)zmax,
               0.0, (double)ymin, (double)zmax,
               0.0, (double)ymin, (double)zmin);
  #undef zmin
  #undef zmax
  #undef ymin
  #undef ymax
  #undef zwidth
  #undef yheight
  #undef mosaich
  #undef mosaicv
  #undef r0
  #undef Q
  #undef DM
  #undef mos_rms_y
  #undef mos_rms_z
  #undef mos_rms_max
  #undef mono_Q
  return(_comp);
} /* class_Monochromator_flat_display */

_class_V_sample *class_V_sample_display(_class_V_sample *_comp
) {
  #define radius (_comp->_parameters.radius)
  #define thickness (_comp->_parameters.thickness)
  #define zdepth (_comp->_parameters.zdepth)
  #define Vc (_comp->_parameters.Vc)
  #define sigma_abs (_comp->_parameters.sigma_abs)
  #define sigma_inc (_comp->_parameters.sigma_inc)
  #define radius_i (_comp->_parameters.radius_i)
  #define radius_o (_comp->_parameters.radius_o)
  #define h (_comp->_parameters.h)
  #define focus_r (_comp->_parameters.focus_r)
  #define pack (_comp->_parameters.pack)
  #define frac (_comp->_parameters.frac)
  #define f_QE (_comp->_parameters.f_QE)
  #define gamma (_comp->_parameters.gamma)
  #define target_x (_comp->_parameters.target_x)
  #define target_y (_comp->_parameters.target_y)
  #define target_z (_comp->_parameters.target_z)
  #define focus_xw (_comp->_parameters.focus_xw)
  #define focus_yh (_comp->_parameters.focus_yh)
  #define focus_aw (_comp->_parameters.focus_aw)
  #define focus_ah (_comp->_parameters.focus_ah)
  #define xwidth (_comp->_parameters.xwidth)
  #define yheight (_comp->_parameters.yheight)
  #define zthick (_comp->_parameters.zthick)
  #define rad_sphere (_comp->_parameters.rad_sphere)
  #define sig_a (_comp->_parameters.sig_a)
  #define sig_i (_comp->_parameters.sig_i)
  #define V0 (_comp->_parameters.V0)
  #define target_index (_comp->_parameters.target_index)
  #define multiples (_comp->_parameters.multiples)
  #define VarsV (_comp->_parameters.VarsV)
  SIG_MESSAGE("[_Sample_display] component Sample=V_sample() DISPLAY [/u/data/pkwi/McStas/mcstas/3.0-dev/tools/Python/mcrun/../mccodelib/../../../obsolete/V_sample.comp:320]");

  printf("MCDISPLAY: component %s\n", _comp->_name);
  
  if (VarsV.shapetyp == 0) {
    circle("xz", 0,  h/2.0, 0, radius_i);
    circle("xz", 0,  h/2.0, 0, radius_o);
    circle("xz", 0, -h/2.0, 0, radius_i);
    circle("xz", 0, -h/2.0, 0, radius_o);
    line(-radius_i, -h/2.0, 0, -radius_i, +h/2.0, 0);
    line(+radius_i, -h/2.0, 0, +radius_i, +h/2.0, 0);
    line(0, -h/2.0, -radius_i, 0, +h/2.0, -radius_i);
    line(0, -h/2.0, +radius_i, 0, +h/2.0, +radius_i);
    line(-radius_o, -h/2.0, 0, -radius_o, +h/2.0, 0);
    line(+radius_o, -h/2.0, 0, +radius_o, +h/2.0, 0);
    line(0, -h/2.0, -radius_o, 0, +h/2.0, -radius_o);
    line(0, -h/2.0, +radius_o, 0, +h/2.0, +radius_o);
  }
  else { 
	if (VarsV.shapetyp == 1) {
      double xmin = -0.5*xwidth;
      double xmax =  0.5*xwidth;
      double ymin = -0.5*yheight;
      double ymax =  0.5*yheight;
      double zmin = -0.5*zthick;
      double zmax =  0.5*zthick;
      multiline(5, xmin, ymin, zmin,
                   xmax, ymin, zmin,
                   xmax, ymax, zmin,
                   xmin, ymax, zmin,
                   xmin, ymin, zmin);
      multiline(5, xmin, ymin, zmax,
                   xmax, ymin, zmax,
                   xmax, ymax, zmax,
                   xmin, ymax, zmax,
                   xmin, ymin, zmax);
      line(xmin, ymin, zmin, xmin, ymin, zmax);
      line(xmax, ymin, zmin, xmax, ymin, zmax);
      line(xmin, ymax, zmin, xmin, ymax, zmax);
      line(xmax, ymax, zmin, xmax, ymax, zmax);
    }
    else {
      circle("xy", 0,  0.0, 0, rad_sphere);
      circle("xz", 0,  0.0, 0, rad_sphere);
      circle("yz", 0,  0.0, 0, rad_sphere);        
    }
  }
  #undef radius
  #undef thickness
  #undef zdepth
  #undef Vc
  #undef sigma_abs
  #undef sigma_inc
  #undef radius_i
  #undef radius_o
  #undef h
  #undef focus_r
  #undef pack
  #undef frac
  #undef f_QE
  #undef gamma
  #undef target_x
  #undef target_y
  #undef target_z
  #undef focus_xw
  #undef focus_yh
  #undef focus_aw
  #undef focus_ah
  #undef xwidth
  #undef yheight
  #undef zthick
  #undef rad_sphere
  #undef sig_a
  #undef sig_i
  #undef V0
  #undef target_index
  #undef multiples
  #undef VarsV
  return(_comp);
} /* class_V_sample_display */


  #undef magnify
  #undef line
  #undef dashed_line
  #undef multiline
  #undef rectangle
  #undef box
  #undef circle
  #undef cylinder
  #undef sphere

int display(void) { /* called by mccode_main for BNL_H8:DISPLAY */
  printf("MCDISPLAY: start\n");

  /* call iteratively all components DISPLAY */
  class_Progress_bar_display(&_Origin_var);

  class_Source_simple_display(&_Source_var);

  class_PSD_monitor_display(&_D0_Source_var);

  class_Guide_display(&_SC1_var);

  class_PSD_monitor_display(&_D1_SC1_Out_var);

  class_Slit_display(&_As1_var);

  class_Slit_display(&_As2_var);

  class_Slit_display(&_As3_var);

  class_Slit_display(&_As4_var);

  class_PSD_monitor_display(&_D2_A4_var);

  class_Arm_display(&_Mono_Cradle_var);

  class_Monochromator_flat_display(&_PG1Xtal_var);

  class_Arm_display(&_Mono_Out_var);

  class_PSD_monitor_display(&_D4_SC2_In_var);

  class_Guide_display(&_SC2_var);

  class_PSD_monitor_display(&_D5_SC2_Out_var);

  class_Arm_display(&_Sample_Cradle_var);

  class_Arm_display(&_Sample_Out_var);

  class_V_sample_display(&_Sample_var);

  class_PSD_monitor_display(&_D7_SC3_In_var);

  class_Guide_display(&_SC3_var);

  class_PSD_monitor_display(&_D8_SC3_Out_var);

  class_Arm_display(&_Ana_Cradle_var);

  class_Monochromator_flat_display(&_PG2Xtal_var);

  class_Arm_display(&_Ana_Out_var);

  class_PSD_monitor_display(&_D10_SC4_In_var);

  class_Guide_display(&_SC4_var);

  class_PSD_monitor_display(&_He3H_var);

  printf("MCDISPLAY: end\n");

  return(0);
} /* display */

void* _getvar_parameters(char* compname)
/* enables settings parameters based use of the GETPAR macro */
{
  #ifdef OPENACC
    #define strcmp(a,b) str_comp(a,b)
  #endif
  if (!strcmp(compname, "Origin")) return (void *) &(_Origin_var._parameters);
  if (!strcmp(compname, "Source")) return (void *) &(_Source_var._parameters);
  if (!strcmp(compname, "D0_Source")) return (void *) &(_D0_Source_var._parameters);
  if (!strcmp(compname, "SC1")) return (void *) &(_SC1_var._parameters);
  if (!strcmp(compname, "D1_SC1_Out")) return (void *) &(_D1_SC1_Out_var._parameters);
  if (!strcmp(compname, "As1")) return (void *) &(_As1_var._parameters);
  if (!strcmp(compname, "As2")) return (void *) &(_As2_var._parameters);
  if (!strcmp(compname, "As3")) return (void *) &(_As3_var._parameters);
  if (!strcmp(compname, "As4")) return (void *) &(_As4_var._parameters);
  if (!strcmp(compname, "D2_A4")) return (void *) &(_D2_A4_var._parameters);
  if (!strcmp(compname, "Mono_Cradle")) return (void *) &(_Mono_Cradle_var._parameters);
  if (!strcmp(compname, "PG1Xtal")) return (void *) &(_PG1Xtal_var._parameters);
  if (!strcmp(compname, "Mono_Out")) return (void *) &(_Mono_Out_var._parameters);
  if (!strcmp(compname, "D4_SC2_In")) return (void *) &(_D4_SC2_In_var._parameters);
  if (!strcmp(compname, "SC2")) return (void *) &(_SC2_var._parameters);
  if (!strcmp(compname, "D5_SC2_Out")) return (void *) &(_D5_SC2_Out_var._parameters);
  if (!strcmp(compname, "Sample_Cradle")) return (void *) &(_Sample_Cradle_var._parameters);
  if (!strcmp(compname, "Sample_Out")) return (void *) &(_Sample_Out_var._parameters);
  if (!strcmp(compname, "Sample")) return (void *) &(_Sample_var._parameters);
  if (!strcmp(compname, "D7_SC3_In")) return (void *) &(_D7_SC3_In_var._parameters);
  if (!strcmp(compname, "SC3")) return (void *) &(_SC3_var._parameters);
  if (!strcmp(compname, "D8_SC3_Out")) return (void *) &(_D8_SC3_Out_var._parameters);
  if (!strcmp(compname, "Ana_Cradle")) return (void *) &(_Ana_Cradle_var._parameters);
  if (!strcmp(compname, "PG2Xtal")) return (void *) &(_PG2Xtal_var._parameters);
  if (!strcmp(compname, "Ana_Out")) return (void *) &(_Ana_Out_var._parameters);
  if (!strcmp(compname, "D10_SC4_In")) return (void *) &(_D10_SC4_In_var._parameters);
  if (!strcmp(compname, "SC4")) return (void *) &(_SC4_var._parameters);
  if (!strcmp(compname, "He3H")) return (void *) &(_He3H_var._parameters);
  return 0;
}

void* _get_particle_var(char *token, _class_particle *p)
/* enables setpars based use of GET_PARTICLE_DVAR macro and similar */
{
  return 0;
}

/* embedding file "mccode_main.c" */

/*******************************************************************************
* mccode_main: McCode main() function.
*******************************************************************************/
int mccode_main(int argc, char *argv[])
{
  /*  double run_num = 0; */
  time_t  t;
  clock_t ct;

#ifdef USE_MPI
  char mpi_node_name[MPI_MAX_PROCESSOR_NAME];
  int  mpi_node_name_len;
#endif /* USE_MPI */

#ifdef MAC
  argc = ccommand(&argv);
#endif

#ifdef USE_MPI
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD, &mpi_node_count); /* get number of nodes */
  MPI_Comm_rank(MPI_COMM_WORLD, &mpi_node_rank);
  MPI_Comm_set_name(MPI_COMM_WORLD, instrument_name);
  MPI_Get_processor_name(mpi_node_name, &mpi_node_name_len);
#endif /* USE_MPI */

  //ct = clock();    /* we use clock rather than time to set the default seed */
  //mcseed=(long)ct;

  // device and host functional RNG seed
  struct timeval tm;
  gettimeofday(&tm, NULL);
  mcseed = (long) tm.tv_sec*1000000 + tm.tv_usec;
  // init global _particle.randstate for random number use
  // during init(), finally() and display(). NOTE: during trace, a local
  // "_particle" variable is present and thus used instead.
  srandom(_hash(mcseed-1));

#ifdef USE_MPI
  /* *** print number of nodes *********************************************** */
  if (mpi_node_count > 1) {
    MPI_MASTER(
    printf("Simulation '%s' (%s): running on %i nodes (master is '%s', MPI version %i.%i).\n",
      instrument_name, instrument_source, mpi_node_count, mpi_node_name, MPI_VERSION, MPI_SUBVERSION);
    );
    /* share the same seed, then adapt random seed for each node */
    MPI_Bcast(&mcseed, 1, MPI_LONG, 0, MPI_COMM_WORLD); /* root sends its seed to slaves */
    mcseed += mpi_node_rank; /* make sure we use different seeds per noe */
  }
#endif /* USE_MPI */

#ifdef OPENACC
#ifdef USE_MPI
  int num_devices = acc_get_num_devices(acc_device_nvidia);
  int my_device = mpi_node_rank % num_devices;
  printf("Node %i should use device %i\n",mpi_node_rank,my_device);
#pragma acc set device_num(my_device) device_type(acc_device_nvidia)
#endif
#endif

  // COMMON seed - not functional
  //time_t  t;
  mcstartdate = (long)t;  /* set start date before parsing options and creating sim file */

  /* *** parse options ******************************************************* */
  SIG_MESSAGE("[" __FILE__ "] main START");
  mcformat = getenv(FLAVOR_UPPER "_FORMAT") ?
             getenv(FLAVOR_UPPER "_FORMAT") : FLAVOR_UPPER;
  instrument_exe = argv[0]; /* store the executable path */
  /* read simulation parameters and options */
  mcparseoptions(argc, argv); /* sets output dir and format */


#ifdef USE_MPI
  if (mpi_node_count > 1) {
    /* share the same seed, then adapt random seed for each node */
    MPI_Bcast(&mcseed, 1, MPI_LONG, 0, MPI_COMM_WORLD); /* root sends its seed to slaves */
    mcseed += mpi_node_rank; /* make sure we use different seeds per node */
  }
#endif


/* *** install sig handler, but only once !! after parameters parsing ******* */
#ifndef NOSIGNALS
#ifdef SIGQUIT
  if (signal( SIGQUIT ,sighandler) == SIG_IGN)
    signal( SIGQUIT,SIG_IGN);   /* quit (ASCII FS) */
#endif
#ifdef SIGABRT
  if (signal( SIGABRT ,sighandler) == SIG_IGN)
    signal( SIGABRT,SIG_IGN);   /* used by abort, replace SIGIOT in the future */
#endif
#ifdef SIGTERM
  if (signal( SIGTERM ,sighandler) == SIG_IGN)
    signal( SIGTERM,SIG_IGN);   /* software termination signal from kill */
#endif
#ifdef SIGUSR1
  if (signal( SIGUSR1 ,sighandler) == SIG_IGN)
    signal( SIGUSR1,SIG_IGN);   /* display simulation status */
#endif
#ifdef SIGUSR2
  if (signal( SIGUSR2 ,sighandler) == SIG_IGN)
    signal( SIGUSR2,SIG_IGN);
#endif
#ifdef SIGHUP
  if (signal( SIGHUP ,sighandler) == SIG_IGN)
    signal( SIGHUP,SIG_IGN);
#endif
#ifdef SIGILL
  if (signal( SIGILL ,sighandler) == SIG_IGN)
    signal( SIGILL,SIG_IGN);    /* illegal instruction (not reset when caught) */
#endif
#ifdef SIGFPE
  if (signal( SIGFPE ,sighandler) == SIG_IGN)
    signal( SIGSEGV,SIG_IGN);    /* floating point exception */
#endif
#ifdef SIGBUS
  if (signal( SIGBUS ,sighandler) == SIG_IGN)
    signal( SIGSEGV,SIG_IGN);    /* bus error */
#endif
#ifdef SIGSEGV
  if (signal( SIGSEGV ,sighandler) == SIG_IGN)
    signal( SIGSEGV,SIG_IGN);   /* segmentation violation */
#endif
#endif /* !NOSIGNALS */


  // init executed by master/host
  siminfo_init(NULL); /* open SIM */
  SIG_MESSAGE("[" __FILE__ "] main INITIALISE");
  init();


#ifndef NOSIGNALS
#ifdef SIGINT
  if (signal( SIGINT ,sighandler) == SIG_IGN)
    signal( SIGINT,SIG_IGN);    /* interrupt (rubout) only after INIT */
#endif
#endif /* !NOSIGNALS */

/* ================ main particle generation/propagation loop ================ */
#ifdef USE_MPI
  /* sliced Ncount on each MPI node */
  mcncount = mpi_node_count > 1 ?
    floor(mcncount / mpi_node_count) :
    mcncount; /* number of rays per node */
#endif

// MT specific init, note that per-ray init is empty
#if RNG_ALG == 2
  mt_srandom(mcseed);
#endif


// main raytrace work loop
#ifndef FUNNEL
  // legacy version
  raytrace_all(mcncount, mcseed);
#else
  MPI_MASTER(
  // "funneled" version in which propagation is more parallelizable
  printf("\nNOTE: CPU COMPONENT grammar activated:\n 1) \"FUNNEL\" raytrace algorithm enabled.\n 2) Any SPLIT's are dynamically allocated based on available buffer size. \n");
	     );
  raytrace_all_funnel(mcncount, mcseed);
#endif


#ifdef USE_MPI
 /* merge run_num from MPI nodes */
  if (mpi_node_count > 1) {
  double mcrun_num_double = (double)mcrun_num;
  mc_MPI_Sum(&mcrun_num_double, 1);
  mcrun_num = (unsigned long long)mcrun_num_double;
  }
#endif


  // save/finally executed by master node/thread/host
  finally();


#ifdef USE_MPI
  MPI_Finalize();
#endif /* USE_MPI */


  return 0;
} /* mccode_main */
/* End of file "mccode_main.c". */

/* end of generated C code ./BNL_H8.c */
