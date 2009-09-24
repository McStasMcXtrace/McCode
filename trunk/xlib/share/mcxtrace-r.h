/*******************************************************************************
*
* McXtrace, X-ray ray-tracing package
*         Copyright (C) 1997-2009, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/mcxtrace-r.h
*
* %Identification
* Written by: KN
* Date:    Aug 29, 1997
* Release: McXtrace X.Y
* Version: $Revision: 1.109 $
*
* Runtime system header for McXtrace.
*
* In order to use this library as an external library, the following variables
* and macros must be declared (see details in the code)
*
*   struct mcinputtable_struct mcinputtable[];
*   int mcnumipar;
*   char mcinstrument_name[], mcinstrument_source[];
*   int mctraceenabled, mcdefaultmain;
*   extern MCNUM  mccomp_storein[];
*   extern MCNUM  mcAbsorbProp[];
*   extern MCNUM  mcScattered;
*   #define MCXTRACE_VERSION "the McXtrace version"
*
* Usage: Automatically embbeded in the c code.
*
* $Id:$
*
* $Log:$
*******************************************************************************/

#ifndef MCXTRACE_R_H
#define MCXTRACE_R_H "$Revision: $"

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <limits.h>
#include <errno.h>
#include <time.h>
#include <float.h>

#ifndef MCXTRACE
#ifdef PACKAGE_VERSION
#define MCXTRACE_VERSION PACKAGE_VERSION
#endif
#endif

/* If the runtime is embedded in the simulation program, some definitions can
   be made static. */

#ifdef MC_EMBEDDED_RUNTIME
#define mcstatic static
#else
#define mcstatic
#endif

#ifdef __dest_os
#if (__dest_os == __mac_os)
#define MAC
#endif
#endif

#ifdef __FreeBSD__
#define NEED_STAT_H
#endif

#if defined(__APPLE__) && defined(__GNUC__)
#define NEED_STAT_H
#endif

#ifdef NEED_STAT_H
#include <sys/stat.h>
#endif

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
#endif /* MC_PATHSEP_C */

#ifndef MCSTAS_VERSION
#define MCSTAS_VERSION "External Run-time"
#endif

#ifdef MC_PORTABLE
#ifndef NOSIGNALS
#define NOSIGNALS
#endif
#endif

#ifdef MAC
#ifndef NOSIGNALS
#define NOSIGNALS
#endif
#endif

#ifdef USE_MPI
#ifndef NOSIGNALS
#define NOSIGNALS
#endif
#endif

#if (USE_NEXUS == 0)
#undef USE_NEXUS
#endif

/* I/O section part ========================================================= */

/* Note: the enum instr_formal_types definition MUST be kept
   synchronized with the one in mcstas.h and with the
   instr_formal_type_names array in cogen.c. */
enum instr_formal_types
  {
    instr_type_double, instr_type_int, instr_type_string
  };
struct mcinputtable_struct {
  char *name; /* name of parameter */
  void *par;  /* pointer to instrument parameter (variable) */
  enum instr_formal_types type;
  char *val;  /* default value */
};

typedef double MCNUM;
typedef struct {MCNUM x, y, z;} Coords;
typedef MCNUM Rotation[3][3];

/* the following variables are defined in the McStas generated C code
   but should be defined externally in case of independent library usage */
#ifndef DANSE
extern struct mcinputtable_struct mcinputtable[];
extern int    mcnumipar;
extern char   mcinstrument_name[], mcinstrument_source[];
extern MCNUM  mccomp_storein[]; /* 11 coords * number of components in instrument */
extern MCNUM  mcAbsorbProp[];
extern MCNUM  mcScattered;
#ifndef MC_ANCIENT_COMPATIBILITY
extern int mctraceenabled, mcdefaultmain;
#endif
#endif

/* file I/O definitions and function prototypes */

struct mcformats_struct {
  char *Name;  /* may also specify: append, partial(hidden), binary */
  char *Extension;
  char *Header;
  char *Footer;
  char *BeginSection;
  char *EndSection;
  char *AssignTag;
  char *BeginData;
  char *EndData;
  char *BeginErrors;
  char *EndErrors;
  char *BeginNcount;
  char *EndNcount;
  };

#ifndef MC_EMBEDDED_RUNTIME /* the mcstatic variables (from mcstas-r.c) */
extern FILE * mcsiminfo_file;
extern int    mcgravitation;
extern int    mcdotrace;
extern struct mcformats_struct mcformats[];
extern struct mcformats_struct mcformat;
extern struct mcformats_struct mcformat_data;
#else
mcstatic FILE *mcsiminfo_file        = NULL;
#endif

/* Useful macros ============================================================ */

#define DETECTOR_OUT(p0,p1,p2) mcdetector_out_0D(NAME_CURRENT_COMP,p0,p1,p2,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_0D(t,p0,p1,p2) mcdetector_out_0D(t,p0,p1,p2,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f) \
     mcdetector_out_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f) \
     mcdetector_out_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_OUT_3D(t,xl,yl,zl,xv,yv,zv,x1,x2,y1,y2,z1,z2,m,n,p,p0,p1,p2,f) \
     mcdetector_out_3D(t,xl,yl,zl,xv,yv,zv,x1,x2,y1,y2,z1,z2,m,n,p,p0,p1,p2,f,NAME_CURRENT_COMP,POS_A_CURRENT_COMP)
#define DETECTOR_CUSTOM_HEADER(t)  if (t && strlen(t)) { \
     mcDetectorCustomHeader=malloc(strlen(t)); \
     if (mcDetectorCustomHeader) strcpy(mcDetectorCustomHeader, t); }

#define randvec_target_rect(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9)  randvec_target_rect_real(p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,0,0,0,1)

/* MPI stuff ================================================================ */

#ifdef USE_MPI
#include "mpi.h"

/*
 * MPI_MASTER(i):
 * execution of i only on master node
 */
#define MPI_MASTER(statement) { \
  if(mpi_node_rank == mpi_node_root)\
  { statement; } \
}

#ifndef MPI_REDUCE_BLOCKSIZE
#define MPI_REDUCE_BLOCKSIZE 10000
#endif

int mc_MPI_Reduce(void* sbuf, void* rbuf,
                  int count, MPI_Datatype dtype,
                  MPI_Op op, int root, MPI_Comm comm);

#define exit(code) MPI_Abort(MPI_COMM_WORLD, code)

#else /* !USE_MPI */
#define MPI_MASTER(instr) instr
#endif /* USE_MPI */

#ifdef USE_MPI
static int mpi_node_count;
#endif

#ifdef USE_THREADS  /* user want threads */
#error Threading (USE_THREADS) support has been removed for very poor efficiency. Use MPI/SSH grid instead.
#endif

/* I/O function prototypes ================================================== */

/* The mcformat.Name may contain additional keywords:
 *  no header: omit the format header
 *  no footer: omit the format footer
 */

void   mcset_ncount(double count);
double mcget_ncount(void);
double mcget_run_num(void);
double mcdetector_out(char *cname, double p0, double p1, double p2, char *filename);
double mcdetector_out_0D(char *t, double p0, double p1, double p2, char *c, Coords pos);
double mcdetector_out_1D(char *t, char *xl, char *yl,
                  char *xvar, double x1, double x2, int n,
                  double *p0, double *p1, double *p2, char *f, char *c, Coords pos);
double mcdetector_out_2D(char *t, char *xl, char *yl,
                  double x1, double x2, double y1, double y2, int m,
                  int n, double *p0, double *p1, double *p2, char *f,
                  char *c, Coords pos);
double mcdetector_out_3D(char *t, char *xl, char *yl, char *zl,
      char *xvar, char *yvar, char *zvar,
                  double x1, double x2, double y1, double y2, double z1, double z2, int m,
                  int n, int p, double *p0, double *p1, double *p2, char *f,
                  char *c, Coords pos);
void   mcinfo_simulation(FILE *f, struct mcformats_struct format,
  char *pre, char *name); /* used to add sim parameters (e.g. in Res_monitor) */
void   mcsiminfo_init(FILE *f);
void   mcsiminfo_close(void);
char *mcfull_file(char *name, char *ext);

#ifndef FLT_MAX
#define FLT_MAX         3.40282347E+38F /* max decimal value of a "float" */
#endif

#ifndef CHAR_BUF_LENGTH
#define CHAR_BUF_LENGTH 1024
#endif

/* Following part is only embedded when not redundent with mcstas.h ========= */

#ifndef MCSTAS_H

#ifndef NOSIGNALS
#include <signal.h>
#define SIG_MESSAGE(msg) strcpy(mcsig_message, msg);
#else
#define SIG_MESSAGE(msg)
#endif /* !NOSIGNALS */



/* Useful macros ============================================================ */

#define RAD2MIN  ((180*60)/PI)
#define MIN2RAD  (PI/(180*60))
#define DEG2RAD  (PI/180)
#define RAD2DEG  (180/PI)
#define AA2MS    629.622368        /* Convert k[1/AA] to v[m/s] */
#define MS2AA    1.58825361e-3     /* Convert v[m/s] to k[1/AA] */
#define K2V      AA2MS
#define V2K      MS2AA
#define Q2V      AA2MS
#define V2Q      MS2AA
#define SE2V     437.393377        /* Convert sqrt(E)[meV] to v[m/s] */
#define VS2E     5.22703725e-6     /* Convert (v[m/s])**2 to E[meV] */
#define FWHM2RMS 0.424660900144    /* Convert between full-width-half-max and */
#define RMS2FWHM 2.35482004503     /* root-mean-square (standard deviation) */
#define HBAR     1.05457168e-34    /* [Js] h bar Planck constant CODATA 2002 */
#define MNEUTRON 1.67492728e-27    /* [kg] mass of neutron CODATA 2002 */
#define GRAVITY  9.81              /* [m/s^2] gravitational acceleration */
#define CELE     1.602176487e-19   /* [C] Elementary charge CODATA 2006*/
#define M_C      299792458         /* [m/s] speed of light CODATA 2006*/
#define E2K      (CELE/(HBAR*M_C)*1e10)   /* Convert k[1/AA] to E [eV]*/
#define K2E      (1e-10*M_C*HBAR/CELE) /*Convert E[eV] to k[1/AA] */ 

#ifndef PI
# ifdef M_PI
#  define PI M_PI
# else
#  define PI 3.14159265358979323846
# endif
#endif

/* mccomp_posa and mccomp_posr are defined in McStas generated C code */
#define POS_A_COMP_INDEX(index) \
    (mccomp_posa[index])
#define POS_R_COMP_INDEX(index) \
    (mccomp_posr[index])
/* mcScattered defined in McStas generated C code */
#define SCATTERED mcScattered

/* Retrieve component information from the kernel */
/* Name, position and orientation (both absolute and relative)  */
/* Any component: For "redundancy", see comment by KN */
#define tmp_name_comp(comp) #comp
#define NAME_COMP(comp) tmp_name_comp(comp)
#define tmp_pos_a_comp(comp) (mcposa ## comp)
#define POS_A_COMP(comp) tmp_pos_a_comp(comp)
#define tmp_pos_r_comp(comp) (mcposr ## comp)
#define POS_R_COMP(comp) tmp_pos_r_comp(comp)
#define tmp_rot_a_comp(comp) (mcrota ## comp)
#define ROT_A_COMP(comp) tmp_rot_a_comp(comp)
#define tmp_rot_r_comp(comp) (mcrotr ## comp)
#define ROT_R_COMP(comp) tmp_rot_r_comp(comp)

/* Current component */
#define NAME_CURRENT_COMP  NAME_COMP(mccompcurname)
#define INDEX_CURRENT_COMP mccompcurindex
#define POS_A_CURRENT_COMP POS_A_COMP(mccompcurname)
#define POS_R_CURRENT_COMP POS_R_COMP(mccompcurname)
#define ROT_A_CURRENT_COMP ROT_A_COMP(mccompcurname)
#define ROT_R_CURRENT_COMP ROT_R_COMP(mccompcurname)



#define SCATTER do {mcDEBUG_SCATTER(mcnlx, mcnly, mcnlz, mcnlkx, mcnlky, mcnlkz, \
    mcnlphi, mcnlEx,mcnlEy,mcnlEz, mcnlp); mcScattered++;} while(0)
#define ABSORB do {mcDEBUG_STATE(mcnlx, mcnly, mcnlz, mcnlkx, mcnlky, mcnlkz, \
    mcnlphi, mcnlEx,mcnlEy,mcnlEz, mcnlp); mcDEBUG_ABSORB(); goto mcabsorb;} while(0)
/* Note: The two-stage approach to MC_GETPAR is NOT redundant; without it,
* after #define C sample, MC_GETPAR(C,x) would refer to component C, not to
* component sample. Such are the joys of ANSI C.

* Anyway the usage of MCGETPAR requires that we use sometimes bare names...
*/
#define MC_GETPAR2(comp, par) (mcc ## comp ## _ ## par)
#define MC_GETPAR(comp, par) MC_GETPAR2(comp,par)

#define STORE_XRAY(index, x,y,z, kx,ky,kz, phi, Ex,Ey,Ez, p) \
  mcstore_xray(mccomp_storein,index, x,y,z, kx,ky,kz, phi, Ex,Ey,Ez, p);
#define RESTORE_XRAY(index, x,y,z, kx,ky,kz, phi, Ex,Ey,Ez, p) \
  mcrestore_xray(mccomp_storein,index, &x,&y,&z, &kx,&ky,&kz, &phi, &Ex,&Ey,&Ez, &p);

/*magnet stuff is probably redundant*/
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
    /* change coordinates from local system to magnet system */ \
    Rotation rotLM, rotTemp; \
    Coords   posLM = coords_sub(POS_A_CURRENT_COMP, mcMagnetPos); \
    rot_transpose(ROT_A_CURRENT_COMP, rotTemp); \
    rot_mul(rotTemp, mcMagnetRot, rotLM); \
    mcMagnetPrecession(mcnlx, mcnly, mcnlz, mcnlt, mcnlvx, mcnlvy, mcnlvz, \
	   	       &mcnlsx, &mcnlsy, &mcnlsz, dt, posLM, rotLM); \
  } while(0)

#define mcPROP_DT(dt) \
  do { \
    if (mcMagnet && dt > 0) PROP_MAGNET(dt);\
    mcnlx += mcnlvx*(dt); \
    mcnly += mcnlvy*(dt); \
    mcnlz += mcnlvz*(dt); \
    mcnlt += (dt); \
    if (isnan(p) || isinf(p)) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }\
  } while(0)

/*An interrupt a'la mcMagnet should be inserted below if there's non-zero permeability*/
/*perhaps some kind of PROP_POL*/

#define mcPROP_DL(dl) \
  do { \
    MCNUM k=sqrt( scalar_prod(mcnlkx,mcnlky,mcnlkz,mcnlkx,mcnlky,mcnlkz));\
    mcnlx += (dl)*mcnlkx/k;\
    mcnly += (dl)*mcnlky/k;\
    mcnlz += (dl)*mcnlkz/k;\
    mcnlphi += k*(dl);\
  }while (0)

/*gravity not an issue with x-rays*/
/* ADD: E. Farhi, Aug 6th, 2001 PROP_GRAV_DT propagation with acceleration. */
#define PROP_GRAV_DT(dt, Ax, Ay, Az) \
  do { \
    if(dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }\
    if (mcMagnet) printf("Spin precession gravity\n"); \
    mcnlx  += mcnlvx*(dt) + (Ax)*(dt)*(dt)/2; \
    mcnly  += mcnlvy*(dt) + (Ay)*(dt)*(dt)/2; \
    mcnlz  += mcnlvz*(dt) + (Az)*(dt)*(dt)/2; \
    mcnlvx += (Ax)*(dt); \
    mcnlvy += (Ay)*(dt); \
    mcnlvz += (Az)*(dt); \
    mcnlt  += (dt); \
    DISALLOW_BACKPROP;\
  } while(0)

/*adapted from PROP_DT(dt)*//*{{{*/
#define PROP_DL(dl) \
  do{ \
    if( dl <0 && mcallowbackprop == 0) { (mcAbsorbProp[INDEX_CURRENT_COMP])++; ABSORB; }; \
    mcPROP_DL(dl); \
  } while (0)

#define PROP_DT(dt) \
  do { \
    if(dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    if (mcgravitation) { Coords mcLocG; double mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    PROP_GRAV_DT(dt, mc_gx, mc_gy, mc_gz); } \
    else mcPROP_DT(dt); \
    DISALLOW_BACKPROP;\
  } while(0)/*}}}*/

#define PROP_Z0 \
  mcPROP_P0(z)

#define PROP_X0 \
  mcPROP_P0(x)

#define PROP_Y0 \
  mcPROP_P0(y)

#define mcPROP_P0(P) \
  do { \
    MCNUM mc_dl,mc_k; \
    if(mcnlk ## P == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mc_k=sqrt(scalar_prod(mcnlkx,mcnlky,mcnlkz,mcnlkx,mcnlky,mcnlkz));\
    mc_dl= -mcnl ## P * mc_k / mcnlk ## P;\
    if(mc_dl<0 && mcallowbackprop==0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; };\
    PROP_DL(mc_dl);\
  } while(0)

/*mcstas versions
#define PROP_Z0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = solve_2nd_order(&mc_dt, -mc_gz/2, -mcnlvz, -mcnlz); \
    if (mc_ret && mc_dt>=0) PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); \
    else { if (mcallowbackprop ==0) {mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }}; }\
    else mcPROP_Z0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define mcPROP_Z0 \
  do { \
    double mc_dt; \
    if(mcnlvz == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mc_dt = -mcnlz/mcnlvz; \
    if(mc_dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mcPROP_DT(mc_dt); \
    mcnlz = 0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define PROP_X0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = solve_2nd_order(&mc_dt, -mc_gx/2, -mcnlvx, -mcnlx); \
    if (mc_ret && mc_dt>=0) PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); \
    else { if (mcallowbackprop ==0) {mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }}; }\
    else mcPROP_X0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define mcPROP_X0 \
  do { \
    double mc_dt; \
    if(mcnlvx == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mc_dt = -mcnlx/mcnlvx; \
    if(mc_dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mcPROP_DT(mc_dt); \
    mcnlx = 0; \
    DISALLOW_BACKPROP;\
  } while(0)

#define PROP_Y0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-GRAVITY,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = solve_2nd_order(&mc_dt, -mc_gy/2, -mcnlvy, -mcnly); \
    if (mc_ret && mc_dt>=0) PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); \
    else { if (mcallowbackprop ==0) {mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }}; }\
    else mcPROP_Y0; \
    DISALLOW_BACKPROP;\
  } while(0)


#define mcPROP_Y0 \
  do { \
    double mc_dt; \
    if(mcnlvy == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mc_dt = -mcnly/mcnlvy; \
    if(mc_dt < 0 && mcallowbackprop == 0) { mcAbsorbProp[INDEX_CURRENT_COMP]++; ABSORB; }; \
    mcPROP_DT(mc_dt); \
    mcnly = 0; \
    DISALLOW_BACKPROP; \
  } while(0)
*/

#define vec_prod(x, y, z, x1, y1, z1, x2, y2, z2) \
	vec_prod_func(&x, &y, &z, x1, y1, z1, x2, y2, z2)
inline void vec_prod_func(double *x, double *y, double *z,
		double x1, double y1, double z1, double x2, double y2, double z2);

void coord_vector_product(Coords* ret, Coords* a, Coords* b);

inline double scalar_prod(
    double x1, double y1, double z1, double x2, double y2, double z2);

inline double coord_scalar_product(Coords* a, Coords* b);

#define NORM(x,y,z) \
	norm_func(&x, &y, &z)
inline void norm_func(double *x, double *y, double *z) {
	double temp = (*x * *x) + (*y * *y) + (*z * *z);
	if (temp != 0) {
		temp = sqrt(temp);
		*x /= temp;
		*y /= temp;
		*z /= temp;
	}
}

inline void coord_norm(Coords* c);

/**
 * Rotate the given xyz in a certain way. This does not seem to be used
 * anywhere, so I haven't bothered to reverse-engineer the maths.
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

#ifdef MC_TRACE_ENABLED
#define DEBUG
#endif

#ifdef DEBUG
#define mcDEBUG_INSTR() if(!mcdotrace); else { printf("INSTRUMENT:\n"); printf("Instrument '%s' (%s)\n", mcinstrument_name, mcinstrument_source); }
#define mcDEBUG_COMPONENT(name,c,t) if(!mcdotrace); else {\
  printf("COMPONENT: \"%s\"\n" \
         "POS: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
         name, c.x, c.y, c.z, t[0][0], t[0][1], t[0][2], \
         t[1][0], t[1][1], t[1][2], t[2][0], t[2][1], t[2][2]); \
  printf("Component %30s AT (%g,%g,%g)\n", name, c.x, c.y, c.z); \
  }
#define mcDEBUG_INSTR_END() if(!mcdotrace); else printf("INSTRUMENT END:\n");
#define mcDEBUG_ENTER() if(!mcdotrace); else printf("ENTER:\n");
#define mcDEBUG_COMP(c) if(!mcdotrace); else printf("COMP: \"%s\"\n", c);
#define mcDEBUG_STATE(x,y,z,kx,ky,kz,phi,Ex,Ey,Ez,p) if(!mcdotrace); else \
  printf("STATE: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
      x,y,z,kx,ky,kz,phi,Ex,Ey,Ez,p);
#define mcDEBUG_SCATTER(x,y,z,kx,ky,kz,phi,Ex,Ey,Ez,p) if(!mcdotrace); else \
  printf("SCATTER: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
      x,y,z,kx,ky,kz,phi,Ex,Ey,Ez,p);
#define mcDEBUG_LEAVE() if(!mcdotrace); else printf("LEAVE:\n");
#define mcDEBUG_ABSORB() if(!mcdotrace); else printf("ABSORB:\n");
#else
#define mcDEBUG_INSTR()
#define mcDEBUG_COMPONENT(name,c,t)
#define mcDEBUG_INSTR_END()
#define mcDEBUG_ENTER()
#define mcDEBUG_COMP(c)
#define mcDEBUG_STATE(x,y,z,kx,ky,kz,phi,Ex,Ey,Ez,p)
#define mcDEBUG_SCATTER(x,y,z,kx,ky,kz,phi,Ex,Ey,Ez,p)
#define mcDEBUG_LEAVE()
#define mcDEBUG_ABSORB()
#endif

#ifdef TEST
#define test_printf printf
#else
#define test_printf while(0) printf
#endif

#ifndef MC_RAND_ALG
#define MC_RAND_ALG 1
#endif

#if MC_RAND_ALG == 0
   /* Use system random() (not recommended). */
#  define MC_RAND_MAX RAND_MAX
#elif MC_RAND_ALG == 1
   /* "Mersenne Twister", by Makoto Matsumoto and Takuji Nishimura. */
#  define MC_RAND_MAX ((unsigned long)0xffffffff)
#  define random mt_random
#  define srandom mt_srandom
#elif MC_RAND_ALG == 2
   /* Algorithm used in McStas CVS-080208 and earlier (not recommended). */
#  define MC_RAND_MAX 0x7fffffff
#  define random mc_random
#  define srandom mc_srandom
#else
#  error "Bad value for random number generator choice."
#endif

double rand01();
double randpm1();
double rand0max(double max);
double randminmax(double min, double max);

#ifndef DANSE
void mcinit(void);
void mcraytrace(void);
void mcsave(FILE *);
void mcfinally(void);
void mcdisplay(void);
#endif

void mcdis_magnify(char *);
void mcdis_line(double, double, double, double, double, double);
void mcdis_dashed_line(double, double, double, double, double, double, int);
void mcdis_multiline(int, ...);
void mcdis_rectangle(char *, double, double, double, double, double);
void mcdis_box(double, double, double, double, double, double);
void mcdis_circle(char *, double, double, double, double);


typedef int mc_int32_t;
mc_int32_t mc_random(void);
void mc_srandom (unsigned int x);
unsigned long mt_random(void);
void mt_srandom (unsigned long x);

Coords coords_set(MCNUM x, MCNUM y, MCNUM z);
Coords coords_get(Coords a, MCNUM *x, MCNUM *y, MCNUM *z);
Coords coords_add(Coords a, Coords b);
Coords coords_sub(Coords a, Coords b);
Coords coords_neg(Coords a);
Coords coords_scale(Coords b, double scale);
double coords_sp(Coords a, Coords b);
Coords coords_xp(Coords b, Coords c);
void   coords_print(Coords a);

void rot_set_rotation(Rotation t, double phx, double phy, double phz);
int  rot_test_identity(Rotation t);
void rot_mul(Rotation t1, Rotation t2, Rotation t3);
void rot_copy(Rotation dest, Rotation src);
void rot_transpose(Rotation src, Rotation dst);
Coords rot_apply(Rotation t, Coords a);
void mccoordschange(Coords a, Rotation t, double *x, double *y, double *z,
    double *kx, double *ky, double *kz);
void mccoordschange_polarisation(Rotation t,
    double *sx, double *sy, double *sz);
double mcestimate_error(double N, double p1, double p2);
void mcreadparams(void);

void mcsetstate(double x, double y, double z, double kx, double ky, double kz,
    double phi, double Ex, double Ey, double Ez, double p);
void mcgenstate(void);
double randnorm(void);
double randtriangle(void);
void normal_vec(double *nx, double *ny, double *nz,
    double x, double y, double z);
int inside_rectangle(double, double, double, double);
int box_intersect(double *dt_in, double *dt_out, double x, double y, double z,
    double vx, double vy, double vz, double dx, double dy, double dz);
int cylinder_intersect(double *t0, double *t1, double x, double y, double z,
    double vx, double vy, double vz, double r, double h);
int sphere_intersect(double *t0, double *t1, double x, double y, double z,
                 double vx, double vy, double vz, double r);
/* ADD: E. Farhi, Aug 6th, 2001 solve_2nd_order */
int solve_2nd_order(double *Idt,
    double A,  double B,  double C);
void randvec_target_circle(double *xo, double *yo, double *zo,
    double *solid_angle, double xi, double yi, double zi, double radius);
#define randvec_target_sphere randvec_target_circle
#define plane_intersect_Gfast solve_2nd_order
void randvec_target_rect_angular(double *xo, double *yo, double *zo,
    double *solid_angle,
               double xi, double yi, double zi, double height, double width, Rotation A);
void randvec_target_rect_real(double *xo, double *yo, double *zo,
    double *solid_angle,
	       double xi, double yi, double zi, double height, double width, Rotation A,
			 double lx, double ly, double lz, int order);
void extend_list(int count, void **list, int *size, size_t elemsize);

int mcstas_main(int argc, char *argv[]);


#endif /* !MCSTAS_H */

#endif /* MCSTAS_R_H */
