/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/mcstas-r.h
*
* %Identification
* Written by: KN
* Date:    Aug 29, 1997
* Release: McStas 1.6
* Version: 1.5
*
* Runtime system header for McStas. 
*
* Usage: Automatically embbeded in the c code.
*
* $Id: mcstas-r.h,v 1.53 2003-04-07 11:50:51 farhi Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.52  2003/04/04 18:20:21  farhi
*	remove some warnings (duplicated decl) for --no-runtime on Dec OSF
*	
*	Revision 1.51  2003/04/04 14:27:19  farhi
*	Moved format definitions to mcstas-r.c for --no-runtime to work
*	
*	Revision 1.50  2003/02/11 12:28:46  farhi
*	Variouxs bug fixes after tests in the lib directory
*	mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
*	read_table:corrected MC_SYS_DIR -> MCSTAS define
*	monitor_nd-lib: fix Log(signal) log(coord)
*	HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
*	Progress_bar: precent -> percent parameter
*	CS: ----------------------------------------------------------------------
*	
* Revision 1.5 2002/10/19 22:46:21 ef
*        gravitation for all with -g. Various output formats.
*
* Revision 1.4 2002/09/17 12:01:21 ef
*	removed unused macros (PROP_Y0, X0), changed randvec_target_sphere to circle
* added randvec_target_rect
*
* Revision 1.3 2002/08/28 11:36:37 ef
*	Changed to lib/share/c code 
*
* Revision 1.2 2001/10/10 11:36:37 ef
*	added signal handler
*
* Revision 1.1 1998/08/29 11:36:37 kn
*	Initial revision
*
*******************************************************************************/

#ifndef MCSTAS_R_H
#define MCSTAS_R_H "$Revision: 1.53 $"

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

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

#ifndef MC_PORTABLE
#ifndef MAC
#ifndef WIN32
#include <signal.h>
#endif /* !MAC */
#endif /* !WIN32 */
#endif /* MC_PORTABLE */

typedef double MCNUM;
typedef struct {MCNUM x, y, z;} Coords;
typedef MCNUM Rotation[3][3];

/* Note: the enum instr_formal_types definition MUST be kept
   synchronized with the one in mcstas.h and with the
   instr_formal_type_names array in cogen.c. */
enum instr_formal_types
  {
    instr_type_double, instr_type_int, instr_type_string
  };
struct mcinputtable_struct {
  char *name;
  void *par;
  enum instr_formal_types type;
};
extern struct mcinputtable_struct mcinputtable[];
extern int mcnumipar;
extern char mcinstrument_name[], mcinstrument_source[];
extern int mctraceenabled, mcdefaultmain;
#ifndef MC_EMBEDDED_RUNTIME
extern FILE *mcsiminfo_file;
extern char  mcsig_message[];
extern int mcgravitation;
extern int mcdotrace;
extern struct mcformats_struct mcformats[];
extern struct mcformats_struct mcformat;
#endif
void mcinit(void);
void mcraytrace(void);
void mcsave(FILE *);
void mcfinally(void);
void mcdisplay(void);

/* MOD: E. Farhi, Sep 25th 2001 set Scattered flag (for groups) */
#define SCATTER do {mcDEBUG_SCATTER(mcnlx, mcnly, mcnlz, mcnlvx, mcnlvy, mcnlvz, \
        mcnlt,mcnlsx,mcnlsy, mcnlp); mcScattered++;} while(0)
#define ABSORB do {mcDEBUG_STATE(mcnlx, mcnly, mcnlz, mcnlvx, mcnlvy, mcnlvz, \
        mcnlt,mcnlsx,mcnlsy, mcnlp); mcDEBUG_ABSORB(); goto mcabsorb;} while(0)
/* Note: The two-stage approach to MC_GETPAR is NOT redundant; without it,
* after #define C sample, MC_GETPAR(C,x) would refer to component C, not to
* component sample. Such are the joys of ANSI C.
  
* Anyway the usage of MCGETPAR requires that we use sometimes bare names...
*/
#define MC_GETPAR2(comp, par) (mcc ## comp ## _ ## par)
#define MC_GETPAR(comp, par) MC_GETPAR2(comp,par)
#define DETECTOR_OUT(p0,p1,p2) mcdetector_out(NAME_CURRENT_COMP,p0,p1,p2,NULL)
#define DETECTOR_OUT_0D(t,p0,p1,p2) mcdetector_out_0D(t,p0,p1,p2,NAME_CURRENT_COMP)
#define DETECTOR_OUT_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f) \
     mcdetector_out_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f,NAME_CURRENT_COMP)
#define DETECTOR_OUT_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f) \
     mcdetector_out_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f,NAME_CURRENT_COMP)
#define DETECTOR_OUT_3D(t,xl,yl,zl,xv,yv,zv,x1,x2,y1,y2,z1,z2,m,n,p,p0,p1,p2,f) \
     mcdetector_out_3D(t,xl,yl,zl,xv,yv,zv,x1,x2,y1,y2,z1,z2,m,n,p,p0,p1,p2,f,NAME_CURRENT_COMP)     
/* ADD: E. Farhi, Sep 20th 2001 save neutron state (in local coords) */
#define STORE_NEUTRON(index, x, y, z, vx, vy, vz, t, sx, sy, sz, p) \
  mcstore_neutron(mccomp_storein,index, x, y, z, vx, vy, vz, t, sx, sy, sz, p);
/* ADD: E. Farhi, Sep 20th 2001 restore neutron state (in local coords) */
#define RESTORE_NEUTRON(index, x, y, z, vx, vy, vz, t, sx, sy, sz, p) \
  mcrestore_neutron(mccomp_storein,index, &x, &y, &z, &vx, &vy, &vz, &t, &sx, &sy, &sz, &p);
#define POS_A_COMP_INDEX(index) \
    (mccomp_posa[index]) 
#define POS_R_COMP_INDEX(index) \
    (mccomp_posr[index]) \

#ifdef MC_TRACE_ENABLED
#define DEBUG
#endif

#ifdef DEBUG
#define mcDEBUG_INSTR() if(!mcdotrace); else printf("INSTRUMENT:\n");
#define mcDEBUG_COMPONENT(name,c,t) if(!mcdotrace); else \
  printf("COMPONENT: \"%s\"\n" \
	 "POS: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
	 name, c.x, c.y, c.z, t[0][0], t[0][1], t[0][2], \
	 t[1][0], t[1][1], t[1][2], t[2][0], t[2][1], t[2][2]);
#define mcDEBUG_INSTR_END() if(!mcdotrace); else printf("INSTRUMENT END:\n");
#define mcDEBUG_ENTER() if(!mcdotrace); else printf("ENTER:\n");
#define mcDEBUG_COMP(c) if(!mcdotrace); else printf("COMP: \"%s\"\n", c);
#define mcDEBUG_STATE(x,y,z,vx,vy,vz,t,s1,s2,p) if(!mcdotrace); else \
  printf("STATE: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
	 x,y,z,vx,vy,vz,t,s1,s2,p);
#define mcDEBUG_SCATTER(x,y,z,vx,vy,vz,t,s1,s2,p) if(!mcdotrace); else \
  printf("SCATTER: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
	 x,y,z,vx,vy,vz,t,s1,s2,p);
#define mcDEBUG_LEAVE() if(!mcdotrace); else printf("LEAVE:\n");
#define mcDEBUG_ABSORB() if(!mcdotrace); else printf("ABSORB:\n");
#else
#define mcDEBUG_INSTR()
#define mcDEBUG_COMPONENT(name,c,t)
#define mcDEBUG_INSTR_END()
#define mcDEBUG_ENTER()
#define mcDEBUG_COMP(c)
#define mcDEBUG_STATE(x,y,z,vx,vy,vz,t,s1,s2,p)
#define mcDEBUG_SCATTER(x,y,z,vx,vy,vz,t,s1,s2,p)
#define mcDEBUG_LEAVE()
#define mcDEBUG_ABSORB()
#endif

#ifdef TEST
#define test_printf printf
#else
#define test_printf while(0) printf
#endif

void mcdis_magnify(char *);
void mcdis_line(double, double, double, double, double, double);
void mcdis_multiline(int, ...);
void mcdis_circle(char *, double, double, double, double);

#define RAD2MIN  ((180*60)/PI)
#define MIN2RAD  (PI/(180*60))
#define DEG2RAD  (PI/180)
#define RAD2DEG  (180/PI)
#define AA2MS    629.719	   /* Convert k[1/AA] to v[m/s] */
#define MS2AA    1.58801E-3	   /* Convert v[m/s] to k[1/AA] */
#define K2V	 AA2MS
#define V2K	 MS2AA
#define Q2V	 AA2MS
#define V2Q	 MS2AA
#define SE2V	 437.3949	   /* Convert sqrt(E)[meV] to v[m/s] */
#define VS2E	 5.227e-6	   /* Convert (v[m/s])**2 to E[meV] */
#define FWHM2RMS 0.424660900144    /* Convert between full-width-half-max and */
#define RMS2FWHM 2.35482004503     /* root-mean-square (standard deviation) */
#define HBAR     1.05459E-34
#define MNEUTRON 1.67492E-27

#ifndef PI
# ifdef M_PI
#  define PI M_PI
# else
#  define PI 3.14159265358979323846
# endif
#endif

typedef int mc_int32_t;
mc_int32_t mc_random(void);
void mc_srandom (unsigned int x);
unsigned long mt_random(void);
void mt_srandom (unsigned long x);

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
   /* Algorithm used in McStas 1.1 and earlier (not recommended). */
#  define MC_RAND_MAX 0x7fffffff
#  define random mc_random
#  define srandom mc_srandom
#else
#  error "Bad value for random number generator choice."
#endif

#define rand01() ( ((double)random())/((double)MC_RAND_MAX+1) )
#define randpm1() ( ((double)random()) / (((double)MC_RAND_MAX+1)/2) - 1 )
#define rand0max(max) ( ((double)random()) / (((double)MC_RAND_MAX+1)/(max)) )
#define randminmax(min,max) ( rand0max((max)-(min)) + (min) )

#define mcPROP_DT(dt) \
  do { \
    mcnlx += mcnlvx*(dt); \
    mcnly += mcnlvy*(dt); \
    mcnlz += mcnlvz*(dt); \
    mcnlt += (dt); \
  } while(0)
  
/* ADD: E. Farhi, Aug 6th, 2001 PROP_GRAV_DT propagation with gravitation */
#define PROP_GRAV_DT(dt, Ax, Ay, Az) \
  do { \
    mcnlx  += mcnlvx*dt + Ax*dt*dt/2; \
    mcnly  += mcnlvy*dt + Ay*dt*dt/2; \
    mcnlz  += mcnlvz*dt + Az*dt*dt/2; \
    mcnlvx += Ax*dt; \
    mcnlvy += Ay*dt; \
    mcnlvz += Az*dt; \
    mcnlt  += dt; \
  } while(0)

#define PROP_DT(dt) \
  do { \
    if(dt < 0) ABSORB; \
    if (mcgravitation) { Coords mcLocG; double mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-9.8,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    PROP_GRAV_DT(dt, mc_gx, mc_gy, mc_gz); } \
    else mcPROP_DT(dt); \
  } while(0)
 
#define PROP_Z0 \
  do { \
    if (mcgravitation) { Coords mcLocG; int mc_ret; \
    double mc_dt, mc_gx, mc_gy, mc_gz; \
    mcLocG = rot_apply(ROT_A_CURRENT_COMP, coords_set(0,-9.8,0)); \
    coords_get(mcLocG, &mc_gx, &mc_gy, &mc_gz); \
    mc_ret = plane_intersect_Gfast(&mc_dt, -mc_gz/2, -mcnlvz, -mcnlz); \
    if (mc_ret && mc_dt>0) PROP_GRAV_DT(mc_dt, mc_gx, mc_gy, mc_gz); \
    else ABSORB; }\
    else mcPROP_Z0; \
  } while(0)


#define mcPROP_Z0 \
  do { \
    double mc_dt; \
    if(mcnlvz == 0) ABSORB; \
    mc_dt = -mcnlz/mcnlvz; \
    if(mc_dt < 0) ABSORB; \
    mcnlx += mcnlvx*mc_dt; \
    mcnly += mcnlvy*mc_dt; \
    mcnlt += mc_dt; \
    mcnlz = 0; \
  } while(0)

#define vec_prod(x, y, z, x1, y1, z1, x2, y2, z2) \
  do { \
    double mcvp_tmpx, mcvp_tmpy, mcvp_tmpz; \
    mcvp_tmpx = (y1)*(z2) - (y2)*(z1); \
    mcvp_tmpy = (z1)*(x2) - (z2)*(x1); \
    mcvp_tmpz = (x1)*(y2) - (x2)*(y1); \
    (x) = mcvp_tmpx; (y) = mcvp_tmpy; (z) = mcvp_tmpz; \
  } while(0)

#define scalar_prod(x1, y1, z1, x2, y2, z2) \
  ((x1)*(x2) + (y1)*(y2) + (z1)*(z2))

#define NORM(x,y,z) \
  do { \
    double mcnm_tmp = sqrt((x)*(x) + (y)*(y) + (z)*(z)); \
    if(mcnm_tmp != 0.0) \
    { \
      (x) /= mcnm_tmp; \
      (y) /= mcnm_tmp; \
      (z) /= mcnm_tmp; \
    } \
  } while(0)

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

Coords coords_set(MCNUM x, MCNUM y, MCNUM z);
Coords coords_get(Coords a, MCNUM *x, MCNUM *y, MCNUM *z);
Coords coords_add(Coords a, Coords b);
Coords coords_sub(Coords a, Coords b);
Coords coords_neg(Coords a);

void rot_set_rotation(Rotation t, double phx, double phy, double phz);
void rot_mul(Rotation t1, Rotation t2, Rotation t3);
void rot_copy(Rotation dest, Rotation src);
void rot_transpose(Rotation src, Rotation dst);
Coords rot_apply(Rotation t, Coords a);
void mccoordschange(Coords a, Rotation t, double *x, double *y, double *z,
    double *vx, double *vy, double *vz, double *time,
    double *s1, double *s2);
void mccoordschange_polarisation(Rotation t,
    double *sx, double *sy, double *sz);
double mcestimate_error(double N, double p1, double p2);
void mcreadparams(void);

void mcsetstate(double x, double y, double z, double vx, double vy, double vz,
		double t, double sx, double sy, double sz, double p);
void mcgenstate(void);
double randnorm(void);
void normal_vec(double *nx, double *ny, double *nz, 
    double x, double y, double z);
int box_intersect(double *dt_in, double *dt_out, double x, double y, double z,
    double vx, double vy, double vz, double dx, double dy, double dz);
int cylinder_intersect(double *t0, double *t1, double x, double y, double z,
    double vx, double vy, double vz, double r, double h);
int sphere_intersect(double *t0, double *t1, double x, double y, double z,
		 double vx, double vy, double vz, double r);
/* ADD: E. Farhi, Aug 6th, 2001 plane_intersect_Gfast */   
int plane_intersect_Gfast(double *Idt, 
    double A,  double B,  double C);
void randvec_target_circle(double *xo, double *yo, double *zo, 
    double *solid_angle, double xi, double yi, double zi, double radius);
#define randvec_target_sphere randvec_target_circle
void randvec_target_rect(double *xo, double *yo, double *zo, 
    double *solid_angle,
	       double xi, double yi, double zi, double height, double width);        
void extend_list(int count, void **list, int *size, size_t elemsize);

void mcset_ncount(double count);
double mcget_ncount(void);
double mcget_run_num(void);
int mcstas_main(int argc, char *argv[]);

/* file i/o definitions and function prototypes */

struct mcformats_struct {
  char *Name;  /* may also specify: append, partial(hidden), binary */
  char *Extension;
  char *Header;
  char *Footer;
  char *BeginSection;
  char *EndSection;
  char *AssignTag;
  char *BeginData;
  char *BeginErrors;
  char *BeginNcount;
  char *EndData;
  char *EndErrors;
  char *EndNcount;
  };

/* in order to be fully portable, the format specifiers must mention each
 * fprintf parameters. In case we do not want to use some of them, we must
 * set the precision to 0.
 * ex: fprintf(f, "printed:%1$s %3$s not printed: %2$.0s\n", "1", "2", "3");
 * such are the joys of ANSI C99 and Single Unix Specification ! 
 * This 0-precision for unused data is automatically checked in mccheck_format
 * Maximum number of positional arguments is NL_RGMAX, which is 9 on System V
 * machines (Dec/Compaq/HP). Some more enjoyable  stuff !! -> we use pfprintf
 */ 
/* The mcformat.Name may contain additional keywords:
 *  partial: will not show the monitor in mcstas.sim, omit the format footer 
 *          (usually the end data), and not print the monitor sum in stdout
 */
 
#ifndef MCSTAS_VERSION
#define MCSTAS_VERSION "External Run-time"
#endif

/* function prototypes */
void mcuse_format(char *format);
double mcdetector_out(char *cname, double p0, double p1, double p2, char *filename);
double mcdetector_out_0D(char *t, double p0, double p1, double p2, char *c);
double mcdetector_out_1D(char *t, char *xl, char *yl,
		  char *xvar, double x1, double x2, int n,
		  double *p0, double *p1, double *p2, char *f, char *c);
double mcdetector_out_2D(char *t, char *xl, char *yl,
		  double x1, double x2, double y1, double y2, int m,
		  int n, double *p0, double *p1, double *p2, char *f, char *c);
double mcdetector_out_3D(char *t, char *xl, char *yl, char *zl,
      char *xvar, char *yvar, char *zvar, 
		  double x1, double x2, double y1, double y2, double z1, double z2, int m,
		  int n, int p, double *p0, double *p1, double *p2, char *f, char *c);  
void mcheader_out(FILE *f,char *parent,
  int m, int n, int p,
  char *xlabel, char *ylabel, char *zlabel, char *title,
  char *xvar, char *yvar, char *zvar,
  double x1, double x2, double y1, double y2, double z1, double z2, 
  char *filename);  /* output header for user data file */
void mcinfo_simulation(FILE *f, struct mcformats_struct format, 
  char *pre, char *name); /* used to add sim parameters (e.g. in Res_monitor) */
void mcsiminfo_init(FILE *f);
void mcsiminfo_close(void);


#ifndef FLT_MAX
#define       FLT_MAX         3.40282347E+38F /* max decimal value of a "float" */
#endif

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

#define SCATTERED mcScattered

#endif /* MCSTAS_R_H */
