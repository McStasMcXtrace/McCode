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
* Version: 1.4
*
* Runtime system header for McStas. 
*
* Usage: Automatically embbeded in the c code.
*
* $Id: mcstas-r.h,v 1.44 2003-01-21 08:38:42 pkwi Exp $
*
*	$Log: not supported by cvs2svn $
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
#define MCSTAS_R_H "$Revision: 1.44 $"

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

#ifndef MAC
#ifndef WIN32
#include <signal.h>
#endif /* !MAC */
#endif /* !WIN32 */

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
void mcinit(void);
void mcraytrace(void);
void mcsave(void);
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
*/
#define MC_GETPAR2(comp, par) (mcc ## comp ## _ ## par)
#define MC_GETPAR(comp, par) MC_GETPAR2(comp,par)
#define DETECTOR_OUT(p0,p1,p2) mcdetector_out(NAME_CURRENT_COMP,p0,p1,p2,NULL)
#define DETECTOR_OUT_0D(t,p0,p1,p2) mcdetector_out_0D(t,p0,p1,p2,NAME_CURRENT_COMP)
#define DETECTOR_OUT_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f) \
     mcdetector_out_1D(t,xl,yl,xvar,x1,x2,n,p0,p1,p2,f,NAME_CURRENT_COMP)
#define DETECTOR_OUT_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f) \
     mcdetector_out_2D(t,xl,yl,x1,x2,y1,y2,m,n,p0,p1,p2,f,NAME_CURRENT_COMP)
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

#define PROP_Z0 \
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

#define PROP_DT(dt) \
  do { \
    if(dt < 0) ABSORB; \
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
    double mcrt_bx, mcrt_by, mcrt_bz, mcrt_v1x, mcrt_v1y, mcrt_v1z; \
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
void mcdetector_out(char *cname, double p0, double p1, double p2,
    char *filename);
void mcdetector_out_0D(char *t, double p0, double p1, double p2, char *cname);
void mcdetector_out_1D(char *t, char *xl, char *yl,
    char *xvar, double x1, double x2, int n,
    double *p0, double *p1, double *p2, char *f, char *c);
void mcdetector_out_2D(char *t, char *xl, char *yl, double x1, double x2,
    double y1,double y2,int m, int n,
    double *p0, double *p1, double *p2, char *f, char *c);
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
  char Name[256];
  char Extension[32];
  char Header[2048];
  char Footer[2048];
  char BeginSection[1024];
  char EndSection[1024];
  char AssignTag[256];
  char BeginData[1024];
  char BeginErrors[1024];
  char BeginNcount[1024];
  char EndData[1024];
  char EndErrors[1024];
  char EndNcount[1024];
  };
  
#define mcNUMFORMATS 6
/* in order to be fully portable, the format specifiers must mention each
 * fprintf parameters. In case we do not want to use some of them, we must
 * set the precision to 0.
 * ex: fprintf(f, "printed:%1$s %3$s not printed: %2$.0s\n", "1", "2", "3");
 * such are the joys of ANSI C99 and Single Unix Specification ! 
 * This 0-precision for unused data is automatically checked in mccheck_format
 */ 
  
struct mcformats_struct mcformats[mcNUMFORMATS] = {
  "McStas", "sim",
    "%1$sFormat: %5$s file\n"
      "%1$sURL:    http://neutron.risoe.dk/\n"
      "%1$sEditor: %8$s on %9$s\n"
      "%1$sCreator:%10$s (%2$s) simulation (McStas " MCSTAS_VERSION ")\n"
      "%1$sDate:   Simulation started (%7$li) %6$s\n"
      "%1$sFile:   %3$s" MC_PATHSEP_S "%4$s\n",
    "%1$sEndDate:%6$s\n",
    "%1$sbegin %2$s\n",
    "%1$send %2$s\n",
    "%1$s%3$s: %4$s\n",
    "", 
    "%1$sErrors [%2$s/%3$s]: \n",
    "%1$sNcount [%2$s/%3$s]: \n",
    "", "", "",
  "XML", "xml",
    "<?xml version=\"1.0\" ?>\n<!--\n"
      "URL:    http://www.neutron.anl.gov/nexus/xml/NXgroup.xml\n"
      "Editor: %8$s on %9$s\n"
      "Creator:%10$s (%2$s) McStas " MCSTAS_VERSION " [neutron.risoe.dk].\n"
      "Date:   Simulation started (%7$li) %6$s\n"
      "File:   %3$s" MC_PATHSEP_S "%4$s\n-->\n"
      "<NX%11$s file_name=\"%3$s" MC_PATHSEP_S "%4$s\" file_time=\"%6$s\""
        " McStas_version=\"" MCSTAS_VERSION "\">\n",
    "</NX%11$s>\n<!-- EndDate:%6$s -->\n",
    "%1$s<NX%2$s name=\"%3$s\">\n",
    "%1$s</NX%2$s>\n",
    "%1$s<%3$s>%4$s</%3$s>\n",
    "%1$s<%7$s long_name=\"%6$s\" axis=\"1\" primary=\"1\" min=\"%15$g\""
        " max=\"%16$g\" dims=\"%4$li\" range=\"1\"></%7$s>\n"
      "%1$s<%9$s long_name=\"%8$s\" axis=\"2\" primary=\"1\" min=\"%17$g\""
        " max=\"%18$g\" dims=\"%5$li\" range=\"1\"></%9$s>\n"
      "%1$s<%11$s long_name=\"%10$s\" axis=\"3\" primary=\"1\" min=\"%19$g\""
        " max=\"%20$g\" dims=\"1\" range=\"1\"></%11$s>\n"
      "%1$s<data long_name=\"%3$s\" signal=\"1\"  axis=\"[%7$s,%9$s]\" file_name=\"%21$s\">",
    "%1$s<errors>", "%1$s<monitor>",
    "%1$s</data>\n", "%1$s</errors>\n", "%1$s</monitor>\n",
  "HTML", "html",
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD %5$s//EN\"\n"
      "\"http://www.w3.org/TR/html4/strict.dtd\">\n"
      "<HTML><HEAD><META name=\"Author\" content=\"%8$s on %9$s\">\n"
      "<META name=\"Creator\" content=\"%2$s McStas " MCSTAS_VERSION " [neutron.risoe.dk] simulation\">\n"
      "<META name=\"Date\" content=\"%6$s\">\n"
      "<TITLE>[McStas %2$s]%3$s" MC_PATHSEP_S "%4$s</TITLE></HEAD>\n"
      "<BODY><h1><a name=\"%11$s\">"
        "McStas simulation %10$s (%2$s): %3$s" MC_PATHSEP_S "%4$s</a></h1><br>\n"
        "This simulation report was automatically created by"
        " <a href=\"http://neutron.risoe.dk/\"><i>McStas " MCSTAS_VERSION "</i></a><br>\n"
        "<pre>User:    %8$s on %9$s<br>\n"
        "%1$sCreator: %10$s (<a href=\"%2$s\">%2$s</a>) McStas simulation<br>\n"
        "%1$sDate:    (%7$li) %6$s<br></pre>\n",
    "<b>EndDate: </b>(%7$li) %6$s<br></BODY></HTML>\n",
    "%1$s<h%7$li><a name=\"%3$s\">%2$s %3$s</a></h%7$li> "
      "[child of <a href=\"#%5$s\">%5$s</a>]<br>\n"
      "%1$sAssociated <a href=\"%3$s\">data file %3$s</a><br>\n"
        "%1$sAssociated <a href=\"%3$s.png\">%2$s image %3$s.png<br>\n"
        "%1$s<img src=\"%3$s.png\" alt=\"%2$s %3$s image (when available)\" width=100></a><br>\n",
    "[end of <a href=\"#%3$s\">%2$s %3$s</a>]<br>\n",
    "%1$s<b>%3$s: </b>%4$s<br>\n",
    "<APPLET Codebase=\"V3D\" Code=\"V3D.class\" archive=\"V3D.jar\" Width=\"300\" Height=\"70\">\n"
      "%1$s<PARAM Name=\"Action\"   Value=\"Exec\">\n"
 	    "%1$s<PARAM Name=\"File\"     Value=\"%21$s\">\n"
	    "%1$s<PARAM Name=\"Format\"   Value=\"ascii\">\n"
	    "%1$s<PARAM Name=\"Type\"     Value=\"4\">\n"
	    "%1$s<PARAM Name=\"Title\"    Value=\"%3$s\">\n"
	    "%1$s<PARAM Name=\"TitleX\"   Value=\"%6$s\">\n"
	    "%1$s<PARAM Name=\"TitleY\"   Value=\"%8$s\">\n"
	    "%1$s<PARAM Name=\"SubTitle\" Value=\"%12$s %13$s %14$s\">\n"
	    "%1$s<PARAM Name=\"NbX\"      Value=\"%4$li\">\n"
	    "%1$s<PARAM Name=\"X0\"       Value=\"%15$g\">\n"
	    "%1$s<PARAM Name=\"X1\"       Value=\"%15$g\">\n"
	    "%1$s<PARAM Name=\"Xn\"       Value=\"%16$g\">\n"
	    "%1$s<PARAM Name=\"NbY\"      Value=\"%5$li\">\n"
	    "%1$s<PARAM Name=\"Y0\"       Value=\"%17$g\">\n"
	    "%1$s<PARAM Name=\"Y1\"       Value=\"%17$g\">\n"
	    "%1$s<PARAM Name=\"Yn\"       Value=\"%18$g\">\n"
      "%1$s</APPLET>DATA<--%19$.0g%20$.0g--><br>\n",
      "%1$sERRORS<br>\n","%1$sNCOUNT<br>\n",
      "%1$sEnd of DATA<br>\n", "%1$sEnd of ERRORS<br>\n", "%1$sEnd of NCOUNT<br>\n", 
  "Matlab", "m",
    "function %11$s = get_%11$s(p)\n"
      "%% %5$s function issued from McStas on %6$s\n"
      "%% McStas simulation %2$s: %3$s" MC_PATHSEP_S "%4$s\n"
      "%% import data using s=get_%11$s;\n"
      "if nargout == 0 | nargin > 0, p=1; else p=0; end\n"
      "%11$s.Format ='%5$s';\n"
      "%11$s.URL    ='http://neutron.risoe.dk';\n"
      "%11$s.Editor ='%8$s on %9$s';\n"
      "%11$s.Creator='%10$s (%2$s) McStas " MCSTAS_VERSION " simulation';\n"
      "%11$s.Date   =%7$li; %% for datestr\n"
      "%11$s.File   ='%3$s" MC_PATHSEP_S "%4$s';\n",
    "%11$s.EndDate=%7$li; %% for datestr\n"
      "function d=mcload_inline(d)\n"
      "%% local inline function to load data\n"
      "copyfile(d.filename,[d.func,'.m']);p=d.parent;"
      "eval(['d=',d.func,';']);d.parent=p;delete([d.func,'.m']);\n"
      "function d=mcplot_inline(d,p)\n"
      "%% local inline function to plot data\n"
      "if ~p, return; end;if isempty(d.data) & isempty(findstr(d.type,'0d')), d=mcload_inline(d); end\n"
      "eval(['l=[',d.xylimits,'];']); S=size(d.data);\n"
      "t1=['[',d.parent,'] ',d.filename,': ',d.title];t = strvcat(t1,['  ',d.variables,'=[',d.values,']'],['  ',d.signal],['  ',d.statistics]);\n"
      "disp(t);\n"
      "if ~isempty(findstr(d.type,'0d')), return;\n"
      "elseif ~isempty(findstr(d.type,'2d'))\n"
      "x=linspace(l(1),l(2),S(1)); y=linspace(l(3),l(4),S(2));\n"
      "figure; surface(x,y,d.data);\n"
      "else\nfigure; x=linspace(l(1),l(2),max(S));\nplot(x,d.data);end\n"
      "xlabel(d.xlabel); ylabel(d.ylabel); title(t);"
      "set(gca,'position',[.18,.18,.7,.65]); set(gcf,'name',t1);grid on;\n",
    "%% Section %2$s [%3$s] (level %7$li)\n"
      "%4$s.class = '%2$s';",
    "%6$s.%4$s = %4$s; clear %4$s;\n",
    "%1$s%2$s.%3$s = '%4$s';\n",
    "%1$s%2$s.func='%2$s';\n%1$s%2$s.data = [ ",
    "%1$s%2$s.errors = [ ",
    "%1$s%2$s.ncount = [ ",
    " ]; %% end of data\nmcplot_inline(%2$s,p);\n",
    " ]; %% end of errors\n",
    " ]; %% end of ncount\n",
  "Scilab", "sci",
    "function %11$s = get_%11$s(p)\n"
      "// %5$s function issued from McStas on %6$s\n"
      "// McStas simulation %2$s: %3$s" MC_PATHSEP_S "%4$s\n"
      "// import data using s=get_%11$s();\nmode(-1); //silent execution\n"
      "if argn(2) > 0, p=1; else p=0; end\n"
      "%11$s = struct();\n"
      "%11$s.Format ='%5$s';\n"
      "%11$s.URL    ='http://neutron.risoe.dk';\n"
      "%11$s.Editor ='%8$s on %9$s';\n"
      "%11$s.Creator='%10$s (%2$s) McStas " MCSTAS_VERSION " simulation';\n"
      "%11$s.Date   =%7$li; // for getdate\n"
      "%11$s.filename   ='%3$s" MC_PATHSEP_S "%4$s';\n",
    "%11$s.EndDate=%7$li; // for getdate\nendfunction\n"
    "function d=mcload_inline(d)\n"
      "// local inline func to load data\n"
      "exec(d.filename,-1);p=d.parent;"
      "if ~execstr('d2='+d.func+'();','errcatch'),d2=d; d.parent=p;end\nendfunction\n"
      "function d=mcplot_inline(d,p)\n"
      "// local inline func to plot data\n"
      "if ~p, return; end;if ~length(d.data) & ~length(strindex(d.type,'0d')), d=mcload_inline(d); end\n"
      "execstr(['l=[',d.xylimits,'];']); S=size(d.data);\n"
      "t1=['['+d.parent+'] '+d.filename+': '+d.title];t = [t1;['  '+d.variables+'=['+d.values+']'];['  '+d.signal];['  '+d.statistics]];\n"
      "mprintf('%%s\\n',t(:));\n"
      "if length(strindex(d.type,'0d')),return;\n"
      "else\nw=winsid();if length(w),w=w($)+1; else w=0; end\n"
      "xbasr(w); xset('window',w);\n"
      "if length(strindex(d.type,'2d'))\n"
      "x=linspace(l(1),l(2),S(1)); y=linspace(l(3),l(4),S(2));\n"
      "z=d.data;f=round(log10(max(abs(z))));fx=max(abs(x)); fy = max(abs(y));\n"
      "if fx>0,fx=log10(fx); else fx=[]; end\n"
      "if fy>0,fy=log10(fy); else fy=[]; end\n"
      "if length(fx),if length(fy),f=f-round((fx+fy)/2); else f=f-round(fx); end\n"
      "end\nz=z/10^f;if f,t1=t1+' [*10^'+string(f)+']';end\n"
      "xset('colormap',hotcolormap(64));plot3d1(x,y,z);\n"
      "else\nx=linspace(l(1),l(2),max(S));\nplot2d(x,d.data);end\nend\n"
      "xtitle(t,d.xlabel,d.ylabel); xname(t1);endfunction\n"
    "%11$s=get_%11$s();",
    "// Section %2$s [%3$s] (level %7$li)\n"
      "%1$s%4$s = struct(); %4$s.class = '%2$s';",
    "%1$s%6$s.%4$s = 0; %6$s.%4$s = %4$s; clear %4$s;\n",
    "%1$s%2$s.%3$s = '%4$s';\n",
    "%1$s%2$s.func='get_%2$s';\n%1$s%2$s.data = [ ",
    "%1$s%2$s.errors = [ ",
    "%1$s%2$s.ncount = [ ",
    " ]; // end of data\nmcplot_inline(%2$s,p);\n",
    " ]; // end of errors\n",
    " ]; // end of ncount\n",
  "IDL", "pro",
    "pro stv, S, T, V\n"
      ";** Procedure that operates S.T = V\n"
      "  sv =size(V)\n"
      "  T=strupcase(T)\n"
      "  TL=strupcase(tag_names(S))\n"
      "  id  =where(TL eq T)\n"
      "  sz =[0,0,0]\n"
      "  vd = N_ELEMENTS(sv) - 2\n"
      "  type = sv[vd]\n"
      "  if id(0) ge 0 then d=execute('sz=SIZE(S.'+T+')')\n"
      "  if (sz(sz(0)+1) ne sv(sv(0)+1)) or (sz(0) ne sv(0)) $\n"
      "      or (sz(sz(0)+2) ne sv(sv(0)+2)) $\n"
      "      or type eq 8 then begin\n"
      "    ES = ''\n"
      "    for k=0,n_elements(TL)-1 do begin\n"
      "      case TL(k) of\n"
      "        T:\n"
      "        else: ES =ES+','+TL(k)+':S.'+TL(k)\n"
      "      endcase\n"
      "    endfor\n"
      "    d=execute('S={'+T+':V'+ ES +'}')\n"
      "   endif else d=execute('S.' +T+'=V')\n"
      "end; PRO stv:{s.t=v}\n"
      "function get_%11$s\n"
      "; %5$s function issued from McStas on %6$s\n"
      "; McStas simulation %2$s: %3$s" MC_PATHSEP_S "%4$s\n"
      "%11$s={Format:'%5$s',URL:'http://neutron.risoe.dk',"
      "Editor:'%8$s on %9$s',$\n"
      "Creator:'%10$s (%2$s) McStas " MCSTAS_VERSION " simulation',$\n"
      "Date:%7$li,"
      "File:'%3$s" MC_PATHSEP_S "%4$s'}\n",
    "stv,%11$s,'EndDate',%7$li ; for systime\nreturn, %11$s\nend\n",
    "; Section %2$s [%3$s] (level %7$li)\n"
      "%1$s%4$s={class:'%2$s'}\n",
    "%1$sstv,%6$s,'%4$s',%4$s\n",
    "%1$sstv,%2$s,'%3$s','%4$s'\n",
    "%1$sdata=[ ",
    "%1$serrors=[ ",
    "%1$sncount=[ ",
    " ]\n%1$sstv,%2$s,'data',reform(data,%4$li,%5$li,/over)\n",
    " ]\n%1$sstv,%2$s,'errors',reform(errors,%4$li,%5$li,/over)\n",
    " ]\n%1$sstv,%2$s,'ncount',reform(ncount,%4$li,%5$li,/over)\n\n"
    };
    
struct mcformats_struct mcformat;

/* function prototypes */
void mcuse_format(char *format);
void mcdetector_out(char *cname, double p0, double p1, double p2, char *filename);
void mcdetector_out_0D(char *t, double p0, double p1, double p2, char *c);
void mcdetector_out_1D(char *t, char *xl, char *yl,
		  char *xvar, double x1, double x2, int n,
		  double *p0, double *p1, double *p2, char *f, char *c);
void mcdetector_out_2D(char *t, char *xl, char *yl,
		  double x1, double x2, double y1, double y2, int m,
		  int n, double *p0, double *p1, double *p2, char *f, char *c);
void mcdetector_out_3D(char *t, char *xl, char *yl, char *zl,
      char *xvar, char *yvar, char *zvar, 
		  double x1, double x2, double y1, double y2, double z1, double z2, int m,
		  int n, int p, double *p0, double *p1, double *p2, char *f, char *c);  


#ifndef FLT_MAX
#define FLT_MAX 1E37
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
#define POS_A_CURRENT_COMP POS_A_COMP(mccompcurname)
#define POS_R_CURRENT_COMP POS_R_COMP(mccompcurname)
#define ROT_A_CURRENT_COMP ROT_A_COMP(mccompcurname)
#define ROT_R_CURRENT_COMP ROT_R_COMP(mccompcurname)

#define SCATTERED mcScattered

#endif /* MCSTAS_R_H */
