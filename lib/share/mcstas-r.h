/*******************************************************************************
* Runtime system for McStas.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: mcstas-r.h
*
*	Author: K.N.			Aug 29, 1997
*
*	$Id: mcstas-r.h,v 1.17 1998-09-24 13:01:39 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.16  1998/09/23 13:52:08  kn
*	Added conversion factors.
*	McStas now uses its own random() implementation (unless
*	USE_SYSTEM_RANDOM is defined).
*
*	Revision 1.15  1998/05/19 07:59:45  kn
*	Hack to make random number generation work with HP's CC C compiler.
*
*	Revision 1.14  1998/04/17 11:50:31  kn
*	Added sphere_intersect.
*
*	Revision 1.13  1998/04/17 10:53:08  kn
*	Added randvec_target_sphere.
*
*	Revision 1.12  1998/03/25 07:23:24  kn
*	Fixed RAND_MAX #define for HPUX.
*
*	Revision 1.11  1998/03/24 13:59:26  lefmann
*	Added #define for RAND_MAX, needed on HPUX.
*
*	Revision 1.10  1998/03/24 13:24:40  lefmann
*	Added HBAR, MNEUTRON.
*
*	Revision 1.9  1998/03/24 07:42:35  kn
*	Added definition for PI.
*
*	Revision 1.8  1998/03/24 07:36:20  kn
*	Make ABSORB macro work better in control structures.
*	Add test_printf().
*	Add rand01(), randpm1(), rand0max(), and randminmax().
*	Add PROP_X0, PROP_Y0, PROP_DT, vec_prod(), scalar_prod(), NORM(), and
*	rotate().
*	Fix typos.
*
*	Revision 1.7  1998/03/20 14:20:10  lefmann
*	Added a few definitions.
*
*	Revision 1.6  1998/03/18 13:21:48  elu_krni
*	Added definition of PROP_Z0 macro.
*
*	Revision 1.5  1998/03/16 08:04:16  kn
*	Added normal distributed random number function randnorm().
*
*	Revision 1.4  1997/12/03 13:34:19  kn
*	Added definition of ABSORB macro.
*
*	Revision 1.3  1997/10/16 14:27:28  kn
*	Added debugging output used by the "display" graphical visualization
*	tool.
*
*	Revision 1.2  1997/09/08 11:31:27  kn
*	Added mcsetstate() function.
*
*	Revision 1.1  1997/09/08 10:39:44  kn
*	Initial revision
*
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

typedef double MCNUM;
typedef struct {MCNUM x, y, z;} Coords;
typedef MCNUM Rotation[3][3];

#define ABSORB do {mcDEBUG_STATE(mcnlx, mcnly, mcnlz, mcnlvx, mcnlvy, mcnlvz, \
        mcnlt,mcnls1,mcnls2, mcnlp); mcDEBUG_ABSORB(); goto mcabsorb;} while(0)

#ifdef DEBUG
#define mcDEBUG_INSTR() printf("INSTRUMENT:\n");
#define mcDEBUG_COMPONENT(name,c,t) \
  printf("COMPONENT: \"%s\"\n" \
	 "POS: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
	 name, c.x, c.y, c.z, t[0][0], t[0][1], t[0][2], \
	 t[1][0], t[1][1], t[1][2], t[2][0], t[2][1], t[2][2]);
#define mcDEBUG_INSTR_END() printf("INSTRUMENT END:\n");
#define mcDEBUG_ENTER() printf("ENTER:\n");
#define mcDEBUG_COMP(c) printf("COMP: \"%s\"\n", c);
#define mcDEBUG_STATE(x,y,z,vx,vy,vz,t,s1,s2,p) \
  printf("STATE: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
	 x,y,z,vx,vy,vz,t,s1,s2,p);
#define mcDEBUG_LEAVE() printf("LEAVE:\n");
#define mcDEBUG_ABSORB() printf("ABSORB:\n");
#else
#define mcDEBUG_INSTR()
#define mcDEBUG_COMPONENT(name,c,t)
#define mcDEBUG_INSTR_END()
#define mcDEBUG_ENTER()
#define mcDEBUG_COMP(c)
#define mcDEBUG_STATE(x,y,z,vx,vy,vz,t,s1,s2,p)
#define mcDEBUG_LEAVE()
#define mcDEBUG_ABSORB()
#endif

#ifdef TEST
#define test_printf printf
#else
#define test_printf while(0) printf
#endif

#define MIN2RAD  (PI/(180*60))
#define DEG2RAD  (PI/180)
#define RAD2DEG  (180/PI)
#define AA2MS    629.719		/* Convert k[1/AA] to v[m/s] */
#define MS2AA    1.58801E-3		/* Convert v[m/s] to k[1/AA] */
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

#ifndef USE_SYSTEM_RANDOM
#ifdef RAND_MAX
# undef RAND_MAX
#endif
#define RAND_MAX 0x7fffffff
#define random mc_random
#define srandom mc_srandom
#endif /* !USE_SYSTEM_RANDOM */

#define rand01() ( ((double)random())/((double)RAND_MAX+1) )
#define randpm1() ( ((double)random()) / (((double)RAND_MAX+1)/2) - 1 )
#define rand0max(max) ( ((double)random()) / (((double)RAND_MAX+1)/(max)) )
#define randminmax(min,max) ( rand0max((max)-(min)) - (min) )

#define PROP_X0 \
  { \
    double mc_dt; \
    if(mcnlvx == 0) ABSORB; \
    mc_dt = -mcnlx/mcnlvx; \
    if(mc_dt < 0) ABSORB; \
    mcnly += mcnlvy*mc_dt; \
    mcnlz += mcnlvz*mc_dt; \
    mcnlt += mc_dt; \
    mcnlx = 0; \
  }

#define PROP_Y0 \
  { \
    double mc_dt; \
    if(mcnlvy == 0) ABSORB; \
    mc_dt = -mcnly/mcnlvy; \
    if(mc_dt < 0) ABSORB; \
    mcnlx += mcnlvx*mc_dt; \
    mcnlz += mcnlvz*mc_dt; \
    mcnlt += mc_dt; \
    mcnly = 0; \
  }

#define PROP_Z0 \
  { \
    double mc_dt; \
    if(mcnlvz == 0) ABSORB; \
    mc_dt = -mcnlz/mcnlvz; \
    if(mc_dt < 0) ABSORB; \
    mcnlx += mcnlvx*mc_dt; \
    mcnly += mcnlvy*mc_dt; \
    mcnlt += mc_dt; \
    mcnlz = 0; \
  }

#define PROP_DT(dt) \
  { \
    mcnlx += mcnlvx*(dt); \
    mcnly += mcnlvy*(dt); \
    mcnlz += mcnlvz*(dt); \
    mcnlt += (dt); \
  }

#define vec_prod(x, y, z, x1, y1, z1, x2, y2, z2) \
  { \
    double mcvp_tmpx, mcvp_tmpy, mcvp_tmpz; \
    mcvp_tmpx = (y1)*(z2) - (y2)*(z1); \
    mcvp_tmpy = (z1)*(x2) - (z2)*(x1); \
    mcvp_tmpz = (x1)*(y2) - (x2)*(y1); \
    (x) = mcvp_tmpx; (y) = mcvp_tmpy; (z) = mcvp_tmpz; \
  }

#define scalar_prod(x1, y1, z1, x2, y2, z2) \
  ((x1)*(x2) + (y1)*(y2) + (z1)*(z2))

#define NORM(x,y,z) \
  { \
    double mcnm_tmp = sqrt((x)*(x) + (y)*(y) + (z)*(z)); \
    if(mcnm_tmp != 0.0) \
    { \
      (x) /= mcnm_tmp; \
      (y) /= mcnm_tmp; \
      (z) /= mcnm_tmp; \
    } \
  }

#define rotate(x, y, z, vx, vy, vz, phi, ax, ay, az) \
  { \
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
  }

Coords coords_set(MCNUM x, MCNUM y, MCNUM z);
Coords coords_add(Coords a, Coords b);
Coords coords_sub(Coords a, Coords b);
Coords coords_neg(Coords a);

void rot_set_rotation(Rotation t, double phx, double phy, double phz);
void rot_mul(Rotation t1, Rotation t2, Rotation t3);
void rot_copy(Rotation dest, Rotation src);
void rot_transpose(Rotation src, Rotation dst);
Coords rot_apply(Rotation t, Coords a);

void mcsetstate(double x, double y, double z, double vx, double vy, double vz,
		double t, double s1, double s2, double p);
void mcgenstate(void);
double randnorm(void);
int cylinder_intersect(double *t0, double *t1, double x, double y, double z,
		       double vx, double vy, double vz, double r, double h);
int sphere_intersect(double *t0, double *t1, double x, double y, double z,
		 double vx, double vy, double vz, double r);
void randvec_target_sphere(double *xo, double *yo, double *zo, double *solid_angle,
			   double xi, double yi, double zi, double radius);
