/*******************************************************************************
* Runtime system for McStas.
*
* 	Project: Monte Carlo Simulation of Triple Axis Spectrometers
* 	File name: mcstas-r.c
*
* 	Author: K.N.			Aug 27, 1997
*
* 	$Id: mcstas-r.c,v 1.16 1998-11-09 08:17:34 kn Exp $
*
* 	$Log: not supported by cvs2svn $
* 	Revision 1.15  1998/10/02 08:38:27  kn
* 	Added DETECTOR_OUT support.
* 	Fixed header comment.
*
* 	Revision 1.14  1998/10/01 08:12:26  kn
* 	Support for embedding the file in the output from McStas.
* 	Added mcstas_main() function.
* 	Added support for command line arguments.
*
* 	Revision 1.13  1998/09/23 13:51:35  kn
* 	McStas now uses its own random() implementation (unless
* 	USE_SYSTEM_RANDOM is defined).
*
* 	Revision 1.12  1998/05/19 07:54:05  kn
* 	In randvec_target_sphere, accept radius=0 to mean that no focusing is to
* 	be done (choose direction uniformly in full 4PI solid angle).
*
* 	Revision 1.11  1998/05/11 12:08:49  kn
* 	Fix bug in cylinder_intersect that caused an infinite cylinder height in
* 	some cases.
*
* 	Revision 1.10  1998/04/17 11:50:08  kn
* 	Added sphere_intersect.
*
* 	Revision 1.9  1998/04/17 10:52:27  kn
* 	Better names in randvec_target_sphere.
*
* 	Revision 1.8  1998/04/16 14:21:49  kn
* 	Added randvec_target() function.
*
* 	Revision 1.7  1998/03/24 07:34:03  kn
* 	Use rand01() in randnorm(). Fix typos.
*
* 	Revision 1.6  1998/03/20 14:19:52  lefmann
* 	Added cylinder_intersect().
*
* 	Revision 1.5  1998/03/16 08:03:41  kn
* 	Added normal distributed random number function randnorm().
*
* 	Revision 1.4  1997/10/16 14:27:05  kn
* 	Add missing #include. Change in mcreadparams() to fit better with the
* 	"display" visualization tool.
*
* 	Revision 1.3  1997/09/08 11:31:22  kn
* 	Added mcsetstate() function.
*
* 	Revision 1.2  1997/09/08 11:16:43  kn
* 	Bug fix in mccoordschange().
*
* 	Revision 1.1  1997/09/08 10:40:03  kn
* 	Initial revision
*
*
* Copyright (C) Risoe National Laboratory, 1997-1998, All rights reserved
*******************************************************************************/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef MCSTAS_R_H
#include "mcstas-r.h"
#endif


#ifdef MC_ANCIENT_COMPATIBILITY
int mctraceenabled = 0;
int mcdefaultmain = 0;
#endif


/* Assign coordinates. */
Coords
coords_set(MCNUM x, MCNUM y, MCNUM z)
{
  Coords a;

  a.x = x;
  a.y = y;
  a.z = z;
  return a;
}

/* Add two coordinates. */
Coords
coords_add(Coords a, Coords b)
{
  Coords c;

  c.x = a.x + b.x;
  c.y = a.y + b.y;
  c.z = a.z + b.z;
  return c;
}

/* Subtract two coordinates. */
Coords
coords_sub(Coords a, Coords b)
{
  Coords c;

  c.x = a.x - b.x;
  c.y = a.y - b.y;
  c.z = a.z - b.z;
  return c;
}

/* Negate coordinates. */
Coords
coords_neg(Coords a)
{
  Coords b;

  b.x = -a.x;
  b.y = -a.y;
  b.z = -a.z;
  return b;
}

/*******************************************************************************
* Get transformation for rotation first phx around x axis, then phy around y,
* then phz around z.
*******************************************************************************/
void
rot_set_rotation(Rotation t, double phx, double phy, double phz)
{
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

/*******************************************************************************
* Matrix multiplication of transformations (this corresponds to combining
* transformations). After rot_mul(T1, T2, T3), doing T3 is equal to doing
* first T2, then T1.
* Note that T3 must not alias (use the same array as) T1 or T2.
*******************************************************************************/
void
rot_mul(Rotation t1, Rotation t2, Rotation t3)
{
  int i,j, k;

  for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++)
      t3[i][j] = t1[i][0]*t2[0][j] + t1[i][1]*t2[1][j] + t1[i][2]*t2[2][j];
}

/*******************************************************************************
* Copy a rotation transformation (needed since arrays cannot be assigned in C).
*******************************************************************************/
void
rot_copy(Rotation dest, Rotation src)
{
  dest[0][0] = src[0][0];
  dest[0][1] = src[0][1];
  dest[0][2] = src[0][2];
  dest[1][0] = src[1][0];
  dest[1][1] = src[1][1];
  dest[1][2] = src[1][2];
  dest[2][0] = src[2][0];
  dest[2][1] = src[2][1];
  dest[2][2] = src[2][2];
}

void
rot_transpose(Rotation src, Rotation dst)
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

Coords
rot_apply(Rotation t, Coords a)
{
  Coords b;

  b.x = t[0][0]*a.x + t[0][1]*a.y + t[0][2]*a.z;
  b.y = t[1][0]*a.x + t[1][1]*a.y + t[1][2]*a.z;
  b.z = t[2][0]*a.x + t[2][1]*a.y + t[2][2]*a.z;
  return b;
}

void
mccoordschange(Coords a, Rotation t, double *x, double *y, double *z,
	       double *vx, double *vy, double *vz, double *time,
	       double *s1, double *s2)
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

  b.x = *vx;
  b.y = *vy;
  b.z = *vz;
  c = rot_apply(t, b);
  *vx = c.x;
  *vy = c.y;
  *vz = c.z;
  /* ToDo: What to do about the spin? */
}


double mcestimate_error(int N, double p1, double p2)
{
  double pmean, n1;
  if(N <= 1)
    return 0;
  pmean = p1 / N;
  n1 = N - 1;
  return sqrt((N/n1)*(p2 - pmean*pmean));
}


void
mcreadparams(void)
{
  extern struct mcinputtable_struct mcinputtable[];
  int i;

  for(i = 0; mcinputtable[i].name != 0; i++)
  {
    printf("Set value of instrument parameter %s:\n", mcinputtable[i].name);
    fflush(stdout);
    scanf("%lf", mcinputtable[i].par);
  }
}



void
mcsetstate(double x, double y, double z, double vx, double vy, double vz,
	   double t, double s1, double s2, double p)
{
  extern double mcnx, mcny, mcnz, mcnvx, mcnvy, mcnvz;
  extern double mcnt, mcns1, mcns2, mcnp;
  
  mcnx = x;
  mcny = y;
  mcnz = z;
  mcnvx = vx;
  mcnvy = vy;
  mcnvz = vz;
  mcnt = t;
  mcns1 = s1;
  mcns2 = s2;
  mcnp = p;
}

void
mcgenstate(void)
{
  mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 0, 1);
}

/* McStas random number routine. */

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * This is derived from the Berkeley source:
 *	@(#)random.c	5.5 (Berkeley) 7/6/88
 * It was reworked for the GNU C Library by Roland McGrath.
 * Rewritten to use reentrant functions by Ulrich Drepper, 1995.
 */

/*******************************************************************************
* Modified for McStas from glibc 2.0.7pre1 stdlib/random.c and
* stdlib/random_r.c.
*
* This way random() is more than four times faster compared to calling
* standard glibc random() on ix86 Linux, probably due to multithread support,
* ELF shared library overhead, etc. It also makes McStas generated
* simulations more portable (more likely to behave identically across
* platforms, important for parrallel computations).
*******************************************************************************/


#define	TYPE_3		3
#define	BREAK_3		128
#define	DEG_3		31
#define	SEP_3		3

static mc_int32_t randtbl[DEG_3 + 1] =
  {
    TYPE_3,

    -1726662223, 379960547, 1735697613, 1040273694, 1313901226,
    1627687941, -179304937, -2073333483, 1780058412, -1989503057,
    -615974602, 344556628, 939512070, -1249116260, 1507946756,
    -812545463, 154635395, 1388815473, -1926676823, 525320961,
    -1009028674, 968117788, -123449607, 1284210865, 435012392,
    -2017506339, -911064859, -370259173, 1132637927, 1398500161,
    -205601318,
  };

static mc_int32_t *fptr = &randtbl[SEP_3 + 1];
static mc_int32_t *rptr = &randtbl[1];
static mc_int32_t *state = &randtbl[1];
#define rand_deg DEG_3
#define rand_sep SEP_3
static mc_int32_t *end_ptr = &randtbl[sizeof (randtbl) / sizeof (randtbl[0])];

mc_int32_t
mc_random (void)
{
  mc_int32_t result;

  *fptr += *rptr;
  /* Chucking least random bit.  */
  result = (*fptr >> 1) & 0x7fffffff;
  ++fptr;
  if (fptr >= end_ptr)
  {
    fptr = state;
    ++rptr;
  }
  else
  {
    ++rptr;
    if (rptr >= end_ptr)
      rptr = state;
  }
  return result;
}

void
mc_srandom (unsigned int x)
{
  /* We must make sure the seed is not 0.  Take arbitrarily 1 in this case.  */
  state[0] = x ? x : 1;
  {
    long int i;
    for (i = 1; i < rand_deg; ++i)
    {
      /* This does:
	 state[i] = (16807 * state[i - 1]) % 2147483647;
	 but avoids overflowing 31 bits.  */
      long int hi = state[i - 1] / 127773;
      long int lo = state[i - 1] % 127773;
      long int test = 16807 * lo - 2836 * hi;
      state[i] = test + (test < 0 ? 2147483647 : 0);
    }
    fptr = &state[rand_sep];
    rptr = &state[0];
    for (i = 0; i < 10 * rand_deg; ++i)
      random ();
  }
}

/* End of McStas random number routine. */

double
randnorm(void)
{
  static double v1, v2, s;
  static int phase = 0;
  double X, u1, u2;

  if(phase == 0)
  {
    do
    {
      u1 = rand01();
      u2 = rand01();
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

/* Written by: EM,NB,ABA 4.2.98 */
int
cylinder_intersect(double *t0, double *t1, double x, double y, double z,
		   double vx, double vy, double vz, double r, double h)
{
  double v, D, t_in, t_out, y_in, y_out;

  v = sqrt(vx*vx+vy*vy+vz*vz); 

  D = (2*vx*x + 2*vz*z)*(2*vx*x + 2*vz*z) 
    - 4*(vx*vx + vz*vz)*(x*x + z*z - r*r);

  if (D>=0) 
  {   
    t_in  = (-(2*vz*z + 2*vx*x) - sqrt(D))/(2*(vz*vz + vx*vx));
    t_out = (-(2*vz*z + 2*vx*x) + sqrt(D))/(2*(vz*vz + vx*vx));
    y_in = vy*t_in + y;
    y_out =vy*t_out + y;

    if ( (y_in > h/2 && y_out > h/2) || (y_in < -h/2 && y_out < -h/2) )
      return 0;
    else
    {
      if (y_in > h/2)
	t_in = ((h/2)-y)/vy;
      if (y_in < -h/2)
	t_in = ((-h/2)-y)/vy;
      if (y_out > h/2)
	t_out = ((h/2)-y)/vy;
      if (y_out < -h/2)
	t_out = ((-h/2)-y)/vy;
    }
    *t0 = t_in;
    *t1 = t_out;
    return 1;
  }
  else
  {
    *t0 = *t1 = 0;
    return 0;
  }
}


/* Calculate intersection between line and sphere. */
int
sphere_intersect(double *t0, double *t1, double x, double y, double z,
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
}


/* Choose random direction towards target at (x,y,z) with given radius. */
/* If radius is zero, choose random direction in full 4PI, no target. */
/* ToDo: It should be possible to optimize this to avoid computing angles. */
void
randvec_target_sphere(double *xo, double *yo, double *zo, double *solid_angle,
	       double xi, double yi, double zi, double radius)
{
  double l, theta0, phi, theta, nx, ny, nz, xt, yt, zt;
  
  if(radius == 0.0)
  {
    /* No target, choose uniformly a direction in full 4PI solid angle. */
    theta = acos (1 - rand0max(2));
    phi = rand0max(2 * PI);
    if(solid_angle)
      *solid_angle = 4*PI;
    nx = 1;
    ny = 0;
    nz = 0;
    xi = 0;
    yi = 1;
    zi = 0;
  }
  else
  {
    l = sqrt(xi*xi + yi*yi + zi*zi); /* Distance to target. */
    theta0 = asin(radius / l);	/* Target size as seen from origin */
    if(solid_angle)
    {
      /* Compute solid angle of target as seen from origin. */
      *solid_angle = 2*PI*(1 - cos(theta0));
    }
  
    /* Now choose point uniformly on sphere surface within angle theta0 */
    theta = acos (1 - rand0max(1 - cos(theta0)));
    phi = rand0max(2 * PI);
    /* Now, to obtain the desired vector rotate (x,y,z) angle theta around a
       perpendicular axis (nx,ny,nz) and then angle phi around (x,y,z). */
    if(xi == 0 && yi == 0)
    {
      nx = 1;
      ny = 0;
      nz = 0;
    }
    else
    {
      nx = -yi;
      ny = xi;
      nz = 0;
    }
  }
  rotate(xt, yt, zt, xi, yi, zi, theta, nx, ny, nz);
  rotate(*xo, *yo, *zo, xt, yt, zt, phi, xi, yi, zi);
}

/* Number of neutron histories to simulate. */
static double mcncount = 1e6;

void
mcset_ncount(double count)
{
  mcncount = count;
}

mcstatic int mcdotrace = 0;

static void
mcsetn_arg(char *arg)
{
  mcset_ncount(strtod(arg, NULL));
}

static void
mcsetseed(char *arg)
{
  srandom(atol(arg));
}

static void
mchelp(char *pgmname)
{
  int i;
  
  fprintf(stderr, "Usage: %s [options] [parm=value ...]\n", pgmname);
  fprintf(stderr,
"Options are:\n"
"  -s seed   --seed=seed      Set random seed\n"
"  -n count  --ncount=count   Set number of neutrons to simulate.\n"
"  -t        --trace          Enable trace of neutron through instrument\n"
"  -h        --help           Show help message\n"
"  -i        --info           Detailed instrument information\n"
"Instrument parameters are:\n");
  for(i = 0; i < mcnumipar; i++)
    fprintf(stderr, "  %s\n", mcinputtable[i].name);
}

static void
mcshowhelp(char *pgmname)
{
  mchelp(pgmname);
  exit(0);
}

static void
mcusage(char *pgmname)
{
  fprintf(stderr, "Error: incorrect commad line arguments\n");
  mchelp(pgmname);
  exit(1);
}

static void
mcinfo(void)
{
  int i;
  
  printf("Name: %s\n", mcinstrument_name);
  printf("Parameters:");
  for(i = 0; i < mcnumipar; i++)
    printf(" %s", mcinputtable[i].name);
  printf("\n");
  printf("Instrument-source: %s\n", mcinstrument_source);
  printf("Trace-enabled: %s\n", mctraceenabled ? "yes" : "no");
  printf("Default-main: %s\n", mcdefaultmain ? "yes" : "no");
  printf("Embedded-runtime: %s\n",
#ifdef MC_EMBEDDED_RUNTIME
	 "yes"
#else
	 "no"
#endif
	 );
  exit(0);
}

static void
mcenabletrace(void)
{
 if(mctraceenabled)
  mcdotrace = 1;
 else
 {
   fprintf(stderr,
	   "Error: trace not enabled.\n"
	   "Please re-run the McStas compiler with the --trace option\n");
   exit(1);
 }
}

void
mcparseoptions(int argc, char *argv[])
{
  int i, j, pos;
  char *p;
  int paramset = 0, *paramsetarray;

  paramsetarray = malloc(mcnumipar*sizeof(*paramsetarray));
  if(paramsetarray == NULL)
  {
    fprintf(stderr, "Error: insufficient memory\n");
    exit(1);
  }
  for(j = 0; j < mcnumipar; j++)
    paramsetarray[j] = 0;
  
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
    else if(!strcmp("-h", argv[i]))
      mcshowhelp(argv[0]);
    else if(!strcmp("--help", argv[i]))
      mcshowhelp(argv[0]);
    else if(!strcmp("-i", argv[i]))
      mcinfo();
    else if(!strcmp("--info", argv[i]))
      mcinfo();
    else if(!strcmp("-t", argv[i]))
      mcenabletrace();
    else if(!strcmp("--trace", argv[i]))
      mcenabletrace();
    else if(argv[i][0] != '-' && (p = strchr(argv[i], '=')) != NULL)
    {
      *p++ = '\0';
      for(j = 0; j < mcnumipar; j++)
	if(!strcmp(mcinputtable[j].name, argv[i]))
	{
	  *(mcinputtable[j].par) = strtod(p, NULL);
	  paramsetarray[j] = 1;
	  paramset = 1;
	  break;
	}
      if(j == mcnumipar)
      {				/* Unrecognized parameter name */
	fprintf(stderr, "Error: unrecognized parameter %s\n", argv[i]);
	exit(1);
      }
    }
    else
      mcusage(argv[0]);
  }
  if(!paramset)
    mcreadparams();		/* Prompt for parameters if not specified. */
  else
  {
    for(j = 0; j < mcnumipar; j++)
      if(!paramsetarray[j])
      {
	fprintf(stderr, "Error: Instrument parameter %s left unset\n",
		mcinputtable[j].name);
	exit(1);
      }
  }    
}

/* McStas main() function. */
int
mcstas_main(int argc, char *argv[])
{
  int run_num = 0;

  srandom(time(NULL));
  mcparseoptions(argc, argv);
  mcinit();
  while(run_num < mcncount)
  {
    mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 0, 1);
    mcraytrace();
    run_num++;
  }
  mcfinally();
  return 0;
}
