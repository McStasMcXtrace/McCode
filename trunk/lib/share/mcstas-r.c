/*******************************************************************************
* Runtime system for McStas.
*
* 	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
* 	File name: mcstas-r.c
*
* 	Author: K.N.			Aug 27, 1997
*
* 	$Id: mcstas-r.c,v 1.10 1998-04-17 11:50:08 kn Exp $
*
* 	$Log: not supported by cvs2svn $
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
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "mcstas-r.h"

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


void
mcreadparams(void)
{
  extern struct {
    char *name;
    MCNUM *par;
  } mcinputtable[];
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

    if ( !((y_in > h/2 && y_out > h/2) || (y_in < -h/2 && y_out < -h/2)) )
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


/* Choose random direction towards target at (x,y,z) with radius r. */
/* ToDo: It should be possible to optimize this to avoid computing angles. */
void
randvec_target_sphere(double *xo, double *yo, double *zo, double *solid_angle,
	       double xi, double yi, double zi, double radius)
{
  double l, theta0, phi, theta, nx, ny, nz, xt, yt, zt;
  
  l = sqrt(xi*xi + yi*yi + zi*zi); /* Distance to target. */
  theta0 = asin(radius / l);	/* Target size as seen from origin */
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
  rotate(xt, yt, zt, xi, yi, zi, theta, nx, ny, nz);
  rotate(*xo, *yo, *zo, xt, yt, zt, phi, xi, yi, zi);
}
