/*******************************************************************************
* Runtime system for McStas.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: mcstas-r.h
*
*	Author: K.N.			Aug 29, 1997
*
*	$Id: mcstas-r.h,v 1.2 1997-09-08 11:31:27 kn Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.1  1997/09/08 10:39:44  kn
*	Initial revision
*
*
* Copyright (C) Risoe National Laboratory, 1991-1997, All rights reserved
*******************************************************************************/

typedef double MCNUM;
typedef struct {MCNUM x, y, z;} Coords;
typedef MCNUM Rotation[3][3];

#ifdef DEBUG
#define mcDEBUG_STATE(x,y,z,vx,vy,vz,t,s1,s2,p) \
  printf("STATE: %g, %g, %g, %g, %g, %g, %g, %g, %g, %g\n", \
	 x,y,z,vx,vy,vz,t,s1,s2,p)
#else
#define mcDEBUG_STATE(x,y,z,vx,vy,vz,t,s1,s2,p)
#endif

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
