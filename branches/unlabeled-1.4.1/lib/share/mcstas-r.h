/*******************************************************************************
* Runtime system for McStas.
*
*	Project: Monte Carlo Simulation of Tripple Axis Spectrometers
*	File name: mcstas-r.h
*
*	Author: K.N.			Aug 29, 1997
*
*	$Id: mcstas-r.h,v 1.4.1.1 1998-03-20 07:58:31 kn Exp $
*
*	$Log: not supported by cvs2svn $
 * Revision 1.5  98/03/16  08:04:16  08:04:16  kn
 * Added normal distributed random number function randnorm().
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
#include <stdio.h>

typedef double MCNUM;
typedef struct {MCNUM x, y, z;} Coords;
typedef MCNUM Rotation[3][3];

#define ABSORB {mcDEBUG_STATE(mcnlx, mcnly, mcnlz, mcnlvx, mcnlvy, mcnlvz, \
          mcnlt,mcnls1,mcnls2, mcnlp); mcDEBUG_ABSORB(); goto mcabsorb;}

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
