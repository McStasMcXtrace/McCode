// -*- C++ -*-
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//                                   Jiao Lin
//                      California Institute of Technology
//                        (C) 2005 All Rights Reserved
//
// {LicenseText}
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//


#ifndef H_MCSTAS2_PROPAGATOR_MACROS
#define H_MCSTAS2_PROPAGATOR_MACROS


#ifndef H_MCSTAS2_TRACING_MACROS
#error propagatrs_macros must be included in tracing_macros.h
#endif


#include "propagators.h"

//strip off from mcstas-r.h

// all macros are supposed by run inside the trace() method of class "Componnt"

#define PROP_DT(dt) \
  do { \
    prop_dt(dt, (*this), x,y,z, vx,vy,vz, p,t); \
  } while(0)
 

#define PROP_Z0 \
  do { \
    prop_z0((*this), x,y,z, vx,vy,vz, p,t);	\
  } while(0)


/*
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
void mcreadparams(void);

void mcsetstate(double x, double y, double z, double vx, double vy, double vz,
		double t, double sx, double sy, double sz, double p);
void mcgenstate(void);
*/
void extend_list(int count, void **list, int *size, size_t elemsize);

void mcset_ncount(double count);
double mcget_ncount(void);
double mcget_run_num(void);
int mcstas_main(int argc, char *argv[]);


#ifndef FLT_MAX
#define       FLT_MAX         3.40282347E+38F /* max decimal value of a "float" */
#endif


#define SCATTERED mcScattered


#endif //H_MCSTAS2_PROPAGATOR_MACROS



// version
// $Id$

// End of file 
