/*****************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/pol-lib.h
*
* %Identification
* Written by: Peter Christiansen
* Date: August, 2006
* Origin: RISOE
* Release: McStas 1.10
* Version: $Revision: 4382 $
*
* This file is to be imported by polarisation components.
* It handles some shared functions.
*
* This library may be used directly as an external library. 
* It has no dependency.
*
* Usage: within SHARE
* %include "pol-lib"
*
****************************************************************************/

#ifndef POL_LIB_H
#define POL_LIB_H "$Revision: 4382 $"

// Constant used
#define mc_pol_omegaL (-2 * PI * 29.16e6) /* MHz*rad/Tesla */
#define mc_pol_mu0 (4*M_PI*1e-7)

/*example field functions should have a variable set of arguments*/
#include <stdarg.h>
#include <stddef.h>
/*macros for some stuff*/
#ifndef MCSTAS_R_H
#include <mcstas-r.h>
#endif


typedef int mcmagnet_field_func (double, double, double, double, double *, double *, double *, void *);
typedef void mcmagnet_prec_func (Coords, Rotation, _class_particle *, double);
typedef va_list mcmagnet_data;

/*here's where the mcstas magnet stack is declared*/
/*the magnet stack*/

typedef struct mcmagnet_field_info {
  int func_id;
  Rotation *rot;
  Coords *pos;
  double *field_parameters;
  int stop;
} mcmagnet_field_info;

void mc_pol_set_timestep(double);
void mc_pol_set_angular_accuracy(double);

#define mcmagnet_pack(dest,id,rotation,position,stopbit,args) \
  do { \
    mcmagnet_field_info * mctmp_p; \
    mctmp_p=(dest); \
    mctmp_p->func_id=id;\
    mctmp_p->rot=(rotation); \
    mctmp_p->pos=(position); \
    mctmp_p->stop=(stopbit); \
    mctmp_p->field_parameters=(args); \
  } while (0);

#define mcmagnet_reset() \
  do { \
    mcMagneticField=NULL; \
    mcMagnetData=NULL; \
    MAGNET_OFF; \
  } while (0);

#define mcmagnet_free(mcmagnet_desc) \
  do { \
    mcmagnet_field_info * mctmp_p=(mcmagnet_desc); \
    if (mctmp_p!=NULL) { \
      if (mctmp_p->data!=NULL) free(mctmp_p->data); \
      free(mctmp_p); \
    } \
  } while(0);

#define MCMAGNET_STOP_ARG INT_MIN

void mcmagnet_print_active();
void mcmagnet_print_field(mcmagnet_field_info *);
void mcmagnet_print_stack();

void *mcmagnet_init_par_backend(int dummy, ...);

int mcmagnet_get_field(_class_particle *_particle, double x, double y, double z, double t, double *bx,double *by, double *bz, double Bprms[8]);
void *mcmagnet_push(_class_particle *_particle, int func_id, Rotation *magnet_rot, Coords *magnet_pos, int stopbit, double Bprms[8]);
void *mcmagnet_pop(_class_particle *_particle);

/*main magnetic field dispatcher function - every request goes through here*/
int field_dispatcher(int field_id, double x, double y, double z, double t, double *bx, double *by, double *bz, void *data);

/*example functions for magnetic fields*/
int const_magnetic_field(double x, double y, double z, double t, double *bx, double *by, double *bz, void *data);
int rot_magnetic_field(double x, double y, double z, double t, double *bx, double *by, double *bz, void *data);
int majorana_magnetic_field(double x, double y, double z, double t, double *bx, double *by, double *bz, void *data);
int gradient_magnetic_field(double x, double y, double z, double t, double *bx, double *by, double *bz, void *data);

/* Routines used for Monochromator and guides/mirrors 
 * in the special (usual) case where
 * the up direction is parallel to the y-axis and 
 * the down direction is anti-parallel to the y-axis */
void GetMonoPolFNFM(double, double, double*, double*);
void GetMonoPolRefProb(double, double, double, double*);
void SetMonoPolRefOut(double, double, double, double*, double*, double*);
void SetMonoPolTransOut(double, double, double, double*, double*, double*);

// Routines for spin precession in magnetic fields
void SimpleNumMagnetPrecession(Coords, Rotation, _class_particle *, double);

// Routines to help calculate the required magnetic field
double GetConstantField(double, double, double);

#endif

/* end of pol-lib.h */
