/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2013, All rights reserved
*         DTU Physics, Lyngby, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/ess_source-lib.h
*
* %Identification
* Written by: PW
* Date: Nov 7, 2013
* Origin: DTU Physics
* Release: McStas 2.1
* Version: 0.1
*
* This file is to be imported by the ESS_moderator_long component
* It defines a set of brilliance definitions (used via function pointer) for
* easier use of the component.
*
* Usage: within SHARE
* %include "ess_source-lib"
*
*******************************************************************************/

#ifndef ESS_SOURCE_LIB_H
#define ESS_SOURCE_LIB_H 0.1
#define ESS_SOURCE_DURATION 2.857e-3
#define ESS_SOURCE_FREQUENCY 14
#define ESS_SOURCE_POWER 5


/* Struct for extra source parameters - for future geometrical adjustments */
struct ess_struct {
  double X;
  double Y;
  double Z;
  double height;
};

typedef struct ess_struct ess_moderator_struct;

//typedef (double *) (double*, double *, double , double, double, double, ess_moderator_struct) functype;
typedef double (*functype)(double* t , double* p, double lambda,  double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras);
//struct ess_moderator_struct modextras;
//functype ESS_Mezei_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras) 

/* Maths for the curves below */
double Mezei_M(double l, double temp);
double Mezei_F(double t, double tau, int n);
double Schoenfeldt_cold(double I_SD, double alpha_SD, double lambda_SD, double alpha_l, double lambda_l, double Exponent, double I_1, double alpha_1, double I_2, double alpha_2, double lambda);
double Schoenfeldt_thermal(double I_th, double T, double I_SD, double alpha, double lambda_cf, double lambda);

/* List of brilliance definitions */
double ESS_Mezei_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_Mezei_thermal(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_Mezei_cold_2012(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_2012_Lieutenant_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_2013_Schoenfeldt_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_2013_Schoenfeldt_thermal(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras);

/* List of geometry definitions - mainly for mcdisplay... */
void ESS_mcdisplay_flat(double geometry);
void ESS_mcdisplay_TDRlike(double geometry);

/* end of ess_source-lib.h */
#endif
