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
  double height_t;
  double height_c;
  double Width_c;
  double tmultiplier;
  double Radius_c;
  double BeamPortAngle;
  int Uniform;
  int is60degs;
  int Wasleft;
};

typedef struct ess_struct ess_moderator_struct;

//typedef (double *) (double*, double *, double , double, double, double, ess_moderator_struct) functype;
typedef double (*functype)(double* t , double* p, double lambda,  double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras);
//struct ess_moderator_struct modextras;
//functype ESS_Mezei_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras) 

/* Maths for the curves below */
double Mezei_M(double l, double temp);
double Mezei_F(double t, double tau, int n);
double ESS_2013_Schoenfeldt_cold_spectrum(double I_SD, double alpha_SD, double lambda_SD, double alpha_l, double lambda_l, double Exponent, double I_1, double alpha_1, double I_2, double alpha_2, double lambda);
double ESS_2013_Schoenfeldt_thermal_spectrum(double I_th, double T, double I_SD, double alpha, double lambda_cf, double lambda);
double ESS_2014_Schoenfeldt_cold_spectrum(double lambda,double height);
double ESS_2014_Schoenfeldt_thermal_spectrum(double lambda, double height);
double ESS_2014_Schoenfeldt_cold_Geom_120_over_60=1;
double ESS_2014_Schoenfeldt_thermal_Geom_120_over_60=1;

/* List of brilliance definitions */
double ESS_Mezei_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_Mezei_thermal(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_Mezei_cold_2012(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_2012_Lieutenant_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_2013_Schoenfeldt_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_2013_Schoenfeldt_thermal(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras);
double ESS_2014_Schoenfeldt_cold(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras); 
double ESS_2014_Schoenfeldt_thermal(double *t, double *p, double lambda, double tfocus_w, double tfocus_t, double tfocus_dt, ess_moderator_struct extras);


/* List of pulse-shape definitions */
double TSC_Simple_TimeDist_Model(double t, double alpha, double pulselength, double gamma);
double TSC_Time_Model(double t, double lambda, double pulselength, double gamma);

/* List of moderator-geometry-weighting definitions */
double TSC_y0_Model(double y,double height, double center);

/* List of geometry definitions - mainly for mcdisplay... */
void ESS_mcdisplay_flat(double geometry);
void ESS_mcdisplay_TDRlike(double geometry);

double ESS_2014_Schoenfeldt_cold_y0(double y0,double height);
double ESS_2014_Schoenfeldt_cold_x0(double x0,double height, double width);
double ESS_2014_Schoenfeldt_thermal_y0(double y0,double height);
double ESS_2014_Schoenfeldt_thermal_x0(double x0,double height, double width);
double ESS_2014_Schoenfeldt_cold_Y(double x0,double height);
double ESS_2014_Schoenfeldt_thermal_Y(double y0,double height);
double ESS_2014_Schoenfeldt_cold_Theta120(double x0,double height);
double ESS_2014_Schoenfeldt_thermal_Theta120(double y0,double height);
double ESS_2014_Schoenfeldt_cold_Theta60(double x0,double height);
double ESS_2014_Schoenfeldt_thermal_Theta60(double y0,double height);

double TSC_alpha_of_lambda_for_t_cold(double lambda,double height);
double TSC_alpha_of_lambda_for_t_thermal(double lambda,double height);
double ESS_2014_Schoenfeldt_cold_timedist(double t, double lambda, double height, double pulselength);
double ESS_2014_Schoenfeldt_thermal_timedist(double t, double lambda, double height, double pulselength);

/* end of ess_source-lib.h */
#endif
