/*******************************************************************************
*
* McStas, the neutron ray-tracing package: Mosaic_anisotropic.comp
*         Copyright 1999-2001 Risoe National Laboratory, Roskilde, Denmark
*
* Library: vitess-lib.h
*
* This file is to be imported by the mcstas2vitess perl script 
* It handles the way Vitess parses parameters.
* Other functions are imported in the Virtual_imput and Virtual_output
* components. 
*******************************************************************************/

#ifndef VITESS_LIB_H
#define VITESS_LIB_H

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/* The Neutron structure, taken from VITESS source code "general.h" */
typedef double VectorType[3];
typedef struct
  {
    double        Time;
    double        Wavelength;
    double        Probability;
    VectorType    Position;
    VectorType    Vector;
    VectorType    Spin;
  } Neutron;

extern char *vitess_infile;	/* Neutron input file name, or NULL. */
extern char *vitess_outfile;	/* Neutron output file name, or NULL. */
extern int vitess_tracepoints;	/* If true, use dots as progress-indicator */
extern int vitess_repcnt;	/* Number of times to repeat this neutron */
extern int vitess_bufsize;	/* The buffer size for neutron read/write */

Neutron mcstas2vitess(double x, double y, double z,
                      double vx, double vy, double vz,
                      double t, 
                      double sx, double sy, double sz,
                      double p);
void vitess2mcstas(Neutron neu,
                   double *x, double *y, double *z,
                   double *vx, double *vy, double *vz,
                   double *sx, double *sy, double *sz,
                   double *t, double *p);
void vitess_option_error(char *opt);
void vitess_parseopt(int argc, char *argv[],
		     double *dptr[], char dchr[], char **sptr[], char schr[]);
int vitess_main(int argc, char *argv[], int **check_finished,
		double *dptr[], char dchr[], char **sptr[], char schr[]);

#endif
