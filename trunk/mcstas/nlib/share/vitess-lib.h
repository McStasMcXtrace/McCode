/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/vitess-lib.h
*
* %Identification
* Written by: KN, EF
* Date:   Aug 28, 2002
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision$
*
* This file is to be imported by the mcstas2vitess perl script
* It handles the way Vitess parses parameters.
* Functions are used by the Virtual_input and Virtual_output
* components.
*
* Usage: within SHARE
* %include "vitess-lib"
*
*******************************************************************************/

#ifndef VITESS_LIB_H
#define VITESS_LIB_H "$Revision$"

#ifndef GENERAL_H
 #include <math.h>
 #include <stdlib.h>
 #include <stdio.h>

 /* The Neutron structure, taken from VITESS 2.3 source code "general.h" */
 typedef double VectorType[3];
 typedef struct
 {
  char           IDGrp[2];
  unsigned long  IDNo;
 }
 TotalID;
 typedef struct
 {
  TotalID        ID;
  char           Debug;
  short          Color;
  double         Time;
  double         Wavelength;
  double         Probability;
  VectorType     Position;
  VectorType     Vector;
  VectorType     Spin;
 }
 Neutron;
#endif

extern char *vitess_infile; /* Neutron input file name, or NULL. */
extern char *vitess_outfile;  /* Neutron output file name, or NULL. */
extern int vitess_tracepoints;  /* If true, use dots as progress-indicator */
extern int vitess_repcnt; /* Number of times to repeat this neutron */
extern int vitess_bufsize;  /* The buffer size for neutron read/write */

/* vitess-lib function prototypes */
/* ========================================================================= */
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

void McInitVt();
void McCleanupVt();
void setParDirectory (char *a);
char* FullParName(char* filename);

#endif

/* end of vitess-lib.h */
