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
* Version: 1.2
*
* This file is to be imported by the mcstas2vitess perl script 
* It handles the way Vitess parses parameters.
* Functions are used by the Virtual_input and Virtual_output
* components.
*
* Usage: within SHARE
* %include "vitess-lib"
*
* $Id: vitess-lib.h,v 1.4 2003-01-21 08:33:59 pkwi Exp $
*
*	$Log: not supported by cvs2svn $
* Revision 1.2 2002/08/28 11:39:00 ef
*	Changed to lib/share/c code
*
* Revision 1.1 2000/08/28 11:39:00 kn
*	Initial revision
*******************************************************************************/

#ifndef VITESS_LIB_H
#define VITESS_LIB_H "$Revision: 1.4 $"

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
int vitess_main(int argc, char *argv[], int **check_finished,
		double *dptr[], char dchr[], char **sptr[], char schr[]);

#endif

/* end of vitess-lib.h */
