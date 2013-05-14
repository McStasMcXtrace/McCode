/*******************************************************************************
*
* McXtrace, X-ray tracing package
*         Copyright (C), All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/shadow-lib.h
*
* %Identification
* Written by: Andrea Prodi
* Date:   November 21, 2011
* Origin: Risoe/ILL
* Release: McXtrace 0.1
* Version: $Revision$
*
* It handles the way Shadow parses parameters.
* Functions are used by the Shadow_input and Shadow_output
* components.
*
* Usage: within SHARE
* %include "shadow-lib"
*
* $Id$
*
* $Log: shadow-lib.h,v $
*******************************************************************************/

#ifndef SHADOW_LIB_H
#define SHADOW_LIB_H "$Revision$"

 #include <math.h>
 #include <stdlib.h>
 #include <stdio.h>

 /* The X-ray structure, taken from SHADOW  source code "general.h" */
 typedef double VectorType[3];
 typedef struct
 {
  VectorType     Position;
  VectorType     Kvector;
  VectorType     EMvector;
  double         Flag;
  double         Wavenumber;
  double         Rayindex;
  VectorType     Phase;
  VectorType     APvector;
 }
 Ray;


/* extern char *shadow_infile; /\* X-ray input file name, or NULL. *\/ */
/* extern char *shadow_outfile;  /\* X-ray output file name, or NULL. *\/ */
/* extern int shadow_tracepoints;  /\* If true, use dots as progress-indicator *\/ */
/* extern int shadow_repcnt; /\* Number of times to repeat this x-ray *\/ */
/* extern int shadow_bufsize;  /\* The buffer size for x-ray read/write *\/ */

/* shadow-lib function prototypes */
/* ========================================================================= */
Ray mcxtrace2shadow(double x, double y, double z,
			double kx, double ky, double kz,
			double phi,
			double t,
			double Ex, double Ey, double Ez,
		        double p, int rayindex);
                     
void shadow2mcxtrace(Ray ray,
                   double *x, double *y, double *z,
                   double *kx, double *ky, double *kz,
                   double *Ex, double *Ey, double *Ez,
                   double *phi, double *t, double *p);

void setParDirectory (char *a);
char* FullParName(char* filename);

#endif

/* end of shadow-lib.h */