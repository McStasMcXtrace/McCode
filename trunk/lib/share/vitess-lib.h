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
* Version: $Revision: 1.15 $
*
* This file is to be imported by the mcstas2vitess perl script
* It handles the way Vitess parses parameters.
* Functions are used by the Virtual_input and Virtual_output
* components.
*
* Usage: within SHARE
* %include "vitess-lib"
*
* $Id: vitess-lib.h,v 1.15 2006-02-06 18:16:36 lieutenant Exp $
*
* $Log: not supported by cvs2svn $
* Revision 1.14  2005/11/07 08:14:41  farhi
* Modifications by Klaus: made mcstas2vitess work again.
*
* Revision 1.13  2005/07/25 14:55:08  farhi
* DOC update:
* checked all parameter [unit] + text to be OK
* set all versions to CVS Revision
*
* Revision 1.12  2005/04/27 14:42:07  lieutenant
* structure 'Neutron' only included if 'general.h' was not included
*
* Revision 1.11  2003/02/11 12:28:46  farhi
* Variouxs bug fixes after tests in the lib directory
* mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
* read_table:corrected MC_SYS_DIR -> MCSTAS define
* monitor_nd-lib: fix Log(signal) log(coord)
* HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
* Progress_bar: precent -> percent parameter
* CS: ----------------------------------------------------------------------
*
* Revision 1.2 2002/08/28 11:39:00 ef
* Changed to lib/share/c code. Updated to Vitess 2.3 Neutron structure.
*
* Revision 1.1 2000/08/28 11:39:00 kn
* Initial revision
*******************************************************************************/

#ifndef VITESS_LIB_H
#define VITESS_LIB_H "$Revision: 1.15 $"

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
