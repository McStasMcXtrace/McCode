/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/vitess-lib.c
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
* Functions are imported in the Virtual_imput and Virtual_output
* components. Embedded within instrument if MC_EMBEDDED_RUNTIME is defined.
*
* Usage: within SHARE
* %include "vitess-lib"
*
* $Id: vitess-lib.c,v 1.7 2003-01-21 08:47:03 pkwi Exp $
*
*	$Log: not supported by cvs2svn $
* Revision 1.2 2002/08/28 11:39:00 ef
*	Changed to lib/share/c code. 
*
* Revision 1.1 2000/08/28 11:39:00 kn
*	Initial revision
*******************************************************************************/

#ifndef VITESS_LIB_H
#error McStas : please import this library with %include "vitess-lib"
#endif  

/* Convert McStas state parameters to VITESS Neutron structure. In
   VITESS, the neutron velocity is represented by a wavelength in
   AAngstroem and a unit direction vector, time is in msec and
   positions are in cm.*/
Neutron mcstas2vitess(double x, double y, double z,
		      double vx, double vy, double vz,
		      double t, 
          double sx, double sy, double sz,
          double p)
{
  double v,s;			/* Neutron speed */
  Neutron neu;			/* Vitess Neutron structure */

  neu.Position[0] = x*100;	/* Convert position from m to cm */
  neu.Position[1] = y*100;
  neu.Position[2] = z*100;
  v = sqrt(vx*vx + vy*vy + vz*vz);
  if(v == 0.0)
  {
    fprintf(stderr, "Error: zero velocity! (mcstas2vitess)\n");
    exit(1);
  }
  neu.Wavelength = 3956.0346/v;	/* Convert speed to wavelength */
  neu.Vector[0] = vx/v;		/* Convert velocity to unit direction vector */
  neu.Vector[1] = vy/v;
  neu.Vector[2] = vz/v;
  s = sqrt(sx*sx+sy*sy+sz*sz);
  if(s != 0.0)
  {
    neu.Spin[0] = sx/s;
    neu.Spin[1] = sy/s;
    neu.Spin[2] = sz/s;
  }
  
  neu.Time = t*1000;		/* Convert time from sec to msec */
  neu.Probability = p;		/* Neutron weight */
  return neu;
}

/* Convert VITESS neutron structure to McStas state parameters. In
   VITESS, the neutron velocity is represented by a wavelength in
   AAngstroem and a unit direction vector, time is in msec and
   positions are in cm. */
void vitess2mcstas(Neutron neu,
		   double *x, double *y, double *z,
		   double *vx, double *vy, double *vz,
       double *t, 
       double *sx, double *sy, double *sz,
		   double *p)
{
  double v;			/* Neutron speed */

  *x = 0.01*neu.Position[0];	/* Convert position from cm to m */
  *y = 0.01*neu.Position[1];
  *z = 0.01*neu.Position[2];
  if(neu.Wavelength == 0.0)
  {
    fprintf(stderr, "Error: zero wavelength! (mcstas2vitess: )\n");
    exit(1);
  }
  v = 3956.0346/neu.Wavelength;	/* Convert wavelength to speed */
  *vx = v*neu.Vector[0];	/* Convert unit direction vector to velocity */
  *vy = v*neu.Vector[1];
  *vz = v*neu.Vector[2];
  *sx = neu.Spin[0];
  *sy = neu.Spin[1];
  *sz = neu.Spin[2];
  *t = 0.001*neu.Time;		/* Convert msec to sec */
  *p = neu.Probability;		/* Neutron weight */
}

/* Standard VITESS option parsing. */
char *vitess_infile;		/* Neutron input file name, or NULL. */
char *vitess_outfile;		/* Neutron output file name, or NULL. */
int vitess_tracepoints;		/* If true, use dots as progress-indicator */
int vitess_repcnt;		/* Number of times to repeat this neutron */
int vitess_bufsize;		/* The buffer size for neutron read/write */

void
vitess_option_error(char *opt)
{
  fprintf(stderr, "Error: Invalid VITESS option '%s'\n", opt);
  exit(1);
}

void
vitess_parseopt(int argc, char *argv[],
		double *dptr[], char dchr[], char **sptr[], char schr[])
{
  int i, j;

  /* Initialize variables to defaults. */
  vitess_infile = NULL;
  vitess_outfile = NULL;
  vitess_tracepoints = 0;
  vitess_repcnt = 1;
  vitess_bufsize = 10000;
  for(i = 0; dptr[i]; i++)
    *dptr[i] = 0;		/* Set all double parameters to zero */
  for(i = 0; sptr[i]; i++)
    *sptr[i] = NULL;		/* Set all string parameters to NULL */

  /* Now loop over all option arguments. */
  for(i = 1; i < argc; i++)
  {
    if(argv[i][0] != '-')
      vitess_option_error(argv[i]);
    switch(argv[i][1])
    {
      case 'f':
	vitess_infile = &argv[i][2];
	break;
      case 'F':
	vitess_outfile = &argv[i][2];
	break;
      case 'J':
	vitess_tracepoints = 1;
	break;
      case 'L':
	if(!freopen(&argv[i][2], "wt", stderr))
	{
	  fprintf(stderr, "Can't open %s for output!\n", &argv[i][2]);
	  exit(1);
	}
	break;
      case 'Z':
	srandom(atol(&(argv[i][2])));
	break;
      case 'A':
	vitess_repcnt = atol(&(argv[i][2]));
	break;
      case 'B':
	vitess_bufsize = atol(&(argv[i][2]));
	break;
      default:
	/* First look for a matching double parameter. */
	for(j = 0; dchr[j]; j++)
	{
	  if(argv[i][1] == dchr[j])
	  {
	    *dptr[j] = atof(&(argv[i][2]));
	    break;
	  }
	}
	if(!dchr[j])
	{
	  /* Then look for a matching string parameter. */
	  for(j = 0; schr[j]; j++)
	  {
	    if(argv[i][1] == schr[j])
	    {
	      *sptr[j] = &(argv[i][2]);
	      break;
	    }
	  }
	  if(!schr[j])
	    vitess_option_error(argv[i]);
	}
    }
  }
}

int vitess_main(int argc, char *argv[], int **check_finished,
		double *dptr[], char dchr[], char **sptr[], char schr[])
{
  /* ToDo: Make a cleaner interface here, to avoid relying on McStas
     internals. For example, this relies on it being ok to omit
     mcsiminfo_init() and mcsiminfo_close(). */
  srandom(time(NULL));	/* Random seed */
  vitess_parseopt(argc, argv, dptr, dchr, sptr, schr); /* VITESS-style option parser */
  mcinit();
  do
  {
    mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1);
    mcraytrace();
  } while(!**check_finished);
  mcfinally();
  return 0;
}

/* end of vitess-lib.c */
