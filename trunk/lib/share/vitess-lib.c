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
* $Id: vitess-lib.c,v 1.12 2005-04-27 14:46:10 lieutenant Exp $
*
*	$Log: not supported by cvs2svn $
*	Revision 1.10  2003/02/11 12:28:46  farhi
*	Variouxs bug fixes after tests in the lib directory
*	mcstas_r  : disable output with --no-out.. flag. Fix 1D McStas output
*	read_table:corrected MC_SYS_DIR -> MCSTAS define
*	monitor_nd-lib: fix Log(signal) log(coord)
*	HOPG.trm: reduce 4000 points -> 400 which is enough and faster to resample
*	Progress_bar: precent -> percent parameter
*	CS: ----------------------------------------------------------------------
*	
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
  static unsigned long  i=0;
  double v,s;			/* Neutron speed */
  Neutron neu;			/* Vitess Neutron structure */

  neu.Position[0] = z*100;	/* Convert position from m to cm */
  neu.Position[1] = x*100;
  neu.Position[2] = y*100;
  v = sqrt(vx*vx + vy*vy + vz*vz);
  if(v == 0.0)
  {
    fprintf(stderr, "Error: zero velocity! (mcstas2vitess)\n");
    exit(1);
  }
  neu.Wavelength = 3956.0346/v;	/* Convert speed to wavelength */
  neu.Vector[0] = vz/v;		/* Convert velocity to unit direction vector */
  neu.Vector[1] = vx/v;
  neu.Vector[2] = vy/v;
  s = sqrt(sx*sx+sy*sy+sz*sz);
  if(s != 0.0)
  {
    neu.Spin[0] = sz/s;
    neu.Spin[1] = sx/s;
    neu.Spin[2] = sy/s;
  }
  
  neu.Time = t*1000;		/* Convert time from sec to msec */
  neu.Probability = p;		/* Neutron weight */
  neu.Color = 0;
  neu.Debug = 'N';
  neu.ID.IDGrp[0] = 'A';
  neu.ID.IDGrp[1] = 'A';
  neu.ID.IDNo     = i++;
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

  *x = 0.01*neu.Position[1];	/* Convert position from cm to m */
  *y = 0.01*neu.Position[2];
  *z = 0.01*neu.Position[0];
  if(neu.Wavelength == 0.0)
  {
    fprintf(stderr, "Error: zero wavelength! (mcstas2vitess: )\n");
    exit(1);
  }
  v = 3956.0346/neu.Wavelength;	/* Convert wavelength to speed */
  *vx = v*neu.Vector[1];	/* Convert unit direction vector to velocity */
  *vy = v*neu.Vector[2];
  *vz = v*neu.Vector[0];	
  *sx = neu.Spin[1];
  *sy = neu.Spin[2];
  *sz = neu.Spin[0];
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

/********************************************************************************************/

#ifdef _MSC_VER
# include <fcntl.h>
# include <io.h>
# define cSlash '\\'
#else
# define cSlash '/'
#endif

double dTimeMeas   =  0.0,  /* time of measurement           [s]   */
       dLmbdWant   =  0.0,  /* desired wavelength            [Ang] */
       dFreq       =  0.0;  /* frequency of the soure        [Hz]  */ 

long     BufferSize;      /* size of the neutron input and output buffer */
Neutron* InputNeutrons;   /* input neutron Buffer */
Neutron* OutputNeutrons;  /* output neutron buffer */

double   wei_min=0.0;     /* Minimal weight for tracing neutron */
long     keygrav=1;
short    bTrace=TRUE,     /* criterion: write trace files */
         bNewFrame=TRUE,  /* criterion: new co-ordinate system set for current module */
         bSepRate=TRUE;   /* criterion: write separate count rates */
char*    ParDirectory;    /* parameter directory */
int      ParDirectoryLength;

void setParDirectory (char *a); 

/**************************************************************/
/* Init does a general program initialization, which is ok    */
/* for all modules of the VITESS program package.             */
/**************************************************************/

void McInitVt()
{
  /* Set some default values */
  BufferSize  = 2;
  LogFilePtr  = stdout;

  if (mcdirname==NULL)
	  setParDirectory(getenv("PWD") ? getenv("PWD") : ".");
  else
    setParDirectory(mcdirname);
  idum    = -mcseed;
  keygrav = (long) mcgravitation;      /* key for gravity 1 -yes (default), 0 - no  */

  /* allocte memory for the neutron buffers */
  if((InputNeutrons=(Neutron *)calloc(BufferSize, sizeof(Neutron)))==NULL) {
    fprintf(LogFilePtr, "Couldn't allocate memory for input buffer\n");
    exit(-1);
  }
  if((OutputNeutrons=(Neutron *)calloc(BufferSize, sizeof(Neutron)))==NULL) {
    fprintf(LogFilePtr, "Couldn't allocate memory for output buffer\n");
    exit(-1);
  }

  /* initalize the random number generator */
  ran3(&idum);
}


/*****************************************************************/
/* Cleanup() does last things before the VITESS module is closed */
/* e.g. buffers are flushed and files are closed etc.            */
/* you should also write your OwnCleanup() for your module       */
/*****************************************************************/
void McCleanupVt()
{
  /* release the buffer memory */
  free(InputNeutrons);
  free(OutputNeutrons);
}


void setParDirectory (char *a) {
  int len;
  if ((len = strlen(a))) {
    /* last character should be a slash */
    if (a[len-1] == cSlash) {
      memcpy ((ParDirectory = (char *) malloc(len+1)), a, len);
    } else {
      memcpy ((ParDirectory = (char *) malloc(len+2)), a, len);
      ParDirectory[len++] = cSlash;
      ParDirectory[len] = 0;
    }
    ParDirectoryLength = len;
  }
}

/* Adding path of parameter directory to file name */
char* FullParName(char* filename)
{
  int sel=0;
  char *res, *a=NULL;
  int alen, blen;

  if (filename == 0)
     return 0;

  /* Do not change an absolute path. */
 #ifdef _MSC_VER
  /* we consider a filename with : as absolute */
  if (strstr(filename, ":")) sel = -1;
 #else
  if (filename[0] == '/') sel = -1;
 #endif
  if (sel == -1) {
    alen = 0;
  } else if (sel == 0) {
    a = ParDirectory;
    alen = ParDirectoryLength;
  } 
  blen = strlen(filename);
  if ((res = (char *) malloc(alen+blen+1)))
  { if (alen)
      strcpy(res, a);
    else
      strcpy(res,"");
    strcat(res, filename);
  }
  return res;
}

/* end of vitess-lib.c */
