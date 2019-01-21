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
* Version: $Revision$
*
* This file is to be imported by the mcstas2vitess perl script
* It handles the way Vitess parses parameters.
* Functions are imported in the Virtual_imput and Virtual_output
* components. Embedded within instrument if MC_EMBEDDED_RUNTIME is defined.
*
* Usage: within SHARE
* %include "vitess-lib"
*
*******************************************************************************/

#ifndef VITESS_LIB_H
#error McStas : please import this library with %include "vitess-lib"
#endif

/********************************************************************************************/
#ifdef WIN32
# include <fcntl.h>
# include <io.h>
# define cSlash '\\'
#else
# define cSlash '/'
#endif

double dTimeMeas   =  0.0,  /* time of measurement           [s]   */
       dLmbdWant   =  0.0,  /* desired wavelength            [Ang] */
       dFreq       =  0.0;  /* frequency of the soure        [Hz]  */

long     BufferSize;       /* size of the neutron input and output buffer */
Neutron* InputNeutrons;    /* input neutron Buffer */
Neutron* OutputNeutrons;   /* output neutron buffer */

double   wei_min=0.0;      /* Minimal weight for tracing neutron */
long     keygrav=1;
short    bTrace=FALSE,     /* criterion: write trace files */
         bOldFrame=FALSE,  /* criterion: co-ordinate system of prev. module used for current module */
         bSepRate=FALSE;   /* criterion: write separate count rates */
char*    ParDirectory;     /* parameter directory */
int      ParDirectoryLength;

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
  double v,s;     /* Neutron speed */
  Neutron neu;      /* Vitess Neutron structure */

  neu.Position[0] = z*100;  /* Convert position from m to cm */
  neu.Position[1] = x*100;
  neu.Position[2] = y*100;
  v = sqrt(vx*vx + vy*vy + vz*vz);
  if(v == 0.0)
  {
    fprintf(stderr, "Error: zero velocity! (mcstas2vitess)\n");
    exit(1);
  }
  neu.Wavelength = 3956.0346/v; /* Convert speed to wavelength */
  neu.Vector[0] = vz/v;   /* Convert velocity to unit direction vector */
  neu.Vector[1] = vx/v;
  neu.Vector[2] = vy/v;
  s = sqrt(sx*sx+sy*sy+sz*sz);
  if(s != 0.0)
  {
    neu.Spin[0] = sz/s;
    neu.Spin[1] = sx/s;
    neu.Spin[2] = sy/s;
  }

  neu.Time = t*1000;    /* Convert time from sec to msec */
  neu.Probability = p;    /* Neutron weight */
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
  double v;     /* Neutron speed */

  *x = 0.01*neu.Position[1];  /* Convert position from cm to m */
  *y = 0.01*neu.Position[2];
  *z = 0.01*neu.Position[0];
  if(neu.Wavelength == 0.0)
  {
    fprintf(stderr, "Error: zero wavelength! (mcstas2vitess: )\n");
    exit(1);
  }
  v = 3956.0346/neu.Wavelength; /* Convert wavelength to speed */
  *vx = v*neu.Vector[1];  /* Convert unit direction vector to velocity */
  *vy = v*neu.Vector[2];
  *vz = v*neu.Vector[0];
  *sx = neu.Spin[1];
  *sy = neu.Spin[2];
  *sz = neu.Spin[0];
  *t = 0.001*neu.Time;    /* Convert msec to sec */
  *p = neu.Probability;   /* Neutron weight */
}

/* Standard VITESS option parsing. */
char *vitess_infile;    /* Neutron input file name, or NULL. */
char *vitess_outfile;   /* Neutron output file name, or NULL. */
int vitess_tracepoints;   /* If true, use dots as progress-indicator */
int vitess_repcnt;    /* Number of times to repeat this neutron */
int vitess_bufsize;   /* The buffer size for neutron read/write */

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
    *dptr[i] = 0;   /* Set all double parameters to zero */
  for(i = 0; sptr[i]; i++)
    *sptr[i] = NULL;    /* Set all string parameters to NULL */

  /* Now loop over all option arguments. */
  for(i = 1; i < argc; i++)
  {
    if(argv[i][0] != '-')
      vitess_option_error(argv[i]);

    if (argv[i][0] == '-' && (argv[i][1] == '-'))
    { switch(argv[i][2])
      {
        case 'f':
          vitess_infile = &argv[i][3];
          break;
        case 'F':
          vitess_outfile = &argv[i][3];
          break;
        case 'J':
          vitess_tracepoints = 1;
          break;
        case 'L':
          if(!freopen(&argv[i][3], "wt", stderr))
          {
            fprintf(stderr, "Can't open %s for output!\n", &argv[i][2]);
            exit(1);
          }
          LogFilePtr=stderr;
          break;
        case 'Z':
          srandom(atol(&(argv[i][3])));
          break;
        case 'G':
          mcgravitation = atol(&(argv[i][3]));
          keygrav = mcgravitation;
          break;
        case 'B':
          vitess_bufsize = atol(&(argv[i][3]));
          break;
        case 'P':
          setParDirectory(&argv[i][3]);
          break;
        case 'U':
          wei_min = atof(&(argv[i][3]));    /* minimal weight for tracing neutron */
      }
    }
    else
    { /* First look for a matching double parameter. */
      for(j = 0; dchr[j]; j++)
      {
        if(argv[i][1] == dchr[j])
        {
          *dptr[j] = atof(&(argv[i][2]));
          goto end_loop;
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
            goto end_loop;
          }
        }
        if(!schr[j])
          vitess_option_error(argv[i]);
      }
    }
end_loop:
  continue;
  }
}

void WriteInstrData(long nModuleNo, VectorType Pos, double dLength, double dRotZ, double dRotY)
{
  FILE*  pFile=NULL;
  char   *pBuffer, sBuffer[CHAR_BUF_SMALL+1];
  int    m, i;

  /* source module writes header lines */
  if (nModuleNo==0)
  { pFile = fopen( FullParName("instrument.inf"), "w");
    fprintf(pFile, "# No ID    module           len [m]  x [m]   y [m]   z [m]    hor. [deg] ver.      W-Par.       H-Par.       R-Par       number  type Description\n");
    fprintf(pFile, "# ------------------------------------------------------------------------------------------------------------------------------------------------\n");
  }
  /* first module of 2nd, 3rd ... part re-writes file up to end of previous part */
  else if (vitess_infile!=NULL)
  { i=-1;
    pFile = fopen(FullParName("instrument.inf"), "r");
    pBuffer=malloc(CHAR_BUF_SMALL*(nModuleNo+3+3));
    for (m=-2; m<nModuleNo; m++)
    { fgets (sBuffer, sizeof(sBuffer)-1, pFile);
      strcpy(&pBuffer[++i*CHAR_BUF_SMALL], sBuffer);
      if (memcmp(sBuffer, "EOP", 3)==0)
      { fgets (sBuffer, sizeof(sBuffer)-1, pFile);
        strcpy(&pBuffer[++i*CHAR_BUF_SMALL], sBuffer);
      }
    }
    fclose(pFile);
    pFile = fopen( FullParName("instrument.inf"), "w");
    for (m=0; m<=i; m++)
      fprintf(pFile, "%s", &pBuffer[CHAR_BUF_SMALL*m]);
    fprintf(pFile, "EOP\n");
    free(pBuffer);
  }
  /* each other module appends a line */
  else
  { pFile = fopen(FullParName("instrument.inf"), "a");
  }

  if (pFile) {
    char cNF=' ';
    if (bOldFrame) cNF='F';
    fprintf(pFile, "%3ld %3d %-18.18s %7.3f %7.3f %7.3f %7.3f  %8.3f %8.3f  %12.4e %12.4e %12.4e %c %5i %5d\n",
                   nModuleNo, 500, instrument_name, dLength, Pos[0], Pos[1], Pos[2],
                   180.0/M_PI*dRotZ, 180.0/M_PI*dRotY, 0.0, 0.0, 0.0, cNF, 0, 0);
    /* mark end of actual part */
    if (vitess_outfile!=NULL && nModuleNo > 0)
      fprintf(pFile, "EOP\n");
    fclose(pFile);
  }
}

void ReadInstrData(long* pModuleNo, VectorType Pos, double* pLength, double* pRotZ, double* pRotY)
{
  FILE*  pFile=NULL;
  int    nModuleID;
  long   No=0, nDum;
  char   sBuffer[CHAR_BUF_LENGTH]="", sLine[CHAR_BUF_LENGTH]="", sLineH[CHAR_BUF_LENGTH]="";

  *pModuleNo = 0;
  Pos[0]   = Pos[1] = Pos[2] = 0.0;
  *pLength = 0.0;
  *pRotY   = 0.0;
  *pRotZ   = 0.0;

  pFile = fopen(FullParName("instrument.inf"), "r");
  if (pFile)
  {
    if (vitess_infile==NULL)
    { /* Read last line and copy content, except:
		   lines containing F at pos 116-118, they have not a new frame) */
      while (ReadLine(pFile, sBuffer, sizeof(sBuffer)-1))
      { sscanf(sBuffer, "%ld", pModuleNo);
		  // ndig = short(floor(lg10(*pModuleNo));
        if (sBuffer[116]!='F' && sBuffer[117]!='F' && sBuffer[118]!='F') strcpy(sLine, sBuffer);
		}
    }
    else
    {  /* read until end of previous part, if input file is used */
      while (ReadLine(pFile, sBuffer, sizeof(sBuffer)-1))
      { if (memcmp(sBuffer, "EOP", 3)==0)
        {  *pModuleNo = No;
           strcpy(sLine, sLineH);
        }
        else
        { sscanf(sBuffer, "%ld", &No);
          if (sBuffer[116]!='F' && sBuffer[117]!='F' && sBuffer[118]!='F') strcpy(sLineH, sBuffer);
		  }
      }
      if (strlen(sLine)==0) {*pModuleNo = No; strcpy(sLine, sLineH);}
    }
    sscanf(sLine, "%ld %3d %18c %lf %lf %lf %lf %lf %lf",
                  &nDum, &nModuleID, sBuffer, pLength, &Pos[0], &Pos[1], &Pos[2], pRotZ, pRotY);
    *pRotZ *= M_PI/180.0;
    *pRotY *= M_PI/180.0;
    fclose(pFile);
  }
}


vitess_write(double NumNeutRead, double NumNeutWritten, double CntRate, double CntRateSqr,
             double dShiftX,     double dShiftY,        double dShiftZ,
             double dHorizAngle, double dVertAngle)
{
  double dRotMatrix[3][3], dRotY, dRotZ, dLength,
         CntRateErr;
  long   nModuleNo=0;
  int    k;
  VectorType Shift,  /* Shift of end position        [m] */
             EndPos; /* end position of prev. module [m] */

  fprintf(LogFilePtr,"\n\nVITESS 2.6 / McStas 1.9  module %s\n", instrument_name);

  /* update 'instrument.inf' */
  ReadInstrData(&nModuleNo, EndPos, &dLength, &dRotZ, &dRotY);
  nModuleNo++;
  Shift[0] = dShiftZ;
  Shift[1] = dShiftX;
  Shift[2] = dShiftY;
  FillRMatrixZY(dRotMatrix, dRotY, dRotZ);
  RotBackVector(dRotMatrix, Shift);
  for (k=0; k<3; k++)
    EndPos[k] += Shift[k];
  dLength += LengthVector(Shift);
  dRotZ   += dHorizAngle;
  dRotY   += dVertAngle;
  WriteInstrData(nModuleNo, EndPos, dLength, dRotZ, dRotY);

  CntRateErr = sqrt( sq(CntRate)/NumNeutWritten
                   + (NumNeutWritten*CntRateSqr-sq(CntRate)) / (NumNeutWritten-1) );
  fprintf(stderr, "%2ld number of trajectories read         : %11.0f\n", nModuleNo, NumNeutRead);
  fprintf(stderr, "   number of trajectories written      : %11.0f\n", NumNeutWritten);
  fprintf(stderr, "(time averaged) neutron count rate     : %11.4e +/- %10.3e n/s \n", CntRate, CntRateErr);

}


/**************************************************************/
/* Init does a general program initialization, which is ok    */
/* for all modules of the VITESS program package.             */
/**************************************************************/

void McInitVt()
{
  /* Set some default values */
  BufferSize  = 2;
  LogFilePtr  = stderr;

  if (dirname==NULL)
    setParDirectory(getenv("PWD") ? getenv("PWD") : ".");
  else
    setParDirectory(dirname);
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
 #ifdef WIN32
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
