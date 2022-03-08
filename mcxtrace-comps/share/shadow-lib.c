/*******************************************************************************
*
* McXtrace, X-ray tracing package
*         Copyright (C), All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Library: share/shadow-lib.c
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
* components. Embedded within instrument if MC_EMBEDDED_RUNTIME is defined.
*
* Usage: within SHARE
* %include "shadow-lib"
*
* $Id$
*
* $Log: shadow-lib.c,v $
*******************************************************************************/

#ifndef SHADOW_LIB_H
#error McXtrace : please import this library with %include "shadow-lib"
#endif

/********************************************************************************************/
#ifdef WIN32
# include <fcntl.h>
# include <io.h>
# define cSlash '\\'
#else
# define cSlash '/'
#endif

#define KM2S 1.0e+08     /* Convert k[1/AA](McXtrace) to k[1/cm](Shadow) */
#define KS2M 1.0e-08     /* Convert k[1/cm](Shadow) to k[1/AA](McXtrace) */

long     BufferSize;       /* size of the particle input and output buffer */
char*    ParDirectory;     /* parameter directory */
int      ParDirectoryLength;

/* Convert McXtrace state parameters to SHADOW x-ray structure. */

Ray mcxtrace2shadow(double x, double y, double z,
          double kx, double ky, double kz,
          double phi,
          double t,
          double Ex, double Ey, double Ez,
	  double p, int rayindex)
{
  double k;     /* ray's momentum */
  Ray ray;        /* Shadow ray structure */

/* Convert spatial coordinates */
  ray.Position[0] = -x;    
  ray.Position[1] =  z;
  ray.Position[2] =  y;

  k = sqrt(kx*kx + ky*ky + kz*kz);
  if(k == 0.0)
  {
    fprintf(stderr, "Error: zero momentum! (mcxtrace2shadow)\n");
    exit(1);
  }
/* Convert momentum to unit direction vector */
  ray.Kvector[0] = -kx/k;   
  ray.Kvector[1] =  kz/k;  
  ray.Kvector[2] =  ky/k;
/* Convert EM vector (s-) */
  ray.EMvector[0] =  p;       /* Asx */
  ray.EMvector[1] =  0.0;       /* Asy */
  ray.EMvector[2] =  0.0;       /* Asz */
/* Convert aux data */
  ray.Flag =  1;       /* Flag=1 ray passed;=0 ray absorbed */
  ray.Wavenumber = KM2S * k;       /*   q [cm-1] */
  ray.Rayindex = rayindex;       /* kk ray number */
/* Convert phases */
  ray.Phase[0] = 0.00;       /* opd */
  ray.Phase[1] =  phi;       /* Fs  */
  ray.Phase[2] = 0.00;       /* Fp  */
/* Convert EM vector (p-), not used in McXtrace */
  ray.APvector[0] = 0.00;       /* Apx */
  ray.APvector[1] = 0.00;       /* Apy */
  ray.APvector[2] = 0.00;       /* Apz */

  return ray;
}


/* Convert SHADOW state parameters to McXtrace x-ray structure. */
void shadow2mcxtrace(Ray ray,
       double *x, double *y, double *z,
       double *kx, double *ky, double *kz,
       double *Ex, double *Ey, double *Ez,
       double *phi, double *t, double *p)
{

/* Convert to McXtrace data structure  */

/* Convert spatial coordinates */
  *x = -ray.Position[0];  
  *y = ray.Position[2];
  *z = ray.Position[1];

  if(ray.Wavenumber == 0.0)
  {
    fprintf(stderr, "Error: zero wavelength! (shadow2mcxtrace: )\n");
    exit(1);
  }
/* Convert unit direction vector to momentum components*/
  *kx = -KS2M * ray.Wavenumber*ray.Kvector[0];  
  *ky =  KS2M * ray.Wavenumber*ray.Kvector[2];
  *kz =  KS2M * ray.Wavenumber*ray.Kvector[1];
/* Convert EM vector (s-) */
  *Ex=ray.EMvector[0];         /* Asx */
  *Ey=ray.EMvector[1];         /* Asy */
  *Ez=ray.EMvector[2];         /* Asz */

/* if (ncol == 13) { */
/* 		ray.Phase[0] = ray_vec[12]; */
/* 		*phi=ray.Phase[1]; */
/* 	    } else if (ncol == 18) { */
/* 		ray.Phase[0] = ray_vec[12]; */
/* 		ray.Phase[1] = ray_vec[13]; */
/* 		ray.Phase[2] = ray_vec[14]; */

  *phi=ray.Phase[1]; 

/* 		/\* Read EM vector (p) *\/ */
/* 		ray.APvector[0] = ray_vec[15]; */
/* 		ray.APvector[1] = ray_vec[16]; */
/* 		ray.APvector[2] = ray_vec[17]; */
/* 	    } */

  *t = 0.0;               /* no time info under Shadow */
  *p = ray.EMvector[0];   /* x-ray  weight */
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

/* end of shadow-lib.c */
