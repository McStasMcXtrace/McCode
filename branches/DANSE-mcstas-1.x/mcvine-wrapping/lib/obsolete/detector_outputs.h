// -*- C++ -*-
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//                                   Jiao Lin
//                      California Institute of Technology
//                        (C) 2005 All Rights Reserved
//
// {LicenseText}
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//


#ifndef H_MCSTAS2_DETECTOR_OUTPUTS
#define H_MCSTAS2_DETECTOR_OUTPUTS

/* file i/o definitions and function prototypes */


#include <stdio.h>

struct mcformats_struct {
  char *Name;  /* may also specify: append, partial(hidden), binary */
  char *Extension;
  char *Header;
  char *Footer;
  char *BeginSection;
  char *EndSection;
  char *AssignTag;
  char *BeginData;
  char *BeginErrors;
  char *BeginNcount;
  char *EndData;
  char *EndErrors;
  char *EndNcount;
  };

/* in order to be fully portable, the format specifiers must mention each
 * fprintf parameters. In case we do not want to use some of them, we must
 * set the precision to 0.
 * ex: fprintf(f, "printed:%1$s %3$s not printed: %2$.0s\n", "1", "2", "3");
 * such are the joys of ANSI C99 and Single Unix Specification ! 
 * This 0-precision for unused data is automatically checked in mccheck_format
 * Maximum number of positional arguments is NL_RGMAX, which is 9 on System V
 * machines (Dec/Compaq/HP). Some more enjoyable  stuff !! -> we use pfprintf
 */ 
/* The mcformat.Name may contain additional keywords:
 *  partial: will not show the monitor in mcstas.sim, omit the format footer 
 *          (usually the end data), and not print the monitor sum in stdout
 */
 
#ifndef MCSTAS_VERSION
#define MCSTAS_VERSION "External Run-time"
#endif

/* function prototypes */
void mcuse_format(char *format);
void mcuse_dir(char *dir);
double mcdetector_out(char *cname, double p0, double p1, double p2, char *filename);
double mcdetector_out_0D(const char *t, double p0, double p1, double p2, char *c);
double mcdetector_out_1D(const char *t, char *xl, char *yl,
			 char *xvar, double x1, double x2, int n,
			 double *p0, double *p1, double *p2, char *f, char *c);
double mcdetector_out_2D(const char *t, char *xl, char *yl,
			 double x1, double x2, double y1, double y2, int m,
			 int n, double *p0, double *p1, double *p2, char *f, char *c);
double mcdetector_out_3D(const char *t, char *xl, char *yl, char *zl,
			 char *xvar, char *yvar, char *zvar, 
			 double x1, double x2, double y1, double y2, double z1, double z2, int m,
			 int n, int p, double *p0, double *p1, double *p2, char *f, char *c);  
void mcheader_out(FILE *f,char *parent,
		  int m, int n, int p,
		  char *xlabel, char *ylabel, char *zlabel, char *title,
		  char *xvar, char *yvar, char *zvar,
		  double x1, double x2, double y1, double y2, double z1, double z2, 
		  char *filename);  /* output header for user data file */
void mcinfo_simulation(FILE *f, struct mcformats_struct format, 
		       char *pre, char *name); /* used to add sim parameters (e.g. in Res_monitor) */
void mcsiminfo_init(FILE *f);
void mcsiminfo_close(void);



#endif// H_MCSTAS2_DETECTOR_OUTPUTS


// version
// $Id$

// Generated automatically by CxxMill on Wed Jun 28 08:35:57 2006

// End of file 
