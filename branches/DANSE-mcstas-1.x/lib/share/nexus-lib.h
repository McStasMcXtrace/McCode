/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2008, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/nexus-lib.h
*
* %Identification
* Written by: EF
* Date:    Jan 17, 2007
* Release: McStas CVS-080208
* Version: $Revision: 1.8 $
*
* NeXus Runtime system header for McStas.
* Overrides default mcstas runtime functions.
* Embedded within instrument in runtime mode.
*
* Usage: Automatically embbeded in the c code whenever required.
*
* $Id: nexus-lib.h,v 1.8 2008-02-09 22:26:27 farhi Exp $
*
* $Log: nexus-lib.h,v $
* Revision 1.8  2008-02-09 22:26:27  farhi
* Major contrib for clusters/multi-core: OpenMP support
* 	try ./configure --with-cc=gcc4.2 or icc
* then mcrun --threads ...
* Also tidy-up configure. Made relevant changes to mcrun/mcgui to enable OpenMP
* Updated install-doc accordingly
*
* Revision 1.7  2007/03/05 19:02:55  farhi
* NEXUS support now works as MPI. NEXUS keyword is optional and only -DUSE_NEXUS is required. All instruments may then export in NEXUS if McStas
* has been installed with --with-nexus
*
* Revision 1.6  2007/03/02 14:35:56  farhi
* Updated install doc for NeXus and reconfigure tool.
* better NeXus support with compression
*
* Revision 1.5  2007/02/09 13:21:38  farhi
* NeXus compression does not work right. Use flat NeXus as default.
*
* Revision 1.4  2007/01/26 16:23:25  farhi
* NeXus final integration (mcplot, mcgui, mcrun).
* Only mcgui initiate mcstas.nxs as default output file, whereas
* simulation may use instr_time.nxs
*
* Revision 1.3  2007/01/22 15:13:42  farhi
* Fully functional NeXus output format.
* Works also for lists, but as catenation is not working in NAPI, one
* has to store all in memory (e.g. with large Monitor_nD bufsize), so that
* its written in one go at the end of sim.
*
* Revision 1.2  2007/01/22 01:38:25  farhi
* Improved NeXus/NXdata support. Attributes may not be at the right place
* yet.
*
* Revision 1.1  2007/01/21 15:43:08  farhi
* NeXus support. Draft version (functional). To be tuned.
*
*
*******************************************************************************/

#ifdef USE_NEXUS

#include "napi.h"
#include <sys/stat.h>

/* NeXus variables to be used in functions */
NXhandle mcnxHandle;
char    *mcnxFilename=NULL;
char     mcnxversion[128];       /* init in cogen_init: 4,5 xml and compress */

/* NeXus output functions that replace calls to pfprintf in mcstas-r */
int mcnxfile_init(char *name, char *ext, char *mode, NXhandle *nxhandle);
int mcnxfile_close(NXhandle *nxHandle);

/* header/footer. f=mcsiminfo_file, datafile */
/* creates Entry=valid_parent+file+timestamp */
int mcnxfile_header(NXhandle nxhandle, char *part,
    char *pre,                  /* %1$s  PRE  */
    char *instrname,            /* %2$s  SRC  */
    char *file,                 /* %3$s  FIL  */
    char *format_name,          /* %4$s  FMT  */
    char *date,                 /* %5$s  DAT  */
    char *user,                 /* %6$s  USR  */
    char *valid_parent,         /* %7$s  PAR = file */
    long  date_l);               /* %8$li DATL */

/* tag=value */
int mcnxfile_tag(NXhandle nxhandle,
    char *pre,          /* %1$s PRE */
    char *valid_section,/* %2$s SEC */
    char *name,         /* %3$s NAM */
    char *value);        /* %4$s VAL */

/* begin/end section */
int mcnxfile_section(NXhandle nxhandle, char *part,
    char *pre,          /* %1$s  PRE  */
    char *type,         /* %2$s  TYP  */
    char *name,         /* %3$s  NAM  */
    char *valid_name,   /* %4$s  VNA  */
    char *parent,       /* %5$s  PAR  */
    char *valid_parent, /* %6$s  VPA  */
    int   level);        /* %7$i  LVL */

/* data block begin/end */
int mcnxfile_datablock(NXhandle nxhandle, char *part,
      char *pre,          /* %1$s   PRE  */
      char *valid_parent, /* %2$s   PAR  */
      char *filename,     /* %3$s   FIL  */
      char *xlabel,       /* %4$s   XLA  */
      char *valid_xlabel, /* %5$s   XVL  */
      char *ylabel,       /* %6$s   YLA  */
      char *valid_ylabel, /* %7$s   YVL  */
      char *zlabel,       /* %8$s   ZLA  */
      char *valid_zlabel, /* %9$s   ZVL  */
      char *title,        /* %10$s  TITL */
      char *xvar,         /* %11$s  XVAR */
      char *yvar,         /* %12$s  YVAR */
      char *zvar,         /* %13$s  ZVAR */
      int  m,            /* %14$i  MDIM */
      int  n,            /* %15$i  NDIM */
      int  p,            /* %16$i  PDIM */
      double x1,           /* %17$g  XMIN */
      double x2,           /* %18$g  XMAX */
      double y1,           /* %19$g  YMIN */
      double y2,           /* %20$g  YMAX */
      double z1,           /* %21$g  ZMIN */
      double z2,           /* %22$g  ZMAX */
      double *p0,
      double *p1,
      double *p2);

#endif
