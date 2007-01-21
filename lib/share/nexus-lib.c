/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright (C) 1997-2006, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Runtime: share/nexus-lib.c
*
* %Identification
* Written by: KN
* Date:    Jan 17, 2007
* Release: McStas 1.10
* Version: $Revision: 1.1 $
*
* NeXus Runtime output functions for McStas.
* Overrides default mcstas runtime functions.
* Embedded within instrument in runtime mode.
*
* Usage: Automatically embbeded in the c code whenever required.
*
* $Id: nexus-lib.c,v 1.1 2007-01-21 15:43:08 farhi Exp $
*
* $Log: not supported by cvs2svn $
*
*******************************************************************************/

#ifdef HAVE_LIBNEXUS

/* NeXus output functions that replace calls to pfprintf in mcstas-r */
int mcnxfile_init(char *name, char *ext, char mode, NXhandle *nxhandle)
{
  int mcnxMode=NXACC_CREATE5;
  mcnxFilename = mcfull_file(name, ext);
  if (mcnxversion==4) mcnxMode =NXACC_CREATE;
  else if (mcnxversion==5) mcnxMode =NXACC_CREATE5;
  else if (mcnxversion==0) mcnxMode =NXACC_CREATEXML;
  if (NXopen(mcnxFilename, mcnxMode, nxhandle) == NX_ERROR) {
    mcsiminfo_file = NULL;
  } else { mcsiminfo_file=(FILE*)mcnxFilename; }
  return(mcsiminfo_file != NULL);
}

int mcnxfile_close(NXhandle *nxHandle)
{
  return(NXclose(nxHandle));
}

/* mcnxfile_header: header/footer. f=mcsiminfo_file, datafile */
/* write class attributes. Returns: NX_ERROR or NX_OK */
int mcnxfile_header(NXhandle nxhandle, char *part,
    char *pre,                  /* %1$s  PRE  */
    char *instrname,            /* %2$s  SRC  */
    char *file,                 /* %3$s  FIL  */
    char *format_name,          /* %4$s  FMT  */
    char *date,                 /* %5$s  DAT  */
    char *user,                 /* %6$s  USR  */
    char *valid_parent,         /* %7$s  PAR = file */
    long  date_l)               /* %8$li DATL */
{
  if (!strcmp(part, "header")) {
    if (NXputattr(nxhandle, "user_name", user, strlen(user), NX_CHAR) == NX_ERROR)
      return(NX_ERROR);
    NXputattr(nxhandle, "file_time", date, strlen(date), NX_CHAR);
    NXputattr(nxhandle, "NeXus_version", NEXUS_VERSION,
      strlen(NEXUS_VERSION), NX_CHAR);
    if (mcnxversion==4) NXputattr(nxhandle, "HDF_version", "4", 1, NX_CHAR);
    if (mcnxversion==5) NXputattr(nxhandle, "HDF5_version", "5", 1, NX_CHAR);
    if (mcnxversion==0) NXputattr(nxhandle, "XML_version", "1.0", 3, NX_CHAR);
    char creator[128];
    sprintf(creator, "%s McStas " MCSTAS_VERSION " [www.mcstas.org]", instrname);
    NXputattr(nxhandle, "creator", creator, strlen(creator), NX_CHAR);
    char *url="http://www.nexusformat.org/";
    NXputattr(nxhandle, "URL", url, strlen(url), NX_CHAR);
    char *browser="hdfview or NXbrowse";
    NXputattr(nxhandle, "Browser", browser, strlen(browser), NX_CHAR);
    return(NXputattr(nxhandle, "Format", format_name, strlen(format_name), NX_CHAR));
  } else
    return(NXputattr(nxhandle, "file_update_time", date, strlen(date), NX_CHAR));
} /* mcnxfile_header */

/* mcnxfile_tag: tag=value in the current group. Returns: NX_ERROR or NX_OK */
int mcnxfile_tag(NXhandle nxhandle,
    char *pre,          /* %1$s PRE */
    char *valid_section,/* %2$s SEC */
    char *name,         /* %3$s NAM */
    char *value)        /* %4$s VAL */
{
  return(NXputattr(nxhandle, name, value, strlen(value), NX_CHAR));
} /* mcnxfile_tag */

/* mcnxfile_section: begin/end section. Returns: NX_ERROR or NX_OK */
int mcnxfile_section(NXhandle nxhandle, char *part,
    char *pre,          /* %1$s  PRE  */
    char *type,         /* %2$s  TYP  */
    char *name,         /* %3$s  NAM  */
    char *valid_name,   /* %4$s  VNA  */
    char *parent,       /* %5$s  PAR  */
    char *valid_parent, /* %6$s  VPA  */
    int   level)        /* %7$i  LVL */
{
  if (!strcmp(part, "begin")) {
    char nxtype[128];
    sprintf(nxtype, "NX%s", type);
    if (NXmakegroup(nxhandle, name, nxtype) == NX_ERROR) return(NX_ERROR);
    return(NXopengroup(nxhandle, name, nxtype));
  } else if (!strcmp(part, "end")) {
    return(NXclosegroup(nxhandle));
  }
} /* mcnxfile_section */

/* mcnxfile_datablock: data block begin/end. Returns: NX_ERROR or NX_OK */
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
      double *p2)
{
  /* first write attributes */
  char creator[128];
  sprintf(creator, "%s component %s", mcinstrument_name, valid_parent);
  if (NXputattr(nxhandle, "creator", creator, strlen(creator), NX_CHAR) == NX_ERROR)
    return(NX_ERROR);
  /* then writes axes, only for data */
  if (strstr(part, "data")) {
    int i;
    /* X axis */
    if (m > 1) {
      double axis[m];
      for(i = 0; i < m; i++)
        axis[i] = x1+(x2-x1)*(i+0.5)/(abs(m));
      if (NXmakedata(nxhandle, valid_xlabel, NX_FLOAT64, 1, &m) == NX_ERROR) return(NX_ERROR);
      NXopendata(nxhandle, valid_xlabel);
      NXputdata (nxhandle, axis);
    }
    if (n > 1) {
      double axis[n];
      for(i = 0; i < n; i++)
        axis[i] = y1+(y2-y1)*(i+0.5)/(abs(n));
      NXmakedata(nxhandle, valid_ylabel, NX_FLOAT64, 1, &n);
      NXopendata(nxhandle, valid_ylabel);
      NXputdata (nxhandle, axis);
    }
    if (p > 1) {
      double axis[p];
      for(i = 0; i < p; i++)
        axis[i] = z1+(z2-z1)*(i+0.5)/(abs(p));
      NXmakedata(nxhandle, valid_zlabel, NX_FLOAT64, 1, &p);
      NXopendata(nxhandle, valid_zlabel);
      NXputdata (nxhandle, axis);
    }
  }
  /* then write data */
  int rank=0;
  int dims[3];
  if (m > 1) { rank++; dims[0]=m; }
  if (n > 1) { rank++; dims[1]=n; }
  if (p > 1) { rank++; dims[2]=p; }

  if (NXmakedata(nxhandle, valid_parent, NX_FLOAT64, rank, dims) == NX_ERROR) return(NX_ERROR);
  NXopendata(nxhandle, valid_parent);
  double *data;
  if (strstr(part,"data"))    data=p1;
  if (strstr(part,"errors"))  data=p2;
  if (strstr(part,"ncount"))  data=p0;
  /* TODO: compute real errors */
  return(NXputdata (nxhandle, data));
} /* mcnxfile_datablock */

#endif
