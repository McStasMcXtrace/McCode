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
* Version: $Revision: 1.2 $
*
* NeXus Runtime output functions for McStas.
* Overrides default mcstas runtime functions.
* Embedded within instrument in runtime mode.
*
* Usage: Automatically embbeded in the c code whenever required.
*
* $Id: nexus-lib.c,v 1.2 2007-01-22 01:38:25 farhi Exp $
*
* $Log: not supported by cvs2svn $
* Revision 1.1  2007/01/21 15:43:08  farhi
* NeXus support. Draft version (functional). To be tuned.
*
*
*******************************************************************************/

#ifdef HAVE_LIBNEXUS

/* NeXus output functions that replace calls to pfprintf in mcstas-r */
int mcnxfile_init(char *name, char *ext, char mode, NXhandle *nxhandle)
{
  int mcnxMode=NXACC_CREATE5;
  char mcnxExt[10];
  strcpy(mcnxExt, ext);
  if (mcnxversion==4) { mcnxMode =NXACC_CREATE; strcpy(mcnxExt, "nx4"); }
  else if (mcnxversion==5) { mcnxMode =NXACC_CREATE5; strcpy(mcnxExt, "nx5"); }
  else if (mcnxversion==0) { mcnxMode =NXACC_CREATEXML; strcpy(mcnxExt, "xml"); }
  mcnxFilename = mcfull_file(name, mcnxExt);
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
    // NXputattr(nxhandle, "file_time", date, strlen(date), NX_CHAR);
    // NXputattr(nxhandle, "NeXus_version", NEXUS_VERSION,
    //  strlen(NEXUS_VERSION), NX_CHAR);
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
  char nxname[1024];
  if (!strcmp(type, "instrument")) strcpy(nxname, "instrument");
  else if (!strcmp(type, "simulation")) strcpy(nxname, "simulation");
  else strcpy(nxname, valid_name);
  if (!strcmp(part, "begin")) {
    char nxtype[128];
    sprintf(nxtype, "NX%s", type);
    NXmakegroup(nxhandle, nxname, nxtype);
    return(NXopengroup(nxhandle, nxname, nxtype));
  } else if (!strcmp(part, "end")) {
    return(NXclosegroup(nxhandle));
  }
} /* mcnxfile_section */

/* mcnxfile_datablock: data block begin/end. Returns: NX_ERROR or NX_OK */
int mcnxfile_datablock(NXhandle nxhandle, char *part,
      char *format, char *valid_parent, char *filename, char *xlabel, char *valid_xlabel, char *ylabel, char *valid_ylabel, char *zlabel, char *valid_zlabel, char *title, char *xvar, char *yvar, char *zvar, int  m, int  n, int  p, double x1, double x2, double y1, double y2, double z1, double z2, double *p0, double *p1, double *p2)
{
  /* first write attributes */
  char creator[128];
  sprintf(creator, "%s: component %s", mcinstrument_name, valid_parent);
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
      NXputattr (nxhandle, "long_name", xlabel, strlen(xlabel), NX_CHAR);
      NXputattr (nxhandle, "short_name", xvar, strlen(xvar), NX_CHAR);
      int naxis=1;
      NXputattr (nxhandle, "axis", &naxis, 1, NX_INT32);
    }
    if (n > 1) {
      double axis[n];
      for(i = 0; i < n; i++)
        axis[i] = y1+(y2-y1)*(i+0.5)/(abs(n));
      NXmakedata(nxhandle, valid_ylabel, NX_FLOAT64, 1, &n);
      NXopendata(nxhandle, valid_ylabel);
      NXputdata (nxhandle, axis);
      NXputattr (nxhandle, "long_name", ylabel, strlen(ylabel), NX_CHAR);
      NXputattr (nxhandle, "short_name", yvar, strlen(yvar), NX_CHAR);
      int naxis=2;
      NXputattr (nxhandle, "axis", &naxis, 1, NX_INT32);
    }
    if (p > 1) {
      double axis[p];
      for(i = 0; i < p; i++)
        axis[i] = z1+(z2-z1)*(i+0.5)/(abs(p));
      NXmakedata(nxhandle, valid_zlabel, NX_FLOAT64, 1, &p);
      NXopendata(nxhandle, valid_zlabel);
      NXputdata (nxhandle, axis);
      NXputattr (nxhandle, "long_name", zlabel, strlen(zlabel), NX_CHAR);
      NXputattr (nxhandle, "short_name", zvar, strlen(zvar), NX_CHAR);
      int naxis=3;
      NXputattr (nxhandle, "axis", &naxis, 1, NX_INT32);
    }
  }
  /* then write data */
  int rank=0;
  int dims[3];
  if (m > 1) { rank++; dims[0]=m; }
  if (n > 1) { rank++; dims[1]=n; }
  if (p > 1) { rank++; dims[2]=p; }
  char nxname[1024];
  double *data;
  if (strstr(part,"data"))    { data=p1; strcpy(nxname, "data"); }
  else if (strstr(part,"errors"))  { data=p2; strcpy(nxname, "errors"); }
  else if (strstr(part,"ncount"))  { data=p0; strcpy(nxname, "ncount"); }
  if (NXmakedata(nxhandle, nxname, NX_FLOAT64, rank, dims) == NX_ERROR) return(NX_ERROR);
  NXopendata(nxhandle, nxname);
  int ret=0;
  int israw=(strstr(format, " raw") != NULL);
  if (data == p2 && !israw) {
    double* s = (double*)malloc(abs(m*n*p)*sizeof(double));
    if (s) {
      long    i;
      for (i=0; i<abs(m*n*p); i++)
        s[i] = mcestimate_error(p0[i],p1[i],p2[i]);
      ret = NXputdata (nxhandle, s);
      free(s);
    } else {
      fprintf(stderr, "McStas: Out of memory for writing NeXus file '%s' (mcfile_datablock)\n", filename);
      ret = NXputdata (nxhandle, data);
      NXputattr(nxhandle, "raw", "yes", 3, NX_CHAR);
    }
  } else
    ret = NXputdata (nxhandle, data);
  NXputattr(nxhandle, "parent", valid_parent, strlen(valid_parent), NX_CHAR);
  int signal=1;
  if (strstr(part,"data")) NXputattr(nxhandle, "signal", &signal, 1, NX_INT32);
  char nxtitle[1024];
  sprintf(nxtitle, "%s %s", nxname, title);
  NXputattr(nxhandle, "long_name", nxtitle, strlen(nxtitle), NX_CHAR);
  return(ret);
} /* mcnxfile_datablock */

#endif
