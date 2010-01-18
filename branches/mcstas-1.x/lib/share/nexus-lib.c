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
* Version: $Revision: 1.12 $
*
* NeXus Runtime output functions for McStas.
* Overrides default mcstas runtime functions.
* Embedded within instrument in runtime mode.
*
* Usage: Automatically embbeded in the c code whenever required.
*
* $Id: nexus-lib.c,v 1.12 2008-02-09 22:26:27 farhi Exp $
*
* $Log: not supported by cvs2svn $
* Revision 1.11  2007/03/06 09:39:15  farhi
* NeXus default output is now "5 zip". Then NEXUS keyword is purely optional.
*
* Revision 1.10  2007/03/05 19:02:55  farhi
* NEXUS support now works as MPI. NEXUS keyword is optional and only -DUSE_NEXUS is required. All instruments may then export in NEXUS if McStas
* has been installed with --with-nexus
*
* Revision 1.9  2007/03/02 14:35:56  farhi
* Updated install doc for NeXus and reconfigure tool.
* better NeXus support with compression
*
* Revision 1.8  2007/02/24 16:44:41  farhi
* nexus support adapted partially for SNS. File name can be specified with -f option of instr.exe or mcrun or follow NEXUS keyword. The NULL filename will set 'instr_timestamp'.
*
* Revision 1.7  2007/02/09 13:21:37  farhi
* NeXus compression does not work right. Use flat NeXus as default.
*
* Revision 1.6  2007/01/26 16:23:25  farhi
* NeXus final integration (mcplot, mcgui, mcrun).
* Only mcgui initiate mcstas.nxs as default output file, whereas
* simulation may use instr_time.nxs
*
* Revision 1.5  2007/01/25 14:57:36  farhi
* NeXus output now supports MPI. Each node writes a data set in the NXdata
* group. Uses compression LZW when -DUSE_NEXUS_COMP.
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

/* NeXus output functions that replace calls to pfprintf in mcstas-r */
int mcnxfile_init(char *name, char *ext, char *mode, NXhandle *nxhandle)
{
  int mcnxMode=NXACC_CREATE5;
  char mcnxExt[10];
  strcpy(mcnxExt, ext);
  char nxversion[128];
  int i;
  if (!mcnxversion || !strlen(mcnxversion)) strcpy(nxversion, "5 zip");
  else for (i=0; i< strlen(mcnxversion) && i < 128; nxversion[i]=tolower(mcnxversion[i++]));

  if    (strstr(nxversion,"xml")) { mcnxMode =NXACC_CREATEXML; strcpy(mcnxExt, "xml"); }
  else if (strstr(nxversion,"4")) { mcnxMode =NXACC_CREATE;     }
  else if (strstr(nxversion,"5")) { mcnxMode =NXACC_CREATE5;    }

  if (!strcmp(mode, "a"))    mcnxMode |= NXACC_RDWR;
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
/* write class attributes in current SDS. Returns: NX_ERROR or NX_OK */
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
    char creator[128];
    sprintf(creator, "%s McStas " MCSTAS_VERSION " [www.mcstas.org]", instrname);
    NXputattr(nxhandle, "creator", creator, strlen(creator), NX_CHAR);
    NXputattr(nxhandle, "simulation_begin", date, strlen(date), NX_CHAR);
    char *url="http://www.nexusformat.org/";
    NXputattr(nxhandle, "URL", url, strlen(url), NX_CHAR);
    char *browser="hdfview or NXbrowse or HDFExplorer";
    NXputattr(nxhandle, "Browser", browser, strlen(browser), NX_CHAR);
#if defined (USE_MPI) || defined(USE_THREADS)
    NXputattr (nxhandle, "number_of_nodes", &mpi_node_count, 1, NX_INT32);
#endif
    return(NXputattr(nxhandle, "Format", format_name, strlen(format_name), NX_CHAR));
  } else
    return(NXputattr(nxhandle, "simulation_end", date, strlen(date), NX_CHAR));
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
  int length;
  if (!strcmp(part, "end_data"))   return(NXclosedata(nxhandle));
  if (!strcmp(part, "end"))        return(NXclosegroup(nxhandle));

  if (!strcmp(type, "instrument")) strcpy(nxname, "instrument");
  else if (!strcmp(type, "simulation")) strcpy(nxname, "simulation");
  else strcpy(nxname, valid_name);
  if (!strcmp(part, "instr_code")) {
    FILE *f;
    char *instr_code=NULL;
    struct stat stfile;
    if (stat(name,&stfile) != 0) {
      instr_code = (char*)malloc(1024);
      if (instr_code) sprintf(instr_code, "File %s not found", name);
    } else {
      long filesize = stfile.st_size;
      f=fopen(name, "r");
      instr_code = (char*)malloc(filesize);
      if (instr_code && f) fread(instr_code, 1, filesize, f);
      if (f) fclose(f);
    }
    length = strlen(instr_code);
    if (length) {
      NXmakedata(nxhandle, "instr_code", NX_CHAR, 1, &length);
        NXopendata(nxhandle, "instr_code");
        NXputdata (nxhandle, instr_code);
        NXputattr (nxhandle, "file_name", name, strlen(name), NX_CHAR);
        NXputattr (nxhandle, "file_size", &length, 1, NX_INT32);
        NXputattr (nxhandle, "McStas_version", MCSTAS_VERSION, strlen(MCSTAS_VERSION), NX_CHAR);
        NXputattr (nxhandle, "instr_name", parent, strlen(parent), NX_CHAR);
      return(NXclosedata(nxhandle));
    } else
    return(NX_ERROR);
  }
  if (!strcmp(part, "begin")) {
    char nxtype[128];
    sprintf(nxtype, "NX%s", type);
    if (NXmakegroup(nxhandle, nxname, nxtype) == NX_ERROR)
      fprintf(stderr, "Warning: could not open SDS to store %s %s information\n",
        nxname, nxtype);
    NXopengroup(nxhandle, nxname, nxtype);
    /* open a SDS to store attributes */
    sprintf(nxname, "Information about %s of type %s is stored in attributes", name, nxtype);
    length = strlen(nxname);
    NXmakedata(nxhandle, "information", NX_CHAR, 1, &length);
    NXopendata(nxhandle, "information");
    NXputdata (nxhandle, nxname);
    NXputattr(nxhandle, "name", name, strlen(name), NX_CHAR);
    NXputattr(nxhandle, "parent", parent, strlen(parent), NX_CHAR);
  }
  return(NX_OK);
} /* mcnxfile_section */

/* mcnxfile_datablock: data block begin/end. Returns: NX_ERROR or NX_OK */
int mcnxfile_datablock(NXhandle nxhandle, char *part,
      char *format, char *valid_parent, char *filename, char *xlabel, char *valid_xlabel, char *ylabel, char *valid_ylabel, char *zlabel, char *valid_zlabel, char *title, char *xvar, char *yvar, char *zvar, int  m, int  n, int  p, double x1, double x2, double y1, double y2, double z1, double z2, double *p0, double *p1, double *p2)
{
  /* write axes, only for data */
  if (strstr(part, "data")) {
    int i;
    if (!strstr(format, "list")) {
    /* X axis */
    if (m > 1) {
      double axis[m];
      for(i = 0; i < m; i++)
        axis[i] = x1+(x2-x1)*(i+0.5)/(abs(m));
      if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
        NXcompmakedata(nxhandle, valid_xlabel, NX_FLOAT64, 1, &m, NX_COMP_LZW, &m);
      else
        NXmakedata(nxhandle, valid_xlabel, NX_FLOAT64, 1, &m);

      NXopendata(nxhandle, valid_xlabel);
      NXputdata (nxhandle, axis);
      NXputattr (nxhandle, "long_name", xlabel, strlen(xlabel), NX_CHAR);
      NXputattr (nxhandle, "short_name", xvar, strlen(xvar), NX_CHAR);
      int naxis=1;
      NXputattr (nxhandle, "axis", &naxis, 1, NX_INT32);
      NXputattr (nxhandle, "units", xvar, strlen(xvar), NX_CHAR);
      int nprimary=1;
      NXputattr (nxhandle, "primary", &nprimary, 1, NX_INT32);
      NXclosedata(nxhandle);
    }
    if (n >= 1) {
      double axis[n];
      for(i = 0; i < n; i++)
        axis[i] = y1+(y2-y1)*(i+0.5)/(abs(n));
      if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
        NXcompmakedata(nxhandle, valid_ylabel, NX_FLOAT64, 1, &n, NX_COMP_LZW, &n);
      else
        NXmakedata(nxhandle, valid_ylabel, NX_FLOAT64, 1, &n);

      NXopendata(nxhandle, valid_ylabel);
      NXputdata (nxhandle, axis);
      NXputattr (nxhandle, "long_name", ylabel, strlen(ylabel), NX_CHAR);
      NXputattr (nxhandle, "short_name", yvar, strlen(yvar), NX_CHAR);
      int naxis=2;
      NXputattr (nxhandle, "axis", &naxis, 1, NX_INT32);
      NXputattr (nxhandle, "units", yvar, strlen(yvar), NX_CHAR);
      int nprimary=1;
      NXputattr (nxhandle, "primary", &nprimary, 1, NX_INT32);
      NXclosedata(nxhandle);
    }
    if (p > 1) {
      double axis[p];
      for(i = 0; i < p; i++)
        axis[i] = z1+(z2-z1)*(i+0.5)/(abs(p));
      if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
        NXcompmakedata(nxhandle, valid_zlabel, NX_FLOAT64, 1, &p, NX_COMP_LZW, &p);
      else
        NXmakedata(nxhandle, valid_zlabel, NX_FLOAT64, 1, &p);

      NXopendata(nxhandle, valid_zlabel);
      NXputdata (nxhandle, axis);
      NXputattr (nxhandle, "long_name", zlabel, strlen(zlabel), NX_CHAR);
      NXputattr (nxhandle, "short_name", zvar, strlen(zvar), NX_CHAR);
      int naxis=3;
      NXputattr (nxhandle, "axis", &naxis, 1, NX_INT32);
       NXputattr (nxhandle, "units", zvar, strlen(zvar), NX_CHAR);
      int nprimary=1;
      NXputattr (nxhandle, "primary", &nprimary, 1, NX_INT32);
      NXclosedata(nxhandle);
    }
  } } /* end format != list for data */
  /* write data */
  int rank=0;
  int dims[3];  /* number of elements to write */
  if (m > 1) { rank++; dims[0]=m; }
  if (n > 1) { rank++; dims[1]=n; }
  if (p > 1) { rank++; dims[2]=p; }
  char *nxname=part;
  double *data;
  if (strstr(part,"data"))         { data=p1; }
  else if (strstr(part,"errors"))  { data=p2; }
  else if (strstr(part,"ncount"))  { data=p0; }
  /* ignore errors for making/opening data (in case this has already been done */
  if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
    NXmakedata(nxhandle, nxname, NX_FLOAT64, rank, dims);
  else
    NXcompmakedata(nxhandle, nxname, NX_FLOAT64, rank, dims, NX_COMP_LZW, dims);

  NXopendata(nxhandle, nxname);
  int israw=(strstr(format, " raw") != NULL);
  if (data == p2 && !israw) {
    double* s = (double*)malloc(abs(m*n*p)*sizeof(double));
    if (s) {
      long    i;
      for (i=0; i<abs(m*n*p); i++)
        s[i] = mcestimate_error(p0[i],p1[i],p2[i]);
      NXputdata (nxhandle, s);
      free(s);
    } else {
      fprintf(stderr, "McStas: Out of memory for writing 'errors' in NeXus file '%s'. Writing 'raw' errors (mcnxfile_datablock)\n", filename);
      NXputdata (nxhandle, data);
      char *msg="yes: 'errors' is p^2, not sigma.";
      NXputattr(nxhandle, "raw", msg, strlen(msg), NX_CHAR);
    }
  } else
    NXputdata (nxhandle, data);
  NXputattr(nxhandle, "parent", valid_parent, strlen(valid_parent), NX_CHAR);
  int signal=1;
  if (strstr(part,"data")) {
    NXputattr(nxhandle, "signal", &signal, 1, NX_INT32);
    NXputattr(nxhandle, "short_name", filename, strlen(filename), NX_CHAR);
  }
  char nxtitle[1024];
  sprintf(nxtitle, "%s '%s'", nxname, title);
  NXputattr(nxhandle, "long_name", nxtitle, strlen(nxtitle), NX_CHAR);
  /* first write attributes */
  char creator[128];
  sprintf(creator, "%s/%s", mcinstrument_name, valid_parent);
  NXputattr(nxhandle, "creator", creator, strlen(creator), NX_CHAR);
  return(NXclosedata(nxhandle));
} /* mcnxfile_datablock */

#endif
