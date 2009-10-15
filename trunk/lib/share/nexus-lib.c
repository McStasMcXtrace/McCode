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
* Version: $Revision: 1.14 $
*
* NeXus Runtime output functions for McStas.
* Overrides default mcstas runtime functions.
* Embedded within instrument in runtime mode.
*
* Usage: Automatically embbeded in the c code whenever required.
*
* $Id: nexus-lib.c,v 1.14 2008/10/21 15:19:19 farhi Exp $
*
* $Log: nexus-lib.c,v $
* Revision 1.14  2008/10/21 15:19:19  farhi
* use common CHAR_BUFFER_LENGTH = 1024
*
* Revision 1.13  2008/08/26 13:32:05  farhi
* Remove Threading support which is poor efficiency and may give wrong
* results
* Add quotes around string instrument parameters from mcgui simulation
* dialog
*
* Revision 1.12  2008/02/09 22:26:27  farhi
* Major contrib for clusters/multi-core: OpenMP support
* 	try ./configure --with-cc=gcc4.2 or icc
* then mcrun --threads ...
* Also tidy-up configure. Made relevant changes to mcrun/mcgui to enable OpenMP
* Updated install-doc accordingly
*
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
  char mcnxExt[CHAR_BUF_LENGTH];
  strcpy(mcnxExt, ext);
  char nxversion[CHAR_BUF_LENGTH];
  int i;
  if (mcdisable_output_files) return(NX_OK);
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
  if (mcdisable_output_files) return(NX_OK);
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
  if (mcdisable_output_files) return(NX_OK);
  if (!strcmp(part, "header")) {
    if (NXputattr(nxhandle, "user_name", user, strlen(user), NX_CHAR) == NX_ERROR)
      return(NX_ERROR);
    char creator[CHAR_BUF_LENGTH];
    sprintf(creator, "%s McStas " MCSTAS_VERSION " [www.mcstas.org]", instrname);
    NXputattr(nxhandle, "creator", creator, strlen(creator), NX_CHAR);
    NXputattr(nxhandle, "simulation_begin", date, strlen(date), NX_CHAR);
    char *url="http://www.nexusformat.org/";
    NXputattr(nxhandle, "URL", url, strlen(url), NX_CHAR);
    char *browser="hdfview or NXbrowse or HDFExplorer";
    NXputattr(nxhandle, "Browser", browser, strlen(browser), NX_CHAR);
#if defined (USE_MPI) 
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
  if (mcdisable_output_files) return(NX_OK);
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
  char nxname[CHAR_BUF_LENGTH];
  int length;
  if (mcdisable_output_files) return(NX_OK);
  if (!strcmp(part, "end_data"))   return(NXclosedata(nxhandle));
  if (!strcmp(part, "end"))        return(NXclosegroup(nxhandle));

  if (!strcmp(type, "instrument"))      strcpy(nxname, "instrument");
  else if (!strcmp(type, "simulation")) strcpy(nxname, "simulation");
  else strcpy(nxname, valid_name);
  if (!strcmp(part, "instr_code")) {
    FILE *f;
    char *instr_code=NULL;
    struct stat stfile;
    if (stat(name,&stfile) != 0) {
      instr_code = (char*)malloc(CHAR_BUF_LENGTH);
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
    char nxtype[CHAR_BUF_LENGTH];
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
int mcnxfile_datablock(NXhandle nxhandle, MCDETECTOR detector, char *part,
  char *valid_parent, char *valid_xlabel, char *valid_ylabel, char *valid_zlabel)
{
  if (mcdisable_output_files) return(NX_OK);
  /* write axes, only for data */
  if (strstr(part, "data")) {
    int i;
    if (!strstr(detector.format.Name, "list")) {
    /* X axis */
    if (detector.m > 1) {
      double axis[detector.m];
      for(i = 0; i < detector.m; i++)
        axis[i] = detector.xmin+(detector.xmax-detector.xmin)*(i+0.5)/detector.m;
      if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
        NXcompmakedata(nxhandle, valid_xlabel, NX_FLOAT64, 1, &detector.m, NX_COMP_LZW, &detector.m);
      else
        NXmakedata(nxhandle, valid_xlabel, NX_FLOAT64, 1, &detector.m);

      NXopendata(nxhandle, valid_xlabel);
      NXputdata (nxhandle, axis);
      NXputattr (nxhandle, "long_name", detector.xlabel, strlen(detector.xlabel), NX_CHAR);
      NXputattr (nxhandle, "short_name", detector.xvar, strlen(detector.xvar), NX_CHAR);
      int naxis=1;
      NXputattr (nxhandle, "axis", &naxis, 1, NX_INT32);
      NXputattr (nxhandle, "units", detector.xvar, strlen(detector.xvar), NX_CHAR);
      int nprimary=1;
      NXputattr (nxhandle, "primary", &nprimary, 1, NX_INT32);
      NXclosedata(nxhandle);
    }
    if (n >= 1) {
      double axis[detector.n];
      for(i = 0; i < detector.n; i++)
        axis[i] = detector.ymin+(detector.ymax-detector.ymin)*(i+0.5)/detector.n;
      if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
        NXcompmakedata(nxhandle, valid_ylabel, NX_FLOAT64, 1, &detector.n, NX_COMP_LZW, &detector.n);
      else
        NXmakedata(nxhandle, valid_ylabel, NX_FLOAT64, 1, &detector.n);

      NXopendata(nxhandle, valid_ylabel);
      NXputdata (nxhandle, axis);
      NXputattr (nxhandle, "long_name", detector.ylabel, strlen(detector.ylabel), NX_CHAR);
      NXputattr (nxhandle, "short_name", detector.yvar, strlen(detector.yvar), NX_CHAR);
      int naxis=2;
      NXputattr (nxhandle, "axis", &naxis, 1, NX_INT32);
      NXputattr (nxhandle, "units", detector.yvar, strlen(detector.yvar), NX_CHAR);
      int nprimary=1;
      NXputattr (nxhandle, "primary", &nprimary, 1, NX_INT32);
      NXclosedata(nxhandle);
    }
    if (p > 1) {
      double axis[detector.p];
      for(i = 0; i < detector.p; i++)
        axis[i] = detector.zmin+(detector.zmax-detector.zmin)*(i+0.5)/detector.p;
      if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
        NXcompmakedata(nxhandle, valid_zlabel, NX_FLOAT64, 1, &detector.p, NX_COMP_LZW, &detector.p);
      else
        NXmakedata(nxhandle, valid_zlabel, NX_FLOAT64, 1, &detector.p);

      NXopendata(nxhandle, valid_zlabel);
      NXputdata (nxhandle, axis);
      NXputattr (nxhandle, "long_name", detector.zlabel, strlen(detector.zlabel), NX_CHAR);
      NXputattr (nxhandle, "short_name", detector.zvar, strlen(detector.zvar), NX_CHAR);
      int naxis=3;
      NXputattr (nxhandle, "axis", &naxis, 1, NX_INT32);
       NXputattr (nxhandle, "units", detector.zvar, strlen(detector.zvar), NX_CHAR);
      int nprimary=1;
      NXputattr (nxhandle, "primary", &nprimary, 1, NX_INT32);
      NXclosedata(nxhandle);
    }
  } } /* end format != list for data */
  /* write data */
  int dims[3]={detector.m,detector.n,detector.p};  /* number of elements to write */
  if (detector.m > 1) { rank++; dims[0]=m; }
  if (detector.n > 1) { rank++; dims[1]=n; }
  if (detector.p > 1) { rank++; dims[2]=p; }
  char *nxname=part;
  double *data;
  if (strstr(part,"data"))         { data=p1; }
  else if (strstr(part,"errors"))  { data=p2; }
  else if (strstr(part,"ncount"))  { data=p0; }
  /* ignore errors for making/opening data (in case this has already been done */
  if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
    NXmakedata(nxhandle, nxname, NX_FLOAT64, detector.rank, dims);
  else
    NXcompmakedata(nxhandle, nxname, NX_FLOAT64, detector.rank, dims, NX_COMP_LZW, dims);

  NXopendata(nxhandle, nxname);
  NXputdata (nxhandle, data);
  NXputattr(nxhandle, "parent", valid_parent, strlen(valid_parent), NX_CHAR);
  int signal=1;
  if (strstr(part,"data")) {
    NXputattr(nxhandle, "signal", &signal, 1, NX_INT32);
    NXputattr(nxhandle, "short_name", detector.filename, strlen(detector.filename), NX_CHAR);
  }
  char nxtitle[CHAR_BUF_LENGTH];
  sprintf(nxtitle, "%s '%s'", nxname, detector.title);
  NXputattr(nxhandle, "long_name", nxtitle, strlen(nxtitle), NX_CHAR);
  /* first write attributes */
  char creator[CHAR_BUF_LENGTH];
  sprintf(creator, "%s/%s", mcinstrument_name, valid_parent);
  NXputattr(nxhandle, "creator", creator, strlen(creator), NX_CHAR);
  return(NXclosedata(nxhandle));
} /* mcnxfile_datablock */

#endif
