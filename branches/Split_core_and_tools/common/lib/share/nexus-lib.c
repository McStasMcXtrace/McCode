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
* Usage: Automatically embbeded in the C code whenever required.
*
*******************************************************************************/

#ifdef USE_NEXUS

/*******************************************************************************
* mcnxfile_init: Initialize NeXus file (open it). handles NeXus 4/5 and compression
* Returns: NX_ERROR or NX_OK
*******************************************************************************/
int mcnxfile_init(char *name, char *ext, char *mode, NXhandle *nxhandle)
{
  int mcnxMode=NXACC_CREATE5;
  char mcnxExt[CHAR_BUF_LENGTH];
  strcpy(mcnxExt, ext);
  char nxversion[CHAR_BUF_LENGTH];
  int i;
  if (!mcnxversion || !strlen(mcnxversion)) strcpy(nxversion, "5 zip");
  else for (i=0; i< strlen(mcnxversion) && i < 128; nxversion[i]=tolower(mcnxversion[i++]));

  if    (strstr(nxversion,"xml")) { mcnxMode =NXACC_CREATEXML; strcpy(mcnxExt, "xml"); }
  else if (strstr(nxversion,"4")) { mcnxMode =NXACC_CREATE;    strcpy(mcnxExt, "h4"); }
  else if (strstr(nxversion,"5")) { mcnxMode =NXACC_CREATE5;   strcpy(mcnxExt, "h5"); }

  if (!strcmp(mode, "a"))    mcnxMode |= NXACC_RDWR;
  mcnxFilename = mcfull_file(name, mcnxExt);
  if (NXopen(mcnxFilename, mcnxMode, nxhandle) == NX_ERROR) {
    fprintf(stderr, "Warning: NeXus: could not open file %s\n", mcnxFilename);
    mcsiminfo_file = NULL;
  } else { mcsiminfo_file=(FILE*)mcnxFilename; }
  return(mcsiminfo_file != NULL);
}

/*******************************************************************************
* mcnxfile_close: Close NeXus file
* Returns: NX_ERROR or NX_OK
*******************************************************************************/
int mcnxfile_close(NXhandle *nxHandle)
{
  return(NXclose(nxHandle));
}

/*******************************************************************************
* mcnxinfo_header: Write general header/footer information tags (header/footer)
*                  into e.g. mcsiminfo_file, datafile
*                  Group and DataSet (information) must have been opened
* Returns: NX_ERROR or NX_OK
*******************************************************************************/
int mcnxinfo_header(NXhandle nxhandle, char *part,
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
    if (NXputattr(nxhandle, "user_name", user, strlen(user), NX_CHAR) == NX_ERROR) {
      fprintf(stderr, "Warning: NeXus: could not write header information in /%s/%s/%s\n", 
        file, instrname, valid_parent);
      return(NX_ERROR);
    }
    char creator[CHAR_BUF_LENGTH];
    sprintf(creator, "%s gerenated with " MCCODE_STRING " (" MCCODE_DATE ") [www.mccode.org]", instrname);
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
  
} /* mcnxinfo_header */

/*******************************************************************************
* mcnxinfo_tag: write a single tag
*               Group and DataSet (information) must have been opened
* Returns: NX_ERROR or NX_OK
*******************************************************************************/
int mcnxinfo_tag(NXhandle nxhandle,
    char *pre,          /* %1$s PRE */
    char *valid_section,/* %2$s SEC */
    char *name,         /* %3$s NAM */
    char *value)        /* %4$s VAL */
{
  return(NXputattr(nxhandle, name, value, strlen(value), NX_CHAR));
} /* mcnxinfo_tag */

/*******************************************************************************
* mcnxfile_instrcode: writes the instrument description file
*                   open/close a new 'description' Data Set in the current opened Group
* Returns: NX_ERROR or NX_OK
*******************************************************************************/
int mcnxfile_instrcode(NXhandle nxhandle, 
    char *name,
    char *parent)
{
  FILE *f;
  char *instr_code=NULL;
  char nxname[CHAR_BUF_LENGTH];
  int length;
  
  struct stat stfile;
  if (stat(name,&stfile) != 0) {
    instr_code = (char*)malloc(CHAR_BUF_LENGTH);
    if (instr_code) 
      sprintf(instr_code, "File %s not found (instrument description %s is missing)", 
        name, parent);
  } else {
    long filesize = stfile.st_size;
    f=fopen(name, "r");
    instr_code = (char*)malloc(filesize);
    if (instr_code && f) fread(instr_code, 1, filesize, f);
    if (f) fclose(f);
  }
  length = instr_code ? strlen(instr_code) : 0;
  if (length) {
    time_t t;
    NXmakedata(nxhandle, "description", NX_CHAR, 1, &length);
    if (NXopendata(nxhandle, "description") == NX_ERROR) {
      fprintf(stderr, "Warning: NeXus: could not write instrument description %s (%s)\n", 
        name, parent);
      free(instr_code);
      return(NX_ERROR);
    }
    NXputdata (nxhandle, instr_code);
    free(instr_code);
    NXputattr (nxhandle, "file_name", name, strlen(name), NX_CHAR);
    NXputattr (nxhandle, "file_size", &length, 1, NX_INT32);
    t=stfile.st_mtime; strncpy(nxname, ctime(&t), CHAR_BUF_LENGTH);
    NXputattr (nxhandle, "file_date", nxname, strlen(nxname), NX_CHAR);
    NXputattr (nxhandle, "MCCODE_STRING", MCCODE_STRING, strlen(MCCODE_STRING), NX_CHAR);
    NXputattr (nxhandle, "name", parent, strlen(parent), NX_CHAR);
    
    return(NXclosedata(nxhandle));
  } else
  return(NX_ERROR);
} /* mcnxfile_instrcode */

/*******************************************************************************
* mcnxfile_section: begin/end a section
*                   open a new 'section' Group and an 'information' Data Set
*                   close the current Group when part="end"
*                   the Data Set 'information' is not closed here
* Returns: NX_ERROR or NX_OK
*******************************************************************************/
int mcnxfile_section(NXhandle nxhandle, char *part,
    char *pre,          /* %1$s  PRE  */
    char *type,         /* %2$s  TYP  */
    char *name,         /* %3$s  NAM  */
    char *valid_name,   /* %4$s  VNA  */
    char *parent,       /* %5$s  PAR  */
    char *valid_parent, /* %6$s  VPA  */
    int   level)        /* %7$i  LVL */
{
  if (!strcmp(part, "end")) {
    int ret;
    ret = NXclosegroup(nxhandle);
    if (ret == NX_ERROR)
      fprintf(stderr, "Warning: NeXus: could not close group %s (%s)\n", 
          valid_name, type);
    return(ret);
  }
  
  if (!strcmp(part, "begin")) {
    int length;
    char nxtype[CHAR_BUF_LENGTH];
    char nxname[CHAR_BUF_LENGTH];
    
    if (!strcmp(type, "instrument"))      strcpy(nxname, "instrument");
    else if (!strcmp(type, "simulation")) strcpy(nxname, "simulation");
    else strcpy(nxname, valid_name);
    snprintf(nxtype, CHAR_BUF_LENGTH, "NX%s", type);
    
    NXMDisableErrorReporting(); /* unactivate NeXus error messages */
    NXmakegroup(nxhandle, nxname, nxtype);
    NXMEnableErrorReporting();  /* enable NeXus error messages */
    
    if (NXopengroup(nxhandle, nxname, nxtype) == NX_ERROR) {
      fprintf(stderr, "Warning: NeXus: could not open group %s (%s) to store 'information'\n", 
          nxname, nxtype);
      return(NX_ERROR);
    }
    /* open a SDS to store attributes */
    snprintf(nxname, CHAR_BUF_LENGTH, "Information about %s of type %s is stored in attributes", name, nxtype);
    length = strlen(nxname);
    
    NXMDisableErrorReporting(); /* unactivate NeXus error messages */
    NXmakedata(nxhandle, "information", NX_CHAR, 1, &length);
    NXMEnableErrorReporting();  /* enable NeXus error messages */
    if (NXopendata(nxhandle, "information") == NX_ERROR) {
      fprintf(stderr, "Warning: NeXus: could not open 'information' for %s (%s)\n", 
          name, nxtype);
      return(NX_ERROR);
    }
    NXputdata (nxhandle, nxname);
    NXputattr(nxhandle, "name", name, strlen(name), NX_CHAR);
    NXputattr(nxhandle, "parent", parent, strlen(parent), NX_CHAR);

  }
  return(NX_OK);
} /* mcnxfile_section */

/*******************************************************************************
* mcnxfile_section: begin/end a data block (data/errors/events)
*                   open/close a 'part' Data Set
*                   open/close Axes (except for lists)
*                   handles compressed Data Set
* Returns: NX_ERROR or NX_OK
*******************************************************************************/
/* mcnxfile_datablock: data block begin/end. Returns: NX_ERROR or NX_OK */
int mcnxfile_data(NXhandle nxhandle, MCDETECTOR detector, char *part,
  char *valid_parent, char *valid_xlabel, char *valid_ylabel, char *valid_zlabel)
{
  /* write axes, only for data */
  if (strstr(part, "data")) {
    int i;
    if (!strstr(detector.format.Name, "list")) {
    
      if (detector.m > 1) {       /* X axis */
        double axis[detector.m];
        int dim=(int)detector.m;
        for(i = 0; i < detector.m; i++)
          axis[i] = detector.xmin+(detector.xmax-detector.xmin)*(i+0.5)/detector.m;
        if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
          NXcompmakedata(nxhandle, valid_xlabel, NX_FLOAT64, 1, &dim, NX_COMP_LZW, &dim);
        else
          NXmakedata(nxhandle, valid_xlabel, NX_FLOAT64, 1, &dim);

        if (NXopendata(nxhandle, valid_xlabel) == NX_ERROR) {
          fprintf(stderr, "Warning: could not open X axis %s in %s\n",
            valid_xlabel, detector.filename);
          return(NX_ERROR);
        }
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
      
      if (detector.n >= 1) {      /* Y axis */
        double axis[detector.n];
        int dim=(int)detector.n;
        for(i = 0; i < detector.n; i++)
          axis[i] = detector.ymin+(detector.ymax-detector.ymin)*(i+0.5)/detector.n;
        if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
          NXcompmakedata(nxhandle, valid_ylabel, NX_FLOAT64, 1, &dim, NX_COMP_LZW, &dim);
        else
          NXmakedata(nxhandle, valid_ylabel, NX_FLOAT64, 1, &dim);

        if (NXopendata(nxhandle, valid_ylabel) == NX_ERROR) {
          fprintf(stderr, "Warning: could not open Y axis %s in %s\n",
            valid_ylabel, detector.filename);
          return(NX_ERROR);
        }
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
      
      if (detector.p > 1) {     /* Z axis */
        double axis[detector.p];
        int dim=(int)detector.p;
        for(i = 0; i < detector.p; i++)
          axis[i] = detector.zmin+(detector.zmax-detector.zmin)*(i+0.5)/detector.p;
        if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
          NXcompmakedata(nxhandle, valid_zlabel, NX_FLOAT64, 1, &dim, NX_COMP_LZW, &dim);
        else
          NXmakedata(nxhandle, valid_zlabel, NX_FLOAT64, 1, &dim);

        if (NXopendata(nxhandle, valid_zlabel) == NX_ERROR) {
          fprintf(stderr, "Warning: could not open Z axis %s in %s\n",
            valid_ylabel, detector.filename);
          return(NX_ERROR);
        }
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
    } /* !list */
  } /* end format != list for data */
  
  /* write data */
  int dims[3]={detector.m,detector.n,detector.p};  /* number of elements to write */
  char *nxname=part;
  double *data;
  if (strstr(part,"data"))         { data=detector.p1; }
  else if (strstr(part,"errors"))  { data=detector.p2; }
  else if (strstr(part,"ncount"))  { data=detector.p0; }
  /* ignore errors for making/opening data (in case this has already been done */
  if (strstr(mcnxversion,"compress") || strstr(mcnxversion,"zip"))
    NXmakedata(nxhandle, nxname, NX_FLOAT64, detector.rank, dims);
  else
    NXcompmakedata(nxhandle, nxname, NX_FLOAT64, detector.rank, dims, NX_COMP_LZW, dims);

  if (NXopendata(nxhandle, nxname) == NX_ERROR) {
        fprintf(stderr, "Warning: could not open DataSet '%s' in %s\n",
          nxname, detector.filename);
        return(NX_ERROR);
      }
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

#endif /* USE_NEXUS */
