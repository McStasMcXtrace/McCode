/*******************************************************************************
* Runtime system for McStas.
*
* 	Project: Monte Carlo Simulation of Triple Axis Spectrometers
* 	File name: mcstas-r.c
*
* 	Author: K.N.			Aug 27, 1997
*
* Copyright (C) Risoe National Laboratory, 1997-1999, All rights reserved
*******************************************************************************/

#include <stdarg.h>
#include <limits.h>
#include <math.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef MCSTAS_R_H
#include "mcstas-r.h"
#endif


#ifdef MC_ANCIENT_COMPATIBILITY
int mctraceenabled = 0;
int mcdefaultmain = 0;
#endif

static long mcseed = 0;
mcstatic int mcdotrace = 0;
static int mcascii_only = 0;
static int mcdisable_output_files = 0;
static int mcsingle_file = 0;

static FILE *mcsiminfo_file = NULL;
static char *mcdirname = NULL;
static char *mcsiminfo_name = "mcstas.sim";

/* MCDISPLAY support. */

void mcdis_magnify(char *what){
  printf("MCDISPLAY: magnify('%s')\n", what);
}

void mcdis_line(double x1, double y1, double z1,
		double x2, double y2, double z2){
  printf("MCDISPLAY: multiline(2,%g,%g,%g,%g,%g,%g)\n",
	 x1,y1,z1,x2,y2,z2);
}

void mcdis_multiline(int count, ...){
  va_list ap;
  double x,y,z;

  printf("MCDISPLAY: multiline(%d", count);
  va_start(ap, count);
  while(count--)
  {
    x = va_arg(ap, double);
    y = va_arg(ap, double);
    z = va_arg(ap, double);
    printf(",%g,%g,%g", x, y, z);
  }
  va_end(ap);
  printf(")\n");
}

void mcdis_circle(char *plane, double x, double y, double z, double r){
  printf("MCDISPLAY: circle('%s',%g,%g,%g,%g)\n", plane, x, y, z, r);
}


/* Assign coordinates. */
Coords
coords_set(MCNUM x, MCNUM y, MCNUM z)
{
  Coords a;

  a.x = x;
  a.y = y;
  a.z = z;
  return a;
}

/* Add two coordinates. */
Coords
coords_add(Coords a, Coords b)
{
  Coords c;

  c.x = a.x + b.x;
  c.y = a.y + b.y;
  c.z = a.z + b.z;
  return c;
}

/* Subtract two coordinates. */
Coords
coords_sub(Coords a, Coords b)
{
  Coords c;

  c.x = a.x - b.x;
  c.y = a.y - b.y;
  c.z = a.z - b.z;
  return c;
}

/* Negate coordinates. */
Coords
coords_neg(Coords a)
{
  Coords b;

  b.x = -a.x;
  b.y = -a.y;
  b.z = -a.z;
  return b;
}

/*******************************************************************************
* Get transformation for rotation first phx around x axis, then phy around y,
* then phz around z.
*******************************************************************************/
void
rot_set_rotation(Rotation t, double phx, double phy, double phz)
{
  double cx = cos(phx);
  double sx = sin(phx);
  double cy = cos(phy);
  double sy = sin(phy);
  double cz = cos(phz);
  double sz = sin(phz);

  t[0][0] = cy*cz;
  t[0][1] = sx*sy*cz + cx*sz;
  t[0][2] = sx*sz - cx*sy*cz;
  t[1][0] = -cy*sz;
  t[1][1] = cx*cz - sx*sy*sz;
  t[1][2] = sx*cz + cx*sy*sz;
  t[2][0] = sy;
  t[2][1] = -sx*cy;
  t[2][2] = cx*cy;
}

/*******************************************************************************
* Matrix multiplication of transformations (this corresponds to combining
* transformations). After rot_mul(T1, T2, T3), doing T3 is equal to doing
* first T2, then T1.
* Note that T3 must not alias (use the same array as) T1 or T2.
*******************************************************************************/
void
rot_mul(Rotation t1, Rotation t2, Rotation t3)
{
  int i,j, k;

  for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++)
      t3[i][j] = t1[i][0]*t2[0][j] + t1[i][1]*t2[1][j] + t1[i][2]*t2[2][j];
}

/*******************************************************************************
* Copy a rotation transformation (needed since arrays cannot be assigned in C).
*******************************************************************************/
void
rot_copy(Rotation dest, Rotation src)
{
  dest[0][0] = src[0][0];
  dest[0][1] = src[0][1];
  dest[0][2] = src[0][2];
  dest[1][0] = src[1][0];
  dest[1][1] = src[1][1];
  dest[1][2] = src[1][2];
  dest[2][0] = src[2][0];
  dest[2][1] = src[2][1];
  dest[2][2] = src[2][2];
}

void
rot_transpose(Rotation src, Rotation dst)
{
  dst[0][0] = src[0][0];
  dst[0][1] = src[1][0];
  dst[0][2] = src[2][0];
  dst[1][0] = src[0][1];
  dst[1][1] = src[1][1];
  dst[1][2] = src[2][1];
  dst[2][0] = src[0][2];
  dst[2][1] = src[1][2];
  dst[2][2] = src[2][2];
}

Coords
rot_apply(Rotation t, Coords a)
{
  Coords b;

  b.x = t[0][0]*a.x + t[0][1]*a.y + t[0][2]*a.z;
  b.y = t[1][0]*a.x + t[1][1]*a.y + t[1][2]*a.z;
  b.z = t[2][0]*a.x + t[2][1]*a.y + t[2][2]*a.z;
  return b;
}

void
mccoordschange(Coords a, Rotation t, double *x, double *y, double *z,
	       double *vx, double *vy, double *vz, double *time,
	       double *s1, double *s2)
{
  Coords b, c;

  b.x = *x;
  b.y = *y;
  b.z = *z;
  c = rot_apply(t, b);
  b = coords_add(c, a);
  *x = b.x;
  *y = b.y;
  *z = b.z;

  b.x = *vx;
  b.y = *vy;
  b.z = *vz;
  c = rot_apply(t, b);
  *vx = c.x;
  *vy = c.y;
  *vz = c.z;
  /* ToDo: What to do about the spin? */
}


void
mccoordschange_polarisation(Rotation t, double *sx, double *sy, double *sz)
{
  Coords b, c;

  b.x = *sx;
  b.y = *sy;
  b.z = *sz;
  c = rot_apply(t, b);
  *sx = c.x;
  *sy = c.y;
  *sz = c.z;
}


/*******************************************************************************
* Find i in adaptive search tree t s.t. v(i) <= v < v(i+1).
*******************************************************************************/
int
adapt_tree_search(struct adapt_tree *t, adapt_t v)
{
  adapt_t F = 0;		/* Current value. */
  int i = 0;			/* Current candidate. */
  int step = t->initstep;
  adapt_t *s = t->s;
  int j;
  for(j = t->root; step > 0; step >>= 1)
  {
    F += s[j];			/* Cumulative value in current node */
    if(v < F)
      j -= step;		/* Value is to the left or above. */
    else
      i = j, j += step;		/* Value is current or to the right. */
  }
  /* Now j is at the bottom of a tree (a leaf node). */
  if(v < F + s[j])
    return i;
  else
    return j;
}

/*******************************************************************************
* Add v to v[i], updating the cumulative sums appropriately.
*******************************************************************************/
void
adapt_tree_add(struct adapt_tree *t, int i, adapt_t v)
{
  int j = t->root;
  int step = t->initstep;
  adapt_t *s = t->s;
  t->total += v;
  t->v[i++] += v;
  for(;;)
  {
    while(j < i)
      j += step, step >>= 1;
    s[j] += v;
    while(j > i)
      j -= step, step >>= 1;
    if(j == i)
      break;
    s[j] -= v;
  }
  if(step)
    s[j - step] -= v;
}

/*******************************************************************************
* Initialise an adaptive search tree. The tree has N nodes, and all nodes are
* initialized to zero. Any N > 0 is allowed, but is rounded up to the nearest
* value of the form N = 2**k - 2.
*******************************************************************************/
struct adapt_tree *
adapt_tree_init(int N)
{
  struct adapt_tree *t;
  int i;
  int depth;

  /* Round up to nearest 2**k - 2 */
  for(depth = 0; ((1 << (depth + 1)) - 2) < N; depth++);
  N = (1 << (depth + 1)) - 2;

  t = malloc(sizeof(*t));
  if(t)
  {
    t->s = malloc((N + 1) * sizeof(*(t->s)));
    t->v = malloc(N * sizeof(*(t->v)));
  }
  if(!(t && t->s && t->v))
  {
    fprintf(stderr, "Fatal error: out of memory\n");
    exit(1);
  }
  t->N = N;
  t->depth = depth;
  t->root = (1 << t->depth) - 1;
  t->initstep = (1 << (t->depth - 1));
  for(i = 0; i < t->N; i++)
  {
    t->s[i] = 0.0;
    t->v[i] = 0.0;
  }
  t->s[i] = 0.0;
  t->total = 0.0;
  return t;
}

/*******************************************************************************
* Free memory allocated to an adaptive search tree.
*******************************************************************************/
void
adapt_tree_free(struct adapt_tree *t)
{
  free(t->v);
  free(t->s);
  free(t);
}


double
mcestimate_error(int N, double p1, double p2)
{
  double pmean, n1;
  if(N <= 1)
    return p1;
  pmean = p1 / N;
  n1 = N - 1;
  /* Note: underflow may cause p2 to become zero; the fabs() below guards
     against this. */
  return sqrt((N/n1)*fabs(p2 - pmean*pmean));
}


/* Instrument input parameter type handling. */
static int
mcparm_double(char *s, void *vptr)
{
  char *p;
  double *v = vptr;

  *v = strtod(s, &p);
  if(*s == '\0' || (p != NULL && *p != '\0') || errno == ERANGE)
    return 0;			/* Failed */
  else
    return 1;			/* Success */
}


static char *
mcparminfo_double(char *parmname)
{
  return "double";
}


static void
mcparmerror_double(char *parm, char *val)
{
  fprintf(stderr, "Error: Invalid value '%s' for floating point parameter %s\n",
	  val, parm);
}


static void
mcparmprinter_double(FILE *f, void *vptr)
{
  double *v = vptr;
  fprintf(f, "%g", *v);
}


static int
mcparm_int(char *s, void *vptr)
{
  char *p;
  int *v = vptr;
  long x;

  *v = 0;
  x = strtol(s, &p, 10);
  if(x < INT_MIN || x > INT_MAX)
    return 0;			/* Under/overflow */
  *v = x;
  if(*s == '\0' || (p != NULL && *p != '\0') || errno == ERANGE)
    return 0;			/* Failed */
  else
    return 1;			/* Success */
}


static char *
mcparminfo_int(char *parmname)
{
  return "int";
}


static void
mcparmerror_int(char *parm, char *val)
{
  fprintf(stderr, "Error: Invalid value '%s' for integer parameter %s\n",
	  val, parm);
}


static void
mcparmprinter_int(FILE *f, void *vptr)
{
  int *v = vptr;
  fprintf(f, "%d", *v);
}


static int
mcparm_string(char *s, void *vptr)
{
  char **v = vptr;
  *v = malloc(strlen(s) + 1);
  if(*v == NULL)
  {
    fprintf(stderr, "mcparm_string: Out of memory.\n");
    exit(1);
  }
  strcpy(*v, s);
  return 1;			/* Success */
}


static char *
mcparminfo_string(char *parmname)
{
  return "string";
}


static void
mcparmerror_string(char *parm, char *val)
{
  fprintf(stderr, "Error: Invalid value '%s' for string parameter %s\n",
	  val, parm);
}


static void
mcparmprinter_string(FILE *f, void *vptr)
{
  char **v = vptr;
  char *p;
  fprintf(f, "\"");
  for(p = *v; *p != '\0'; p++)
  {
    switch(*p)
    {
      case '\n':
	fprintf(f, "\\n");
	break;
      case '\r':
	fprintf(f, "\\r");
	break;
      case '"':
	fprintf(f, "\\\"");
	break;
      case '\\':
	fprintf(f, "\\\\");
	break;
      default:
	fprintf(f, "%c", *p);
    }
  }
  fprintf(f, "\"");
}


static struct
  {
    int (*getparm)(char *, void *);
    char * (*parminfo)(char *);
    void (*error)(char *, char *);
    void (*printer)(FILE *, void *);
  } mcinputtypes[] =
      {
	mcparm_double, mcparminfo_double, mcparmerror_double,
		mcparmprinter_double,
	mcparm_int, mcparminfo_int, mcparmerror_int,
		mcparmprinter_int,
	mcparm_string, mcparminfo_string, mcparmerror_string,
		mcparmprinter_string
      };


FILE *
mcnew_file(char *name)
{
  int dirlen;
  char *mem;
  FILE *file;

  dirlen = mcdirname ? strlen(mcdirname) : 0;
  mem = malloc(dirlen + 1 + strlen(name) + 1);
  if(!mem)
  {
    fprintf(stderr, "Error: Out of memory\n");
    exit(1);
  }
  strcpy(mem, "");
  if(dirlen)
  {
    strcat(mem, mcdirname);
    if(mcdirname[dirlen - 1] != MC_PATHSEP_C &&
       name[0] != MC_PATHSEP_C)
      strcat(mem, MC_PATHSEP_S);
  }
  strcat(mem, name);
  file = fopen(mem, "w");
  if(!file)
    fprintf(stderr, "Warning: could not open output file '%s'\n", mem);
  free(mem);
  return file;
}


static void
mcinfo_out(char *pre, FILE *f)
{
  int i;

  fprintf(f, "%sName: %s\n", pre, mcinstrument_name);
  fprintf(f, "%sParameters:", pre);
  for(i = 0; i < mcnumipar; i++)
    fprintf(f, " %s(%s)", mcinputtable[i].name,
	    (*mcinputtypes[mcinputtable[i].type].parminfo)
		(mcinputtable[i].name));
  fprintf(f, "\n");
  fprintf(f, "%sInstrument-source: %s\n", pre, mcinstrument_source);
  fprintf(f, "%sTrace-enabled: %s\n", pre, mctraceenabled ? "yes" : "no");
  fprintf(f, "%sDefault-main: %s\n", pre, mcdefaultmain ? "yes" : "no");
  fprintf(f, "%sEmbedded-runtime: %s\n", pre,
#ifdef MC_EMBEDDED_RUNTIME
	 "yes"
#else
	 "no"
#endif
	 );
}


static void
mcruninfo_out(char *pre, FILE *f)
{
  int i;
  time_t t;
  if(!f)
    return;
  time(&t);
  fprintf(f, "%sDate: %s", pre, ctime(&t)); /* Note: ctime adds '\n' ! */
  fprintf(f, "%sNcount: %g\n", pre, mcget_ncount());
  fprintf(f, "%sTrace: %s\n", pre, mcdotrace ? "yes" : "no");
  if(mcseed)
    fprintf(f, "%sSeed: %ld\n", pre, mcseed);
  for(i = 0; i < mcnumipar; i++)
  {
    fprintf(f, "%sParam: %s=", pre, mcinputtable[i].name);
    (*mcinputtypes[mcinputtable[i].type].printer)(f, mcinputtable[i].par);
    fprintf(f, "\n");
  }
}


void
mcsiminfo_init(void)
{
  if(mcdisable_output_files)
    return;
  mcsiminfo_file = mcnew_file(mcsiminfo_name);
  if(!mcsiminfo_file)
    fprintf(stderr,
	    "Warning: could not open simulation description file '%s'\n",
	    mcsiminfo_name);
  else
  {
    mcsiminfo_out("begin instrument\n");
    mcinfo_out("  ", mcsiminfo_file);
    mcsiminfo_out("end instrument\n");
    mcsiminfo_out("\nbegin simulation\n");
    mcruninfo_out("  ", mcsiminfo_file);
    mcsiminfo_out("end simulation\n");
  }
}


void
mcsiminfo_out(char *format, ...)
{
  va_list ap;
  double x,y,z;

  if(mcsiminfo_file)
  {
    va_start(ap, format);
    vfprintf(mcsiminfo_file, format, ap);
    va_end(ap);
  }
}


void
mcsiminfo_close()
{
  if(mcsiminfo_file)
    fclose(mcsiminfo_file);
}


void
mcdetector_out(char *cname, double p0, double p1, double p2, char *filename)
{
  printf("Detector: %s_I=%g %s_ERR=%g %s_N=%g",
	 cname, p1, cname, mcestimate_error(p0,p1,p2), cname, p0);
  if(filename)
    printf(" \"%s\"", filename);
  printf("\n");
}


static void
mcdatainfo_out_0D(char *pre, FILE *f, char *t,
		  double I, double I_err, double N, char *c)
{
  if(!f)
    return;
  fprintf(f, "%stype: array_0d\n", pre);
  fprintf(f, "%scomponent: %s\n", pre, c);
  fprintf(f, "%stitle: %s\n", pre, t);
  fprintf(f, "%svariables: I I_err N\n", pre);
  fprintf(f, "%svalues: %g %g %g\n", pre, I, I_err, N);
}

static void
mcdatainfo_out_1D(char *pre, FILE *f, char *t, char *xl, char *yl, char *xvar,
		  double x1, double x2, int n, char *fname, char *c, int do_errb)
{
  if(!f)
    return;
  fprintf(f, "%stype: array_1d(%d)\n", pre, n);
  fprintf(f, "%scomponent: %s\n", pre, c);
  fprintf(f, "%stitle: %s\n", pre, t);
  if(fname)
    fprintf(f, "%sfilename: '%s'\n", pre, fname);
  fprintf(f, "%svariables: %s I%s\n", pre, xvar, do_errb ? " I_err N" : "");
  fprintf(f, "%sxvar: %s\n", pre, xvar);
  fprintf(f, "%syvar: %s\n", pre, do_errb ? "(I,I_err)" : "I");
  fprintf(f, "%sxlabel: '%s'\n%sylabel: '%s'\n", pre, xl, pre, yl);
  fprintf(f, "%sxlimits: %g %g\n", pre, x1, x2);
}

static void
mcdatainfo_out_2D(char *pre, FILE *f, char *t, char *xl, char *yl,
		  double x1, double x2, double y1, double y2,
		  int m, int n, char *fname, char *c)
{
  if(!f)
    return;
  fprintf(f, "%stype: array_2d(%d,%d)\n", pre, m, n);
  fprintf(f, "%scomponent: %s\n", pre, c);
  fprintf(f, "%stitle: %s\n", pre, t);
  if(fname)
    fprintf(f, "%sfilename: '%s'\n", pre, fname);
  fprintf(f, "%sxlabel: '%s'\n%sylabel: '%s'\n", pre, xl, pre, yl);
  fprintf(f, "%sxylimits: %g %g %g %g\n", pre, x1, x2, y1, y2);
}  

/*******************************************************************************
* Output single detector/monitor data (p0, p1, p2).
* Title is t, component name is c.
*******************************************************************************/
void
mcdetector_out_0D(char *t, double p0, double p1, double p2, char *c)
{
  /* Write data set information to simulation description file. */
  mcsiminfo_out("\nbegin data\n");
  mcdatainfo_out_0D("  ", mcsiminfo_file, t,
		    p1, mcestimate_error(p0, p1, p2), p0, c);
  mcsiminfo_out("end data\n");
  /* Finally give 0D detector output. */
  mcdetector_out(c, p0, p1, p2, NULL);
}


/*******************************************************************************
* Output 1d detector data (p0, p1, p2) for n bins linearly
* distributed across the range x1..x2 (x1 is lower limit of first
* bin, x2 is upper limit of last bin). Title is t, axis labels are xl
* and yl. File name is f, component name is c.
*******************************************************************************/
void
mcdetector_out_1D(char *t, char *xl, char *yl,
		  char *xvar, double x1, double x2, int n,
		  int *p0, double *p1, double *p2, char *f, char *c)
{
  int i;
  FILE *outfile = NULL;
  int Nsum;
  double Psum, P2sum;
  char *pre;
  int do_errb = p0 && p2;

  /* Write data set information to simulation description file. */
  mcsiminfo_out("\nbegin data\n");
  mcdatainfo_out_1D("  ", mcsiminfo_file, t, xl, yl, xvar, x1, x2,
		    n, f, c, do_errb);
  /* Loop over array elements, computing total sums and writing to file. */
  Nsum = Psum = P2sum = 0;
  pre = "";
  if(mcsingle_file)
  {
    outfile = mcsiminfo_file;
    f = NULL;
    mcsiminfo_out("  begin array2D (%d,%d)\n", do_errb ? 4 : 2, n);
    pre = "    ";
  }
  else if(f && !mcdisable_output_files)	/* Don't write if filename is NULL */
    outfile = mcnew_file(f);
  if(outfile && !mcascii_only && !mcsingle_file)
  {
    fprintf(outfile, "# Instrument-source: %s\n", mcinstrument_source);
    mcruninfo_out("# ", outfile);
    mcdatainfo_out_1D("# ", outfile, t, xl, yl, xvar, x1, x2,
		      n, f, c, do_errb);
  }
  for(i = 0; i < n; i++)
  {
    if(outfile)
    {
      fprintf(outfile, "%s%g %g", pre, x1 + (i + 0.5)/n*(x2 - x1), p1[i]);
      if(do_errb)
	fprintf(outfile, " %g %d", mcestimate_error(p0[i],p1[i],p2[i]), p0[i]);
      fprintf(outfile, "\n");
    }
    Nsum += p0 ? p0[i] : 1;
    Psum += p1[i];
    P2sum += p2 ? p2[i] : p1[i]*p1[i];
  }
  if(mcsingle_file)
    mcsiminfo_out("  end array2D\n");
  else if(outfile)
    fclose(outfile);
  mcsiminfo_out("end data\n");
  /* Finally give 0D detector output. */
  mcdetector_out(c, Nsum, Psum, P2sum, f);
}


void
mcdetector_out_2D(char *t, char *xl, char *yl,
		  double x1, double x2, double y1, double y2, int m,
		  int n, int *p0, double *p1, double *p2, char *f, char *c)
{
  int i, j;
  FILE *outfile = NULL;
  int Nsum;
  double Psum, P2sum;
  char *pre;

  /* Write data set information to simulation description file. */
  mcsiminfo_out("\nbegin data\n");
  mcdatainfo_out_2D("  ", mcsiminfo_file,
		    t, xl, yl, x1, x2, y1, y2, m, n, f, c);
  /* Loop over array elements, computing total sums and writing to file. */
  Nsum = Psum = P2sum = 0;
  pre = "";
  if(mcsingle_file)
  {
    outfile = mcsiminfo_file;
    f = NULL;
    mcsiminfo_out("  begin array2D (%d,%d)\n", m, n);
    pre = "    ";
  }
  else if(f && !mcdisable_output_files)	/* Don't write if filename is NULL */
    outfile = mcnew_file(f);
  if(outfile && !mcascii_only && !mcsingle_file)
  {
    fprintf(outfile, "# Instrument-source: %s\n", mcinstrument_source);
    mcruninfo_out("# ", outfile);
    mcdatainfo_out_2D("# ", outfile,
		      t, xl, yl, x1, x2, y1, y2, m, n, f, c);
  }
  for(j = 0; j < n; j++)
  {
    if(outfile)
      fprintf(outfile,"%s", pre);
    for(i = 0; i < m; i++)
    {
      if(outfile)
	fprintf(outfile, "%g ", p1[i*n + j]);
      Nsum += p0 ? p0[i*n + j] : 1;
      Psum += p1[i*n + j];
      P2sum += p2 ? p2[i*n + j] : p1[i*n + j]*p1[i*n + j];
    }
    if(outfile)
      fprintf(outfile,"\n");
  }
  if(mcsingle_file)
    mcsiminfo_out("  end array2D\n");
  else if(outfile)
    fclose(outfile);
  mcsiminfo_out("end data\n");
  /* Finally give 0D detector output. */
  mcdetector_out(c, Nsum, Psum, P2sum, f);
}


void
mcreadparams(void)
{
  int i,j,status;
  char buf[1024];
  char *p;
  int len;

  for(i = 0; mcinputtable[i].name != 0; i++)
  {
    do
    {
      printf("Set value of instrument parameter %s (%s):\n",
	     mcinputtable[i].name,
	     (*mcinputtypes[mcinputtable[i].type].parminfo)
		  (mcinputtable[i].name));
      fflush(stdout);
      p = fgets(buf, 1024, stdin);
      if(p == NULL)
      {
	fprintf(stderr, "Error: empty input\n");
	exit(1);
      }
      len = strlen(buf);
      for(j = 0; j < 2; j++)
      {
	if(len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r'))
	{
	  len--;
	  buf[len] = '\0';
	}
      }
      status = (*mcinputtypes[mcinputtable[i].type].getparm)
		   (buf, mcinputtable[i].par);
      if(!status)
      {
	(*mcinputtypes[mcinputtable[i].type].error)(mcinputtable[i].name, buf);
      }
    } while(!status);
  }
}



void
mcsetstate(double x, double y, double z, double vx, double vy, double vz,
	   double t, double sx, double sy, double sz, double p)
{
  extern double mcnx, mcny, mcnz, mcnvx, mcnvy, mcnvz;
  extern double mcnt, mcnsx, mcnsy, mcnsz, mcnp;

  mcnx = x;
  mcny = y;
  mcnz = z;
  mcnvx = vx;
  mcnvy = vy;
  mcnvz = vz;
  mcnt = t;
  mcnsx = sx;
  mcnsy = sy;
  mcnsz = sz;
  mcnp = p;
}

void
mcgenstate(void)
{
  mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1);
}

/* McStas random number routine. */

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * This is derived from the Berkeley source:
 *	@(#)random.c	5.5 (Berkeley) 7/6/88
 * It was reworked for the GNU C Library by Roland McGrath.
 * Rewritten to use reentrant functions by Ulrich Drepper, 1995.
 */

/*******************************************************************************
* Modified for McStas from glibc 2.0.7pre1 stdlib/random.c and
* stdlib/random_r.c.
*
* This way random() is more than four times faster compared to calling
* standard glibc random() on ix86 Linux, probably due to multithread support,
* ELF shared library overhead, etc. It also makes McStas generated
* simulations more portable (more likely to behave identically across
* platforms, important for parrallel computations).
*******************************************************************************/


#define	TYPE_3		3
#define	BREAK_3		128
#define	DEG_3		31
#define	SEP_3		3

static mc_int32_t randtbl[DEG_3 + 1] =
  {
    TYPE_3,

    -1726662223, 379960547, 1735697613, 1040273694, 1313901226,
    1627687941, -179304937, -2073333483, 1780058412, -1989503057,
    -615974602, 344556628, 939512070, -1249116260, 1507946756,
    -812545463, 154635395, 1388815473, -1926676823, 525320961,
    -1009028674, 968117788, -123449607, 1284210865, 435012392,
    -2017506339, -911064859, -370259173, 1132637927, 1398500161,
    -205601318,
  };

static mc_int32_t *fptr = &randtbl[SEP_3 + 1];
static mc_int32_t *rptr = &randtbl[1];
static mc_int32_t *state = &randtbl[1];
#define rand_deg DEG_3
#define rand_sep SEP_3
static mc_int32_t *end_ptr = &randtbl[sizeof (randtbl) / sizeof (randtbl[0])];

mc_int32_t
mc_random (void)
{
  mc_int32_t result;

  *fptr += *rptr;
  /* Chucking least random bit.  */
  result = (*fptr >> 1) & 0x7fffffff;
  ++fptr;
  if (fptr >= end_ptr)
  {
    fptr = state;
    ++rptr;
  }
  else
  {
    ++rptr;
    if (rptr >= end_ptr)
      rptr = state;
  }
  return result;
}

void
mc_srandom (unsigned int x)
{
  /* We must make sure the seed is not 0.  Take arbitrarily 1 in this case.  */
  state[0] = x ? x : 1;
  {
    long int i;
    for (i = 1; i < rand_deg; ++i)
    {
      /* This does:
	 state[i] = (16807 * state[i - 1]) % 2147483647;
	 but avoids overflowing 31 bits.  */
      long int hi = state[i - 1] / 127773;
      long int lo = state[i - 1] % 127773;
      long int test = 16807 * lo - 2836 * hi;
      state[i] = test + (test < 0 ? 2147483647 : 0);
    }
    fptr = &state[rand_sep];
    rptr = &state[0];
    for (i = 0; i < 10 * rand_deg; ++i)
      random ();
  }
}

/* "Mersenne Twister", by Makoto Matsumoto and Takuji Nishimura. */
/* See http://www.math.keio.ac.jp/~matumoto/emt.html for original source. */

/*   Coded by Takuji Nishimura, considering the suggestions by */
/* Topher Cooper and Marc Rieffel in July-Aug. 1997.           */

/* This library is free software; you can redistribute it and/or   */
/* modify it under the terms of the GNU Library General Public     */
/* License as published by the Free Software Foundation; either    */
/* version 2 of the License, or (at your option) any later         */
/* version.                                                        */
/* This library is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of  */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            */
/* See the GNU Library General Public License for more details.    */
/* You should have received a copy of the GNU Library General      */
/* Public License along with this library; if not, write to the    */
/* Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA   */ 
/* 02111-1307  USA                                                 */

/* Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.       */
/* When you use this, send an email to: matumoto@math.keio.ac.jp   */
/* with an appropriate reference to your work.                     */

#define N 624
#define M 397
#define MATRIX_A 0x9908b0df   /* constant vector a */
#define UPPER_MASK 0x80000000 /* most significant w-r bits */
#define LOWER_MASK 0x7fffffff /* least significant r bits */

#define TEMPERING_MASK_B 0x9d2c5680
#define TEMPERING_MASK_C 0xefc60000
#define TEMPERING_SHIFT_U(y)  (y >> 11)
#define TEMPERING_SHIFT_S(y)  (y << 7)
#define TEMPERING_SHIFT_T(y)  (y << 15)
#define TEMPERING_SHIFT_L(y)  (y >> 18)

static unsigned long mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* initializing the array with a NONZERO seed */
void
mt_srandom(unsigned long seed)
{
    /* setting initial seeds to mt[N] using         */
    /* the generator Line 25 of Table 1 in          */
    /* [KNUTH 1981, The Art of Computer Programming */
    /*    Vol. 2 (2nd Ed.), pp102]                  */
    mt[0]= seed & 0xffffffff;
    for (mti=1; mti<N; mti++)
        mt[mti] = (69069 * mt[mti-1]) & 0xffffffff;
}

unsigned long
mt_random(void)
{
    unsigned long y;
    static unsigned long mag01[2]={0x0, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if sgenrand() has not been called, */
            mt_srandom(4357); /* a default initial seed is used   */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

        mti = 0;
    }
  
    y = mt[mti++];
    y ^= TEMPERING_SHIFT_U(y);
    y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
    y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
    y ^= TEMPERING_SHIFT_L(y);

    return y;
}
#undef N
#undef M
#undef MATRIX_A
#undef UPPER_MASK
#undef LOWER_MASK
#undef TEMPERING_MASK_B
#undef TEMPERING_MASK_C
#undef TEMPERING_SHIFT_U
#undef TEMPERING_SHIFT_S
#undef TEMPERING_SHIFT_T
#undef TEMPERING_SHIFT_L
/* End of "Mersenne Twister". */

/* End of McStas random number routine. */

double
randnorm(void)
{
  static double v1, v2, s;
  static int phase = 0;
  double X, u1, u2;

  if(phase == 0)
  {
    do
    {
      u1 = rand01();
      u2 = rand01();
      v1 = 2*u1 - 1;
      v2 = 2*u2 - 1;
      s = v1*v1 + v2*v2;
    } while(s >= 1 || s == 0);

    X = v1*sqrt(-2*log(s)/s);
  }
  else
  {
    X = v2*sqrt(-2*log(s)/s);
  }

  phase = 1 - phase;
  return X;
}

/* Compute normal vector to (x,y,z). */
void normal_vec(double *nx, double *ny, double *nz,
                double x, double y, double z)
{
  double ax = fabs(x);
  double ay = fabs(y);
  double az = fabs(z);
  double l;
  if(x == 0 && y == 0 && z == 0)
  {
    *nx = 0;
    *ny = 0;
    *nz = 0;
    return;
  }
  if(ax < ay)
  {
    if(ax < az)
    {                           /* Use X axis */
      l = sqrt(z*z + y*y);
      *nx = 0;
      *ny = z/l;
      *nz = -y/l;
      return;
    }
  }
  else
  {
    if(ay < az)
    {                           /* Use Y axis */
      l = sqrt(z*z + x*x);
      *nx = z/l;
      *ny = 0;
      *nz = -x/l;
      return;
    }
  }
  /* Use Z axis */
  l = sqrt(y*y + x*x);
  *nx = y/l;
  *ny = -x/l;
  *nz = 0;
}

/* If intersection with box dt_in and dt_out is returned */
/* This function written by Stine Nyborg, 1999. */
int box_intersect(double *dt_in, double *dt_out,
                  double x, double y, double z,
                  double vx, double vy, double vz,
                  double dx, double dy, double dz)
{
  double x_in, y_in, z_in, tt, t[6], a, b;
  int i, count, s;

      /* Calculate intersection time for each of the six box surface planes
       *  If the box surface plane is not hit, the result is zero.*/
  
  if(vx != 0)
   {
    tt = -(dx/2 + x)/vx;
    y_in = y + tt*vy;
    z_in = z + tt*vz;
    if( y_in > -dy/2 && y_in < dy/2 && z_in > -dz/2 && z_in < dz/2)
      t[0] = tt;
    else
      t[0] = 0;

    tt = (dx/2 - x)/vx;
    y_in = y + tt*vy;
    z_in = z + tt*vz;
    if( y_in > -dy/2 && y_in < dy/2 && z_in > -dz/2 && z_in < dz/2)
      t[1] = tt;
    else
      t[1] = 0;
   }
  else
    t[0] = t[1] = 0;

  if(vy != 0)
   {
    tt = -(dy/2 + y)/vy;
    x_in = x + tt*vx;
    z_in = z + tt*vz;
    if( x_in > -dx/2 && x_in < dx/2 && z_in > -dz/2 && z_in < dz/2)
      t[2] = tt;
    else
      t[2] = 0;

    tt = (dy/2 - y)/vy;
    x_in = x + tt*vx;
    z_in = z + tt*vz;
    if( x_in > -dx/2 && x_in < dx/2 && z_in > -dz/2 && z_in < dz/2)
      t[3] = tt;
    else
      t[3] = 0;
   }
  else
    t[2] = t[3] = 0;

  if(vz != 0)
   {
    tt = -(dz/2 + z)/vz;
    x_in = x + tt*vx;
    y_in = y + tt*vy;
    if( x_in > -dx/2 && x_in < dx/2 && y_in > -dy/2 && y_in < dy/2)
      t[4] = tt;
    else
      t[4] = 0;

    tt = (dz/2 - z)/vz;
    x_in = x + tt*vx;
    y_in = y + tt*vy;
    if( x_in > -dx/2 && x_in < dx/2 && y_in > -dy/2 && y_in < dy/2)
      t[5] = tt;
    else
      t[5] = 0;
   }
  else
    t[4] = t[5] = 0;

  /* The intersection is evaluated and *dt_in and *dt_out are assigned */

  a = b = s = 0;
  count = 0;

  for( i = 0; i < 6; i = i + 1 )
    if( t[i] == 0 )
      s = s+1;
    else if( count == 0 )
    {
      a = t[i];
      count = 1;
    }
    else
    {
      b = t[i];
      count = 2;
    }

  if ( a == 0 && b == 0 )
    return 0;
  else if( a < b )
  {
    *dt_in = a;
    *dt_out = b;
    return 1;
  }
  else
  {
    *dt_in = b;
    *dt_out = a;
    return 1;
  }

}

/* Written by: EM,NB,ABA 4.2.98 */
int
cylinder_intersect(double *t0, double *t1, double x, double y, double z,
		   double vx, double vy, double vz, double r, double h)
{
  double v, D, t_in, t_out, y_in, y_out;

  v = sqrt(vx*vx+vy*vy+vz*vz);

  D = (2*vx*x + 2*vz*z)*(2*vx*x + 2*vz*z)
    - 4*(vx*vx + vz*vz)*(x*x + z*z - r*r);

  if (D>=0)
  {
    t_in  = (-(2*vz*z + 2*vx*x) - sqrt(D))/(2*(vz*vz + vx*vx));
    t_out = (-(2*vz*z + 2*vx*x) + sqrt(D))/(2*(vz*vz + vx*vx));
    y_in = vy*t_in + y;
    y_out =vy*t_out + y;

    if ( (y_in > h/2 && y_out > h/2) || (y_in < -h/2 && y_out < -h/2) )
      return 0;
    else
    {
      if (y_in > h/2)
	t_in = ((h/2)-y)/vy;
      if (y_in < -h/2)
	t_in = ((-h/2)-y)/vy;
      if (y_out > h/2)
	t_out = ((h/2)-y)/vy;
      if (y_out < -h/2)
	t_out = ((-h/2)-y)/vy;
    }
    *t0 = t_in;
    *t1 = t_out;
    return 1;
  }
  else
  {
    *t0 = *t1 = 0;
    return 0;
  }
}


/* Calculate intersection between line and sphere. */
int
sphere_intersect(double *t0, double *t1, double x, double y, double z,
		 double vx, double vy, double vz, double r)
{
  double A, B, C, D, v;

  v = sqrt(vx*vx + vy*vy + vz*vz);
  A = v*v;
  B = 2*(x*vx + y*vy + z*vz);
  C = x*x + y*y + z*z - r*r;
  D = B*B - 4*A*C;
  if(D < 0)
    return 0;
  D = sqrt(D);
  *t0 = (-B - D) / (2*A);
  *t1 = (-B + D) / (2*A);
  return 1;
}

/* plane_intersect_Gfast 
 * intersection of a plane and a trajectory with gravitation */
/* this function calculates the intersection between a neutron trajectory
 * and a plane with acceleration gx,gy,gz. The neutron starts at point x,y,z
 * with velocity vx, vy, vz. The plane has a normal vector nx,ny,nz and 
 * contains the point wx,wy,wz
 * The function returns 0 if no intersection occured after the neutron started
 * and 1 if there is an intersection. Then *Idt is the elapsed time until 
 * the neutron hits the roof.
 */
/* Let n=(nx,ny,nz) be the normal plane vector (one of the six sides) 
 * Let W=(wx,wy,wz) be Any point on this plane (for instance at z=0)
 * The problem consists in solving the 2nd order equation:
 *      1/2.n.g.t^2 + n.v.t + n.(r-W) = 0 (1)
 * Without acceleration, t=-n.(r-W)/n.v
 */
  
int plane_intersect_Gfast(double *Idt, double A, double B, double C)
{
    /* plane_intersect_Gfast(&dt, A, B, C)
     * A = 0.5 n.g; B = n.v; C = n.(r-W);
     * no cceleration when A=0
     */
    double D, sD;
    double dt1, dt2;
    
    *Idt = -1;
    
    if (A == 0) /* this plane is parallel to the acceleration */
    {
      if (B == 0)  /* the speed is parallel to the plane, no intersection */
        return (0);
      else  /* no acceleration case */
        { *Idt = -C/B; 
          if (*Idt >= 0) return (2);
          else return (0); }
    }
    else
    {
      /* Delta > 0: neutron trajectory hits the mirror */
      D = B*B - 4*A*C;
      if (D >= 0)
      {
        sD = sqrt(D);
        dt1 = (-B + sD)/2/A;
        dt2 = (-B - sD)/2/A;
        if (dt1 <0 && dt2 >=0) *Idt = dt2;
        else
        if (dt2 <0 && dt1 >=0) *Idt = dt1;
        else
        if (dt1 <0 && dt2 < 0) return (0);
        else
        if (dt1 < dt2) *Idt = dt1;
        else
          *Idt = dt2;
        return (1);
      }
      else  /* Delta <0: no intersection */
        return (0);
    }     
}

/* Choose random direction towards target at (x,y,z) with given radius. */
/* If radius is zero, choose random direction in full 4PI, no target. */
/* ToDo: It should be possible to optimize this to avoid computing angles. */
void
randvec_target_sphere(double *xo, double *yo, double *zo, double *solid_angle,
	       double xi, double yi, double zi, double radius)
{
  double l, theta0, phi, theta, nx, ny, nz, xt, yt, zt;

  if(radius == 0.0)
  {
    /* No target, choose uniformly a direction in full 4PI solid angle. */
    theta = acos (1 - rand0max(2));
    phi = rand0max(2 * PI);
    if(solid_angle)
      *solid_angle = 4*PI;
    nx = 1;
    ny = 0;
    nz = 0;
    xi = 0;
    yi = 1;
    zi = 0;
  }
  else
  {
    l = sqrt(xi*xi + yi*yi + zi*zi); /* Distance to target. */
    theta0 = asin(radius / l);	/* Target size as seen from origin */
    if(solid_angle)
    {
      /* Compute solid angle of target as seen from origin. */
      *solid_angle = 2*PI*(1 - cos(theta0));
    }

    /* Now choose point uniformly on sphere surface within angle theta0 */
    theta = acos (1 - rand0max(1 - cos(theta0)));
    phi = rand0max(2 * PI);
    /* Now, to obtain the desired vector rotate (x,y,z) angle theta around a
       perpendicular axis (nx,ny,nz) and then angle phi around (x,y,z). */
    if(xi == 0 && yi == 0)
    {
      nx = 1;
      ny = 0;
      nz = 0;
    }
    else
    {
      nx = -yi;
      ny = xi;
      nz = 0;
    }
  }
  rotate(xt, yt, zt, xi, yi, zi, theta, nx, ny, nz);
  rotate(*xo, *yo, *zo, xt, yt, zt, phi, xi, yi, zi);
}

/* Make sure a list is big enough to hold element COUNT.
*
* The list is an array, and the argument 'list' is a pointer to a pointer to
* the array start. The argument 'size' is a pointer to the number of elements
* in the array. The argument 'elemsize' is the sizeof() an element. The
* argument 'count' is the minimum number of elements needed in the list.
*
* If the old array is to small (or if *list is NULL or *size is 0), a
* sufficuently big new array is allocated, and *list and *size are updated.
*/
void extend_list(int count, void **list, int *size, size_t elemsize)
{
  if(count >= *size)
  {
    void *oldlist = *list;
    if(*size > 0)
      *size *= 2;
    else
      *size = 32;
    *list = malloc(*size*elemsize);
    if(!*list)
    {
      fprintf(stderr, "\nFatal error: Out of memory.\n");
      exit(1);
    }
    if(oldlist)
    {
      memcpy(*list, oldlist, count*elemsize);
      free(oldlist);
    }
  }
}

/* Number of neutron histories to simulate. */
static double mcncount = 1e6;
double mcrun_num = 0;

void
mcset_ncount(double count) 
{
  mcncount = count;
}

double
mcget_ncount(void)
{
  return mcncount;
}

double
mcget_run_num(void)
{
  return mcrun_num;
}

static void
mcsetn_arg(char *arg)
{
  mcset_ncount(strtod(arg, NULL));
}

static void
mcsetseed(char *arg)
{
  mcseed = atol(arg);
  if(mcseed)
    srandom(mcseed);
  else
  {
    fprintf(stderr, "Error seed most not be zero.\n");
    exit(1);
  }
}

static void
mchelp(char *pgmname)
{
  int i;

  fprintf(stderr, "Usage: %s [options] [parm=value ...]\n", pgmname);
  fprintf(stderr,
"Options are:\n"
"  -s SEED   --seed=SEED      Set random seed (must be != 0)\n"
"  -n COUNT  --ncount=COUNT   Set number of neutrons to simulate.\n"
"  -d DIR    --dir=DIR        Put all data files in directory DIR.\n"
"  -f FILE   --file=FILE      Put all data in a single file.\n"
"  -t        --trace          Enable trace of neutron through instrument.\n"
"  -a        --ascii-only     Do not put any headers in the data files.\n"
"  --no-output-files          Do not write any data files.\n"
"  -h        --help           Show help message.\n"
"  -i        --info           Detailed instrument information.\n"
);
  if(mcnumipar > 0)
  {
    fprintf(stderr, "Instrument parameters are:\n");
    for(i = 0; i < mcnumipar; i++)
      fprintf(stderr, "  %-16s(%s)\n", mcinputtable[i].name,
	(*mcinputtypes[mcinputtable[i].type].parminfo)(mcinputtable[i].name));
  }
}

static void
mcshowhelp(char *pgmname)
{
  mchelp(pgmname);
  exit(0);
}

static void
mcusage(char *pgmname)
{
  fprintf(stderr, "Error: incorrect command line arguments\n");
  mchelp(pgmname);
  exit(1);
}

static void
mcinfo(void)
{
  mcinfo_out("", stdout);
  exit(0);
}

static void
mcenabletrace(void)
{
 if(mctraceenabled)
  mcdotrace = 1;
 else
 {
   fprintf(stderr,
	   "Error: trace not enabled.\n"
	   "Please re-run the McStas compiler"
		   "with the --trace option, or rerun the\n"
	   "C compiler with the MC_TRACE_ENABLED macro defined.\n");
   exit(1);
 }
}

static void
mcuse_dir(char *dir)
{
#ifdef MC_PORTABLE
  fprintf(stderr, "Error: "
	  "Directory output cannot be used with portable simulation.\n");
  exit(1);
#else  /* !MC_PORTABLE */
  if(mkdir(dir, 0777))
  {
    fprintf(stderr, "Error: unable to create directory '%s'.\n", dir);
    fprintf(stderr, "(Maybe the directory already exists?)\n");
    exit(1);
  }
  mcdirname = dir;
#endif /* !MC_PORTABLE */
}

static void
mcuse_file(char *file)
{
  mcsiminfo_name = file;
  mcsingle_file = 1;
}

void
mcparseoptions(int argc, char *argv[])
{
  int i, j, pos;
  char *p;
  int paramset = 0, *paramsetarray;

  /* Add one to mcnumipar to aviod allocating zero size memory block. */
  paramsetarray = malloc((mcnumipar + 1)*sizeof(*paramsetarray));
  if(paramsetarray == NULL)
  {
    fprintf(stderr, "Error: insufficient memory\n");
    exit(1);
  }
  for(j = 0; j < mcnumipar; j++)
    paramsetarray[j] = 0;

  for(i = 1; i < argc; i++)
  {
    if(!strcmp("-s", argv[i]) && (i + 1) < argc)
      mcsetseed(argv[++i]);
    else if(!strncmp("-s", argv[i], 2))
      mcsetseed(&argv[i][2]);
    else if(!strcmp("--seed", argv[i]) && (i + 1) < argc)
      mcsetseed(argv[++i]);
    else if(!strncmp("--seed=", argv[i], 7))
      mcsetseed(&argv[i][7]);
    else if(!strcmp("-n", argv[i]) && (i + 1) < argc)
      mcsetn_arg(argv[++i]);
    else if(!strncmp("-n", argv[i], 2))
      mcsetn_arg(&argv[i][2]);
    else if(!strcmp("--ncount", argv[i]) && (i + 1) < argc)
      mcsetn_arg(argv[++i]);
    else if(!strncmp("--ncount=", argv[i], 9))
      mcsetn_arg(&argv[i][9]);
    else if(!strcmp("-d", argv[i]) && (i + 1) < argc)
      mcuse_dir(argv[++i]);
    else if(!strncmp("-d", argv[i], 2))
      mcuse_dir(&argv[i][2]);
    else if(!strcmp("--dir", argv[i]) && (i + 1) < argc)
      mcuse_dir(argv[++i]);
    else if(!strncmp("--dir=", argv[i], 6))
      mcuse_dir(&argv[i][6]);
    else if(!strcmp("-f", argv[i]) && (i + 1) < argc)
      mcuse_file(argv[++i]);
    else if(!strncmp("-f", argv[i], 2))
      mcuse_file(&argv[i][2]);
    else if(!strcmp("--file", argv[i]) && (i + 1) < argc)
      mcuse_file(argv[++i]);
    else if(!strncmp("--file=", argv[i], 7))
      mcuse_file(&argv[i][7]);
    else if(!strcmp("-h", argv[i]))
      mcshowhelp(argv[0]);
    else if(!strcmp("--help", argv[i]))
      mcshowhelp(argv[0]);
    else if(!strcmp("-i", argv[i]))
      mcinfo();
    else if(!strcmp("--info", argv[i]))
      mcinfo();
    else if(!strcmp("-t", argv[i]))
      mcenabletrace();
    else if(!strcmp("--trace", argv[i]))
      mcenabletrace();
    else if(!strcmp("-a", argv[i]))
      mcascii_only = 1;
    else if(!strcmp("--ascii-only", argv[i]))
      mcascii_only = 1;
    else if(!strcmp("--no-output-files", argv[i]))
      mcdisable_output_files = 1;
    else if(argv[i][0] != '-' && (p = strchr(argv[i], '=')) != NULL)
    {
      *p++ = '\0';
      for(j = 0; j < mcnumipar; j++)
	if(!strcmp(mcinputtable[j].name, argv[i]))
	{
	  int status;
	  status = (*mcinputtypes[mcinputtable[j].type].getparm)(p,
			mcinputtable[j].par);
	  if(!status)
	  {
	    (*mcinputtypes[mcinputtable[j].type].error)
	      (mcinputtable[j].name, p);
	    exit(1);
	  }
	  paramsetarray[j] = 1;
	  paramset = 1;
	  break;
	}
      if(j == mcnumipar)
      {				/* Unrecognized parameter name */
	fprintf(stderr, "Error: unrecognized parameter %s\n", argv[i]);
	exit(1);
      }
    }
    else
      mcusage(argv[0]);
  }
  if(!paramset)
    mcreadparams();		/* Prompt for parameters if not specified. */
  else
  {
    for(j = 0; j < mcnumipar; j++)
      if(!paramsetarray[j])
      {
	fprintf(stderr, "Error: Instrument parameter %s left unset\n",
		mcinputtable[j].name);
	exit(1);
      }
  }
  free(paramsetarray);
}

#ifndef MAC
#ifndef WIN32
/* This is the signal handler that makes simulation stop, and save results */
void sighandler(int sig)
{
 
        char *tmp;
        char *atfile;
        FILE *fnum;
        time_t t1;
 
        t1 = time(NULL);
 
        printf("\n# McStas: Signal %i detected in simulation %s (%s): ", sig,  mcinstrument_name, mcinstrument_source);
        switch (sig) {
        case SIGINT : printf(" SIGINT "); break;  /* Ctrl-C */
        case SIGQUIT : printf(" SIGQUIT "); break;
        case SIGABRT : printf(" SIGABRT "); break;
        case SIGTRAP : printf(" SIGTRAP "); break;
        case SIGTERM : printf(" SIGTERM "); break;
        case SIGPIPE : printf(" SIGPIPE "); break;
        case SIGPWR  : printf(" SIGPWR "); break;
        case SIGUSR1 : printf(" SIGUSR1 "); break;
        case SIGUSR2 : printf(" SIGUSR2 "); break;
        case SIGILL  : printf(" SIGILL "); break;
        case SIGFPE  : printf(" SIGFPE "); break;
        case SIGBUS  : printf(" SIGBUS "); break;
        case SIGSEGV : printf(" SIGSEGV "); break;
        case SIGURG  : printf(" SIGURG "); break;
        default : break;
        }
  printf("\n");
  printf("# Date %s",ctime(&t1));

  if (sig == SIGUSR1)
  {
    if (mcget_ncount())
      printf("# McStas: simulation now at %.2f %% (%10.1f/%10.1f)\n", 100*mcget_run_num()/mcget_ncount(), mcget_run_num(), mcget_ncount());
    else
      printf("# McStas: simulation now at Init (%10.1f/%10.1f)\n",  mcget_run_num(), mcget_ncount());
    fflush(stdout);
    return;
  }
  else
  if ((sig == SIGUSR2) || (sig == SIGQUIT) || (sig == SIGTERM) || (sig == SIGABRT) || (sig == SIGALRM) || (sig == SIGINT))
        {
    if (mcget_ncount())
       printf("# McStas: finishing simulation at %.2f %% (%10.1f/%10.1f)\n", 100*mcget_run_num()/mcget_ncount(), mcget_run_num(), mcget_ncount());
    else 
       printf("# McStas: finishing simulation at Init (%10.1f/%10.1f)\n", mcget_run_num(), mcget_ncount());  
    mcset_ncount(mcget_run_num());
    return;
        }
  else
        {
    if (mcget_ncount())
      printf("McStas: SYSTEM stop at %.2f %% (%10.1f/%10.1f)\n", 100*mcget_run_num()/mcget_ncount(), mcget_run_num(), mcget_ncount());
    else 
      printf("McStas: SYSTEM stop at Init (%10.1f/%10.1f)\n", mcget_run_num(), mcget_ncount());  
    exit(-1);
  }
 
}
#endif /* !MAC */
#endif /* !WIN32 */

/* McStas main() function. */
int
mcstas_main(int argc, char *argv[])
{
/*  double run_num = 0; */

#ifdef MAC
  argc = ccommand(&argv);
#endif

#ifndef MAC
#ifndef WIN32
  /* install sig handler, but only once !! */
        signal( SIGINT ,sighandler);    /* interrupt (rubout) */
        signal( SIGQUIT ,sighandler);   /* quit (ASCII FS) */
        signal( SIGABRT ,sighandler);   /* used by abort, replace SIGIOT in the future */
        signal( SIGTRAP ,sighandler);   /* trace trap (not reset when caught) */
        signal( SIGTERM ,sighandler);   /* software termination signal from kill */
        signal( SIGPIPE ,sighandler);   /* write on a pipe with no one to read it */
 
        signal( SIGPWR ,sighandler);
        signal( SIGUSR1 ,sighandler); /* display simulation status */
        signal( SIGUSR2 ,sighandler);
        signal( SIGILL ,sighandler);    /* illegal instruction (not reset when caught) */
        signal( SIGFPE ,sighandler);    /* floating point exception */
        signal( SIGBUS ,sighandler);    /* bus error */
        signal( SIGSEGV ,sighandler);   /* segmentation violation */
#ifdef SIGSYS
        signal( SIGSYS ,sighandler);    /* bad argument to system call */
#endif
        signal( SIGURG ,sighandler);    /* urgent socket condition */
#endif /* !MAC */
#endif /* !WIN32 */

  srandom(time(NULL));
  mcparseoptions(argc, argv);
  mcsiminfo_init();
  mcinit();
  while(mcrun_num < mcncount)
  {
    mcsetstate(0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1);
    mcraytrace();
    mcrun_num++;
  }
  mcfinally();
  mcsiminfo_close();
  return 0;
}


