/*******************************************************************************
* Runtime system for McStas.
*
* 	Project: Monte Carlo Simulation of Triple Axis Spectrometers
* 	File name: mcstas-r.c
*
* 	Author: K.N.			Aug 27, 1997
*
* Copyright (C) Risoe National Laboratory, 1997-2001, All rights reserved
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

/* ========================================================================= */
/* ADD: E.Farhi, Aug 6th, 2001: Monitor_nD section */
/* this routine is used to save results at end of simulation, but also 
 * during simulation (SIGUSR... intermediate steps) */ 
/* ========================================================================= */
  
void Monitor_nD_Init(aDEFS, aVars, m_xwidth, m_yheight, m_zthick, m_xmin, m_xmax, m_ymin, m_ymax, m_zmin, m_zmax)
  MonitornD_Defines_type *aDEFS;
  MonitornD_Variables_type *aVars;
  double m_xwidth, m_yheight, m_zthick;
  double m_xmin, m_xmax, m_ymin, m_ymax, m_zmin, m_zmax;
  {
    long carg = 1;
    char *option_copy, *token;
    char Flag_New_Token = 1;
    char Flag_End       = 1;
    char Flag_All       = 0;
    char Flag_No        = 0;
    char Flag_abs       = 0;
    char Set_aVars_Coord_Type;
    char Set_Coord_Flag = 0;
    char Set_aVars_Coord_Label[30];
    char Set_aVars_Coord_Var[30];
    char Short_Label[MONnD_COORD_NMAX][30];
    char Set_Coord_Mode;
    long i=0, j=0;
    double lmin, lmax, XY;
    long    t;

    
    t = (long)time(NULL);

    aDEFS->COORD_NONE   =0;
    aDEFS->COORD_X      =1;
    aDEFS->COORD_Y      =2;
    aDEFS->COORD_Z      =3;
    aDEFS->COORD_VX     =4;
    aDEFS->COORD_VY     =5;
    aDEFS->COORD_VZ     =6;
    aDEFS->COORD_T      =7;
    aDEFS->COORD_P      =8;
    aDEFS->COORD_SX     =9;
    aDEFS->COORD_SY     =10;
    aDEFS->COORD_SZ     =11;
    aDEFS->COORD_KX     =12;
    aDEFS->COORD_KY     =13;
    aDEFS->COORD_KZ     =14;
    aDEFS->COORD_K      =15;
    aDEFS->COORD_V      =16;
    aDEFS->COORD_ENERGY =17;
    aDEFS->COORD_LAMBDA =18;
    aDEFS->COORD_RADIUS =19;
    aDEFS->COORD_HDIV   =20;
    aDEFS->COORD_VDIV   =21;
    aDEFS->COORD_ANGLE  =22;
    aDEFS->COORD_NCOUNT =23;
    aDEFS->COORD_THETA  =24;
    aDEFS->COORD_PHI    =25;
    aDEFS->COORD_USER1  =26;
    aDEFS->COORD_USER2  =27;

/* token modifiers */
    aDEFS->COORD_VAR    =0;    /* next token should be a variable or normal option */
    aDEFS->COORD_MIN    =1;    /* next token is a min value */
    aDEFS->COORD_MAX    =2;    /* next token is a max value */
    aDEFS->COORD_DIM    =3;    /* next token is a bin value */
    aDEFS->COORD_FIL    =4;    /* next token is a filename */
    aDEFS->COORD_EVNT   =5;    /* next token is a buffer size value */
    aDEFS->COORD_3HE    =6;    /* next token is a 3He pressure value */
    aDEFS->COORD_INTERM =7;    /* next token is an intermediate save value (%) */
    aDEFS->COORD_LOG    =32;   /* next variable will be in log scale */
    aDEFS->COORD_ABS    =64;   /* next variable will be in abs scale */

    strcpy(aDEFS->TOKEN_DEL, " =,;[](){}:");  /* token separators */

    aDEFS->SHAPE_SQUARE =0;    /* shape of the monitor */
    aDEFS->SHAPE_DISK   =1;
    aDEFS->SHAPE_SPHERE =2;
    aDEFS->SHAPE_CYLIND =3;
    aDEFS->SHAPE_BOX    =4;

    aVars->Sphere_Radius     = 0;
    aVars->Cylinder_Height   = 0;
    aVars->Flag_With_Borders = 0;   /* 2 means xy borders too */
    aVars->Flag_List         = 0;   /* 1 store 1 buffer, 2 is list all */
    aVars->Flag_Multiple     = 0;   /* 1 when n1D, 0 for 2D */
    aVars->Flag_Verbose      = 0;
    aVars->Flag_Shape        = aDEFS->SHAPE_SQUARE;
    aVars->Flag_Auto_Limits  = 0;   /* get limits from first Buffer */
    aVars->Flag_Absorb       = 0;   /* monitor is also a slit */
    aVars->Flag_per_cm2      = 0;   /* flux is per cm2 */
    aVars->Flag_log          = 0;   /* log10 of the flux */
    aVars->Flag_parallel     = 0;   /* set neutron state back after detection (parallel components) */
    aVars->Coord_Number      = 0;   /* total number of variables to monitor, plus intensity (0) */
    aVars->Buffer_Block      = 1000;     /* Buffer size for list or auto limits */
    aVars->Neutron_Counter   = 0;   /* event counter, simulation total counts is mcget_ncount() */
    aVars->Buffer_Counter    = 0;   /* index in Buffer size (for realloc) */
    aVars->Buffer_Size       = 0;
    aVars->UserVariable1     = 0;
    aVars->UserVariable2     = 0;
    aVars->He3_pressure      = 0;
    aVars->IntermediateCnts  = 0;
    
    Set_aVars_Coord_Type = aDEFS->COORD_NONE;
    Set_Coord_Mode = aDEFS->COORD_VAR;
    
    /* handle size parameters */
    /* normal use is with xwidth, yheight, zthick */
    /* if xmin,xmax,ymin,ymax,zmin,zmax are non 0, use them */
    if (fabs(m_xmin-m_xmax) == 0) 
      { aVars->mxmin = -fabs(m_xwidth)/2; aVars->mxmax = fabs(m_xwidth)/2; }
    else
      { if (m_xmin < m_xmax) {aVars->mxmin = m_xmin; aVars->mxmax = m_xmax;}
        else {aVars->mxmin = m_xmax; aVars->mxmax = m_xmin;}
      }
    if (fabs(m_ymin-m_ymax) == 0) 
      { aVars->mymin = -fabs(m_yheight)/2; aVars->mymax = fabs(m_yheight)/2; }
    else
      { if (m_ymin < m_ymax) {aVars->mymin = m_ymin; aVars->mymax = m_ymax;}
        else {aVars->mymin = m_ymax; aVars->mymax = m_ymin;}
      }
    if (fabs(m_zmin-m_zmax) == 0) 
      { aVars->mzmin = -fabs(m_zthick)/2; aVars->mzmax = fabs(m_zthick)/2; }
    else
      { if (m_zmin < m_zmax) {aVars->mzmin = m_zmin; aVars->mzmax = m_zmax; }
        else {aVars->mzmin = m_zmax; aVars->mzmax = m_zmin; }
      }
      
    if (fabs(aVars->mzmax-aVars->mzmin) == 0)
      aVars->Flag_Shape        = aDEFS->SHAPE_SQUARE;
    else
      aVars->Flag_Shape        = aDEFS->SHAPE_BOX;
      
    /* parse option string */ 
    
    option_copy = (char*)malloc(strlen(aVars->option));
    if (option_copy == NULL)
    {
      printf("Monitor_nD: %s cannot allocate option_copy (%i). Fatal.\n", aVars->compcurname, strlen(aVars->option));
      exit(-1);
    }
    
    
    if (strlen(aVars->option))
    {
      Flag_End = 0;
      strcpy(option_copy, aVars->option);
    }
    
    if (strstr(aVars->option, "cm2") || strstr(aVars->option, "cm^2")) aVars->Flag_per_cm2 = 1;
  
    if (aVars->Flag_per_cm2) strcpy(aVars->Coord_Label[0],"Intensity [n/cm^2/s]");
    else strcpy(aVars->Coord_Label[0],"Intensity [n/s]");
    strcpy(aVars->Coord_Var[0],"p");
    aVars->Coord_Type[0] = aDEFS->COORD_P;
    aVars->Coord_Bin[0] = 1;
    aVars->Coord_Min[0] = 0;
    aVars->Coord_Max[0] = FLT_MAX;
    
    /* default file name is comp name+dateID */
    sprintf(aVars->Mon_File, "%s_%i", aVars->compcurname, t); 
  
    carg = 1;
    while((Flag_End == 0) && (carg < 128))
    {
      if (Flag_New_Token) /* to get the previous token sometimes */
      {
        if (carg == 1) token=(char *)strtok(option_copy,aDEFS->TOKEN_DEL);
        else token=(char *)strtok(NULL,aDEFS->TOKEN_DEL);
        if (token == NULL) Flag_End=1;
      }
      Flag_New_Token = 1;
      if ((token != NULL) && (strlen(token) != 0))
      {
      /* first handle option values from preceeding keyword token detected */
        if (Set_Coord_Mode == aDEFS->COORD_MAX)
        { 
          if (!Flag_All) 
            aVars->Coord_Max[aVars->Coord_Number] = atof(token); 
          else
            for (i = 0; i <= aVars->Coord_Number; aVars->Coord_Max[i++] = atof(token));
          Set_Coord_Mode = aDEFS->COORD_VAR; Flag_All = 0;
        }
        if (Set_Coord_Mode == aDEFS->COORD_MIN)
        { 
          if (!Flag_All) 
            aVars->Coord_Min[aVars->Coord_Number] = atof(token); 
          else
            for (i = 0; i <= aVars->Coord_Number; aVars->Coord_Min[i++] = atof(token));
          Set_Coord_Mode = aDEFS->COORD_MAX; 
        }
        if (Set_Coord_Mode == aDEFS->COORD_DIM)
        { 
          if (!Flag_All) 
            aVars->Coord_Bin[aVars->Coord_Number] = atoi(token); 
          else
            for (i = 0; i <= aVars->Coord_Number; aVars->Coord_Bin[i++] = atoi(token));
          Set_Coord_Mode = aDEFS->COORD_VAR; Flag_All = 0;
        }
        if (Set_Coord_Mode == aDEFS->COORD_FIL)
        { 
          if (!Flag_No) strcpy(aVars->Mon_File,token); 
          else { strcpy(aVars->Mon_File,""); aVars->Coord_Number = 0; Flag_End = 1;}
          Set_Coord_Mode = aDEFS->COORD_VAR;
        }
        if (Set_Coord_Mode == aDEFS->COORD_EVNT)
        { 
          if (!strcmp(token, "all") || Flag_All) aVars->Flag_List = 2;
          else { i = atoi(token); if (i) aVars->Buffer_Counter = i; }
          Set_Coord_Mode = aDEFS->COORD_VAR; Flag_All = 0;
        }
        if (Set_Coord_Mode == aDEFS->COORD_3HE)
        { 
            aVars->He3_pressure = atof(token); 
            Set_Coord_Mode = aDEFS->COORD_VAR; Flag_All = 0; 
        }
        if (Set_Coord_Mode == aDEFS->COORD_INTERM)
        { 
            aVars->Intermediate = atof(token); 
            Set_Coord_Mode = aDEFS->COORD_VAR; Flag_All = 0; 
        }

        /* now look for general option keywords */
        if (!strcmp(token, "borders")) 
        { if (Flag_No) { aVars->Flag_With_Borders = 0; Flag_No = 0; }
          else aVars->Flag_With_Borders = 1; }
        if (!strcmp(token, "verbose")) 
        { if (Flag_No) { aVars->Flag_Verbose = 0; Flag_No = 0; }
          else aVars->Flag_Verbose      = 1; }
        if (!strcmp(token, "log")) 
        { if (Flag_No) { aVars->Flag_log = 0; Flag_No = 0; }
          else aVars->Flag_log      = 1; }
        if (!strcmp(token, "abs")) 
        { Flag_abs      = 1; }
        if (!strcmp(token, "multiple")) 
        { if (Flag_No) { aVars->Flag_Multiple = 0; Flag_No = 0; }
          else aVars->Flag_Multiple = 1; }
        if (!strcmp(token, "list")) 
        { if (Flag_No) { aVars->Flag_List = 0; Flag_No = 0; }
          else aVars->Flag_List = 1; 
          Set_Coord_Mode = aDEFS->COORD_EVNT; }

        if (!strcmp(token, "limits") || !strcmp(token, "min")) Set_Coord_Mode = aDEFS->COORD_MIN;
        if (!strcmp(token, "slit") || !strcmp(token, "absorb")) 
        { if (Flag_No) { aVars->Flag_Absorb = 0; Flag_No = 0; }
          else aVars->Flag_Absorb = 1; }
        if (!strcmp(token, "max")) Set_Coord_Mode = aDEFS->COORD_MAX;
        if (!strcmp(token, "bins")) Set_Coord_Mode = aDEFS->COORD_DIM;
        if (!strcmp(token, "file")) 
        { Set_Coord_Mode = aDEFS->COORD_FIL;
          if (Flag_No) { strcpy(aVars->Mon_File,""); aVars->Coord_Number = 0; Flag_End = 1;}}
        if (!strcmp(token, "unactivate")) { Flag_End = 1; aVars->Coord_Number = 0; }
        if (!strcmp(token, "all")) 
        { if (Flag_No) { Flag_All = 0; Flag_No = 0; }
          else Flag_All = 1; }
        if (!strcmp(token, "sphere")) 
        { if (Flag_No) { aVars->Flag_Shape = aDEFS->SHAPE_SQUARE; Flag_No = 0; }
          else aVars->Flag_Shape = aDEFS->SHAPE_SPHERE; }
        if (!strcmp(token, "cylinder")) 
        { if (Flag_No) { aVars->Flag_Shape = aDEFS->SHAPE_SQUARE; Flag_No = 0; }
          else aVars->Flag_Shape = aDEFS->SHAPE_CYLIND; }
        if (!strcmp(token, "square")) aVars->Flag_Shape = aDEFS->SHAPE_SQUARE; 
        if (!strcmp(token, "disk")) aVars->Flag_Shape = aDEFS->SHAPE_DISK; 
        if (!strcmp(token, "box")) aVars->Flag_Shape = aDEFS->SHAPE_BOX; 
        if (!strcmp(token, "parallel")) aVars->Flag_parallel = 1; 
        if (!strcmp(token, "auto")) 
        { if (Flag_No) { aVars->Flag_Auto_Limits = 0; Flag_No = 0; }
          else aVars->Flag_Auto_Limits = 1; }
        if (!strcmp(token, "premonitor"))
        { if (Flag_No) { aVars->Flag_UsePreMonitor = 0; Flag_No = 0; }
        else aVars->Flag_UsePreMonitor == 1; } 
        if (!strcmp(token, "3He_pressure"))
        { if (!Flag_No)  Set_Coord_Mode = aDEFS->COORD_3HE; 
          aVars->He3_pressure = 3; }
        if (!strcmp(token, "intermediate"))
        { if (!Flag_No)  Set_Coord_Mode = aDEFS->COORD_INTERM; 
          aVars->Intermediate = 5; }
        if (!strcmp(token, "no") || !strcmp(token, "not")) Flag_No = 1;
  
        /* now look for variable names to monitor */
        Set_aVars_Coord_Type = aDEFS->COORD_NONE; lmin = 0; lmax = 0;

        if (!strcmp(token, "x")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_X; strcpy(Set_aVars_Coord_Label,"x [m]"); strcpy(Set_aVars_Coord_Var,"x"); lmin = aVars->mxmin; lmax = aVars->mxmax; }
        if (!strcmp(token, "y")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_Y; strcpy(Set_aVars_Coord_Label,"y [m]"); strcpy(Set_aVars_Coord_Var,"y"); lmin = aVars->mymin; lmax = aVars->mymax; }
        if (!strcmp(token, "z")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_Z; strcpy(Set_aVars_Coord_Label,"z [m]"); strcpy(Set_aVars_Coord_Var,"z"); lmin = aVars->mzmin; lmax = aVars->mzmax; }
        if (!strcmp(token, "k") || !strcmp(token, "wavevector")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_K; strcpy(Set_aVars_Coord_Label,"|k| [Angs-1]"); strcpy(Set_aVars_Coord_Var,"k"); lmin = 0; lmax = 10; }
        if (!strcmp(token, "v"))
          { Set_aVars_Coord_Type = aDEFS->COORD_V; strcpy(Set_aVars_Coord_Label,"Velocity [m/s]"); strcpy(Set_aVars_Coord_Var,"v"); lmin = 0; lmax = 10000; }
        if (!strcmp(token, "t") || !strcmp(token, "time")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_T; strcpy(Set_aVars_Coord_Label,"TOF [s]"); strcpy(Set_aVars_Coord_Var,"t"); lmin = 0; lmax = .1; }
        if ((!strcmp(token, "p") || !strcmp(token, "intensity") || !strcmp(token, "flux")))
          { Set_aVars_Coord_Type = aDEFS->COORD_P;  
            if (aVars->Flag_per_cm2) strcpy(Set_aVars_Coord_Label,"Intensity [n/cm^2/s]");
            else strcpy(Set_aVars_Coord_Label,"Intensity [n/s]"); 
            strcpy(Set_aVars_Coord_Var,"I"); 
            lmin = 0; lmax = FLT_MAX; }

        if (!strcmp(token, "vx")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_VX; strcpy(Set_aVars_Coord_Label,"vx [m/s]"); strcpy(Set_aVars_Coord_Var,"vx"); lmin = -1000; lmax = 1000; }
        if (!strcmp(token, "vy")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_VY; strcpy(Set_aVars_Coord_Label,"vy [m/s]"); strcpy(Set_aVars_Coord_Var,"vy"); lmin = -1000; lmax = 1000; }
        if (!strcmp(token, "vz")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_VZ; strcpy(Set_aVars_Coord_Label,"vz [m/s]"); strcpy(Set_aVars_Coord_Var,"vz"); lmin = -10000; lmax = 10000; }
        if (!strcmp(token, "kx")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_KX; strcpy(Set_aVars_Coord_Label,"kx [Angs-1]"); strcpy(Set_aVars_Coord_Var,"kx"); lmin = -1; lmax = 1; }
        if (!strcmp(token, "ky")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_KY; strcpy(Set_aVars_Coord_Label,"ky [Angs-1]"); strcpy(Set_aVars_Coord_Var,"ky"); lmin = -1; lmax = 1; }
        if (!strcmp(token, "kz")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_KZ; strcpy(Set_aVars_Coord_Label,"kz [Angs-1]"); strcpy(Set_aVars_Coord_Var,"kz"); lmin = -10; lmax = 10; }
        if (!strcmp(token, "sx")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_SX; strcpy(Set_aVars_Coord_Label,"sx [1]"); strcpy(Set_aVars_Coord_Var,"sx"); lmin = -1; lmax = 1; }
        if (!strcmp(token, "sy")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_SY; strcpy(Set_aVars_Coord_Label,"sy [1]"); strcpy(Set_aVars_Coord_Var,"sy"); lmin = -1; lmax = 1; }
        if (!strcmp(token, "sz")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_SZ; strcpy(Set_aVars_Coord_Label,"sz [1]"); strcpy(Set_aVars_Coord_Var,"sz"); lmin = -1; lmax = 1; }

        if (!strcmp(token, "energy") || !strcmp(token, "omega")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_ENERGY; strcpy(Set_aVars_Coord_Label,"Energy [meV]"); strcpy(Set_aVars_Coord_Var,"E"); lmin = 0; lmax = 100; }
        if (!strcmp(token, "lambda") || !strcmp(token, "wavelength")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_LAMBDA; strcpy(Set_aVars_Coord_Label,"Wavelength [Angs]"); strcpy(Set_aVars_Coord_Var,"L"); lmin = 0; lmax = 100; }
        if (!strcmp(token, "radius")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_RADIUS; strcpy(Set_aVars_Coord_Label,"Radius [m]"); strcpy(Set_aVars_Coord_Var,"R"); lmin = 0; lmax = m_xmax; }
        if (!strcmp(token, "angle")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_ANGLE; strcpy(Set_aVars_Coord_Label,"Angle [deg]"); strcpy(Set_aVars_Coord_Var,"A"); lmin = -5; lmax = 5; }
        if (!strcmp(token, "hdiv")|| !strcmp(token, "divergence") || !strcmp(token, "xdiv") || !strcmp(token, "dx")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_HDIV; strcpy(Set_aVars_Coord_Label,"Hor. Divergence [deg]"); strcpy(Set_aVars_Coord_Var,"HD"); lmin = -5; lmax = 5; }
        if (!strcmp(token, "vdiv") || !strcmp(token, "ydiv") || !strcmp(token, "dy")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_VDIV; strcpy(Set_aVars_Coord_Label,"Vert. Divergence [deg]"); strcpy(Set_aVars_Coord_Var,"VD"); lmin = -5; lmax = 5; }
        if (!strcmp(token, "theta") || !strcmp(token, "longitude")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_THETA; strcpy(Set_aVars_Coord_Label,"Longitude [deg]"); strcpy(Set_aVars_Coord_Var,"th"); lmin = -180; lmax = 180; }
        if (!strcmp(token, "phi") || !strcmp(token, "lattitude")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_PHI; strcpy(Set_aVars_Coord_Label,"Lattitude [deg]"); strcpy(Set_aVars_Coord_Var,"ph"); lmin = -180; lmax = 180; }
        if (!strcmp(token, "ncounts")) 
          { Set_aVars_Coord_Type = aDEFS->COORD_NCOUNT; strcpy(Set_aVars_Coord_Label,"Neutrons [1]"); strcpy(Set_aVars_Coord_Var,"N"); lmin = 0; lmax = 1e10; }
        if (!strcmp(token, "user") || !strcmp(token, "user1"))
          { Set_aVars_Coord_Type = aDEFS->COORD_USER1; strncpy(Set_aVars_Coord_Label,aVars->UserName1,32); strcpy(Set_aVars_Coord_Var,"U1"); lmin = -1e10; lmax = 1e10; }
        if (!strcmp(token, "user2"))
          { Set_aVars_Coord_Type = aDEFS->COORD_USER2; strncpy(Set_aVars_Coord_Label,aVars->UserName2,32); strcpy(Set_aVars_Coord_Var,"U2"); lmin = -1e10; lmax = 1e10; }
        if ((Set_aVars_Coord_Type != aDEFS->COORD_NONE) && aVars->Flag_log) { Set_aVars_Coord_Type |= aDEFS->COORD_LOG; aVars->Flag_log = 0; }
        if ((Set_aVars_Coord_Type != aDEFS->COORD_NONE) && Flag_abs) { Set_aVars_Coord_Type |= aDEFS->COORD_ABS; Flag_abs = 0; }

        /* now stores variable keywords detected, if any */ 
        if (Set_aVars_Coord_Type != aDEFS->COORD_NONE)
        {
          if (aVars->Coord_Number < MONnD_COORD_NMAX) aVars->Coord_Number++;
          else if (aVars->Flag_Verbose) printf("Monitor_nD: %s reached max number of variables (%i).\n", aVars->compcurname, MONnD_COORD_NMAX);
          aVars->Coord_Type[aVars->Coord_Number] = Set_aVars_Coord_Type;
          strcpy(aVars->Coord_Label[aVars->Coord_Number], Set_aVars_Coord_Label); 
          strcpy(aVars->Coord_Var[aVars->Coord_Number], Set_aVars_Coord_Var);
          if (lmin > lmax) { XY = lmin; lmin=lmax; lmax = XY; }
          aVars->Coord_Min[aVars->Coord_Number] = lmin;
          aVars->Coord_Max[aVars->Coord_Number] = lmax;
          aVars->Coord_Bin[aVars->Coord_Number] = 20;
          Set_Coord_Mode = aDEFS->COORD_VAR;
          Flag_All = 0;
          Flag_No = 0;
        }
      carg++;
      } /* end if token */
    } /* end while carg */
    free(option_copy);
    if (carg == 128) printf("Monitor_nD: %s reached max number of tokens (%i). Skipping.\n", aVars->compcurname, 128);
    
    if ((aVars->Flag_Shape == aDEFS->SHAPE_BOX) && (fabs(aVars->mzmax - aVars->mzmin) == 0)) aVars->Flag_Shape = aDEFS->SHAPE_SQUARE;
    
    if (aVars->Flag_log == 1)
    {
        aVars->Coord_Type[0] |= aDEFS->COORD_LOG;
    }

    /* now setting Monitor Name from variable labels */
    strcpy(aVars->Monitor_Label,"");
    for (i = 0; i <= aVars->Coord_Number; i++)
    {
      Set_aVars_Coord_Type = (aVars->Coord_Type[i] & 31);
      if ((Set_aVars_Coord_Type == aDEFS->COORD_THETA)
       || (Set_aVars_Coord_Type == aDEFS->COORD_PHI)
       || (Set_aVars_Coord_Type == aDEFS->COORD_X)
       || (Set_aVars_Coord_Type == aDEFS->COORD_Y)
       || (Set_aVars_Coord_Type == aDEFS->COORD_Z)
       || (Set_aVars_Coord_Type == aDEFS->COORD_RADIUS))
       strcpy(Short_Label[i],"Position"); 
      else
      if ((Set_aVars_Coord_Type == aDEFS->COORD_VX)
       || (Set_aVars_Coord_Type == aDEFS->COORD_VY)
       || (Set_aVars_Coord_Type == aDEFS->COORD_VZ)
       || (Set_aVars_Coord_Type == aDEFS->COORD_V))
       strcpy(Short_Label[i],"Velocity"); 
      else
      if ((Set_aVars_Coord_Type == aDEFS->COORD_KX)
       || (Set_aVars_Coord_Type == aDEFS->COORD_KY)
       || (Set_aVars_Coord_Type == aDEFS->COORD_KZ)
       || (Set_aVars_Coord_Type == aDEFS->COORD_K))
       strcpy(Short_Label[i],"Wavevector"); 
      else
      if ((Set_aVars_Coord_Type == aDEFS->COORD_SX)
       || (Set_aVars_Coord_Type == aDEFS->COORD_SY)
       || (Set_aVars_Coord_Type == aDEFS->COORD_SZ))
       strcpy(Short_Label[i],"Spin");
      else
      if ((Set_aVars_Coord_Type == aDEFS->COORD_HDIV)
       || (Set_aVars_Coord_Type == aDEFS->COORD_VDIV)
       || (Set_aVars_Coord_Type == aDEFS->COORD_ANGLE))
       strcpy(Short_Label[i],"Divergence");
      else
      if (Set_aVars_Coord_Type == aDEFS->COORD_ENERGY)
       strcpy(Short_Label[i],"Energy"); 
      else
      if (Set_aVars_Coord_Type == aDEFS->COORD_LAMBDA)
       strcpy(Short_Label[i],"Wavelength"); 
      else
      if (Set_aVars_Coord_Type == aDEFS->COORD_NCOUNT)
       strcpy(Short_Label[i],"Neutron counts");
      else
      if (Set_aVars_Coord_Type == aDEFS->COORD_T)
          strcpy(Short_Label[i],"Time Of Flight");
      else
      if (Set_aVars_Coord_Type == aDEFS->COORD_P)
          strcpy(Short_Label[i],"Intensity");
      else
      if (Set_aVars_Coord_Type == aDEFS->COORD_USER1)
          strncpy(Short_Label[i],aVars->UserName1,32);
      else
      if (Set_aVars_Coord_Type == aDEFS->COORD_USER2)
          strncpy(Short_Label[i],aVars->UserName2,32);
      else
          strcpy(Short_Label[i],"Unknown"); 
          
      if (aVars->Coord_Type[i] & aDEFS->COORD_ABS)
          strcat(aVars->Coord_Label[i]," (abs)");
      if (aVars->Coord_Type[i] & aDEFS->COORD_LOG)
          strcat(aVars->Coord_Label[i]," (log)");
          
      strcat(aVars->Monitor_Label, " ");
      strcat(aVars->Monitor_Label, Short_Label[i]);
    } /* end for Short_Label */
    strcat(aVars->Monitor_Label, " Monitor");
    if (aVars->Flag_Shape == aDEFS->SHAPE_SQUARE) strcat(aVars->Monitor_Label, " (Square)");
    if (aVars->Flag_Shape == aDEFS->SHAPE_DISK)   strcat(aVars->Monitor_Label, " (Disk)");
    if (aVars->Flag_Shape == aDEFS->SHAPE_SPHERE) strcat(aVars->Monitor_Label, " (Sphere)");
    if (aVars->Flag_Shape == aDEFS->SHAPE_CYLIND) strcat(aVars->Monitor_Label, " (Cylinder)");
    if (aVars->Flag_Shape == aDEFS->SHAPE_BOX)    strcat(aVars->Monitor_Label, " (Box)");
    if (((aVars->Flag_Shape == aDEFS->SHAPE_CYLIND) || (aVars->Flag_Shape == aDEFS->SHAPE_SPHERE) || (aVars->Flag_Shape == aDEFS->SHAPE_BOX))
        && strstr(aVars->option, "outgoing"))
    {
      aVars->Flag_Shape *= -1;
      strcat(aVars->Monitor_Label, " [out]");
    }
    if (aVars->Flag_UsePreMonitor == 1)
    {
        strcat(aVars->Monitor_Label, " at ");
        strncat(aVars->Monitor_Label, aVars->UserName1,32);
    }
    if (aVars->Flag_log == 1)
    {
        strcat(aVars->Monitor_Label, " [log] ");
    }
    
    /* aVars->Coord_Number  0   : intensity
     * aVars->Coord_Number  1:n : detector variables */
    
    /* now allocate memory to store variables in TRACE */
    if ((aVars->Coord_Number != 2) && !aVars->Flag_Multiple && !aVars->Flag_List) 
    { aVars->Flag_Multiple = 1; aVars->Flag_List = 0; } /* default is n1D */
    
   /* list and auto limits case : aVars->Flag_List or aVars->Flag_Auto_Limits 
    * -> Buffer to flush and suppress after aVars->Flag_Auto_Limits
    */
    if ((aVars->Flag_Auto_Limits || aVars->Flag_List) && aVars->Coord_Number)
    { /* Dim : (aVars->Coord_Number+2)*aVars->Buffer_Block matrix (for p, dp) */ 
      aVars->Mon2D_Buffer = (double *)malloc((aVars->Coord_Number+2)*aVars->Buffer_Block*sizeof(double));
      if (aVars->Mon2D_Buffer == NULL)
      { printf("Monitor_nD: %s cannot allocate aVars->Mon2D_Buffer (%li). No list and auto limits.\n", aVars->compcurname, aVars->Buffer_Block*(aVars->Coord_Number+2)*sizeof(double)); aVars->Flag_List = 0; aVars->Flag_Auto_Limits = 0; }
      else
      {
        for ( i=0; i < (aVars->Coord_Number+2)*aVars->Buffer_Block; aVars->Mon2D_Buffer[i++] = (double)0);
      }
      aVars->Buffer_Size = aVars->Buffer_Block;
    }
    
    /* 1D and n1D case : aVars->Flag_Multiple */
    if (aVars->Flag_Multiple && aVars->Coord_Number)
    { /* Dim : aVars->Coord_Number*aVars->Coord_Bin[i] vectors */ 
      aVars->Mon2D_N  = (int    **)malloc((aVars->Coord_Number)*sizeof(int *));
      aVars->Mon2D_p  = (double **)malloc((aVars->Coord_Number)*sizeof(double *));
      aVars->Mon2D_p2 = (double **)malloc((aVars->Coord_Number)*sizeof(double *));
      if ((aVars->Mon2D_N == NULL) || (aVars->Mon2D_p == NULL) || (aVars->Mon2D_p2 == NULL))
      { printf("Monitor_nD: %s n1D cannot allocate aVars->Mon2D_N/p/2p (%i). Fatal.\n", aVars->compcurname, (aVars->Coord_Number)*sizeof(double *)); exit(-1); }
      for (i= 1; i <= aVars->Coord_Number; i++)
      { 
        aVars->Mon2D_N[i-1]  = (int    *)malloc(aVars->Coord_Bin[i]*sizeof(int));
        aVars->Mon2D_p[i-1]  = (double *)malloc(aVars->Coord_Bin[i]*sizeof(double));
        aVars->Mon2D_p2[i-1] = (double *)malloc(aVars->Coord_Bin[i]*sizeof(double));
        if ((aVars->Mon2D_N == NULL) || (aVars->Mon2D_p == NULL) || (aVars->Mon2D_p2 == NULL))
        { printf("Monitor_nD: %s n1D cannot allocate %s aVars->Mon2D_N/p/2p[%li] (%i). Fatal.\n", aVars->compcurname, aVars->Coord_Var[i], i, (aVars->Coord_Bin[i])*sizeof(double *)); exit(-1); }
        else 
        {
          for (j=0; j < aVars->Coord_Bin[i]; j++ )
          { aVars->Mon2D_N[i-1][j] = (int)0; aVars->Mon2D_p[i-1][j] = (double)0; aVars->Mon2D_p2[i-1][j] = (double)0; }
        }
      }
    }
    else /* 2D case : aVars->Coord_Number==2 and !aVars->Flag_Multiple and !aVars->Flag_List */
    if ((aVars->Coord_Number == 2) && !aVars->Flag_Multiple)
    { /* Dim : aVars->Coord_Bin[1]*aVars->Coord_Bin[2] matrix */ 
      aVars->Mon2D_N  = (int    **)malloc((aVars->Coord_Bin[1])*sizeof(int *));
      aVars->Mon2D_p  = (double **)malloc((aVars->Coord_Bin[1])*sizeof(double *));
      aVars->Mon2D_p2 = (double **)malloc((aVars->Coord_Bin[1])*sizeof(double *));
      if ((aVars->Mon2D_N == NULL) || (aVars->Mon2D_p == NULL) || (aVars->Mon2D_p2 == NULL))
      { printf("Monitor_nD: %s 2D cannot allocate %s aVars->Mon2D_N/p/2p (%i). Fatal.\n", aVars->compcurname, aVars->Coord_Var[1], (aVars->Coord_Bin[1])*sizeof(double *)); exit(-1); }
      for (i= 0; i < aVars->Coord_Bin[1]; i++)
      { 
        aVars->Mon2D_N[i]  = (int    *)malloc(aVars->Coord_Bin[2]*sizeof(int));
        aVars->Mon2D_p[i]  = (double *)malloc(aVars->Coord_Bin[2]*sizeof(double));
        aVars->Mon2D_p2[i] = (double *)malloc(aVars->Coord_Bin[2]*sizeof(double));
        if ((aVars->Mon2D_N == NULL) || (aVars->Mon2D_p == NULL) || (aVars->Mon2D_p2 == NULL))
        { printf("Monitor_nD: %s 2D cannot allocate %s aVars->Mon2D_N/p/2p[%li] (%i). Fatal.\n", aVars->compcurname, aVars->Coord_Var[1], i, (aVars->Coord_Bin[2])*sizeof(double *)); exit(-1); }
        else
        {
          for (j=0; j < aVars->Coord_Bin[2]; j++ )
          { aVars->Mon2D_N[i][j] = (int)0; aVars->Mon2D_p[i][j] = (double)0; aVars->Mon2D_p2[i][j] = (double)0; }
        }
      }
    }      
      /* no Mon2D allocated for 
       * (aVars->Coord_Number != 2) && !aVars->Flag_Multiple && aVars->Flag_List */
    
    aVars->psum  = 0;
    aVars->p2sum = 0;
    aVars->Nsum  = 0;
    
    aVars->area  = fabs(aVars->mxmax - aVars->mxmin)*fabs(aVars->mymax - aVars->mymin)*1E4; /* in cm**2 for square and box shapes */
    aVars->Sphere_Radius = fabs(aVars->mxmax - aVars->mxmin);
    if ((abs(aVars->Flag_Shape) == aDEFS->SHAPE_DISK) || (abs(aVars->Flag_Shape) == aDEFS->SHAPE_SPHERE))
    {
      aVars->area = PI*aVars->Sphere_Radius*aVars->Sphere_Radius; /* disk shapes */
    }
    if (aVars->area == 0) aVars->Coord_Number = 0;
    if (aVars->Coord_Number == 0 && aVars->Flag_Verbose)  printf("Monitor_nD: %s is unactivated (0D)\n", aVars->compcurname);
    aVars->Cylinder_Height = fabs(aVars->mymax - aVars->mymin);
    
    if (aVars->Intermediate < 0) aVars->Intermediate = 0;
    if (aVars->Intermediate > 1) aVars->Intermediate /= 100;
    aVars->IntermediateCnts = aVars->Intermediate*mcget_ncount();
  } /* end Monitor_nD_Init */
  
void Monitor_nD_Trace(aDEFS, aVars)
  MonitornD_Defines_type *aDEFS;
  MonitornD_Variables_type *aVars;
{

  double  XY=0;
  double  t0 = 0;
  double  t1 = 0;
  long    i,j;
  double  pp;
  double  Coord[MONnD_COORD_NMAX];
  long    Coord_Index[MONnD_COORD_NMAX];
  char    While_End   =0;
  long    While_Buffer=0;
  int     intersect = 0;
  char    Set_aVars_Coord_Type = aDEFS->COORD_NONE;
  
  
  pp = aVars->cp;
  
  if (aVars->Coord_Number > 0)
    {
      /* aVars->Flag_Auto_Limits */
      if ((aVars->Buffer_Counter >= aVars->Buffer_Block) && (aVars->Flag_Auto_Limits == 1))
      {
        /* auto limits case : get limits in Buffer for each variable */ 
              /* Dim : (aVars->Coord_Number+2)*aVars->Buffer_Block matrix (for p, dp) */ 
        if (aVars->Flag_Verbose) printf("Monitor_nD: %s getting %i Auto Limits from List (%li).\n", aVars->compcurname, aVars->Coord_Number, aVars->Buffer_Counter);
        for (i = 1; i <= aVars->Coord_Number; i++)
        {
          aVars->Coord_Min[i] = FLT_MAX;
          aVars->Coord_Max[i] = -FLT_MAX;
          for (j = 0; j < aVars->Buffer_Block; j++)
          { 
            XY = aVars->Mon2D_Buffer[j*(aVars->Coord_Number+2) + (i-1)];  /* scanning variables in Buffer */
            if (XY < aVars->Coord_Min[i]) aVars->Coord_Min[i] = XY;
            if (XY > aVars->Coord_Max[i]) aVars->Coord_Max[i] = XY;
          }
        }
        aVars->Flag_Auto_Limits = 2;  /* pass to 2nd auto limits step */
      }

      /* manage realloc for list all if Buffer size exceeded */
      if ((aVars->Buffer_Counter >= aVars->Buffer_Block) && (aVars->Flag_List == 2))
      {
        aVars->Mon2D_Buffer  = (double *)realloc(aVars->Mon2D_Buffer, (aVars->Coord_Number+2)*(aVars->Neutron_Counter+aVars->Buffer_Block)*sizeof(double));
        if (aVars->Mon2D_Buffer == NULL)
              { printf("Monitor_nD: %s cannot reallocate aVars->Mon2D_Buffer[%li] (%li). Skipping.\n", aVars->compcurname, i, (aVars->Neutron_Counter+aVars->Buffer_Block)*sizeof(double)); aVars->Flag_List = 1; }
        else { aVars->Buffer_Counter = 0; aVars->Buffer_Size = aVars->Neutron_Counter+aVars->Buffer_Block; }
      }

      while (!While_End)
      { /* we generate Coord[] and Coord_index[] from Buffer (auto limits) or passing neutron */
        if (aVars->Flag_Auto_Limits == 2)
        {
          if (While_Buffer < aVars->Buffer_Block)
          {
            /* first while loops (While_Buffer) */
            /* auto limits case : scan Buffer within limits and store in Mon2D */ 
            Coord[0] = aVars->Mon2D_Buffer[While_Buffer*(aVars->Coord_Number+2) + (aVars->Coord_Number)];
            
            for (i = 1; i <= aVars->Coord_Number; i++)
            {
              /* scanning variables in Buffer */
              XY = (aVars->Coord_Max[i]-aVars->Coord_Min[i]);
              Coord[i] = aVars->Mon2D_Buffer[While_Buffer*(aVars->Coord_Number+2) + (i-1)];
              if (XY > 0) Coord_Index[i] = floor((aVars->Mon2D_Buffer[(i-1) + While_Buffer*(aVars->Coord_Number+2)]-aVars->Coord_Min[i])*aVars->Coord_Bin[i]/XY);
              else Coord_Index[i] = 0;
              if (aVars->Flag_With_Borders)
              {
                if (Coord_Index[i] < 0) Coord_Index[i] = 0;
                if (Coord_Index[i] >= aVars->Coord_Bin[i]) Coord_Index[i] = aVars->Coord_Bin[i] - 1;
              }
            } /* end for */
            While_Buffer++;
          } /* end if in Buffer */
          else /* (While_Buffer >= aVars->Buffer_Block) && (aVars->Flag_Auto_Limits == 2) */ 
          {
            aVars->Flag_Auto_Limits = 0;
            if (!aVars->Flag_List) /* free Buffer not needed (no list to output) */
            { /* Dim : (aVars->Coord_Number+2)*aVars->Buffer_Block matrix (for p, dp) */ 
              free(aVars->Mon2D_Buffer);
            }
          }
        }
        else /* aVars->Flag_Auto_Limits == 0 or 1 */
        {
          for (i = 0; i <= aVars->Coord_Number; i++)
          { /* handle current neutron : last while */

            XY = 0;
            Set_aVars_Coord_Type = (aVars->Coord_Type[i] & 31);
            if (Set_aVars_Coord_Type == aDEFS->COORD_X) XY = aVars->cx; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_Y) XY = aVars->cy; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_Z) XY = aVars->cz; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_VX) XY = aVars->cvx; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_VY) XY = aVars->cvy; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_VZ) XY = aVars->cvz; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_KX) XY = V2K*aVars->cvx; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_KY) XY = V2K*aVars->cvy; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_KZ) XY = V2K*aVars->cvz; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_SX) XY = aVars->csx; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_SY) XY = aVars->csy; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_SZ) XY = aVars->csz; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_T) XY = aVars->ct; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_P) XY = aVars->cp; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_HDIV) XY = RAD2DEG*atan2(aVars->cvx,aVars->cvz); 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_VDIV) XY = RAD2DEG*atan2(aVars->cvy,aVars->cvz); 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_V) XY = sqrt(aVars->cvx*aVars->cvx+aVars->cvy*aVars->cvy+aVars->cvz*aVars->cvz); 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_RADIUS) XY = sqrt(aVars->cx*aVars->cx+aVars->cy*aVars->cy); 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_K) { XY = sqrt(aVars->cvx*aVars->cvx+aVars->cvy*aVars->cvy+aVars->cvz*aVars->cvz);  XY *= V2K; }
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_ENERGY) { XY = aVars->cvx*aVars->cvx+aVars->cvy*aVars->cvy+aVars->cvz*aVars->cvz;  XY *= VS2E; }
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_LAMBDA) { XY = sqrt(aVars->cvx*aVars->cvx+aVars->cvy*aVars->cvy+aVars->cvz*aVars->cvz);  XY *= V2K; if (XY != 0) XY = 2*PI/XY; }
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_NCOUNT) XY = Coord[i]+1; 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_ANGLE) 
            {  XY = sqrt(aVars->cvx*aVars->cvx+aVars->cvy*aVars->cvy+aVars->cvz*aVars->cvz);
               if (aVars->cvz != 0) 
               {
                 XY= RAD2DEG*atan2(XY,aVars->cvz);
               } else XY = 0;
            }
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_THETA)  { if (aVars->cz != 0) XY = RAD2DEG*atan2(aVars->cx,aVars->cz); } 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_PHI) { if (aVars->cz != 0) XY = RAD2DEG*atan2(aVars->cy,aVars->cz); } 
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_USER1) XY = aVars->UserVariable1;
                  else
            if (Set_aVars_Coord_Type == aDEFS->COORD_USER2) XY = aVars->UserVariable2;
                  else
            XY = 0;
            
            if (aVars->Coord_Type[i] & aDEFS->COORD_ABS) XY=fabs(XY);
            
            if (aVars->Coord_Type[i] & aDEFS->COORD_LOG)
            {  if (XY > 0) XY = log(XY)/log(10);
               else XY = -100; }

            Coord[i] = XY;
            if (!aVars->Flag_Auto_Limits)
            {
              XY = (aVars->Coord_Max[i]-aVars->Coord_Min[i]);
              if (XY > 0) Coord_Index[i] = floor((Coord[i]-aVars->Coord_Min[i])*aVars->Coord_Bin[i]/XY);
              else Coord_Index[i] = 0;
              if (aVars->Flag_With_Borders)
              {
                if (Coord_Index[i] < 0) Coord_Index[i] = 0;
                if (Coord_Index[i] >= aVars->Coord_Bin[i]) Coord_Index[i] = aVars->Coord_Bin[i] - 1;
              }
            } /* else Auto_Limits will get Index later from Buffer */
          } /* end for i */
          While_End = 1;
        } /* end else if aVars->Flag_Auto_Limits == 2 */

        if (aVars->Flag_Auto_Limits != 2) /* not when reading auto limits Buffer */
        { /* now store Coord into Buffer (no index needed) if necessary */
          if ((aVars->Buffer_Counter < aVars->Buffer_Block) && ((aVars->Flag_List) || (aVars->Flag_Auto_Limits == 1)))
          {
            for (i = 0; i < aVars->Coord_Number; i++)
            {
              aVars->Mon2D_Buffer[i + aVars->Neutron_Counter*(aVars->Coord_Number+2)] = Coord[i+1];
            }
            aVars->Mon2D_Buffer[aVars->Coord_Number + aVars->Neutron_Counter*(aVars->Coord_Number+2)]   = pp;
            aVars->Mon2D_Buffer[(aVars->Coord_Number+1) + aVars->Neutron_Counter*(aVars->Coord_Number+2)] = pp*pp;
            aVars->Buffer_Counter++;
            if (aVars->Flag_Verbose && (aVars->Buffer_Counter >= aVars->Buffer_Block) && (aVars->Flag_List == 1)) printf("Monitor_nD: %s %li neutrons stored in List.\n", aVars->compcurname, aVars->Buffer_Counter);
          }
          aVars->Neutron_Counter++;
        } /* end (aVars->Flag_Auto_Limits != 2) */

        /* store n1d/2d section for Buffer or current neutron in while */

        if (aVars->Flag_Auto_Limits != 1) /* not when storing auto limits Buffer */
        {
        /* 1D and n1D case : aVars->Flag_Multiple */
          if (aVars->Flag_Multiple)
          { /* Dim : aVars->Coord_Number*aVars->Coord_Bin[i] vectors (intensity is not included) */ 
            for (i= 0; i < aVars->Coord_Number; i++)
            {
              j = Coord_Index[i+1];
              if (j >= 0 && j < aVars->Coord_Bin[i+1])
              {
                aVars->Mon2D_N[i][j]++;
                aVars->Mon2D_p[i][j] += pp;
                aVars->Mon2D_p2[i][j] += pp*pp;
              }
            }
          }
          else /* 2D case : aVars->Coord_Number==2 and !aVars->Flag_Multiple and !aVars->Flag_List */
          if ((aVars->Coord_Number == 2) && !aVars->Flag_Multiple)
          { /* Dim : aVars->Coord_Bin[1]*aVars->Coord_Bin[2] matrix */
            i = Coord_Index[1];
            j = Coord_Index[2];
            if (i >= 0 && i < aVars->Coord_Bin[1] && j >= 0 && j < aVars->Coord_Bin[2])
            {
              aVars->Mon2D_N[i][j]++;
              aVars->Mon2D_p[i][j] += pp;
              aVars->Mon2D_p2[i][j] += pp*pp;
            }
          }
        } /* end (aVars->Flag_Auto_Limits != 1) */
      } /* end while */
    } /* end if aVars->Coord_Number */
  } /* end Monitor_nD_Trace */

void Monitor_nD_OutPut(MonitornD_Defines_type *aDEFS, MonitornD_Variables_type *aVars, char dofree)
  /* dofree = 0 : no free, =1 : free variables (last call) */
  {
    char   *fname;
    long    i,j;
    int    *p0m = NULL;
    double *p1m = NULL;
    double *p2m = NULL;
    char    Coord_X_Label[1024];
    double  min1d, max1d; 
    double  min2d, max2d;
    int     bin1d, bin2d;
    char    While_End = 0;
    long    While_Buffer = 0;
    double  XY, pp;
    double  Coord[MONnD_COORD_NMAX];
    long    Coord_Index[MONnD_COORD_NMAX];
    char    label[1024];
    
    if (dofree == 0)
    {
      if (aVars->Flag_Verbose) printf("Monitor_nD: %s save intermediate results (%.2f %%).\n", aVars->compcurname, 100*mcget_run_num()/mcget_ncount());
    }

    /* check Buffer flush when end of simulation reached */
    if ((aVars->Buffer_Counter < aVars->Buffer_Block) && aVars->Flag_Auto_Limits)
    {
      /* Get Auto Limits */
      if (aVars->Flag_Verbose) printf("Monitor_nD: %s getting %i Auto Limits from List (%li).\n", aVars->compcurname, aVars->Coord_Number, aVars->Buffer_Counter);
      for (i = 1; i <= aVars->Coord_Number; i++)
      {
        aVars->Coord_Min[i] = FLT_MAX;
        aVars->Coord_Max[i] = -FLT_MAX;

        for (j = 0; j < aVars->Buffer_Counter; j++)
        { 
                XY = aVars->Mon2D_Buffer[j*(aVars->Coord_Number+2) + (i-1)];  /* scanning variables in Buffer */
          if (XY < aVars->Coord_Min[i]) aVars->Coord_Min[i] = XY;
          if (XY > aVars->Coord_Max[i]) aVars->Coord_Max[i] = XY;

        }
      }
      aVars->Flag_Auto_Limits = 2;  /* pass to 2nd auto limits step */
      aVars->Buffer_Block = aVars->Buffer_Counter;
    
      while (!While_End)
      { /* we generate Coord[] and Coord_index[] from Buffer (auto limits) or passing neutron */
        if (While_Buffer < aVars->Buffer_Block)
        {
          /* first while loops (While_Buffer) */
          Coord[0] = aVars->Mon2D_Buffer[While_Buffer*(aVars->Coord_Number+2) + (aVars->Coord_Number)];
          pp = Coord[0];
            
          /* auto limits case : scan Buffer within limits and store in Mon2D */ 
          for (i = 1; i <= aVars->Coord_Number; i++)
          {
            /* scanning variables in Buffer */
            XY = (aVars->Coord_Max[i]-aVars->Coord_Min[i]);
            Coord[i] = aVars->Mon2D_Buffer[While_Buffer*(aVars->Coord_Number+2) + (i-1)];
            if (XY > 0) Coord_Index[i] = floor((aVars->Mon2D_Buffer[(i-1) + While_Buffer*(aVars->Coord_Number+2)]-aVars->Coord_Min[i])*aVars->Coord_Bin[i]/XY);
            else Coord_Index[i] = 0;
            if (aVars->Flag_With_Borders)
            {
              if (Coord_Index[i] < 0) Coord_Index[i] = 0;
              if (Coord_Index[i] >= aVars->Coord_Bin[i]) Coord_Index[i] = aVars->Coord_Bin[i] - 1;
            }
          } /* end for */
          While_Buffer++;
        } /* end if in Buffer */
        else /* (While_Buffer >= aVars->Buffer_Block) && (aVars->Flag_Auto_Limits == 2) */ 
        {
          aVars->Flag_Auto_Limits = 0;
          While_End = 1;
          if (!aVars->Flag_List && dofree) /* free Buffer not needed (no list to output) */
          { /* Dim : (aVars->Coord_Number+2)*aVars->Buffer_Block matrix (for p, dp) */ 
            free(aVars->Mon2D_Buffer);
          }
        }

        /* store n1d/2d section from Buffer */

        /* 1D and n1D case : aVars->Flag_Multiple */
        if (aVars->Flag_Multiple)
        { /* Dim : aVars->Coord_Number*aVars->Coord_Bin[i] vectors (intensity is not included) */ 
          for (i= 0; i < aVars->Coord_Number; i++)
          {
            j = Coord_Index[i+1];
            if (j >= 0 && j < aVars->Coord_Bin[i+1])
            {
              aVars->Mon2D_N[i][j]++;
              aVars->Mon2D_p[i][j] += pp;
              aVars->Mon2D_p2[i][j] += pp*pp;
            }
          }
        }
        else /* 2D case : aVars->Coord_Number==2 and !aVars->Flag_Multiple and !aVars->Flag_List */
        if ((aVars->Coord_Number == 2) && !aVars->Flag_Multiple)
        { /* Dim : aVars->Coord_Bin[1]*aVars->Coord_Bin[2] matrix */
          i = Coord_Index[1];
          j = Coord_Index[2];
          if (i >= 0 && i < aVars->Coord_Bin[1] && j >= 0 && j < aVars->Coord_Bin[2])
          {
            aVars->Mon2D_N[i][j]++;
            aVars->Mon2D_p[i][j] += pp;
            aVars->Mon2D_p2[i][j] += pp*pp;
          }
        } /* end store 2D/1D */
      } /* end while */
    } /* end Force Get Limits */

    if (aVars->Flag_Verbose) 
    {
      printf("Monitor_nD: %s is a %s.\n", aVars->compcurname, aVars->Monitor_Label);
      printf("Monitor_nD: version %s with options=%s\n", Monitor_nD_Version, aVars->option);
    }

    /* write oputput files (sent to file as p[i*n + j] vectors) */
    if (aVars->Coord_Number == 0) 
    {
      /* DETECTOR_OUT_0D(aVars->Monitor_Label, aVars->Nsum, aVars->psum, aVars->p2sum); */
      mcdetector_out_0D(aVars->Monitor_Label, aVars->Nsum, aVars->psum, aVars->p2sum, aVars->compcurname);
    }
    else
    if (strlen(aVars->Mon_File) > 0) 
    { 
      fname = (char*)malloc(strlen(aVars->Mon_File)+10*aVars->Coord_Number);
      if (aVars->Flag_List) /* List: DETECTOR_OUT_2D */
      {
        if (aVars->Flag_List == 2) aVars->Buffer_Size = aVars->Neutron_Counter;
        strcpy(fname,aVars->Mon_File);
        if (strchr(aVars->Mon_File,'.') == NULL) strcat(fname, "_list");

        min1d = 1; max1d = aVars->Coord_Number+2;
        min2d = 0; max2d = aVars->Buffer_Size; 
        bin1d = aVars->Coord_Number+2; bin2d = aVars->Buffer_Size;
        strcpy(Coord_X_Label,"");
        for (i= 1; i <= aVars->Coord_Number; i++)
        {
          if (min2d < aVars->Coord_Min[i]) min2d = aVars->Coord_Min[i];
          if (max2d < aVars->Coord_Max[i]) max2d = aVars->Coord_Max[i];
          strcat(Coord_X_Label, aVars->Coord_Var[i]);
          strcat(Coord_X_Label, " ");
          if (strchr(aVars->Mon_File,'.') == NULL)
                  { strcat(fname, "."); strcat(fname, aVars->Coord_Var[i]); }
        }
        strcat(Coord_X_Label, "I I_err");
        if (aVars->Flag_Verbose) printf("Monitor_nD: %s write monitor file %s List (%ix%li).\n", aVars->compcurname, fname,(aVars->Coord_Number+2),aVars->Buffer_Size);
        p0m = (int    *)malloc((aVars->Coord_Number+2)*aVars->Buffer_Size*sizeof(int));
        p1m = (double *)malloc((aVars->Coord_Number+2)*aVars->Buffer_Size*sizeof(double)); 
        if (min2d == max2d) max2d = min2d+1e-6;
        if (min1d == max1d) max1d = min1d+1e-6;
        if (dofree == 0)
        {
          sprintf(label, "%s (%.2f %%)", aVars->Monitor_Label, 100*mcget_run_num()/mcget_ncount());
        }
        else
          strcpy(label, aVars->Monitor_Label);
        if (p1m == NULL) /* use Raw Buffer line output */
        {
          if (aVars->Flag_Verbose) printf("Monitor_nD: %s cannot allocate memory for List transpose. Skipping.\n", aVars->compcurname);
          if (p0m != NULL) free(p0m);
          mcdetector_out_2D(
            label,
            aVars->Coord_Label[0],
            Coord_X_Label,
            min2d, max2d, 
            min1d, max1d, 
            bin2d,
            bin1d,
            (int *)aVars->Mon2D_Buffer,aVars->Mon2D_Buffer,aVars->Mon2D_Buffer,
            fname, aVars->compcurname); 
        
        }
        else /* transpose for column output */
        {
          for (i=0; i < aVars->Coord_Number+2; i++) 
            for (j=0; j < aVars->Buffer_Size; j++)
            {
              p1m[i*aVars->Buffer_Size+j] = aVars->Mon2D_Buffer[j*(aVars->Coord_Number+2) + i];
              p0m[i*aVars->Buffer_Size+j] = 1;
            }
            
          mcdetector_out_2D(
              label,
              Coord_X_Label,
              aVars->Coord_Label[0],
              min1d, max1d, 
              min2d, max2d, 
              bin1d,
              bin2d,
              p0m,p1m,aVars->Mon2D_Buffer,
              fname, aVars->compcurname); 
          free(p0m);
          free(p1m);
        }
      }
      if (aVars->Flag_Multiple) /* n1D: DETECTOR_OUT_1D */
      {
        for (i= 0; i < aVars->Coord_Number; i++)
        {
          
          strcpy(fname,aVars->Mon_File);
          if (strchr(aVars->Mon_File,'.') == NULL)
                  { strcat(fname, "."); strcat(fname, aVars->Coord_Var[i+1]); }
          sprintf(Coord_X_Label, "%s monitor", aVars->Coord_Label[i+1]);
          if (dofree == 0)
          {
            sprintf(label, "%s (%.2f %%)", Coord_X_Label, 100*mcget_run_num()/mcget_ncount());
          }
          else
            strcpy(label, Coord_X_Label);
          if (aVars->Flag_Verbose) printf("Monitor_nD: %s write monitor file %s 1D (%i).\n", aVars->compcurname, fname, aVars->Coord_Bin[i+1]);
          min1d = aVars->Coord_Min[i+1];
          max1d = aVars->Coord_Max[i+1];
          if (min1d == max1d) max1d = min1d+1e-6;
          p1m = (double *)malloc(aVars->Coord_Bin[i+1]*sizeof(double));
          p2m = (double *)malloc(aVars->Coord_Bin[i+1]*sizeof(double));
          if (p2m == NULL) /* use Raw Buffer line output */
          {
            if (aVars->Flag_Verbose) printf("Monitor_nD: %s cannot allocate memory for output. Using raw data.\n", aVars->compcurname);
            if (p1m != NULL) free(p1m);
            mcdetector_out_1D(
            label,
            aVars->Coord_Label[i+1],
            aVars->Coord_Label[0],
            aVars->Coord_Var[i+1],
            min1d, max1d, 
            aVars->Coord_Bin[i+1],
            aVars->Mon2D_N[i],aVars->Mon2D_p[i],aVars->Mon2D_p2[i],
            fname, aVars->compcurname); 
          }
          else
          {
            if (aVars->Flag_log != 0)
            {
              XY = FLT_MAX;
              for (j=0; j < aVars->Coord_Bin[i+1]; j++) /* search min of signal */
                if ((XY > aVars->Mon2D_p[i][j]) && (aVars->Mon2D_p[i][j] > 0)) XY = aVars->Mon2D_p[i][j];
              if (XY <= 0) XY = -log(FLT_MAX)/log(10); else XY = log(XY)/log(10)-1;
            }
            
            for (j=0; j < aVars->Coord_Bin[i+1]; j++)
            {
              if (aVars->Flag_log == 0)
              {
                p1m[j] = aVars->Mon2D_p[i][j];
                p2m[j] = aVars->Mon2D_p2[i][j];
              }
              else
              {
                if (aVars->Mon2D_p[i][j] > 0)
                {
                  p1m[j] = log(aVars->Mon2D_p[i][j])/log(10);
                  p2m[j] = (aVars->Mon2D_p2[i][j])/(aVars->Mon2D_p[i][j]*aVars->Mon2D_p[i][j]);
                }
                else
                {
                  p1m[j] = XY;
                  p2m[j] = 0;
                }
              }
            }
            mcdetector_out_1D(
              label,
              aVars->Coord_Label[i+1],
              aVars->Coord_Label[0],
              aVars->Coord_Var[i+1],
              min1d, max1d, 
              aVars->Coord_Bin[i+1],
              aVars->Mon2D_N[i],p1m,p2m,
              fname, aVars->compcurname); 
            
          }
          if (p1m != NULL) free(p1m);
          if (p2m != NULL) free(p2m);
            
        }
      }
      else
      if (aVars->Coord_Number == 2)  /* 2D: DETECTOR_OUT_2D */
      {
        strcpy(fname,aVars->Mon_File);

        p0m = (int    *)malloc(aVars->Coord_Bin[1]*aVars->Coord_Bin[2]*sizeof(int));
        p1m = (double *)malloc(aVars->Coord_Bin[1]*aVars->Coord_Bin[2]*sizeof(double));
        p2m = (double *)malloc(aVars->Coord_Bin[1]*aVars->Coord_Bin[2]*sizeof(double));
        if (p2m == NULL) 
        {
          if (aVars->Flag_Verbose) printf("Monitor_nD: %s cannot allocate memory for 2D array (%i). Skipping.\n", aVars->compcurname, 3*aVars->Coord_Bin[1]*aVars->Coord_Bin[2]*sizeof(double));
          if (p0m != NULL) free(p0m);
          if (p1m != NULL) free(p1m);
        }
        else
        {
          if (aVars->Flag_log != 0)
          {
            XY = FLT_MAX;
            for (i= 0; i < aVars->Coord_Bin[1]; i++)
              for (j= 0; j < aVars->Coord_Bin[2]; j++) /* search min of signal */
                if ((XY > aVars->Mon2D_p[i][j]) && (aVars->Mon2D_p[i][j]>0)) XY = aVars->Mon2D_p[i][j];
            if (XY <= 0) XY = -log(FLT_MAX)/log(10); else XY = log(XY)/log(10)-1;
          }
          for (i= 0; i < aVars->Coord_Bin[1]; i++)
          {
            for (j= 0; j < aVars->Coord_Bin[2]; j++)
            {
              p0m[j + i*aVars->Coord_Bin[2]] = aVars->Mon2D_N[i][j];
              if (aVars->Flag_log == 0)
              {
                p1m[j + i*aVars->Coord_Bin[2]] = aVars->Mon2D_p[i][j];
                p2m[j + i*aVars->Coord_Bin[2]] = aVars->Mon2D_p2[i][j];
              }
              else
              {
                if (aVars->Mon2D_p[i][j] > 0)
                {
                  p1m[j + i*aVars->Coord_Bin[2]] = log(aVars->Mon2D_p[i][j])/log(10);
                  p2m[j + i*aVars->Coord_Bin[2]] = aVars->Mon2D_p2[i][j]/aVars->Mon2D_p[i][j];
                }
                else
                {
                  p1m[j + i*aVars->Coord_Bin[2]] = XY;
                  p2m[j + i*aVars->Coord_Bin[2]] = 0;
                }
              }
            }
          }
          if (strchr(aVars->Mon_File,'.') == NULL)
                  { strcat(fname, "."); strcat(fname, aVars->Coord_Var[1]);
              strcat(fname, "_"); strcat(fname, aVars->Coord_Var[2]); }
          if (aVars->Flag_Verbose) printf("Monitor_nD: %s write monitor file %s 2D (%ix%i).\n", aVars->compcurname, fname, aVars->Coord_Bin[1], aVars->Coord_Bin[2]);

          min1d = aVars->Coord_Min[1];
          max1d = aVars->Coord_Max[1];
          if (min1d == max1d) max1d = min1d+1e-6;
          min2d = aVars->Coord_Min[2];
          max2d = aVars->Coord_Max[2];
          if (min2d == max2d) max2d = min2d+1e-6;
          if (dofree == 0)
          {
            sprintf(label, "%s (%.2f %%)", aVars->Monitor_Label, 100*mcget_run_num()/mcget_ncount());
          }
          else
            strcpy(label, aVars->Monitor_Label);

          mcdetector_out_2D(
            label,
            aVars->Coord_Label[1],
            aVars->Coord_Label[2],
            min1d, max1d,  
            min2d, max2d,  
            aVars->Coord_Bin[1],
            aVars->Coord_Bin[2],
            p0m,p1m,p2m,
            fname, aVars->compcurname);

          if (p0m != NULL) free(p0m);
          if (p1m != NULL) free(p1m);
          if (p2m != NULL) free(p2m); 
        }
      }
      free(fname);
    }

    /* Now Free memory Mon2D.. */
    if ((aVars->Flag_Auto_Limits || aVars->Flag_List) && aVars->Coord_Number)
    { /* Dim : (aVars->Coord_Number+2)*aVars->Buffer_Block matrix (for p, dp) */ 
      if (aVars->Mon2D_Buffer != NULL && dofree) free(aVars->Mon2D_Buffer);
    }
       
    /* 1D and n1D case : aVars->Flag_Multiple */
    if (aVars->Flag_Multiple && aVars->Coord_Number && dofree)
    { /* Dim : aVars->Coord_Number*aVars->Coord_Bin[i] vectors */ 
      for (i= 0; i < aVars->Coord_Number; i++)
      { 
        free(aVars->Mon2D_N[i]);
        free(aVars->Mon2D_p[i]);
        free(aVars->Mon2D_p2[i]);
      }
      free(aVars->Mon2D_N);
      free(aVars->Mon2D_p);
      free(aVars->Mon2D_p2);
    }
    

    /* 2D case : aVars->Coord_Number==2 and !aVars->Flag_Multiple and !aVars->Flag_List */
    if ((aVars->Coord_Number == 2) && !aVars->Flag_Multiple && dofree)
    { /* Dim : aVars->Coord_Bin[1]*aVars->Coord_Bin[2] matrix */
      for (i= 0; i < aVars->Coord_Bin[1]; i++)
      { 
        free(aVars->Mon2D_N[i]);
        free(aVars->Mon2D_p[i]);
        free(aVars->Mon2D_p2[i]);
      }
      free(aVars->Mon2D_N); 
      free(aVars->Mon2D_p);
      free(aVars->Mon2D_p2);
    }
  } /* end Monitor_nD_OutPut */
